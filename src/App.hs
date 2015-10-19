{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- For lucid
{-# LANGUAGE FlexibleContexts    #-}

module App
    ( serve
    ) where

import           BasePrelude
import qualified Data.Configurator                  as C
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding                          as T
import Lucid
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow   as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.FromField   as PG
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import  Web.Spock.Safe hiding (SessionId) -- why does this exist?
import  Web.Users.Types
import  Web.Users.Postgresql ()
import qualified Data.Time as Time
import Control.Monad.Trans (liftIO)


data Turtle = Turtle
    { t_name  :: Text
    , t_color :: Color
    } deriving Show

data Color = Red | Blue | Orange | Purple deriving Show

instance PG.FromField Color where
  fromField f mdata =
    case mdata of
      Nothing -> PG.returnError PG.UnexpectedNull f ""
      Just b  -> case T.decodeUtf8 b of
        "RED"    -> return Red
        "BLUE"   -> return Blue
        "ORANGE" -> return Orange
        "PURPLE" -> return Purple
        _ -> error "invalid color"

instance PG.FromRow Color where
  fromRow = PG.field

instance PG.ToField Color where
  toField Red    = PG.toField ("RED"    :: Text)
  toField Blue   = PG.toField ("BLUE"   :: Text)
  toField Orange = PG.toField ("ORANGE" :: Text)
  toField Purple = PG.toField ("PURPLE" :: Text)

instance PG.ToRow Turtle where
  toRow (Turtle name color) =
    PG.toRow (name, color)

instance PG.FromRow Turtle where
  fromRow = Turtle <$> PG.field <*> PG.field

loadConnInfo :: FilePath -> IO PG.ConnectInfo
loadConnInfo fpath = do
  dbCfg <- C.load [C.Required fpath]
  PG.ConnectInfo <$> C.require dbCfg "host"
                 <*> C.require dbCfg "port"
                 <*> C.require dbCfg "user"
                 <*> C.require dbCfg "pass"
                 <*> C.require dbCfg "db"

mkConnBuilder :: PG.ConnectInfo -> ConnBuilder PG.Connection
mkConnBuilder connInfo =
  ConnBuilder { cb_createConn = PG.connect connInfo
              , cb_destroyConn = PG.close
              , cb_poolConfiguration =
                -- TODO(cgag): figure out if these are reasonable values,
                -- don't know anything about striping.  Not sure how long
                -- we should keep a connection open either.
                PoolCfg { pc_stripes = 1
                        , pc_resPerStripe = 5
                        , pc_keepOpenTime = 60 }
              }

myapp :: SpockM PG.Connection (Maybe SessionId) () ()
myapp =
  do middleware (staticPolicy (addBase "public"))
     get root (lucid $ layout $
         do p_ "hello"
            p_ "world")
     get "/init" $
         do void $ runQuery insertCurtis
            html "hello"
     get "/login" (lucid $ layout $
         form_ [method_ "post", action_ "/login"] $
            do label_ [Lucid.for_ "email"] "email"
               input_ [type_ "text", name_ "email"]
               label_ [Lucid.for_ "pass"] "pass"
               input_ [type_ "password", name_ "pass"]
               input_ [type_ "submit"])
     post "/login" $
          do email <- param' "email"
             pass  <- param' "pass"
             mSessID <- runQuery (\conn -> do
               now <- Time.getCurrentTime
               -- 1000 days.  Really need a package for these stupid multiplications
               authUser conn email (PasswordPlain pass) (3600 * 24 * 1000))
             msg <- case mSessID of
                 Just sid -> do writeSession (Just sid)
                                return ("Created session: " <> show sid)
                 Nothing  -> return "Failed to create session"
             lucid (toHtml msg)
     get "/register" (lucid $ layout $
         form_ [method_ "post", action_ "/register"] $
            do label_ [Lucid.for_ "email"] "email"
               input_ [type_ "text", name_ "email"]
               label_ [Lucid.for_ "pass"] "pass"
               input_ [type_ "password", name_ "pass"]
               input_ [type_ "submit"])
     post "/register" $
          do email <- param' "email"
             pass  <- param' "pass"
             let user = User { u_name  = email
                             , u_email = email
                             , u_password = makePassword (PasswordPlain pass)
                             , u_active = True
                             , u_more   = Nothing :: Maybe Integer
                             }
             eErrUID <- runQuery (`createUser` user)
             -- why can't i use a let here?
             lucid $ layout $ p_ (toHtml $ case eErrUID of
                Left e -> case e of
                    UsernameOrEmailAlreadyTaken -> "Email is in use"
                    InvalidPassword -> "Invalid password"
                Right uid -> "Successfully created user: " <> show uid)
     get "/all" $
         do sess <- readSession
            -- TODO(cgag): use maybe fn, maybe monad?
            userStr <- case sess of
                  Just sid -> do 
                      mUid <- runQuery (\conn -> verifySession conn sid 0)
                      case mUid of
                          Just uid -> 
                              do liftIO $ print ("loking up user: " <> show uid)
                                 -- should make a newtype or alias for our concrete
                                 -- user type. If getUserById fails to parse the more field,
                                 -- we jsut get nothing here.
                                 mUser :: Maybe (User (Maybe Integer)) <- runQuery (`getUserById` uid) 
                                 liftIO $ print mUser
                                 case mUser of 
                                    Just user -> return (T.pack . show $ user)
                                    Nothing -> return "fuck, no user with this id"
                          Nothing -> return "no user"
                  Nothing -> return "invalid session"
            turtles <- runQuery getAllTurtles
            let text = foldMap (T.append "<br/>" . T.pack . show) turtles
            html (mconcat [text
                          ,"<br/>"
                          ,T.pack (show sess)
                          ,"<br/>"
                          ,userStr])
     get "/allColors" $
         do colors <- runQuery getAllTurtleColors
            html (foldMap (T.append "<br/>" . T.pack . show) colors)

serve :: Int -> IO ()
serve port = do
    connInfo <- loadConnInfo "db.cfg"
    conn <- PG.connect connInfo
    initUserBackend conn
    housekeepBackend conn
    runSpock port (spock (spockCfg connInfo) myapp)
  where
    -- spockCfg :: PG.ConnectInfo -> SpockCfg
    spockCfg connInfo =
      SpockCfg { spc_initialState = ()
               , spc_database = PCConn (mkConnBuilder connInfo)
               , spc_sessionCfg = (defaultSessionCfg Nothing) {
                   sc_cookieName= "appCookie"
                 }
                 -- 10MB = (1 byte * 1000 bytes/kb * 1000 kb/mb * 10)
                 -- TODO(cgag): I should make a tiny package with these stupid multiplications
                 -- in it. Something like 10 * ByteSize.Megabyte.
               , spc_maxRequestSize = Just (1024 * 1024 * 10)
               }

getAllTurtles :: PG.Connection -> IO [Turtle]
getAllTurtles conn = PG.query_ conn "select * from turtles"

getAllTurtleColors :: PG.Connection -> IO [Color]
getAllTurtleColors conn = PG.query_ conn "select color from turtles"

insertCurtis :: PG.Connection -> IO Int64
insertCurtis conn =
  PG.executeMany conn
                 "insert into turtles (name, color) values (?,?)"
                 [Turtle "Raphael" Red, Turtle "Leonardo" Blue]

lucid :: Html () -> SpockAction conn sess state ()
lucid = html . TL.toStrict . renderText

layout :: Html () -> Html ()
layout x =  doctypehtml_ $
            do body_ x
