{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- For lucid
{-# LANGUAGE FlexibleContexts    #-}
-- HVect
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module App
    ( serve
    ) where

import           BasePrelude
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Data.Configurator                    as C
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Lazy                       as TL

import           Data.HVect
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.ToRow     as PG
import           Lucid
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
import           Web.Spock.Safe                       hiding (SessionId)
import           Web.Users.Postgresql                 ()
import           Web.Users.Types                      (User (..))
import qualified Web.Users.Types                      as U

type AppState = ()
type AppUser   = User (Maybe Integer)
type AppAction ctx a = SpockActionCtx ctx
                                      PG.Connection 
                                      (Maybe U.SessionId) 
                                      AppState
                                      a

data Turtle = Turtle
    { t_name  :: Text
    , t_color :: Color
    } deriving Show

data Color = Red | Blue | Orange | Purple deriving Show

instance ToJSON Turtle where
    toJSON (Turtle {t_name=name, t_color=color}) =
        object [ "name" .= name
               , "color" .= (T.toLower . T.pack . show) color
               ]

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

myapp :: SpockM PG.Connection (Maybe U.SessionId) () ()
myapp =
  do middleware (staticPolicy (addBase "public"))
     prehook initHook $
       do get root (lucid $ layout $
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
                 mSessID <- runQuery (\conn -> 
                    do -- 1000 days.  Really need a package for these stupid multiplications
                       U.authUser conn email (U.PasswordPlain pass) (3600 * 24 * 1000))
                 msg <- case mSessID of
                     Just sid -> do writeSession (Just sid)
                                    return ("Created session: " <> show sid)
                     Nothing  -> return "Failed to create session"
                 lucid (toHtml msg)

          get "/logout" $
              do mSessId <- readSession
                 maybe (text "not logged in")
                       (\sessId -> do runQuery (`U.destroySession` sessId)
                                      redirect "/")
                       mSessId

          get "/register" (lucid $ layout $
               form_ [method_ "post", action_ "/register"] $
                  do label_ [Lucid.for_ "email"] "email"
                     input_ [type_ "text", name_ "email"]
                     label_ [Lucid.for_ "pass"] "pass"
                     input_ [type_ "password", name_ "pass"]
                     label_ [Lucid.for_ "pass_confirm"] "password confirmation"
                     input_ [type_ "password", name_ "pass_confirm"]
                     input_ [type_ "submit"])

          post "/register" $
              do email       <- param' "email"
                 pass        <- param' "pass"
                 passConfirm <- param' "pass_confirm"
                 when (pass /= passConfirm) $
                   -- TODO(cgag): flash message with error
                   redirect "/register" 
                 let user = User { u_name  = email
                                 , u_email = email
                                 , u_password = U.makePassword (U.PasswordPlain pass)
                                 , u_active = True
                                 , u_more   = Nothing :: Maybe Integer
                                 }
                 eErrUID <- runQuery (`U.createUser` user)
                 -- why can't i use a let here?
                 lucid $ layout $ p_ (toHtml $ case eErrUID of
                    Left e -> case e of
                        U.UsernameOrEmailAlreadyTaken -> "Email is in use"
                        U.InvalidPassword -> "Invalid password"
                    Right uid -> "Successfully created user: " <> show uid)

          get "/allColors" $
             do colors <- runQuery getAllTurtleColors
                html (foldMap (T.append "<br/>" . T.pack . show) colors)
          
          get "/all" $
             do mUser <- getUserFromSession
                let userStr = maybe "no user" (T.pack . show) mUser
                turtles <- runQuery getAllTurtles
                let ret = foldMap (T.append "<br/>" . T.pack . show) turtles
                html (mconcat [ret
                              ,"<br/>"
                              ,userStr])

          prehook authHook $
            do get "/protected" $ text "only logged in users should see this"

               get "/allJSON" $
                  do ts <- runQuery getAllTurtles
                     json ts


serve :: Int -> IO ()
serve port = do
    connInfo <- loadConnInfo "db.cfg"
    conn <- PG.connect connInfo
    U.initUserBackend conn
    U.housekeepBackend conn
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

lucid :: Html () -> SpockActionCtx ctx conn sess state ()
lucid = html . TL.toStrict . renderText

layout :: Html () -> Html ()
layout x =  doctypehtml_ $
                body_  $
                  do nav
                     x
  where
    nav = ul_ $
            do li_ (a_ [href_ "/"] "home")
               li_ (a_ [href_ "/login"] "login")
               li_ (a_ [href_ "/register"] "register")

initHook :: (Monad m) => ActionCtxT () m (HVect '[])
initHook = return HNil

-- TODO(cgag): this should an intended route before redirecting
-- to login, then login should redirect to that intended route.
authHook :: AppAction (HVect xs) (HVect (AppUser ': xs))
authHook = 
  do oldCtx <- getContext
     mUser  <- getUserFromSession
     case mUser of
         Nothing   -> redirect "/login"
         Just user -> return (user :&: oldCtx)

getUserFromSession :: AppAction ctx (Maybe AppUser)
getUserFromSession = 
  runMaybeT $
  do sessId <- MaybeT readSession
     uid    <- MaybeT $ runQuery (\conn -> U.verifySession conn sessId 0)
     user   <- MaybeT $ runQuery (`U.getUserById` uid)
     return user
