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
import qualified Data.Text.Encoding                          as T
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow   as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.FromField   as PG
import           Web.Spock.Safe

newtype SessionID = SessionID Int

data Turtle = Turtle
    { t_name  :: Text
    , t_color :: Color
    } deriving Show

data Color = Red | Blue | Orange | Purple deriving Show

instance PG.FromField Color where
  fromField f mdata = 
    case mdata of
      Nothing -> PG.returnError PG.UnexpectedNull f ""
      Just b -> case T.decodeUtf8 b of
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

myapp :: SpockM PG.Connection (Maybe SessionID) () ()
myapp = do
    get root $
      do html "home"
    get "/init" $
      do void $ runQuery insertCurtis
         html "hello"
    get "/all" $ do
      turtles <- runQuery getAllTurtles
      html (foldMap (T.append "<br/>" . T.pack . show) turtles)
    get "/allColors" $ do
      colors <- runQuery getAllTurtleColors
      html (foldMap (T.append "<br/>" . T.pack . show) colors)

serve :: Int -> IO ()
serve port = do
  connInfo <- loadConnInfo "db.cfg"
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
