{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8         as B
import           Data.Char                     (toLower)
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import           Data.IORef
import           Data.Maybe                    (fromMaybe)
import           Data.Pool                     (createPool)
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as P
-- import qualified Database.PostgreSQL.Simple.Transaction as P
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           System.Environment            (getEnv)
import           Text.Show.Pretty              (getDataDir)

data Foo = Foo

data Bar = Bar

fooInit :: SnapletInit b Foo
fooInit = makeSnaplet "foo" "Foo snaplet" Nothing $ return Foo

barInit :: SnapletLens b Foo -> SnapletInit b Bar
barInit _h = makeSnaplet "bar" "Bar snaplet" Nothing $ return Bar

data App = App
    { _heist       :: Snaplet (Heist App)
    , _db          :: Snaplet Postgres
    , _foo         :: Snaplet Foo
    , _bar         :: Snaplet Bar
    , _companyName :: IORef B.ByteString
    }

data PGEnvConfig = PGEnvConfig
    { _user   :: String
    , _pswd   :: String
    , _host   :: String
    , _port   :: String
    , _dbName :: String
    }

makeLenses ''App
makeLenses ''PGEnvConfig

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)


getPGConn :: MonadIO m => m PGEnvConfig
getPGConn = liftIO $ do
  eUsr <- getEnv "POSTGRESQL_USERNAME"
  ePwd <- getEnv "POSTGRESQL_PASSWORD"
  eHst <- getEnv "POSTGRESQL_SERVICE_HOST"
  ePrt <- getEnv "POSTGRESQL_SERVICE_PORT"
  eDBN  <- getEnv "POSTGRESQL_DATABASE"
  return $ PGEnvConfig eUsr ePwd eHst ePrt eDBN

makeConnStr :: PGEnvConfig -> String
makeConnStr envConf = "user='" ++ view user envConf ++
  "' pass='" ++ view pswd envConf ++
  "' host='" ++ view host envConf ++
  "' port='" ++ view port envConf ++
  "' database='" ++ view dbName envConf


description :: T.Text
description = "PostgreSQL abstraction"


datadir :: Maybe (IO FilePath)
datadir = Just $ fmap (<>"/resources/db") getDataDir

makePGConfig :: MonadIO m => C.Config -> m PGSConfig
makePGConfig config = liftIO $ do
  envConf <- getPGConn
  stripes <- C.lookupDefault 1 config "numStripes"
  idle <- C.lookupDefault 5 config "idletime"
  resources <- C.lookupDefault 20 config "maxResourcesPerStripe"
  let connstr = B.pack $ makeConnStr envConf in
    return $ PGSConfig connstr stripes idle resources

myPgsInit :: Snap.SnapletInit b Postgres
myPgsInit = makeSnaplet "postgresql-simple" description datadir $ do
  config <- makePGConfig =<< getSnapletUserConfig
  initHelper config

initHelper :: MonadIO m => PGSConfig -> m Postgres
initHelper PGSConfig{..} = do
    pool <- liftIO $ createPool (P.connectPostgreSQL pgsConnStr) P.close
                                pgsNumStripes (realToFrac pgsIdleTime)
                                pgsResources
    return $ PostgresPool pool

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/", writeText "hello")
         , ("insult", insultHandler)
         ]

isVowel :: Char -> Bool --
isVowel = (`elem` ("aeiou" :: String))  . toLower

insultQuery :: Query
insultQuery = "SELECT a.string AS first b.string AS second c.string AS noun from short_adjective a, long adjective b, noun c ORDER BY random() limit 1"

insultHandler :: Handler App App ()
insultHandler = do
  results <- query_ insultQuery
  let result = head results
      (first,second,noun) :: (String, String, String) = result
      article = if isVowel $ head first then "an" else "a" in
        writeText $ T.pack $ "Thout art " ++ article ++ " " ++ first ++ " " ++ second ++ " " ++ noun ++ "!"

appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
  hs <- nestSnaplet "heist" heist $ heistInit "templates"
  fs <- nestSnaplet "foo" foo fooInit
  d  <- nestSnaplet "db" db myPgsInit
  bs <- nestSnaplet "" bar $ nameSnaplet "newname" $ barInit foo
  addRoutes [ ("hello", writeText "hello world")
            , ("fooname", with foo namePage)
            , ("barname", with bar namePage)
            , ("company", companyHandler)
            ]
  wrapSite (<|> heistServe)
  ref <- liftIO $ newIORef "fooCorp"
  addRoutes routes
  return $ App hs d fs bs ref

namePage :: Handler b v ()
namePage = do
  mname <- getSnapletName
  writeText $ fromMaybe "This shouldn't happen." mname

companyHandler :: Handler App App ()
companyHandler = method GET getter <|> method POST setter
  where
    getter = do
      nameRef <- gets _companyName
      name <- liftIO $ readIORef nameRef
      writeBS name
    setter = do
      mname <- getParam "name"
      nameRef <- gets _companyName
      liftIO $ maybe (return ()) (writeIORef nameRef) mname
      getter

instance HasHeist App where heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig appInit
