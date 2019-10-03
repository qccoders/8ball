{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators   #-}
module Main where
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
import           Data.HashSet
import           Data.Maybe
import           GHC.Generics
import           Magic8Ball
import           Network.HTTP.Client         (Manager, defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.Client
import           System.Environment

type Magic8BallApi =
  "webhooks" :> Header "Authorization" String :> ReqBody '[PlainText, JSON] String :> Post '[JSON] ()
  :<|> "webhooks" :> Capture "uri" String :> Delete '[JSON] ()
  :<|> "answer" :> QueryParam "q" String :> QueryParam "ttl" String :> Get '[JSON] Answer

data Answer = Answer {
  name     :: String,
  response :: Magic8Ball,
  delay    :: Int,
  children :: [Answer]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  (x:xs) <- getArgs
  nodeRegistry <- newTVarIO ([] :: HashSet String)
  withStdoutLogger $ \aplogger -> do
    let settings = setPort (read x :: Int) $ setLogger aplogger defaultSettings
    runSettings settings (application nodeRegistry)
  --run (read x :: Int) (application nodeRegistry)
  where application nodeRegistry = serve api (server nodeRegistry)

api :: Proxy Magic8BallApi
api = Proxy

server :: TVar (HashSet String) -> Server Magic8BallApi
server registry =
  registerHandler registry
  :<|> unregisterHandler registry
  :<|> answerHandler registry
  where
    registerHandler :: TVar (HashSet String) ->  Maybe String -> String -> Handler ()
    registerHandler registry Nothing uri = do
      _ <- liftIO $ putStrLn "Bad Request"
      return ()
    registerHandler registry (Just authHeader) uri = do
      _ <- liftIO . putStrLn $ "registration received: " ++ uri
      -- check auth header for correct info
      case parseBaseUrl uri of
        Left _ -> return ()
        Right baseUrl -> liftIO . atomically . modifyTVar registry . insert $ showBaseUrl baseUrl

    unregisterHandler :: TVar (HashSet String) -> String -> Handler ()
    unregisterHandler registry = liftIO . atomically . modifyTVar registry . delete

    answerHandler :: TVar (HashSet String) -> Maybe String -> Maybe String -> Handler Answer
    answerHandler registry question ttl = do
      _ <- liftIO . putStrLn $ "QUESTION: " ++ fromMaybe "*NO QUESTION PROVIDED, RETARD ALERT*" question
      a <- liftIO ask
      manager' <- liftIO $ newManager defaultManagerSettings
      urls <- liftIO $ readTVarIO registry
      let uris = fmap (async . makeRequest manager' question) . rights . fmap parseBaseUrl . toList $ urls
      listOfRunningTasks <- traverse liftIO uris-- create a list of actions to run.
      listOfResults <- traverse liftIO $ fmap wait listOfRunningTasks
      -- query children, with a maximum time of watever the TTL is.
      _ <- liftIO . print $ ans listOfResults a
      return $ ans listOfResults a
        where ans listOfResults' a' = Answer {
                name = "",
                response = a',
                delay = 0,
                children = rights listOfResults'
                }
makeRequest :: Manager -> Maybe String -> BaseUrl -> IO (Either ClientError Answer)
makeRequest manager' question url = runClientM (answer question (Just "5")) (mkClientEnv manager' url)

-- clients
register :: Maybe String -> String -> ClientM ()
unregister :: String -> ClientM ()
answer :: Maybe String -> Maybe String -> ClientM Answer
register :<|> unregister :<|> answer = client api
