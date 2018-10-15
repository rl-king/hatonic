{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Int
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant
import System.IO
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


type Api =
  "resource" :> Capture "id" Int64 :> Get '[JSON] Text


api :: Proxy Api
api = Proxy


run :: IO ()
run = do
  let port = 8080
      connectionSettings =
        Connection.settings "localhost" 5432 "zotonic" "" "elmginger"
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings

  Right connection <- Connection.acquire connectionSettings
  runSettings settings =<< mkApp connection


session :: Int64 -> Session Text
session id = do
  Session.statement id stmnt


stmnt :: Statement Int64 Text
stmnt =
  Statement sql encoder decoder True
  where sql = "SELECT name FROM rsc WHERE id = $1"
        encoder = Encoders.param Encoders.int8
        decoder = Decoders.singleRow $ Decoders.column Decoders.text

mkApp :: Connection -> IO Application
mkApp c =
  return . gzip def { gzipFiles = GzipCompress } . serve api $ server c


server :: Connection -> Server Api
server c =
  hellow c


hellow :: Connection -> Int64 -> Handler Text
hellow c id = do
  Right result <- liftIO $ Session.run (session id) c
  return result
