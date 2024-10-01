{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LogicServer.Server(app) where
import Servant.API
import GHC.Generics
import Data.Aeson
import Servant
import qualified Data.ByteString.Lazy.Char8 as BC


data Pong = Pong deriving (Show)

instance MimeRender PlainText Pong where
  mimeRender _ = BC.pack . show


type API =    "ping" :> Get '[PlainText] Pong
         :<|> "hello" :> ReqBody '[JSON, PlainText] String :> Post '[PlainText] String


pingHandler :: Handler Pong
pingHandler = return Pong

helloHandler :: String -> Handler String
helloHandler name = return $ "Hello " ++ name

server :: Server API
server =    pingHandler
       :<|> helloHandler

apiProxy :: Proxy API
apiProxy = Proxy

app :: Application
app = serve apiProxy server


