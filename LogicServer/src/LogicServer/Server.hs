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
import LogicServer.Sequent


data Pong = Pong deriving (Show)

instance MimeRender PlainText Pong where
  mimeRender _ = BC.pack . show


-- type API =    "ping" :> Get '[PlainText] Pong
--          :<|> "hello" :> ReqBody '[JSON, PlainText] String :> Post '[PlainText] String
type API = "solve" :> ReqBody '[JSON] (Expr, Expr) :> Post '[JSON] Bool



pingHandler :: Handler Pong
pingHandler = return Pong

helloHandler :: String -> Handler String
helloHandler name = return $ "Hello " ++ name

solveHandler :: (Expr, Expr) -> Handler Bool
solveHandler (given, goal) = return $ solve ( given `therefore` goal)

-- server :: Server API
-- server =    pingHandler
--        :<|> helloHandler

server :: Server API
server = solveHandler

apiProxy :: Proxy API
apiProxy = Proxy

app :: Application
app = serve apiProxy server


