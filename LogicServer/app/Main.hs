module Main (main) where
import LogicServer.Server(app)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = run 8081 (logStdoutDev app)
