module Main where
import qualified New
import qualified Network.Wai.Handler.Warp  as Warp

main :: IO ()
main = Warp.run 8080 New.app
