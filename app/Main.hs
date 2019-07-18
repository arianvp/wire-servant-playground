module Main where
import qualified Scratchpad
import qualified Network.Wai.Handler.Warp  as Warp

main :: IO ()
main = Warp.run 8080 Scratchpad.app
