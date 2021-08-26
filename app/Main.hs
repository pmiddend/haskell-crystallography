module Main where

import Prelude(print, Either (Left, Right), putStrLn, Show (show), Foldable (length))
import System.IO(IO)
import Control.Applicative(pure)
import Control.Monad(forM_)
import Data.ByteString(getContents)
import Xtal.MTZ(parseMtz, mtzHeader, mtzHistory, mtzReflections)
import Data.Monoid((<>))

main :: IO ()
main = do
  stdin <- getContents
  case parseMtz stdin of
    Left e -> putStrLn e
    Right v -> --putStrLn ("mtz read successful: " <> show (mtzHeader v) <> " header(s), " <> show (length (mtzHistory v)) <> " history(s): " <> show (mtzHistory v))
      forM_ (mtzReflections v) print
