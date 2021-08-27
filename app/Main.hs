{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (pure)
import Control.Monad (forM_, (=<<))
import Data.ByteString (getContents)
import Data.Function ((.))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
import Database.MySQL.Base (ciDatabase, ciPassword, ciUser, connect, defaultConnectInfo, query_)
import System.Environment (getArgs)
import System.IO (IO)
import qualified System.IO.Streams as Streams
import Xtal.MTZ (MtzFile, mtzDataset, mtzHeader, mtzHistory, mtzLocateHeader, mtzLocateHeaders, mtzReflections, mtzTitle, parseMtz)
import Prelude (Either (Left, Right), Foldable (length), Show (show), print, undefined)

showText :: Show a => a -> Text
showText = pack . show

mtzdmp :: MtzFile -> IO ()
mtzdmp mtz = do
  putStrLn " * Title:"
  putStrLn ""
  putStrLn (" " <> fromMaybe "no title" (mtzLocateHeader mtzTitle mtz))
  putStrLn ""
  putStrLn (" * Number of Datasets = " <> showText (length (mtzLocateHeaders mtzDataset mtz)))

main :: IO ()
main = do
  args <- getArgs
  conn <-
    connect
      defaultConnectInfo
        { ciUser = (args !! 0),
          ciPassword = (args !! 1),
          ciDatabase = (args !! 2)
        }
  (defs, is) <- query_ conn "SELECT crystal_id FROM Crystals"
  print =<< Streams.toList is

mtzmain :: IO ()
mtzmain = do
  stdin <- getContents
  case parseMtz stdin of
    Left e -> putStrLn (pack e)
    Right v -> mtzdmp v

--forM_ (mtzReflections v) print
