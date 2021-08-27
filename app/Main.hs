{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List((!!))
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative (pure)
import Control.Monad (forM_, (=<<))
import Data.ByteString (getContents)
import Data.Function ((.), ($))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Database.MySQL.Simple (ConnectInfo(..), connect, defaultConnectInfo, query_, Only(Only))
import System.Environment (getArgs)
import System.IO (IO)
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
        { connectHost = args !! 0,
          connectUser = args !! 1,
          connectPassword = args !! 2,
          connectDatabase = args !! 3
        }
  xs <- query_ conn "SELECT crystal_id FROM Crystals"
  forM_ xs $ \(Only crystalId) -> putStrLn crystalId

mtzmain :: IO ()
mtzmain = do
  stdin <- getContents
  case parseMtz stdin of
    Left e -> putStrLn (pack e)
    Right v -> mtzdmp v

--forM_ (mtzReflections v) print
