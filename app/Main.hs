{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude(print, Either (Left, Right), Show (show), Foldable (length), undefined)
import Data.Text.IO(putStrLn)
import Data.Text(pack, Text)
import System.IO(IO)
import Control.Applicative(pure)
import Control.Monad(forM_, (=<<))
import Data.ByteString(getContents)
import Xtal.MTZ(parseMtz, mtzHeader, mtzHistory, mtzReflections, MtzFile, mtzTitle, mtzLocateHeader, mtzLocateHeaders, mtzDataset)
import Data.Monoid((<>))
import Data.Maybe(fromMaybe)
import Data.Function((.))
import Database.MySQL.Base(connect, query_, ciUser, ciPassword, ciDatabase, defaultConnectInfo)
import qualified System.IO.Streams as Streams

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
  conn <- connect defaultConnectInfo { ciUser = "root", ciPassword = "root", ciDatabase = "SARS_COV_2_v2" }
  (defs, is) <- query_ conn "SELECT crystal_id FROM Crystals"
  print =<< Streams.toList is

mtzmain :: IO ()
mtzmain = do
  stdin <- getContents
  case parseMtz stdin of
    Left e -> putStrLn (pack e)
    Right v -> mtzdmp v
      --forM_ (mtzReflections v) print
