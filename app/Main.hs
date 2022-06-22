{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (pure)
import Control.Monad (forM_, void, (=<<))
import Data.ByteString (getContents, readFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (head, (!!))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, drop, pack, take, unpack)
import Data.Text.IO (putStrLn)
import System.Environment (getArgs)
import System.IO (IO)
import Xtal.MTZ
import Prelude (Either (Left, Right), Foldable (length), Show (show), print, undefined)

showText :: Show a => a -> Text
showText = pack . show

mtzdmp :: MtzFile -> IO ()
mtzdmp mtz = do
  putStrLn " * Title:"
  putStrLn ""
  putStrLn (" " <> fromMaybe "no title" (mtzLocateHeader mtzTitle mtz))
  putStrLn ""
  let datasets = mtzLocateHeaders mtzDataset mtz
  putStrLn (" * Number of Datasets = " <> showText (length datasets))
  forM_ datasets $ \ds -> putStrLn $ mtzDatasetName ds <> " (ID " <> showText (mtzDatasetDatasetId ds) <> ")"
  putStrLn ""
  putStrLn " * HISTORY for current MTZ file :"
  putStrLn ""
  forM_ (mtzHistory mtz) $ \h -> putStrLn (" " <> bsToText h)

-- clustermain :: IO ()
-- clustermain = do
--   args <- getArgs
--   conn <-
--     connect
--       defaultConnectInfo
--         { connectHost = head args,
--           connectUser = args !! 1,
--           connectPassword = args !! 2,
--           connectDatabase = args !! 3
--         }
--   xs <- query_ conn "SELECT mtz_path FROM Data_Reduction"
--   let renderTime = renderDuration
--       progressStyle = defStyle { stylePostfix = elapsedTime renderTime <> msg " " <> remainingTime renderTime "N/A" <> msg " " <> percentage }
--   progress <- newProgressBar progressStyle 10 (Progress 0 (length xs) ())
--   let nonNullPaths :: [Text]
--       nonNullPaths = mapMaybe fromOnly xs
--       rewrite :: Text -> Text
--       rewrite = ("sshfs/" <>) . drop 23
--       action fileName = do
--         fileContents <- readFile (unpack fileName)
--         case parseMtz fileContents of
--           Left e -> putStrLn ("ERROR:   " <> fileName <> ": " <> pack e)
--           Right v -> pure ()
--         incProgress progress 1
--           --Right v -> putStrLn ("SUCCESS: " <> fileName)
--   parallel_ (action . rewrite <$> nonNullPaths)
--   stopGlobalPool
--   --forM_ (rewrite <$> nonNullPaths) $ \f -> do
--   --forM_ xs $ \(Only mtzPath) -> putStrLn (fromMaybe "NULL" mtzPath)

main :: IO ()
main = do
  stdin <- getContents
  case parseMtz stdin of
    Left e -> putStrLn (pack e)
    Right v -> mtzdmp v

--forM_ (mtzReflections v) print
