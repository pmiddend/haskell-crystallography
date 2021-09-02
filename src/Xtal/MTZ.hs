{-# LANGUAGE OverloadedStrings #-}

module Xtal.MTZ
  ( parseMtz,
    mtzHeader,
    mtzHistory,
    mtzReflections,
    mtzLocateHeader,
    mtzLocateHeaders,
    mtzTitle,
    mtzDataset,
    MtzDatasetInfo,
    MtzFile
  )
where

import Control.Applicative (Alternative (many), pure, (*>), (<*), (<*>), (<|>))
import Control.Monad (mzero, replicateM, (>>=))
import Data.Attoparsec.Binary (anyWord32le)
import Data.Attoparsec.ByteString (IResult (Done, Fail, Partial), Parser, anyWord8, endOfInput, parse, parseOnly, skipWhile, string, take, takeWhile1)
import qualified Data.Attoparsec.Text as AT
import Data.Binary (decode, decodeOrFail)
import Data.Binary.Get (getFloathost, runGetOrFail)
import Data.Bool ((&&))
import Data.ByteString (ByteString, dropWhileEnd, getContents, length, unpack)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Eq ((/=), (==))
import Data.Function (id, (.))
import Data.Functor (($>), (<$>), (<&>))
import Data.Int (Int)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as BoxedVector
import qualified Data.Vector.Unboxed as UnboxedVector
import Data.Word (Word8)
import Debug.Trace (trace, traceShowId)
import Text.Show (Show (show))
import Prelude (Bool (True), Char, Double, Either (Left, Right), Float, Functor (fmap), Maybe (Just, Nothing), MonadFail (fail), const, either, fromIntegral, read, undefined, (*), (+), (-))

data MtzNcolInfo = MtzNcolInfo
  { mtzNcolColumnCount :: Int,
    mtzNcolReflCount :: Int,
    mtzNcolBatchCount :: Int
  }
  deriving (Show)

data MtzCellParameters = MtzCellParameters
  { mtzCellA :: Double,
    mtzCellB :: Double,
    mtzCellC :: Double,
    mtzCellAlpha :: Double,
    mtzCellBeta :: Double,
    mtzCellGamma :: Double
  }
  deriving (Show)

data MtzColInfo = MtzColInfo
  { mtzColName :: Text.Text,
    mtzColType :: Char,
    mtzColMinValue :: Double,
    mtzColMaxValue :: Double,
    mtzColDatasetId :: Int
  }
  deriving (Show)

data MtzColsrcInfo = MtzColsrcInfo
  { mtzColsrcName :: Text.Text,
    mtzColsrcMetadata :: Text.Text
  }
  deriving (Show)

data MtzSymInfInfo = MtzSymInfInfo
  { mtzNumberOfSymmetryOperations :: Int,
    mtzNumberOfPrimitiveOperations :: Int,
    mtzLatticeType :: Char,
    mtzSpaceGroupNumber :: Int,
    mtzSpaceGroupName :: Text.Text,
    mtzPointGroupName :: Text.Text
  }
  deriving (Show)

data MtzResoInfo = MtzResoInfo
  { mtzResolutionMinimum :: Double,
    mtzResolutionMaximum :: Double
  }
  deriving (Show)

newtype MtzSortInfo = MtzSortInfo [Int] deriving (Show)

type MtzSymmetryOperation = Text.Text

type MtzValmInfo = Maybe Double

data MtzProjectInfo = MtzProjectInfo
  { mtzProjectDatasetId :: Int,
    mtzProjectName :: Text.Text
  }
  deriving (Show)

data MtzCrystalInfo = MtzCrystalInfo
  { mtzCrystalDatasetId :: Int,
    mtzCrystalName :: Text.Text
  }
  deriving (Show)

data MtzDatasetInfo = MtzDatasetInfo
  { mtzDatasetDatasetId :: Int,
    mtzDatasetName :: Text.Text
  }
  deriving (Show)

data MtzDcellInfo = MtzDcellInfo
  { mtzDcellDatasetId :: Int,
    mtzDcellParameters :: MtzCellParameters
  }
  deriving (Show)

data MtzDwavelInfo = MtzDwavelInfo
  { mtzDwavelDatasetId :: Int,
    mtzDwavelWavelength :: Double
  }
  deriving (Show)

data MtzHeaderEntry
  = MtzVersion ByteString
  | MtzTitle ByteString
  | MtzNcol MtzNcolInfo
  | MtzSort MtzSortInfo
  | MtzCell MtzCellParameters
  | MtzSymInf MtzSymInfInfo
  | MtzSymm MtzSymmetryOperation
  | MtzReso MtzResoInfo
  | MtzValm MtzValmInfo
  | MtzCol MtzColInfo
  | MtzColsrc MtzColsrcInfo
  | MtzNdif Int
  | MtzProject MtzProjectInfo
  | MtzCrystal MtzCrystalInfo
  | MtzDataset MtzDatasetInfo
  | MtzDcell MtzDcellInfo
  | MtzDwavel MtzDwavelInfo
  | MtzEnd
  | MtzHist Int
  | MtzUnknownHeader ByteString
  deriving (Show)

mtzHeaderIsTitle :: MtzHeaderEntry -> Maybe ByteString
mtzHeaderIsTitle (MtzTitle n) = Just n
mtzHeaderIsTitle _ = Nothing

mtzHeaderIsHist :: MtzHeaderEntry -> Maybe Int
mtzHeaderIsHist (MtzHist n) = Just n
mtzHeaderIsHist _ = Nothing

mtzHeaderIsNcol :: MtzHeaderEntry -> Maybe MtzNcolInfo
mtzHeaderIsNcol (MtzNcol n) = Just n
mtzHeaderIsNcol _ = Nothing

mtzHeaderIsColInfo :: MtzHeaderEntry -> Maybe MtzColInfo
mtzHeaderIsColInfo (MtzCol n) = Just n
mtzHeaderIsColInfo _ = Nothing

bsToText :: ByteString -> Text.Text
bsToText = Text.pack . BS8.unpack

applyTextParser :: AT.Parser a -> ByteString -> Parser a
applyTextParser p s = either fail pure (AT.parseOnly p (bsToText s))

textSkipWs :: AT.Parser ()
textSkipWs = AT.skipWhile (== ' ')

ncolInfoParser :: AT.Parser MtzNcolInfo
ncolInfoParser =
  MtzNcolInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.decimal)

cellParamParser :: AT.Parser MtzCellParameters
cellParamParser =
  MtzCellParameters
    <$> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)

sortInfoParser :: AT.Parser MtzSortInfo
sortInfoParser = MtzSortInfo <$> replicateM 5 (textSkipWs *> AT.decimal)

colsrcParser :: AT.Parser MtzColsrcInfo
colsrcParser = do
  label <- AT.takeWhile (/= ' ')
  metadata <- AT.skipSpace *> AT.takeText
  pure (MtzColsrcInfo (if label == "M/ISYM" then "M_ISYM" else label) metadata)

projectParser :: AT.Parser MtzProjectInfo
projectParser =
  MtzProjectInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> (Text.dropWhileEnd (== ' ') <$> AT.takeText))

crystalParser :: AT.Parser MtzCrystalInfo
crystalParser =
  MtzCrystalInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> (Text.dropWhileEnd (== ' ') <$> AT.takeText))

datasetParser :: AT.Parser MtzDatasetInfo
datasetParser =
  MtzDatasetInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> (Text.dropWhileEnd (== ' ') <$> AT.takeText))

dcellParser :: AT.Parser MtzDcellInfo
dcellParser =
  MtzDcellInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> cellParamParser)

dwavelParser :: AT.Parser MtzDwavelInfo
dwavelParser =
  MtzDwavelInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.double)

symInfParser :: AT.Parser MtzSymInfInfo
symInfParser =
  MtzSymInfInfo
    <$> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.anyChar)
    <*> (textSkipWs *> AT.decimal)
    <*> (textSkipWs *> AT.char '\'' *> AT.takeWhile1 (/= '\'') <* AT.char '\'')
    <*> (textSkipWs *> AT.takeText)

resoParser :: AT.Parser MtzResoInfo
resoParser =
  MtzResoInfo
    <$> (textSkipWs *> AT.double)
    <*> (textSkipWs *> AT.double)

colParser :: AT.Parser MtzColInfo
colParser = do
  label <- textSkipWs *> AT.takeWhile1 (/= ' ')
  type_ <- textSkipWs *> AT.anyChar
  min <- textSkipWs *> AT.double
  max <- textSkipWs *> AT.double
  datasetId <- textSkipWs *> AT.decimal
  pure (MtzColInfo (if type_ == 'Y' && label == "M/ISYM" then "M_ISYM" else label) type_ min max datasetId)

valmParser :: AT.Parser MtzValmInfo
valmParser = (AT.string "NAN" $> Nothing) <|> (Just <$> AT.double)

mtzHeaderEntryParser =
  let rawHeader :: ByteString -> Parser ByteString
      rawHeader title = string (traceShowId title <> " ") *> take (80 - length title - 1)
      strippedHeader title = dropWhileEnd (== 32) <$> rawHeader title
      subParsedHeader title parser ctor = ((rawHeader title >>= applyTextParser parser) <&> ctor)
   in (MtzVersion <$> strippedHeader "VERS")
        <|> (MtzTitle <$> strippedHeader "TITLE")
        <|> subParsedHeader "NCOL" ncolInfoParser MtzNcol
        <|> subParsedHeader "CELL" cellParamParser MtzCell
        <|> subParsedHeader "SORT" sortInfoParser MtzSort
        <|> subParsedHeader "SYMINF" symInfParser MtzSymInf
        <|> (MtzSymm <$> bsToText <$> strippedHeader "SYMM")
        <|> subParsedHeader "RESO" resoParser MtzReso
        <|> subParsedHeader "VALM" valmParser MtzValm
        <|> subParsedHeader "COLUMN" colParser MtzCol
        <|> subParsedHeader "COLSRC" colsrcParser MtzColsrc
        <|> (MtzNdif . read . BS8.unpack <$> rawHeader "NDIF")
        <|> subParsedHeader "PROJECT" projectParser MtzProject
        <|> subParsedHeader "CRYSTAL" crystalParser MtzCrystal
        <|> subParsedHeader "DATASET" datasetParser MtzDataset
        <|> subParsedHeader "DCELL" dcellParser MtzDcell
        <|> subParsedHeader "DWAVEL" dwavelParser MtzDwavel
        <|> (rawHeader "END" $> MtzEnd)
        <|> subParsedHeader "MTZHIST" (textSkipWs *> AT.decimal) MtzHist

--        <|> MtzUnknownHeader <$> take 80

magicHeaderLocation = 20

mtzRecordSize = 80

data MtzFile = MtzFile
  { mtzReflections :: BoxedVector.Vector (UnboxedVector.Vector Float),
    mtzHeader :: [MtzHeaderEntry],
    mtzHistory :: [ByteString]
  }

type MtzHeaderGetter a = MtzHeaderEntry -> Maybe a

mtzLocateHeader :: MtzHeaderGetter a -> MtzFile -> Maybe a
mtzLocateHeader f = listToMaybe . mapMaybe f . mtzHeader

mtzLocateHeaders :: MtzHeaderGetter a -> MtzFile -> [a]
mtzLocateHeaders f = mapMaybe f . mtzHeader

mtzTitle :: MtzHeaderGetter Text.Text
mtzTitle (MtzTitle n) = Just (bsToText n)
mtzTitle _ = Nothing

mtzDataset :: MtzHeaderGetter MtzDatasetInfo
mtzDataset (MtzDataset n) = Just n
mtzDataset _ = Nothing

mtzParser = do
  -- Byte 1 to 4
  string "MTZ "
  -- Byte 4 to 8
  headerLocationInEntries <- anyWord32le
  -- for some reason, we have to subtract exactly 4 bytes, or one
  -- entry here, to get the real location. Maybe the four bytes are a
  -- "header start" word32.
  let headerLocationInBytes = (headerLocationInEntries - 1) * 4
  -- Byte 9
  realAndComplexFormat <- anyWord8
  -- Byte 10
  integerAndCharacterFormat <- anyWord8
  -- Byte 11
  unused1 <- anyWord8
  -- Byte 12
  unused2 <- anyWord8
  -- Manual says we start at byte 21 with the data, but actually,
  -- records are always 80 bytes, and we have the first record filled
  -- with some metadata, so we skip ahead to the next record at byte
  -- 80
  let currentLocation = 12
  unused3 <- take (mtzRecordSize - currentLocation)
  rawData <- take (fromIntegral headerLocationInBytes - mtzRecordSize)
  headerEntries <- many mtzHeaderEntryParser
  let locateHeader pred = listToMaybe (mapMaybe pred headerEntries)
      ncols = locateHeader mtzHeaderIsNcol
  historyEntries <- replicateM (fromMaybe 0 (locateHeader mtzHeaderIsHist)) (dropWhileEnd (== 32) <$> take 80)
  string "MTZENDOFHEADERS"
  skipWhile (== 32)
  endOfInput
  case locateHeader mtzHeaderIsNcol of
    Nothing -> fail "NCOL not found"
    Just (MtzNcolInfo colCount reflCount _batchCount) -> do
      case runGetOrFail (replicateM (colCount * reflCount) getFloathost) (BSL.fromStrict rawData) of
        Left (_, _, errorMessage) -> fail ("error decoding floats: " <> errorMessage)
        Right (_, _consumedBytes, floatVector) -> do
          let unfolder :: UnboxedVector.Vector Float -> (UnboxedVector.Vector Float, UnboxedVector.Vector Float)
              unfolder = UnboxedVector.splitAt colCount
              unfoldedVector :: BoxedVector.Vector (UnboxedVector.Vector Float)
              unfoldedVector = BoxedVector.unfoldrExactN reflCount unfolder (UnboxedVector.fromList floatVector)
          pure (MtzFile unfoldedVector headerEntries historyEntries)

parseMtz = parseOnly mtzParser
