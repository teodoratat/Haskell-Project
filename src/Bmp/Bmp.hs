{-# LANGUAGE OverloadedStrings #-}

module Bmp.Bmp (Pixel, pixel, ImgRow (..), BmpImg (..), createBitmap) where

import Control.DeepSeq
import Control.Monad.Writer
import Data.Bits
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Word as W

data Pixel = Pixel {r :: W.Word8, g :: W.Word8, b :: W.Word8} deriving (Show)

pixel = Pixel

instance NFData Pixel where
  rnf px = r px `seq` g px `seq` b px `seq` ()

newtype ImgRow = ImgRow [Pixel]

newtype BmpImg = BmpImg [ImgRow]

data FileHeader = FileHeader
  { header :: W.Word16,
    fileSize :: W.Word32,
    reserved1 :: W.Word16,
    reserved2 :: W.Word16,
    offset :: W.Word32
  }

data FileInfoHeader = FileInfoHeader
  { headerSize :: W.Word32,
    bitmapWidth :: W.Word16,
    bitmapHeight :: W.Word16,
    nrColorPlanes :: W.Word16,
    bitsPerPixel :: W.Word16
  }

roundUp :: Int -> Int -> Int
roundUp n m
  | m >= n = m
  | otherwise = ((n + m - 1) `div` m) * m

getFileHeaderBytes :: FileHeader -> BSB.Builder
getFileHeaderBytes hd =
  execWriter wr
  where
    wr :: Writer BSB.Builder ()
    wr = do
      tell $ BSB.word16BE $ header hd
      tell $ BSB.word32LE $ fileSize hd
      tell $ BSB.word16LE $ reserved1 hd
      tell $ BSB.word16LE $ reserved2 hd
      tell $ BSB.word32LE $ offset hd

getDibHeaderBytes :: FileInfoHeader -> BSB.Builder
getDibHeaderBytes hd =
  execWriter wr
  where
    wr :: Writer BSB.Builder ()
    wr = do
      tell $ BSB.word32LE $ headerSize hd
      tell $ BSB.word16LE $ bitmapWidth hd
      tell $ BSB.word16LE $ bitmapHeight hd
      tell $ BSB.word16LE $ nrColorPlanes hd
      tell $ BSB.word16LE $ bitsPerPixel hd

fileHeaderSize :: W.Word32
fileHeaderSize = 2 + 4 + 2 + 2 + 4

dibHeaderSize :: W.Word32
dibHeaderSize = 4 + 2 + 2 + 2 + 2

bsToW16 :: BS.ByteString -> W.Word16
bsToW16 bs =
  BS.foldl (\v b -> (v `shiftL` 8) .|. (fromIntegral b)) 0 $ BS.take 2 bs

createFileHeader :: FileInfoHeader -> FileHeader
createFileHeader dibHeader =
  FileHeader
    { header = bsToW16 "BM",
      fileSize = dataSize + fileHeaderSize + dibHeaderSize,
      reserved1 = 0,
      reserved2 = 0,
      offset = fileHeaderSize + dibHeaderSize
    }
  where
    width = bitmapWidth dibHeader
    rowSize = fromIntegral $ width * ((bitsPerPixel dibHeader) `div` 8)
    alignedRowSize = roundUp rowSize 4
    dataSize :: W.Word32
    dataSize = (fromIntegral alignedRowSize) * fromIntegral (bitmapHeight dibHeader)

createDibHeader :: BmpImg -> FileInfoHeader
createDibHeader (BmpImg img) =
  FileInfoHeader
    { headerSize = dibHeaderSize,
      bitmapWidth = fromIntegral $ length firstRow,
      bitmapHeight = fromIntegral $ length img,
      nrColorPlanes = 1,
      bitsPerPixel = 3 * 8
    }
  where
    (ImgRow firstRow) = head img

getImgDataBytes :: BmpImg -> BSB.Builder
getImgDataBytes (BmpImg img) =
  let getRowBytes :: ImgRow -> BSB.Builder
      getRowBytes (ImgRow row) =
        padRow (mconcat [mconcat $ map BSB.word8 [b px, g px, r px] | px <- row]) rowLen
        where
          rowLen = length row

      padRow :: BSB.Builder -> Int -> BSB.Builder
      padRow row rowLen =
        let paddedSize = roundUp rowLen 4
         in if paddedSize > rowLen
              then row `mappend` mconcat (replicate (fromIntegral (paddedSize - rowLen)) (BSB.int8 0))
              else row
   in mconcat $ map getRowBytes img

createBitmap :: BmpImg -> BSB.Builder
createBitmap img =
  let dibHeader = createDibHeader img
      fileHeader = createFileHeader dibHeader
      wr :: Writer BSB.Builder ()
      wr = do
        tell $ getFileHeaderBytes fileHeader
        tell $ getDibHeaderBytes dibHeader
        tell $ getImgDataBytes img
   in execWriter wr