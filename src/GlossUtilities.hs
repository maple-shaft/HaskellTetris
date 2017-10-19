{-# LANGUAGE TemplateHaskell #-}
module GlossUtilities where

import Data.ByteString as B
import Data.FileEmbed
import Data.Maybe
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import Codec.BMP
import Data.Word

bgTransparencyByte :: Word8
bgTransparencyByte = 150

bgFileData :: B.ByteString
bgFileData = $(embedFile "res/bg.bmp")

menuFileData :: B.ByteString
menuFileData = $(embedFile "res/menu.bmp")

createPicture :: Bool -> Int -> Int -> B.ByteString -> Picture
createPicture isTransparent x y fileData = bitmapOfBMP cBmp
  where rawData = B.drop 0x36 fileData
        rawData32 = convert24BitTo32BitBMP isTransparent rawData
        cBmp = packRGBA32ToBMP32 x y rawData32

-- | Intersperses Alpha Transparency bits to create a 32 bit bitmap
convert24BitTo32BitBMP :: Bool -> B.ByteString -> B.ByteString
convert24BitTo32BitBMP isTransparent x
         | (not $ B.null x) && (B.head x == 0x00) = (convert24BitTo32BitBMP isTransparent $ B.tail x)
         | not $ B.null x = B.append (B.snoc (B.take 3 x) transparencyByte) (convert24BitTo32BitBMP isTransparent $ B.drop 3 x)
         | otherwise = B.empty
  where transparencyByte = if isTransparent then 255 else bgTransparencyByte
        

-- | Create a list of translated Picture objects to fill the background
backgroundPic :: IO Picture
backgroundPic = return $ pictures $ trans <$> [-5..5] <*> [-5..5]
  where trans x y = translate (x * 164) (y * 155) $ tile
        tile = createPicture False 164 155 bgFileData

menuPic :: Picture
menuPic = pictures $ trans <$> [-30..30] <*> [-150..150]
  where tile = createPicture True 5 2 menuFileData
        trans x y = translate (x * 5) (y * 2) $ tile