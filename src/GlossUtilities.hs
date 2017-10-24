{-# LANGUAGE TemplateHaskell #-}
module GlossUtilities where

import Data.ByteString as B
import Data.FileEmbed
import Data.Maybe
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import Codec.BMP
import Codec.Picture.Png
import Data.Word

import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Storable        (unsafeToForeignPtr)

bgTransparencyByte :: Word8
bgTransparencyByte = 150

bgFileData :: B.ByteString
bgFileData = $(embedFile "res/bg.bmp")

menuFileData :: B.ByteString
menuFileData = $(embedFile "res/menu.png")

startButtonFileData :: B.ByteString
startButtonFileData = $(embedFile "res/start.png")

exitButtonFileData :: B.ByteString
exitButtonFileData = $(embedFile "res/exit.png")

createPictureFromPNG :: B.ByteString -> Picture
createPictureFromPNG b = fromJust mpic
  where dyn = decodePng b
        mpic = case (dyn) of
                  Right d -> fromDynamicImage d
                  _       -> Nothing

startButtonPic :: Picture
startButtonPic = createPictureFromPNG startButtonFileData

exitButtonPic :: Picture
exitButtonPic = createPictureFromPNG exitButtonFileData

menuPic :: Picture
menuPic = createPictureFromPNG menuFileData

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

------------
        
fromDynamicImage :: DynamicImage -> Maybe Picture
fromDynamicImage (ImageY8 img)     = Just $ fromImageY8 img
fromDynamicImage (ImageYA8 img)    = Just $ fromImageYA8 img
fromDynamicImage (ImageRGB8 img)   = Just $ fromImageRGB8 img
fromDynamicImage (ImageRGBA8 img)  = Just $ fromImageRGBA8 img
fromDynamicImage (ImageYCbCr8 img) = Just $ fromImageYCbCr8 img
fromDynamicImage (ImageRGBF _)     = Nothing
fromDynamicImage (ImageYF _)       = Nothing

-- | O(N) conversion from 'PixelRGBA8' image to gloss 'Picture', where N is the number of pixels.
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 (Image { imageWidth = w, imageHeight = h, imageData = id }) =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr id
{-# INLINE fromImageRGBA8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelRGB8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageRGB8 :: Image PixelRGB8 -> Picture
fromImageRGB8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageRGB8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelY8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageY8 :: Image Pixel8 -> Picture
fromImageY8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageY8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelYA8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageYA8 :: Image PixelYA8 -> Picture
fromImageYA8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageYA8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelYCbCr8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageYCbCr8 :: Image PixelYCbCr8 -> Picture
fromImageYCbCr8 = fromImageRGB8 . convertImage
{-# INLINE fromImageYCbCr8 #-}

