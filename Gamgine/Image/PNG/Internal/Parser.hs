{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Gamgine.Image.PNG.Internal.Parser where

import Text.Parsec.Prim
import Text.Parsec.Combinator

import Data.Word
import Data.Bits
import Numeric (showHex)

import qualified Gamgine.Image.PNG.Internal.LBS as LBS
import Gamgine.Image.PNG.Internal.LBS (LBS)
import qualified Data.ByteString.Lazy as LB

type Parser = Parsec LBS ()

word8 :: (Stream LBS m Word8) => Word8 -> ParsecT LBS u m Word8
word8 = satisfy . (==)

word16 :: (Stream LBS m Word8) => Word16 -> ParsecT LBS u m Word16
word16 w = (word8 hi >> word8 lo >> return w) <?> "0x" ++ showHex w ""
 where
   hi = fromIntegral (w `shiftR` 8)
   lo = fromIntegral w

word32 :: (Stream LBS m Word8) => Word32 -> ParsecT LBS u m Word32
word32 w = (word16 hi >> word16 lo >> return w) <?> "0x" ++ showHex w ""
 where
   hi = fromIntegral (w `shiftR` 16)
   lo = fromIntegral w

satisfy :: (Stream LBS m Word8) => (Word8 -> Bool) -> ParsecT LBS u m Word8
satisfy f = tokenPrim (\c -> "0x" ++ showHex c "")
                      (\pos _ _ -> pos)
                      (\c -> if f c then Just c else Nothing)

anyWord8 :: (Stream LBS m Word8) => ParsecT LBS u m Word8
anyWord8 = anyToken

anyWord16 :: (Stream LBS m Word8) => ParsecT LBS u m Word16
anyWord16 = do
  hi <- anyWord8
  lo <- anyWord8
  return $ (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

anyWord32 :: (Stream LBS m Word8) => ParsecT LBS u m Word32
anyWord32 = do
  hi <- anyWord16
  lo <- anyWord16
  return $ (fromIntegral hi `shiftL` 16) .|. fromIntegral lo

string :: (Stream LBS m Word8) => LBS -> ParsecT LBS u m LBS
string s = mapM_ word8 (LBS.unpack s) >> return s

block :: (Stream LBS m Word8) => Int -> ParsecT LBS u m LBS
block size = do  -- count size anyWord8 >>= return . LB.pack
  i <- getInput
  let (s,r) = LBS.splitAt (fromIntegral size) i
  setInput r
  return s

allowedValues :: (a -> Parser a) -> [(a,b)] -> Parser b
allowedValues fn = choice . map (\(val,res) -> fn val >> return res)

parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile p fname
    = do input <- LBS.readFile fname
         return $ case runP p () fname input of
                    Left err  -> Left (show err)
                    Right x   -> Right x
