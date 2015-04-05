{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Gamgine.Image.PNG.Internal.LBS
    ( LBS(..)
    , unpack
    , unpackToString
    , splitAt
    , readFile
    , concat
    , pack
    , null
    , head
    , tail
    ) where

import Prelude hiding (splitAt, readFile, concat, null, head, tail)
import Text.Parsec.Prim (Stream(..))
import Data.Word
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))

newtype LBS = LBS { unLSB :: LB.ByteString } deriving (Show)

instance (Monad m) => Stream LBS m Word8 where
    uncons = return . uncons'

uncons' :: LBS -> Maybe (Word8, LBS)
uncons' (LBS bs) = case LB.uncons bs of
    Just (w, bs) -> Just (w, LBS bs)
    Nothing      -> Nothing

unpack :: LBS -> [Word8]
unpack = LB.unpack . unLSB

unpackToString :: LBS -> String
unpackToString = C.unpack . unLSB

splitAt :: Int64 -> LBS -> (LBS, LBS)
splitAt idx (LBS bs) =
    let (bs1, bs2) = LB.splitAt idx bs
	in (LBS bs1, LBS bs2)

readFile :: FilePath -> IO LBS
readFile fp = LBS <$> LB.readFile fp

concat :: [LBS] -> LBS
concat = LBS . LB.concat . map unLSB

pack :: [Word8] -> LBS
pack = LBS . LB.pack

null :: LBS -> Bool
null = LB.null . unLSB

head :: LBS -> Word8
head = LB.head . unLSB

tail :: LBS -> LBS
tail = LBS . LB.tail . unLSB
