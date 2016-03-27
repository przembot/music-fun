module Lib where

import Data.Maybe (fromMaybe)
import Data.Char (toUpper, toLower, isLower)
import Text.Parsec
import Data.Text (Text)
import Data.List (elemIndex)

import Codec.Compression.Zlib     (compress, decompress)
import Data.Binary                (Binary(..), encode, decode)
import Foreign.Storable           (Storable)
import AI.HNN.FF.Network
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Numeric.LinearAlgebra

-- | Basic chord identifier
-- | There are 14 basic sounds
-- | plus 3 kinds of chords (min, maj, 7)
-- | min 0, max 12*3 = 36
newtype Chord = Chord { unChord :: Int }
  deriving Eq

basicTones :: [String]
basicTones = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a:_) !? 0 = Just a
(_:as) !? n = as !? (n-1)

basicTone :: Chord -> Maybe String
basicTone (Chord s) = basicTones !? (s `div` 12)

allChords :: [String]
allChords = basicTones >>= \t -> [t, fmap toUpper t, t++"7"]

chordInfo :: Chord -> Maybe String
chordInfo ch@(Chord s) = basicTone ch >>= \bas ->
  case (s `mod` 3) of
    0 -> return bas -- minor
    1 -> return (fmap toUpper bas) -- major
    _ -> return (bas ++ "7") -- 7

instance Show Chord where
  show = fromMaybe "unknown note" . chordInfo

type Parser a = Parsec Text Text a

anyKey :: Parser Char
anyKey = oneOf "cdefgabCDEFGAB"

halfToneSign :: Parser Char
halfToneSign = oneOf "#$"

sevenSign :: Parser Char
sevenSign = char '7'

-- | It's like in round table, i. e. -1 position
-- from the first chair is also the last one
normalize :: Int -> Int
normalize n
  | n < 0 = n + 12
  | n > 11 = n - 12
  | otherwise = n

generate :: Int -> (Int -> a) -> [a]
generate n f = take n $ f <$> iterate (+1) 0

markSelected :: Int -> Vector Double
markSelected s = fromList $ generate 36 (\i -> if i == s then 1 else 0)

oneChord :: Parser Chord
oneChord = do
  tone <- anyKey
  halfTone <- optionMaybe halfToneSign
  seven <- optionMaybe sevenSign
  let
    isMin = isLower tone
    res = elemIndex ((toLower tone):[]) basicTones >>= (\toneId ->
      case halfTone of
        (Just '$') -> return . normalize $ toneId - 1
        (Just '#') -> return . normalize $ toneId + 1
        _ -> return toneId)
        >>= (\tId -> Just . Chord $ case seven of
                    (Just _) -> (tId*3) + 2
                    _ -> if isMin
                            then (tId*3)
                            else (tId*3 + 1))
  case res of
     (Just c) -> return c
     _ -> unexpected "Cannot parse chord"


-- Forcing lazy bytestring to be evaluated, so file IO operations
-- will be closed before saving to the same file
loadNet :: (Storable a, Element a, Binary a) => FilePath -> IO (Network a)
loadNet fp = decode . decompress . BL.fromStrict <$> B.readFile fp
{-# INLINE loadNet #-}

saveNet :: (Storable a, Element a, Binary a) => FilePath -> Network a -> IO ()
saveNet fp = BL.writeFile fp . compress . encode
{-# INLINE saveNet #-}
