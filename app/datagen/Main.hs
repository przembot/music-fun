{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Parsec (runParser)

import AI.HNN.FF.Network

import System.IO (isEOF)

import Data.Text.IO as T (getLine)
import System.Directory (doesFileExist)

import Lib


-- | Lifts two chords to the NN sample
generateTestCase :: Chord -> Chord -> Sample Double
generateTestCase (Chord a) (Chord b) =
  (markSelected a) --> (markSelected b)

-- | Every two neighboor chords are good samples to feed the network
genSamples :: [Chord] -> Samples Double
genSamples [] = []
genSamples (_:[]) = []
genSamples (a:(b:as)) = generateTestCase a b : genSamples (b:as)

fileName :: String
fileName = "network.nn"

main :: IO ()
main = do
  netExists <- doesFileExist fileName
  net <- if netExists
            then loadNet fileName
            else createNetwork 36 [18,22,30] 36

  input <- genSamples <$> readChords
  let
    newnet = trainNTimes 5000 0.4 tanh tanh' net input
  saveNet fileName newnet


readChords :: IO [Chord]
readChords = isEOF >>= \eof -> if eof then return [] else
  T.getLine >>= \line ->
    case runParser oneChord "" "" line of
        (Left err) -> print err >> readChords
        (Right c) -> fmap (c:) readChords
