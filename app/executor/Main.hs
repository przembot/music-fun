{-# LANGUAGE OverloadedStrings #-}
module Main where

import Numeric.LinearAlgebra
import AI.HNN.FF.Network

import Text.Parsec

import Data.List

import Control.Monad
import Data.Text.IO as T (getLine)

import Lib

getChord :: IO Chord
getChord =
  T.getLine >>= \line ->
    case runParser oneChord "" "" line of
           (Left err) -> print err >> getChord
           (Right c) -> return c

main :: IO ()
main = do
  net <- loadNetwork "network.nn" :: IO (Network Double)
  forever $ do
    chord <- markSelected . unChord <$> getChord
    print . sortBy (\(_,v1) (_,v2) -> v2 `compare` v1)
          . filter (\(_, val) -> val >= 0.5)
          . zip allChords
          . toList $ (output net tanh chord)
