{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Text (Text, pack)

import Text.Parsec (runParser)

import Lib

toChord :: Text -> Chord
toChord input =
  case runParser oneChord "" "" input of
    (Left _) -> Chord (-1)
    (Right c) -> c

main :: IO ()
main = hspec $ do
  describe "parser validator" $ do
    it "parser should parse all possible chords" $ do
      unChord . toChord . pack <$> allChords `shouldBe` [0..35]
