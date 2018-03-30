{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import Problems

main :: IO ()
main = hspec $ do
  describe "Problem DNA" $ do
    it "Solves the example problem" $ do
      dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" `shouldBe` "20 12 17 21"

  describe "Problem RNA" $ do
    it "Solves the example problem" $ do
      rna "GATGGAACTTGACTACGTAAATT" `shouldBe` "GAUGGAACUUGACUACGUAAAUU"

  describe "Problem REVC" $ do
    it "Solves the example problem" $ do
      revc "AAAACCCGGT" `shouldBe` "ACCGGGTTTT"
