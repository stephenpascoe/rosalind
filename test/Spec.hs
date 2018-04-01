{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import Problems

main :: IO ()
main = hspec $ do
  describe "Problem DNA" $ do
    it "Solves the example problem" $ do
      dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" `shouldBe` Right "20 12 17 21"

  describe "Problem RNA" $ do
    it "Solves the example problem" $ do
      rna "GATGGAACTTGACTACGTAAATT" `shouldBe` Right "GAUGGAACUUGACUACGUAAAUU"

  describe "Problem REVC" $ do
    it "Solves the example problem" $ do
      revc "AAAACCCGGT" `shouldBe` Right "ACCGGGTTTT"

  describe "Problem FIB" $ do
    it "Solves the example problem" $ do
      fib "5 3" `shouldBe` Right "19"

  describe "Problem gc" $ do
    it "Solves the example problem" $ do
      gc gcInput `shouldBe` Right "Rosalind_0808\n60.91954"



gcInput = ">Rosalind_6404\n\
           \CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\n\
           \TCCCACTAATAATTCTGAGG\n\
           \>Rosalind_5959\n\
           \CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\n\
           \ATATCCATTTGTCAGCAGACACGC\n\
           \>Rosalind_0808\n\
           \CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\n\
           \TGGGAACCTGCGGGCAGTAGGTGGAAT"
