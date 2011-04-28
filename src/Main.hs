-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
 main

) where

import Trie
import Data.Hashable
import Data.Array

main = do
    word_data <- readFile "../dictionary.txt"
    putStrLn $ show $ hash Trie { valid = True, children = listArray ('a','z') (replicate 26 Empty) }
    let trie = newTrieWithWordList $ lines word_data
    let trie' = trie `seq` compressTrie trie
    putStrLn $ show $ isWordInTrie trie' "bat"
    return ()





