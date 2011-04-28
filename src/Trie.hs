
module Trie (
 Trie(..),
 newTrieWithWordList,
 isWordInTrie,
 compressTrie
) where

import Data.Hashable
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.HashMap as H
import Data.Maybe

data Trie = Empty | Trie { valid :: Bool, children :: A.Array Char Trie } deriving (Show, Ord, Eq)

instance Hashable Trie where
    hash Empty = 42
    hash a = (hash $ valid a) `combine` hashOfChildren
        where hashOfChildren = foldr (\(i, e) acc -> acc `combine` (hash i) `combine` (hash e) ) 0 $ A.assocs $ children a


newTrieWithWordList :: [String] -> Trie
newTrieWithWordList [] = Empty
newTrieWithWordList wordList = Trie {
    valid = not . null $ filter null wordList, --if there is an item in the list that has length 0
    children = A.listArray ('a','z') $ [newTrieWithWordList . map tail . filter (L.isPrefixOf [letter]) $ wordList | letter <- ['a'..'z']]
} --radix sort the wordList and remove first letter

isWordInTrie :: Trie -> String -> Bool
isWordInTrie Empty _ = False
isWordInTrie trie [] = valid trie
isWordInTrie trie (x:xs) = isWordInTrie ( children trie A.! x ) xs

compressTrie :: Trie -> Trie
compressTrie trie = fst $ process trie H.empty

process :: Trie -> H.HashMap Trie Trie -> (Trie, H.HashMap Trie Trie)
process Empty hashMap = (Empty, hashMap)
process trie hashMap = (Trie {
    valid = valid trie,
    children = A.listArray ('a','z') trieChildren
}, hashMap')
    where
        (hashMap', trieChildren) = L.mapAccumL mapWithAccum hashMap (A.elems $ children trie)

mapWithAccum :: H.HashMap Trie Trie -> Trie -> (H.HashMap Trie Trie, Trie)
mapWithAccum hashMap trie = if isInHashMap
    then (hashMap', fromJust $ H.lookup trie' hashMap')
    else (H.insert trie' trie' hashMap', trie)
    where isInHashMap = H.member trie' hashMap'
          (trie', hashMap') = process trie hashMap
