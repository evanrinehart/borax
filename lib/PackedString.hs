module PackedString where

{- A string is any number of characters between " characters.
The characters are packed into adjacent objects (lvalues
sequential) and terminated with the character '*e'.  The
rvalue of the string is the lvalue of the object containing
the first character.  See section 8.0 for library functions
used to manipulate strings in a machine independent
fashion. -}

import Data.Char

-- 5 ascii characters fit in a 36-bit word
charsPerWord = 5
bitsPerChar  = 7
base         = 2 ^ bitsPerChar

-- if *e appears in the string, truncate string there (includes *e)
-- otherwise append *e to the end
terminate :: String -> String
terminate [] = "\EOT"
terminate ('\EOT':_) = "\EOT"
terminate (c:cs) = c : terminate cs

unterminate :: [Char] -> [Char]
unterminate "" = ""
unterminate ('\EOT':_) = ""
unterminate (c:cs) = c : unterminate cs

packChars :: [Char] -> Int
packChars = foldl1 (\a x -> a*base + x) . map ord

unpackChars :: Int -> [Char]
unpackChars = reverse . f where
  f 0 = []
  f i = let (q,r) = i `divMod` base in chr r : f q

-- pack characters into numbers up to charsPerWord at a time
packString :: String -> [Int]
packString str = case splitAt charsPerWord str of
  ([],[])   -> []
  (cs,rest) -> packChars cs : packString rest

unpackString :: [Int] -> String
unpackString [] = ""
unpackString (n:ns) = unpackChars n ++ unpackString ns where
