module Doc where

data Doc =
  Nil |
  Newline |
  Tab |
  Text String |
  Doc :<> Doc

flatten :: Doc -> String
flatten d = f d "" where
  f Nil rest         = rest
  f Newline rest     = '\n' : rest
  f Tab rest         = '\t' : rest
  f (Text str) rest  = str ++ rest
  f (d1 :<> d2) rest = f d1 (f d2 rest)

nil :: Doc
nil = Nil

newline :: Doc
newline = Newline

tab :: Doc
tab = Tab

text :: String -> Doc
text = Text

instance Semigroup Doc where
  (<>) = (:<>)

instance Monoid Doc where
  mempty = nil

showN :: Int -> Doc
showN = text . show

showSum :: [Doc] -> Doc
showSum = joinDocs (text " + ")

showCommasSpace :: [Doc] -> Doc
showCommasSpace = joinDocs (text ", ")

showCommas :: [Doc] -> Doc
showCommas = joinDocs (text ",")

showIndent :: String -> Doc -> Doc
showIndent header body =
  let alignSpace = replicate (max 1 (8 - length header)) ' '
  in text header <> text alignSpace <> body

joinDocs :: Doc -> [Doc] -> Doc
joinDocs sep []     = nil
joinDocs sep [d]    = d
joinDocs sep (d:ds) = d <> sep <> joinDocs sep ds
