module Prettify where
    import SimpleJSON

    data Doc = Empty
             | Char Char
             | Text String
             | Line
             | Concat Doc Doc
             | Union Doc Doc
               deriving (Show, Eq)

    empty :: Doc
    empty = Empty

    text :: String -> Doc
    text "" = Empty
    text s  = Text s

    double :: Double -> Doc
    double d = text (show d)

    line :: Doc
    line = Line

    (<>) :: Doc -> Doc -> Doc
    Empty <> y = y
    x <> Empty = x
    x <> y     = x `Concat` y

    char :: Char -> Doc
    char = Char

    hcat :: [Doc] -> Doc
    hcat  = fold (<>)

    fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
    fold = (`foldr` empty)

    fsep :: [Doc] -> Doc
    fsep = fold (</>)

    (</>) :: Doc -> Doc -> Doc
    x </> y = x <> softline <> y

    softline :: Doc
    softline = group line

    group :: Doc -> Doc
    group x = flatten x `Union` x

    flatten :: Doc -> Doc
    flatten (x `Concat` y) = flatten x `Concat` flatten y
    flatten Line           = Char ' '
    flatten (x `Union` _)  = flatten x
    flatten other          = other

    punctuate :: Doc -> [Doc] -> [Doc]
    punctuate _   []     = []
    punctuate _   [d]    = [d]
    punctuate sep (d:ds) = (d <> sep) : punctuate sep ds
    
