-- chapter 1

main = interact wordCount
  where wordCount input = show (length (lines input)) ++ "\n"
