module Nullable where
    data Optional a = Present a
                    | Absent
                      deriving (Show)
