module PutJSON where
    import Data.List (intercalate)
    import SimpleJSON

    renderJValue :: JValue -> String

    renderJValue (JString s)   = show s
    renderJValue (JNumber n)   = show n
    renderJValue (JBool True)  = "true"
    renderJValue (JBool False) = "false"
    renderJValue JNull         = "null"

    renderJValue (JArray a)    = "[" ++ pairs a ++ "]"
        where pairs [] = ""
              pairs vs = intercalate "," (map renderJValue vs)

    renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
        where pairs [] = ""
              pairs vs = intercalate "," (map renderPair vs)
              renderPair (key, value) = show key ++ renderJValue value

    putJValue :: JValue -> IO ()
    putJValue v = putStrLn (renderJValue v)
