module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getBool
    , getDouble
    , getObject
    , getArray
    , isNull
    ) where

    data JValue = JString String
                | JNumber Double
                | JBool Bool
                | JNull
                | JObject [(String, JValue)]
                | JArray [JValue]
                  deriving (Eq, Ord, Show)

    getString :: JValue -> Maybe String
    getString (JString x) = Just x
    getString _           = Nothing

    getInt :: JValue -> Maybe Integer
    getInt (JNumber n) = Just (truncate n)
    getInt _           = Nothing

    getBool :: JValue -> Maybe Bool
    getBool (JBool b) = Just b
    getBool _       = Nothing

    getDouble :: JValue -> Maybe Double
    getDouble (JNumber d) = Just d
    getDouble _           = Nothing

    getObject :: JValue -> Maybe [(String, JValue)]
    getObject (JObject o) = Just o
    getObject _           = Nothing

    getArray :: JValue -> Maybe [JValue]
    getArray (JArray a) = Just a
    getArray _          = Nothing

    isNull :: JValue -> Bool
    isNull = (== JNull)
