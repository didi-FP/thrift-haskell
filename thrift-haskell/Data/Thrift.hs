class Thrift a where
    fromTValue :: TValue -> Either String a
    toTValue :: a -> TValue
