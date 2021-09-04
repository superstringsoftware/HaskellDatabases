{-# LANGUAGE OverloadedStrings, 
             OverloadedLists,
             RecordWildCards,
             RankNTypes
#-}


module Muon.Data.Column where

import Data.Int
import Data.Word

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

import Data.Text
import Data.Binary
import Data.Vector.Binary -- Binary instances for Vectors!!! 

data GenericColumn = CInt    (U.Vector Int64) 
                   | CDouble (U.Vector Double) 
                   | CBool   (U.Vector Bool)
                   | CWord   (U.Vector Word64)
                   | CText   (V.Vector Text)
                   deriving Show

data TableColumn = TableColumn {
    colName :: Text,
    col  :: GenericColumn
} deriving Show

data Table = Table {
    tableName :: Text,
    columns :: V.Vector TableColumn
} deriving Show

applyFunc :: (forall v a. (G.Vector v a, Show a) => v a -> b) -> GenericColumn -> b
applyFunc f (CInt v) = f v
applyFunc f (CDouble v) = f v
applyFunc f (CBool v) = f v
applyFunc f (CWord v) = f v
applyFunc f (CText v) = f v

prettyPrintCT :: Table -> IO ()
prettyPrintCT (Table {tableName = tn, columns = cols}) = putStrLn (unpack "--------------------------------------------------------------")  >> 
    putStrLn (unpack tn) >> putStrLn (unpack "--------------------------------------------------------------")  >>
    G.mapM_ fff cols where
        fff (TableColumn {..})= putStrLn ((unpack colName) ++ "\t\t\t") >> applyFunc (G.mapM_ print) col
 

-- TESTING ---------------
cint :: U.Vector Int64
cint = G.fromList [1,2,43,234,23,412,24,12,4,252,1,2,43,234,23,412,24,12,4,252]

cdouble :: U.Vector Double
cdouble = G.fromList [13,23,433,23.4,233,4.12,22,1.2,4,-25.2, 1,2,43,234,23,412,24,12,4,252]

creg :: V.Vector Text
creg = G.fromList ["EMEA", "NA", "EMEA", "RoW", "NA", "RoW", "EMEA", "EMEA", "NA", "RoW", "EMEA", "NA", "RoW", "NA", "RoW", "NA", "RoW", "EMEA", "NA", "EMEA"]

ctable :: Table
ctable = Table {
    tableName = "Test Table",
    columns = G.fromList [
        TableColumn { colName = "Orders #",  col = CInt cint },
        TableColumn { colName = "Revenue $", col = CDouble cdouble },
        TableColumn { colName = "Region", col = CText creg }
    ]
}


-- Binary instance for Generic Column
instance Binary GenericColumn where
    put (CInt v) =      do put (0 :: Word8) >> put v
    put (CDouble v) =   do put (1 :: Word8) >> put v
    put (CWord v) =     do put (2 :: Word8) >> put v
    put (CBool v) =     do put (3 :: Word8) >> put v
    put (CText v) =     do put (4 :: Word8) >> put v

    get = do t <- get :: Get Word8
             case t of
                0 -> get >>= return . CInt 
                1 -> get >>= return . CDouble
                2 -> get >>= return . CWord
                3 -> get >>= return . CBool
                4 -> get >>= return . CText