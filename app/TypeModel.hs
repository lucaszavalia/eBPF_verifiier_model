{-# LANGUAGE GADTs #-}
module TypeModel where

import Data.Word
import Data.List

--Formal model of the EBPFL's type system
data EBPFtype = EBPFfunction (EBPFflattype -> EBPFflattype) | EBPFflattype
data EBPFflattype = EBPFpointer | EBPFbasic | EBPFempty  deriving(Eq, Show)
data EBPFpointer = EBPFptr EBPFregion Int | EBPFpkt | EBPFmap deriving(Eq, Show)
data EBPFregion = EBPFstack | EBPFcontext |EBPFshared deriving(Eq, Show)
data EBPFbasic = Word8 | Word16 | Word32 | Word64 deriving (Eq, Show)

--Types for region are separated from EBPFpointer types for reasons
data RegionType = StackType | PacketType | SharedType | ContextType deriving(Eq, Show)

--Contexts
type TypeContext = [(String, EBPFtype)]
type RegionStructure = [(String, EBPFtype, (Int, Int))]
type RegionContextStructure = (EBPFregion, Int, Int, RegionStructure)
type RegionContext = [RegionContextStructure]

typeContextUpdate :: String -> EBPFtype -> TypeContext -> TypeContext
typeContextUpdate = \nam typ con -> (nam,typ):con

typeContextUnion :: TypeContext -> TypeContext -> TypeContext
typeContextUnion = \tc1 tc2 -> tc1 `union` tc2

regionContextUpdate :: EBPFtype -> Int -> RegionContext -> RegionContext
regionContextUpdate = if searchRegionContextStructure /= Nothing  
   where
   findRegionContext :: RegionContext -> Int -> Maybe RegionContextStructure
   findRegionContext = \x n -> 
   searchRegionContext :: RegionContext -> Int -> Bool
   searchRegionContext = \x n -> fmap (\y -> testRegionStructure y n) x
   testRegionContextStructure :: RegionContextStructure -> Int -> Bool
   testRegionContextStructure = \(w, x, y, z) n -> n <= x && y <= n
   unJust :: Maybe a -> a
   unJust = (\Just x -> x)
