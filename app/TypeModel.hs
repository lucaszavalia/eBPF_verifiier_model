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
type Name = String
type Cell = (Name, EBPFtype)
type TypeContext = [Cell]
type Triplet = (Name, EBPFtype, Int)
type RegionStructure = [Triplet]
type Region = (RegionType, Int, Int, RegionStructure)
type RegionContext = [Region]

instance Eq Cell where
   (==) x y = (fst x == fst y) && (snd x == snd y)

instance Eq Triplet where
   (==) x y = (get1 x == get1 y) && (get2 x == get2 y) && (get3 x == get3 y)
   where
      get1 = (\(a,b,c)->a)
      get2 = (\(a,b,c)->b)
      get3 = (\(a,b,c)->c)

instance Eq Region where
   (==) x y = (get1 x == get1 y) && (get2 x == get2 y) && (get3 x == get3 y)
   where
      get1 = (\(a,b,c)->a)
      get2 = (\(a,b,c)->b)
      get3 = (\(a,b,c)->c)


typeContextUpdate :: Name -> EBPFtype -> TypeContext -> TypeContext
typeContextUpdate na ty tc
   | na `elem` fmap fst tc && ty `elem` fmap snd tc      = tc
   | na `elem` fmap fst tc && ty `notElem` fmap snd tc   = (na, ty):(removeName na tc)
   | otherwise                                           = (na, ty):tc
   where
      removeName :: Name -> TypeContext -> TypeContext
      removeName _ [] = []
      removeName na' ((n,t):tc')
         | na' == n  = removeName tc'
         | otherwise = (n,t):(removeName tc')

typeContextUnion :: TypeContext -> TypeContext -> TypeContext
typeContextUnion = \tc1 tc2 -> tc1 `union` tc2

weight :: EBPFtype -> Integer
weight (Word8 x) = 1
weight (Word16 x) = 2
weight (Word32 x) = 4
weight (Word64 x) = 8

regionContextUpdate :: Integer -> EBPFtype -> RegionContext -> RegionContext
regionContextUpdate n t rc = if inBounds n == True
                             then {-not sure how to make this case faithful to the documentation-} 
                             else rc



