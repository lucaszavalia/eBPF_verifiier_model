{-# LANGUAGE GADTs #-}
module TypeModel where

import Data.Word
import Data.List
import EBPFparser


--Formal model of the EBPFL's type system
data EBPFtype = EBPFfunction (EBPFflattype -> EBPFflattype) | EBPFflat EBPFflattype | EBPFempty
data EBPFflattype = EP EBPFpointer | EB EBPFbasic deriving(Eq, Show)
data EBPFpointer = EBPFptr EBPFregion Integer | EBPFpkt | EBPFmap deriving(Eq, Show)
data EBPFregion = EBPFstack | EBPFcontext |EBPFshared deriving(Eq, Show)
data EBPFbasic = EBPFu8 Word8 | EBPFu16 Word16 | EBPFu32 Word32 | EBPFu64 Word64 deriving (Eq, Show)

--Types for region are separated from EBPFpointer types for reasons
data RegionType = StackType | PacketType | SharedType | ContextType deriving(Eq, Show)

--Contexts
data Cell = Cell { name :: String, ebpfType :: EBPFtype }
data TypeContext = TypeContext [Cell]
data Triplet = Triplet { var :: String, eBPFType :: EBPFtype, index :: Integer }
data RegionStructure = RegionStructure [Triplet]
data Region = Region { regionType :: RegionType, upperBound :: Integer, lowerBound :: Integer, structure :: RegionStructure }
data RegionContext = RegionContext [Region]
data Simulation = Simulation { typeContext :: TypeContext, regionContext :: RegionContext, instructions :: [EBPFinstruction] }
data Instant = Instant { tContext :: TypeContext, rContext :: RegionContext, instruction :: EBPFinstruction }
data Judgement = Judgement { inst :: EBPFinstruction, typ :: EBPFtype }

--instances of Show and Eq
instance Show EBPFtype where
   show (EBPFfunction fn) = "EBPF function"
   show (EBPFflat fl) = show fl

instance Eq EBPFtype where
   (==) (EBPFfunction fn1) (EBPFfunction fn2) = True
   (==) (EBPFflat fl1) (EBPFflat fl2) = fl1 == fl2

instance Eq Cell where 
   (==) = (\(Cell name1 type1) (Cell name2 type2) -> (name1 == name2) && (type1 == type2))

instance Eq Triplet where
   (==) = (\(Triplet name1 type1 index1) (Triplet name2 type2 index2) -> (name1 == name2) && (type1 == type2) && (index1 == index2))

instance Eq Region where
   (==) = (\(Region rho1 u1 ell1 c1) (Region rho2 u2 ell2 c2) -> (rho1 == rho2) && (u1 == u2) && (ell1 == ell2))

--configure type context functions
(|:) :: String -> EBPFtype -> Cell --make cell out of name and type
(|:) = (\a b -> Cell a b)
infixl 7 |:

(|-) :: TypeContext -> Cell -> Bool --is (name,type) in context?
(|-) = (\(TypeContext x) y -> y `elem` x) 
infixl 6 |-

(|~) :: TypeContext -> String -> EBPFtype --get type by name
(|~) _ [] = EBPFempty
(|~) (TypeContext ((Cell n t):tc')) na'
   | n == na'  = t
   | otherwise = (TypeContext $ tail tc') |~ na'

(~|) :: TypeContext -> Cell -> TypeContext --type context update 
(~|) = (\x (Cell a b) -> typeContextUpdate a b x)

(|:|) :: Cell -> TypeContext -> TypeContext --type context push front
(|:|) = (\x (TypeContext y) -> TypeContext (x:y))

(|++) :: TypeContext -> TypeContext -> TypeContext --type context append
(|++) = (\(TypeContext c1) (TypeContext c2) -> TypeContext (c1 ++ c2))

typeContextUpdate :: String -> EBPFtype -> TypeContext -> TypeContext
typeContextUpdate na ty tc
   | tc |- na |: ty    = tc
   | (tc |~ na) /= ty  = (Cell na ty) |:| (removeString na tc)
   | otherwise         = (Cell na ty) |:| tc
   where
      removeString :: String -> TypeContext -> TypeContext
      removeString _ (TypeContext []) = TypeContext []
      removeString na' (TypeContext ((Cell n t):tc'))
         | na' == n  = removeString na' (TypeContext tc')
         | otherwise = (Cell n t) |:| removeString na' (TypeContext tc')

typeContextUnion :: TypeContext -> TypeContext -> TypeContext
typeContextUnion = (\(TypeContext tc1) (TypeContext tc2) -> TypeContext (tc1 `union` tc2)) -- talk to ameer about this


weight :: EBPFtype -> Integer
weight (EBPFflat (EB (EBPFu8 _))) = 1 
weight (EBPFflat (EB (EBPFu16 _))) = 2
weight (EBPFflat (EB (EBPFu32 _))) = 4
weight (EBPFflat (EB (EBPFu64 _))) = 8
weight (EBPFflat (EP _)) = 8
weight (EBPFempty) = 8
weight (EBPFfunction _) = 8

regionContextUpdate :: String -> EBPFtype -> Integer -> RegionContext -> RegionContext
regionContextUpdate = (\v tau n (RegionContext delta) -> RegionContext (fmap (\(Region a b c d) -> if a == StackType then stackUpdate v tau n (Region a b c d) else Region a b c d) delta))
   where
      stackUpdate :: String -> EBPFtype -> Integer -> Region -> Region
      stackUpdate v tau n stk = if inBounds tau n stk == True
         then if hasOverlap tau n stk == True
            then updateOverlap v tau n stk (overlapSet v tau n stk)
            else updateNoOverlap v tau n stk
         else stk
      inBounds :: EBPFtype -> Integer -> Region -> Bool
      inBounds = (\x y (Region a b c d) -> if y + c + weight(x) <= b then True else False)
      hasOverlap :: EBPFtype -> Integer -> Region -> Bool
      hasOverlap = (\x y z -> (inTripletBounds y z) || (inTripletBounds (y + weight(x)) z))
      inTripletBounds :: Integer -> Region -> Bool
      inTripletBounds = (\j k -> j `elem` foldl (++) [] (getTripletBounds k))
      getTripletBounds :: Region -> [[Integer]]
      getTripletBounds = (\(Region a b c (RegionStructure d)) -> fmap (\(Triplet v tau i) -> [i..(i + weight(tau))]) d)
      updateOverlap :: String -> EBPFtype -> Integer -> Region -> RegionStructure -> Region
      updateOverlap = (\v tau n (Region a b c (RegionStructure d)) (RegionStructure oset) -> Region a b c (RegionStructure ((d \\ oset) `union` [(Triplet v tau n)])))
      overlapSet :: String -> EBPFtype -> Integer -> Region -> RegionStructure
      overlapSet = (\v tau n (Region a b c d) -> fixTriplets $ filterTriplets tau n (expandTripletBounds d))
      expandTripletBounds :: RegionStructure -> [(String, EBPFtype, [Integer])]
      expandTripletBounds = (\(RegionStructure d) -> fmap (\(Triplet v tau i) -> (v, tau, [i..(weight(tau)+i)])) d)
      filterTriplets :: EBPFtype -> Integer -> [(String, EBPFtype, [Integer])] -> [(String, EBPFtype, [Integer])]
      filterTriplets = (\tau n trips -> filter (\(a, b, c) -> c `intersect` [n..(n+weight(tau))] /= []) trips)
      fixTriplets :: [(String, EBPFtype, [Integer])] -> RegionStructure
      fixTriplets = (\x -> RegionStructure $ (\(a,b,c) -> Triplet a b c) <$>(\(a, b, c) -> (a, b, (head c))) <$> x)
      updateNoOverlap :: String -> EBPFtype -> Integer -> Region -> Region
      updateNoOverlap = (\v tau n (Region a b c (RegionStructure d)) -> Region a b c (RegionStructure (d `union` [(Triplet v tau n)])))

--type checking
ebpfRegionContextInit :: RegionContext
ebpfRegionContextInit = RegionContext [ 
   (Region StackType 512 0 (RegionStructure []))
   , (Region ContextType 613 513 (RegionStructure []))
   ] --figure out where to context begin and end

ebpfTypeContextInit :: TypeContext
ebpfTypeContextInit = TypeContext [
   (Cell"r1" (EBPFflat (EP (EBPFptr EBPFcontext 0))))
   , (Cell "r10" (EBPFflat (EP (EBPFptr EBPFstack 512)))) 
   ]

{-typeCheck :: Simulation -> [Judgement]
typeCheck (Simulation gamma delta insts) = (\x -> analyzeInstruction gamma delta x) <$> insts

analyzeInstruction :: TypeContext -> RegionContext -> EBPFinstruction -> Judgement
analyzeInstruction gamma' delta' inst-}
   
