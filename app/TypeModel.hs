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
type Simulation = (TypeContext, RegionContext, [EBPFinstruction])
type Instant = (TypeContext, RegionContext, EBPFinstruction)
type Judgement = (EBPFinstruction, EBPFtype)

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
typeContextUnion = (\tc1 tc2 -> tc1 `union` tc2)

(:::) :: Name -> EBPFtype -> Cell
(:::) = (\a, b -> (a,b))
infixl 7 (:::)

(|-) :: TypeContext -> Cell -> Bool
(|-) = (\x, y -> x `elem` y) 
infixl 6 (|-)

(:-) :: TypeContext -> Name -> EBPFtype
(:-) = (\x, y -> if lookup y x == Nothing then EBPFempty else (\(Just z) -> z) $ lookup y x)

(-:) :: TypeContext -> Cell -> TypeContext 
(-:) = (\x, (Cell a b) -> typeContextUpdate a b x)

weight :: EBPFtype -> Integer
   weight typ
      | (Word8 typ)     = 1
      | (Word16 typ)    = 2
      | (Word32 typ)    = 4
      | (Word64 typ)    = 8
      | otherwise       = 8

regionContextUpdate :: String -> EBPFtype -> Integer -> RegionConext -> RegionContext
regionContextUpdate = (\v, tau, n, delta -> 
   fmap (\(Region a b c d) ->
      if a == StackType 
         then Region a b c (stackUpdate d)
         else Region a b c d)
   delta)
   where
      stackUpdate :: String -> EBPFtype -> Integer -> Region -> Region
      stackUpdate v tau n stk = if inBounds tau n stk == True
         then if hasOverlap tau n stk == True
            then updateOverlap v tau n stk (overlapSet v tau n stk)
            else updateNoOverlap v tau n stk
         else stk
      inBounds :: EBPFtype -> Integer -> Region -> Bool
      inBounds = (\x, y, (Region a b c d) -> if y + c <= y + c + weight(y) <= y then True else False)
      hasOverlap :: EBPFtype -> Integer -> Region -> Bool
      hasOverlap = (\x, y, z -> (inTripletBounds y z) || (inTripletBounds (y + weight(x))))
      inTripletBounds :: Integer -> Region -> Bool
      inTripletBounds = (\j, k -> foldl (&&) True (fmap (\w -> j `elem` w (getTripletBounds k))))
      getTripletBounds :: Region -> [[Int]]
      getTripletBounds = (\(Region a b c d) -> fmap (\(Triplet v tau i) -> [i..(i + weight(tau))]) d)
      updateOverlap :: EBPFtype -> Integer -> Region -> Regionstructure -> Region
      updateOverlap = (\v, tau, n, oset, (Region a b c d) -> Region a b c d ((d \\ oset) `union` [(Triplet v tau n)]))
      overlapSet :: String -> EBPFtype -> Integer -> Region -> RegionStructure
      overlapSet = (\v, tau, n, (Region a b c d) -> fixTriplets $ filterTriplets tau n (expandTripletBounds d))
      expandTripletBounds :: RegionStructure -> [(Name, EBPFtype, [Int])]
      expandTripletBounds = (\d -> fmap (\(Triplet v tau i) -> (v, tau, [i..(weight(tau)+i)])) d)
      filterTriplets :: EBPFtype -> Integer -> [(Name, EBPFtype, [Int])] -> [(Name, EBPFtype, [Int])]
      filterTriplets = (\tau, n, trips -> filter (\(a, b, c) -> c `intersect` [n..(n+weight(tau))]) trips)
      fixTriplets :: [(Name, EBPFtype, [Int])] -> RegionStructure
      fixTriplets = (\x -> fmap (\(a, b, c) -> (a, b, (head c))) x)
      updateNoOverlap String -> EBPFtype -> Integer -> Region -> Region
      updateNoOverlap = (\v, tau, n, (Region a b c d) -> Region a b c (d `union` [(Triplet v tau n)]))

--type checking
ebpfRegionContextInit :: RegionContext
ebpfRegionContextInit = [ (Region StackType 512 0 []) ] --figure out where to context begin and end

ebpfTypeContextInit :: TypeContext
ebpfTypeContextInit = [ ("r1", (EBPFptr EBPFcontext 0)), ("r10", (EBPFptr EBPFstack 0)) ]

{-typeCheck :: Simulation -> [Judgement]  
typeCheck (g,d,[]) = []
typeCheck (g,d,i) = (analyzeInstant (g,d,(head i))):(typeCheck (g,d,(tail i)))
   where
      analyzeInstant :: Instant -> Judgement
      analyzeInstant (gamma, delta, (EBPFinstruction (ArithmeticJumpOpcode ic so co) sr de os im))
         | co == BPF_MOV && so == BPF_X   =  -}
