{-# LANGUAGE TypeSynonymInstances #-}
module EBPFparser where

import Data.Word
import Data.Bits
import Data.List
import qualified Data.Elf as ELF

data InstructionClass = BPF_LD
                   | BPF_LDX
                   | BPF_ST
                   | BPF_STX
                   | BPF_ALU
                   | BPF_JMP
                   | BPF_JMP32
                   | BPF_ALU64
                   deriving (Show, Eq)

data AJCode = BPF_ADD
          | BPF_SUB
          | BPF_MUL
          | BPF_DIV
          | BPF_OR
          | BPF_AND
          | BPF_LSH
          | BPF_RSH
          | BPF_NEG
          | BPF_MOD
          | BPF_XOR
          | BPF_MOV
          | BPF_ARSH
          | BPF_END
          | BPF_JA
          | BPF_JEQ
          | BPF_JGT
          | BPF_JGE
          | BPF_JSET
          | BPF_JNE
          | BPF_JSGT
          | BPF_JSGE
          | BPF_CALL
          | BPF_EXIT
          | BPF_JLT
          | BPF_JLE
          | BPF_JSLT
          | BPF_JSLE
          deriving (Show, Eq) 

data SourceModifier = BPF_K
                  | BPF_X
                  deriving (Show, Eq)

data SizeModifier = BPF_W
               | BPF_H
               | BPF_B
               | BPF_DW
               deriving (Show, Eq)

data ModeModifier = BPF_IMM
               | BPF_ABS
               | BPF_IND
               | BPF_MEM
               | BPF_ATOMIC
               deriving (Show, Eq)

data Opcode = ArithmeticJumpOpcode { -- 3 bits, 1 bit, 4 bits
                 iclass :: Maybe InstructionClass,
                 source :: Maybe SourceModifier,
                 code :: Maybe AJCode
              } |
              LoadStoreOpcode { -- 3 bits, 2 bits, 3 bits
                 iClass :: Maybe InstructionClass, 
                 size :: Maybe SizeModifier, 
                 mode :: Maybe ModeModifier
              } deriving (Eq)

instance Show Opcode where
   show (ArithmeticJumpOpcode ic so co) = "Arithmetic/Jump Opcode {\n"
                                        ++ "\tInstruction Class: " ++ (show ic) ++ "\n"
                                        ++ "\tSource Modifier: " ++ (show so) ++ "\n"
                                        ++ "\tOperation: " ++ (show co) ++ "\n"
                                        ++ "}\n"
   show (LoadStoreOpcode ic si mo) = "Load/Store Opcode {\n"
                                   ++ "\tInstruction Class: " ++ (show ic) ++ "\n"
                                   ++ "\tSize Modifier: " ++ (show si) ++ "\n"
                                   ++ "\tMode Modifier: " ++ (show mo) ++ "\n"
                                   ++ "}\n"

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 deriving (Eq)

instance Show Register where
   show r
      | r == R0   = "r0"
      | r == R1   = "r1"
      | r == R2   = "r2"
      | r == R3   = "r3"
      | r == R4   = "r4"
      | r == R5   = "r5"
      | r == R6   = "r6"
      | r == R7   = "r7"
      | r == R8   = "r8"
      | r == R9   = "r9"
      | r == R10  = "r10"
      | otherwise = "r?"


data PreEBPFinstruction = PreEBPFinstruction {
                         opcode :: Maybe Opcode,
                         src :: Maybe Register,
                         dest :: Maybe Register,
                         offset :: Integer,
                         immediate :: Integer
                       } deriving (Eq)

instance Show PreEBPFinstruction where
   show (PreEBPFinstruction op sr de os im) = "EBPF Instruction:\n"
                                           ++ (show op)
                                           ++ "Additional Data {\n"
                                           ++ "\tSource Register: " ++ (show sr) ++ "\n"
                                           ++ "\tDestination Register: " ++ (show de) ++ "\n"
                                           ++ "\tOffset: " ++ (show os) ++ "\n"
                                           ++ "\tImmediate: " ++ (show im) ++ "\n"
                                           ++ "}\n\n"

data EBPFinstruction = EBPFinstruction {
                        operationCode :: Opcode,
                        sourceReg :: Register,
                        destinationReg :: Register,
                        offsetVal :: Integer,
                        immediateVal :: Integer
                      } | EmptyInstruction deriving (Eq)

instance Show EBPFinstruction where
   show (EBPFinstruction (ArithmeticJumpOpcode ic so co) sr de os im)
      | ic == Just BPF_ALU   && so == Just BPF_X       = (show de) 
                                                         ++ " " 
                                                         ++ (getOperation co) 
                                                         ++ " (u32)" 
                                                         ++ (show sr) 
                                                         ++ ";\n"
      | ic == Just BPF_ALU64 && so == Just BPF_X       = (show de) 
                                                         ++ " " 
                                                         ++ (getOperation co) 
                                                         ++ " " 
                                                         ++ (show sr) 
                                                         ++ ";\n"
      | ic == Just BPF_ALU   && so == Just BPF_K       = (show de) 
                                                         ++ " " 
                                                         ++ (getOperation co) 
                                                         ++ " (u32)" 
                                                         ++ (show im) 
                                                         ++ ";\n"
      | ic == Just BPF_ALU64 && so == Just BPF_K       = (show de) 
                                                         ++ " " 
                                                         ++ (getOperation co) 
                                                         ++ " " 
                                                         ++ (show im) 
                                                         ++ ";\n"
      | ic == Just BPF_JMP32 && co /= Just BPF_CALL    = "if "
                                                         ++ (show de)
                                                         ++ " "
                                                         ++ (getComparison co)
                                                         ++ " "
                                                         ++ (show sr)
                                                         ++ " goto PC+"
                                                         ++ (show os)
                                                         ++ ";\n"
      | ic == Just BPF_JMP32 && co == Just BPF_CALL    = "call "
                                                         ++ (show im)
                                                         ++ ";\n"
      | ic == Just BPF_JMP   && (notSpecial co)        = "if "
                                                         ++ (show de)
                                                         ++ " "
                                                         ++ (getComparison co)
                                                         ++ " "
                                                         ++ (show sr)
                                                         ++ " goto PC+"
                                                         ++ (show os)
                                                         ++ ";\n"
      | ic == Just BPF_JMP   && co == Just BPF_EXIT    = "exit;\n"
      | ic == Just BPF_JMP   && co == Just BPF_JA      = "jmp "
                                                         ++ "PC+"
                                                         ++ (show os)
                                                         ++ ";\n"
      | otherwise                                      = "Non-Standard Arithmetic/Jump Instruction;\n" 
      where
         getOperation = (\x -> if x /= Nothing 
                               then if (lookupOperation $ (\(Just y) -> y) x) /= Nothing 
                                    then (\(Just y) -> y) $ lookupOperation  $ (\(Just y) -> y) x 
                                    else "" 
                               else ""
                        )
         lookupOperation = (\x -> lookup x operationStringMap)
         notSpecial = (\x -> x /= Just BPF_EXIT || x /= Just BPF_JA)
         getComparison =  (\x -> if x /= Nothing 
                                 then if (lookupComparison $ (\(Just y) -> y) x) /= Nothing
                                      then (\(Just y) -> y) $ lookupComparison $ (\(Just y) -> y) x
                                      else "" 
                                 else ""
                          )
         lookupComparison = (\x -> lookup x comparisonStringMap)
   show (EBPFinstruction (LoadStoreOpcode ic si mo) sr de os im)
      | ic == Just BPF_LDX   = (show de) 
                               ++ " = (*" 
                               ++ (getSize si) 
                               ++ " *) (" 
                               ++ (show sr) 
                               ++ " + " 
                               ++ (show os) 
                               ++ ");\n" 
      | ic == Just BPF_LD    = "Non-Standard Load;\n"
      | ic == Just BPF_STX   = "(*"
                               ++ (getSize si)
                               ++ "*) ("
                               ++ (show de)
                               ++ " + "
                               ++ (show os)
                               ++ ") = "
                               ++ (show sr)
                               ++ ";\n"
      | ic == Just BPF_ST    = "(*"
                               ++ (getSize si)
                               ++ "*) ("
                               ++ (show de)
                               ++ " + "
                               ++ (show os)
                               ++ ") = "
                               ++ (show im)
                               ++ ";\n"
      | otherwise       = "Non-Standard Load/Store Operation;\n"
      where
         getSize =  (\x -> if x /= Nothing
                           then if (lookupSize $ (\(Just y) -> y) x) /= Nothing 
                                then (\(Just y) -> y) $ lookupSize $ (\(Just y) -> y) x  
                                else ""
                           else ""
                    )
         lookupSize = (\x -> lookup x sizeStringMap)
   show EmptyInstruction = "Empty Instruction;\n" 

data EBPFsection = EBPFsection {title :: String, instructions :: [EBPFinstruction]}

instance Show EBPFsection where
   show (EBPFsection t i) = (showTitle t) ++ (showInstructions i) where
      showTitle = (\x -> x ++ ":\n")
      showInstructions = (\x -> foldl (++) "" (fmap (\y -> "   " ++ (show y)) x))
                   
operationStringMap = [(BPF_ADD, "+=")
    , (BPF_SUB, "-=")
    , (BPF_MUL, "*=")
    , (BPF_DIV, "/=")
    , (BPF_OR, "|=")
    , (BPF_AND, "&=")
    , (BPF_LSH, "<<=")
    , (BPF_RSH, ">>=")
    , (BPF_NEG, "=~")
    , (BPF_MOD, "%=")
    , (BPF_XOR, "^=")
    , (BPF_MOV, "=")
    , (BPF_ARSH, "sign extending shift right")
    , (BPF_END, "endianness conversion")
    ]

comparisonStringMap = [(BPF_JEQ, "==")
    , (BPF_JGT, ">")
    , (BPF_JGE, ">=")
    , (BPF_JSET, "&")
    , (BPF_JNE, "!=")
    , (BPF_JSGT, ">")
    , (BPF_JSGE, ">=")
    , (BPF_JLT, "<")
    , (BPF_JLE, "<=")
    , (BPF_JSLT, "<")
    , (BPF_JSLE, "<=")
    ]

sizeStringMap = [(BPF_W, "u32")
    , (BPF_H, "u16")
    , (BPF_B, "u8")
    , (BPF_DW, "u64")
    ]

classHexMap = [ (0x00, BPF_LD)
    , (0x01, BPF_LDX)
    , (0x02, BPF_ST)
    , (0x03, BPF_STX)
    , (0x04, BPF_ALU)
    , (0x05, BPF_JMP)
    , (0x06, BPF_JMP32)
    , (0x07, BPF_ALU64)
    ]

loadClass = [BPF_LD, BPF_LDX]
storeClass = [BPF_ST, BPF_STX]
arithmeticClass = [BPF_ALU, BPF_ALU64]
jumpClass = [BPF_JMP, BPF_JMP32]

sourceHexMap = [(0x00, BPF_K), (0x08, BPF_X)]

arithmeticHexMap :: [(Word64, AJCode)]
arithmeticHexMap =
    [ (0x00, BPF_ADD)
    , (0x10, BPF_SUB)
    , (0x20, BPF_MUL)
    , (0x30, BPF_DIV)
    , (0x40, BPF_OR)
    , (0x50, BPF_AND)
    , (0x60, BPF_LSH)
    , (0x70, BPF_RSH)
    , (0x80, BPF_NEG)
    , (0x90, BPF_MOD)
    , (0xa0, BPF_XOR)
    , (0xb0, BPF_MOV)
    , (0xc0, BPF_ARSH)
    , (0xd0, BPF_END)
    ]

jumpHexMap :: [(Word64, AJCode)]
jumpHexMap =
    [ (0x00, BPF_JA)
    , (0x10, BPF_JEQ)
    , (0x20, BPF_JGT)
    , (0x30, BPF_JGE)
    , (0x40, BPF_JSET)
    , (0x50, BPF_JNE)
    , (0x60, BPF_JSGT)
    , (0x70, BPF_JSGE)
    , (0x80, BPF_CALL)
    , (0x90, BPF_EXIT)
    , (0xa0, BPF_JLT)
    , (0xb0, BPF_JLE)
    , (0xc0, BPF_JSLT)
    , (0xd0, BPF_JSLE)
    ]

sizeHexMap =
    [ (0x00, BPF_W)
    , (0x08, BPF_H)
    , (0x10, BPF_B)
    , (0x18, BPF_DW)
    ]

modeHexMap =
    [ (0x00, BPF_IMM)
    , (0x20, BPF_ABS)
    , (0x40, BPF_IND)
    , (0x60, BPF_MEM)
    , (0xc0, BPF_ATOMIC)
    ]

registerHexMap =
    [ (0x00, R0)
    , (0x01, R1)
    , (0x02, R2)
    , (0x03, R3)
    , (0x04, R4)
    , (0x05, R5)
    , (0x06, R6)
    , (0x07, R7)
    , (0x08, R8)
    , (0x09, R9)
    , (0x0a, R10)
    ]


validateEBPF :: PreEBPFinstruction -> EBPFinstruction
validateEBPF (PreEBPFinstruction op de sr os im)
   | op /= Nothing && de /= Nothing && sr /= Nothing  = (EBPFinstruction (unjust op) (unjust sr) (unjust de) os im)
   | otherwise                                        = EmptyInstruction
   where
      unjust = (\(Just y) -> y)

parseEBPF :: Word64 -> PreEBPFinstruction
parseEBPF inst = PreEBPFinstruction (getOpcode inst) (getDest inst) (getSrc inst) (getOffset inst) (getImmediate inst)
   where
      getOpcode :: Word64 -> Maybe Opcode
      getOpcode n0
         | (isLoad $ getClass n0) == True          = Just $ LoadStoreOpcode (getClass n0) (getSize n0) (getMode n0)
         | (isStore $ getClass n0) == True         = Just $ LoadStoreOpcode (getClass n0) (getSize n0) (getMode n0)
         | (isJump $ getClass n0) == True          = Just $ ArithmeticJumpOpcode (getClass n0) (getSourceModifier n0) (getJCode n0)
         | (isArithmetic $ getClass n0) == True    = Just $ ArithmeticJumpOpcode (getClass n0) (getSourceModifier n0) (getACode n0)
         | otherwise                               = Nothing
      getClass :: Word64 -> Maybe InstructionClass
      getClass = (\x -> (lookup ((.&.) x classBitmask) classHexMap))
      getSourceModifier :: Word64 -> Maybe SourceModifier
      getSourceModifier = (\x -> (lookup ((.&.) x sourceBitmask) sourceHexMap))
      getACode :: Word64 -> Maybe AJCode
      getACode = (\x -> (lookup ((.&.) x opBitmask) arithmeticHexMap))
      getJCode :: Word64 -> Maybe AJCode
      getJCode = (\x -> (lookup ((.&.) x opBitmask) jumpHexMap))
      getSize :: Word64 -> Maybe SizeModifier
      getSize = (\x -> (lookup ((.&.) x sizeBitmask) sizeHexMap))
      getMode :: Word64 -> Maybe ModeModifier
      getMode = (\x -> (lookup ((.&.) x modeBitmask) modeHexMap))
      getDest :: Word64 -> Maybe Register
      getDest = (\x -> (lookup (((.&.) x destBitmask) `shiftR` 8) registerHexMap))
      getSrc :: Word64 -> Maybe Register
      getSrc = (\x -> (lookup (((.&.) x srcBitmask) `shiftR` 12) registerHexMap))
      getOffset :: Word64 -> Integer
      getOffset = (\x -> toInteger $ ((.&.) x offsetBitmask) `shiftR` 16)
      getImmediate :: Word64 -> Integer
      getImmediate = (\x -> toInteger $ ((.&.) x immediateBitmask) `shiftR` 32)
      isLoad :: Maybe InstructionClass -> Bool
      isLoad = (\x -> if x /= Nothing then ((\(Just y) -> y) x) `elem` loadClass else False)
      isStore :: Maybe InstructionClass -> Bool
      isStore = (\x -> if x /= Nothing then ((\(Just y) -> y) x) `elem` storeClass else False)
      isJump :: Maybe InstructionClass -> Bool
      isJump = (\x -> if x /= Nothing then ((\(Just y) -> y) x) `elem` jumpClass else False)
      isArithmetic :: Maybe InstructionClass -> Bool
      isArithmetic = (\x -> if x /= Nothing then ((\(Just y) -> y) x) `elem` arithmeticClass else False)
      classBitmask :: Word64
      classBitmask = 0x07
      sourceBitmask :: Word64
      sourceBitmask = 0x1 `shiftL` 3
      opBitmask :: Word64
      opBitmask = 0xF `shiftL` 4
      sizeBitmask :: Word64
      sizeBitmask = 0x3 `shiftL` 3
      modeBitmask :: Word64
      modeBitmask = 0x7 `shiftL` 5
      destBitmask :: Word64
      destBitmask = 0xF `shiftL` 8
      srcBitmask :: Word64
      srcBitmask = 0xF `shiftL` 12
      offsetBitmask :: Word64
      offsetBitmask = 0xFF `shiftL` 16
      immediateBitmask :: Word64
      immediateBitmask = 0xFFFF `shiftL` 32

      
