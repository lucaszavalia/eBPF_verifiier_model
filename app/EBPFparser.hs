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

data ACode = BPF_ADD
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
          deriving (Show, Eq)

data JCode = BPF_JA
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

data AJCode = ACode | JCode deriving (Show, Eq)

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
                 iclass :: InstructionClass,
                 source :: SourceModifier,
                 code :: AJCode
              } |
              LoadStoreOpcode { -- 3 bits, 2 bits, 3 bits
                 iClass :: InstructionClass, 
                 size :: SizeModifier, 
                 mode :: ModeModifier
              } deriving (Show, Eq)

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 deriving (Show, Eq)

data EBPFinstruction = EBPFinstruction {
                         opcode :: Opcode,
                         src :: Register,
                         dest :: Register,
                         offset :: Int,
                         immediate :: Int
                       } deriving (Show, Eq)


classHexMap = [ (0x00, BPF_LD)
    , (0x01, BPF_LDX)
    , (0x02, BPF_ST)
    , (0x03, BPF_STX)
    , (0x04, BPF_ALU)
    , (0x05, BPF_JMP)
    , (0x06, BPF_JMP32)
    , (0x07, BPF_ALU64)
    ]

sourceHexMap = [(0x00, BPF_K), (0x08, BPF_X)]

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

parseEBPF :: Word64 -> Maybe InstructionClass
parseEBPF inst = getClass inst  
   where
      getClass :: Word64 -> Maybe InstructionClass
      getClass = (\x -> (lookup ((.&.) x classBitmask) classHexMap))
      classBitmask :: Word64
      classBitmask = 0x07
      {-sourceBitmask ::Word64
      sourceBitmask = 0x1 `shiftL` 3
      jocodeBitmask :: Word64
      jocodeBitmask = 0xF `shiftL` 4
      sizeBitmask :: Word64
      sizeBitmask = 0x4 `shiftL` 3
      modeBitmask :: Word64
      modeBitmask = 0x7 `shiftL` 5
      destBitmask :: Word64
      destBitmask = 0x7 `shiftL` 8
      srcBitmask :: Word64
      srcBitmask = 0x7 `shiftL` 12
      offsetBitmask :: Word64
      offsetBitmask = 0xFF `shiftL` 16
      immediateBitmask :: Word64
      immediateBitmask = 0xFFFF `shiftL` 32-}

      
