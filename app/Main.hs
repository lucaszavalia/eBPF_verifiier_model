module Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Elf as ELF
import qualified Data.Word as DW
import qualified Data.Binary.Get as BG
import EBPFparser

main :: IO ()
main = putStrLn "Welcome to the EBPF verifier model!" >>
   putStrLn "Name of EBPF Object File: " >>
   getLine >>= (\filename ->
      BS.readFile filename >>= (\file ->
         putStrLn 
         $ show
         $ parseInstructions
         $ mapSection64
         $ ELF.elfSections 
         $ ELF.parseElf file
      )
   )
   where
      parseInstructions :: [(String, [DW.Word64])] -> [(String, [Maybe InstructionClass])]
      parseInstructions = (\x -> fmap (\(a,b) -> (a, fmap parseEBPF b)) x)
      mapSection64 :: [ELF.ElfSection] -> [(String, [DW.Word64])]
      mapSection64 = (\x -> fmap (\y -> (getName y, getSection64 y)) x)
      getSection64 :: ELF.ElfSection -> [DW.Word64]
      getSection64 = (\x -> splitBytes $ BS.unpack $ ELF.elfSectionData x)
      getName :: ELF.ElfSection -> String
      getName = ELF.elfSectionName
      splitBytes :: [DW.Word8] -> [DW.Word64]
      splitBytes = (\x -> toWord64 $ fromWord8toBS $ groupBy8 $ adjustListLength x)
      adjustListLength :: [DW.Word8] -> [DW.Word8]
      adjustListLength xs
         | (length xs) `mod` 8 == 0    = xs
         | otherwise                   = xs ++ take (8-((length xs) `mod` 8)) (repeat 0)
      toWord64 :: [BS.ByteString] -> [DW.Word64]
      toWord64 = (\x -> fmap (\y -> BG.runGet BG.getWord64be (BSL.fromStrict y)) x)  
      fromWord8toBS :: [[DW.Word8]] -> [BS.ByteString]
      fromWord8toBS = (\x -> fmap (\y -> BS.pack y) x)
      groupBy8 :: [DW.Word8] -> [[DW.Word8]]
      groupBy8 [] = []
      groupBy8 xs = [take 8 xs] ++ groupBy8 (drop 8 xs)
