module Main where

import System.IO
import System.Process
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
{-import qualified Data.ByteString.Char8 as BSC-}
import qualified Data.Elf as ELF
import qualified Data.Word as DW
import qualified Data.Binary.Get as BG
import EBPFparser

main :: IO ()
main = putStrLn "Welcome to the EBPF verifier model!" >>
   putStrLn "Name of EBPF Object File: " >>
   getLine >>= (\filename ->
      BS.readFile filename >>= (\file ->
         (\x -> writeFile "./results/model_output.txt" x) 
         $ show
         $ parseInstructions
         $ mapSection64
         $ ELF.parseElf file
      )
      >> putStrLn "Directory of the eBPF Type Checker executable? " >>
      getLine >>= (\commandName ->
         callCommand (commandName 
                     ++ " " 
                     ++ filename 
                     ++ " -v > ./results/type_checker_ouput.txt")
      )
   )
   where
      parseInstructions :: [(String, DW.Word64, [DW.Word64])] -> [EBPFsection]
      parseInstructions = (\x -> fmap (\(a,n,b) -> EBPFsection a n (fmap (\y -> validateEBPF $ parseEBPF y) b)) x)
      mapSection64 :: ELF.Elf -> [(String, DW.Word64, [DW.Word64])]
      mapSection64 = (\x -> fmap (\y -> (getName y, getStart y, getSection64 y)) (filterSections $ ELF.elfSections x))
      filterSections :: [ELF.ElfSection] -> [ELF.ElfSection]
      filterSections = (\x -> filter (\y -> ELF.elfSectionType y == ELF.SHT_PROGBITS) x)
      getSection64 :: ELF.ElfSection -> [DW.Word64]
      getSection64 = (\x -> splitBytes $ BS.unpack $ ELF.elfSectionData x)
      getName :: ELF.ElfSection -> String
      getName = ELF.elfSectionName
      getStart :: ELF.ElfSection -> DW.Word64
      getStart = ELF.elfSectionAddr
      splitBytes :: [DW.Word8] -> [DW.Word64]
      splitBytes = (\x -> toWord64 $ fromWord8toBS $ groupBy8 $ adjustListLength x)
      adjustListLength :: [DW.Word8] -> [DW.Word8]
      adjustListLength xs
         | (length xs) `mod` 8 == 0    = xs
         | otherwise                   = xs ++ take (8-((length xs) `mod` 8)) (repeat 0)
      toWord64 :: [BS.ByteString] -> [DW.Word64]
      toWord64 = (\x -> fmap (\y -> BG.runGet BG.getWord64le (BSL.fromStrict y)) x)  
      fromWord8toBS :: [[DW.Word8]] -> [BS.ByteString]
      fromWord8toBS = (\x -> fmap (\y -> BS.pack y) x)
      groupBy8 :: [DW.Word8] -> [[DW.Word8]]
      groupBy8 [] = []
      groupBy8 xs = [take 8 xs] ++ groupBy8 (drop 8 xs)
