-- | Loading AVR program images.
--
-- AVR code memory is 16-bit word-addressed — each instruction word is 16 bits,
-- stored little-endian in the flat @.bin@ — so an AVR program image is read as
-- little-endian 16-bit words.  This is the AVR-specific width choice layered on
-- top of ISACLE's generic 'readBinWith'; SoC descriptions just consume
-- 'readAvrProgram'.
module AVR.Program
    ( readAvrProgram
    ) where

import Prelude
import Isacle.System.Rom (readBin16LE)

-- | Read an assembled AVR program (@.bin@) as a list of little-endian 16-bit
-- code words.  Hand the result to 'Isacle.System.SystemDSL.createRom' (via
-- 'Isacle.System.SystemDSL.RomImage') — padding to the ROM's declared size
-- happens there.
readAvrProgram :: FilePath -> IO [Integer]
readAvrProgram = readBin16LE
