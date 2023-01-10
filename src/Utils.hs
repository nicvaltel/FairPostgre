module Utils where

import Data.Bits (Bits (shiftL, shiftR, (.|.)))
import Data.Word (Word16, Word32, Word8)

octets :: Word32 -> [Word8]
octets w =
  [ fromIntegral (w `shiftR` 24),
    fromIntegral (w `shiftR` 16),
    fromIntegral (w `shiftR` 8),
    fromIntegral w
  ]

fromOctets :: [Word8] -> Word32
fromOctets = foldl accum 0
  where
    accum a o = a `shiftL` 8 .|. fromIntegral o

fromOctets16 :: [Word8] -> Word16
fromOctets16 = foldl accum 0
  where
    accum a o = a `shiftL` 8 .|. fromIntegral o
