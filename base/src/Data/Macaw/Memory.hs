{-|
Copyright   : (c) Galois Inc, 2015-2016
Maintainer  : jhendrix@galois.com

Declares 'Memory', a type for representing segmented memory with permissions.

To support shared libraries and relocatable executable, our memory
representation supports relocatable regions.  Each region is defined
by an index, and multiple segments may be relative to the same index.
The index `0` is reserved to denote absolute addresses, while the other
regions may have other addresses.  Essentially, our model distinguishes
segments from regions.  Segments define a contiguous region of bytes with
some value while regions define a unknown offset in memory.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Macaw.Memory
  ( Memory
  , memAddrWidth
  , memWidth
  , emptyMemory
  , InsertError(..)
  , showInsertError
  , insertMemSegment
  , memSegments
  , executableSegments
  , readonlySegments
    -- * AddrWidthRepr
  , AddrWidthRepr(..)
  , addrWidthNatRepr
  , addrWidthClass
    -- * Endianness
  , Endianness(..)
    -- * MemSegment operations
  , MemSegment
  , RegionIndex
  , RelocMap
  , memSegment
  , segmentBase
  , segmentOffset
  , segmentFlags
  , segmentContents
  , ppMemSegment
  , segmentSize
  , SegmentRange(..)
  , DropError(..)
  , dropErrorAsMemError
  , dropSegmentRangeListBytes
    -- * MemWord
  , MemWord
  , MemWidth(..)
  , memWord
  , memWordInteger
    -- * Segment offsets
  , MemSegmentOff
  , viewSegmentOff
  , resolveAddr
  , resolveAbsoluteAddr
  , resolveSegmentOff
  , msegSegment
  , msegOffset
  , msegAddr
  , incSegmentOff
  , diffSegmentOff
  , contentsAfterSegmentOff
  , clearSegmentOffLeastBit
  , memAsAddrPairs
    -- * Symbols
  , SymbolRef(..)
  , SymbolType(..)
  , SymbolPrecedence(..)
  , SymbolRequirement(..)
  , SymbolVersion(..)
    -- * General purposes addrs
  , MemAddr
  , addrBase
  , addrOffset
  , absoluteAddr
  , relativeAddr
  , relativeSegmentAddr
  , asAbsoluteAddr
  , asSegmentOff
  , diffAddr
  , incAddr
  , addrLeastBit
  , clearAddrLeastBit
    -- * Symbols
  , MemSymbol(..)
    -- * Reading
  , MemoryError(..)
  , addrContentsAfter
  , readByteString
  , readAddr
  , readWord8
  , readWord16be
  , readWord16le
  , readWord32be
  , readWord32le
  , readWord64be
  , readWord64le
    -- * Utilities
  , bsWord8
  , bsWord16be
  , bsWord16le
  , bsWord32be
  , bsWord32le
  , bsWord64be
  , bsWord64le
  , AddrSymMap
  ) where

import           Control.Exception (assert)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Proxy
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import           Data.Parameterized.NatRepr

import qualified Data.Macaw.Memory.Permissions as Perm

------------------------------------------------------------------------
-- AddrWidthRepr

-- | An address width
data AddrWidthRepr w
   = (w ~ 32) => Addr32
     -- ^ A 32-bit address
   | (w ~ 64) => Addr64
     -- ^ A 64-bit address

-- | The nat representation of this address.
addrWidthNatRepr :: AddrWidthRepr w -> NatRepr w
addrWidthNatRepr Addr32 = knownNat
addrWidthNatRepr Addr64 = knownNat

------------------------------------------------------------------------
-- Endianness

-- | Indicates whether bytes are stored in big or little endian representation.
data Endianness = BigEndian | LittleEndian
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Utilities

-- | Split a bytestring into an equivalent list of byte strings with a given size.
--
-- This drops the last bits if the total length is not a multiple of the size.
regularChunks :: Int -> BS.ByteString -> [BS.ByteString]
regularChunks sz bs
  | BS.length bs < sz = []
  | otherwise = BS.take sz bs : regularChunks sz (BS.drop sz bs)

bsWord8 :: BS.ByteString -> Word8
bsWord8 bs
    | BS.length bs /= 1 = error "bsWord8 given bytestring with bad length."
    | otherwise = BS.index bs 0

bsWord16be :: BS.ByteString -> Word16
bsWord16be bs
    | BS.length bs /= 2 = error "bsWord16be given bytestring with bad length."
    | otherwise = w 0 .|. w 1
  where w i = fromIntegral (BS.index bs i) `shiftL` ((1 - i) `shiftL` 3)

bsWord16le :: BS.ByteString -> Word16
bsWord16le bs
    | BS.length bs /= 2 = error "bsWord16le given bytestring with bad length."
    | otherwise = w 0 .|. w 1
  where w i = fromIntegral (BS.index bs i) `shiftL` (i `shiftL` 3)

bsWord32be :: BS.ByteString -> Word32
bsWord32be bs
    | BS.length bs /= 4 = error "bsWord32be given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3
  where w i = fromIntegral (BS.index bs i) `shiftL` ((3 - i) `shiftL` 3)

bsWord32le :: BS.ByteString -> Word32
bsWord32le bs
    | BS.length bs /= 4 = error "bsWord32le given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3
  where w i = fromIntegral (BS.index bs i) `shiftL` (i `shiftL` 3)

bsWord64be :: BS.ByteString -> Word64
bsWord64be bs
    | BS.length bs /= 8 = error "bsWord64be given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3 .|. w 4 .|. w 5 .|. w 6 .|. w 7
  where w i = fromIntegral (BS.index bs i) `shiftL` ((7 - i) `shiftL` 3)

bsWord64le :: BS.ByteString -> Word64
bsWord64le bs
    | BS.length bs /= 8 = error "bsWord64le given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3 .|. w 4 .|. w 5 .|. w 6 .|. w 7
  where w i = fromIntegral (BS.index bs i) `shiftL` (i `shiftL` 3)

------------------------------------------------------------------------
-- MemBase

-- | This represents a particular numeric address in memory.
--
-- Internally, the address is stored with all bits greater than the
-- width equal to 0.
newtype MemWord (w :: Nat) = MemWord { _memWordValue :: Word64 }

-- | Treat the word as an integer.
-- A version of `fromEnum` that won't wrap around.
memWordInteger :: MemWord w -> Integer
memWordInteger = fromIntegral . _memWordValue

instance Show (MemWord w) where
  showsPrec _ (MemWord w) = showString "0x" . showHex w

instance Pretty (MemWord w) where
  pretty = text . show

instance Eq (MemWord w) where
  MemWord x == MemWord y = x == y

instance Ord (MemWord w) where
  compare (MemWord x) (MemWord y) = compare x y

-- | Typeclass for legal memory widths
class (1 <= w) => MemWidth w where

  -- | Returns @AddrWidthRepr@ to identify width of pointer.
  --
  -- The argument is ignored.
  addrWidthRepr :: p w -> AddrWidthRepr w

  -- | @addrWidthMod w@ returns @2^(8 * addrSize w - 1)@.
  addrWidthMod :: p w -> Word64

  -- | Returns number of bytes in addr.
  --
  -- The argument is not evaluated.
  addrSize :: p w -> Int

  -- Rotates the value by the given index.
  addrRotate :: MemWord w -> Int -> MemWord w

  -- | Read an address with the given endianess.
  addrRead :: Endianness -> BS.ByteString -> Maybe (MemWord w)

-- | Returns number of bits in address.
addrBitSize :: MemWidth w => p w -> Int
addrBitSize w = 8 * addrSize w

-- | Convert word64 @x@ into mem word @x mod 2^w-1@.
memWord :: forall w . MemWidth w => Word64 -> MemWord w
memWord x = MemWord (x .&. addrWidthMod p)
  where p :: Proxy w
        p = Proxy

instance MemWidth w => Num (MemWord w) where
  MemWord x + MemWord y = memWord $ x + y
  MemWord x - MemWord y = memWord $ x - y
  MemWord x * MemWord y = memWord $ x * y
  abs = id
  fromInteger = memWord . fromInteger
  negate (MemWord x) = memWord (negate x)
  signum (MemWord x) = memWord (signum x)

instance MemWidth w => Bits (MemWord w) where

  MemWord x .&. MemWord y = memWord (x .&. y)
  MemWord x .|. MemWord y = memWord (x .|. y)
  MemWord x `xor` MemWord y = memWord (x `xor` y)
  complement (MemWord x) = memWord (complement x)
  MemWord x `shift` i = memWord (x `shift` i)
  x `rotate` i = addrRotate x i
  bitSize = addrBitSize
  bitSizeMaybe x = Just (addrBitSize x)
  isSigned _ = False
  MemWord x `testBit` i = x `testBit` i
  bit i = memWord (bit i)
  popCount (MemWord x) = popCount x

instance MemWidth w => Enum (MemWord w) where
  toEnum = memWord . fromIntegral
  fromEnum (MemWord x) = fromIntegral x

instance MemWidth w => Real (MemWord w) where
  toRational (MemWord x) = toRational x

instance MemWidth w => Integral (MemWord w) where
  MemWord x `quotRem` MemWord y = (MemWord q, MemWord r)
    where (q,r) = x `quotRem` y
  toInteger (MemWord x) = toInteger x

instance MemWidth w => Bounded (MemWord w) where
  minBound = 0
  maxBound = MemWord (addrWidthMod (Proxy :: Proxy w))

instance MemWidth 32 where
  addrWidthRepr _ = Addr32
  addrWidthMod _ = 0xffffffff
  addrRotate (MemWord w) i = MemWord (fromIntegral ((fromIntegral w :: Word32) `rotate` i))
  addrSize _ = 4
  addrRead e s
    | BS.length s < 4 = Nothing
    | otherwise =
      case e of
        BigEndian -> Just $ MemWord $ fromIntegral $ bsWord32be s
        LittleEndian -> Just $ MemWord $ fromIntegral $ bsWord32le s

instance MemWidth 64 where
  addrWidthRepr _ = Addr64
  addrWidthMod _ = 0xffffffffffffffff
  addrRotate (MemWord w) i = MemWord (w `rotate` i)
  addrSize _ = 8
  addrRead e s
    | BS.length s < 8 = Nothing
    | otherwise =
      case e of
        BigEndian    -> Just $ MemWord $ fromIntegral $ bsWord64be s
        LittleEndian -> Just $ MemWord $ fromIntegral $ bsWord64le s

-- | Number of bytes in an address
addrWidthClass :: AddrWidthRepr w -> (MemWidth w => a) -> a
addrWidthClass Addr32 x = x
addrWidthClass Addr64 x = x

------------------------------------------------------------------------
-- SegmentRange

-- | Characterized the version information on a symbol
data SymbolVersion
   = ObjectSymbol
     -- ^ The symbol comes from an object file and hence does not
     -- have GNU version information.  Version information
     -- may be part of the symbol name however.
   | VersionedSymbol !BS.ByteString !BS.ByteString
     -- ^ A symbol with version information from version information
     -- in a shared library or executable.
     --
     -- The first value is the name of the shared object.  The second
     -- is the version associated with the symbol.
   | UnversionedSymbol
     -- ^ The symbol had the default *global* version information.

-- | Describes symbol precedence
data SymbolPrecedence
   = SymbolStrong
     -- ^ Symbol has high precedence
   | SymbolLocal
     -- ^ The symbol has high precedence, but only visible within the
     -- object file that created it.
   | SymbolWeak
     -- ^ Symbol has low precedence

-- | Describes whether symbol is required during linking.
data SymbolRequirement
   = SymbolRequired
     -- ^ Undefined symbol must be found during linking
   | SymbolOptional
     -- ^ Undefined symbol treated as zero if not found during linking.

-- | This defines information about the symbol related to whether
-- it is defined (and if so how it binds) or undefined (and if so what
-- requiremens there are for a match).
data SymbolType
   = DefinedSymbol !SymbolPrecedence
     -- ^ The symbol is defined and globally visible.
     --
     -- The strong symbol flag controls the precedence.  If true, then
     -- this definition must be used for the symbol with that name,
     -- and the linker is not allowed to replace the symbol.  Is
     -- false, then the linker will use a strong symbol if it exists,
     -- and one of the weak symbols if it does not.
   | UndefinedSymbol !SymbolRequirement
     -- ^ An undefined symbol
     --
     -- The Boolean flag controls whether the symbol must be defined.
     -- If it is @False@ and the linker cannot find a definition, then
     -- it just treats the symbol address as @0@.  If it is @True@ and
     -- the linker cannot find a definition, then it must throw an
     -- error.

-- | The name of a symbol along with optional version information.
--
-- Note that this is used for referencing undefined symbols, while
-- @MemSymbol@ is used for defined symbols.
data SymbolRef =
  SymbolRef { symbolName :: !BS.ByteString
            , symbolVersion :: !SymbolVersion
            , symbolType :: !SymbolType
            }

-- | Defines a portion of a segment.
--
-- The parameter denotes the width of a memory address.
data SegmentRange (w :: Nat)
   = ByteRegion !BS.ByteString
     -- ^ A region with specificed bytes
   | SymbolicRef !SymbolRef
     -- ^ A region containing a symbolic reference.
   | BSSRegion !(MemWord w)
     -- ^ A region containing the given number of zero-initialized bytes.

rangeSize :: forall w . MemWidth w => SegmentRange w -> MemWord w
rangeSize (ByteRegion bs) = fromIntegral (BS.length bs)
rangeSize (SymbolicRef _) = fromIntegral (addrSize (error "rangeSize nat evaluated" :: NatRepr w))
rangeSize (BSSRegion sz)  = sz

instance Show (SegmentRange w) where
  showsPrec _ (ByteRegion bs) = \s -> foldr ppByte s (BS.unpack bs)
    where ppByte w | w < 16    = showChar '0' . showHex w
                   | otherwise = showHex w
  showsPrec _ (SymbolicRef s) = shows (BSC.unpack (symbolName s))
  showsPrec _ (BSSRegion sz)  = showString "bss[" . shows sz . showChar ']'

  showList [] = id
  showList (h : r) = showsPrec 10 h . showList r

------------------------------------------------------------------------
-- SegmentContents

-- | A sequence of values in the segment.
newtype SegmentContents w = SegmentContents { segContentsMap :: Map.Map (MemWord w) (SegmentRange w) }

-- | Create the segment contents from a list of ranges.
contentsFromList :: MemWidth w => [SegmentRange w] -> SegmentContents w
contentsFromList elts = SegmentContents $ Map.fromList (offsets `zip` elts)
  where offsets = scanl (\s r -> s + rangeSize r) 0 elts

contentsSize :: MemWidth w => SegmentContents w -> MemWord w
contentsSize (SegmentContents m) =
  case Map.maxViewWithKey m of
    Nothing -> 0
    Just ((start, c),_) -> start + rangeSize c

-- | Return list of contents from given word or an error if this we can't cleanly
-- partition a relocation
-- due to a relocation.
contentsAfterSegmentOff :: MemWidth w
                        => MemSegmentOff w
                        -> Either (MemoryError w) [SegmentRange w]
contentsAfterSegmentOff mseg = do
  -- Get offset within segment to get
  let off = msegOffset mseg
  -- Get complete contents of segment
  let contents = segmentContents (msegSegment mseg)
  -- Split the map into all segments starting strictly before offset,
  -- memory starting at offset (if any), and contents strictly after offset.
  let (premap,mv,post) = Map.splitLookup off (segContentsMap contents)
  case mv of
    -- If something starts at offset, then return it and everything after.
    Just v -> Right $ v : Map.elems post
    -- If no memory starts exactly at offset, then
    -- look at the last segment starting before offset.
    Nothing ->
      case Map.maxViewWithKey premap of
        -- This implies nothing starts before the segment offset, which should not be
        -- allowed
        Nothing -> error $ "Memory.contentsAfterSegmentOff invalid contents"
        -- If last segment is a byte region then we drop elements before offset.
        Just ((pre_off, ByteRegion bs),_) -> do
          let v = ByteRegion (BS.drop (fromIntegral (off - pre_off)) bs)
          Right $ v : Map.elems post
        -- If last segment is a BSS region, then we drop elements before offset.
        Just ((pre_off, BSSRegion sz),_) -> do
          let v = BSSRegion (sz - fromIntegral (off - pre_off))
          Right $ v : Map.elems post
        -- If last segment is a symbolic reference, then the code is asking
        -- us to partition a symbolic reference in two, which we cannot do.
        Just ((_, SymbolicRef{}),_) ->
          Left (UnexpectedRelocation (relativeSegmentAddr mseg))

contentsList :: SegmentContents w -> [(MemWord w, SegmentRange w)]
contentsList (SegmentContents m) = Map.toList m

------------------------------------------------------------------------
-- Code for injecting relocations into segments.


-- | Contents of segment/section before symbol folded in.
data PresymbolData = PresymbolData !L.ByteString !Int64

mkPresymbolData :: L.ByteString -> Int64 -> PresymbolData
mkPresymbolData contents0 sz
  | sz >= L.length contents0 = PresymbolData contents0 (sz - L.length contents0)
  | otherwise = PresymbolData (L.take sz contents0) 0


-- | Convert bytes into a segment range list.
singleSegment :: L.ByteString -> [SegmentRange w]
singleSegment contents | L.null contents = []
                       | otherwise = [ByteRegion (L.toStrict contents)]

-- | @bssSegment cnt@ creates a BSS region with size @cnt@.
bssSegment :: MemWidth w => Int64 -> [SegmentRange w]
bssSegment c | c <= 0 = []
             | otherwise = [BSSRegion (fromIntegral c)]

-- | Return all segment ranges from remainder of data.
allSymbolData :: MemWidth w => PresymbolData -> [SegmentRange w]
allSymbolData (PresymbolData contents bssSize) =
  singleSegment contents ++ bssSegment bssSize

-- | Take the given amount of data out of presymbol data.
takeSegment :: MemWidth w => Int64 -> PresymbolData -> [SegmentRange w]
takeSegment cnt (PresymbolData contents bssSize) =
  singleSegment (L.take cnt contents)
  ++ bssSegment (min (cnt - L.length contents) bssSize)

-- | @dropSegment cnt dta@ drops @cnt@ bytes from @dta@.
dropSegment :: Int64 -> PresymbolData -> PresymbolData
dropSegment cnt (PresymbolData contents bssSize)
  | cnt <= L.length contents = PresymbolData (L.drop cnt contents) bssSize
  | otherwise = PresymbolData L.empty (bssSize - (cnt - L.length contents))

-- | Maps an address to the symbol that it is associated for.
type RelocMap w = Map w SymbolRef

-- | This takes a list of symbols and an address and coerces into a memory contents.
--
-- If the size is different from the length of file contents, then the file content
-- buffer is truncated or zero-extended as in a BSS.
byteSegments :: forall w
             .  MemWidth w
             => RelocMap (MemWord w) -- ^ Map from addresses to symbolis
             -> MemWord w -- ^ Base address for segment
             -> L.ByteString -- ^ File contents for segment.
             -> Int64 -- ^ Expected size
             -> [SegmentRange w]
byteSegments allSymbols initBase contents0 sz =
    bytesToSegmentsAscending symbolPairs initBase (mkPresymbolData contents0 sz)
  where -- Parse the map to get a list of symbols starting at base0.
        symbolPairs
          = Map.toList
          $ Map.dropWhileAntitone (< initBase) allSymbols

        -- Get last address for this region
        end :: MemWord w
        end = initBase + fromIntegral sz

        -- Get size of pointer
        ptrSize :: MemWord w
        ptrSize = fromIntegral (addrSize initBase)

        -- Traverse the list of symbols that we should parse.
        bytesToSegmentsAscending ::[(MemWord w, SymbolRef)] -- ^ List of symbols within the segment.
                                 -> MemWord w -- ^ The starting address of memory
                                 -> PresymbolData
                                  -- ^ The remaining bytes in memory including a number extra bss.
                                 -> [SegmentRange w]
        bytesToSegmentsAscending ((addr,tgt):rest) base contents | addr < end =
            takeSegment (fromIntegral off) contents
            ++ [SymbolicRef tgt]
            ++ bytesToSegmentsAscending rest (addr + ptrSize) post
          where off = addr - base
                post   = dropSegment (fromIntegral (off + ptrSize)) contents
        bytesToSegmentsAscending _ _ contents = allSymbolData contents

------------------------------------------------------------------------
-- MemSegment

-- | This is used to identify the relative offset for a segment.
--
-- Absolute segments have a base index of zero.
type RegionIndex = Int

-- | Describes a memory segment.
--
-- Memory segments are non-overlapping regions of memory.
data MemSegment w
   = MemSegment { segmentBase  :: !RegionIndex
                  -- ^ Base for this segment
                  --
                  -- N.B. 0 indicates a fixed base address of zero.
                , segmentOffset :: !(MemWord w)
                  -- ^ Offset of segment relative to segmentBase
                , segmentFlags :: !Perm.Flags
                                  -- ^ Permisison flags
                , segmentContents :: !(SegmentContents w)
                                     -- ^ Map from offsets to the contents of
                                     -- the segment.
                }

-- | This creates a memory segment.
memSegment :: forall w
           .  MemWidth w
           => RegionIndex
              -- ^ Index of base (0=absolute address)
           -> RelocMap (MemWord w)
              -- ^ Relocations we may need to apply when creating the
              -- segment.  These are all relative to the given region.
           -> MemWord w
              -- ^ Offset of segment
           -> Perm.Flags
              -- ^ Flags if defined
           -> L.ByteString
           -- ^ File contents for segment.
           -> Int64
           -- ^ Expected size (must be positive)
           -> MemSegment w
memSegment base allSymbols off flags bytes sz
      -- Return nothing if size is not positive
    | not (sz > 0) = error $ "Memory segments must have a positive size."
      -- Check for overflow in contents end
    | toInteger off + toInteger sz > toInteger (maxBound :: MemWord w) =
      error "Contents two large for base."
    | otherwise =
      MemSegment { segmentBase = base
                 , segmentOffset = off
                 , segmentFlags = flags
                 , segmentContents = contents
                 }
  where contentsl = byteSegments allSymbols off bytes sz
        contents = contentsFromList contentsl

instance Eq (MemSegment w) where
  x == y = segmentBase   x == segmentBase y
        && segmentOffset x == segmentOffset y

instance Ord (MemSegment w) where
  compare x y
    =  compare (segmentBase x)   (segmentBase y)
    <> compare (segmentOffset x) (segmentOffset y)

-- | Return the size of the segment data.
segmentSize :: MemWidth w => MemSegment w -> MemWord w
segmentSize = contentsSize . segmentContents

-- | Pretty print a memory segment.
ppMemSegment :: MemWidth w => MemSegment w -> Doc
ppMemSegment s =
  indent 2 $ vcat [ text "base   =" <+> text (show (segmentBase s))
                  , text "offset =" <+> text (show (segmentOffset s))
                  , text "flags  =" <+> text (show (segmentFlags s))
                  , text "size   =" <+> text (show (segmentSize s))
                  ]

instance MemWidth w => Show (MemSegment w) where
  show = show . ppMemSegment

------------------------------------------------------------------------
-- Memory

type SegmentOffsetMap w = Map.Map RegionIndex (Map.Map (MemWord w) (MemSegment w))

-- | The state of the memory.
data Memory w = Memory { memAddrWidth :: !(AddrWidthRepr w)
                         -- ^ Return the address width of the memory
                       , memSegmentMap :: !(SegmentOffsetMap w)
                       }

-- | Get memory segments.
memSegments :: Memory w -> [MemSegment w]
memSegments m = concatMap Map.elems (Map.elems (memSegmentMap m))

-- | Return nat repr associated with memory.
memWidth :: Memory w -> NatRepr w
memWidth = addrWidthNatRepr . memAddrWidth

instance MemWidth w => Show (Memory w) where
  show = show . memSegments

-- | A memory with no segments.
emptyMemory :: AddrWidthRepr w -> Memory w
emptyMemory w = Memory { memAddrWidth = w
                       , memSegmentMap = Map.empty
                       }

-- | Get executable segments.
executableSegments :: Memory w -> [MemSegment w]
executableSegments = filter (Perm.isExecutable . segmentFlags) . memSegments

-- | Get readonly segments
readonlySegments :: Memory w -> [MemSegment w]
readonlySegments = filter (Perm.isReadonly . segmentFlags) . memSegments

data InsertError w
   = OverlapSegment (MemSegment w) (MemSegment w)
     -- ^ The inserted segment overlaps with the given segment.

showInsertError :: InsertError w -> String
showInsertError (OverlapSegment _base _seg) = "overlaps with memory segment."

insertSegmentOffsetMap :: MemWidth w
                       => MemSegment w
                       -> SegmentOffsetMap w
                       -> Either (InsertError w) (SegmentOffsetMap w)
insertSegmentOffsetMap seg segOffMap =
  let base = segmentBase seg
      m = Map.findWithDefault Map.empty base segOffMap
   in case Map.lookupGE (segmentOffset seg) m of
        Just (next,old) | next < segmentOffset seg + segmentSize seg ->
          Left $ OverlapSegment seg old
        _ ->
          Right $ Map.insert base (Map.insert (segmentOffset seg) seg m) segOffMap

-- | Insert segment into memory or fail if this overlaps with another
-- segment in memory.
insertMemSegment :: MemSegment w
                 -> Memory w
                 -> Either (InsertError w) (Memory w)
insertMemSegment seg mem = addrWidthClass (memAddrWidth mem) $ do
  absMap <- insertSegmentOffsetMap seg (memSegmentMap mem)
  pure $ Memory { memAddrWidth = memAddrWidth mem
                , memSegmentMap = absMap
                }

------------------------------------------------------------------------
-- MemSegmentOff
-- | A pair containing a segment and offset.
--
-- Functions that return a segment-offset pair enforce that the offset
-- is strictly less than the size of the memory segment in bytes.
data MemSegmentOff w = MemSegmentOff { msegSegment :: !(MemSegment w)
                                     , msegOffset :: !(MemWord w)
                                     }
  deriving (Eq, Ord)

{-# DEPRECATED viewSegmentOff "Use msegSegment and msegOffset." #-}
viewSegmentOff :: MemSegmentOff w -> (MemSegment w, MemWord w)
viewSegmentOff mseg = (msegSegment mseg, msegOffset mseg)

-- | Return the segment associated with the given address if well-defined.
resolveAddr :: Memory w -> RegionIndex -> MemWord w -> Maybe (MemSegmentOff w)
resolveAddr mem idx addr = addrWidthClass (memAddrWidth mem) $ do
  m <- Map.lookup idx (memSegmentMap mem)
  case Map.lookupLE addr m of
    Just (base, seg) | addr - base < segmentSize seg ->
      Just $! MemSegmentOff { msegSegment = seg
                            , msegOffset = addr - base
                            }
    _ -> Nothing

-- | Return the segment associated with the given address if well-defined.
resolveAbsoluteAddr :: Memory w -> MemWord w -> Maybe (MemSegmentOff w)
resolveAbsoluteAddr mem addr = resolveAddr mem 0 addr

-- | Make a segment offset pair after ensuring the offset is valid
resolveSegmentOff :: MemWidth w => MemSegment w -> MemWord w -> Maybe (MemSegmentOff w)
resolveSegmentOff seg off
  | off < segmentSize seg = Just (MemSegmentOff seg off)
  | otherwise = Nothing

-- | Return the absolute address associated with the segment offset pair (if any)
msegAddr :: MemWidth w => MemSegmentOff w -> Maybe (MemWord w)
msegAddr mseg = do
  let seg = msegSegment mseg
   in if segmentBase seg == 0 then
        Just (segmentOffset seg + msegOffset mseg)
       else
        Nothing

-- | Clear the least-significant bit of an segment offset.
clearSegmentOffLeastBit :: MemWidth w => MemSegmentOff w -> MemSegmentOff w
clearSegmentOffLeastBit (MemSegmentOff seg off) = MemSegmentOff seg (off .&. complement 1)

-- | Increment a segment offset by a given amount.
--
-- Returns 'Nothing' if the result would be out of range.
incSegmentOff :: MemWidth w => MemSegmentOff w -> Integer -> Maybe (MemSegmentOff w)
incSegmentOff (MemSegmentOff seg off) inc
  | 0 <= next && next <= toInteger (segmentSize seg) = Just $ MemSegmentOff seg (fromInteger next)
  | otherwise = Nothing
  where next = toInteger off + inc

-- | Return the difference between two segment offsets pairs or `Nothing` if undefined.
diffSegmentOff :: MemWidth w => MemSegmentOff w -> MemSegmentOff w -> Maybe Integer
diffSegmentOff (MemSegmentOff xseg xoff) (MemSegmentOff yseg yoff)
  | segmentBase xseg == segmentBase yseg =
    Just $ toInteger (segmentOffset xseg + xoff) - toInteger (segmentOffset yseg + yoff)
  | otherwise = Nothing

instance MemWidth w => Show (MemSegmentOff w) where
  showsPrec p (MemSegmentOff seg off) =
    if segmentBase seg == 0 then
      showString "0x" . showHex (segmentOffset seg + off)
     else
       showParen (p > 6)
        $ showString "segment"
        . shows (segmentBase seg)
        . showString "+"
        . shows (segmentOffset seg + off)

instance MemWidth w => Pretty (MemSegmentOff w) where
  pretty = text . show

-- | Return list of segmented address values in memory.
--
-- Each address includes the value and the base.
memAsAddrPairs :: Memory w
               -> Endianness
               -> [(MemSegmentOff w, MemSegmentOff w)]
memAsAddrPairs mem end = addrWidthClass (memAddrWidth mem) $ do
  seg <- memSegments mem
  (contents_offset,r) <- contentsList (segmentContents seg)
  let sz = addrSize mem
  case r of
    ByteRegion bs -> assert (BS.length bs `rem` fromIntegral sz == 0) $ do
      (off,w) <-
        zip [contents_offset..]
            (regularChunks (fromIntegral sz) bs)
      let Just val = addrRead end w
      case resolveAbsoluteAddr mem val of
        Just val_ref -> do
          pure (MemSegmentOff seg off, val_ref)
        _ -> []
    SymbolicRef{} -> []
    BSSRegion{} -> []

------------------------------------------------------------------------
-- MemAddr

-- | An address in memory.  Due to our use of relocatable "regions",
-- this internally represented by a region index for the base, and an
-- offset.
--
-- This representation does not require that the address is mapped to
-- actual memory (see `MemSegmentOff` for an address representation
-- that ensures the reference points to allocated memory).
data MemAddr w
   = MemAddr { addrBase :: {-# UNPACK #-} !RegionIndex
             , addrOffset :: {-# UNPACK #-} !(MemWord w)
             }
     -- ^ An address formed from a specific value.
  deriving (Eq, Ord)

-- | Given an absolute address, this returns a segment and offset into the segment.
absoluteAddr :: MemWord w -> MemAddr w
absoluteAddr o = MemAddr { addrBase = 0, addrOffset = o }

-- | Construct an address relative to an existing memory segment.
relativeAddr :: MemWidth w => MemSegment w -> MemWord w -> MemAddr w
relativeAddr seg off = MemAddr { addrBase = segmentBase seg, addrOffset = segmentOffset seg + off }

-- | Convert the segment offset to an address.
relativeSegmentAddr :: MemWidth w => MemSegmentOff w -> MemAddr w
relativeSegmentAddr (MemSegmentOff seg off) = relativeAddr seg off

-- | Return an absolute address if the region of the memAddr is 0.
asAbsoluteAddr :: MemWidth w => MemAddr w -> Maybe (MemWord w)
asAbsoluteAddr (MemAddr i w) = if i == 0 then Just w else Nothing

-- | Return a segment offset from the address if defined.
asSegmentOff :: Memory w -> MemAddr w -> Maybe (MemSegmentOff w)
asSegmentOff mem (MemAddr i addr) = resolveAddr mem i addr

-- | Clear the least significant bit of an address.
clearAddrLeastBit :: MemAddr w -> MemAddr w
clearAddrLeastBit (MemAddr i (MemWord off)) = MemAddr i (MemWord (off .&. complement 1))

-- | Return True if least-significant bit in addr is set.
addrLeastBit :: MemAddr w -> Bool
addrLeastBit (MemAddr _ (MemWord off)) = off `testBit` 0

-- | Increment an address by a fixed amount.
incAddr :: MemWidth w => Integer -> MemAddr w -> MemAddr w
incAddr o (MemAddr i off) = MemAddr i (off + fromInteger o)

-- | Returns the number of bytes between two addresses if they point to
-- the same region and `Nothing` if they are different segments.
diffAddr :: MemWidth w => MemAddr w -> MemAddr w -> Maybe Integer
diffAddr (MemAddr xb xoff) (MemAddr yb yoff)
  | xb == yb = Just $ toInteger xoff - toInteger yoff
  | otherwise = Nothing

instance MemWidth w => Show (MemAddr w) where
  showsPrec _ (MemAddr 0 a) = showString "0x" . showHex a
  showsPrec p (MemAddr i off) =
    showParen (p > 6)
    $ showString "segment"
    . shows i
    . showString "+"
    . shows off

instance MemWidth w => Pretty (MemAddr w) where
  pretty = text . show


------------------------------------------------------------------------
-- AddrSymMap

-- | Maps code addresses to the associated symbol name if any.
type AddrSymMap w = Map.Map (MemSegmentOff w) BSC.ByteString

------------------------------------------------------------------------
-- DropError

-- | An error that occured when droping byes.
data DropError
   = DropUnexpectedRelocation
   | DropInvalidAddr

dropErrorAsMemError :: MemAddr w -> DropError -> MemoryError w
dropErrorAsMemError a DropUnexpectedRelocation = UnexpectedRelocation a
dropErrorAsMemError a DropInvalidAddr          = InvalidAddr a

-- | Given a contiguous list of segment ranges and a number of bytes to drop, this
-- returns the remaining segment ranges or throws an error.
dropSegmentRangeListBytes :: forall w
                          .  MemWidth w
                          => [SegmentRange w]
                          -> Int
                          -> Either DropError [SegmentRange w]
dropSegmentRangeListBytes ranges 0 = Right ranges
dropSegmentRangeListBytes (ByteRegion bs : rest) cnt = do
  let sz = BS.length bs
  if sz > cnt then
    Right $ ByteRegion (BS.drop cnt bs) : rest
   else
    dropSegmentRangeListBytes rest (cnt - sz)
dropSegmentRangeListBytes (SymbolicRef _:rest) cnt = do
  let sz = addrSize (error "rangeSize nat evaluated" :: NatRepr w)
  if sz > cnt then
    Left DropUnexpectedRelocation
   else
    dropSegmentRangeListBytes rest (cnt - sz)
dropSegmentRangeListBytes (BSSRegion sz : rest) cnt =
  if toInteger sz > toInteger cnt then
    Right $ BSSRegion (sz - fromIntegral cnt) : rest
   else
    dropSegmentRangeListBytes rest (cnt - fromIntegral sz)
dropSegmentRangeListBytes [] _ =
  Left DropInvalidAddr

------------------------------------------------------------------------
-- MemoryError

-- | Type of errors that may occur when reading memory.
data MemoryError w
   = UserMemoryError (MemAddr w) !String
     -- ^ the memory reader threw an unspecified error at the given location.
   | InvalidInstruction (MemAddr w) ![SegmentRange w]
     -- ^ The memory reader could not parse the value starting at the given address.
   | AccessViolation (MemAddr w)
     -- ^ Memory could not be read, because it was not defined.
   | PermissionsError (MemAddr w)
     -- ^ Memory could not be read due to insufficient permissions.
   | UnexpectedRelocation (MemAddr w)
     -- ^ Read from location that partially overlaps a relocated entry
   | UnexpectedBSS (MemAddr w)
     -- ^ We unexpectedly encountered a BSS segment/section.
   | InvalidAddr (MemAddr w)
     -- ^ The data at the given address did not refer to a valid memory location.

instance MemWidth w => Show (MemoryError w) where
  show (UserMemoryError _ msg) = msg
  show (InvalidInstruction start contents) =
    "Invalid instruction at " ++ show start ++ ": " ++ showList contents ""
  show (AccessViolation a)   =
    "Access violation at " ++ show a ++ "."
  show (PermissionsError a)  =
    "Insufficient permissions at " ++ show a ++ "."
  show (UnexpectedRelocation a)   =
    "Attempt to read an unexpected relocation entry at " ++ show a ++ "."
  show (UnexpectedBSS a) =
    "Attempt to read zero initialized BSS memory at " ++ show a ++ "."
  show (InvalidAddr a) =
    "Attempt to interpret an invalid address: " ++ show a ++ "."

------------------------------------------------------------------------
-- Memory symbol

-- | Type for representing a symbol that has a defined location in
-- this memory.
data MemSymbol w = MemSymbol { memSymbolName :: !BS.ByteString
                               -- ^ Name of symbol
                             , memSymbolStart :: !(MemSegmentOff w)
                               -- ^ Address that symbol starts up.
                             , memSymbolSize :: !(MemWord w)
                               -- ^ Size of symbol as defined in table.
                             }

------------------------------------------------------------------------
-- Memory reading utilities

-- | This resolves a memory address into a segment offset pair if it
-- points to a valid pair.
resolveMemAddr :: Memory w -> MemAddr w -> Either (MemoryError w) (MemSegmentOff w)
resolveMemAddr mem addr =
  case asSegmentOff mem addr of
    Just p -> Right p
    Nothing -> Left (InvalidAddr addr)

-- | Return contents starting from location or throw a memory error if there
-- is an unaligned relocation.
addrContentsAfter :: Memory w
                  -> MemAddr w
                  -> Either (MemoryError w) [SegmentRange w]
addrContentsAfter mem addr = do
  addrWidthClass (memAddrWidth mem) $
    contentsAfterSegmentOff =<< resolveMemAddr mem addr

readByteString' :: MemWidth w
                => BS.ByteString
                -> [SegmentRange w]
                -> MemAddr w
                -> Word64
                -> Either (MemoryError w) BS.ByteString
readByteString' _ _ _ 0 = pure BS.empty
readByteString' _ [] addr _ = Left (InvalidAddr addr)
readByteString' prev (ByteRegion bs:rest) addr sz =
  if toInteger sz <= toInteger (BS.length bs) then
    pure $ prev <> BS.take (fromIntegral sz) bs
   else do
    let addr' = incAddr (fromIntegral (BS.length bs)) addr
    let sz' = sz - fromIntegral (BS.length bs)
    readByteString' (prev <> bs) rest addr' sz'
readByteString' _ (SymbolicRef{}:_) addr _ = do
  Left (UnexpectedRelocation addr)
readByteString' prev (BSSRegion cnt:rest) addr sz =
  if toInteger sz <= toInteger cnt then
    pure $ prev <> BS.replicate (fromIntegral sz) 0
   else do
    let addr' = incAddr (toInteger sz) addr
    let sz' = sz - fromIntegral cnt
    readByteString' (prev <> BS.replicate (fromIntegral cnt) 0) rest addr' sz'

-- | Attemtp to read a bytestring of the given length
readByteString :: Memory w -> MemAddr w -> Word64 -> Either (MemoryError w) BS.ByteString
readByteString mem addr sz = do
  l <- addrContentsAfter mem addr
  addrWidthClass (memAddrWidth mem) $ readByteString' BS.empty l addr sz

-- | Read an address from the value in the segment or report a memory error.
readAddr :: Memory w
         -> Endianness
         -> MemAddr w
         -> Either (MemoryError w) (MemAddr w)
readAddr mem end addr = addrWidthClass (memAddrWidth mem) $ do
  let sz = fromIntegral (addrSize addr)
  bs <- readByteString mem addr sz
  let Just val = addrRead end bs
  Right $ MemAddr 0 val

-- | Read a big endian word16
readWord8 :: Memory w -> MemAddr w -> Either (MemoryError w) Word8
readWord8 mem addr = bsWord8 <$> readByteString mem addr 1

-- | Read a big endian word16
readWord16be :: Memory w -> MemAddr w -> Either (MemoryError w) Word16
readWord16be mem addr = bsWord16be <$> readByteString mem addr 2

-- | Read a little endian word16
readWord16le :: Memory w -> MemAddr w -> Either (MemoryError w) Word16
readWord16le mem addr = bsWord16le <$> readByteString mem addr 2

-- | Read a big endian word32
readWord32be :: Memory w -> MemAddr w -> Either (MemoryError w) Word32
readWord32be mem addr = bsWord32be <$> readByteString mem addr 4

-- | Read a little endian word32
readWord32le :: Memory w -> MemAddr w -> Either (MemoryError w) Word32
readWord32le mem addr = bsWord32le <$> readByteString mem addr 4

-- | Read a big endian word64
readWord64be :: Memory w -> MemAddr w -> Either (MemoryError w) Word64
readWord64be mem addr = bsWord64be <$> readByteString mem addr 8

-- | Read a little endian word64
readWord64le :: Memory w -> MemAddr w -> Either (MemoryError w) Word64
readWord64le mem addr = bsWord64le <$> readByteString mem addr 8
