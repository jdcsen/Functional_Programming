{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FTransforms where
import Data.Array
import Data.Data
import Data.Maybe
import Data.Sequence

-- Quick aliasing for sequence length
slength = Data.Sequence.length
sdrop   = Data.Sequence.drop

-- Transform Coordinates
-- TFCoord <Row> <Column> <Value>
data TFCoord = TFCoord Int Int Int deriving (Data, Show)

-- Transform Type
data TFormE = RShuffle | -- Swaps two rows, VP
              CShuffle | -- Shuffles two rows, VP
              Insert     -- Inserts a single element, VP/NP
              deriving (Eq, Typeable, Data, Show)


-- Transformations are implemented as nested Semigroups, to organize the
-- definitions of our associate ops, and to provide clear delineations between
-- different groups that function differently. Associative operators that are
-- Homogeneous with regard to type are handled by the leaf Semigroups, and Heterogeneous
-- type association is handled by the top level Transform associate ops. The ops
-- in the leaf datatypes generally maintain state, though there are special cases
--
-- Special Transforms:
-- The CF and CP Transforms are special, in the sense that they're never intended
-- to appear in an opchain, rather they're types solely intended to be associated
-- to an opchain in order to reformat the opchain to a different data representation
-- more appropriate for certain actions.
--
-- Canonical Transforms #######################################################
--
-- Canonical Transforms force NP/VP forms to have constant time board indexing,
-- and as such, these canonical NP/VP forms are the only candidates for detonation,
-- and are the only things returned from I/O. However, Canonical  can not be
-- split down into component operations. In order to maximize parallelization,
-- one should opt for opchain representation until such a time that the memory
-- costs of the opchain representation outweigh those of the canonical form, and
-- when the opchain no longer needs to be decomposed into constituent parts,
-- given that it's an irreversible operation. When a more compact representation
-- is desired, this should be done by the application of a CPressTransform,
-- which will determine the size of the opchain, and append a CTransform if a
-- CTransform is warranted.
--
-- When transformations are associated with a CTransform the result is the input
-- operand on the left hand side, compressed into its canonical form. This
-- polymorhism allows us to apply a CTransform to any Transform, and have that
-- Transform convert itself into its canonical form.
--
--
-- Compression Transforms #####################################################
--
-- CPresTransforms were a post-hoc extension to the transform system, when it
-- became apparent that our opchain-style representation would eventually far
-- outpace our Canonical form in size for boards that are closer to being
-- completed. It was simple to add a transformation that, when appended, acts
-- polymorphically like both a CTransform or a VN/VPTransform, depending on
-- which representation is more efficient. Much like the Canonical Transforms,
-- the CPressTransforms themselves would never be found in an opchain, it simply
-- transforms itself into the proper Transformation when appended.

-- We define our Transforms in terms of certain fundamental datatypes, to allow
-- us to only write one set of representation-conscious association functions.
--   Note: Use 'type' and  'newtype' where we can.
-- A single operation transform.
data FUnitR  = FUnitR TFormE TFCoord deriving (Data, Show)
funit :: FTransform -> Maybe FUnitR
funit (FUnit unitr) = Just unitr
funit _ = Nothing

-- A sequence of transforms
type FSeqR   = Seq FTransform
fseq :: FTransform -> Maybe FSeqR
fseq (FSeq seq) = Just seq
fseq _ = Nothing

  -- Canonical form of a transform.
type FCFormR = Array Int (Array Int (Maybe Int))
fcform :: FTransform -> Maybe FCFormR
fcform (FCForm fcform) = Just fcform
fcform _ = Nothing

-- A "tag" transform, with no paramaters, intended to reformat the transform
-- into a different representation. FTags persist inside the transform, rather
-- they change the representation and evaporate. FTags are purely left associative,
-- and evaporate if applied to the left side of anything.
--data FTagF = FTagF (FTransform -> FTransform) deriving (Typeable, Data)
--data FTagR = FTagR TTagE FTagF  deriving (Typeable, Data)
-- Our different kinds of tag Transforms.
data FTagR where
  Identity   :: FTagR
  CanonForm  :: FTagR
  CPressForm :: FTagR
  Shift :: TFCoord -> FTagR
  deriving (Typeable, Data, Show)
ftag :: FTransform -> Maybe FTagR
ftag (FTag ttag) = Just ttag
ftag _ = Nothing


-- Metadata for the board. A post-hoc way of integrating comments (or even other metadata! JSON board info?) into the transform system.
type FMetaR = String
fmeta :: FTransform -> Maybe FMetaR
fmeta (FMeta str) = Just str
fmeta _ = Nothing

-- A class with a function to let us decompose transforms into their constituent
-- parts. I tried and tried to make the type system do this on its own, but after
-- hours and hours, I've either hit the limits of the Haskell type system with
-- my fanaticism, or I'm missing something. The latter is far more likely.
class FDecomposable a where
  dcmpf :: a -> FTransform

class FAssemblable a where
  fassmbl :: FTransform -> a

-- Since we use it to access our data, this data type should have as short
-- as possible data constructors so our pattern matches don't get stupid long.
data FTransform where
  FSeq   :: FSeqR   -> FTransform
  FCForm :: FCFormR -> FTransform
  FUnit  :: FUnitR  -> FTransform
  FTag   :: FTagR   -> FTransform
  FMeta  :: String  -> FTransform
  deriving (Typeable, Data, Show)

-- A set of methods to access the contents of an FTransform

-- Decomposing an FTransform is the identity
instance FDecomposable FTransform where
  dcmpf tform = tform

-- Helper to return the tail element of a transform (since we have weird, nested,
-- heterogeneous-outside-of-the-same-list FSeqs). We need this because I'd like
-- to know whether or not an append can be homogeneous before I actually do it,
-- in order to keep FTransforms ~reasonably~ flat. I don't know if that's just
-- my imperative brain butting in where it doesn't belong though.
ftail :: FTransform -> FTransform
ftail (FSeq (as :|> a)) = ftail a
ftail (FCForm a) = FCForm a
ftail (FUnit a) = FUnit a
ftail (FTag a) = FTag a
ftail (FMeta a) = FMeta a

-- Define a set of rules that dictate whether or not elements are merged
-- into the same sequence.
--
-- Through careful mergability rules, we can build a data structure where
-- all FSeqs contain either all FUnits of identical transform type, a single FCForm,
-- or a FSeq of the above types.
--
-- NOTE: Basically just a tail-head FTransform specific partial equality function.
--       Should I have done that (defined an equality function and tested tails
--       and heads) instead? Or is this just what that looks like when you choose
--       the typeclass-based design paradigm that I chose?

-- Functions to attempt specific merge types, which we can then prefer over each
-- other.
--  Note: Sort of just recursive subroutines for the fmerge recursive routine.
--        The call _back in_ to fmerge, but in a specific way. Is there a name for that?
-- fseqmerge: Combine two sequences of identical type.
-- fseqchunk: Combine two sequences by chopping off the ends of each and merging them.
-- fseqsplit: Combine two FTransforms into a single Sequence of two FTransforms
fseqmerge :: FSeqR -> FSeqR -> Maybe FSeqR
-- Merge empty sequences.
fseqmerge a     Empty = Just a
fseqmerge Empty b     = Just b

-- Merge sequences of length +1
fseqmerge (a_head :<| (aseq :|> a_tail))
          (b_head :<| (bseq :|> b_tail)) =
  if isJust (fmerge a_tail b_head) &&
     isJust (fmerge a_head b_tail)
  then Just $ singleton a_head >< aseq >< singleton a_tail ><
              singleton b_head >< bseq >< singleton b_tail -- Base case
  else Nothing

-- Merge single sequences
fseqmerge (Empty :|> a_tail)
          (b_head :<| (bseq :|> b_tail)) =
  if isJust (fmerge a_tail b_head) &&
     isJust (fmerge a_tail b_head)
  then Just $ singleton a_tail >< singleton b_head >< bseq >< singleton b_tail -- Base case
  else Nothing

fseqmerge (a_head :<| (aseq :|> a_tail))
          (b_head :<| Empty) =
  if isJust (fmerge a_tail b_head) &&
     isJust (fmerge a_head b_head)
  then Just $ singleton a_head >< aseq >< singleton a_tail >< singleton b_head -- Base case
  else Nothing

fseqmerge (Empty :|> a_tail)
          (b_head :<| Empty) =
  if isJust (fmerge a_tail b_head)
  then Just $ singleton a_tail >< singleton b_head -- Base case
  else Nothing

fseqchunk :: FSeqR -> FSeqR -> Maybe FSeqR
fseqchunk (aseq :|> a_tail)
          (b_head :<| bseq)
  | isJust chunked_seq = chunked_seq
  | otherwise = Nothing
  where aseq_l = FSeq $ sdrop (slength aseq - 1) aseq
        bseq_r = FSeq $ sdrop 0 bseq
        chunked_seq= do center_merge <- fmerge a_tail b_head
                        right_merge <- fmerge center_merge  bseq_r
                        chunked <- fmerge aseq_l right_merge
                        fseq chunked

fseqsplit :: FSeqR -> FSeqR -> FTransform
fseqsplit a b = FSeq (fromList [FSeq a, FSeq b])


--
-- NOTE: Only has the possibility of returning Nothing when called on non-containers.
fmerge :: FTransform -> FTransform -> Maybe FTransform
-- Metadata doesn't get merged at all.
fmerge (FMeta a) _         = Nothing
fmerge _         (FMeta b) = Nothing

-- Canonical forms don't get merged at all.
fmerge (FCForm a) _ = Nothing
fmerge _ (FCForm a) = Nothing

-- Tag function definitions (Note: I should have just defined a typeclass with
-- a special FTransform -> FTransform function, I think. That'd be cleaner.)
fmerge (FTag Identity) a = Just a
fmerge a (FTag Identity) = Just a

-- Canon Form applied to the left is a No-op
fmerge (FTag CanonForm) a = Just a
fmerge a (FTag CanonForm) = Just a --TODO: Implement

-- Shift Tags applied to the left are No-ops
fmerge (FTag (Shift _)) a      = Just a
fmerge a (FTag (Shift coords)) = Just cform
  where cform = fromJust $ fmerge a (FTag CanonForm) --TODO: Implement

-- All other tags just lay in the chain.
fmerge (FTag a) _               = Nothing
fmerge _               (FTag b) = Nothing

-- Unit forms get merged, IF they match TFormE type
--   Critical Rule: If you can merge with _a unit_, you can merge with the container
--                  that unit is in.
fmerge (FUnit (FUnitR rtype_e rcoord))
       (FUnit (FUnitR ltype_e lcoord))
  | rtype_e == ltype_e = Just $ FSeq $ fromList [FUnit (FUnitR rtype_e rcoord),
                                                 FUnit (FUnitR ltype_e lcoord)]
  | otherwise = Nothing

-- Merge empty sequences
fmerge (FSeq Empty) a = Just a
fmerge a (FSeq Empty) = Just a

-- Mergability is determined by the tail and head elements of the left and
-- right elements
--  -- If two 'far ends' (head of left, tail of right) can be merged, we can merge
--    the whole structure.
--  -- If only the tail of the left and the head of the right can be merged, we
--     should break off those elements and merge them.
--  -- If NOTHING can be merged, we just convert ourself into a container-of-
--     containers.
fmerge (FSeq seq_r)
       (FSeq seq_l)
  | isJust $ fseqmerge seq_r seq_l = Just $ FSeq $ fromJust $ fseqmerge seq_r seq_l
  | isJust $ fseqchunk seq_r seq_l = Just $ FSeq $ fromJust $ fseqchunk seq_r seq_l
  | otherwise = Just (fseqsplit seq_r seq_l)

-- Wrap single elements in a sequence.
fmerge (FSeq fseq) b = fmerge (FSeq fseq) (FSeq (singleton b))
fmerge a (FSeq fseq) = fmerge (FSeq (singleton a)) (FSeq fseq)

-- FMerged, Guaranteed! If we fail, we just split.
fmergeg :: FTransform -> FTransform -> FTransform
fmergeg a b
  | isJust $ fmerge a b = fromJust $ fmerge a b
  | otherwise           = FSeq (fromList [a, b])

instance Semigroup FTransform where
  -- Merge elements according to mergability rules.
  (<>) a b = fmergeg a b


