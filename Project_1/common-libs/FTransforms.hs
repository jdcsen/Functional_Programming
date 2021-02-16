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
data TFCoord = TFCoord Int Int Int deriving (Data)

-- Transform Type
data TFormE = RShuffle | -- Swaps two rows, VP
              CShuffle | -- Shuffles two rows, VP
              Insert   | -- Inserts a single element, VP/NP
              Shift     -- Moves a set of ops to a new location
                         --   Note: Post-hoc addition for board
                         --         parsing.
              deriving (Eq, Typeable, Data)


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
data FUnitR  = FUnitR TFormE TFCoord deriving (Data)
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
-- into a different representation. FTags don't generally persist inside the
-- transform, rather they change the representation and evaporate.
type FTagR   = TTagE
ftag :: FTransform -> Maybe TTagE
ftag (FTag ttag) = Just ttag
ftag _ = Nothing

-- Metadata for the board. A post-hoc way of integrating comments (or even other metadata! JSON board info?) into the transform system.
type FMetaR = String
fmeta :: FTransform -> Maybe FMetaR
fmeta (FMeta str) = Just str

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
  deriving (Typeable, Data)

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
fseqmerge (a_head :<| (aseq :|> a_tail))
          (b_head :<| (bseq :|> b_tail)) =
  if isJust (fmerge a_tail b_head) &&
     isJust (fmerge a_head b_tail)
  then Just $ aseq >< bseq -- Base case
  else Nothing

fseqchunk :: FSeqR -> FSeqR -> Maybe FSeqR
fseqchunk (a_head :<| (aseq :|> a_tail))
          (b_head :<| (bseq :|> b_tail))
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

-- Tags don't get merged (but really, they should never be there in the first place)
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
  | isJust $ fseqmerge seq_r seq_l = Just $ FSeq $ fromJust $ fseqmerge seq_l seq_r
  | isJust $ fseqchunk seq_r seq_l = Just $ FSeq $ fromJust $ fseqchunk seq_l seq_r
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

-- Our different kinds of tag Transforms.
data TTagE = Identity | CanonForm | CPressForm deriving (Eq, Data)

