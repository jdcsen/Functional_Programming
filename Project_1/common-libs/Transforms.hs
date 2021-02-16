{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Transforms where
import Data.Data
import FTransforms

--
-- Our Sudoku boards are defined as a sequence of validity and validity non-preserving
-- transformations used to build and solve a sudoku board, with an implicit "empty"
-- board at the beginning of each transformation chain.
--
-- Transform/Boards have a few special forms.
-- to reformat itself in the form of an NxN patch array that can be batch-applied
-- to another NxN array. Certain functions only accept cannon forms, such as the print
-- and detonate functions. "print" does what it says on the tin, and "detonate"
-- is a transform that
-- of the board post-transformation.
--
--
-- In essence, boards are transformations, and transformations are boards. Loading
-- a board from a file is a transformation, but with a special evaluation function
-- that strips IO. IO transformations need to be eval'd before introduction into
-- an opchain, but all other evaluation can be done on a lazy, ad-hoc basis to
-- facilitate parallelization.
--
-- This allows us to easily compose and partially compute transformations, with an
-- eye towards whether or not a given Transform opchain produces a board we can
-- trust as being "valid", so we can put off the expensive explode op required to
-- verify validity.

data Transform where
  VP :: VPTransform -> Transform
  VN :: VNTransform -> Transform
  TG :: FTagR  -> Transform

-- Decomposing a Transform involves decomposing its components.
-- TODO: Monoid the hell out of this.
instance FDecomposable Transform where
  dcmpf (VP tform) = dcmpf tform
  dcmpf (VN tform) = dcmpf tform
  dcmpf (TG tform) = FTag tform

-- Note: We can't assemble bare Transform types, decomposing to FTransform
-- strips validity preservation information.

-- Overall Transform Semigroup
-- Note: Is this still a mathematically sound Semigroup? Certain associative ops
--       are poorly defined. Maybe it needs fleshing out, or maybe I'll just leave
--       unreasonable associative ops as the identity function. We'll see how it
--       works in production.
instance Semigroup Transform where

  -- When we associate a VN transform with a VP transform, we taint the VP Transform
  (<>)  (VN tforml) (VP tformr) = VN $ tforml <> VNT tformr
  (<>)  (VP tforml) (VN tformr) = VN $ VNT tforml <> tformr

  -- Transforms which are taint-homogeneous are handled by their respective subgroups.
  (<>)  (VP tforml) (VP tformr) = VP $ tforml <> tformr
  (<>)  (VN tforml) (VN tformr) = VN $ tforml <> tformr

  -- TODO: Through clever Semigroup constructors we can add
  -- and remove validity taint to our opchain.

  -- TODO: Move this.
  -- Note: We can't strip non-preserving state from Canonical forms without detonating
  --       them, which is a pretty expensive op to perform just to _see_ if we
  --       could sanitize some of the inputs of their VN taint. We'll just provide
  --       that functionality in a detonate-style function.


-- A Validity Preserving transformation is a transformation that's known to preserve
-- the validity of any Transform that it's applied to. This can be determined by
-- by nature of the transformations, or by verifying the opchain by detonating it.
data VPTransform where
  VPT  :: FTransform   -> VPTransform
  deriving (Typeable, Data)

-- mooooonnnnooooiiidd
instance FDecomposable VPTransform where
  dcmpf (VPT  tform) = tform

-- Shorthand for upcasting so we can just have a generic
-- Mo-nad, dude.
instance FAssemblable VPTransform where
  fassmbl a = VPT a

-- VPTransform Semigroup
instance Semigroup VPTransform where
  -- Pass all associative ops to our fundamental types.
  (<>) a b = fassmbl $ dcmpf a <> dcmpf b

-- Validity Non-preserving transformations are predominantly used when solving,
-- and will "taint" any validity preserving transformations they're associated
-- with, to let us know that we are no longer working with an opchain that's
-- assumed to be valid. VN chains CAN be valid, but must be carefully checked
-- before the VP Taint can be stripped and we can use the bare VP Transforms.
--
-- Currently, NP chains are validated en masse through the detonate function
-- in order to turn them into validity preserving chains.
--  TODO/Optimization Potential: We could decompose VN chains into known VP and
--                               speculatively NP ones in order to split work.
--
-- VNTransform canonical forms are provided by a wrapped VPC
--
-- The disambiguation between Validity Preserving and Non-Preserving transforms
-- allows us to easily implement backtracking by speculatively executing the frontier,
-- and the fact that opchains can be separated into Canonical forms and ones that
-- are explicitly lazily executed allows us to parallelize our work to
-- an _embarrassing_ degree.
data VNTransform where
  VNT    :: VPTransform   -> VNTransform
  deriving (Typeable, Data)

--   TODO: Monoid it up
instance FDecomposable VNTransform where
  -- Just let VPTransform decompose it.
  dcmpf (VNT tform) = dcmpf tform

-- Assembling VN types is just assembling from VP, with a VN wrapper.
-- Come on, now. Monads.
instance FAssemblable VNTransform where
  fassmbl a = VNT (VPT a)

-- VNTransform Semigroup
instance Semigroup VNTransform where
  -- Let the VPTransform functions handle our associative ops
  (<>) (VNT l_tform) (VNT r_tform) = new_tform
    where new_tform = VNT $ l_tform <> r_tform


-- detonate returns the following values:
--  * A transformation, either VP or VN, depending on the concrete state of the
--    board. If a VN transformation is returned, the transformation chain results
--    in an invalid board, and vise versa
--  * A set of potential
detonate :: Transform -> (Transform, [Transform])
detonate (VP (VPT (FCForm  arr))) = exploded
  where exploded = (VP (VPT (FCForm arr)), []) --TODO: Implement
detonate (VN (VNT (VPT (FCForm arr)))) = exploded
  where exploded = (VN (VNT (VPT (FCForm arr))), []) --TODO: Implement

-- Some old code, just for fun.

--data ExplodedBoard = ExplodedBoard { row_exclusion :: [[Integer]]
--                                   , col_exclusion :: [[Integer]]
--                                   , box_exclusion :: [[Integer]]
--                                   , source_board :: Board
--                                   }
--
--explodeBoard :: Board -> Maybe ExplodedBoard
--
--explodeBoard board
--  | valid     = explodedBoard
--  | otherwise = Nothing
--  where explodedBoard = Just (ExplodedBoard [[toInteger 0]] [[toInteger 0]] [[toInteger 0]] board)
--        valid = False


--_insertValues :: [(Int, Int, Integer)] -> Board -> Board
--_insertValues values (Board board_core max_row max_col) = board
--  where row_vals = groupBy (\(row1, col1, val1) -> (row2, col2, val2) -> row1 == row2) values -- Since our boards are row major, group the values by row.
--        old_row    = board_core ! row
--        new_row    = old_row // [(col, value)]
--        board      = Board (board_core // [(row, new_row)]) max_row max_col


---- Convert transforms to their canonical form.
---- Two transforms with identical canonical forms are identical.
--canonize :: Transform -> CTransform
--
---- Validity preserving transforms
--canonize (VP (VPTransform tforms)) = canon_tform
--  where canon_tform = VPC (listArray (0,0) [] )  -- TODO: Implement, force (validity preserving) insert.
--
---- Validity non-preserving transforms
--canonize (VN (VNTransform tforms)) = canon_tform
--  where canon_tform = VNCTransform (listArray (0,0) [] ) -- TODO: Implement, force (validity preserving) insert.



  -- Base Case: Sequences of identical type are merged.
--  (<>)  (VPSeq (as :|> a))
--        (VPSeq (bs :|> b))
--    | vptformTypeEq a b = VPSeq (as >< bs)
--
--  -- Base Case: Canon forms get lazily appended to.
--  -- Note: Order matters for us, but is there a more concise way to match this?
--  -- Special Case: Append to a list of the same type
--  (<>)  (VPC vpctformr)
--        (VPSeq (xs :|> VPC vpctforml)) = VPSeq (xs |> VPC vpctformr)
--
--  (<>)  (VPSeq (xs :|> VPC vpctformr))
--        (VPC vpctforml) = VPSeq (VPC vpctforml <| xs)
--
--  (<>)  (VPC vpctform) r_tform = new_tform
--    where new_tform = VPSeq (singleton (VPC vpctform) |> r_tform)
--
--  (<>)  l_tform (VPC vpctform)  = new_tform
--    where new_tform = VPSeq (singleton l_tform |> VPC vpctform)

  -- Defer work to the Canonize functions, lazily apply transformations.


-- LEGACY NOTES ###############################################################
--data Board = Board { board :: Array Int (Array Int Integer)
--                   , max_row :: Int
--                   , max_col :: Int
--                   } deriving (Eq, Show, Read)

-- Generates an empty board, as a base to apply transformations to.
--genBaseBoard :: Int -> Int -> Maybe Board
--genBaseBoard _ 0 = Nothing
--genBaseBoard 0 _ = Nothing
--genBaseBoard row col
--  | row <= 0 = Nothing
--  | col <= 0 = Nothing
--  | otherwise = Just board
--  where column     = array (0,row) [(0,0)]
--        board_core = array (0,row) [(0, column)]
--        board      = Board board_core row col

-- Transformations can be both validity-preserving, and validity non-preserving.
--  * The parameters for all transformations end in a board, to allow for
--    simple currying
--  * Board operations are implemented as Applicative Functors, to allow board
--    generation to be parallelized as well as possible.
--    * Applicative paramaters (Coords, TransformE) would allow us to group large
--      insert ops or canceling operations
--    * Extremely large boards could be split into 'byte' sized chunks: HAH!
--      * PLUS the immense parallelization potential in the metaprogramming for
--        the NP-Complete computation tree traversal
-- LEGACY NOTES ###############################################################


-- LEGACY NOTES ###############################################################
-- The data representation for the transform type
-- NOTE: Group transforms must be stored in some normal form, such that we don't
--       duplicate work. This can likely be done as we add to the list, making
--       the associative op refuse to add anything that would "reverse" a prior
--       op. Worst case N per associative op, How can I bring this down?
--        * Insert? Just don't insert into a populated cell while solving, and
--          don't add to a board when generating.
--        * Have a secondary list of "cancellable" ops? Ops that would be possible
--          to reverse with an identical op.
--            * Row and Col swaps can be reversed, unless there's an op of a different
--              type in between. On the other hand, entire SEQUENCES can be reversed.
--              * VN transforms can't be reversed by VP transforms, but VN transforms can reverse VP transforms.
--        * ALL ops can be transformed into sequences of Insert ops, given an actual board, that might be the way to check equivalence
--          * LONG chains for Row/Col swaps though
--            * Hash, then check?
--            * Have to associate a hash to multiple opchains/transforms to be fully correct.
-- LEGACY NOTES ###############################################################

---- Pattern match data constructor equality, since I couldn't get Data.Type.Equality to
---- work. (Note: Am I looking in the wrong place?)
--tformEq :: FTransform -> FTransform -> Bool
---- Determine Sequence equality recursively.
--tformEq (FSeq (as :|> a)) (FSeq (bs :|> b)) = tformEq a b
--tformEq (FCForm _ ) (FCForm _) = True
--tformEq (FUnit _ )  (FUnit _) = True
--tformEq (FTag _ )   (FTag _) = True


  -- Since CTransforms are intended to have the form CTransform -> Transform -> Transform,
  -- this needs to be done here, and can't be passed down to the CTransform Semigroup.
--  (<>)  CF          tformr = tformr <> CF
--  (<>)  (VP tforml) CF     = VP tforml -- TODO: Implement
--  (<>)  (VN tforml) CF     = VN tforml -- TODO: Implement


  -- TODO: Implement compression operators.
--  (<>)  CP tform       = tform <> CP
--  (<>)  (VP tforml) CP = VP tforml
--  (<>)  (VN tforml) CP = VN tforml
  -- NOTE: Compiler warning, since I think right now this only handles the CF case,
  -- which is already handled above. Uncomment if we add any more Transform types.
  --(<>)  tforml      CP = tforml -- All other forms are the identity.

  -- Homogeneous merges just call the root operators.
  -- Final pattern, catches undefined homogeneous cases.
  --(<>) (VP a) (VP b) = VP (a <> b) -- Hmmmmmm. This looks... Distributive...
  --  | (a b) (c d) =
