module RepTest where
import Test.HUnit
import SantoriniRep

-- Top level test case.
repTests = TestList [TestLabel "fromJBoard Tests" fromJBoardTests,
                     TestLabel "toJBoard Tests" toJBoardTests]

-- fromJBoard Test Cases.
fromJBoardTests = TestList [TestLabel "Empty to Empty" fromJempt2empt,
                            TestLabel "Single Player to Single Player" fromJsp2sp,
                            TestLabel "Full to Full (Flat)" fromJfe2fe,
                            TestLabel "Full to Full (Uneven)" fromJfu2fu,
                            TestLabel "Wall Translation" fromJWalls]

-- Verify empty case.
fromJempt2empt =
   TestCase 
    ( assertEqual
        "Assert that we can convert an empty JBoard to an empty IBoard."
        gIBoardEmpty
        (fromJBoard gJBoardEmpty)
    )

-- Verify single player case
fromJsp2sp =
   TestCase 
    ( assertEqual
        "Assert that we can convert an single player JBoard to a single player IBoard."
        gIBoardEmpty
        (fromJBoard gJBoardEmpty)
    )

-- Verify full, empty board case
fromJfe2fe =
   TestCase 
    ( assertEqual
        "Assert that we can convert a full JBoard to a full IBoard (Flat ground)."
        gIBoardEmpty
        (fromJBoard gJBoardEmpty)
    )

-- Verify full, uneven board case
fromJfu2fu =
   TestCase 
    ( assertEqual
        "Assert that we can convert a full JBoard to a full IBoard (Uneven ground)."
        gIBoardEmpty
        (fromJBoard gJBoardEmpty)
    )

-- Verify walls are translated properly.
fromJWalls =
   TestCase 
    ( assertEqual
        "Assert that walls in a JBoard properly translate to walls in the IBoard."
        gIBoardEmpty
        (fromJBoard gJBoardEmpty)
    )

-- toJBoard Test Cases.
toJBoardTests = TestList [TestLabel "Empty to Empty" toJempt2empt,
                            TestLabel "Single Player to Single Player" toJsp2sp,
                            TestLabel "Full to Full (Flat)" toJfe2fe,
                            TestLabel "Full to Full (Uneven)" toJfu2fu,
                            TestLabel "Wall Translation" toJWalls]

toJempt2empt =
   TestCase 
    ( assertEqual
        "Assert that we can convert an empty IBoard to an empty JBoard."
        gJBoardEmpty
        (toJBoard gIBoardEmpty)
    )

-- Verify single player case
toJsp2sp =
   TestCase 
    ( assertEqual
        "Assert that we can convert an single player JBoard to a single player IBoard."
        gJBoardEmpty
        (toJBoard gIBoardEmpty)
    )

-- Verify full, empty board case
toJfe2fe =
   TestCase 
    ( assertEqual
        "Assert that we can convert a full JBoard to a full IBoard (Flat ground)."
        gJBoardEmpty
        (toJBoard gIBoardEmpty)
    )

-- Verify full, uneven board case
toJfu2fu =
   TestCase 
    ( assertEqual
        "Assert that we can convert a full JBoard to a full IBoard (Uneven ground)."
        gJBoardEmpty
        (toJBoard gIBoardEmpty)
    )

-- Verify walls are translated properly.
toJWalls =
   TestCase 
    ( assertEqual
        "Assert that walls in a JBoard properly translate to walls in the IBoard."
        gJBoardEmpty
        (toJBoard gIBoardEmpty)
    )