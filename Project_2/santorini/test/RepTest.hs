module RepTest where
import Test.HUnit
import SantoriniRep

-- Rename some of the constants for use in test cases
jwh = gJWallHeight
iwh = gIWallHeight

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
        ( IBoard
            { iturn = 1,
              ispaces =
                [ [1, 1, 1, 1, 1],
                  [1, 2, 1, 2, 1],
                  [1, 1, 1, 1, 1],
                  [1, 0, 1, 0, 1],
                  [1, 1, 1, 1, 1]
                ],
              iplayers =
                [ IPlayer { icard = Apollo, itokens = [IPt 1 1] }]
            }

        )
        ( fromJBoard
            JBoard
              { turn = Just 1,
                spaces =
                  Just
                    [ [1, 1, 1, 1, 1],
                      [1, 2, 1, 2, 1],
                      [1, 1, 1, 1, 1],
                      [1, 0, 1, 0, 1],
                      [1, 1, 1, 1, 1]
                    ],
                players =
                  [ JPlayer { card = "Apollo", tokens = Just [[2, 2]] }]
              }
        )
    )

-- Verify full, empty board case
fromJfe2fe =
  TestCase
    ( assertEqual
        "Assert that we can convert a full JBoard to a full IBoard (Flat ground)."
        ( IBoard
            { iturn = 1,
              ispaces =
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 1 3]},
                  IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 3 3]}
                ]
            }
        )
        ( fromJBoard
            JBoard
              { turn = Just 1,
                spaces =
                  Just
                    [ [0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0]
                    ],
                players =
                  [ JPlayer { card = "Apollo", tokens = Just [[2,2], [2,4]]},
                    JPlayer { card = "Apollo", tokens = Just [[4,2], [4,4]]}
                  ]
              }
        )
    )

-- Verify full, uneven board case
fromJfu2fu =
  TestCase
   ( assertEqual
       "Assert that we can convert a full JBoard to a full IBoard (Uneven ground)."
       ( IBoard
           { iturn = 1,
             ispaces =
               [ [2, 1, 1, 1, 1],
                 [1, 0, 1, 0, 1],
                 [1, 1, 1, 1, 1],
                 [1, 0, 1, 0, 1],
                 [1, 1, 1, 1, 2]
               ],
             iplayers =
               [ IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 3 3]},
                 IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 1 3]}
               ]
           }
       )
       ( fromJBoard
           JBoard
             { turn = Just 1,
               spaces =
                 Just
                   [ [2, 1, 1, 1, 1],
                     [1, 0, 1, 0, 1],
                     [1, 1, 1, 1, 1],
                     [1, 0, 1, 0, 1],
                     [1, 1, 1, 1, 2]
                   ],
               players =
                 [ JPlayer { card = "Apollo", tokens = Just [[4,2], [4,4]]},
                   JPlayer { card = "Apollo", tokens = Just [[2,2], [2,4]]}
                 ]
             }
       )
   )

-- Verify walls are translated properly.
fromJWalls =
  TestCase
   ( assertEqual
       "Assert that walls in a JBoard properly translate to walls in the IBoard."
       ( IBoard
           { iturn = 1,
             ispaces =
               [ [iwh, iwh, iwh, iwh, iwh],
                 [iwh, 0,   iwh, 0,   iwh],
                 [iwh, iwh, iwh, iwh, iwh],
                 [iwh, 0,   iwh, 0,   iwh],
                 [iwh, iwh, iwh, iwh, iwh]
               ],
             iplayers =
               [ IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 3 3]},
                 IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 1 3]}
               ]
           }
       )
       ( fromJBoard
           JBoard
             { turn = Just 1,
               spaces =
                 Just
                   [ [jwh, jwh, jwh, jwh, jwh],
                     [jwh, 0,   jwh, 0,   jwh],
                     [jwh, jwh, jwh, jwh, jwh],
                     [jwh, 0,   jwh, 0,   jwh],
                     [jwh, jwh, jwh, jwh, jwh]
                   ],
               players =
                 [ JPlayer { card = "Apollo", tokens = Just [[2,2], [4,4]]},
                   JPlayer { card = "Apollo", tokens = Just [[4,2], [2,4]]}
                 ]
             }
       )
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
        ( JBoard
            { turn = Just 1,
              spaces =
                Just
                  [ [1, 1, 1, 1, 1],
                    [1, 2, 1, 2, 1],
                    [1, 1, 1, 1, 1],
                    [1, 0, 1, 0, 1],
                    [1, 1, 1, 1, 1]
                  ],
              players =
                [ JPlayer { card = "Apollo", tokens = Just [[2, 2]] } ]
            }
        )
        ( toJBoard IBoard
            { iturn = 1,
              ispaces =
                [ [1, 1, 1, 1, 1],
                  [1, 2, 1, 2, 1],
                  [1, 1, 1, 1, 1],
                  [1, 0, 1, 0, 1],
                  [1, 1, 1, 1, 1]
                ],
              iplayers =
                [ IPlayer { icard = Apollo, itokens = [IPt 1 1] } ]
            }

        )
   )

-- Verify full, empty board case
toJfe2fe =
  TestCase
   ( assertEqual
       "Assert that we can convert a full JBoard to a full IBoard (Flat ground)."
        ( JBoard
            { turn = Just 1,
              spaces =
                Just
                  [ [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0],
                    [0, 0, 0, 0, 0]
                  ],
              players =
                [ JPlayer { card = "Apollo", tokens = Just [[2,2], [2,4]]},
                  JPlayer { card = "Apollo", tokens = Just [[4,2], [4,4]]}
                ]
            }
        )
        ( toJBoard IBoard
            { iturn = 1,
              ispaces =
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ],
              iplayers =
                [ IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 1 3]},
                  IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 3 3]}
                ]
            }
        )
   )

-- Verify full, uneven board case
toJfu2fu =
  TestCase
   ( assertEqual
       "Assert that we can convert a full JBoard to a full IBoard (Uneven ground)."
       ( JBoard
           { turn = Just 1,
             spaces =
               Just
                 [ [2, 1, 1, 1, 1],
                   [1, 0, 1, 0, 1],
                   [1, 1, 1, 1, 1],
                   [1, 0, 1, 0, 1],
                   [1, 1, 1, 1, 2]
                 ],
             players =
               [ JPlayer { card = "Apollo", tokens = Just [[4,2], [4,4]]},
                 JPlayer { card = "Apollo", tokens = Just [[2,2], [2,4]]}
               ]
           }
       )
       ( toJBoard IBoard
           { iturn = 1,
             ispaces =
               [ [2, 1, 1, 1, 1],
                 [1, 0, 1, 0, 1],
                 [1, 1, 1, 1, 1],
                 [1, 0, 1, 0, 1],
                 [1, 1, 1, 1, 2]
               ],
             iplayers =
               [ IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 3 3]},
                 IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 1 3]}
               ]
           }
       )
   )

-- Verify walls are translated properly.
toJWalls =
  TestCase
   ( assertEqual
       "Assert that walls in a JBoard properly translate to walls in the IBoard."
       ( JBoard
            { turn = Just 1,
              spaces =
                Just
                  [ [jwh, jwh, jwh, jwh, jwh],
                    [jwh, 0,   jwh, 0,   jwh],
                    [jwh, jwh, jwh, jwh, jwh],
                    [jwh, 0,   jwh, 0,   jwh],
                    [jwh, jwh, jwh, jwh, jwh]
                  ],
              players =
                [ JPlayer { card = "Apollo", tokens = Just [[2, 2], [4, 4]]},
                  JPlayer { card = "Apollo", tokens = Just [[4, 2], [2, 4]]}
                ]
            }
       )
       ( toJBoard IBoard
           { iturn = 1,
             ispaces =
               [ [iwh, iwh, iwh, iwh, iwh],
                 [iwh, 0,   iwh, 0,   iwh],
                 [iwh, iwh, iwh, iwh, iwh],
                 [iwh, 0,   iwh, 0,   iwh],
                 [iwh, iwh, iwh, iwh, iwh]
               ],
             iplayers =
               [ IPlayer { icard = Apollo, itokens = [IPt 1 1, IPt 3 3]},
                 IPlayer { icard = Apollo, itokens = [IPt 3 1, IPt 1 3]}
               ]
           }
       )
   )
