module AIKernels where

import SantoriniRep (IBoard, AIKernel)

identityKernel :: IBoard -> IBoard
identityKernel = id
