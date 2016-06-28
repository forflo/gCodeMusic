--------------------------------------------------
-- Simple embedded domain specific language     --
-- that helps you to play music on your reprap! --
-- Author: Florian Mayer | Date: 28. June. 2016 --
--------------------------------------------------
{-# LANGUAGE GADTs #-}

------------------------
-- Specification of EDSL
data SingleNote =
    C | CIS | D | DIS |
    E | F | FIS | G |
    GIS | A | AIS | H |
    C1 | CIS1 | D1 | DIS1 |
    E1 | F1 | FIS1 | G1 |
    GIS1 | A1 | AIS1 | H1 deriving (Show, Eq, Ord, Enum)

type Duration = Double
type Notespec = (SingleNote, Duration)
type Seconds = Double
type Name = String

data GcodeMusic where
  Begin :: Name -> GcodeMusic
  Pause :: Seconds -> GcodeMusic
  Note :: Notespec -> GcodeMusic
  (:+) :: GcodeMusic -> GcodeMusic -> GcodeMusic
  (:|) :: GcodeMusic -> GcodeMusic -> GcodeMusic

infixl 4 :|
infixl 5 :+

instance Show GcodeMusic where
  show (Begin name) = "(Begin: " ++ show name ++ ")"
  show (Pause f) = "(Pause :" ++ show f ++ ")"
  show (Note n) = "(note: " ++ show n ++ ")"
  show (e1 :| e2) = "(" ++ show e1 ++ " :| " ++ show e2 ++ ")"
  show (e1 :+ e2) = "(" ++ show e1 ++ " :+ " ++ show e2 ++ ")"

---------------------
-- evaluator for EDSL
type ReferenceDuration = Double
type ReferenceSpeed = Integer
generateGCode :: ReferenceSpeed -> ReferenceDuration -> GcodeMusic -> String

generateGCode speed duration (e1 :| e2) =
  generateGCode speed duration e1 ++ generateGCode speed duration e2

generateGCode speed duration (Pause dbl) =
  "G4 P"
  ++ show (floor (dbl * 1000))
  ++ " ; pauses note\n"

generateGCode speed duration (e1 :+ e2) =
  error "currently unimplemented"

generateGCode _ _ (Begin name) =
  "G21 ; metric values\n" ++
  "G91 ; relative positioning\n" ++
  "M82 ; extruder absolute mode\n" ++
  "M117 " ++ name ++ " ; write neat message\n" ++
  "M107 ; fan off\n" ++
  "G28 X0 Y0\n" ++
  "G28 Z0\n\n"

generateGCode speed refDuration (Note (note, duration)) =
  let lookup = zip [C .. H1] (forwards speed)
      fn = (calcLength refDuration (findFValue note lookup) duration)
      round' digits number =
        (fromInteger $ round $ number * (10 ^ digits)) /
        (10.0 ^^ digits)
      feedrate = (ceiling $ findFValue note lookup)
      newCoordinate = (round' 2 fn)
  in
    "G1 F"
    ++ show feedrate
    ++ " ; sets feedrate to " ++ show feedrate ++ " mm/s"
    ++ "\nG1 X"
    ++ show newCoordinate
    ++ " ; moves to X="
    ++ show newCoordinate ++ "\n"

---------------------------------------
-- helper functions for generateGCode
calcLength:: ReferenceDuration -> Double -> Double -> Double
calcLength refDuration forwardFeed duration =
  (refDuration * duration) * (forwardFeed / 60)

findFValue :: SingleNote -> [(SingleNote, Double)] -> Double
findFValue note lookup =
  let isSearched = (\(x, _) -> x == note)
  in snd $ (filter isSearched lookup) !! 0

forwards :: ReferenceSpeed -> [Double]
forwards speed =
  map (fromIntegral . ceiling) $
  map (\chromaticTone ->
          (fromInteger speed) * 2 **
          (1/12 * fromInteger chromaticTone))
  [0 .. toInteger $ length [C .. H1]]

-----------------
-- simple example
-- Syntax for Notes:
--    Note (<note>, <duration, e.g. 1, 1/2, 1/4, 1/8 ...>)
simple :: GcodeMusic
simple =
  Begin "Simple Test"
  :| Note (C, 1) :| Pause 0.2
  :| Note (C1, 1)

alleMeineEntchen :: GcodeMusic
alleMeineEntchen =
  Begin "Alle meine Entchen"
  :| Note (C, 1/4) :| Pause 0.2
  :| Note (D, 1/4) :| Pause 0.2
  :| Note (E, 1/4) :| Pause 0.2
  :| Note (F, 1/4)
  :| Pause 0.2
  :| Note (G, 1/2) :| Pause 0.2
  :| Note (G, 1/2)
  :| Pause 0.2
  :| Note (A, 1/4) :| Pause 0.2
  :| Note (A, 1/4) :| Pause 0.2
  :| Note (A, 1/4) :| Pause 0.2
  :| Note (A, 1/4) :| Pause 0.2
  :| Note (G, 1/2)
  :| Pause 0.5

test :: GcodeMusic
test =
  Begin "Alle meine Entchen"
  :| Note (C, 1/2) :| Pause 0.2
  :| Note (D, 1/2) :| Pause 0.2
  :| Note (E, 1/2) :| Pause 0.2
  :| Note (F, 1/2) :| Pause 0.2
  :| Note (G, 1/2) :| Pause 0.2
  :| Note (G, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (G, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (A, 1/2) :| Pause 0.2
  :| Note (G, 1/2) :| Pause 0.2
  :| Note (F, 1/2) :| Pause 0.2
  :| Note (F, 1/2) :| Pause 0.2
  :| Note (F, 1/2) :| Pause 0.2
  :| Note (F, 1/2) :| Pause 0.2
  :| Note (E, 1/2) :| Pause 0.2
  :| Note (E, 1/2) :| Pause 0.2
  :| Note (D, 1/2) :| Pause 0.2
  :| Note (D, 1/2) :| Pause 0.2
  :| Note (D, 1/2) :| Pause 0.2
  :| Note (D, 1/2) :| Pause 0.2
  :| Note (C1, 1/2)
