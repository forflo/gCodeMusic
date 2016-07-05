------------------------------------------------------
-- Simple embedded domain specific language         --
-- that helps you to play music on your 3d printer! --
-- Author: Florian Mayer | Date: 28. June. 2016     --
------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Map as M;
import Data.Maybe;
import Debug.Trace as T;

------------------------
-- Specification of EDSL
data SingleNote =
    C | CIS | D | DIS |
    E | F | FIS | G |
    GIS | A | AIS | H |
    C1 | CIS1 | D1 | DIS1 |
    E1 | F1 | FIS1 | G1 |
    GIS1 | A1 | AIS1 | H1 |
    C2 | CIS2 | D2 | DIS2 |
    E2 | F2 | FIS2 | G2 |
    GIS2 | A2 | AIS2 | H2 |
    C3 | CIS3 | D3 | DIS3 |
    E3 | F3 | FIS3 | G3 |
    GIS3 | A3 | AIS3 | H3 |
    C4 | CIS4 | D4 | DIS4 |
    E4 | F4 | FIS4 | G4 |
    GIS4 | A4 | AIS4 | H4 deriving (Show, Eq, Ord, Enum)

data Axis = X | Y | Z deriving (Show, Eq)

type Duration = Double
type Seconds = Double
type Name = String
type Feedrate = Integer

data MusicSheet where
  BeginSheet :: MusicSheet
  BaseFeedZ :: Integer -> MusicSheet -- tune frequency
  BaseFeedX :: Integer -> MusicSheet -- tune freq
  BaseFeedY :: Integer -> MusicSheet -- tune freq
  ReferenceDuration :: Seconds -> MusicSheet
  ResetAxis :: [Axis] -> MusicSheet
  (:-:) :: MusicSheet -> MusicSheet -> MusicSheet
  Title :: Name -> MusicSheet
  BeginMusic :: Sheet -> MusicSheet
  BeginTransposed :: Integer -> Sheet -> MusicSheet

infixr 3 :-:

data Sheet where
  Pause :: Duration -> Sheet
  OneNote :: (SingleNote, Duration) -> Sheet
  TwoNote :: (SingleNote, SingleNote, Duration) -> Sheet
  ThreeNote :: (SingleNote, SingleNote, SingleNote, Duration) -> Sheet
  (:+) :: Sheet -> Sheet -> Sheet
  (:|) :: Sheet -> Sheet -> Sheet

infixr 4 :|
infixr 5 :+

instance Show MusicSheet where
  show (ReferenceDuration duration) =
    "(Reference duration: )" ++ show duration ++ ")"
  show (BaseFeedZ feedrate) = "(BaseFeedZ" ++ show feedrate ++")"
  show (BaseFeedX feedrate) = "(BaseFeedX" ++ show feedrate ++")"
  show (BaseFeedY feedrate) = "(BaseFeedY" ++ show feedrate ++")"
  show (ResetAxis axis) = "(ResetAxis " ++ show axis ++ ")"
  show (BeginSheet) = "(BeginSheet)"
  show (m1 :-: m2) = "(" ++ show m1 ++ " :-: "++ show m2 ++ ")"
  show (Title name) = "(Title: " ++ show name ++ ")"
  show (BeginMusic music) = "(Music: " ++ show music ++ ")"
  show (BeginTransposed amount sheet) =
    "(TransposedMusic: " ++ show sheet ++
    " using transposition of " ++ show amount ++
    " halve tones)"

instance Show Sheet where
  show (Pause f) = "(Pause :" ++ show f ++ ")"
  show (OneNote n) = "(note: " ++ show n ++ ")"
  show (TwoNote n) = "(note: " ++ show n ++ ")"
  show (ThreeNote n) = "(note: " ++ show n ++ ")"
  show (e1 :| e2) = "(" ++ show e1 ++ " | " ++ show e2 ++ ")"
  show (e1 :+ e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"

---------------------
-- evaluator for EDSL
-- MusicSheet part
type Preferences = M.Map String Double
defaultPrefs :: Preferences
defaultPrefs = M.fromList []

evalMusicSheet :: MusicSheet -> Preferences -> String
evalMusicSheet (BeginSheet) _ =
  "G21 ; metric values\n" ++
  "G91 ; relative positioning\n" ++
  "M82 ; extruder absolute mode\n" ++
  "M107 ; fan off\n"

evalMusicSheet (Title name) _ =
  "M117 " ++ name ++ " ; writes name neat message\n"

evalMusicSheet (BaseFeedX feedrate :-: m2) preferences =
  "; Using X axis base feedrate: " ++ show feedrate ++ "\n"
  ++ evalMusicSheet m2 (M.insert "feedX" (fromInteger feedrate) preferences)

evalMusicSheet (BaseFeedY feedrate :-: m2) preferences =
  "; Using Y axis base feedrate: " ++ show feedrate ++ "\n"
  ++ evalMusicSheet m2 (M.insert "feedY" (fromInteger feedrate) preferences)

evalMusicSheet (BaseFeedZ feedrate :-: m2) preferences =
  "; Using Z axis base feedrate: " ++ show feedrate ++ "\n"
  ++ evalMusicSheet m2 (M.insert "feedZ" (fromInteger feedrate) preferences)

evalMusicSheet (ReferenceDuration duration :-: m2) preferences =
  "; Using " ++ show duration ++ "s as reference duration\n"
  ++ evalMusicSheet m2 (M.insert "duration" duration preferences)

evalMusicSheet (ResetAxis axisList) _ =
  concat $ map (\axis -> "G28 " ++ show axis ++ "0 ; reset axis\n") axisList

evalMusicSheet (BeginMusic sheet) preferences =
  generateGCode sheet preferences

evalMusicSheet (BeginTransposed amount sheet) preferences =
  generateGCode (transposeSheet sheet amount) preferences

evalMusicSheet (m1 :-: m2) preferences =
  evalMusicSheet m1 preferences ++ evalMusicSheet m2 preferences

-- Sheet part
generateGCode :: Sheet -> Preferences -> String
generateGCode (e1 :| e2) preferences =
  generateGCode e1 preferences ++ generateGCode e2 preferences

-- calculates the future position
generateGCode (OneNote (note, duration) :+ e2) preferences =
  let lookup = zip [C .. H1] (forwards $ fromJust (M.lookup "feedZ" preferences))
      fn = (calcLength (findFValue note lookup) duration preferences)
      newCoordinate = (round' 10 fn)
      futureZ = fromJust (M.lookup "futureZ" preferences) + newCoordinate
  in generateGCode e2 (M.insert "futureZ" futureZ preferences)

generateGCode (Pause fraction) preferences =
  "G4 P"
  ++ show (ceiling (1000 * fraction * fromJust (M.lookup "duration" preferences)))
  ++ " ; pauses note\n"

generateGCode (e1 :+ e2) preferences =
  generateGCode e1 preferences ++ generateGCode e2 preferences

generateGCode (OneNote (note, duration)) preferences =
  let ([(Z, lenZ)], feedrate) = getLF [Z] [note] preferences duration
  in "G1 F"
     ++ show feedrate
     ++ " ; sets feedrate to "
     ++ show feedrate ++ " mm/min"
     ++ "\nG1 Z"
     ++ show lenZ
     ++ " ; moves to Z="
     ++ show lenZ ++ "\n"

generateGCode (TwoNote (note1, note2, duration)) preferences =
  let ([(X, lenX), (Y, lenY)], feedrate) = getLF [X, Y] [note1, note2] preferences duration
  in "G1 X" ++ show lenX
     ++ " Y" ++ show lenY
     ++ " F"
     ++ show feedrate
     ++ "\n"

generateGCode (ThreeNote (note1, note2, note3, duration)) preferences =
  let ([(Z, lenZ), (X, lenX), (Y, lenY)], feedrate) =
        getLF [Z, X, Y] [note1, note2, note3] preferences duration
  in "G1 Z" ++ show lenZ
     ++ " X" ++ show lenX
     ++ " Y" ++ show lenY
     ++ " F"
     ++ show feedrate
     ++ "\n"

getLF :: [Axis] -> [SingleNote] -> Preferences -> Duration -> ([(Axis, Double)], Double)
getLF axis n preferences duration =
  let calc = \feedrate -> round' 10 $ calcLength feedrate duration preferences
      zipNF x = zip notes (forwards' x preferences)
      feedTable = map zipNF axis
      feedRates = zipWith findFValue n feedTable
      distances = map calc feedRates
      -- TODO: this is not correct
      feedrate = round' 10 (vectorLength feedRates)
  in (zip axis distances, feedrate)

---------------------------------------
-- helper functions for generateGCode
calcLength :: Double -> Double -> Preferences -> Double
calcLength velocity duration preferences =
  (fromJust (M.lookup "duration" preferences) * duration) *
  (velocity / 60) -- unit is mm/s

vectorLength :: [Double] -> Double
vectorLength vector =
  sqrt (sum (map (**2) vector))

notes :: [SingleNote]
notes = [C .. H4]

ceilingDbl :: Double -> Integer
ceilingDbl value =
  ceiling (toRational value)

findFValue :: SingleNote -> [(SingleNote, Double)] -> Double
findFValue note lookup =
  let isSearched = \(x, _) -> x == note
  in snd $ (filter isSearched lookup) !! 0

round' :: Integer -> Double -> Double
round' digits number =
  (fromInteger $ round $ number * (10 ^ digits)) /
  (10.0 ^^ digits)

forwards :: Double -> [Double]
forwards speed =
  let upperBound = toInteger (length notes)
  in map
     (\chromaticTone ->
        speed * 2 **
        (1/12 * fromInteger chromaticTone))
     [0 .. upperBound]

forwards' :: Axis -> Preferences -> [Double]
forwards' axis preferences =
  let upperBound = toInteger (length notes)
      axisToStr a = case (a) of
        X -> "feedX"
        Y -> "feedY"
        Z -> "feedZ"
      speed = fromJust $ M.lookup (axisToStr axis) preferences
  in map
     (\chromaticTone ->
        speed * 2 **
        (1/12 * fromInteger chromaticTone))
     [0 .. upperBound]

transposeSheet :: Sheet -> Integer -> Sheet
transposeSheet sheet amount
  | amount < 0 = sheetMap (transposer pred) sheet
  | amount > 0 = sheetMap (transposer succ) sheet
  | otherwise = sheet
  where transposer = \dir note -> iterate dir note !! (fromInteger $ abs amount)


sheetMap :: (SingleNote -> SingleNote) -> Sheet -> Sheet
sheetMap f (OneNote (a, d)) = (OneNote (f a, d))
sheetMap f (TwoNote (a, b, d)) = (TwoNote (f a, f b, d))
sheetMap f (ThreeNote (a, b, c, d)) = (ThreeNote (f a, f b, f c, d))
sheetMap f (l :+ r) = sheetMap f l :+ sheetMap f r
sheetMap f (l :| r) = sheetMap f l :| sheetMap f r
sheetMap f e = e

-----------------
-- simple example
-- Syntax for Notes:
--    Note (<note>, <duration, e.g. 1, 1/2, 1/4, 1/8 ...>)
complex :: MusicSheet
complex =
  BeginSheet :-:
  BaseFeedZ 50 :-:
  BaseFeedX 499 :-:
  BaseFeedY 500 :-:
  ReferenceDuration 1 :-:
  ResetAxis [X, Y, Z] :-:
  Title "Alle meine Entchen" :-:
  BeginMusic (
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :|
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :+ ThreeNote (C, D, E, 1/4)
  ) :-:
  ResetAxis [X, Y, Z] :-:
  BeginTransposed 2 (
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :|
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :+ ThreeNote (C, D, E, 1/4)
  )


testSheet :: Sheet
testSheet =
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :|
    TwoNote (C, E, 1/2) :+ Pause (1/2) :+
    TwoNote (D, G, 1/2) :+ Pause (1/2) :+ ThreeNote (C, D, E, 1/4)


simple :: MusicSheet
simple =
  BeginSheet :-:
  BaseFeedZ 50 :-:
  BaseFeedX 499 :-:
  BaseFeedY 500 :-:
  ReferenceDuration 2 :-:
  BeginMusic
  (
    ThreeNote (C, C1, C2, 2)
  )
