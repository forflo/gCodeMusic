------------------------------------------------------
-- Simple embedded domain specific language         --
-- that helps you to play music on your 3d printer! --
-- Author: Florian Mayer | Date: 28. June. 2016     --
------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Map as M;
import Data.Maybe

------------------------
-- Specification of EDSL
data SingleNote =
    C | CIS | D | DIS |
    E | F | FIS | G |
    GIS | A | AIS | H |
    C1 | CIS1 | D1 | DIS1 |
    E1 | F1 | FIS1 | G1 |
    GIS1 | A1 | AIS1 | H1 deriving (Show, Eq, Ord, Enum)

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
  BeginTransposed :: Sheet -> Integer -> MusicSheet

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

evalMusicSheet (BeginTransposed sheet amount) preferences =
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
  ++ show (floor (fraction * fromJust (M.lookup "duration" preferences)))
  ++ " ; pauses note\n"

generateGCode (e1 :+ e2) preferences =
  generateGCode e1 preferences ++ generateGCode e2 preferences

generateGCode (OneNote (note, duration)) preferences =
  let lookup = zip [C .. H1] (forwards $ fromJust (M.lookup "feedZ" preferences))
      fn = (calcLength (findFValue note lookup) duration preferences)
      feedrate = (ceiling $ findFValue note lookup)
      lenZ = (round' 10 fn)
  in
    "G1 F"
    ++ show feedrate
    ++ " ; sets feedrate to "
    ++ show feedrate ++ " mm/min"
    ++ "\nG1 Z"
    ++ show lenZ
    ++ " ; moves to Z="
    ++ show lenZ ++ "\n"

generateGCode (TwoNote (note1, note2, duration)) preferences =
  let feedTableX = zip [C .. H1] (forwards $ fromJust (M.lookup "feedX" preferences))
      feedTableY = zip [C .. H1] (forwards $ fromJust (M.lookup "feedY" preferences))
      feedrateX = findFValue note1 feedTableX
      feedrateY = findFValue note2 feedTableY
      lenX = round' 10 $ calcLength feedrateX duration preferences
      lenY = round' 10 $ calcLength feedrateY duration preferences
      feedrate = round' 10 $ (sqrt $ feedrateX ** 2 + feedrateY ** 2) / duration
  in
    "G1 X" ++ show lenX
    ++ " Y" ++ show lenY
    ++ " G1 F"
    ++ show feedrate
    ++ " ; sets feedrate to "
    ++ show feedrate ++ " mm/min\n"

generateGCode (ThreeNote (note1, note2, note3, duration)) preferences =
  let feedTableX = zip [C .. H1] (forwards $ fromJust (M.lookup "feedX" preferences))
      feedTableY = zip [C .. H1] (forwards $ fromJust (M.lookup "feedY" preferences))
      feedTableZ = zip [C .. H1] (forwards $ fromJust (M.lookup "feedZ" preferences))
      feedrateX = findFValue note1 feedTableX
      feedrateY = findFValue note2 feedTableY
      feedrateZ = findFValue note2 feedTableZ
      lenX = round' 10 $ calcLength feedrateX duration preferences
      lenY = round' 10 $ calcLength feedrateY duration preferences
      lenZ = round' 10 $ calcLength feedrateZ duration preferences
      -- TODO: this is not correct
      feedrate = round' 10 $ (sqrt $ feedrateX ** 2 + feedrateY ** 2 + feedrateZ ** 2) / duration
  in
    "G1 X" ++ show lenX
    ++ " Y" ++ show lenY
    ++ " Z" ++ show lenZ
    ++ " G1 F"
    ++ show feedrate
    ++ " ; sets feedrate to "
    ++ show feedrate ++ " mm/min\n"

---------------------------------------
-- helper functions for generateGCode
calcLength :: Double -> Double -> Preferences -> Double
calcLength velocity duration preferences =
  (fromJust (M.lookup "duration" preferences) * duration) *
  (velocity / 60) -- unit is mm/s

ceilingDbl :: Double -> Integer
ceilingDbl value =
  ceiling (toRational value)

findFValue :: SingleNote -> [(SingleNote, Double)] -> Double
findFValue note lookup =
  let isSearched = (\(x, _) -> x == note)
  in snd $ (filter isSearched lookup) !! 0

round' :: Integer -> Double -> Double
round' digits number =
  (fromInteger $ round $ number * (10 ^ digits)) /
  (10.0 ^^ digits)

forwards :: Double -> [Double]
forwards speed =
  let upperBound = 40
  in
    map (fromIntegral . ceiling) $
    map (\chromaticTone ->
           speed * 2 **
           (1/12 * fromInteger chromaticTone))
    [0 .. upperBound]

transposeSheet :: Sheet -> Integer -> Sheet
transposeSheet sheet amount =
  sheetMap (\x -> iterate succ x !! (fromInteger amount)) sheet

sheetMap :: (SingleNote -> SingleNote) -> Sheet -> Sheet
sheetMap f (OneNote (a, d)) = (OneNote (f a, d))
sheetMap f (TwoNote (a, b, d)) = (TwoNote (f a, f b, d))
sheetMap f (ThreeNote (a, b, c, d)) = (ThreeNote (f a, f b, f c, d))
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
  Title "Alle meine entchen" :-:
  BeginMusic
  (
    TwoNote (C, E, 1/2) :+ Pause 0.1 :+
    TwoNote (D, G, 1/2) :+ Pause 0.1 :|
    TwoNote (C, E, 1/2) :+ Pause 0.1 :+
    TwoNote (D, G, 1/2) :+ Pause 0.1 :+ ThreeNote (C, D, E, 1/4)
  )

--simple :: MusicSheet
--simple =
--  Begin 1 "Simple Test"
--  :| OneNote (C, 4) :+ OneNote (E, 4) :+ OneNote (G, 4) :| Pause 0.2
--  :| OneNote (C, 4) :+ OneNote (DIS, 4) :+ OneNote (G, 4) :| Pause 0.2

--fuerElise :: MusicSheet
--fuerElise =
--  Begin 1 "Fuer Elise"
--  :+ OneNote (E1, 1/4) :+ Pause 0.1
--  :+ OneNote (DIS1, 1/4) :+ Pause 0.1
--  :|
--     OneNote (E1, 1/4) :+ Pause 0.1
--  :+ OneNote (DIS1, 1/4) :+ Pause 0.1
--  :+ OneNote (E1, 1/4) :+ Pause 0.1
--  :+ OneNote (H, 1/4) :+ Pause 0.1
--  :+ OneNote (D1, 1/4) :+ Pause 0.1
--  :+ OneNote (C1, 1/4) :+ Pause 0.1
--  :|
--     OneNote (A, 1/4) :+ Pause 0.1
--  :+ Pause 0.4
--  :+ OneNote (C, 1/4) :+ Pause 0.1
--  :+ OneNote (E, 1/4) :+ Pause 0.1
--  :+ OneNote (A, 1/4) :+ Pause 0.1
--  :|
--     OneNote (H, 1/4) :+ Pause 0.1

--alleMeineEntchen :: MusicSheet
--alleMeineEntchen =
--  Begin 1 "Alle meine Entchen"
--  :+ OneNote (C, 1/4) :+ Pause 0.2
--  :+ OneNote (D, 1/4) :+ Pause 0.2
--  :+ OneNote (E, 1/4) :+ Pause 0.2
--  :+ OneNote (F, 1/4)
--  :| Pause 0.2
--  :+ OneNote (G, 1/2) :+ Pause 0.2
--  :+ OneNote (G, 1/2)
--  :| Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (G, 1/2)
--  :| Pause 0.5

--test :: MusicSheet
--test =
--  Begin "Alle meine Entchen"
--  :+ OneNote (C, 1/4) :+ Pause 0.2
--  :+ OneNote (D, 1/4) :+ Pause 0.2
--  :+ OneNote (E, 1/4) :+ Pause 0.2
--  :+ OneNote (F, 1/4) :+ Pause 0.2
--  :|
--  OneNote (G, 1/2) :+ Pause 0.2
--  :+ OneNote (G, 1/2) :+ Pause 0.2
--  :|
--  OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :|
--  OneNote (G, 1/1) :+ Pause 0.2
--  :|
--  OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :+ OneNote (A, 1/4) :+ Pause 0.2
--  :|
--  OneNote (G, 1/1) :+ Pause 0.2
--  :|
--  OneNote (F, 1/4) :+ Pause 0.2
--  :+ OneNote (F, 1/4) :+ Pause 0.2
--  :+ OneNote (F, 1/4) :+ Pause 0.2
--  :+ OneNote (F, 1/4) :+ Pause 0.2
--  :|
--  OneNote (E, 1/2) :+ Pause 0.2
--  :+ OneNote (E, 1/2) :+ Pause 0.2
--  :|
--  OneNote (D, 1/4) :+ Pause 0.2
--  :+ OneNote (D, 1/4) :+ Pause 0.2
--  :+ OneNote (D, 1/4) :+ Pause 0.2
--  :+ OneNote (D, 1/4) :+ Pause 0.2
--  :|
--  OneNote (C, 1/1)
