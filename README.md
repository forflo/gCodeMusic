# A fun music EDSL

## What's this repo about?

Our faculty recently sponsored a second 3d printer
for our computer science laboratory.
The old one - a crappy german reprap - instantly
became the second choice, because it's inaccurate
as hell as the steppers lose steps every few milimeters.

I didn't want the printer to die alone on a scrapyard,
so taught him to play music :)

## Basic idea

3D printers usually interpret a simple numerical
code called `GCode`.

A typical `gcode` file looks like this:

```G-code
G21 ; metric values
G91 ; relative positioning
M82 ; extruder absolute mode
M117 My 3d model; write neat message
M107 ; fan off
G28 X0 Y0
G28 Z0

G1 F50 ; sets feedrate to 50 mm/min
G1 X0.21 ; moves to X=0.21
G4 P200 ; pauses for 200 ms
```

Down at the bottom of the above code sample,
you can see two lines beginning with the Command
`G1`. `G1` tells the gcode interpreter of the
printer to move to a specific coordiante.

If the command `G91` is present in the header
of the file, `G1` commands only act relative.
That means, if you'd write `G1 X10.2` the
printer would add 10.2 mm to the current
position of the X axis regardless of it's
current position.

With `G1`'s F parameter, you can also precisely
set the feed rate (unit mm per minute). With
```G-Code
G21 ; metric values
G91 ; relative positioning
G1 F120
G1 Z110
```
you'd instruct the interpreter to move
110 milimeter along the Z axis using a speed
of 2 mm/s. The printer would exacly need 55 seconds
to complete this operation.

## Usage

Lets start with a trivial but comprehensive example:

```Haskell
BeginSheet :-:
BaseFeedZ 50 :-:
BaseFeedX 499 :-:
BaseFeedY 500 :-:
ReferenceDuration 1 :-:
ResetAxis [X, Y, Z] :-:
Title "Alle meine Entchen" :-:
BeginMusic (
  OneNote (H1, 1/8) :+
  TwoNote (C, E, 1/2) :+
  ThreeNote (C, D, E, 1/4) :|
  Pause (1/8)
)
```

- `BeginSheet` is a nyadic function that outputs
  general gcode configuration commands

    ```G-code
    G21 ; metric values
    G91 ; relative positioning
    M82 ; extruder absolute mode
    ```

- `BaseFeedZ <real or fractional>` specifies what feed forward velocity
  is to be used for the deepest note (that would be C)
  on axis Z. Similarily, `BaseFeedY` and `BaseFeedX`
  specify the same values for axes *X* and *Y*

- `ReferenceDuration <real or fractional>` specifies the
  duration for one whole note in a four-four time in seconds

- `ResetAxis [<Axis>, ...]` outputs gcode to move given axes into
  their respective home positions, which would normally just be
  the absolute position value of 0

- `<Axis>` is one of the following *literal* names: `X`, `Y` or `Z`

- `Title <string>` outputs gcode that leads to the 3D printer
  displaying the specified string on it's status display

- `<real>` is just a normal floating point number

- `<fractional>`: This is a rational number with both numerator and denominator
  written as `(<numerator> / <denominator>)`, where `<numerator>` and
  `<denominator>` both have to be integers. Note that the parentheses are 
  syntactically crucial here

- `BeginMusic ( <Sheet> )` is used to group notes and pauses.

- `<Sheet>` only contains the following primitives:
  `OneNote (<Note>, <Duration>)`,
  `TwoNote (<Note>, <Note>, <Duration>)`,
  `ThreeNote (<Note>, <Note>, <Note>, <Duration>)`,
  `:+`, and `:|`

- `<Duration>` = `<real or fractional>`

- The operators `:+` and `:|` are used to chain `OneNote`,
  `TwoNote` and `ThreeNote` functions together. The effects
  of both operators are exactly equivalent and thus they can
  be used interchangeably. Despite this fact, however, 
  `:|` should be used to show where a bar line occurs and
  `:+` is intended to solely delimit individual notes.

- `<Note>` is one literal name from the following set `{`
    `C`,  `CIS`,  `D`,  `DIS`,
    `E`,  `F`,  `FIS`,  `G`,
    `GIS`,  `A`,  `AIS`,  `H`,
    `C1`,  `CIS1`,  `D1`,  `DIS1`,
    `E1`,  `F1`,  `FIS1`,  `G1`,
    `GIS1`,  `A1`,  `AIS1`,  `H1`,
    `C2`,  `CIS2`,  `D2`,  `DIS2`,
    `E2`,  `F2`,  `FIS2`,  `G2`,
    `GIS2`,  `A2`,  `AIS2`,  `H2`,
    `C3`,  `CIS3`,  `D3`,  `DIS3`,
    `E3`,  `F3`,  `FIS3`,  `G3`,
    `GIS3`,  `A3`,  `AIS3`,  `H3`,
    `C4`,  `CIS4`,  `D4`,  `DIS4`,
    `E4`,  `F4`,  `FIS4`,  `G4`,
    `GIS4`,  `A4`,  `AIS4`,  `H4` `}`

## Syntax reference

The complete haskell EDSL is described best using it's actual
implementation in Haskell itself. This shall be given as follows

```Haskell

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

data Axis =
    X | Y | Z deriving (Show, Eq)

data MusicSheet where
  BeginSheet :: MusicSheet
  BaseFeedZ :: Integer -> MusicSheet -- tune frequency
  BaseFeedX :: Integer -> MusicSheet -- tune freq
  BaseFeedY :: Integer -> MusicSheet -- tune freq
  ReferenceDuration :: Double -> MusicSheet
  ResetAxis :: [Axis] -> MusicSheet
  (:-:) :: MusicSheet -> MusicSheet -> MusicSheet
  Title :: String -> MusicSheet
  BeginMusic :: Sheet -> MusicSheet
  BeginTransposed :: Integer -> Sheet -> MusicSheet

infixr 3 :-:

data Sheet where
  Pause :: Double -> Sheet
  OneNote :: (SingleNote, Double) -> Sheet
  TwoNote :: (SingleNote, SingleNote, Double) -> Sheet
  ThreeNote :: (SingleNote, SingleNote, SingleNote, Double) -> Sheet
  (:+) :: Sheet -> Sheet -> Sheet
  (:|) :: Sheet -> Sheet -> Sheet

infixr 4 :|
infixr 5 :+
```
