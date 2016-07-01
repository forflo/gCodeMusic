# A fun music EDSL

## What's this repo about?

Our faculty recently sponsored a second 3d printer
for our computer science laboratory.
The old one - a crappy german reprap - instantly
became the second choice, because it's inaccurate
as hell as the steppers lose torque every few milimeters.

I didn't want the printer to die alone on a scrapyard,
so taught him to play music :)

## Basic idea

3D printers usually interpret a simple numerical
code called `GCode`.

A typical `gcode` file looks like this:

```
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
```
G21 ; metric values
G91 ; relative positioning
G1 F120
G1 Z110
```
you'd instruct the interpreter to move
110 milimeter along the Z axis using a speed
of 2 mm/s. The printer would exacly need 55 seconds
to complete this operation.

