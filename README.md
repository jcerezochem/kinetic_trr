# kinetic_trr

Program to compute kinetic energy per atom 
(in cm-1) along a trajectory in trr format

This program uses the port of the xdr library
to read trr files from Fortran90 code from:
https://github.com/kmtu/xdrfort


## Third party programs
This project uses the following codes:

- xdr library (licensed under GPLv3):
trrlib/xdrfile.c
trrlib/lib/xdrfile.c
trrlib/lib/xdrfile_trr.c
trrlib/lib/xdrfile_xtc.c
trrlib/include/xdrfile.h
trrlib/include/xdrfile_trr.h
trrlib/include/xdrfile_xtc.h

The credits, from that project header read:
 * Copyright (c) Erik Lindahl, David van der Spoel 2003,2004.
 * Coordinate compression (c) by Frans van Hoesel.

- xdrfort: Port to Fortran90 of the xdr library (license under LGPL (not specified))
trrlib/xdr.f90

See more about the port in: https://github.com/kmtu/xdrfort

