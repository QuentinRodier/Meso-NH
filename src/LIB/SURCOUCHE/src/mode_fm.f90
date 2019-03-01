!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s):
!
! Modifications:
!  D. Gazen    April 2016: change error message
!  P. Wautelet May 2016  : use NetCDF Fortran module
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 29/10/2018: better detection of older MNH version numbers
!  P. Wautelet 13/12/2018: moved some operations to new mode_io_*_nc4 modules
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN + move management
!                          of NNCID and NLFIFLU to the nc4 and lfi subroutines
!  P. Wautelet 21/01/2019: add LIO_ALLOW_NO_BACKUP and LIO_NO_WRITE to modd_io_ll
!                          to allow to disable writes (for bench purposes)
!  P. Wautelet 06/02/2019: simplify OPEN_ll and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_FILE_ADD2LIST
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_FILE_ADD2LIST)
!  P. Wautelet 19/02/2019: simplification/restructuration/cleaning of open/close subroutines (TBCto be continued)
!  P. Wautelet 27/02/2019: use recursive calls to open/close DES files
!  P. Wautelet 27/02/2019: remove CLOSE_ll subroutine (from mode_io.f90)
!-----------------------------------------------------------------

MODULE MODE_FM

use mode_io_ll
use mode_io_file

IMPLICIT NONE 

END MODULE MODE_FM
