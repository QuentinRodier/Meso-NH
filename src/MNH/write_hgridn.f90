!MNH_LIC Copyright 1996-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_WRITE_HGRIDn
!     ########################
INTERFACE
      SUBROUTINE WRITE_HGRID_n(TPFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE    ! File to write
!
END SUBROUTINE WRITE_HGRID_n
!
END INTERFACE
END MODULE MODI_WRITE_HGRIDn
!
!     ################################
      SUBROUTINE WRITE_HGRID_n(TPFILE)
!     ################################
!
!!****  *WRITE_HGRID_n* - to write grid information in FM file of model _n
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      IO_Field_write   : to write data in LFIFM file
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_GRID : contains projection definition
!!        XLAT0
!!        XLON0
!!        XRPK
!!        XBETA
!!        XLATORI
!!        XLONORI
!!      Module MODD_GRID_n : contains domain definition
!!        XXHAT
!!        XYHAT
!!      Module MODD_DIM_n : contains domain size
!!        NIMAX
!!        NJMAX
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        26/09/96
!!        V.Masson      18/08/97 call to fmwrit directly with dates and strings
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CONF
USE MODD_CONF_n
USE MODD_DIM_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_IO,             ONLY: TFILEDATA
!
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE    ! File to write
!
!
!*       0.2   declarations of local variables
!
! NONE
!-------------------------------------------------------------------------------
!
CALL IO_Field_write(TPFILE,'LAT0',  XLAT0)
CALL IO_Field_write(TPFILE,'LON0',  XLON0)
CALL IO_Field_write(TPFILE,'RPK',   XRPK)
CALL IO_Field_write(TPFILE,'BETA',  XBETA)
CALL IO_Field_write(TPFILE,'LATORI',XLATORI)
CALL IO_Field_write(TPFILE,'LONORI',XLONORI)
CALL IO_Field_write(TPFILE,'IMAX',  NIMAX)
CALL IO_Field_write(TPFILE,'JMAX',  NJMAX)
CALL IO_Field_write(TPFILE,'XHAT',  XXHAT)
CALL IO_Field_write(TPFILE,'YHAT',  XYHAT)
!
IF (CSTORAGE_TYPE=='TT') THEN
  CALL IO_Field_write(TPFILE,'THINSHELL',LTHINSHELL)
  CALL IO_Field_write(TPFILE,'CARTESIAN',LCARTESIAN)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID_n
