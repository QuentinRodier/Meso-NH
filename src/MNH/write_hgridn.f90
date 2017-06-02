!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_WRITE_HGRIDn
!     ########################
INTERFACE
      SUBROUTINE WRITE_HGRID_n(TPFILE)
!
USE MODD_IO_ll, ONLY: TFILEDATA
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
!!      FMWRIT   : to write data in LFIFM file
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
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!      Module MODD_LUNIT_n :
!!        CLUOUT
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CONF
USE MODD_CONF_n
USE MODD_DIM_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODD_LUNIT_n
USE MODD_PARAMETERS
!
USE MODE_FM
USE MODE_FMWRIT
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
INTEGER               :: ILUOUT
INTEGER               :: IRESP
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
CALL IO_WRITE_FIELD(TPFILE,'LAT0',  CLUOUT,XLAT0)
CALL IO_WRITE_FIELD(TPFILE,'LON0',  CLUOUT,XLON0)
CALL IO_WRITE_FIELD(TPFILE,'RPK',   CLUOUT,XRPK)
CALL IO_WRITE_FIELD(TPFILE,'BETA',  CLUOUT,XBETA)
CALL IO_WRITE_FIELD(TPFILE,'LATORI',CLUOUT,XLATORI)
CALL IO_WRITE_FIELD(TPFILE,'LONORI',CLUOUT,XLONORI)
CALL IO_WRITE_FIELD(TPFILE,'IMAX',  CLUOUT,NIMAX)
CALL IO_WRITE_FIELD(TPFILE,'JMAX',  CLUOUT,NJMAX)
CALL IO_WRITE_FIELD(TPFILE,'XHAT',  CLUOUT,XXHAT)
CALL IO_WRITE_FIELD(TPFILE,'YHAT',  CLUOUT,XYHAT)
!
IF (CSTORAGE_TYPE=='TT') THEN
  CALL IO_WRITE_FIELD(TPFILE,'THINSHELL',CLUOUT,LTHINSHELL)
  CALL IO_WRITE_FIELD(TPFILE,'CARTESIAN',CLUOUT,LCARTESIAN)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID_n
