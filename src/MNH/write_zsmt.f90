!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 newsrc 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_WRITE_ZSMT_n
!     ######################
!
INTERFACE 
!
      SUBROUTINE WRITE_ZSMT_n(HFILE)
!
CHARACTER(LEN=28),   INTENT(IN)  :: HFILE ! name of the input/output file
!
END SUBROUTINE WRITE_ZSMT_n
!
END INTERFACE
!
END MODULE MODI_WRITE_ZSMT_n
!
!
!
!     #############################
      SUBROUTINE WRITE_ZSMT_n(HFILE)
!     #############################
!
!!****  *WRITE_ZSMT* writes smoothed orography for SLEVE coordinate
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        nov 2005
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
USE MODD_GRID_n,  ONLY : XZSMT
USE MODD_LUNIT_n, ONLY : CLUOUT
!
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER(LEN=28),   INTENT(IN)  :: HFILE ! name of the input/output file
!
!
!*       0.2   declarations of local variables
!
!
INTEGER           :: IRESP    ! return code for I/O
CHARACTER(LEN=16) :: YRECFM   ! name of record
INTEGER           :: IGRID    ! grid location
INTEGER           :: ILENCH   ! length of comment string
CHARACTER(LEN=100):: YCOMMENT ! comment string
!-------------------------------------------------------------------------------
!
!*       1.    writes smoothed orography in the file
!              -------------------------------------
!
YRECFM='ZSMT'
YCOMMENT='X_Y_ZSMT (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFILE,YRECFM,CLUOUT,'XY',XZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_ZSMT_n
