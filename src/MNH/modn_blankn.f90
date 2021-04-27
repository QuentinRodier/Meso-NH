!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #################
      MODULE MODN_BLANK_n
!     #################
!
!!****  *MODN_BLANK$n* -  Namelist module for MesoNH developpers namelist
!!
!!    PURPOSE
!!    -------
!!
!!       The purpose of this module is to specify the namelist NAM_BLANKn
!!      which offer dummy real, integer, logical and character variables for
!!      test and debugging purposes.
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_BLANKn : contains declaration of dummy variables
!!
!!    REFERENCE
!!    ---------
!!      None
!!
!!    AUTHOR
!!    ------
!!	K. Suhre   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!! 
!!    Original 25/04/96
!!    Modification 14/12/00 (P.Jabouille) add dummy arrays
!!    Modification 26/10/21 (Q.Rodier) Use for n model (grid-nesting)
!!                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_BLANK_n, ONLY:&
        LDUMMY1_n => LDUMMY1, &
        LDUMMY2_n => LDUMMY2, &
        LDUMMY3_n => LDUMMY3, &
        LDUMMY4_n => LDUMMY4, &
        LDUMMY5_n => LDUMMY5, &
        LDUMMY6_n => LDUMMY6, &
        LDUMMY7_n => LDUMMY7, &
        LDUMMY8_n => LDUMMY8, &
        XDUMMY1_n => XDUMMY1, &
        XDUMMY2_n => XDUMMY2, &
        XDUMMY3_n => XDUMMY3, &
        XDUMMY4_n => XDUMMY4, &
        XDUMMY5_n => XDUMMY5, &
        XDUMMY6_n => XDUMMY6, &
        XDUMMY7_n => XDUMMY7, &
        XDUMMY8_n => XDUMMY8, &
        CDUMMY1_n => CDUMMY1, &
        CDUMMY2_n => CDUMMY2, &
        CDUMMY3_n => CDUMMY3, &
        CDUMMY4_n => CDUMMY4, &
        CDUMMY5_n => CDUMMY5, &
        CDUMMY6_n => CDUMMY6, &
        CDUMMY7_n => CDUMMY7, &
        CDUMMY8_n => CDUMMY8, &
        NDUMMY1_n => NDUMMY1, &
        NDUMMY2_n => NDUMMY2, &
        NDUMMY3_n => NDUMMY3, &
        NDUMMY4_n => NDUMMY4, &
        NDUMMY5_n => NDUMMY5, &
        NDUMMY6_n => NDUMMY6, &
        NDUMMY7_n => NDUMMY7, &
        NDUMMY8_n => NDUMMY8

!
IMPLICIT NONE
LOGICAL, SAVE :: LDUMMY1, LDUMMY2, LDUMMY3, LDUMMY4,&
                 LDUMMY5, LDUMMY6, LDUMMY7, LDUMMY8
CHARACTER*80, SAVE :: CDUMMY1, CDUMMY2, CDUMMY3, CDUMMY4,&
                 CDUMMY5, CDUMMY6, CDUMMY7, CDUMMY8
INTEGER, SAVE :: NDUMMY1, NDUMMY2, NDUMMY3, NDUMMY4,&
                 NDUMMY5, NDUMMY6, NDUMMY7, NDUMMY8
REAL, SAVE    :: XDUMMY1, XDUMMY2, XDUMMY3, XDUMMY4,&
                 XDUMMY5, XDUMMY6, XDUMMY7, XDUMMY8

!
NAMELIST/NAM_BLANKn/XDUMMY1,XDUMMY2,XDUMMY3,XDUMMY4,&
                     XDUMMY5,XDUMMY6,XDUMMY7,XDUMMY8,&
                     NDUMMY1,NDUMMY2,NDUMMY3,NDUMMY4,&
                     NDUMMY5,NDUMMY6,NDUMMY7,NDUMMY8,&
                     LDUMMY1,LDUMMY2,LDUMMY3,LDUMMY4,&
                     LDUMMY5,LDUMMY6,LDUMMY7,LDUMMY8,&
                     CDUMMY1,CDUMMY2,CDUMMY3,CDUMMY4,&
                     CDUMMY5,CDUMMY6,CDUMMY7,CDUMMY8
!
CONTAINS
!
SUBROUTINE INIT_NAM_BLANKn
  XDUMMY1 = XDUMMY1_n
  XDUMMY2 = XDUMMY2_n
  XDUMMY3 = XDUMMY3_n
  XDUMMY4 = XDUMMY4_n
  XDUMMY5 = XDUMMY5_n
  XDUMMY6 = XDUMMY6_n
  XDUMMY7 = XDUMMY7_n
  XDUMMY8 = XDUMMY8_n
!
  NDUMMY1 = NDUMMY1_n
  NDUMMY2 = NDUMMY2_n
  NDUMMY3 = NDUMMY3_n
  NDUMMY4 = NDUMMY4_n
  NDUMMY5 = NDUMMY5_n
  NDUMMY6 = NDUMMY6_n
  NDUMMY7 = NDUMMY7_n
  NDUMMY8 = NDUMMY8_n
!
  CDUMMY1 = CDUMMY1_n
  CDUMMY2 = CDUMMY2_n
  CDUMMY3 = CDUMMY3_n
  CDUMMY4 = CDUMMY4_n
  CDUMMY5 = CDUMMY5_n
  CDUMMY6 = CDUMMY6_n
  CDUMMY7 = CDUMMY7_n
  CDUMMY8 = CDUMMY8_n
!
  LDUMMY1 = LDUMMY1_n
  LDUMMY2 = LDUMMY2_n
  LDUMMY3 = LDUMMY3_n
  LDUMMY4 = LDUMMY4_n
  LDUMMY5 = LDUMMY5_n
  LDUMMY6 = LDUMMY6_n
  LDUMMY7 = LDUMMY7_n
  LDUMMY8 = LDUMMY8_n
END SUBROUTINE INIT_NAM_BLANKn
!
SUBROUTINE UPDATE_NAM_BLANKn
 XDUMMY1_n = XDUMMY1
 XDUMMY2_n = XDUMMY2
 XDUMMY3_n = XDUMMY3
 XDUMMY4_n = XDUMMY4
 XDUMMY5_n = XDUMMY5
 XDUMMY6_n = XDUMMY6
 XDUMMY7_n = XDUMMY7
 XDUMMY8_n = XDUMMY8
!
 NDUMMY1_n = NDUMMY1
 NDUMMY2_n = NDUMMY2
 NDUMMY3_n = NDUMMY3
 NDUMMY4_n = NDUMMY4
 NDUMMY5_n = NDUMMY5
 NDUMMY6_n = NDUMMY6
 NDUMMY7_n = NDUMMY7
 NDUMMY8_n = NDUMMY8
!
 CDUMMY1_n = CDUMMY1
 CDUMMY2_n = CDUMMY2
 CDUMMY3_n = CDUMMY3
 CDUMMY4_n = CDUMMY4
 CDUMMY5_n = CDUMMY5
 CDUMMY6_n = CDUMMY6
 CDUMMY7_n = CDUMMY7
 CDUMMY8_n = CDUMMY8
!
 LDUMMY1_n = LDUMMY1
 LDUMMY2_n = LDUMMY2
 LDUMMY3_n = LDUMMY3
 LDUMMY4_n = LDUMMY4
 LDUMMY5_n = LDUMMY5
 LDUMMY6_n = LDUMMY6
 LDUMMY7_n = LDUMMY7
 LDUMMY8_n = LDUMMY8
END SUBROUTINE UPDATE_NAM_BLANKn

END MODULE MODN_BLANK_n
