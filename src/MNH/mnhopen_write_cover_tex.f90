!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##################################
      SUBROUTINE MNHOPEN_WRITE_COVER_TEX(KTEX)
!     ##################################
!
!!****  *MNHOPEN_WRITE_COVER_TEX* - opens cover listing file (in MESONH universe)
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF, ONLY : CPROGRAM
USE MODE_ll
USE MODE_IO_ll
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(OUT) :: KTEX ! logical unit of Tex file
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP
CHARACTER(LEN=20) :: YTEX           ! name of tex file
!-------------------------------------------------------------------------------
!
!*       5.     Prints of cover parameters in a tex file
!               ----------------------------------------
!
IF (CPROGRAM =='PGD   ') THEN
  YTEX='class_cover_data.tex'
  CALL OPEN_ll(unit=KTEX,file=YTEX,iostat=IRESP,action='WRITE', &
               form='FORMATTED',position="REWIND",mode=GLOBAL)
ELSE
  KTEX=0
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHOPEN_WRITE_COVER_TEX
