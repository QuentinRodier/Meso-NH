!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_PGD_ARRANGE_COVER(HPROGRAM,OWATER_TO_NATURE,OTOWN_TO_ROCK, &
     OTOWN_TO_COVER, IREPLACE_COVER)
!     ##########################################################################
!!
!!    PURPOSE
!!    -------
!!    initialyse change water (not lake) to nature and/or town to rock keys
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     05/03/09
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODE_POS_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM         ! program calling READ_PGD
LOGICAL,           INTENT(OUT) :: OWATER_TO_NATURE ! T: Change Wetland treated as inland water into nature
LOGICAL,           INTENT(OUT) :: OTOWN_TO_ROCK    ! T: Change Town into Rock 
LOGICAL,           INTENT(OUT) :: OTOWN_TO_COVER   ! T: Change Town into selected COVER 
INTEGER,           INTENT(OUT) :: IREPLACE_COVER   ! The COVER number to be used to replace the Town
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT  ! logical unit
INTEGER :: ILUNAM  ! logical unit
!
LOGICAL :: GFOUND
!
!*    0.2    local namelist variables
!            ------------------------
!
LOGICAL :: LWATER_TO_NATURE
LOGICAL :: LTOWN_TO_ROCK
LOGICAL :: LTOWN_TO_COVER
INTEGER :: NREPLACE_COVER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_PGD_ARRANGE_COVER/LWATER_TO_NATURE,LTOWN_TO_ROCK,LTOWN_TO_COVER,NREPLACE_COVER
!
!------------------------------------------------------------------------------
!
!*       1.    defaults
! 
IF (LHOOK) CALL DR_HOOK('READ_PGD_ARRANGE_COVER',0,ZHOOK_HANDLE)
!
LWATER_TO_NATURE = .FALSE.
LTOWN_TO_ROCK    = .FALSE.
LTOWN_TO_COVER   = .FALSE.
NREPLACE_COVER   = NUNDEF
!
!------------------------------------------------------------------------------
!
!*       2.    opening of namelist
! 
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*       3.    reading of namelist
! 
 CALL POSNAM(ILUNAM,'NAM_PGD_ARRANGE_COVER',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGD_ARRANGE_COVER)
!
!-------------------------------------------------------------------------------
!
!*       4.    initialize keys
! 
OWATER_TO_NATURE = LWATER_TO_NATURE
OTOWN_TO_ROCK    = LTOWN_TO_ROCK
OTOWN_TO_COVER   = LTOWN_TO_COVER
IREPLACE_COVER   = NREPLACE_COVER
!
!------------------------------------------------------------------------------
!
!*       5.    close namelist file
! 
CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ARRANGE_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_ARRANGE_COVER
