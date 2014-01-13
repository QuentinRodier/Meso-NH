!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!##################
MODULE MODE_SPLIT_GRID_PARAMETER
!##################
!
CONTAINS
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERX1(HPROGRAM,HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
!     #############################################################
!
!!****  * - routine to split a real array on the splitted grid 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_SPLIT_GRID_PARAMETER_OL
#endif
#ifdef MNH
USE MODI_SPLIT_GRID_PARAMETERX1_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),       INTENT(IN) :: HPROGRAM    ! calling program
 CHARACTER(LEN=10),      INTENT(IN) :: HGRID       ! grid type
 CHARACTER(LEN=6),       INTENT(IN) :: HREC        ! name of the parameter
INTEGER,                INTENT(IN) :: KDIM        ! size of PFIELD
INTEGER,                INTENT(IN) :: KSIZE       ! size of PFIELD_SPLIT
REAL, DIMENSION(KDIM ), INTENT(IN) :: PFIELD      ! real field for complete grid
REAL, DIMENSION(KSIZE), INTENT(OUT):: PFIELD_SPLIT! real field for splitted grid
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERX1',0,ZHOOK_HANDLE)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL SPLIT_GRID_PARAMETERX1_MNH(HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
#endif
ENDIF
!
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL SPLIT_GRID_PARAMETERX1_OL(HPROGRAM,HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERX1',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERX1
!
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERN0(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_SPLIT)
!     #############################################################
!
!!****  * - routine to define an integer related to splitted grid
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
USE MODE_SPLIT_GRID_PARAMETER_OL
#endif
#ifdef MNH
USE MODI_SPLIT_GRID_PARAMETERN0_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=10), INTENT(IN) :: HGRID        ! grid type
 CHARACTER(LEN=6),  INTENT(IN) :: HREC         ! name of the parameter
INTEGER,           INTENT(IN) :: KFIELD       ! integer scalar for complete grid
INTEGER,           INTENT(OUT):: KFIELD_SPLIT ! integer scalar for splitted grid
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERN0',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL SPLIT_GRID_PARAMETERN0_MNH(HGRID,HREC,KFIELD,KFIELD_SPLIT)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
  CALL SPLIT_GRID_PARAMETERN0_OL(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_SPLIT)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERN0',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERN0
!
END MODULE MODE_SPLIT_GRID_PARAMETER
