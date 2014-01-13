!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #####################################
      SUBROUTINE GET_TYPE_DIM_n(HTYPE,KDIM)
!     #####################################
!
!!****  *GET_TYPE_DIM_n* - routine to get the number of point for any surface type
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    -------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : XCOVER, XNATURE, XSEA, XWATER, XTOWN
!
USE MODI_CONVERT_COVER_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)      :: HTYPE    ! Type of surface
INTEGER,           INTENT(INOUT)   :: KDIM     ! size of the mask
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZSEA   ! sea cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZNATURE! nature cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZTOWN  ! town cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZWATER ! water cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZLAND  ! land cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZFULL  ! total cover
!
INTEGER           :: ILU    ! total horizontal size
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',0,ZHOOK_HANDLE)
IF (.NOT. ASSOCIATED(XCOVER) .AND. LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',1,ZHOOK_HANDLE)
IF (.NOT. ASSOCIATED(XCOVER)) RETURN
!
!*        1.    Fractions
!              ---------
!
ILU = SIZE(XCOVER,1)
!
ALLOCATE(ZSEA   (ILU))
ALLOCATE(ZNATURE(ILU))
ALLOCATE(ZTOWN  (ILU))
ALLOCATE(ZWATER (ILU))
ALLOCATE(ZLAND (ILU))
IF (.NOT. ASSOCIATED(XSEA)) THEN
  CALL CONVERT_COVER_FRAC(XCOVER,ZSEA,ZNATURE,ZTOWN,ZWATER)
ELSE
  ZSEA    = XSEA
  ZNATURE = XNATURE
  ZWATER  = XWATER
  ZTOWN   = XTOWN
END IF
ZLAND = ZTOWN + ZNATURE
!
ALLOCATE(ZFULL(ILU))
ZFULL=1.
!
SELECT CASE (HTYPE)
  CASE ('FULL  ')
   KDIM = ILU
   !
  CASE ('EXTZON')
   KDIM = ILU
   !
  CASE ('NATURE')
   KDIM = COUNT(ZNATURE(:) > 0.)
   !
  CASE ('SEA   ')
   KDIM = COUNT(ZSEA(:) > 0.)
   !
  CASE ('TOWN  ')
   KDIM = COUNT(ZTOWN(:) > 0.)
   !
  CASE ('WATER ')
   KDIM = COUNT(ZWATER(:) > 0.)
   !
  CASE ('LAND  ')
   KDIM = COUNT(ZLAND(:) > 0.)
   !
END SELECT
!-------------------------------------------------------------------------------
DEALLOCATE(ZSEA   )
DEALLOCATE(ZNATURE)
DEALLOCATE(ZTOWN  )
DEALLOCATE(ZWATER )
DEALLOCATE(ZFULL  )
DEALLOCATE(ZLAND  )
IF (LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_TYPE_DIM_n
