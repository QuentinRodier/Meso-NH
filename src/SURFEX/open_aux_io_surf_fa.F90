!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE OPEN_AUX_IO_SURF_FA(HFILE,HFILETYPE,HMASK)
!     #######################################################
!
!!****  *OPEN_AUX_IO_SURF_ASC* - chooses the routine to OPENialize IO
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
!!	V. Masson    *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_ASC,ONLY:NUNIT,CFILEIN,CFILEOUT,NMASK,NLUOUT,NFULL,CMASK
USE MODI_GET_LUOUT
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(:),POINTER  :: IMASK
INTEGER                        :: ILU,IRET, IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_FA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('ASCII ',NLUOUT)
!
NUNIT=9
!
OPEN(UNIT=NUNIT,FILE=HFILE,FORM='FORMATTED')
!
CMASK = HMASK
 CALL READ_SURF('ASCII ','DIM_FULL',ILU,IRET)
NFULL = ILU
!
!------------------------------------------------------------------------------
!
IL = NFULL
 CALL GET_TYPE_DIM_n(HMASK,IL)
ALLOCATE(IMASK(IL))
 CALL GET_SURF_MASK_n(HMASK,IL,IMASK,NFULL,NLUOUT)
!
ALLOCATE(NMASK(SIZE(IMASK)))
NMASK(:)=IMASK(:)
DEALLOCATE(IMASK)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_FA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF_FA
