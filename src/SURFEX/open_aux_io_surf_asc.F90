!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE OPEN_AUX_IO_SURF_ASC(HFILE,HFILETYPE,HMASK)
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
USE MODI_IO_BUFF_CLEAN_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_1D_MASK
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
INTEGER                        :: IRET, IL
REAL, DIMENSION(:),ALLOCATABLE :: ZFULL  ! total cover
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_ASC',0,ZHOOK_HANDLE)
 CALL IO_BUFF_CLEAN_n
 CALL GET_LUOUT('ASCII ',NLUOUT)
!
NUNIT=9
!
OPEN(UNIT=NUNIT,FILE=TRIM(HFILE)//'.txt',FORM='FORMATTED')
!
CMASK = HMASK
 CALL READ_SURF('ASCII ','DIM_FULL',NFULL,IRET)
ALLOCATE(NMASK(NFULL))
ALLOCATE(ZFULL(NFULL))
ZFULL=1.
 CALL GET_1D_MASK(NFULL,NFULL,ZFULL,NMASK)
DEALLOCATE(ZFULL)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_ASC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF_ASC
