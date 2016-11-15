!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_COVER_n (DGU, &
                                     U, &
                                    HPROGRAM)
!     #################################
!
!!****  *WRITESURF_COVER_n* - writes cover fields
!!
!!    PURPOSE
!!    -------
!!       
!!
!!
!!**  METHOD
!!    ------
!!      
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!      M. Moge     02/2015 parallelization using WRITE_LCOVER
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODE_WRITE_SURF_COV, ONLY : WRITE_SURF_COV
!
USE MODI_WRITE_SURF
USE MODI_WRITE_LCOVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: IINFO
LOGICAL, DIMENSION(JPCOVER)    :: OCOVER   ! tmp list of covers
!
!-------------------------------------------------------------------------------
!
!*       1.     Cover classes :
!               -------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_COVER_N',0,ZHOOK_HANDLE)
!
YCOMMENT = '(-)'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'FRAC_SEA   ',U%XSEA,   IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'FRAC_NATURE',U%XNATURE,IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'FRAC_WATER ',U%XWATER, IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'FRAC_TOWN  ',U%XTOWN,  IRESP,HCOMMENT=YCOMMENT)
!
CALL WRITE_LCOVER(DGU,U,HPROGRAM,U%LCOVER)
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF_COV(DGU, U, &
                     HPROGRAM,'COVER',U%XCOVER(:,:),U%LCOVER,IRESP,HCOMMENT=YCOMMENT,HDIR='H')
!
!-------------------------------------------------------------------------------
!
!*       2.     Orography :
!               ---------
!
YRECFM='ZS'
YCOMMENT='X_Y_ZS (M)'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,U%XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_COVER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_COVER_n
