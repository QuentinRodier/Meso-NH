!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_GREENROOF_n(HPROGRAM)
!     ###############################################
!
!!****  *WRITESURF_PGD_TEB_GREENROOF_n* - writes ISBA fields describing urban greenroofs
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
!!     A. Lemonsu & C. de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_GREENROOF_n,   ONLY : CISBA_GR, CSCOND_GR,      &
                                   NLAYER_GR, NTIME_GR,      &
                                   XRUNOFFB_GR, XWDRAIN_GR 
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
!
!* soil scheme option
!
YRECFM='GR_ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CISBA_GR,IRESP,HCOMMENT=YCOMMENT)
!
!* thermal conductivity option
!
YRECFM='GR_SCOND'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CSCOND_GR,IRESP,HCOMMENT=YCOMMENT)
!
!* number of soil layers
!
YRECFM='GR_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NLAYER_GR,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for green roof chacteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GR_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NTIME_GR,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GR_RUNOFFB'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRUNOFFB_GR,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GR_WDRAIN'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWDRAIN_GR,IRESP,HCOMMENT=YCOMMENT)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_GREENROOF_n
