!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SSO_n(HPROGRAM)
!     #################################
!
!!****  *WRITESURF_SSO_n* - writes orographic fields
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
USE MODD_SURF_ATM_SSO_n, ONLY : XAVG_ZS, XSSO_STDEV, XSIL_ZS,    &
                                  XSSO_ANIS, XSSO_DIR, XSSO_SLOPE, &
                                  XMIN_ZS, XMAX_ZS,                &
                                  XAOSIP, XAOSIM, XAOSJP, XAOSJM,  &
                                  XHO2IP, XHO2IM, XHO2JP, XHO2JM  
!
USE MODI_WRITE_SURF
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Orography :
!               ---------
!
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SSO_N',0,ZHOOK_HANDLE)
YRECFM='AVG_ZS'
YCOMMENT='X_Y_AVG_ZS (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_ZS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SIL_ZS'
YCOMMENT='X_Y_SIL_ZS (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSIL_ZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Subgrid Orography :
!               -----------------
!
YRECFM='SSO_STDEV'
YCOMMENT='X_Y_SSO_STDEV (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSSO_STDEV(:),IRESP,HCOMMENT=YCOMMENT)
!
!
YRECFM='MIN_ZS'
YCOMMENT='X_Y_MIN_ZS (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XMIN_ZS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='MAX_ZS'
YCOMMENT='X_Y_MAX_ZS (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XMAX_ZS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SSO_ANIS'
YCOMMENT='X_Y_SSO_ANIS (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSSO_ANIS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SSO_DIR'
YCOMMENT='X_Y_SSO_DIR (DEG)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSSO_DIR(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SSO_SLOPE'
YCOMMENT='X_Y_SSO_SLOPE (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSSO_SLOPE(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       3.     Subgrid Orography roughness:
!               ---------------------------
!
!
YRECFM='HO2IP'
YCOMMENT='X_Y_HO2IP (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2IP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JP'
YCOMMENT='X_Y_HO2JP (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2JP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2IM'
YCOMMENT='X_Y_HO2IM (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2IM(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JM'
YCOMMENT='X_Y_HO2JM (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2JM(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSIP'
YCOMMENT='X_Y_AOSIP (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSIP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJP'
YCOMMENT='X_Y_AOSJP (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSJP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSIM'
YCOMMENT='X_Y_AOSIM (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSIM(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJM'
YCOMMENT='X_Y_AOSJM (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSJM(:),IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_SSO_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SSO_n
