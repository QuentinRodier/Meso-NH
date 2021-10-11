!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_2M_10M (DUO, DFO, D, HSURF, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_2M_10M* - writes 2m and 10m diagnostics
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!      Original    01/2004
!!      Modified    01/2006 : sea flux parameterization.
!!      P.LeMoigne    04/2013 : Add accumulated diagnostics
!!      Modified    04/2013, P. Le Moigne: FLake chemistry
!!      S. Belamari 06/2014 : Introduce NBLOCK to avoid errors due to NBLOCK=0
!!                            when coupled with ARPEGE/ALADIN/AROME
!!      B. Decharme 02/2016 : NBLOCK instead of LCOUNTW for compilation in AAA
!!      V. MAsson   11/2018 : separate generic routine for 2M and 10m diagnostics
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
#ifdef SFX_OL
USE MODD_IO_SURF_OL, ONLY : LRESET_DIAG_ol=>LRESET_DIAG
#endif
!
#ifdef SFX_NC
USE MODD_IO_SURF_NC, ONLY : LRESET_DIAG_nc=>LRESET_DIAG
#endif
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
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
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DFO
TYPE(DIAG_t), INTENT(INOUT) :: D
!
CHARACTER(LEN=*),  INTENT(IN)  :: HSURF    ! type of surface
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
LOGICAL           :: GRESET
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_2M_10M',0,ZHOOK_HANDLE)
!
GRESET=.TRUE.
#ifdef SFX_ARO
GRESET=(NBLOCK>0)
#endif
#ifdef SFX_NC
IF (.NOT. LRESET_DIAG_nc) GRESET = .FALSE.
#endif
#ifdef SFX_OL
IF (.NOT. LRESET_DIAG_ol) GRESET = .FALSE.
#endif
!
!
!*       6.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (DFO%N2M>=1) THEN
  !
  YRECFM='T2M'//HSURF
  YCOMMENT='2 meters temperature'//' (K)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='T2MMIN'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)D%XT2M_MIN(:)=XUNDEF
  !
  YRECFM='T2MMAX'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)D%XT2M_MAX(:)=-XUNDEF
  !
  YRECFM='Q2M'//HSURF
  YCOMMENT='2 meters specific humidity'//' (KG/KG)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2M'//HSURF
  YCOMMENT='2 meters relative humidity'//' (KG/KG)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2MMIN'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)D%XHU2M_MIN(:)=XUNDEF
  !
  YRECFM='HU2MMAX'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)D%XHU2M_MAX(:)=-XUNDEF
  !
  YRECFM='ZON10M'//HSURF
  YCOMMENT='10 meters zonal wind'//' (M/S)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='MER10M'//HSURF
  YCOMMENT='10 meters meridian wind'//' (M/S)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10M'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10MMAX'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)D%XWIND10M_MAX(:)=0.0
  !
  ! Mean near surface air temperature [K]
  !
  IF (D%NCOUNT_STEP/=0) THEN
    YRECFM='T2MMEA'//HSURF
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M_MEAN(:)/D%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
    IF(GRESET) D%XT2M_MEAN(:)=0.0
    !
    ! Mean near surface specific humidity [KG/KG]
    !
    YRECFM='Q2MMEA'//HSURF
    YCOMMENT='2 meters specific humidity'//' (KG/KG)'
    CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XQ2M_MEAN(:)/D%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
    IF(GRESET) D%XQ2M_MEAN(:)=0.0
    !
    ! Mean near surface relative humidity [1]
    !
    YRECFM='HU2MMEA'//HSURF
    YCOMMENT='X_Y_'//YRECFM//' (-)'
    CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M_MEAN(:)/D%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
    IF(GRESET) D%XHU2M_MEAN(:)=0.0
    !
    ! Calculate vector average wind speed and direction at 10 m
    !
    D%XWIFF_MEAN = SQRT((D%XZON10M_MEAN(:)/D%NCOUNT_STEP)**2+(D%XMER10M_MEAN(:)/D%NCOUNT_STEP)**2)
    D%XWIDD_MEAN = 180.0 + (90.0-180.0*ATAN2(D%XMER10M_MEAN(:)/D%NCOUNT_STEP,D%XZON10M_MEAN(:)/D%NCOUNT_STEP)/3.141592)
    !
    WHERE (D%XWIDD_MEAN(:).GT.360.0)
      D%XWIDD_MEAN(:) = D%XWIDD_MEAN(:) - 360.0
    ENDWHERE
    !
    IF(GRESET) D%XZON10M_MEAN(:)=0.0
    IF(GRESET) D%XMER10M_MEAN(:)=0.0
  ENDIF
  !
  YRECFM='WFF10MM'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIFF_MEAN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WDD10MM'//HSURF
  YCOMMENT='X_Y_'//YRECFM//' (degrees)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIDD_MEAN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(GRESET) D%NCOUNT_STEP = 0
  !
END IF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_2M_10M',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_2M_10M
