!SFX_LIC Copyright 2010-2019 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CONVERT_PATCH_ALB_ISBA (DTCO, DTV, IO, KDEC, KDEC2, PCOVER, OCOVER,&
                                     HSFTYPE, KPATCH, KK, PK, PEK, OALBSOIL, OALBVEG, PWG1, PWSAT)
!     ##############################################################
!
!!**** *CONVERT_PATCH_ALB_ISBA* 
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    16/06/22 split vegetation albedo computation from convert_patch_isba.F90
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n,    ONLY : DATA_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_P_t, ISBA_PE_t, ISBA_K_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODD_DATA_COVER,     ONLY : XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,     &
                                XDATA_ALBUV_VEG,                        &
                                XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,   &
                                XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS, &
                                XDATA_HTRUNK_HVEG, XDATA_WCROWN_HVEG
!
!
USE MODI_AV_PGD_PARAM
USE MODI_AV_PGD_1P
USE MODI_SOIL_ALBEDO
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t),    INTENT(IN)    :: DTV
TYPE(ISBA_OPTIONS_t), INTENT(IN)    :: IO
TYPE(ISBA_K_t),       INTENT(INOUT) :: KK
TYPE(ISBA_P_t),       INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),      INTENT(INOUT) :: PEK
!
INTEGER,                 INTENT(IN) :: KDEC
INTEGER,                 INTENT(IN) :: KDEC2
REAL,    DIMENSION(:,:), INTENT(IN) :: PCOVER
LOGICAL, DIMENSION(:),   INTENT(IN) :: OCOVER
CHARACTER(LEN=*),        INTENT(IN) :: HSFTYPE ! nature / garden
INTEGER,                 INTENT(IN) :: KPATCH
!
LOGICAL,                 INTENT(IN) :: OALBSOIL  ! update bare soil albedo fields
LOGICAL,                 INTENT(IN) :: OALBVEG ! update only vegetation albedo
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(IN) :: PWG1
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PWSAT
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
CHARACTER(LEN=3)  :: YNAT, YVEG, YBAR
!
LOGICAL               :: GDATA     ! Flag where initialization can be done
!                                  ! either with ecoclimap of data fields specified
!                                  ! by user on the natural points (GDTA=T)
!                                  ! For fields in town, only ecoclimap option
!                                  ! is treated in this routine (GDATA=F)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ALB_ISBA',0,ZHOOK_HANDLE)
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (HSFTYPE=='NAT') THEN
!
  YNAT  = 'NAT'
  YVEG  = 'VEG'
  YBAR  = 'BAR'
  GDATA = .TRUE.
!
ELSEIF (HSFTYPE=='GRD') THEN
!
  YNAT  = 'GRD'
  YVEG  = 'GRV'
  YBAR  = 'GRB'
  GDATA = .FALSE.
!
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!
!*    3.      Time varying vegetation albedo only
!             -----------------------------------
!
IF (OALBVEG) THEN
  !
  IF (GDATA .AND. ANY(DTV%LDATA_ALBNIR_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBNIR_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBNIR_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_VEG,PCOVER,XDATA_ALB_VEG_NIR(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)    
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_VEG,PCOVER,XDATA_ALBNIR_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
  !
  IF (GDATA .AND. ANY(DTV%LDATA_ALBVIS_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBVIS_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBVIS_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_VEG,PCOVER,XDATA_ALB_VEG_VIS(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)      
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_VEG,PCOVER,XDATA_ALBVIS_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
  !
  IF ((IO%CALBEDO=='CM13'.OR.IO%LTR_ML)) THEN
    PEK%XALBUV_VEG(:)=PEK%XALBVIS_VEG(:)
  ELSEIF (GDATA .AND. ANY(DTV%LDATA_ALBUV_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBUV_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBUV_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBUV_VEG,PCOVER,XDATA_ALBUV_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    5.      Bare soil albedo
!             ----------------
!
IF (OALBSOIL) THEN
  !
  IF (GDATA .AND. ANY(DTV%LDATA_ALBNIR_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBNIR_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBNIR_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YBAR,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_SOIL,PCOVER,XDATA_ALB_SOIL_NIR(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YBAR,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "NIR" )  
  ENDIF
  !
  IF (GDATA .AND. ANY(DTV%LDATA_ALBVIS_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBVIS_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBVIS_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YBAR,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_SOIL,PCOVER,XDATA_ALB_SOIL_VIS(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YBAR,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "VIS" )
  ENDIF
  !
  IF (IO%CALBEDO=='CM13'.OR.IO%LTR_ML) THEN
    PEK%XALBUV_SOIL(:)=PEK%XALBVIS_SOIL(:)
  ELSEIF (GDATA .AND. ANY(DTV%LDATA_ALBUV_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBUV_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBUV_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "UV"  )  
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ALB_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONVERT_PATCH_ALB_ISBA
