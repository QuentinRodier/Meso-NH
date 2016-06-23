!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_TEB_FIELD (B, BOP, DTCO, IG, U, TG, T, TOP,GCP, &
                               HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!     #################################################################################
!
!
!!****  *PREP_HOR_TEB_FIELD* - reads, interpolates and prepares a TEB field
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!------------------------------------------------------------------
!
!
!
!
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_PREP,     ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, XLAT_OUT, XLON_OUT, &
                          XX_OUT, XY_OUT, CMASK
USE MODD_PREP_TEB, ONLY : XGRID_ROOF, XGRID_ROAD, XGRID_WALL, XGRID_FLOOR, LSNOW_IDEAL_TEB, &
                          XWSNOW_ROOF, XRSNOW_ROOF, XTSNOW_ROOF, XLWCSNOW_ROOF, XASNOW_ROOF, &
                          XWSNOW_ROAD, XRSNOW_ROAD, XTSNOW_ROAD, XLWCSNOW_ROAD, XASNOW_ROAD, &
                          XHUI_BLD, XHUI_BLD_DEF
!
USE MODD_CSTS, ONLY: XG, XP00
USE MODD_SURF_PAR, ONLY: XUNDEF
!
USE MODE_THERMOS
!
USE MODI_READ_PREP_TEB_CONF
USE MODI_READ_PREP_TEB_SNOW
USE MODI_PREP_TEB_GRIB
USE MODI_PREP_TEB_UNIF
USE MODI_PREP_TEB_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_EXTERN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
INTEGER,            INTENT(IN)  :: KPATCH
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, DIMENSION(:), ALLOCATABLE :: ZSG1SNOW, ZSG2SNOW, ZHISTSNOW, ZAGESNOW
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:) :: ZPS !surface pressure
REAL, PARAMETER               :: ZRHOA=1.19 ! volumic mass of air at 20C and 1000hPa
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_TEB_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                        HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
 CMASK = 'TOWN'
!
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_ROOF') THEN
  CALL READ_PREP_TEB_SNOW(HPROGRAM,T%CUR%TSNOW_ROOF%SCHEME,T%CUR%TSNOW_ROOF%NLAYER,&
                                   T%CUR%TSNOW_ROAD%SCHEME,T%CUR%TSNOW_ROAD%NLAYER,&
                                   YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE)
  IF (LEN_TRIM(YFILE)>0 .AND. LEN_TRIM(YFILETYPE)>0) GUNIF = .FALSE.                                   
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZAGESNOW(SIZE(XWSNOW_ROOF)))                                 
  CALL PREP_HOR_SNOW_FIELDS(DTCO, &
                            IG, U,GCP, &
                            HPROGRAM,HSURF,              &
                            YFILE,YFILETYPE,             &
                            YFILEPGD, YFILEPGDTYPE,      &
                            ILUOUT,GUNIF,1,KPATCH,       &
                            SIZE(TG%XLAT),T%CUR%TSNOW_ROOF, TOP%TTIME,&
                            XWSNOW_ROOF, XRSNOW_ROOF,    &
                            XTSNOW_ROOF, XLWCSNOW_ROOF,  &
                            XASNOW_ROOF,                 &
                            LSNOW_IDEAL_TEB, ZSG1SNOW,   &
                            ZSG2SNOW, ZHISTSNOW, ZAGESNOW)
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)
  DEALLOCATE(ZAGESNOW)                            
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (HSURF=='SN_ROAD') THEN
  CALL READ_PREP_TEB_SNOW(HPROGRAM,T%CUR%TSNOW_ROOF%SCHEME,T%CUR%TSNOW_ROOF%NLAYER,&
                                   T%CUR%TSNOW_ROAD%SCHEME,T%CUR%TSNOW_ROAD%NLAYER,&
                                   YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE)
  IF (LEN_TRIM(YFILE)>0 .AND. LEN_TRIM(YFILETYPE)>0) GUNIF = .FALSE.                                   
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZAGESNOW(SIZE(XWSNOW_ROAD)))                                   
  CALL PREP_HOR_SNOW_FIELDS(DTCO, &
                            IG, U,GCP, &
                            HPROGRAM,HSURF,              &
                            YFILE,YFILETYPE,             &
                            YFILEPGD, YFILEPGDTYPE,      &                            
                            ILUOUT,GUNIF,1,KPATCH,       &
                            SIZE(TG%XLAT),T%CUR%TSNOW_ROAD, TOP%TTIME,&
                            XWSNOW_ROAD, XRSNOW_ROAD,    &
                            XTSNOW_ROAD, XLWCSNOW_ROAD,  &
                            XASNOW_ROAD,                 &
                            LSNOW_IDEAL_TEB, ZSG1SNOW,   &
                            ZSG2SNOW, ZHISTSNOW, ZAGESNOW)
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)
  DEALLOCATE(ZAGESNOW)                               
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!
!*      4.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
  CALL PREP_TEB_UNIF(ILUOUT,HSURF,ZFIELDIN)
ELSE IF (YFILETYPE=='GRIB  ') THEN
  CALL PREP_TEB_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
 ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '.OR. YFILETYPE=='FA    ') THEN
  CALL PREP_TEB_EXTERN(DTCO,GCP, &
                       HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,KPATCH,ZFIELDIN)
 ELSE IF (YFILETYPE=='BUFFER') THEN
  CALL PREP_TEB_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
 ELSE
  CALL ABOR1_SFX('PREP_HOR_TEB_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!*      5.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT(SIZE(TG%XLAT),SIZE(ZFIELDIN,2)))
!
 CALL HOR_INTERPOL(DTCO, U,GCP, &
                   ILUOUT,ZFIELDIN,ZFIELDOUT)
!
!*      6.     Return to historical variable
!
SELECT CASE (HSURF)
 CASE('ZS     ') 
  ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
  XZS_LS(:) = ZFIELDOUT(:,1)
 CASE('WS_ROOF') 
  ALLOCATE(T%CUR%XWS_ROOF(SIZE(ZFIELDOUT,1)))
  T%CUR%XWS_ROOF(:) = ZFIELDOUT(:,1)
 CASE('WS_ROAD')
  ALLOCATE(T%CUR%XWS_ROAD(SIZE(ZFIELDOUT,1)))
  T%CUR%XWS_ROAD(:) = ZFIELDOUT(:,1)
 CASE('TI_ROAD') 
  ALLOCATE(T%CUR%XTI_ROAD(SIZE(ZFIELDOUT,1)))
  T%CUR%XTI_ROAD(:) = ZFIELDOUT(:,1)
 CASE('TI_BLD ') 
  ALLOCATE(B%CUR%XTI_BLD (SIZE(ZFIELDOUT,1)))
  B%CUR%XTI_BLD (:) = ZFIELDOUT(:,1)
 CASE('QI_BLD ') 
  ALLOCATE(B%CUR%XQI_BLD (SIZE(ZFIELDOUT,1)))
  IF (ALL(ZFIELDOUT .GE. XUNDEF-1.E+5 .AND. ZFIELDOUT .LE. XUNDEF+1.E+5)) THEN
     ALLOCATE(ZPS(SIZE(ZFIELDOUT,1)))
     ZPS = XP00 - ZRHOA * XG * XZS_LS
     IF (XHUI_BLD==XUNDEF) THEN
        ZFIELDOUT(:,1) = XHUI_BLD_DEF * QSAT(B%CUR%XTI_BLD, ZPS)
     ELSE
        ZFIELDOUT(:,1) = XHUI_BLD * QSAT(B%CUR%XTI_BLD, ZPS)
     ENDIF
     DEALLOCATE(ZPS)
  ENDIF
  B%CUR%XQI_BLD (:) = ZFIELDOUT(:,1)
 CASE('T_WIN1 ') 
  ALLOCATE(B%CUR%XT_WIN1 (SIZE(ZFIELDOUT,1)))
  B%CUR%XT_WIN1 (:) = ZFIELDOUT(:,1)
 CASE('T_WIN2 ') 
  ALLOCATE(B%CUR%XT_WIN2 (SIZE(ZFIELDOUT,1)))
  B%CUR%XT_WIN2 (:) = ZFIELDOUT(:,1)
 CASE('T_FLOOR')
  ALLOCATE(B%CUR%XT_FLOOR(SIZE(ZFIELDOUT,1),BOP%NFLOOR_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_FLOOR,ZFIELDOUT,B%CUR%XD_FLOOR,B%CUR%XT_FLOOR)
 CASE('T_MASS')
  ALLOCATE(B%CUR%XT_MASS(SIZE(ZFIELDOUT,1),BOP%NFLOOR_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_FLOOR,ZFIELDOUT,B%CUR%XD_FLOOR,B%CUR%XT_MASS)    
 CASE('T_ROAD ') 
  ALLOCATE(T%CUR%XT_ROAD(SIZE(ZFIELDOUT,1),TOP%NROAD_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_ROAD,ZFIELDOUT,T%CUR%XD_ROAD,T%CUR%XT_ROAD)
 CASE('T_WALLA')
  ALLOCATE(T%CUR%XT_WALL_A(SIZE(ZFIELDOUT,1),TOP%NWALL_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_WALL,ZFIELDOUT,T%CUR%XD_WALL,T%CUR%XT_WALL_A)
 CASE('T_WALLB')
  ALLOCATE(T%CUR%XT_WALL_B(SIZE(ZFIELDOUT,1),TOP%NWALL_LAYER))
  IF (TOP%CWALL_OPT=='UNIF') THEN
    T%CUR%XT_WALL_B = T%CUR%XT_WALL_A
  ELSE
    CALL INIT_FROM_REF_GRID(XGRID_WALL,ZFIELDOUT,T%CUR%XD_WALL,T%CUR%XT_WALL_B)
  END IF  
 CASE('T_ROOF ') 
  ALLOCATE(T%CUR%XT_ROOF(SIZE(ZFIELDOUT,1),TOP%NROOF_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_ROOF,ZFIELDOUT,T%CUR%XD_ROOF,T%CUR%XT_ROOF)
 CASE('T_CAN  ') 
  ALLOCATE(T%CUR%XT_CANYON(SIZE(ZFIELDOUT,1)))
  T%CUR%XT_CANYON (:) = ZFIELDOUT(:,1)
 CASE('Q_CAN  ') 
  ALLOCATE(T%CUR%XQ_CANYON(SIZE(ZFIELDOUT,1)))
  T%CUR%XQ_CANYON (:) = ZFIELDOUT(:,1)
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*      7.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUT)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
 CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! temperature profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! temperature profile
!
INTEGER                                  :: JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
REAL, DIMENSION(SIZE(PD2,1))             :: ZD  ! output total thickness
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
ZD2(:,:) = 0.
ZD (:)   = 0.
!
DO JL=1,SIZE(ZD2,2)
  ZD2(:,JL) = ZD(:) + PD2(:,JL)/2.
  ZD (:)    = ZD(:) + PD2(:,JL)
END DO
!
DO JL=1,SIZE(PT1,2)
  ZD1(:,JL) = PGRID1(JL) * ZD(:)
END DO
!
 CALL INTERP_GRID(ZD1,PT1,ZD2,PT2)
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_TEB_FIELD
