!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_HOR_TEB_GARDEN_FIELD* - reads, interpolates and prepares an ISBA field
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
!!      P. Le Moigne 03/2007, Ajout initialisation par ascllv
!!      B. Decharme  01/2009, Optional Arpege deep soil temperature initialization
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,            ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS,       &
                                 XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT,       &
                                 LINTERP, CMASK

USE MODD_PREP_TEB_GARDEN, ONLY : XGRID_SOIL, NGRID_LEVEL,                  &
                                 XWSNOW, XRSNOW, XTSNOW, XASNOW, LSNOW_IDEAL
USE MODD_TEB_n,           ONLY : TTIME
USE MODD_TEB_VEG_n,       ONLY : CISBA
USE MODD_TEB_GARDEN_n,    ONLY : XWG, XWGI, XTG, XWR, XLAI,                &
                                 NGROUND_LAYER,                            &
                                 XVEGTYPE, XDG, XWWILT, XWFC,              &
                                 XROOTFRAC, XWSAT, TSNOW
USE MODD_TEB_GRID_n,      ONLY : XLAT, XLON
USE MODD_ISBA_PAR,        ONLY : XWGMIN
USE MODD_DATA_COVER_PAR,  ONLY : NVEGTYPE
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_READ_PREP_TEB_GARDEN_CONF
USE MODI_READ_PREP_GARDEN_SNOW
USE MODI_PREP_TEB_GARDEN_ASCLLV
USE MODI_PREP_TEB_GARDEN_GRIB
USE MODI_PREP_TEB_GARDEN_UNIF
USE MODI_PREP_TEB_GARDEN_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_GARDEN_EXTERN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, POINTER,     DIMENSION(:,:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER,     DIMENSION(:,:)   :: ZFIELD ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUT ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZVEGTYPE_PATCH ! vegtype for each patch
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZW        ! work array (x, fine   soil grid)
REAL, ALLOCATABLE, DIMENSION(:)     :: ZSUM
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZF        ! work array (x, output soil grid)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZDG       ! out T grid (x, output soil grid)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZPATCH    ! work array for patches
REAL, ALLOCATABLE, DIMENSION(:)     :: ZSG1SNOW, ZSG2SNOW, ZHISTSNOW, ZAGESNOW
INTEGER                             :: ILUOUT    ! output listing logical unit
!
LOGICAL                             :: GUNIF     ! flag for prescribed uniform field
INTEGER                             :: JVEGTYPE  ! loop on vegtypes
INTEGER                             :: JLAYER    ! loop on layers
INTEGER                             :: JI
INTEGER                             :: IWORK     ! Work integer
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_TEB_GARDEN_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                               HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'TOWN  '
!
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_VEG ') THEN
  CALL READ_PREP_GARDEN_SNOW(HPROGRAM,TSNOW%SCHEME,TSNOW%NLAYER,YFILE,YFILETYPE)
  IF (LEN_TRIM(YFILE)>0 .AND. LEN_TRIM(YFILETYPE)>0) GUNIF = .FALSE.  
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW)))
  ALLOCATE(ZAGESNOW(SIZE(XWSNOW)))
  ALLOCATE(ZPATCH(SIZE(XVEGTYPE,1),1))
  ALLOCATE(ZVEGTYPE_PATCH (SIZE(XVEGTYPE,1),SIZE(XVEGTYPE,2),1))
  !
  ZPATCH=1.
  ZVEGTYPE_PATCH(:,:,1) = XVEGTYPE(:,:)
  CALL PREP_HOR_SNOW_FIELDS(HPROGRAM,HSURF,                 &
                            YFILE,YFILETYPE,                &
                            YFILEPGD, YFILEPGDTYPE,         &
                            ILUOUT,GUNIF,1,                 &
                            SIZE(XLAT),TSNOW, TTIME,        &
                            XWSNOW, XRSNOW, XTSNOW, XASNOW, &
                            LSNOW_IDEAL, ZSG1SNOW,          &
                            ZSG2SNOW, ZHISTSNOW, ZAGESNOW,  &
                            ZVEGTYPE_PATCH, ZPATCH          )
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)
  DEALLOCATE(ZAGESNOW)                            
  DEALLOCATE(ZPATCH)
  DEALLOCATE(ZVEGTYPE_PATCH)
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
  CALL PREP_TEB_GARDEN_UNIF(ILUOUT,HSURF,ZFIELDIN)
ELSE IF (YFILETYPE=='ASCLLV') THEN
  CALL PREP_TEB_GARDEN_ASCLLV(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='GRIB  ') THEN
  CALL PREP_TEB_GARDEN_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   ') THEN
   CALL PREP_TEB_GARDEN_EXTERN(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='BUFFER') THEN
   CALL PREP_TEB_GARDEN_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE
   CALL ABOR1_SFX('PREP_HOR_TEB_GARDEN_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT(SIZE(XLAT),SIZE(ZFIELDIN,2),SIZE(ZFIELDIN,3)))
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),SIZE(ZFIELDIN,2)))
!
DO JVEGTYPE = 1, SIZE(ZFIELDIN,3)
  ZFIELD=ZFIELDIN(:,:,JVEGTYPE)
  IF (SIZE(ZFIELDIN,3)==NVEGTYPE) LINTERP = (XVEGTYPE(:,JVEGTYPE) > 0.)
  CALL HOR_INTERPOL(ILUOUT,ZFIELD,ZFIELDOUT(:,:,JVEGTYPE))
  LINTERP = .TRUE.
END DO
!
DEALLOCATE(ZFIELD)

!-------------------------------------------------------------------------------------
!
!*      6.     Transformation from vegtype grid to averaged grid
!
ALLOCATE(ZW (SIZE(ZFIELDOUT,1),SIZE(ZFIELDOUT,2)))
ALLOCATE(ZSUM (SIZE(ZFIELDOUT,1)))
ZW = 0.
!
DO JLAYER=1,SIZE(ZW,2)
  ZSUM(:) = SUM(XVEGTYPE(:,:),2,ZFIELDOUT(:,JLAYER,:)/=XUNDEF)
  DO JVEGTYPE=1,NVEGTYPE
    WHERE (ZFIELDOUT(:,JLAYER,JVEGTYPE)/=XUNDEF) 
      ZW(:,JLAYER) = ZW(:,JLAYER) + XVEGTYPE(:,JVEGTYPE) * ZFIELDOUT(:,JLAYER,JVEGTYPE) / ZSUM(:)
    END WHERE
  END DO
  DO JI=1,SIZE(ZW,1)
    IF (ALL(ZFIELDOUT(JI,JLAYER,:)==XUNDEF)) ZW(JI,JLAYER) = XUNDEF
  ENDDO
END DO
!
!-------------------------------------------------------------------------------------
!
!*      7.     Return to historical variable
!
!
SELECT CASE (HSURF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WG     ') 
  ALLOCATE(ZF (SIZE(ZFIELDOUT,1),NGROUND_LAYER))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,XDG,ZF)
  !
  !* retrieves soil water content from soil relative humidity
  ALLOCATE(XWG(SIZE(ZFIELDOUT,1),NGROUND_LAYER))
  XWG(:,:) = XWWILT + ZF(:,:) * (XWFC-XWWILT)
  XWG(:,:) = MAX(MIN(XWG(:,:),XWSAT),XWGMIN)
  !
  WHERE(ZF(:,:)==XUNDEF)XWG(:,:)=XUNDEF
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WGI    ')
  ALLOCATE(ZF (SIZE(ZFIELDOUT,1),NGROUND_LAYER))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,XDG,ZF)
  !
  !* retrieves soil ice content from soil relative humidity
  ALLOCATE(XWGI(SIZE(ZFIELDOUT,1),NGROUND_LAYER))
  XWGI(:,:) = ZF(:,:) * XWSAT
  XWGI(:,:) = MAX(MIN(XWGI(:,:),XWSAT),0.)
  !
  WHERE(ZF(:,:)==XUNDEF)XWGI(:,:)=XUNDEF
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TG     ') 
  IWORK=NGROUND_LAYER
  ALLOCATE(XTG(SIZE(ZFIELDOUT,1),IWORK))
  ALLOCATE(ZDG(SIZE(XDG,1),IWORK))
  IF (CISBA=='2-L'.OR.CISBA=='3-L') THEN
    ZDG(:,1) = 0.
    ZDG(:,2) = 0.40   ! deep temperature for force-restore taken at 20cm
    IF(CISBA=='3-L') ZDG(:,3) = 5.60   ! climatological temperature, usually not used
  ELSE
    !* diffusion method, the soil grid is the same as for humidity
    ZDG(:,:) = XDG(:,:)
  END IF
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,ZDG,XTG)
  DEALLOCATE(ZDG)
  !
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WR     ') 
  ALLOCATE(XWR(SIZE(ZFIELDOUT,1)))
  XWR(:) = ZW(:,1)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('LAI    ') 
  !* LAI is updated only if present and pertinent (evolutive LAI) in input file

   WHERE (ZW(:,1)/=XUNDEF) XLAI(:) = ZW(:,1)
  !
END SELECT
!
DEALLOCATE(ZW)
!-------------------------------------------------------------------------------------
!
!*      8.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUT)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JI, JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
!
INTEGER :: ILAYER1, ILAYER2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
IF (SIZE(PT1,2)==3) THEN
!
!* 1. case with only 3 input levels (typically coming from 'UNIF')
!     -----------------------------
!
  IF (CISBA=='2-L' .OR. CISBA=='3-L') THEN
    !* Possible LTEMP_ARP case
    IF(SIZE(PT2,2)>3)THEN
       ILAYER1=3
       ILAYER2=SIZE(PT2,2)
    ELSE
       ILAYER1=SIZE(PT2,2)
       ILAYER2=0
    ENDIF
    !* historical 2L or 3L ISBA version
    PT2(:,1:ILAYER1) = PT1(:,1:ILAYER1) 
    !* Possible LTEMP_ARP case
    IF(ILAYER2>0)THEN
       DO JL=ILAYER1+1,ILAYER2
         PT2(:,JL) = PT2(:,ILAYER1)
       ENDDO
    ENDIF
    IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
    RETURN
!    
  ELSEIF(CISBA=='DIF')THEN
       !surface layer (generally 0.01m imposed)
       PT2(:,1) = PT1(:,1) 
       !deep layers
       DO JL=2,NGROUND_LAYER
          PT2(:,JL) = PT1(:,3)
       END DO
       !if root layers
       DO JI=1,SIZE(PT1,1)
          DO JL=2,NGROUND_LAYER
             IF(XROOTFRAC(JI,JL)<=1.0)THEN 
                PT2(JI,JL) = PT1(JI,2)
                EXIT
             ENDIF
          END DO
       END DO 
       IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
    RETURN
  END IF    
!
END IF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 2. case with fine grid as input (general case)
!     ----------------------------
!
  ZD2(:,:) = 0.
  !
  ZD2(:,1) = PD2(:,1)/2.
  DO JL=2,SIZE(ZD2,2)
    ZD2(:,JL) = (PD2(:,JL-1)+PD2(:,JL)) /2.
  END DO
  !
  DO JL=1,SIZE(PT1,2)
    ZD1(:,JL) = PGRID1(JL)
  END DO
  !
  CALL INTERP_GRID(ZD1,PT1(:,:),ZD2,PT2(:,:))
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_TEB_GARDEN_FIELD
