!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_ISBA_FIELD (DTCO, IG, I, UG, U, USS,GCP, &
                                HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,OKEY)
!     #################################################################################
!
!!****  *PREP_HOR_ISBA_FIELD* - reads, interpolates and prepares an ISBA field
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
!!      M. Lafaysse  07/2012, allow netcdf input files
!!      B. Decharme  07/2012, Bug init uniform snow
!!      M. Lafaysse 11/2012,  snow liquid water content
!!      B. Decharme  03/2014, external init with FA files
!!                            new vertical interpolation
!!      P Samuelsson 10/2014, MEB
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_PREP,     ONLY : XZS_LS, LINTERP, CMASK

USE MODD_PREP_ISBA, ONLY : XGRID_SOIL, NGRID_LEVEL, LSNOW_IDEAL,    &
                           XWSNOW, XRSNOW, XTSNOW, XLWCSNOW, XASNOW,          &
                           XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW


USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODI_READ_PREP_ISBA_CONF
USE MODI_READ_PREP_ISBA_SNOW
USE MODI_PREP_ISBA_ASCLLV
USE MODI_PREP_ISBA_GRIB
USE MODI_PREP_ISBA_UNIF
USE MODI_PREP_ISBA_BUFFER
USE MODI_ABOR1_SFX
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_ISBA_EXTERN
USE MODI_PREP_ISBA_NETCDF
USE MODI_VEGTYPE_TO_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
LOGICAL, OPTIONAL,  INTENT(INOUT):: OKEY
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILETYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILE_SNOW     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD_SNOW     ! name of file 
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, POINTER, DIMENSION(:,:,:)     :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:)       :: ZFIELD, ZPATCH ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTP ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTV !
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZW        ! work array (x, fine   soil grid, npatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZF        ! work array (x, output soil grid, npatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZDG       ! out T grid (x, output soil grid, npatch)
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
LOGICAL                       :: GUNIF_SNOW! flag for prescribed uniform field
INTEGER                       :: JPATCH    ! loop on patches
INTEGER                       :: JVEGTYPE  ! loop on vegtypes
INTEGER                       :: INI, INL, INP, JJ, JL, IP_I, IP_O, JP, JVEG ! Work integer
INTEGER, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,3)) :: IWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_ISBA_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                         HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
 CMASK = 'NATURE'
!
INI=SIZE(IG%XLAT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_VEG ') THEN
  CALL READ_PREP_ISBA_SNOW(HPROGRAM,I%TSNOW%SCHEME,I%TSNOW%NLAYER,YFILE_SNOW,YFILETYPE_SNOW,&
                                YFILEPGD_SNOW,YFILEPGDTYPE_SNOW,GUNIF_SNOW)
  IF(.NOT.GUNIF_SNOW.AND.LEN_TRIM(YFILE_SNOW)==0.AND.LEN_TRIM(YFILETYPE_SNOW)==0)THEN
    IF(LEN_TRIM(YFILE)/=0.AND.LEN_TRIM(YFILETYPE)/=0)THEN
       YFILE_SNOW    =YFILE
       YFILETYPE_SNOW=YFILETYPE
       YFILEPGD_SNOW    =YFILEPGD
       YFILEPGDTYPE_SNOW=YFILEPGDTYPE       
    ELSE
       GUNIF_SNOW=.TRUE.
       IF(ALL(XWSNOW==XUNDEF))XWSNOW=0.0
    ENDIF
  ENDIF
  CALL PREP_HOR_SNOW_FIELDS(DTCO, &
                            IG, U,GCP, &
                            HPROGRAM, HSURF,                     &
                            YFILE_SNOW, YFILETYPE_SNOW,          &
                            YFILEPGD_SNOW, YFILEPGDTYPE_SNOW,    &
                            ILUOUT, GUNIF_SNOW, I%NPATCH, 1,     &
                            INI,I%TSNOW, I%TTIME,                &
                            XWSNOW, XRSNOW, XTSNOW, XLWCSNOW,    &
                            XASNOW, LSNOW_IDEAL, XSG1SNOW,       &
                            XSG2SNOW, XHISTSNOW, XAGESNOW,       &
                            I%XVEGTYPE, I%XVEGTYPE_PATCH, I%XPATCH,    &
                            OKEY                                 )
  DEALLOCATE(XWSNOW)
  DEALLOCATE(XRSNOW)
  DEALLOCATE(XTSNOW)
  DEALLOCATE(XLWCSNOW)
  DEALLOCATE(XSG1SNOW)
  DEALLOCATE(XSG2SNOW)
  DEALLOCATE(XHISTSNOW)
  DEALLOCATE(XAGESNOW)
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
  CALL PREP_ISBA_UNIF(ILUOUT,HSURF,ZFIELDIN)
ELSE IF (YFILETYPE=='ASCLLV') THEN
  CALL PREP_ISBA_ASCLLV(DTCO, UG, U, USS, &
                        HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='GRIB  ') THEN
  CALL PREP_ISBA_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN,OKEY)
ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '.OR.YFILETYPE=='FA    ') THEN
   CALL PREP_ISBA_EXTERN(DTCO, I, U,GCP, &
                         HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN,OKEY)
ELSE IF (YFILETYPE=='BUFFER') THEN
   CALL PREP_ISBA_BUFFER(IG, U, &
                         HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='NETCDF') THEN
   CALL PREP_ISBA_NETCDF(DTCO, U, &
                         HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
ELSE
   CALL ABOR1_SFX('PREP_HOR_ISBA_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Horizontal interpolation
!
INL = SIZE(ZFIELDIN,2)
INP = SIZE(ZFIELDIN,3)
!
ALLOCATE(ZFIELDOUTP(INI,INL,INP))
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),INL))
!
ALLOCATE(ZPATCH(INI,INP))
ZPATCH(:,:) = 0.
!
IF (INP==NVEGTYPE) THEN
  ZPATCH(:,:) = I%XVEGTYPE(:,:)
ELSEIF (INP==I%NPATCH) THEN
  ZPATCH(:,:) = I%XPATCH(:,:)
ELSEIF (INP<I%NPATCH) THEN
  DO JP = 1,I%NPATCH
    DO JVEG = 1,NVEGTYPE
      IP_I = VEGTYPE_TO_PATCH(JVEG,INP)
      IP_O = VEGTYPE_TO_PATCH(JVEG,I%NPATCH)
      !
      ! pour chaque patch d'entrée à interpoler, le masque
      ! est la somme des patchs de sortie (plus détaillés) présent sur 
      ! chaque point
      IF (IP_O==JP) THEN
        ZPATCH(:,IP_I) = ZPATCH(:,IP_I) + I%XPATCH(:,IP_O)
        EXIT
      ENDIF
    ENDDO
  ENDDO
ELSEIF (INP>I%NPATCH) THEN
  DO JP = 1,INP
    DO JVEG = 1,NVEGTYPE
      IP_I = VEGTYPE_TO_PATCH(JVEG,INP)
      IP_O = VEGTYPE_TO_PATCH(JVEG,I%NPATCH)
      !
      ! pour chaque patch d'entrée à interpoler, le masque
      ! est le patch de sortie (moins détaillé) présent 
      ! sur ce point
      IF (IP_I==JP) THEN
        ZPATCH(:,IP_I) = I%XPATCH(:,IP_O)
        EXIT
      ENDIF
    ENDDO
  ENDDO
ENDIF
!
DO JPATCH = 1, INP

  ZFIELD=ZFIELDIN(:,:,JPATCH)
  LINTERP(:) = (ZPATCH(:,JPATCH) > 0.)
  CALL HOR_INTERPOL(DTCO, U,GCP, &
                    ILUOUT,ZFIELD,ZFIELDOUTP(:,:,JPATCH))
  LINTERP = .TRUE.

END DO
!
DEALLOCATE(ZFIELD)
!
ALLOCATE(ZFIELDOUTV(INI,INL,NVEGTYPE))
!
 CALL PUT_ON_ALL_VEGTYPES(INI,INL,INP,NVEGTYPE,ZFIELDOUTP,ZFIELDOUTV)
!
DEALLOCATE(ZFIELDOUTP)
!
!-------------------------------------------------------------------------------------
!
!*      6.     Transformation from vegtype grid to patch grid
!
ALLOCATE(ZW (INI,SIZE(ZFIELDOUTV,2),I%NPATCH))
!
ZW = 0.
 CALL VEGTYPE_GRID_TO_PATCH_GRID(I%NPATCH,I%XVEGTYPE_PATCH,I%XPATCH,ZFIELDOUTV,ZW)
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
 CASE('ZS     ') 
  ALLOCATE(XZS_LS(INI))
  XZS_LS(:) = ZFIELDOUTV(:,1,1)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WG     ') 
  ALLOCATE(ZF (INI,I%NGROUND_LAYER,I%NPATCH))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,I%XDG,ZF)
  !
  !* retrieves soil water content from soil relative humidity
  ALLOCATE(I%XWG(INI,I%NGROUND_LAYER,I%NPATCH))
  I%XWG(:,:,:)=XUNDEF
  IF(I%CISBA=='DIF')THEN
     IWORK(:,:)=I%NWG_LAYER(:,:)
  ELSE
     IWORK(:,:)=SIZE(I%XWG,2)
  ENDIF
  DO JPATCH=1,I%NPATCH
    DO JJ=1,INI
       IF(IWORK(JJ,JPATCH)==NUNDEF)CYCLE
       INL=IWORK(JJ,JPATCH)
       DO JL=1,INL
          I%XWG(JJ,JL,JPATCH) = I%XWWILT(JJ,JL) + ZF(JJ,JL,JPATCH) * (I%XWFC(JJ,JL)-I%XWWILT(JJ,JL))
          I%XWG(JJ,JL,JPATCH) = MAX(MIN(I%XWG(JJ,JL,JPATCH),I%XWSAT(JJ,JL)),XWGMIN)
       ENDDO
    ENDDO
  ENDDO
  !
  WHERE(ZF(:,:,:)==XUNDEF)I%XWG(:,:,:)=XUNDEF
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WGI    ')
  ALLOCATE(ZF (INI,I%NGROUND_LAYER,I%NPATCH))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,I%XDG,ZF)
  !
  !* retrieves soil ice content from soil relative humidity
  ALLOCATE(I%XWGI(INI,I%NGROUND_LAYER,I%NPATCH))
  I%XWGI(:,:,:)=0.0
  IF(I%CISBA=='DIF')THEN
     IWORK(:,:)=I%NWG_LAYER(:,:)
  ELSE
     IWORK(:,:)=2
  ENDIF  
  DO JPATCH=1,I%NPATCH
    DO JJ=1,INI
       IF(IWORK(JJ,JPATCH)==NUNDEF)CYCLE
       INL=IWORK(JJ,JPATCH)
       DO JL=1,INL
          I%XWGI(JJ,JL,JPATCH) = ZF(JJ,JL,JPATCH) * I%XWSAT(JJ,JL)
          I%XWGI(JJ,JL,JPATCH) = MAX(MIN(I%XWGI(JJ,JL,JPATCH),I%XWSAT(JJ,JL)),0.)
       ENDDO
    ENDDO
  END DO
  !
  WHERE(ZF  (:,:,:)==XUNDEF )I%XWGI(:,:,:)=XUNDEF
  WHERE(I%XWGI(:,:,:)<=1.0E-10)I%XWGI(:,:,:)=0.0
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TG     ') 
  IF(I%LTEMP_ARP)THEN
    INL=I%NTEMPLAYER_ARP
  ELSE
    INL=I%NGROUND_LAYER
  ENDIF
  ALLOCATE(I%XTG(INI,INL,I%NPATCH))
  ALLOCATE(ZDG(SIZE(I%XDG,1),INL,SIZE(I%XDG,3)))
  IF (I%CISBA=='2-L'.OR.I%CISBA=='3-L') THEN
     DO JPATCH=1,I%NPATCH
        ZDG(:,1,JPATCH) = 0.01
        ZDG(:,2,JPATCH) = 0.40                    ! deep temperature for force-restore taken at 20cm
        IF(I%CISBA=='3-L') ZDG(:,3,JPATCH) = 5.00   ! climatological temperature, usually not used
     ENDDO         
     IF(I%LTEMP_ARP)THEN
       DO JPATCH=1,I%NPATCH
          ZDG(:,3,JPATCH) = 1.0
          DO JL=4,INL
             ZDG(:,JL,JPATCH) = ZDG(:,JL-1,JPATCH)+1.0
          ENDDO
       ENDDO
     ENDIF
  ELSE
    !* diffusion method, the soil grid is the same as for humidity
    ZDG(:,:,:) = I%XDG(:,:,:)
  END IF
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,ZDG,I%XTG)
  DEALLOCATE(ZDG)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WR     ') 
  ALLOCATE(I%XWR(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XWR(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CASE('WRL    ') 
  ALLOCATE(I%XWRL(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XWRL(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CASE('WRLI   ') 
  ALLOCATE(I%XWRLI(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XWRLI(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WRVN   ') 
  ALLOCATE(I%XWRVN(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XWRVN(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TV     ') 
  ALLOCATE(I%XTV(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XTV(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TL     ') 
  ALLOCATE(I%XTL(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XTL(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TC     ') 
  ALLOCATE(I%XTC(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XTC(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('QC     ') 
  ALLOCATE(I%XQC(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XQC(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('LAI    ') 
  !* LAI is updated only if present and pertinent (evolutive LAI) in input file
  IF (ANY(ZW(:,:,:)/=XUNDEF)) THEN
    DO JPATCH=1,I%NPATCH
      I%XLAI(:,JPATCH) = ZW(:,1,JPATCH)
    END DO
  END IF
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('ICE_STO') 
  ALLOCATE(I%XICE_STO(INI,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    I%XICE_STO(:,JPATCH) = ZW(:,1,JPATCH)
  END DO
  !
END SELECT
!
DEALLOCATE(ZW)
!-------------------------------------------------------------------------------------
!
!*      8.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUTV)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',1,ZHOOK_HANDLE)
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
USE MODI_INTERP_GRID_NAT
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),     INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JI,JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
!
INTEGER :: ILAYER1, ILAYER2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IF (SIZE(PT1,2)==3) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 1. case with only 3 input levels (typically coming from 'UNIF')
!     -----------------------------
!
  IF (I%CISBA=='2-L' .OR. I%CISBA=='3-L') THEN
    !* Possible LTEMP_ARP case
    IF(SIZE(PT2,2)>3)THEN
      ILAYER1=3
      ILAYER2=SIZE(PT2,2)
    ELSE
      ILAYER1=SIZE(PT2,2)
      ILAYER2=0
    ENDIF
    !* historical 2L or 3L ISBA version
    DO JPATCH=1,I%NPATCH
      PT2(:,1:ILAYER1,JPATCH) = PT1(:,1:ILAYER1,JPATCH) 
      !* Possible LTEMP_ARP case
      IF(ILAYER2>0)THEN
        DO JL=ILAYER1+1,ILAYER2
          PT2(:,JL,JPATCH) = PT2(:,ILAYER1,JPATCH)
        ENDDO
      ENDIF
    END DO
  ELSEIF(I%CISBA=='DIF')THEN
    DO JPATCH=1,I%NPATCH
       !surface layer (generally 0.01m imposed)
       PT2(:,1,JPATCH) = PT1(:,1,JPATCH) 
       !second layer
       PT2(:,2,JPATCH) = 0.25*PT1(:,1,JPATCH)+0.75*PT1(:,2,JPATCH)
       !others layers
       DO JI=1,SIZE(PT1,1)
         DO JL=3,I%NGROUND_LAYER
           IF(PD2(JI,JL,JPATCH)<=I%XDG2(JI,JPATCH))THEN 
            !root layers
             PT2(JI,JL,JPATCH) = PT1(JI,2,JPATCH)
           ELSE
            !deep layers
             PT2(JI,JL,JPATCH) = PT1(JI,3,JPATCH)
           ENDIF
         END DO
       END DO 
    END DO 
  END IF    
!  
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 2. case with fine grid as input (general case)
!     ----------------------------
!
  DO JL=1,SIZE(PT1,2)
     ZD1(:,JL) = PGRID1(JL)
  ENDDO
!
  DO JPATCH=1,I%NPATCH
     ZD2(:,:) = PD2(:,:,JPATCH)
     CALL INTERP_GRID_NAT(ZD1,PT1(:,:,JPATCH),ZD2,PT2(:,:,JPATCH))
  ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ENDIF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_ISBA_FIELD
