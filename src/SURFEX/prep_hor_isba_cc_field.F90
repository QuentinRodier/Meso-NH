!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_ISBA_CC_FIELD (DTCO, U, &
                                    IG, I,GCP, &
                                   HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_HOR_ISBA_CC_FIELD* - reads, interpolates and prepares an ISBA-CC field
!                                   only external case implemeted
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
!!     B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2014
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_CO2V_PAR,  ONLY : XCA_NIT, XCC_NIT
!
USE MODD_PREP,      ONLY : LINTERP, CMASK
!

USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODI_READ_PREP_ISBA_CONF
USE MODI_ABOR1_SFX
USE MODI_HOR_INTERPOL
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_ISBA_CC_EXTERN
USE MODI_PUT_ON_ALL_VEGTYPES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=8),   INTENT(IN)  :: HSURF     ! type of field
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
REAL, POINTER, DIMENSION(:,:,:)     :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:)       :: ZFIELD    ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTP ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTV ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZW        ! work array (x, fine   soil grid, npatch)
!
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
LOGICAL                       :: GPREP_AGS ! flag to prepare ags field (only external case implemeted)
!
INTEGER                       :: JPATCH    ! loop on patches
INTEGER                       :: JVEGTYPE  ! loop on vegtypes
INTEGER                       :: INI, INL, INP, JJ, JL ! Work integer
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_CC_FIELD',0,ZHOOK_HANDLE)
!
!*      1.     Reading of input file name and type
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_ISBA_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,   &
                         HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
 CMASK = 'NATURE'
!
INI=SIZE(IG%XLAT)
!
GPREP_AGS = .TRUE.
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
   GPREP_AGS = .FALSE.
ELSE IF (YFILETYPE=='ASCLLV') THEN
   GPREP_AGS = .FALSE.
ELSE IF (YFILETYPE=='GRIB  ') THEN
   GPREP_AGS = .FALSE.
ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '.OR.YFILETYPE=='FA    ') THEN
   CALL PREP_ISBA_CC_EXTERN(GCP,&
                            HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN,GPREP_AGS)
ELSE IF (YFILETYPE=='BUFFER') THEN
   GPREP_AGS = .FALSE.
ELSE IF (YFILETYPE=='NETCDF') THEN
   GPREP_AGS = .FALSE.
ELSE
   CALL ABOR1_SFX('PREP_HOR_ISBA_CC_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Horizontal interpolation
!
IF(GPREP_AGS)THEN
!
  INL = SIZE(ZFIELDIN,2)
  INP = SIZE(ZFIELDIN,3)
!
  ALLOCATE(ZFIELDOUTP(INI,INL,INP))
  ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),INL))
!
  DO JPATCH = 1, INP
    ZFIELD(:,:)=ZFIELDIN(:,:,JPATCH)
    IF (INP==NVEGTYPE) THEN
       LINTERP = (I%XVEGTYPE(:,JPATCH) > 0.)
    ELSEIF(INP==I%NPATCH)THEN
       LINTERP = (I%XPATCH(:,JPATCH) > 0.)
    ENDIF
    CALL HOR_INTERPOL(DTCO, U,GCP, &
                      ILUOUT,ZFIELD,ZFIELDOUTP(:,:,JPATCH))
    LINTERP = .TRUE.
  END DO
!
  DEALLOCATE(ZFIELD)
  DEALLOCATE(ZFIELDIN)
!
  ALLOCATE(ZFIELDOUTV(INI,INL,NVEGTYPE))
!
  CALL PUT_ON_ALL_VEGTYPES(INI,INL,INP,NVEGTYPE,ZFIELDOUTP,ZFIELDOUTV)
!
  DEALLOCATE(ZFIELDOUTP)
!
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Transformation from vegtype grid to patch grid
!
IF(GPREP_AGS)THEN
!
  ALLOCATE(ZW (INI,SIZE(ZFIELDOUTV,2),I%NPATCH))
!
  ZW(:,:,:) = 0.
  CALL VEGTYPE_GRID_TO_PATCH_GRID(I%NPATCH,I%XVEGTYPE_PATCH,I%XPATCH,ZFIELDOUTV,ZW)
!
ELSE
!
  SELECT CASE (HSURF)
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !  
    CASE('BIOMASS') 
     ALLOCATE(ZW(INI,I%NNBIOMASS,I%NPATCH))
     ZW(:,:,:) = 0.
     WHERE(I%XLAI(:,:)/=XUNDEF)
       ZW(:,1,:) = I%XLAI(:,:) * I%XBSLAI_NITRO(:,:)
     ENDWHERE
     ZW(:,2,:) = MAX( 0., (ZW(:,1,:)/ (XCC_NIT/10.**XCA_NIT))  &
                          **(1.0/(1.0-XCA_NIT)) - ZW(:,1,:) )
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('LITTER') 
     ALLOCATE(ZW(INI,I%NNLITTER*I%NNLITTLEVS,I%NPATCH))
     ZW(:,:,:) = 0.0
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('SOILCARB') 
     ALLOCATE(ZW(INI,I%NNSOILCARB,I%NPATCH))
     ZW(:,:,:) = 0.0
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('LIGNIN') 
     ALLOCATE(ZW(INI,I%NNLITTLEVS,I%NPATCH))
     ZW(:,:,:) = 0.0
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
  END SELECT
!
ENDIF
!-------------------------------------------------------------------------------------
!
!*      7.     Return to historical variable
!
!
SELECT CASE (HSURF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('BIOMASS') 
  ALLOCATE(I%XBIOMASS(INI,I%NNBIOMASS,I%NPATCH))
  INL=MIN(I%NNBIOMASS,SIZE(ZW,2))
  DO JL=1,INL
     WHERE(ZW(:,JL,:)/=XUNDEF)
       I%XBIOMASS(:,JL,:) = ZW(:,JL,:)
     ELSEWHERE
       I%XBIOMASS(:,JL,:) = 0.0
     ENDWHERE
  ENDDO
  IF(I%NNBIOMASS>INL)THEN
    DO JL=INL+1,I%NNBIOMASS
       WHERE(ZW(:,JL,:)/=XUNDEF)
         I%XBIOMASS(:,JL,:) = ZW(:,INL,:)
       ELSEWHERE
         I%XBIOMASS(:,JL,:) = 0.0
       ENDWHERE
    ENDDO          
  ENDIF
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('LITTER') 
  ALLOCATE(I%XLITTER(INI,I%NNLITTER,I%NNLITTLEVS,I%NPATCH))
  DO JPATCH=1,I%NPATCH
    INL=0
    DO JJ=1,I%NNLITTER
       DO JL=1,I%NNLITTLEVS
          INL=INL+1
          WHERE(ZW(:,INL,JPATCH)/=XUNDEF)
             I%XLITTER(:,JJ,JL,JPATCH) = ZW(:,INL,JPATCH)
          ELSEWHERE
             I%XLITTER(:,JJ,JL,JPATCH) = 0.0
          ENDWHERE
       ENDDO
    ENDDO
  END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('SOILCARB') 
  ALLOCATE(I%XSOILCARB(INI,I%NNSOILCARB,I%NPATCH))
  WHERE(ZW(:,:,:)/=XUNDEF)
    I%XSOILCARB(:,:,:) = ZW(:,:,:)
  ELSEWHERE
    I%XSOILCARB(:,:,:) = 0.0
  ENDWHERE
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('LIGNIN') 
  ALLOCATE(I%XLIGNIN_STRUC(INI,I%NNLITTLEVS,I%NPATCH))
  WHERE(ZW(:,:,:)/=XUNDEF)
    I%XLIGNIN_STRUC(:,:,:) = ZW(:,:,:)
  ELSEWHERE
    I%XLIGNIN_STRUC(:,:,:) = 0.0
  ENDWHERE
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
END SELECT
!
DEALLOCATE(ZW)
!-------------------------------------------------------------------------------------
!
!*      8.     Deallocations
!
IF (ALLOCATED(ZFIELDOUTV)) DEALLOCATE(ZFIELDOUTV)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_CC_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
!
END SUBROUTINE PREP_HOR_ISBA_CC_FIELD
