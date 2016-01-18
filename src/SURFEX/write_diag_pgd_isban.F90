!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_PGD_ISBA_n(HPROGRAM)
!     #########################################
!
!!****  *WRITE_DIAG_PGD_ISBA_n* - writes the ISBA physiographic diagnostic fields
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      Modified    10/2004 by P. Le Moigne: add XZ0REL, XVEGTYPE_PATCH
!!      Modified    11/2005 by P. Le Moigne: limit length of VEGTYPE_PATCH field names
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_n,     ONLY : NPATCH, CPHOTO, CHORT, CISBA,                           &
                              XLAI, XVEG, XZ0,XALBNIR_SOIL,XALBVIS_SOIL,XALBUV_SOIL,&
                              XRSMIN, XGAMMA, XRGL, XCV, XEMIS, XDG, XWRMAX_CF,     &
                              XZ0REL, XVEGTYPE_PATCH, XALBNIR, XALBVIS, XALBUV,     &
                              XPATCH, XWATSUP, TSEED, TREAP, XIRRIG, XD_ICE,        &
                              XROOTFRAC, NWG_LAYER, XDROOT, XDG2,                   &
                              XWSAT, XWFC, XWWILT, XRUNOFFD, CSOC, XFRACSOC   
USE MODD_AGRI,       ONLY : LAGRIP
!
USE MODD_DIAG_MISC_ISBA_n,ONLY : LSURF_DIAG_ALBEDO
!
USE MODD_IO_SURF_FA, ONLY : LFANOCOMPACT, LPREP
!
USE MODD_CH_ISBA_n,  ONLY : XSOILRC_SO2, XSOILRC_O3, CCH_DRY_DEP, NBEQ
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,3)) :: ZWORK ! Work array
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=2)  :: YLVLV, YPAS
!
INTEGER           :: JJ, JL, JP, ILAYER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
!* Leaf Area Index
!
IF (CPHOTO=='NON' .OR. CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
  !
  YRECFM='LAI'
  YCOMMENT='leaf area index'
  YCOMMENTUNIT='-'
  !
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XLAI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Vegetation fraction
!
YRECFM='VEG'
YCOMMENT='vegetation fraction'
YCOMMENTUNIT='-'
!
CALL WRITE_SURF_FIELD2D(HPROGRAM,XVEG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Surface roughness length (without snow)
!
YRECFM='Z0VEG'
YCOMMENT='surface roughness length (without snow)'
YCOMMENTUNIT='M'
!
CALL WRITE_SURF_FIELD2D(HPROGRAM,XZ0(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
!* Fraction for each patch
!
IF(.NOT.LFANOCOMPACT.OR.LPREP)THEN
  YRECFM='PATCH'
  YCOMMENT='fraction for each patch'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XPATCH(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!-------------------------------------------------------------------------------
!
!* Soil depth for each patch
!
DO JL=1,SIZE(XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A2,I1)') 'DG',JL
  ELSE
    WRITE(YRECFM,FMT='(A2,I2)') 'DG',JL          
  ENDIF
  YCOMMENT='soil depth'
  YCOMMENTUNIT='M'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XDG(:,JL,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
END DO
!-------------------------------------------------------------------------------
!
IF(CISBA=='DIF')THEN
!
!* Root depth
!
  YRECFM='DROOT_DIF'
  YCOMMENT='Root depth in ISBA-DIF'
!
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XDROOT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='DG2_DIF'
  YCOMMENT='DG2 depth in ISBA-DIF'
!
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XDG2(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Runoff depth
!
  YRECFM='RUNOFFD'
  YCOMMENT='Runoff deph in ISBA-DIF'
!
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XRUNOFFD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Total soil depth for mositure
!
  ZWORK(:,:)=XUNDEF
  DO JP=1,SIZE(XDG,3)
     DO JJ=1,SIZE(XDG,1)
        JL=NWG_LAYER(JJ,JP)
        IF(JL/=NUNDEF)THEN
          ZWORK(JJ,JP)=XDG(JJ,JL,JP)
        ENDIF
     ENDDO
  ENDDO
  YRECFM='DTOT_DIF'
  YCOMMENT='Total soil depth for moisture in ISBA-DIF'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Root fraction for each patch
!
  DO JL=1,SIZE(XROOTFRAC,2)
     IF (JL<10) THEN
       WRITE(YRECFM,FMT='(A8,I1)') 'ROOTFRAC',JL
     ELSE
       WRITE(YRECFM,FMT='(A8,I2)') 'ROOTFRAC',JL          
     ENDIF  
     YCOMMENT='root fraction by layer'
     YCOMMENTUNIT='-'
     ZWORK(:,:)=XUNDEF
     DO JJ=1,SIZE(XDG,1)
        WHERE(JL<=NWG_LAYER(JJ,:).AND.NWG_LAYER(JJ,:)/=NUNDEF)
              ZWORK(JJ,:)=XROOTFRAC(JJ,JL,:)
        ENDWHERE
     ENDDO
     CALL WRITE_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
  END DO
!
!* SOC fraction for each layer
!
  IF(CSOC=='SGH')THEN
    DO JL=1,SIZE(XDG,2)
     IF (JL<10) THEN
       WRITE(YRECFM,FMT='(A7,I1)') 'FRACSOC',JL
     ELSE
       WRITE(YRECFM,FMT='(A7,I2)') 'FRACSOC',JL          
     ENDIF  
     YCOMMENT='SOC fraction by layer (-)'
     CALL WRITE_SURF(HPROGRAM,YRECFM,XFRACSOC(:,JL),IRESP,HCOMMENT=YCOMMENT)
    END DO
  ENDIF
!
ENDIF        
!
!-------------------------------------------------------------------------------
!
DO JL=1,SIZE(XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A4,I1)') 'WSAT',JL
   ELSE
     WRITE(YRECFM,FMT='(A4,I2)') 'WSAT',JL          
   ENDIF  
  YCOMMENT='soil porosity by layer (m3/m3)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XWSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A3,I1)') 'WFC',JL
   ELSE
     WRITE(YRECFM,FMT='(A3,I2)') 'WFC',JL          
   ENDIF  
  YCOMMENT='field capacity by layer (m3/m3)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XWFC(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A5,I1)') 'WWILT',JL
   ELSE
     WRITE(YRECFM,FMT='(A5,I2)') 'WWILT',JL          
   ENDIF  
  YCOMMENT='wilting point by layer (m3/m3)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XWWILT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO     
!
!-------------------------------------------------------------------------------
! For Earth System Model
IF(LFANOCOMPACT.AND..NOT.LPREP)THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
YRECFM='Z0REL'
YCOMMENT='orography roughness length (M)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0REL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* Runoff soil ice depth for each patch
!
IF(CHORT=='SGH'.AND.CISBA/='DIF')THEN
  YRECFM='DICE'
  YCOMMENT='soil ice depth for runoff'
  YCOMMENTUNIT='m'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XD_ICE(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type for each patch
!
DO JL=1,SIZE(XVEGTYPE_PATCH,2)
  WRITE(YPAS,'(I2)') JL 
  YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  WRITE(YRECFM,FMT='(A9)') 'VEGTY_P'//YLVLV
  YCOMMENT='fraction of each vegetation type for each patch'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XVEGTYPE_PATCH(:,JL,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
END DO
!-------------------------------------------------------------------------------
!
!* other surface parameters
!
YRECFM='RSMIN'
YCOMMENT='minimum stomatal resistance'
YCOMMENTUNIT='SM-1'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XRSMIN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
YRECFM='GAMMA'
YCOMMENT='coefficient for RSMIN calculation'
YCOMMENTUNIT='-'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XGAMMA(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
YRECFM='CV'
YCOMMENT='vegetation thermal inertia coefficient'
YCOMMENTUNIT='-'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
YRECFM='RGL'
YCOMMENT='maximum solar radiation usable in photosynthesis'
YCOMMENTUNIT='-'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XRGL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
YRECFM='EMIS_ISBA'
YCOMMENT='surface emissivity'
YCOMMENTUNIT='-'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XEMIS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
YRECFM='WRMAX_CF'
YCOMMENT='coefficient for maximum water interception'
YCOMMENTUNIT='-'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XWRMAX_CF(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
IF (LSURF_DIAG_ALBEDO) THEN
!
!* Soil albedos
!
!
   YRECFM='ALBNIR_S'
   YCOMMENT='soil near-infra-red albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBNIR_SOIL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBVIS_S'
   YCOMMENT='soil visible albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBVIS_SOIL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBUV_S'
   YCOMMENT='soil UV albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBUV_SOIL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
!* albedos
!
   YRECFM='ALBNIR_ISBA'
   YCOMMENT='total near-infra-red albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBNIR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBVIS_ISBA'
   YCOMMENT='total visible albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBVIS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBUV_ISBA'
   YCOMMENT='total UV albedo'
   YCOMMENTUNIT='-'
   CALL WRITE_SURF_FIELD2D(HPROGRAM,XALBUV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
END IF
!
!-------------------------------------------------------------------------------
!
!* chemical soil resistances
!
IF (CCH_DRY_DEP=='WES89' .AND. NBEQ>0) THEN
  YRECFM='SOILRC_SO2'
  YCOMMENT='bare soil resistance for SO2'
  YCOMMENTUNIT='?'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XSOILRC_SO2(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  YRECFM='SOILRC_O3'
  YCOMMENT='bare soil resistance for O3'
  YCOMMENTUNIT='?'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XSOILRC_O3(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
END IF
!
!-------------------------------------------------------------------------------
!
IF (LAGRIP .AND. (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') ) THEN
!
!* seeding and reaping
!
!
  YRECFM='TSEED'
  YCOMMENT='date of seeding (-)'
!
  CALL WRITE_SURF(HPROGRAM,YRECFM,TSEED(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='TREAP'
  YCOMMENT='date of reaping (-)'
!
  CALL WRITE_SURF(HPROGRAM,YRECFM,TREAP(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* irrigated fraction
!
  YRECFM='IRRIG'
  YCOMMENT='flag for irrigation (irrigation if >0.)'
  YCOMMENTUNIT='-'
!
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XIRRIG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!-------------------------------------------------------------------------------
!
!* water supply for irrigation
!
  YRECFM='WATSUP'
  YCOMMENT='water supply during irrigation process'
  YCOMMENTUNIT='mm'
!
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XWATSUP(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!-------------------------------------------------------------------------------
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_PGD_ISBA_n
