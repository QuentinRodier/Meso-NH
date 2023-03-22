!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_FX_ISBA_n (DTCO, DUO, U, ID, IO, S, K, NP, NPE, NK, ISS, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_FX_ISBA*
!!
!!    PURPOSE
!!    -------
!!
!!    Writes the ISBA miscellaneous diagnostic fields as specified by cmip
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2016
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_DIAG_n,           ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_SURFEX_n,         ONLY : ISBA_DIAG_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_NP_t, ISBA_P_t, ISBA_NPE_t, ISBA_PE_t, ISBA_S_t,  ISBA_NK_t, ISBA_K_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_SSO_n,            ONLY : SSO_t
!
USE MODD_XIOS, ONLY : NLUT,LALLOW_ADD_DIM,     &
                      YVEGTYPE_DIM_NAME,       &
                      YGROUND_LAYER_DIM_NAME,  &
                      YLANDUSE_TILES_DIM_NAME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,           ONLY : XRHOLW
USE MODD_DATA_COVER_PAR
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_PATCH_TO_TILE_FUNC
USE MODI_PATCH_TO_LUT_FUNC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_DIAG_t),    INTENT(INOUT) :: ID
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_K_t),       INTENT(INOUT) :: K
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NK_t),      INTENT(INOUT) :: NK
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
TYPE(SSO_t),          INTENT(INOUT) :: ISS
!
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t),         POINTER :: PK
TYPE(ISBA_K_t),         POINTER :: KK
TYPE(ISBA_PE_t),        POINTER :: PEK
TYPE(DIAG_t),           POINTER :: DK
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
TYPE(DIAG_MISC_ISBA_t), POINTER :: DMK
!
!
INTEGER, PARAMETER :: ITILES = 7
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
CHARACTER(LEN=4 ) :: YLVL
!
REAL, DIMENSION(U%NSIZE_NATURE)                 :: ZDGT
REAL, DIMENSION(U%NSIZE_NATURE)                 :: ZDG2
REAL, DIMENSION(U%NSIZE_NATURE)                 :: ZSUMP
REAL, DIMENSION(U%NSIZE_NATURE)                 :: ZWORK
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)        :: ZP_WORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZL_WORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZL_SUMP
!
REAL, DIMENSION(U%NSIZE_NATURE,NVEGTYPE)        :: ZVEGTYPE
REAL, DIMENSION(U%NSIZE_NATURE,ITILES)          :: ZWORKTILES
REAL, DIMENSION(U%NSIZE_NATURE,NLUT)            :: ZWORK_LUT
!
INTEGER, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)    :: IWORK
!
INTEGER           :: JI, JL, JP, INI, INL, INP, IMASK
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_ISBA_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
INI  = U%NSIZE_NATURE
INL  = IO%NGROUND_LAYER
INP  = IO%NPATCH
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE', 'ISBA_VEG_EVOLUTION.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Orography roughness length
!-------------------------------------------------------------------------------
!
YRECFM='z0orog'
YCOMMENT='orography roughness length (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ISS%XZ0REL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Soil texture
!-------------------------------------------------------------------------------
!
YRECFM='clayFrac'
YCOMMENT='Clay Fraction over land (-)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,K%XCLAY(:,:))
!
YRECFM='sandFrac'
YCOMMENT='Sand Fraction over land (-)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,K%XSAND(:,:))
!
YRECFM='siltFrac'
YCOMMENT='Silt Fraction over land (-)'
ZL_WORK(:,:)=MAX(0.0,1.0-(K%XSAND(:,:)+K%XCLAY(:,:)))
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
!-------------------------------------------------------------------------------
! * Soil imposed soc
!-------------------------------------------------------------------------------
!
YRECFM='socFrac'
YCOMMENT='SOC Fraction over land (-)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,S%XFRACSOC(:,:))
!
!-------------------------------------------------------------------------------
! * Thickness of Soil Layers
!-------------------------------------------------------------------------------
!
ZL_WORK(:,:) = 0.0
!
IF(IO%CISBA=='DIF')THEN
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JL=1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZL_WORK(IMASK,JL)=ZL_WORK(IMASK,JL)+PK%XPATCH(JI)*PK%XDZG(JI,JL)
        ENDDO
     ENDDO
  ENDDO
!
ELSE       
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZL_WORK(IMASK,1)=ZL_WORK(IMASK,1)+PK%XPATCH(JI)* PK%XDG(JI,1)
        ZL_WORK(IMASK,2)=ZL_WORK(IMASK,2)+PK%XPATCH(JI)*(PK%XDG(JI,2)-PK%XDG(JI,1))
        IF(IO%CISBA=='3-L')THEN
          ZL_WORK(IMASK,3)=ZL_WORK(IMASK,3)+PK%XPATCH(JI)*MAX(0.0,PK%XDG(JI,3)-PK%XDG(JI,2))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
YRECFM='slthick' 
YCOMMENT='Thickness of Soil Layers (m)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
!-------------------------------------------------------------------------------
! * Soil depth and Maximum Root Depth
!-------------------------------------------------------------------------------
!
ZDGT (:) = 0.0
ZDG2 (:) = 0.0
!
IF(IO%CISBA=='DIF')THEN
!
  ZWORK(:) = 0.0
  ZSUMP(:) = 0.0
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
!
        IMASK = PK%NR_P(JI)
!
        ZDG2(IMASK)=ZDG2(IMASK)+PK%XPATCH(JI)*PK%XDG2(JI)
!
        JL=PK%NWG_LAYER(JI)
        IF(JL/=NUNDEF)THEN
          ZDGT(IMASK)=ZDGT(IMASK)+PK%XPATCH(JI)*PK%XDG(JI,JL)
        ENDIF
!
        IF(PK%XDROOT(JI)/=XUNDEF)THEN
          ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PK%XDROOT(JI)
          ZSUMP(IMASK)=ZSUMP(IMASK)+PK%XPATCH(JI)
        ENDIF
!
     ENDDO
  ENDDO
!
  WHERE(ZSUMP(:)>0.0)
        ZWORK(:)=ZWORK(:)/ZSUMP(:)
  ELSEWHERE
        ZWORK(:)=0.0
  ENDWHERE
  ZWORK(:)=MIN(ZDGT(:),ZWORK(:))
!
  YRECFM='rootd' 
  YCOMMENT='Maximum Root Depth excluding non-vegetated area (m)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
ELSE       
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZDGT(IMASK)=ZDGT(IMASK)+PK%XPATCH(JI)*PK%XDG(JI,INL)
        ZDG2(IMASK)=ZDG2(IMASK)+PK%XPATCH(JI)*PK%XDG(JI,  2)
     ENDDO
  ENDDO
!
ENDIF
!
YRECFM='soild' 
YCOMMENT='Soil Hydrological Depth (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZDGT(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rzd2' 
YCOMMENT='Root Zone Depth equivalent to D2 in force-restore scheme (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZDG2(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Root profile 
!-------------------------------------------------------------------------------
!
ZL_WORK(:,:) = 0.0
ZL_SUMP(:,:) = 0.0
!
IF(IO%CISBA=='DIF')THEN
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF(PK%NWG_LAYER(JI)/=NUNDEF.AND.PK%XDROOT(JI)/=XUNDEF)THEN
          ZL_WORK(IMASK,1)=ZL_WORK(IMASK,1)+PK%XPATCH(JI)*PK%XROOTFRAC(JI,1)
          ZL_SUMP(IMASK,1)=ZL_SUMP(IMASK,1)+PK%XPATCH(JI)                   
        ENDIF
     ENDDO
  ENDDO
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JL=2,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF.AND.PK%XDROOT(JI)/=XUNDEF)THEN
             ZL_WORK(IMASK,JL)=ZL_WORK(IMASK,JL)+PK%XPATCH(JI)*(PK%XROOTFRAC(JI,JL)-PK%XROOTFRAC(JI,JL-1))
             ZL_SUMP(IMASK,JL)=ZL_SUMP(IMASK,JL)+PK%XPATCH(JI)                   
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!
ELSE
!
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF (PK%XDG(JI,1)/=XUNDEF) THEN
          ZL_WORK(IMASK,1) = ZL_WORK(IMASK,1) + PK%XPATCH(JI)*     PK%XDG(JI,1)/(PK%XDG(JI,2)-PK%XDG(JI,1))
          ZL_WORK(IMASK,2) = ZL_WORK(IMASK,2) + PK%XPATCH(JI)*(1.0-PK%XDG(JI,1)/(PK%XDG(JI,2)-PK%XDG(JI,1)))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
YRECFM='rzd2sl' 
YCOMMENT='Root Distribution over the grid-cell or rzd2 (-)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
IF(IO%CISBA=='DIF')THEN
  WHERE(ZL_SUMP(:,:)>0.0)
        ZL_WORK(:,:)=ZL_WORK(:,:)/ZL_SUMP(:,:)
  ELSEWHERE
        ZL_WORK(:,:)=0.0
  ENDWHERE
ENDIF
!
YRECFM='rootdsl' 
YCOMMENT='Root Distribution excluded non-vegetated area (-)'
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
!-------------------------------------------------------------------------------
! * Capacity of Soil to Store Water
!-------------------------------------------------------------------------------
!
! CMIP asks the total water holding capacity of all the soil in the grid cell relatively to the field capacity.
! even if it should be the porosity. The field capacity of soil is the maximum content of water it can retain 
! against gravitational drainage.
!
ZWORK  (:) = 0.0
!
IF(IO%CISBA=='DIF')THEN
!
  DO JP=1,INP
     PK => NP%AL(JP)
     KK => NK%AL(JP)
     DO JL=1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF)THEN
             ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*KK%XWFC(JI,JL)*PK%XDZG(JI,JL)*XRHOLW
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!
ELSE
!
  DO JP=1,INP
     PK => NP%AL(JP)
     KK => NK%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*KK%XWFC(JI,1)*PK%XDG(JI,2)*XRHOLW
        IF(IO%CISBA=='3-L')THEN
          ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*KK%XWSAT(JI,1)*MAX(0.0,PK%XDG(JI,3)-PK%XDG(JI,2))*XRHOLW
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
YRECFM='mrsofc'
YCOMMENT='Capacity of Soil to Store Water (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Soil hydraulic properties
!-------------------------------------------------------------------------------
!
IF(IO%CISBA=='DIF')THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JL=1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IWORK(IMASK,JP) = PK%NWG_LAYER(JI)
        ENDDO
     ENDDO
  ENDDO
ELSE
  IWORK(:,:) = INL
ENDIF
!
! * Saturated Hydraulic Conductivity
!
YRECFM='ksat'
YCOMMENT='Saturated Hydraulic Conductivity over land (m s-1)'
ZL_WORK(:,:) = 0.0
ZL_SUMP(:,:) = 0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         IF(JL<=IWORK(IMASK,JP).AND.IWORK(IMASK,JP)/=NUNDEF)THEN
           ZL_WORK(IMASK,JL)=ZL_WORK(IMASK,JL)+PK%XPATCH(JI)*PK%XCONDSAT(JI,JL)
           ZL_SUMP(IMASK,JL)=ZL_SUMP(IMASK,JL)+PK%XPATCH(JI)
         ENDIF
      ENDDO
   ENDDO
ENDDO
WHERE(ZL_SUMP(:,:)>0.0)
      ZL_WORK(:,:)=ZL_WORK(:,:)/ZL_SUMP(:,:)
ELSEWHERE
      ZL_WORK(:,:)=0.0
ENDWHERE
CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
YRECFM='soilsat'
YCOMMENT='soil saturation over land (m3/m3)'
ZL_WORK(:,:)=K%XWSAT(:,:)
IF(IO%CISBA=='DIF')THEN
  WHERE(ZL_SUMP(:,:)==0.0)ZL_WORK(:,:)=0.0
  CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
ELSE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZL_WORK(:,1),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
YRECFM='fldcapacity'
YCOMMENT='Field Capacity over land (m3/m3)'
ZL_WORK(:,:)=K%XWFC(:,:)
IF(IO%CISBA=='DIF')THEN
  WHERE(ZL_SUMP(:,:)==0.0)ZL_WORK(:,:)=0.0
  CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
ELSE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZL_WORK(:,1),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
YRECFM='wilt'
YCOMMENT='Wilting Point over land (m3/m3)'
ZL_WORK(:,:)=K%XWWILT(:,:)
IF(IO%CISBA=='DIF')THEN
  WHERE(ZL_SUMP(:,:)==0.0)ZL_WORK(:,:)=0.0
  CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
ELSE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZL_WORK(:,1),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (IO%CISBA=='DIF') THEN
!
   YRECFM='mpotsat'
   YCOMMENT='Matric potential at saturation (m)'
   ZL_WORK(:,:)=K%XMPOTSAT(:,:)
   WHERE(ZL_SUMP(:,:)==0.0)ZL_WORK(:,:)=0.0
   CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
   YRECFM='bcoef'
   YCOMMENT='Slope of the soil water retention curve (-)'
   ZL_WORK(:,:)=K%XBCOEF(:,:)
   WHERE(ZL_SUMP(:,:)==0.0)ZL_WORK(:,:)=0.0
   CALL WRITE_3D_FIELD(INL,YGROUND_LAYER_DIM_NAME,ZL_WORK)
!
ENDIF
!-------------------------------------------------------------------------------
! * Land-use Tiles Fraction (fixed field)
!-------------------------------------------------------------------------------
!
IF (ID%O%LLUTILES_BUDGET) THEN
!
  YRECFM='fracLut'
  YCOMMENT='fraction of land-use tile (%)'
  DO JP=1,INP
     DO JI=1,INI
        ZP_WORK(JI,JP)=S%XFRAC_LAND(JI)*100.0
     ENDDO
  ENDDO   
  ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZP_WORK(:,:),LFRAC=.TRUE.)
  CALL WRITE_3D_FIELD(NLUT,YLANDUSE_TILES_DIM_NAME,ZWORK_LUT)
!
ENDIF
!
YRECFM='nwdFracLut'
YCOMMENT='fraction of land use tile tile that is non-woody vegetation (-)'
ZWORK(:)=S%XFRAC_LAND(:)*(S%XVEGTYPE(:,NVT_NO  ) &
                         +S%XVEGTYPE(:,NVT_ROCK) &
                         +S%XVEGTYPE(:,NVT_SNOW) &
                         +S%XVEGTYPE(:,NVT_C3  ) &
                         +S%XVEGTYPE(:,NVT_C4  ) &
                         +S%XVEGTYPE(:,NVT_IRR ) &
                         +S%XVEGTYPE(:,NVT_GRAS) &
                         +S%XVEGTYPE(:,NVT_TROG) &
                         +S%XVEGTYPE(:,NVT_PARK) &
                         +S%XVEGTYPE(:,NVT_BOGR) )
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
!
!
!-------------------------------------------------------------------------------
! * Vegetation cover fractions in % of total area
!-------------------------------------------------------------------------------
!
!Patch 1 = Tree
!Patch 2 = Shrub (not yet in isba)
!Patch 3 = natural Grass
!Patch 4 = Crop
!Patch 5 = Pasture (not yet in isba)
!Patch 6 = no-vegetated (baresoil and rock)
!Patch 7 = residual (permanent snow and ice)
!
DO JP=1,INP
   DO JI=1,INI
      ZP_WORK(JI,JP)=S%XFRAC_LAND(JI)*100.0 
   ENDDO
ENDDO
ZWORKTILES(:,:)=PATCH_TO_TILE_FUNC(S%XPATCH(:,:),ZP_WORK(:,:),LFRAC=.TRUE.)
!
! * tree fraction
!
YRECFM='treeFrac'
YCOMMENT='Tree Cover Fraction over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,1),IRESP,HCOMMENT=YCOMMENT)
!
! * shrub fraction
!
IF(IO%NPATCH==19)THEN
  YRECFM='shrubFrac'
  YCOMMENT='Shrub Cover Fraction over land (%)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,2),IRESP,HCOMMENT=YCOMMENT)
ELSE
  ZWORKTILES(:,2) = 0.0
ENDIF
!
! * grass fraction
!
YRECFM='grassFrac'
YCOMMENT='Grass Cover Fraction over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,3),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='grassFracC3'
YCOMMENT='C3 Grass Cover Fraction over land (%)'
ZWORK(:)=S%XFRAC_LAND(:)*100.0*(S%XVEGTYPE(:,NVT_GRAS) &
                               +S%XVEGTYPE(:,NVT_PARK) &
                               +S%XVEGTYPE(:,NVT_BOGR) )
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='grassFracC4'
YCOMMENT='C4 Grass Cover Fraction over land (%)'
ZWORK(:)=S%XFRAC_LAND(:)*100.0*(S%XVEGTYPE(:,NVT_TROG) &
                               +S%XVEGTYPE(:,NVT_IRR ) )
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * crop fraction
!
YRECFM='cropFrac'
YCOMMENT='Crop Cover Fraction over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,4),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='cropFracC3'
YCOMMENT='C3 Crop Cover Fraction over land (%)'
ZWORK(:)=S%XVEGTYPE(:,NVT_C3)*S%XFRAC_LAND(:)*100.0
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='cropFracC4'
YCOMMENT='C4 Crop Cover Fraction over land (%)'
ZWORK(:)=S%XVEGTYPE(:,NVT_C4)*S%XFRAC_LAND(:)*100.0
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * pasture fraction (not yet in Surfex)
!
!IF(IO%NPATCH==19)THEN
!  YRECFM='pastureFrac'
!  YCOMMENT='Pasture Cover Fraction over land (%)'
!  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,5),IRESP,HCOMMENT=YCOMMENT)
!ELSE
  ZWORKTILES(:,5) = 0.0
!ENDIF
!
! * Total vegetation fraction
!
YRECFM='vegFrac'
YCOMMENT='Total vegetated fraction (%)'
ZWORK(:) = 0.0
DO JP=1,5
   DO JI=1,INI
      ZWORK(JI) = ZWORK(JI) + ZWORKTILES(JI,JP)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * baresoil fraction
!
YRECFM='baresoilFrac'
YCOMMENT='Baresoil and rock cover Fraction over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,6),IRESP,HCOMMENT=YCOMMENT)
!
! * ResidualFrac fraction
!
YRECFM='residualFrac'
YCOMMENT='Residual Cover Fraction over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,7),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Fraction of Grid Cell Covered with Glacier of total area
!-------------------------------------------------------------------------------
!
YRECFM='sftgif' !sftgrf
YCOMMENT='Fraction of Grid Cell Covered with Glacier over land (%)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORKTILES(:,7),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Plant Functional Type Grid Fraction of total area
!-------------------------------------------------------------------------------
!
YRECFM='lCoverFrac'
DO JP=1,NVEGTYPE
   DO JI=1,INI
      ZVEGTYPE(JI,JP)=S%XVEGTYPE(JI,JP)*S%XFRAC_LAND(JI)*100.0 
   ENDDO
ENDDO
YCOMMENT='Plant Functional Type Grid Fraction over land (%)'
CALL WRITE_3D_FIELD(NVEGTYPE,YVEGTYPE_DIM_NAME,ZVEGTYPE)
!
!
!-------------------------------------------------------------------------------
! * C3 and C4 cover fractions in % of total area
!-------------------------------------------------------------------------------
!
YRECFM='c3PftFrac'
YCOMMENT='Percentage of entire grid cell that is covered by C3 PFTs (%)'
ZWORK(:)=0.0
DO JP=4,7
   DO JI=1,INI
      ZWORK(JI)=ZWORK(JI)+S%XVEGTYPE(JI,JP)*S%XFRAC_LAND(JI)*100.0
   ENDDO
ENDDO
ZWORK(:)=ZWORK(:)+S%XVEGTYPE(:,10)*S%XFRAC_LAND(:)*100.0
DO JP=12,NVEGTYPE
   DO JI=1,INI
      ZWORK(JI)=ZWORK(JI)+S%XVEGTYPE(JI,JP)*S%XFRAC_LAND(JI)*100.0
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='c4PftFrac'
YCOMMENT='Percentage of entire grid cell that is covered by C4 PFTs (%)'
ZWORK(:)=0.0
DO JP=8,9
   DO JI=1,INI
      ZWORK(JI)=ZWORK(JI)+S%XVEGTYPE(JI,JP)*S%XFRAC_LAND(JI)*100.0
   ENDDO
ENDDO
ZWORK(:)=ZWORK(:)+S%XVEGTYPE(:,11)*S%XFRAC_LAND(:)*100.0
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE WRITE_3D_FIELD(KDIM,YDIM,PWORK)
!
USE MODI_WRITE_SURF
!
INTEGER,              INTENT(IN) :: KDIM
CHARACTER(LEN=*),     INTENT(IN) :: YDIM
REAL, DIMENSION(:,:), INTENT(IN) :: PWORK
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YREC           ! Name of the article to be write
CHARACTER(LEN=4 ) :: YLVL
INTEGER           :: ICH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_ISBA_N:WRITE_3D_FIELD',0,ZHOOK_HANDLE)
!
IF(HPROGRAM=='FA')THEN
  ICH=MIN(11,LEN_TRIM(YRECFM))
ELSE
  ICH=LEN_TRIM(YRECFM)
ENDIF
!
IF (LALLOW_ADD_DIM)  THEN
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,PWORK(:,:),IRESP,HCOMMENT=YCOMMENT,HNAM_DIM=YDIM)
ELSE
  DO JL=1,KDIM
     WRITE(YLVL,'(I4)') JL
     YREC=YRECFM(:ICH)//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,PWORK(:,JL),IRESP,HCOMMENT=YCOMMENT)
   ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_ISBA_N:WRITE_3D_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_3D_FIELD
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_MIP_FX_ISBA_n
