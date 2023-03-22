!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_MISC_ISBA_n (DTCO, DUO, U, ID, IO, S, K, NP, NPE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_MISC_ISBA*
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
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_DIAG_n,         ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
USE MODD_SURFEX_n,       ONLY : ISBA_DIAG_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_NP_t, ISBA_P_t, ISBA_NPE_t, ISBA_PE_t, ISBA_S_t, ISBA_K_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
!
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM,          &
                      YPATCHES_DIM_NAME,       &
                      YGROUND_LAYER_DIM_NAME
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_UNPACK_SAME_RANK
USE MODI_WRITE_PATCH_CMIP_ISBA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_DIAG_t),    INTENT(INOUT) :: ID
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_K_t),       INTENT(INOUT) :: K
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t),         POINTER :: PK
TYPE(ISBA_PE_t),        POINTER :: PEK
TYPE(DIAG_t),           POINTER :: DK
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
TYPE(DIAG_MISC_ISBA_t), POINTER :: DMK
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=14) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
CHARACTER(LEN=4 ) :: YLVL
CHARACTER(LEN=20) :: YFORM
!
REAL, DIMENSION(U%NSIZE_NATURE)                  :: ZWORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)        :: ZP_WORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZL_WORK
!
INTEGER           :: JI, JL, JP, INL, INP, IWORK, IMASK
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
INL  = IO%NGROUND_LAYER
INP  = IO%NPATCH
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE', 'ISBA_DIAGNOSTICS.OUT.nc')
!
ZWORK  (:  ) = XUNDEF
ZP_WORK(:,:) = XUNDEF
ZL_WORK(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
! * Vegetation properties
!-------------------------------------------------------------------------------
!
! * leaf area Index
!
YRECFM='lai'
YCOMMENT='leaf area index over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XLAI(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * vegetation fraction
!
YRECFM='cnc'
YCOMMENT='Canopy covered (or veg) fraction (-)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK  (IMASK   )=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XVEG(JI)
      ZP_WORK(IMASK,JP)=PEK%XVEG(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
!-------------------------------------------------------------------------------
! * Soil wetness
!-------------------------------------------------------------------------------
!
YRECFM='mrsow'
YCOMMENT='total soil wetness over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XSOIL_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='mrsowrz'
YCOMMENT='total soil wetness index over the root zone (-)'
IF(IO%CISBA=='DIF') THEN
  ZWORK(:)=ID%DM%XFRD2_TSWI(:)
ELSE
  ZWORK(:)=ID%DM%XTSWI(:,2)
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='vegstress'
YCOMMENT='soil water stress index for plant transpiration over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XF2(:),IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XF2(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
!-------------------------------------------------------------------------------
! * Snow properties
!-------------------------------------------------------------------------------
!
! * fractions of precipitation on snow pack
!
YRECFM='prrsn'
YCOMMENT='fraction of rainfall on snow over land (-)'
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   WHERE(ID%DE%XRAINFALL(:)*ID%DM%XPSN(:)>0.0)
         ZWORK(:)=ID%DM%XPSN(:)
   ELSEWHERE
         ZWORK(:)=XUNDEF
   ENDWHERE
ELSE
   ZWORK(:)=0.0
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='prsnsn'
YCOMMENT='fraction of snowfall on snow over land (-)'
WHERE(ID%DE%XSNOWFALL(:)*ID%DM%XPSN(:)>0.0)
      ZWORK(:)=1.0
ELSEWHERE
      ZWORK(:)=XUNDEF
ENDWHERE
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * snow fractions
!
YRECFM='sncg'
YCOMMENT='snow covered fraction over bare land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XPSNG(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sncv'
YCOMMENT='snow covered fraction over veg (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XPSNV(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='snct'
YCOMMENT='total snow covered fraction including vegetation masking over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XPSN(:),IRESP,HCOMMENT=YCOMMENT)
!
! * snow age
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   YRECFM='agesno'
   YCOMMENT='snow age over land (day)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTSNOWAGE(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
! * Sub-Grid hydrology (including Floodplain and groundwater) properties
!-------------------------------------------------------------------------------
!
IF (IO%CRUNOFF=='SGH '.OR.IO%CRUNOFF=='DT92') THEN     
   YRECFM='fsat'
   YCOMMENT='soil saturated fraction (-)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XFSAT(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (IO%LFLOOD) THEN
   YRECFM='fldplainfrac'
   YCOMMENT='floodplain fraction see by isba over land (-)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,K%XFFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (IO%LWTD) THEN
!
   YRECFM='wtd_isba'
   YCOMMENT='water table depth see by isba over land (m)'
   WHERE(K%XFWTD(:)>0.0)
         ZWORK(:)=MAX(0.0,-K%XWTD(:))
   ELSEWHERE
         ZWORK(:)=XUNDEF
   ENDWHERE
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='wtdf_isba'
   YCOMMENT='grid-cell fraction of water table to rise see by isba over land (-)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,K%XFWTD(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Frozen soil properties
!-------------------------------------------------------------------------------
!
IF (IO%CISBA=='DIF') THEN
!
! * Permafrost active layer thickness
!
   YRECFM='dmlt'
   YCOMMENT='depth to soil thaw (active layer thickess) over land (m)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XALT(:),IRESP,HCOMMENT=YCOMMENT)
   DO JP=1,INP
      PK => NP%AL(JP)
      DMK => ID%NDM%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XALT(:),ZP_WORK(:,JP),XUNDEF)
   ENDDO
   CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Frozen soil depth
!
   YRECFM='dfr'
   YCOMMENT='frozen soil depth over land (m)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XFLT(:),IRESP,HCOMMENT=YCOMMENT)
   DO JP=1,INP
      PK => NP%AL(JP)
      DMK => ID%NDM%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XFLT(:),ZP_WORK(:,JP),XUNDEF)
   ENDDO
   CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Permafrost layer thickness
!
   YRECFM='tpf'
   YCOMMENT='permafrost layer thickness over land (m)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XPLT(:),IRESP,HCOMMENT=YCOMMENT)
   DO JP=1,INP
      PK => NP%AL(JP)
      DMK => ID%NDM%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XPLT(:),ZP_WORK(:,JP),XUNDEF)
   ENDDO
   CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Liquid water in permafrost layer 
!
   YRECFM='pflw'
   YCOMMENT='liquid water content of permafrost layer over land (kg m-2)'
   WHERE(ID%DM%XPLT(:)>0.0)
         ZWORK(:)=ID%DM%XSOIL_TWG(:)-ID%DM%XSOIL_TWGI(:)
   ELSEWHERE
         ZWORK(:)=0.0
   ENDWHERE
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Nudging Increment of water and snow
!-------------------------------------------------------------------------------
!
IF (IO%CNUDG_WG/='DEF') THEN
   !
   YRECFM='nudgincsm'
   YCOMMENT='Nudging Increment of Water in Soil Mositure over land (kg m-2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XNUDGINCSM(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (LALLOW_ADD_DIM) THEN
      !
      YRECFM='nudgincsml'
      YCOMMENT='Nudging Increment of Water in Soil Moisture over land per layer (kg m-2)'
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XNUDGINCSML(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
      !
      YRECFM='nudgincsm_p'
      YCOMMENT='Nudging Increment of Water in Soil Moisture over land per patch (kg m-2)'
      DO JP=1,INP
         PK => NP%AL(JP)
         DMK => ID%NDM%AL(JP)
         CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XNUDGINCSM(:),ZP_WORK(:,JP),XUNDEF)
      ENDDO
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZP_WORK(:,:),IRESP,YCOMMENT,HNAM_DIM=YPATCHES_DIM_NAME)
      !
      DO JP=1,INP
         PK => NP%AL(JP)
         DMK => ID%NDM%AL(JP)
         ZL_WORK(:,:)=XUNDEF
         WRITE(YLVL,'(I2)') JP
         YRECFM='nudgincsml_p'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
         YCOMMENT='Nudging Increment of Water in Soil Moisture over land per patch per layer (kg m-2)'
         CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XNUDGINCSML(:,:),ZL_WORK(:,:),XUNDEF)          
         CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZL_WORK(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
      ENDDO 
      !
   ENDIF 
   !
ENDIF
!
IF (IO%LNUDG_SWE) THEN
   !
   YRECFM='nudgincswe'
   YCOMMENT='Nudging Increment of Snow over land (kg m-2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XNUDGINCSWE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (LALLOW_ADD_DIM) THEN
      YRECFM='nudginswe_p'
      YCOMMENT='Nudging Increment of Snow per patch (kg m-2)'
      DO JP=1,INP
         PK => NP%AL(JP)
         DMK => ID%NDM%AL(JP)
         CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XNUDGINCSWE(:),ZP_WORK(:,JP),XUNDEF)
      ENDDO
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZP_WORK(:,:),IRESP,YCOMMENT,HNAM_DIM=YPATCHES_DIM_NAME)
      !
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_MISC_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_DIAG_MIP_MISC_ISBA_n
