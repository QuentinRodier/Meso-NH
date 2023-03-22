!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_LUT_ISBA_n(DTCO, DUO, U, ID, IO, S, K, NP, NPE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_LUT_ISBA* - writes the ISBA diagnostic fields for CMIP
!!
!!    PURPOSE
!!    -------
!!    this routines handles the writing of output fields by cmip tiles
!!    Note: tiling depends on patch that can be modified in LANDUSE_TILES_PAR module 
!!
!!**  METHOD
!!    ------
!!    produce surfex field under landuse tiles (psl,crp,pst,urb)
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      R. Séférian *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2016
!!      Roland Séférian   05/2017 correct units for carbon flux
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_DIAG_n,           ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_SURFEX_n,         ONLY : ISBA_DIAG_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_NP_t, ISBA_P_t, ISBA_NPE_t, ISBA_PE_t, ISBA_S_t, ISBA_K_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_XIOS,       ONLY : NLUT, LALLOW_ADD_DIM, YLANDUSE_TILES_DIM_NAME
!
USE MODD_CO2V_PAR,   ONLY : XMC, XMCO2, XPCCO2, XGTOKG
USE MODD_CSTS,       ONLY : XDAY
!
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
USE MODI_WRITE_SURF
USE MODI_UNPACK_SAME_RANK
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
TYPE(DIAG_t),           POINTER :: DK
TYPE(ISBA_P_t),         POINTER :: PK
TYPE(ISBA_PE_t),        POINTER :: PEK
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
TYPE(DIAG_MISC_ISBA_t), POINTER :: DMK
!
!
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: JI, JL, JK, JP, INP, INL, INLIT, IMASK
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH) :: ZNOPATCH
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH) :: ZWORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH) :: ZNBP
REAL, DIMENSION(U%NSIZE_NATURE,NLUT)      :: ZWORK_LUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N',0,ZHOOK_HANDLE)
!
INP=IO%NPATCH
INL=IO%NGROUND_LAYER
INLIT=IO%NNLITTER
!
ZWORK    (:,:) = XUNDEF
ZNBP     (:,:) = XUNDEF
ZWORK_LUT(:,:) = XUNDEF
ZNOPATCH (:,:) = 1.0
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Near surface atmospheric variables 
!-------------------------------------------------------------------------------
!
YRECFM='tasLut'
YCOMMENT='near-surface air temperature (2m above displacement height, i.e. t_ref) on land-use tile (K)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XT2M(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='hussLut'
YCOMMENT='near-surface air specific humidty (2m above displacement height, i.e. t_ref) on land-use tile (-)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XQ2M(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Radiative fluxes
!-------------------------------------------------------------------------------
!
YRECFM='rsusLut'
YCOMMENT='short wave upward radiation on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XSWU(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='rlusLut'
YCOMMENT='long wave upward radiation on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XLWU(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='rsdsLut'
YCOMMENT='short wave upward radiation on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XSWD(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='rldsLut'
YCOMMENT='long wave upward radiation on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XLWD(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Energy fluxes
!-------------------------------------------------------------------------------
!
YRECFM='hfssLut'
YCOMMENT='Sensible heat flux on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XH(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='hflsLut'
YCOMMENT='Sensible heat flux on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XLE(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
YRECFM='nrjbudLut'
YCOMMENT='Energy budget as residue on land-use tile (W m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XNRJBUD(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Surface Temperature (K)
!-------------------------------------------------------------------------------
!
YRECFM='tslsiLut'
YCOMMENT='skin temperature on land-use tile (K)'
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DK%XTSRAD(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Land water reservoirs
!-------------------------------------------------------------------------------
!
! * top soil reservoir
!
YRECFM='mrsosLut'
IF(IO%CISBA=='DIF') THEN
  YCOMMENT='top soil moisture over 10cm on land-use tile (kg m-2)'
ELSE
  YCOMMENT='surface soil moisture over 1cm on land-use tile (kg m-2)'
ENDIF
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XSURF_TWG(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
! * root soil reservoir
!
YRECFM='rzwcLut'
YCOMMENT='root zone soil moisture on land-use tile (kg m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XROOT_TWG(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
! * total soil reservoir
!
YRECFM='mrsoLut'
YCOMMENT='total soil moisture on land-use tile (kg m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XSOIL_TWG(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
! * snow water equivalent
!
YRECFM='sweLut'
YCOMMENT='snow water equivalent on land-use tile (kg m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XTWSNOW(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)

!-------------------------------------------------------------------------------
! * Soil wetness
!-------------------------------------------------------------------------------
!
YRECFM='mrsowLut'
YCOMMENT='total soil wetness on land-use tile (-)'
DO JP=1,INP
   PK => NP%AL(JP)
   DMK => ID%NDM%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XSOIL_TSWI(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Runoff fluxes
!-------------------------------------------------------------------------------
!
YRECFM='mrroLut'
YCOMMENT='total runoff on land-use tile (kg m-2 s-1)'
ZWORK(:,:)=XUNDEF
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRUNOFF(:)+DEK%XDRAIN(:),ZWORK(:,JP),XUNDEF)
ENDDO
IF(IO%LGLACIER)THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK,JP)=ZWORK(IMASK,JP)+DEK%XICEFLUX(JI)
     ENDDO
  ENDDO
ENDIF
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
! * Vegetation properties
!-------------------------------------------------------------------------------
!
! * vegetation fraction
!
YRECFM='cncLut'
YCOMMENT='canopy covered fraction on land-use tile  (-)'
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XVEG(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
! leaf area Index
!
YRECFM='laiLut'
YCOMMENT='leaf area index on land-use tile  (-)'
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XLAI(:),ZWORK(:,JP),XUNDEF)
ENDDO
WHERE(ZWORK(:,:) == XUNDEF )
  ZWORK(:,:) = 0.0
ENDWHERE
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! * Carbon general DIAG
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF(IO%CPHOTO=='NON')THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! * Natural Carbon flux
!-------------------------------------------------------------------------------
!
! * Gross primary productivity
!
YRECFM='gppLut'
YCOMMENT='gross primary productivity of carbon on land-use tile (kgC m-2 s-1)'
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:),ZWORK(:,JP),XUNDEF)
ENDDO
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Autotrophic Respiration
!
YRECFM='raLut'
YCOMMENT='plant respiration carbon flux on land-use tile (kgC m-2 s-1)'
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESP_AUTO(:),ZWORK(:,JP),XUNDEF)
ENDDO
WHERE(ZWORK(:,:)/=XUNDEF)ZWORK(:,:)=ZWORK(:,:)*(XMC/XMCO2)
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!    
! * Heterotrophic Respiration
!
YRECFM='rhLut'
YCOMMENT='heterotrophic respiration carbon flux on land-use tile (kgC m-2 s-1)'
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESP_ECO(:)-DEK%XRESP_AUTO(:),ZWORK(:,JP),XUNDEF)
ENDDO
WHERE(ZWORK(:,:)/=XUNDEF)ZWORK(:,:)=ZWORK(:,:)*(XMC/XMCO2)
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Net Primary Productivity
!
YRECFM='nppLut'
YCOMMENT='net primary productivity of carbon on land-use tile (kgC m-2 s-1)'
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:)-DEK%XRESP_AUTO(:),ZWORK(:,JP),XUNDEF)
ENDDO
WHERE(ZWORK(:,:)/=XUNDEF)ZWORK(:,:)=ZWORK(:,:)*(XMC/XMCO2)
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
!-------------------------------------------------------------------------------
! * Biome Carbon flux
!-------------------------------------------------------------------------------
!
! * Net land to Atmosphere Carbon Flux
!
YRECFM='nbpLut'
YCOMMENT='Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on land-use tile (kgC m-2 s-1)'
!
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:)-DEK%XRESP_ECO(:),ZNBP(:,JP),XUNDEF)
ENDDO
WHERE(ZNBP(:,:)/=XUNDEF)ZNBP(:,:)=ZNBP(:,:)*(XMC/XMCO2)
!
IF(IO%LFIRE  )THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZNBP(IMASK,JP) = ZNBP(IMASK,JP) - DEK%XFIRECO2(JI)
     ENDDO
  ENDDO
ENDIF
!
IF(IO%LCLEACH)THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZNBP(IMASK,JP) = ZNBP(IMASK,JP) - DEK%XFDOC(JI)
     ENDDO
  ENDDO
ENDIF
!
IF(IO%LLULCC )THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZNBP (IMASK,JP) = ZNBP (IMASK,JP) - PEK%XFLUATM(JI) - DEK%XFHARVEST(JI) - PEK%XFANTATM(JI)
     ENDDO
  ENDDO
ENDIF
!
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZNBP(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Net Ecosystem Carbon Balance
!
YRECFM='necbLut'
YCOMMENT='net rate of carbon accumulation (or loss)  on land-use tile (kgC m-2 s-1)'
ZWORK(:,:) = ZNBP(:,:)
IF(IO%LLULCC )THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK,JP) = ZWORK(IMASK,JP) + PEK%XFANTATM(JI)
     ENDDO
  ENDDO
ENDIF
!
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
!-------------------------------------------------------------------------------
! * Vegetation Carbon Stock
!-------------------------------------------------------------------------------
!
IF(IO%CPHOTO=='NIT'.OR.IO%CPHOTO=='NCB')THEN
  YRECFM='cVegLut'
  YCOMMENT='Carbon Mass in vegetation on land-use tile (kgC m-2)'
  ZWORK(:,:) = 0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JL=1,IO%NNBIOMASS
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZWORK(IMASK,JP)=ZWORK(IMASK,JP)+PEK%XBIOMASS(JI,JL)*XPCCO2
        ENDDO
     ENDDO
  ENDDO
  ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
  CALL WRITE_LUT_FIELD(ZWORK_LUT)
ENDIF
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! * Soil Carbon Diag (CNT)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF(IO%CRESPSL/='CNT'.AND.IO%CRESPSL/='DIF')THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! * Natural Carbon Stock
!-------------------------------------------------------------------------------
!
! * Total litter carbon stock
!
YRECFM='cLitterLut'
YCOMMENT='litter carbon content on land-use tile (kgC m-2)'
!
ZWORK(:,:)=0.0
!
IF(IO%CRESPSL=='CNT')THEN
!        
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JL=1,INLIT
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZWORK(IMASK,JP)=ZWORK(JI,JP)+(PEK%XLITTER(JI,JL,1)+PEK%XLITTER(JI,JL,2))*XGTOKG
        ENDDO
     ENDDO
  ENDDO
!
ELSEIF(IO%CRESPSL=='DIF')THEN
!        
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JK=1,INLIT
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZWORK(IMASK,JP) = ZWORK(IMASK,JP)+PEK%XSURFACE_LITTER(JI,JK)*XGTOKG
        ENDDO
     ENDDO
  ENDDO
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JK=1,INLIT
        DO JL=1,INL
           DO JI=1,PK%NSIZE_P
              IMASK = PK%NR_P(JI)
              IF(JL<=PK%NWG_LAYER(JI))THEN
                ZWORK(IMASK,JP) = ZWORK(IMASK,JP)+PEK%XSOILDIF_LITTER(JI,JL,JK)*XGTOKG
              ENDIF
           ENDDO
        ENDDO
     ENDDO
  ENDDO
!        
ENDIF
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT)
!
! * Total Soil carbon stock
!
YRECFM='cSoilLut'
YCOMMENT='soil carbon content on land-use tile (kgC m-2 s-1)'
ZWORK(:,:)=0.0
IF(IO%CRESPSL=='CNT')THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JK=1,IO%NNSOILCARB
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZWORK(IMASK,JP)=ZWORK(IMASK,JP)+PEK%XSOILCARB(JI,JK)*XGTOKG
        ENDDO
     ENDDO
  ENDDO
ELSEIF(IO%CRESPSL=='DIF')THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JL=1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IF(JL<=PK%NWG_LAYER(JI))THEN
               ZWORK(IMASK,JP) = ZWORK(IMASK,JP)+(PEK%XSOILDIF_CARB(JI,JL,1) &
                                                + PEK%XSOILDIF_CARB(JI,JL,2) &
                                                + PEK%XSOILDIF_CARB(JI,JL,3))*XGTOKG
           ENDIF
        ENDDO
     ENDDO
 ENDDO        
ENDIF
ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
!-------------------------------------------------------------------------------
! * Riverine doc Flux
!-------------------------------------------------------------------------------
!
IF (IO%LCLEACH) THEN
   YRECFM='fCLeachLut'
   YCOMMENT='leaching carbon runoff (from soil to river) on land-use tile (kgC m-2 s-1)'
   DO JP=1,INP
      PK => NP%AL(JP)
      DEK => ID%NDE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XFDOC(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
ENDIF
!
!-------------------------------------------------------------------------------
! * Fire Carbon emissions
!-------------------------------------------------------------------------------
!
IF (IO%LFIRE) THEN
   YRECFM='cTotFireLut'
   YCOMMENT='total carbon loss from natural and managed fire on land-use tile (kgC m-2 s-1)'
   DO JP=1,INP
      PK => NP%AL(JP)
      DEK => ID%NDE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XFIRECO2(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(S%XPATCH(:,:),ZWORK(:,:))
   CALL WRITE_LUT_FIELD(ZWORK_LUT)
ENDIF
!
!-------------------------------------------------------------------------------
! * Land-use Land-cover Change Fluxes 
!   (allready account for pacth fraction except fRegrowthLut)
!-------------------------------------------------------------------------------
!
IF (IO%LLULCC) THEN
!
! * Carbon flux toward the atmosphere due to land-use clearance
!
   YRECFM='fDeforestLut'
   YCOMMENT='Deforested biomass as a result of anthropogenic land-use change on land-use tile (kgC m-2 s-1)'
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XFLUATM(:)+PEK%XFLURES(:)+PEK%XFLUANT(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Carbon flux toward the atmosphere due to land-use clearance
!
   YRECFM='fLulccAtmLut'
   YCOMMENT='carbon mass flux into atmosphere due to any human activity on land-use tile (kgC m-2 s-1)'
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XFLUATM(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Carbon flux toward the anthropogenic carbon pool due to land-use clearance
!
   YRECFM='fLulccProLut'
   YCOMMENT='carbon harvested that enters anthropogenic product pools on land-use tile (kgC m-2 s-1)'  
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XFLUANT(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Carbon flux toward the litter carbon pool due to land-use clearance
!
   YRECFM='fLulccResLut'
   YCOMMENT='carbon transferred to soil or litter pools on land-use tile (kgC m-2 s-1)'
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XFLURES(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Carbon flux from the anthropogenic carbon pool to the atmosphere
!
   YRECFM='fProdDecoLut'
   YCOMMENT='decomposition out of product pools to CO2 in atmosphere on land-use tile (kgC m-2 s-1)' 
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XFANTATM(:),ZWORK(:,JP),XUNDEF)
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
! * Anthropogenic carbon pool
!
   YRECFM='cProductLut'
   YCOMMENT='carbon content of products of anthropogenic land-use change on land-use tile (kgC m-2)'  
   ZWORK(:,:)=0.0
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,IO%NNDECADAL
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZWORK(IMASK,JP)=ZWORK(IMASK,JP)+PEK%XCSTOCK_DECADAL(JI,JL)
         ENDDO
      ENDDO
   ENDDO
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,IO%NNCENTURY
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZWORK(IMASK,JP)=ZWORK(IMASK,JP)+PEK%XCSTOCK_CENTURY(JI,JL)
         ENDDO
      ENDDO
   ENDDO
   ZWORK_LUT(:,:)=PATCH_TO_LUT_FUNC(ZNOPATCH(:,:),ZWORK(:,:),LFRAC=.TRUE.)
   CALL WRITE_LUT_FIELD(ZWORK_LUT) 
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N',1,ZHOOK_HANDLE)
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE WRITE_LUT_FIELD(PWORK)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWORK
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YREC           ! Name of the article to be write
CHARACTER(LEN=4 ) :: YLVL
INTEGER           :: ICH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N:WRITE_LUT_FIELD',0,ZHOOK_HANDLE)
!
IF(HPROGRAM=='FA')THEN
  ICH=MIN(11,LEN_TRIM(YRECFM))
ELSE
  ICH=LEN_TRIM(YRECFM)
ENDIF
!
IF (LALLOW_ADD_DIM)  THEN
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,PWORK(:,:),IRESP,YCOMMENT,HNAM_DIM=YLANDUSE_TILES_DIM_NAME)
ELSE
  DO JL=1,NLUT
     WRITE(YLVL,'(I4)') JL
     YREC=YRECFM(:ICH)//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,PWORK(:,JL),IRESP,YCOMMENT)    
   ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_LUT_ISBA_N:WRITE_LUT_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_LUT_FIELD
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_MIP_LUT_ISBA_n
