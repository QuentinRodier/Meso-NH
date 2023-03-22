!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_CARBON_ISBA_n (DTCO, DUO, U, ID, IO, S, NP, NPE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_CARBON_ISBA*
!!
!!    PURPOSE
!!    -------
!!
!!    Writes the ISBA diagnostic fields relative to carbon cycle as specified by cmip
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
!!      R. Séférian   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2016
!!      Roland Séférian   05/2017 correct units for carbon flux
!!      Roland Séférian   11/2020 crop harvesting
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
USE MODD_ISBA_n,         ONLY : ISBA_NP_t, ISBA_P_t, ISBA_NPE_t, ISBA_PE_t, ISBA_S_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
!
USE MODD_CO2V_PAR,   ONLY : XMC, XMCO2, XPCCO2, XGTOKG
USE MODD_CSTS,       ONLY : XDAY
!
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM,            &
                      YSOIL_CARBON_POOL_DIM_NAME,&
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
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_DIAG_t),    INTENT(INOUT) :: ID
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
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
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=14) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=14) :: YWORK          ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
CHARACTER(LEN=4 ) :: YLVL
!
REAL, PARAMETER                              :: ZDGSFC100 = 1.0 !(m)
REAL, PARAMETER                              :: ZDGSFC30  = 0.3 !(m)
!
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZWORK
!
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZBIOMASS
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZLITTER, ZLIT_SOIL, ZLIT_SURF
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZSOILCARB, ZSOC100, ZSOC30
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZLULUCARB
REAL, DIMENSION(U%NSIZE_NATURE)               :: ZFLUC, ZFLUATM
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZSOILCARBL
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NNSOILCARB)  :: ZCSOILPOOLS
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)      :: ZP_WORK
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)      :: ZP_BIOMASS
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)      :: ZP_LITTER
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)      :: ZP_SOILCARB
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)      :: ZP_LULUCARB
!
REAL              :: ZWGHT
!
INTEGER           :: JI, JL, JK, JP, INP, INL, INLIT, INCARB, IMASK
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_CARBON_ISBA_N',0,ZHOOK_HANDLE)
!
INP    = IO%NPATCH
INL    = IO%NGROUND_LAYER
INLIT  = IO%NNLITTER
INCARB = IO%NNSOILCARB
!
ZWORK    (:) = XUNDEF
ZBIOMASS (:) = XUNDEF
ZLITTER  (:) = XUNDEF
ZSOILCARB(:) = XUNDEF
ZLULUCARB(:) = XUNDEF
ZFLUC    (:) = XUNDEF
ZFLUATM  (:) = XUNDEF
!
ZCSOILPOOLS(:,:) = XUNDEF
ZP_WORK    (:,:) = XUNDEF
ZP_BIOMASS (:,:) = XUNDEF
ZP_LITTER  (:,:) = XUNDEF
ZP_SOILCARB(:,:) = XUNDEF
ZP_LULUCARB(:,:) = XUNDEF
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Natural Carbon flux
!-------------------------------------------------------------------------------
!
! * Gross primary productivity (global and specific tilling)
!
YRECFM='gpp'
YCOMMENT='gross primary productivity of carbon (kgC m-2 s-1)'
ZWORK(:) = ID%DE%XGPP(:)*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Autotrophic Respiration
!
YRECFM='ra'
YCOMMENT='plant respiration carbon flux (kgC m-2 s-1)'
ZWORK(:) = ID%DE%XRESP_AUTO(:)*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESP_AUTO(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!    
! * Heterotrophic Respiration
!
YRECFM='rh'
YCOMMENT='heterotrophic respiration carbon flux (kgC m-2 s-1)'
ZWORK(:) = (ID%DE%XRESP_ECO(:)-ID%DE%XRESP_AUTO(:))*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESP_ECO(:)-DEK%XRESP_AUTO(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
!
! * Heterotrophic Respiration from soil
!
YRECFM='rhSoil'
YCOMMENT='heterotrophic respiration carbon flux from soil carbon (kgC m-2 s-1)'
ZWORK(:) = ID%DE%XRESPSCARB(:)*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESPSCARB(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Heterotrophic Respiration from litter
!
YRECFM='rhLitter'
YCOMMENT='heterotrophic respiration carbon flux from litter (kgC m-2 s-1)'
ZWORK(:) = ID%DE%XRESPLIT(:)*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XRESPLIT(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Net Primary Productivity
!
YRECFM='npp'
YCOMMENT='net primary productivity of carbon (kgC m-2 s-1)'
ZWORK(:) = (ID%DE%XGPP(:)-ID%DE%XRESP_AUTO(:))*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:)-DEK%XRESP_AUTO(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
! * Net Ecosystem Productivity
!
YRECFM='nep'
YCOMMENT='net ecosystem productivity of carbon (kgC m-2 s-1)'
ZWORK(:) = (ID%DE%XGPP(:)-ID%DE%XRESP_ECO(:))*(XMC/XMCO2)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   CALL UNPACK_SAME_RANK(PK%NR_P,DEK%XGPP(:)-DEK%XRESP_ECO(:),ZP_WORK(:,JP),XUNDEF)
ENDDO
WHERE(ZP_WORK(:,:)/=XUNDEF)ZP_WORK(:,:)=ZP_WORK(:,:)*(XMC/XMCO2)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! * Vegetation carbon diag
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF(IO%CPHOTO/='NIT'.AND.IO%CPHOTO/='NCB')THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_CARBON_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! * Land Carbon Stock
!-------------------------------------------------------------------------------
!
ZBIOMASS (:) = 0.0
ZLITTER  (:) = 0.0
ZLIT_SOIL(:) = 0.0
ZLIT_SURF(:) = 0.0
ZSOILCARB(:) = 0.0
ZLULUCARB(:) = 0.0
!
ZCSOILPOOLS(:,:) = 0.0
ZSOILCARBL (:,:) = 0.0
!
ZP_BIOMASS (:,:) = 0.0
ZP_LITTER  (:,:) = 0.0
ZP_SOILCARB(:,:) = 0.0
ZP_LULUCARB(:,:) = 0.0
!
! * vegetation biomass
!
YRECFM='cVeg'
YCOMMENT='Carbon Mass in vegetation (kgC m-2)'
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,IO%NNBIOMASS
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZBIOMASS  (IMASK   )=ZBIOMASS  (IMASK   )+PEK%XBIOMASS(JI,JL)*XPCCO2*PK%XPATCH(JI)
         ZP_BIOMASS(IMASK,JP)=ZP_BIOMASS(IMASK,JP)+PEK%XBIOMASS(JI,JL)*XPCCO2
      ENDDO
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZBIOMASS(:),IRESP,HCOMMENT=YCOMMENT)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_BIOMASS)
!
IF(IO%CRESPSL=='CNT')THEN
!
! * Total litter carbon stock
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JL=1,INLIT
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZLIT_SURF(IMASK   )=ZLIT_SURF(IMASK   )+PK%XPATCH(JI)*PEK%XLITTER(JI,JL,1)*XGTOKG
           ZLIT_SOIL(IMASK   )=ZLIT_SURF(IMASK   )+PK%XPATCH(JI)*PEK%XLITTER(JI,JL,2)*XGTOKG
           ZP_LITTER(IMASK,JP)=ZP_LITTER(IMASK,JP)+(PEK%XLITTER(JI,JL,1)+PEK%XLITTER(JI,JL,2))*XGTOKG
        ENDDO
     ENDDO
  ENDDO
!
! * Total soil carbon (kgC m-2)
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JL=1,INCARB
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZSOILCARB  (IMASK   )=ZSOILCARB  (IMASK   )+PEK%XSOILCARB(JI,JL)*XGTOKG*PK%XPATCH(JI)
           ZCSOILPOOLS(IMASK,JL)=ZCSOILPOOLS(IMASK,JL)+PEK%XSOILCARB(JI,JL)*XGTOKG*PK%XPATCH(JI)
           ZP_SOILCARB(IMASK,JP)=ZP_SOILCARB(IMASK,JP)+PEK%XSOILCARB(JI,JL)*XGTOKG
        ENDDO
     ENDDO
  ENDDO
!
ELSEIF(IO%CRESPSL=='DIF')THEN
!
! * Total litter carbon stock
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JK=1,INLIT
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZLIT_SURF(IMASK   ) = ZLIT_SURF(IMASK   )+PEK%XSURFACE_LITTER(JI,JK)*XGTOKG*PK%XPATCH(JI)
           ZP_LITTER(IMASK,JP) = ZP_LITTER(IMASK,JP)+PEK%XSURFACE_LITTER(JI,JK)*XGTOKG
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
                 ZLIT_SOIL(IMASK   ) = ZLIT_SOIL(IMASK   )+PEK%XSOILDIF_LITTER(JI,JL,JK)*XGTOKG*PK%XPATCH(JI)
                 ZP_LITTER(IMASK,JP) = ZP_LITTER(IMASK,JP)+PEK%XSOILDIF_LITTER(JI,JL,JK)*XGTOKG
              ENDIF
           ENDDO
        ENDDO
     ENDDO
  ENDDO
!
! * Total soil carbon (kgC m-2)
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JK=1,INCARB
        DO JL=1,INL
           DO JI=1,PK%NSIZE_P
              IMASK = PK%NR_P(JI)
              IF(JL<=PK%NWG_LAYER(JI))THEN
                 ZSOILCARB  (IMASK   ) = ZSOILCARB  (IMASK   )+PEK%XSOILDIF_CARB(JI,JL,JK)*XGTOKG*PK%XPATCH(JI)
                 ZSOILCARBL (IMASK,JL) = ZSOILCARBL (IMASK,JL)+PEK%XSOILDIF_CARB(JI,JL,JK)*XGTOKG*PK%XPATCH(JI)
                 ZCSOILPOOLS(IMASK,JK) = ZCSOILPOOLS(IMASK,JK)+PEK%XSOILDIF_CARB(JI,JL,JK)*XGTOKG*PK%XPATCH(JI)
                 ZP_SOILCARB(IMASK,JP) = ZP_SOILCARB(IMASK,JP)+PEK%XSOILDIF_CARB(JI,JL,JK)*XGTOKG
              ENDIF
           ENDDO
        ENDDO
     ENDDO
  ENDDO
!
ENDIF
!
IF(IO%CRESPSL=='CNT'.OR.IO%CRESPSL=='DIF')THEN
!
! * Total litter carbon stock
!
  YRECFM='cLitter'
  YCOMMENT='litter carbon content (kgC m-2)'
  ZLITTER(:)=ZLIT_SURF(:)+ZLIT_SOIL(:) 
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLITTER(:),IRESP,HCOMMENT=YCOMMENT)
  CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_LITTER)
!
! * Total soil carbon (kgC m-2)
!
  YRECFM='cSoil'
  YCOMMENT='soil carbon content (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSOILCARB(:),IRESP,HCOMMENT=YCOMMENT)
  CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_SOILCARB)
!
ENDIF
!
IF(IO%CPHOTO=='NCB'.AND.IO%LLULCC)THEN
!
! * Anthropogenic carbon pool
!
   YRECFM='cProduct'
   YCOMMENT='carbon_content_of_products_of_anthropogenic_land_use_change (kgC m-2)'
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,IO%NNDECADAL
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZLULUCARB  (IMASK   )=ZLULUCARB  (IMASK   )+PEK%XCSTOCK_DECADAL(JI,JL)*PK%XPATCH(JI)
            ZP_LULUCARB(IMASK,JP)=ZP_LULUCARB(IMASK,JP)+PEK%XCSTOCK_DECADAL(JI,JL)
         ENDDO
      ENDDO
   ENDDO
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,IO%NNCENTURY
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZLULUCARB  (IMASK   )=ZLULUCARB  (IMASK   )+PEK%XCSTOCK_CENTURY(JI,JL)*PK%XPATCH(JI)
            ZP_LULUCARB(IMASK,JP)=ZP_LULUCARB(IMASK,JP)+PEK%XCSTOCK_CENTURY(JI,JL)
         ENDDO
      ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLULUCARB(:),IRESP,HCOMMENT=YCOMMENT)
   CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_LULUCARB)
!
ENDIF
!
YRECFM='cLand'
YCOMMENT='Carbon Mass over land (kgC m-2)'
ZWORK(:)=ZBIOMASS(:)+ZLITTER(:)+ZSOILCARB(:)+ZLULUCARB(:)
ZP_WORK(:,:)=ZP_BIOMASS(:,:)+ZP_LITTER(:,:)+ZP_SOILCARB(:,:)+ZP_LULUCARB(:,:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
CALL WRITE_PATCH_CMIP_ISBA(DUO,U,ID,IO,S,HPROGRAM,YRECFM,YCOMMENT,ZP_WORK)
!
!-------------------------------------------------------------------------------
! * Low vegetation Carbon Stock
!-------------------------------------------------------------------------------
!
! * Leaf Carbon Stock
!
YRECFM='cLeaf'
YCOMMENT='Carbon Mass in Leaf (kgC m-2)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XBIOMASS(JI,1)*XPCCO2
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Stem Carbon Stock
!
YRECFM='cStem'
YCOMMENT='Carbon Mass in Stem (kgC m-2)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*(PEK%XBIOMASS(JI,2)+PEK%XBIOMASS(JI,3))*XPCCO2
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Wood Carbon Stock
!-------------------------------------------------------------------------------
!
IF(IO%CPHOTO=='NCB')THEN
!
! * Wood Carbon Stock
!
  YRECFM='cWood'
  YCOMMENT='Carbon Mass in Wood (kgC m-2)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XBIOMASS(JI,5)*XPCCO2
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Root Carbon Stock
!
  YRECFM='cRoot'
  YCOMMENT='Carbon Mass in Root (kgC m-2)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*(PEK%XBIOMASS(JI,4)+PEK%XBIOMASS(JI,6))*XPCCO2
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! * Soil Carbon Diag (CNT or DIF)
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
! * Above-ground litter carbon stock
!
YRECFM='cLitterSurf'
YCOMMENT='surface litter carbon content (kgC m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLIT_SURF(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Below-ground litter carbon stock
!
YRECFM='cLitterSub'
YCOMMENT='subsurface litter carbon content (kgC m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLIT_SOIL(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Soil carbon stock by layer
!
IF(IO%CRESPSL=='DIF')THEN
  YRECFM='cSoilLev'
  YCOMMENT='Carbon mass in each soil level  (kgC m-2)'      
  IF (LALLOW_ADD_DIM)  THEN
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSOILCARBL(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  ELSE
      DO JL=1,INL
          WRITE(YLVL,'(I4)') JL
          YWORK=YRECFM(:LEN_TRIM(YRECFM))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
          CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YWORK,ZSOILCARBL(:,JL),IRESP,YCOMMENT)    
      ENDDO
  ENDIF       
ENDIF
!
! * Soil carbon stock above 30cm and 1m
!
IF(IO%CRESPSL=='DIF')THEN       
  !
  ZSOC30(:)=0.0
  ZSOC100(:)=0.0
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     !
     DO JL=1,INL
        !
        DO JI=1,PK%NSIZE_P
           !
           IMASK = PK%NR_P(JI)
           IF(JL<=PK%NWG_LAYER(JI))THEN
             !
             ZWGHT=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDGSFC30-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))*PK%XPATCH(JI)*XGTOKG
             ZSOC30(IMASK)=ZSOC30(JI)+(PEK%XSOILDIF_CARB(JI,JL,1)+PEK%XSOILDIF_CARB(JI,JL,2)+PEK%XSOILDIF_CARB(JI,JL,3))*ZWGHT
             !
             ZWGHT=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDGSFC100-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))*PK%XPATCH(JI)*XGTOKG
             ZSOC100(IMASK)=ZSOC100(JI)+(PEK%XSOILDIF_CARB(JI,JL,1)+PEK%XSOILDIF_CARB(JI,JL,2)+PEK%XSOILDIF_CARB(JI,JL,3))*ZWGHT
             !
           ENDIF
           !
        ENDDO
        !
     ENDDO
    !
  ENDDO
  !
  YRECFM='cSoilAbov30'
  YCOMMENT='soil carbon content above 30 cm (kgC m-2)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSOC30(:),IRESP,YCOMMENT)    
  !
  YRECFM='cSoilAbov100'
  YCOMMENT='soil carbon content above 1 m (kgC m-2)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSOC100(:),IRESP,YCOMMENT)    
  !
ENDIF
!
! * Fast Soil carbon stock
!
YRECFM='cSoilFast'
YCOMMENT='fast soil pool carbon content (kgC m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZCSOILPOOLS(:,1),IRESP,YCOMMENT)    
!
! * Medium Soil carbon stock
!
YRECFM='cSoilMedium'
YCOMMENT='medium soil pool carbon content (kgC m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZCSOILPOOLS(:,2),IRESP,YCOMMENT)    
!
! * Slow Soil carbon stock
YRECFM='cSoilSlow'
YCOMMENT='slow soil pool carbon content (kgC m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZCSOILPOOLS(:,3),IRESP,YCOMMENT)    
!
! * Soil carbon stocks per pools
!
YRECFM='cSoilPools'
YCOMMENT='soil pools carbon content (kgC m-2 s-1)'
IF (LALLOW_ADD_DIM)  THEN
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZCSOILPOOLS(:,:),IRESP,YCOMMENT,HNAM_DIM=YSOIL_CARBON_POOL_DIM_NAME)
ELSE
    DO JL=1,INCARB
       WRITE(YLVL,'(I4)') JL
       YWORK=YRECFM(:LEN_TRIM(YRECFM))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
       CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YWORK,ZCSOILPOOLS(:,JL),IRESP,YCOMMENT)    
    ENDDO
ENDIF
!
! * Turn-over times for Soil carbon stocks per pools
!
YRECFM='tSoilPools'
YCOMMENT='turnover rate of each model soil carbon pool (s-1)'
IF (LALLOW_ADD_DIM)  THEN
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTSOILPOOL(:,:),IRESP,YCOMMENT,HNAM_DIM=YSOIL_CARBON_POOL_DIM_NAME)
ELSE
    DO JL=1,INCARB
        WRITE(YLVL,'(I4)') JL
        YWORK=YRECFM(:LEN_TRIM(YRECFM))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YWORK,ID%DM%XTSOILPOOL(:,JL),IRESP,YCOMMENT)    
    ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
! * Transfer of carbon from biomass to litter 
!-------------------------------------------------------------------------------
!
YRECFM='fVegLitter'
YCOMMENT='total Carbon Mass Flux from Vegetation to Litter (kgC m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XTURNVTOT(:),IRESP,YCOMMENT)    
!
!-------------------------------------------------------------------------------
! * Transfer of carbon from litter to soil
!-------------------------------------------------------------------------------
!
YRECFM='fLitterSoil'
YCOMMENT='total Carbon Mass Flux from Litter to Soil (kgC m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFLTOSCARB(:),IRESP,YCOMMENT)    
!
!-------------------------------------------------------------------------------
! * Riverine doc Flux
!-------------------------------------------------------------------------------
!
IF(IO%LCLEACH)THEN
  YRECFM='fCLeach'
  YCOMMENT='leaching carbon runoff from soil to river (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFDOC(:),IRESP,YCOMMENT)    
ENDIF
!
!-------------------------------------------------------------------------------
! * Fire disturbance
!-------------------------------------------------------------------------------
!
!
IF(IO%LFIRE)THEN
!
! * Fire Fraction
!
  YRECFM='burntFracAll'
  YCOMMENT='natural and anthropogenic burnt area fraction (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XFIREFRA(:),IRESP,YCOMMENT)    
!
! * Natural Fire Fraction
!
  YRECFM='burntArea'
  YCOMMENT='natural burnt area fraction (%)'
  ZWORK(:) = ID%DM%XFIREFRA(:)*100.
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,YCOMMENT)    
!
! * Fire Carbon emissions
!
  YRECFM='fFire'
  YCOMMENT='Carbon Mass Flux into Atmosphere due to CO2 Emission from all Fire (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFIRECO2(:),IRESP,YCOMMENT)    
!
! * Fire Carbon emissions
!
  YRECFM='fFireNat'
  YCOMMENT='Carbon Mass Flux into Atmosphere due to CO2 Emission from natural Fire (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFIRECO2(:),IRESP,YCOMMENT)    
! 
! * Fire Black Carbon emissions
!
  YRECFM='fFireBCarb'
  YCOMMENT='Black Carbon Flux into Atmosphere due to Emission from natural Fire (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFIREBCS(:),IRESP,YCOMMENT)    
!
! * Biomass to litter transfer due to fire
!
  YRECFM='fireVegToLit'
  YCOMMENT='biomass transfered to litter due to fire (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFIRETURNOVER(:),IRESP,YCOMMENT)    
!
! * Fire index
!
  YRECFM='fireIndex'
  YCOMMENT='fire index (-)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XFIREIND(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Litter governing fire occurence
!
  YRECFM='fireMoistLit'
  YCOMMENT='litter moisture content governing fire occurence (m3/m3)' 
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XMOISTLIT_FIRE(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!-------------------------------------------------------------------------------
! * Land-use Land Cover Change 
!   (allready account for pacth fraction except fRegrowth)
!-------------------------------------------------------------------------------
!
IF (IO%LLULCC) THEN
!
! * Net Carbon flux toward the atmosphere due to land-use
!
  YRECFM='fLuc'
  YCOMMENT='Net Carbon Mass Flux into Atmosphere due to Land Use Change (kgC m-2 s-1)'
  ZFLUC(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZFLUC(IMASK)=ZFLUC(IMASK)+PEK%XFLUATM(JI)+PEK%XFANTATM(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZFLUC(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux toward the atmosphere due to land-use clearance
!
  YRECFM='fDeforest'
  YCOMMENT='Deforested biomass as a result of anthropogenic land use change, including slash (kgC m-2 s-1)'  
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PEK%XFLUATM(JI)+PEK%XFLURES(JI)+PEK%XFLUANT(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux toward the atmosphere due to land-use clearance
!
  YRECFM='fDeforestToA'
  YCOMMENT='carbon mass flux into atmosphere due to any human activity (kgC m-2 s-1)'
  ZFLUATM(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZFLUATM(IMASK)=ZFLUATM(IMASK)+PEK%XFLUATM(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux toward the anthropogenic carbon pool due to land-use clearance
!
  YRECFM='fDeforestToP'
  YCOMMENT='Deforested biomass into product pool as a result of anthropogenic land use change (kgC m-2 s-1)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PEK%XFLUANT(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux toward the litter carbon pool due to land-use clearance
  YRECFM='fDeforestToL'
  YCOMMENT='Biomass carbon transferred to litter pools due to lulucf processes (kgC m-2 s-1)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PEK%XFLURES(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux from the anthropogenic carbon pool to the atmosphere
!
  YRECFM='fProdDecomp'
  YCOMMENT='decomposition out of product pools to CO2 in atmosphere (kgC m-2 s-1)'
  ZWORK(:)=0.0
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK)=ZWORK(IMASK)+PEK%XFANTATM(JI)
     ENDDO
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Carbon flux to the atmosphere due to crop harvesting
!
  YRECFM='fHarvest'
  YCOMMENT='Carbon flux into atmosphere due to crop harvesting (kgC m-2 s-1)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XFHARVEST(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!-------------------------------------------------------------------------------
! * Biome Carbon flux
!-------------------------------------------------------------------------------
!
! * Net land to Atmosphere Carbon Flux
!
YRECFM='nbp'
YCOMMENT='surface net downward mass flux of carbon due to all land processes (kgC m-2 s-1)'
ZWORK(:) = (ID%DE%XGPP(:) - ID%DE%XRESP_ECO(:))*XMC/XMCO2
IF(IO%LFIRE  ) ZWORK(:) = ZWORK(:) - ID%DE%XFIRECO2(:)
IF(IO%LCLEACH) ZWORK(:) = ZWORK(:) - ID%DE%XFDOC(:)
IF(IO%LLULCC ) ZWORK(:) = ZWORK(:) - ZFLUC(:) - ID%DE%XFHARVEST(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Net Ecosystem Carbon Balance
!
YRECFM='necb'
YCOMMENT='net rate of carbon accumulation (or loss) (kgC m-2 s-1)'
ZWORK(:) = 0.0
ZWORK(:) = (ID%DE%XGPP(:) - ID%DE%XRESP_ECO(:))*XMC/XMCO2
IF(IO%LFIRE  ) ZWORK(:) = ZWORK(:) - ID%DE%XFIRECO2(:)
IF(IO%LCLEACH) ZWORK(:) = ZWORK(:) - ID%DE%XFDOC(:)
IF(IO%LLULCC ) ZWORK(:) = ZWORK(:) - ZFLUATM(:) - ID%DE%XFHARVEST(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_CARBON_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_MIP_CARBON_ISBA_n
