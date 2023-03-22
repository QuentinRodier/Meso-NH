!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_WATER_ISBA_n (DTCO, DUO, U, ID, IO, S, K, NP, NPE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_WATER_ISBA*
!!
!!    PURPOSE
!!    -------
!!
!!    Writes the ISBA diagnostic fields relative to water cycle as specified by cmip
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
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM,          &
                      YPATCHES_DIM_NAME,       &
                      YSNOW_LAYER_DIM_NAME,    &
                      YGROUND_LAYER_DIM_NAME
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,       ONLY : XRHOLW
!
USE MODN_SFX_OASIS,  ONLY : XTSTEP_CPL_LAND
USE MODD_SFX_OASIS,  ONLY : LCPL_LAND, LCPL_FLOOD, NTWS_ID
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_UNPACK_SAME_RANK
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
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
CHARACTER(LEN=4)  :: YLVL
!
REAL, DIMENSION(U%NSIZE_NATURE)                 :: ZWORK
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH)       :: ZWORK_PATCH
!
REAL, DIMENSION(U%NSIZE_NATURE,NPE%AL(1)%TSNOW%NLAYER)  :: ZSNLIQ
REAL, DIMENSION(U%NSIZE_NATURE,NPE%AL(1)%TSNOW%NLAYER)  :: ZSWE
REAL, DIMENSION(U%NSIZE_NATURE,NPE%AL(1)%TSNOW%NLAYER)  :: ZSND
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZWGT
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZWGIT
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZMOIST
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZFROZEN
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZLIQUID
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZAVG_WG
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZAVG_WGI
REAL, DIMENSION(U%NSIZE_NATURE,IO%NGROUND_LAYER) :: ZSUMPATCH

INTEGER           :: JI, JL, JP, INL, INLS, INP, JDAY, IMASK
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_WATER_ISBA_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
INL  = IO%NGROUND_LAYER
INLS = NPE%AL(1)%TSNOW%NLAYER
INP  = IO%NPATCH
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
ZWORK(:) = XUNDEF
!
ZWORK_PATCH(:,:) = XUNDEF
!
ZSNLIQ (:,:) = XUNDEF
ZSWE   (:,:) = XUNDEF
ZSND   (:,:) = XUNDEF
ZWGT   (:,:) = XUNDEF
ZWGIT  (:,:) = XUNDEF
ZMOIST (:,:) = XUNDEF
ZFROZEN(:,:) = XUNDEF
ZLIQUID(:,:) = XUNDEF
!
ZAVG_WG   (:,:) = 0.0
ZAVG_WGI  (:,:) = 0.0
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Precipitation fluxes
!-------------------------------------------------------------------------------
!
! * Only if global field missing
!
!
YRECFM='pr_land'
YCOMMENT='Precipitation rate over land (kg m-2 s-1)'
ZWORK(:) = ID%DE%XRAINFALL(:)+ID%DE%XSNOWFALL(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='prra_land'
YCOMMENT='rainfall rate over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XRAINFALL(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='prsn_land'
YCOMMENT='snowfall rate over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XSNOWFALL(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Precipitation onto canopy
!
YRECFM='prveg'
YCOMMENT='Precipitation onto canopy over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XRRVEG(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Total Evaporation fluxes
!-------------------------------------------------------------------------------
!
YRECFM='potet'
YCOMMENT='Potential Evapotranspiration over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XEPOT(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='et'
YCOMMENT='total evapotranspiration over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tran'
YCOMMENT='plant transpiration over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLETR(JI)/PK%XLVTT(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='evspsblveg'
YCOMMENT='water evaporation from Canopy including sublimation over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLER(JI)/PK%XLVTT(JI)
   ENDDO
ENDDO
IF (ISIZE_LMEB_PATCH>0) THEN
   DO JP=1,INP
      PK => NP%AL(JP)
      DEK => ID%NDE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLES_CV(JI)/PK%XLSTT(JI)
      ENDDO
   ENDDO
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='evspsblsoi'
YCOMMENT='water evaporation from soil including sublimation over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * (DEK%XLEG(JI)/PK%XLVTT(JI)+DEK%XLEGI(JI)/PK%XLSTT(JI))
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (IO%LFLOOD) THEN
   YRECFM='evspsblfld'
   YCOMMENT='water evaporation from floodplains including sublimation over land (kg m-2 s-1)'
   ZWORK(:)=0.0
   DO JP=1,INP
      PK => NP%AL(JP)
      DEK => ID%NDE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * (DEK%XLE_FLOOD(JI)/PK%XLVTT(JI)+DEK%XLEI_FLOOD(JI)/PK%XLSTT(JI))
      ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
! * Sublimation fluxes
!-------------------------------------------------------------------------------
!
YRECFM='sbl_land'
YCOMMENT='surface snow and ice sublimation flux over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DK => ID%ND%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DK%XSUBL(JI)/PK%XLSTT(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sblsn'
YCOMMENT='sublimation from snow area over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLES(JI)/PK%XLSTT(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sblnosn'
YCOMMENT='sublimation from snow-free area over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLEGI(JI)/PK%XLSTT(JI)
   ENDDO
ENDDO
IF (IO%LFLOOD) THEN
   DO JP=1,INP
      PK => NP%AL(JP)
      DEK => ID%NDE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLEI_FLOOD(JI)/PK%XLSTT(JI)
      ENDDO
   ENDDO
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Liquid Evaporation fluxes
!-------------------------------------------------------------------------------
!
YRECFM='ec'
YCOMMENT='liquid water evaporation from Canopy over land (kg m-2 s-1)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLER(JI)/PK%XLVTT(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='es'
YCOMMENT='liquid water evaporation from soil over land (kg m-2 s-1)'
ZWORK(:)=ID%DE%XLEG(:)
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   DEK => ID%NDE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLEG(JI)/PK%XLVTT(JI)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='eow'
YCOMMENT='liquid water evaporation from open water over land (kg m-2 s-1)'
ZWORK(:)=0.0
IF (IO%LFLOOD) THEN
   DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLE_FLOOD(JI)/PK%XLVTT(JI)
     ENDDO
   ENDDO
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   YRECFM='esn'
   YCOMMENT='liquid water evaporation from snow over land (kg m-2 s-1)'
   DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLESL(JI)/PK%XLVTT(JI)
     ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
! * MEB Evaporation fluxes
!-------------------------------------------------------------------------------
!
IF (ISIZE_LMEB_PATCH>0) THEN
   !
   YRECFM='sblsnc'
   YCOMMENT='snow sublimation from vegetation canopy over land (kg m-2 s-1)'
   DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLES_CV(JI)/PK%XLSTT(JI)
     ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='tranc'
   YCOMMENT='transpiration from overstory canopy vegetation over land (kg m-2 s-1)'
   DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLETR_CV(JI)/PK%XLVTT(JI)
     ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='ecc'
   YCOMMENT='interception evaporation from overstory canopy vegetation over land (kg m-2 s-1)'
   DO JP=1,INP
     PK => NP%AL(JP)
     DEK => ID%NDE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLER_CV(JI)/PK%XLVTT(JI)
     ENDDO
   ENDDO
   !
   IF (IO%LMEB_LITTER) THEN
      !
      YRECFM='elit'
      YCOMMENT='litter evaporation over land (kg m-2 s-1)'
      DO JP=1,INP
         PK => NP%AL(JP)
         DEK => ID%NDE%AL(JP)
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLELITTER(JI)/PK%XLVTT(JI)
         ENDDO
      ENDDO
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='sbllit'
      YCOMMENT='litter sublimation over land (kg m-2 s-1)'
      DO JP=1,INP
         PK => NP%AL(JP)
         DEK => ID%NDE%AL(JP)
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * DEK%XLELITTERI(JI)/PK%XLSTT(JI)
         ENDDO
      ENDDO
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
      !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
! * Runoff fluxes
!-------------------------------------------------------------------------------
!
! * total runoff
!
YRECFM='mrro'
YCOMMENT='total runoff over land (kg m-2 s-1)'
ZWORK(:)=ID%DE%XRUNOFF(:)+ID%DE%XDRAIN(:)
IF(IO%LGLACIER)THEN
  ZWORK(:)=ZWORK(:)+ID%DE%XICEFLUX(:)
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * surface runoff
!
YRECFM='mrros'
YCOMMENT='surface runoff over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XRUNOFF(:),IRESP,HCOMMENT=YCOMMENT)
!
! * drainage
!
YRECFM='mrrob'
YCOMMENT='subsurface runoff over land (kg m-2 s-1)'
ZWORK(:)=ID%DE%XDRAIN(:)
IF(IO%LGLACIER)THEN
  ZWORK(:)=ZWORK(:)+ID%DE%XICEFLUX(:)
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * calving
!
IF (IO%LGLACIER) THEN
   YRECFM='iceberg'
   YCOMMENT='iceberg calving flux over land (kg m-2 s-1)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XICEFLUX(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
! * floodplain flood
!
IF (IO%LFLOOD) THEN
   !
   YRECFM='pfld'
   YCOMMENT='floodplains precipitation interception over land (kg m-2 s-1)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XPFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='ifld'
   YCOMMENT='floodplains infiltration into the soil over land (kg m-2 s-1)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XIFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
!
!-------------------------------------------------------------------------------
! * Snow melt and refreezing
!-------------------------------------------------------------------------------
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   !
   YRECFM='snmtot'
   YCOMMENT='snowmelt over the entire snowpack over land (kg m-2 s-1)'
   ZWORK(:)=ID%DE%XMELTSTOT(:)
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XMELTSTOT(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='snrefr'
   YCOMMENT='refreezing of water in the snowpack over land (kg m-2 s-1)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XSNREFREEZ(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
!
YRECFM='snm'
YCOMMENT='snowmelt over land (kg m-2 s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XMELT(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Change in water reservoirs
!-------------------------------------------------------------------------------
!
YRECFM='dslw'
YCOMMENT='change in soil moisture over land (kg m-2)'
ZWORK(:)=ID%DE%XDWG(:)+ID%DE%XDWGI(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dsn'
YCOMMENT='change in snow water equivalent over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XDSWE(:),IRESP,HCOMMENT=YCOMMENT)
!        
YRECFM='dcw'
YCOMMENT='change in interception storage over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XDWR(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dsw'
YCOMMENT='change in surface water storage over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XDSWFREE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='watbud_isba'
YCOMMENT='land water budget as residue over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DE%XWATBUD(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Land water reservoirs
!-------------------------------------------------------------------------------
!
! * vegetation reservoir
!
YRECFM='cw'
YCOMMENT='canopy water amount over land (kg m-2)'
ZWORK(:)=ID%DM%XWR
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XWR(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (ISIZE_LMEB_PATCH>0) THEN
   YRECFM='snwc'
   YCOMMENT='mass of snow intercepted by vegetation (kg m-2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XWRVN(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
! * Litter reservoir
!
IF (ISIZE_LMEB_PATCH>0.AND.IO%LMEB_LITTER) THEN
!
   YRECFM='litw'
   YCOMMENT='Litter Liquid Water Content over land (kg m-2)'
   ZWORK(:)=0.0
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XWRL(JI)
      ENDDO
   ENDDO 
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='litwi'
  YCOMMENT='Litter Solid Water Content over land (kg m-2)'
  ZWORK(:)=0.0
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*PEK%XWRLI(JI)
      ENDDO
   ENDDO
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
! * top soil reservoir
!
YRECFM='mrsos'
IF(IO%CISBA=='DIF') THEN
  YCOMMENT='top soil moisture over 10cm over land (kg m-2)'
ELSE
  YCOMMENT='surface soil moisture over 1cm over land (kg m-2)'
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XSURF_TWG(:),IRESP,HCOMMENT=YCOMMENT)
!
! * root soil reservoir
!
YRECFM='rzwc'
YCOMMENT='root zone soil moisture over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XROOT_TWG(:),IRESP,HCOMMENT=YCOMMENT)
!
! * total soil reservoir
!
YRECFM='mrso'
YCOMMENT='total soil moisture over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XSOIL_TWG(:),IRESP,HCOMMENT=YCOMMENT)
!
! * liquid soil reservoir
!
YRECFM='mrlso'
YCOMMENT='Soil Liquid Water Content over land (kg m-2)'
ZWORK(:)=ID%DM%XSOIL_TWG(:)-ID%DM%XSOIL_TWGI(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Soil Frozen Water Content 
!
YRECFM='mrfso'
YCOMMENT='Soil Frozen Water Content over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XSOIL_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
!
! * floodplains reservoir
!
IF (LCPL_FLOOD.AND.IO%LFLOOD) THEN
   YRECFM='sw'
   YCOMMENT='surface water storage over land (kg m-2)'
   ZWORK(:)=K%XPIFLOOD(:)*XTSTEP_CPL_LAND
   ZWORK(:)=MAX(0.0,ZWORK(:)+S%XCPL_PFLOOD(:)-S%XCPL_IFLOOD(:)-S%XCPL_EFLOOD(:))
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
! * terrestrial water storage
!
YRECFM='tws'
YCOMMENT='terrestrial water storage over land (kg m-2)'
ZWORK(:)=ID%DM%XWR(:)+ID%DM%XTWSNOW(:)+ID%DM%XSOIL_TWG(:)
IF (LCPL_LAND.AND.NTWS_ID/=NUNDEF) THEN
  ZWORK(:)=ZWORK(:)+S%XCPL_TWS(:)
ENDIF
IF (ISIZE_LMEB_PATCH>0) THEN
   ZWORK(:)=ZWORK(:)+ID%DM%XWRVN
   IF(IO%LMEB_LITTER)THEN  
     DO JP=1,INP
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZWORK(IMASK)=ZWORK(IMASK)+PK%XPATCH(JI)*(PEK%XWRL(JI)+PEK%XWRLI(JI))
        ENDDO
     ENDDO 
   ENDIF
ENDIF
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Soil moisture profiles
!-------------------------------------------------------------------------------
!
! * Liquid and solid moisture profile (kg m-2)
!
ZWGT (:,:) = 0.0
ZWGIT(:,:) = 0.0
ZSUMPATCH(:,:) = 0.0
!   
IF (IO%CISBA=='DIF') THEN
   !
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            IF(JL<=PK%NWG_LAYER(JI))THEN
              ZSUMPATCH(IMASK,JL) = ZSUMPATCH(IMASK,JL) +  PK%XPATCH(JI)
              ZWGIT    (IMASK,JL) = ZWGIT    (IMASK,JL) +  PK%XPATCH(JI)*PEK%XWGI(JI,JL)*PK%XDZG(JI,JL)*XRHOLW
              ZWGT     (IMASK,JL) = ZWGT     (IMASK,JL) +  PK%XPATCH(JI)*PEK%XWG (JI,JL)*PK%XDZG(JI,JL)*XRHOLW
              ZAVG_WG  (IMASK,JL) = ZAVG_WG  (IMASK,JL) +  PK%XPATCH(JI)*PEK%XWG (JI,JL)
              ZAVG_WGI (IMASK,JL) = ZAVG_WGI (IMASK,JL) +  PK%XPATCH(JI)*PEK%XWGI(JI,JL)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   !
   WHERE (ZSUMPATCH(:,:)/=0.0)
     ZAVG_WG (:,:) = ZAVG_WG (:,:)/ZSUMPATCH(:,:)
     ZAVG_WGI(:,:) = ZAVG_WGI(:,:)/ZSUMPATCH(:,:)
   ELSEWHERE
     ZAVG_WG (:,:) = XUNDEF
     ZAVG_WGI(:,:) = XUNDEF
   ENDWHERE
   !  
ELSE
   !
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)   
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         !
         ZWGIT(IMASK,1) = ZWGIT(IMASK,1) + PK%XPATCH(JI)*PEK%XWGI(JI,1)*ZWORK(JI)*PK%XDG(JI,1)*XRHOLW
         ZWGT (IMASK,1) = ZWGT (IMASK,1) + PK%XPATCH(JI)*PEK%XWG (JI,1)*ZWORK(JI)*PK%XDG(JI,1)*XRHOLW
         !
         ZWGIT(IMASK,2) = ZWGIT(IMASK,2) + PK%XPATCH(JI)*PEK%XWGI(JI,2)*ZWORK(JI)*PK%XDG(JI,2)*XRHOLW
         ZWGT (IMASK,2) = ZWGT (IMASK,2) + PK%XPATCH(JI)*PEK%XWG (JI,2)*ZWORK(JI)*PK%XDG(JI,2)*XRHOLW
         !
         IF(IO%CISBA=='3-L')THEN
           ZWORK(IMASK)   = MAX(0.0,PK%XDG(JI,3)-PK%XDG(JI,2))
           ZWGT (IMASK,3) = ZWGT(IMASK,3) + PK%XPATCH(JI)*PEK%XWG(JI,3)*ZWORK(IMASK)*XRHOLW
           ZWGIT(IMASK,3) = 0.0
         ENDIF
         !
      ENDDO
   ENDDO
   !
   !For isba force-restore, the first layer mass are removed
   !from the second layer to conserv total soil mass
   ZWGIT(:,2) = ZWGIT(:,2) - ZWGIT(:,1)
   ZWGT (:,2) = ZWGT (:,2) - ZWGT (:,1)
   !
ENDIF
!
! * Layer moisture profile (kg m-2)
!
ZMOIST(:,:) = ZWGT(:,:) + ZWGIT(:,:)
!
! * Average layer fraction of frozen moisture (-)
!
WHERE(ZMOIST (:,:)>0.0)
      ZFROZEN(:,:) = ZWGIT(:,:) / ZMOIST(:,:)
      ZLIQUID(:,:) = ZWGT (:,:) / ZMOIST(:,:)
ELSEWHERE
      ZFROZEN(:,:) = XUNDEF
      ZLIQUID(:,:) = XUNDEF
ENDWHERE
!
! * Write fields
!
IF(LALLOW_ADD_DIM)THEN 
  !
  YRECFM='mrsol'
  YCOMMENT='Total water content of soil layer over land (kg m-2)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZMOIST(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='mrsll' 
  YCOMMENT='Liquid water content of soil layer over land (kg m-2)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGT(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='mrsfl' 
  YCOMMENT='Frozen water content of soil layer over land (kg m-2)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGIT(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='wg' 
  YCOMMENT='Liquid water content of soil layer over land (m3 m-m)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZAVG_WG(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='wgi' 
  YCOMMENT='Frozen water content of soil layer over land (m3 m-m)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZAVG_WGI(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='mrlqso' 
  YCOMMENT='average layer fraction of liquid moisture over land (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLIQUID(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  YRECFM='mrfsofr' 
  YCOMMENT='average layer fraction of frozen moisture over land (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZFROZEN(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     !
     WRITE(YNUM,'(I2)') JP
     YRECFM='wg_patch'//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
     YCOMMENT='soil liquid water content by pacth over land (m3 m-3)'
     ZWGT(:,:)=XUNDEF
     CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XWG(:,:),ZWGT(:,:),XUNDEF)
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGT(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)    
     !
     YRECFM='wgi_patch'//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
     YCOMMENT='soil solid water content by pacth over land (m3 m-3)'
     ZWGT(:,:)=XUNDEF
     CALL UNPACK_SAME_RANK(PK%NR_P,PEK%XWGI(:,:),ZWGT(:,:),XUNDEF)
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGT(:,:),IRESP,YCOMMENT,HNAM_DIM=YGROUND_LAYER_DIM_NAME)
     !
  ENDDO
  !
ELSE
  !
  DO JL=1,INL
     !
     WRITE(YLVL,'(I4)') JL
     YRECFM='mrsol'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Total water content of soil layer over land (kg m-2)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZMOIST(:,JL),IRESP,HCOMMENT=YCOMMENT)
     !
     WRITE(YLVL,'(I4)') JL
     YRECFM='mrsll'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Liquid water content of soil layer over land (kg m-2)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGT(:,JL),IRESP,HCOMMENT=YCOMMENT)
     !
     WRITE(YLVL,'(I4)') JL
     YRECFM='mrsfl'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Frozen water content of soil layer over land (kg m-2)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWGIT(:,JL),IRESP,HCOMMENT=YCOMMENT)
     !
     WRITE(YLVL,'(I4)') JL
     YRECFM='mrlqso'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='average layer fraction of liquid moisture over land (-)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZLIQUID(:,JL),IRESP,HCOMMENT=YCOMMENT)
     !
     WRITE(YLVL,'(I4)') JL
     YRECFM='mrfsofr'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='average layer fraction of frozen moisture over land (-)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZFROZEN(:,JL),IRESP,HCOMMENT=YCOMMENT)
     !
  END DO
  !
ENDIF
!
!-------------------------------------------------------------------------------
! * Snow reservoir
!-------------------------------------------------------------------------------
!
YRECFM='snw'
YCOMMENT='total snow water equivalent over land (kg m-2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTWSNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
IF(LALLOW_ADD_DIM)THEN 
  YRECFM='swe_patch' 
  YCOMMENT='snow water equivalent by pacth over land (kg m-2)'
  DO JP=1,INP
     PK => NP%AL(JP)
     DMK => ID%NDM%AL(JP)
     CALL UNPACK_SAME_RANK(PK%NR_P,DMK%XTWSNOW(:),ZWORK_PATCH(:,JP),XUNDEF)
  ENDDO
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK_PATCH(:,:),IRESP,YCOMMENT,HNAM_DIM=YPATCHES_DIM_NAME)
ENDIF
!
YRECFM='snd'
YCOMMENT='surface snow thickness over land (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTDSNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   YRECFM='lwsnl'
   YCOMMENT='total liquid water in snow pack over land (kg m-2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTSNOWLIQ(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
! * Snow profiles
!-------------------------------------------------------------------------------
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
   !
   ZSNLIQ(:,:) = 0.0
   ZSWE  (:,:) = 0.0
   ZSND  (:,:) = 0.0
   !   
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DMK => ID%NDM%AL(JP)
      DO JL=1,INLS
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            IF(PEK%TSNOW%WSNOW(JI,JL)>0.0)THEN
              ZSNLIQ(IMASK,JL) = ZSNLIQ(IMASK,JL) + PK%XPATCH(JI) * DMK%XSNOWLIQ(JI,JL) * XRHOLW
              ZSWE  (IMASK,JL) = ZSWE  (IMASK,JL) + PK%XPATCH(JI) * PEK%TSNOW%WSNOW(JI,JL)
              ZSND  (IMASK,JL) = ZSND  (IMASK,JL) + PK%XPATCH(JI) * PEK%TSNOW%WSNOW(JI,JL) / PEK%TSNOW%RHO  (JI,JL)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   !
   IF (LALLOW_ADD_DIM)  THEN 
      !
      YRECFM='snwl' 
      YCOMMENT='snow water equivalent by layer over land (kg m-2)'
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSWE(:,:),IRESP,YCOMMENT,HNAM_DIM=YSNOW_LAYER_DIM_NAME)    
      !
      YRECFM='sndl' 
      YCOMMENT='snow depth by layer over land (m)'
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSND(:,:),IRESP,YCOMMENT,HNAM_DIM=YSNOW_LAYER_DIM_NAME)    
      !
      YRECFM='lwsnll' 
      YCOMMENT='liquid water in snow by layer over land (kg m-2)'
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSNLIQ(:,:),IRESP,YCOMMENT,HNAM_DIM=YSNOW_LAYER_DIM_NAME)    
      !
   ELSE
      !
      DO JL=1,INLS
         !
         WRITE(YLVL,'(I4)') JL
         !
         YRECFM='snwl'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
         YCOMMENT='snow water equivalent by layer over land (kg m-2)'
         CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSWE(:,JL),IRESP,HCOMMENT=YCOMMENT)
         !
         YRECFM='sndl'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
         YCOMMENT='snow depth by layer over land (m)'
         CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSND(:,JL),IRESP,HCOMMENT=YCOMMENT)
         !
         YRECFM='lwsnll'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
         YCOMMENT='liquid water in snow layer over land (kg m-2)'
         CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZSNLIQ(:,JL),IRESP,HCOMMENT=YCOMMENT)
         !
      ENDDO
      !
   ENDIF
   !
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_WATER_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_DIAG_MIP_WATER_ISBA_n
