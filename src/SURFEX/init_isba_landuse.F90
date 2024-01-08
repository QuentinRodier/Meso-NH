!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_INIT_ISBA_LANDUSE

INTERFACE  
  
!#############################################################
SUBROUTINE INIT_ISBA_LANDUSE (IG, IO, S, K, NK, NP, NPE, DTI, HPROGRAM, KI)  
!#############################################################
!
!!****  *INIT_ISBA_LANDUSE* - routine to initialize land use for ISBA field
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!      Completelly reframed 08/2016 R. Séférian
!!      R. Séférian 10/2016 correct error in landuse computation fields
!!      R. Séférian 11/2016 : add cmip6 diagnostics
!!      J. Colin    12/2017 : add computations in case the water or snow is
!!                            nudged seperately on each patch
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n,     ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_K_t, ISBA_NK_t, &
                                ISBA_NP_t, ISBA_NPE_t
USE MODD_DATA_ISBA_n,    ONLY : DATA_ISBA_t
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(GRID_t),          INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_K_t),        INTENT(INOUT) :: K
TYPE(ISBA_NK_t),       INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),      INTENT(INOUT) :: NPE
TYPE(DATA_ISBA_t),     INTENT(INOUT) :: DTI
!
CHARACTER(LEN=6),                 INTENT(IN)    :: HPROGRAM          ! program calling surf. schemes
INTEGER,                          INTENT(IN)    :: KI
!
END SUBROUTINE INIT_ISBA_LANDUSE

END INTERFACE

END MODULE MODI_INIT_ISBA_LANDUSE
!#############################################################
SUBROUTINE INIT_ISBA_LANDUSE (IG, IO, S, K, NK, NP, NPE, DTI, HPROGRAM, KI)  
!#############################################################
!
!!****  *INIT_ISBA_LANDUSE* - routine to initialize land use for ISBA field
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!      Completelly reframed 08/2016 R. Séférian
!!      R. Séférian 10/2016 correct error in landuse computation fields
!!      R. Séférian 11/2016 : add cmip6 diagnostics
!!      J. Colin    12/2017 : add computations in case the water or snow is
!!                            nudged seperately on each patch
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n,     ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t, ISBA_NK_t, &
                                ISBA_NP_t, ISBA_NPE_t
USE MODD_DATA_ISBA_n,    ONLY : DATA_ISBA_t
!
USE MODD_INIT_LANDUSE
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF, XSURF_EPSILON, LEN_HREC
!
USE MODD_CSTS,           ONLY : XDAY
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODD_ASSIM,          ONLY : LASSIM, CASSIM_ISBA, NIE
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT,XCA_NIT,XCC_NIT,XPCCO2,XGTOKG,XKGTOG
!
USE MODI_ABOR1_SFX
!
USE MODI_SURF_PATCH
!
USE MODI_READ_SURF
USE MODE_READ_SURF_LAYERS
!
USE MODI_GET_LUOUT
!
USE MODI_LANDUSE_HYDRO 
USE MODI_LANDUSE_BIOMASS
USE MODI_LANDUSE_CARBON
USE MODI_LANDUSE_CARBON_DIF
USE MODI_LANDUSE_CARBON_MANAGING
USE MODI_LANDUSE_HYDRO_NUDGING 
!
USE MODI_ATTRIBUTE_CLOSEST_VEGTYPE
USE MODI_ATTRIBUTE_CLOSEST_VEGTYPE_NUDGING
!
!
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(GRID_t),          INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_K_t),        INTENT(INOUT) :: K
TYPE(ISBA_NK_t),       INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),      INTENT(INOUT) :: NPE
TYPE(DATA_ISBA_t),     INTENT(INOUT) :: DTI
!
CHARACTER(LEN=6),                 INTENT(IN)    :: HPROGRAM          ! program calling surf. schemes
INTEGER,                          INTENT(IN)    :: KI
!
!
!*       0.2   Declarations of local arguments on Patch grid
!
TYPE(LULCC_NP_t) :: TLU
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK, PNEAR
TYPE(LULCC_P_t), POINTER :: OLD
!
!*       0.3   Declarations of local arguments on complete ISBA grid
!
REAL, DIMENSION(KI,                 IO%NPATCH) :: ZPATCH_OLD      ! previous year PATCHES (ISBA grid)
REAL, DIMENSION(KI,NVEGTYPE,        IO%NPATCH) :: ZVEG_PATCH_OLD  ! previous year fraction of vegtype by patch
REAL, DIMENSION(KI,IO%NGROUND_LAYER,IO%NPATCH) :: ZDG_OLD         ! previous year soil layer depth
REAL, DIMENSION(KI,                 IO%NPATCH) :: ZWORK           ! work array (ISBA grid)
!
! To compute budget
REAL, DIMENSION(KI)                            :: ZBUDGET,ZBIO_GRID_OLD,ZBIO_GRID_NEW,ZLULCC_HARVEST_GRID 
REAL, DIMENSION(KI)                            :: ZLITTER_GRID_OLD,ZCSOIL_GRID_OLD,ZLITTER_GRID_NEW,ZCSOIL_GRID_NEW
!
CHARACTER(LEN=LEN_HREC)                        :: YRECFM            ! Name of the article to be read
CHARACTER(LEN=4)                               :: YLVL
INTEGER                                        :: IRESP             ! Error code after redding
!
LOGICAL                                        :: GLULU             ! Logical to perform luluccf computation of not
!
LOGICAL :: GDIM, GSTOP
!
INTEGER :: ILUOUT                                  ! unit of output listing file
INTEGER :: JI, JJ, JL, JNL, JNLS, JNC, JP, JP_NEAR ! loop counter
INTEGER :: INP, INL, INS, INB, INLIT, INLITS, INC  ! dimension
INTEGER :: JT, INTIME                              ! loop on time (nudging) and size
INTEGER :: IMASK, ISIZE_LMEB_PATCH                 ! Work integer
REAL    :: ZEPSILON, ZNDAYS, ZCC_CA, ZINVCA         ! Work real
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
IF(TRIM(CASSIM_ISBA)=="ENKF".AND.(LASSIM.OR.NIE/=0)) THEN
   CALL ABOR1_SFX('ABORT: Assimilation procedure not implemented under Land-use change case')
ENDIF
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
INP   =IO%NPATCH
INL   =IO%NGROUND_LAYER
INS   =NPE%AL(1)%TSNOW%NLAYER
INB   =IO%NNBIOMASS
INLIT =IO%NNLITTER
INLITS=IO%NNLITTLEVS
INC   =IO%NNSOILCARB
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
!-------------------------------------------------------------------------------
!
!* initialize total co2 land use flux to atm
!
ALLOCATE(S%XFLUTOATM     (KI))
ALLOCATE(S%XFHARVESTTOATM(KI))
S%XFLUTOATM     (:)=0.0
S%XFHARVESTTOATM(:)=0.0
!
!-------------------------------------------------------------------------------
!
!* initialize water and carbon conservation
!
ALLOCATE(S%XWCONSRV(KI))
ALLOCATE(S%XCCONSRV(KI))
S%XWCONSRV(:) = 0.0
S%XCCONSRV(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!* local ISBA grid variables init
!
ZBIO_GRID_OLD      (:) = 0.0
ZBIO_GRID_NEW      (:) = 0.0
ZLULCC_HARVEST_GRID(:) = 0.0
!
ZLITTER_GRID_OLD(:) = 0.0
ZCSOIL_GRID_OLD (:) = 0.0
ZLITTER_GRID_NEW(:) = 0.0
ZCSOIL_GRID_NEW (:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       1. Get previous year variables
!        -------------------------------
!
! Note: this considers that vegetation distribution has been
! determined from compute_isba_parameters before
! This concerns VEGTYPE
!
ZPATCH_OLD    (:,:)   = 0.
ZVEG_PATCH_OLD(:,:,:) = 0.
!
ALLOCATE(S%XVEGTYPE_OLD(KI,NVEGTYPE))
DO JL=1,NVEGTYPE
   WRITE(YLVL,'(I4)') JL
   YRECFM='VEGTYPE'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
   CALL READ_SURF(HPROGRAM,YRECFM,S%XVEGTYPE_OLD(:,JL),IRESP)
ENDDO   
CALL SURF_PATCH(INP,S%XVEGTYPE_OLD,DTI%NPAR_VEG_IRR_USE,ZPATCH_OLD,ZVEG_PATCH_OLD)
!
!* Control check for troncature error (occurs with ascii)
!
IF (HPROGRAM=='ASCII'.OR.HPROGRAM=='TEXTE') THEN
  ZEPSILON=1.0E-8
ELSE
  ZEPSILON=XSURF_EPSILON
ENDIF
!
GLULU=ANY(ABS(S%XPATCH(:,:)-ZPATCH_OLD(:,:))>ZEPSILON)
!
!-------------------------------------------------------------------------------
!
IF(GLULU)THEN
!        
!-------------------------------------------------------------------------------
!
!* Initialyse previous year variables 
!
  CALL INIT_LULCC_NP(TLU,IO%NPATCH)  
!
! *  Read previous DG 
!
  CALL READ_SURF(HPROGRAM,'SPLIT_PATCH',GDIM,IRESP)
  CALL READ_SURF_LAYERS(HPROGRAM,'DG',GDIM,ZDG_OLD(:,:,:),IRESP)
!
!* Save previous year total land water content
!
  DO JP = 1,INP
!
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
!
     ALLOCATE(OLD%WG   (PK%NSIZE_P,INL))
     ALLOCATE(OLD%WGI  (PK%NSIZE_P,INL))
     ALLOCATE(OLD%WSN  (PK%NSIZE_P,INS))
     ALLOCATE(OLD%WR   (PK%NSIZE_P    ))
     OLD%DG (:,:) = XUNDEF
     OLD%WG (:,:) = PEK%XWG        (:,:) ! m3.m-3
     OLD%WGI(:,:) = PEK%XWGI       (:,:) ! m3.m-3
     OLD%WSN(:,:) = PEK%TSNOW%WSNOW(:,:) ! kg.m-2
     OLD%WR (:)   = PEK%XWR        (:)   ! kg.m-2
     IF (ISIZE_LMEB_PATCH>0) THEN
        OLD%WR(:)=OLD%WR(:)+PEK%XWRL(:)+PEK%XWRLI(:)+PEK%XWRVN(:)
     ENDIF
!
     ALLOCATE(OLD%PATCH(PK%NSIZE_P))
     CALL PACK_SAME_RANK(PK%NR_P,ZPATCH_OLD(:,JP),OLD%PATCH(:))
!
     ALLOCATE(OLD%DG(PK%NSIZE_P,INL))
     CALL PACK_SAME_RANK(PK%NR_P,ZDG_OLD(:,:,JP),OLD%DG(:,:))
!
     ALLOCATE(OLD%DZG(PK%NSIZE_P,INL))
     OLD%DZG(:,1)=OLD%DG(:,1)
     DO JL=2,INL
        OLD%DZG(:,JL)=OLD%DG(:,JL)-OLD%DG(:,JL-1)
     ENDDO   
!
  ENDDO
!
!     
!* Save previous year vegtype by patch
!
  IF(IO%CPHOTO=='NCB')THEN
!
    DO JP = 1,INP
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       OLD => TLU%AL(JP)
       ALLOCATE(OLD%VEGTYPE_PATCH(PK%NSIZE_P,NVEGTYPE))
       CALL PACK_SAME_RANK(PK%NR_P,ZVEG_PATCH_OLD(:,JP,:),OLD%VEGTYPE_PATCH(:,:))
    ENDDO
!
  ENDIF
!     
!* Save previous year biomass
!
  IF(IO%CPHOTO=='NIT'.OR.IO%CPHOTO=='NCB')THEN
!
    DO JP = 1,INP
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       OLD => TLU%AL(JP)
       ALLOCATE(OLD%LULCC_HARVEST(PK%NSIZE_P))        
       ALLOCATE(OLD%TURNOVER     (PK%NSIZE_P,INB))        
       ALLOCATE(OLD%BIOMASS      (PK%NSIZE_P,INB))        
       OLD%LULCC_HARVEST(:)   = 0.0
       OLD%TURNOVER     (:,:) = 0.0
       OLD%BIOMASS      (:,:) = PEK%XBIOMASS(:,:)
    ENDDO
!
    ZCC_CA=(XCC_NIT/EXP(XCA_NIT*LOG(10.)))  
    ZINVCA=(1.0/(1.0-XCA_NIT))
!
  ENDIF
!
!* Save previous year Leaf Area Index
!
  IF(IO%CPHOTO/='NON' .AND. IO%CPHOTO/='AGS' .AND. IO%CPHOTO/='AST') THEN
!
    DO JP = 1,INP
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       OLD => TLU%AL(JP)
       ALLOCATE(OLD%LAI(PK%NSIZE_P))        
       OLD%LAI(:) = PEK%XLAI(:) 
       WHERE(PK%XPATCH(:)==0)
             PEK%XLAI(:)=XUNDEF
       ENDWHERE
    ENDDO
!
  ENDIF
!
!* Save previous year carbon pools
!
  IF(IO%CRESPSL=='CNT')THEN
!
    DO JP = 1,INP
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       OLD => TLU%AL(JP)
       ALLOCATE(OLD%LIGNIN_STRUC(PK%NSIZE_P,INLITS))
       ALLOCATE(OLD%LITTER      (PK%NSIZE_P,INLIT,INLITS))
       ALLOCATE(OLD%SOILCARB    (PK%NSIZE_P,INC))
       OLD%LIGNIN_STRUC(:,:)   = PEK%XLIGNIN_STRUC(:,:)
       OLD%LITTER      (:,:,:) = PEK%XLITTER      (:,:,:)
       OLD%SOILCARB    (:,:)   = PEK%XSOILCARB    (:,:)
    ENDDO
!
  ELSEIF(IO%CRESPSL=='DIF')THEN
!
    DO JP = 1,INP
!
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       OLD => TLU%AL(JP)
!
       ALLOCATE(OLD%SURF_LITTER(PK%NSIZE_P,INLIT))
       ALLOCATE(OLD%SOIL_LIGNIN(PK%NSIZE_P,INL))
       ALLOCATE(OLD%SOIL_LITTER(PK%NSIZE_P,INL,INLIT))
       ALLOCATE(OLD%SOIL_CARBON(PK%NSIZE_P,INL,INC))
       OLD%SURF_LIGNIN(:)     = PEK%XSURFACE_LIGNIN_STRUC(:)
       OLD%SURF_LITTER(:,:)   = PEK%XSURFACE_LITTER(:,:)
       OLD%SOIL_LIGNIN(:,:)   = PEK%XSOILDIF_LIGNIN_STRUC(:,:)
       OLD%SOIL_LITTER(:,:,:) = PEK%XSOILDIF_LITTER(:,:,:)
       OLD%SOIL_CARBON(:,:,:) = PEK%XSOILDIF_CARB(:,:,:)
!
       ALLOCATE(OLD%WG_LAYER(PK%NSIZE_P))
       OLD%WG_LAYER(:) = NUNDEF
       DO JL=1,INL
          DO JI=1,PK%NSIZE_P
             IF(OLD%PATCH(JI)>0.0.AND.PEK%XWG(JI,JL)/=XUNDEF)THEN
                OLD%WG_LAYER(JI) = JL
             ENDIF
          ENDDO
       ENDDO
!
    ENDDO
!
  ENDIF
!
!*       2. Treat case when new PFTs is created due to LULCC
!        ---------------------------------------------------
!
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'!!!                                                        !!!'
  WRITE(ILUOUT,*)'!!!                    WARNING    WARNING                  !!!'
  WRITE(ILUOUT,*)'!!!                                                        !!!'
  WRITE(ILUOUT,*)'!!!              Patches Distribution has changed          !!!' 
  WRITE(ILUOUT,*)'!!!  Land-use Land Cover Change computation are performed  !!!'
  WRITE(ILUOUT,*)'!!!                                                        !!!'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!
  WRITE(*,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(*,*)'!!!                                                        !!!'
  WRITE(*,*)'!!!                    WARNING    WARNING                  !!!'
  WRITE(*,*)'!!!                                                        !!!'
  WRITE(*,*)'!!!              Patches Distribution has changed          !!!' 
  WRITE(*,*)'!!!  Land-use Land Cover Change computation are performed  !!!'
  WRITE(*,*)'!!!                                                        !!!'
  WRITE(*,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!
! Note: The algorithm ATTRIBUTE_CLOSEST_VEGTYPE is simple:
! It uses attribution rules for each kind of vegetation
! to find the closest patch matching when a new patch 
! is created in the grid cell
!-------------------------------------------------------------------------------
!
! * Ensures that solely created patches are treated
!
  DO JP=1,INP
    !
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    OLD => TLU%AL(JP)
    !
    DO JI=1,PK%NSIZE_P
      !
      IF ( (PK%XPATCH(JI) > 0.) .AND. (OLD%PATCH(JI) == 0.) ) THEN
        !
        !* New patch appears
        !
        IMASK = PK%NR_P(JI)
        CALL ATTRIBUTE_CLOSEST_VEGTYPE(INP,NVEGTYPE,ZPATCH_OLD(IMASK,:),JP,JP_NEAR)
        !
        PNEAR => NPE%AL(JP_NEAR)
        !
        !* Soil temperature
        !
        IF(IO%LTEMP_ARP)THEN
          IMASK=IO%NTEMPLAYER_ARP
        ELSEIF(IO%CISBA=='DIF')THEN
          IMASK=INL
        ELSE
          IMASK=2 !Only 2 temperature layer in ISBA-FR
        ENDIF
        !
        DO JL=1,IMASK
          PEK%XTG(JI,JL) = PNEAR%XTG(JI,JL)
        ENDDO
        !
        !* soil liquid and ice water contents
        !
        DO JL=1,INL
          PEK%XWG (JI,JL) = PNEAR%XWG (JI,JL)
          PEK%XWGI(JI,JL) = PNEAR%XWGI(JI,JL)
        ENDDO
        !
        IF(IO%CISBA/='DIF')THEN
          PEK%XWGI(JI,3) = 0.0 !Only 2 soil ice layers in ISBA-FR
        ENDIF
        !
        DO JL=1,INL
           IF(PEK%XWG(JI,JL)/=XUNDEF.AND.(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL))>K%XWSAT(IMASK,JL))THEN
              PEK%XWGI(JI,JL) = K%XWSAT(IMASK,JL) - PEK%XWG(JI,JL)
           ENDIF
        ENDDO
        !
        !* water intercepted on leaves
        !
        PEK%XWR(JI) = 0.0
        !
        !* glacier ice storage (semi-pro)
        !
        IF(IO%LGLACIER)THEN
          PEK%XICE_STO(JI) = 0.0
        ENDIF
        !
        !* snow Albedo
        !
        PEK%TSNOW%ALB(JI) = PNEAR%TSNOW%ALB(JI)
        !
        !* snow water equivalent and density
        !
        DO JL = 1,INS
           PEK%TSNOW%WSNOW(JI,JL) = PNEAR%TSNOW%WSNOW(JI,JL)
           PEK%TSNOW%RHO  (JI,JL) = PNEAR%TSNOW%RHO  (JI,JL)
        ENDDO
        !
        !* Heat content and age
        !
        IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') THEN
          DO JL = 1,INS
             PEK%TSNOW%HEAT(JI,JL) = PNEAR%TSNOW%HEAT(JI,JL)
             PEK%TSNOW%AGE (JI,JL) = PNEAR%TSNOW%AGE (JI,JL)
          ENDDO
        END IF
        !
        !* Snow Gran and History
        !
        IF (PEK%TSNOW%SCHEME=='CRO') THEN
          DO JL = 1,INS
             PEK%TSNOW%GRAN1(JI,JL) = PNEAR%TSNOW%GRAN1(JI,JL)
             PEK%TSNOW%GRAN2(JI,JL) = PNEAR%TSNOW%GRAN2(JI,JL)
             PEK%TSNOW%HIST (JI,JL) = PNEAR%TSNOW%HIST (JI,JL)
          ENDDO
        END IF
        !
        !* aerodynamical resistance
        !
        PEK%XRESA(JI) = PNEAR%XRESA(JI)
        !
        !* Leaf Area Index
        !
        IF (IO%CPHOTO/='NON' .AND. IO%CPHOTO/='AGS' .AND. IO%CPHOTO/='AST') THEN
           PEK%XLAI(JI) = PEK%XLAIMIN(JI) ! Parameter already initialized with new land use
        END IF
        !
        !* Assimilation and evapotranspiration
        !
        IF (IO%CPHOTO/='NON') THEN
           PEK%XAN   (JI) = 0.0 
           PEK%XANDAY(JI) = 0.0 
           PEK%XANFM (JI) = XANFMINIT 
        END IF
        !
        !* biomass (similar computation as in prep are done using LAIMIN)
        !
        IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
            PEK%XRESP_BIOMASS(JI,:) = 0.0 
            PEK%XBIOMASS     (JI,:) = 0.0
            PEK%XBIOMASS     (JI,1) = PEK%XLAIMIN(JI) * PK%XBSLAI_NITRO(JI)                                 ! Parameter initialized
            PEK%XBIOMASS     (JI,2) = MAX(0.,EXP(ZINVCA*LOG(PEK%XBIOMASS(JI,1)/ZCC_CA))-PEK%XBIOMASS(JI,1)) ! Optimization : X**n = EXP(n*LOG(X))
        END IF
        !
        !* litter and soil carbon (previous mean litter and soil carbon are attributed to the new patch)
        !
        IF (IO%CRESPSL=='CNT') THEN
          !       
          DO JNL=1,IO%NNLITTER
            DO JNLS=1,IO%NNLITTLEVS
              PEK%XLITTER(JI,JNL,JNLS) = PNEAR%XLITTER(JI,JNL,JNLS)
            ENDDO
          ENDDO
          !
          DO JNLS=1,IO%NNLITTLEVS
            PEK%XLIGNIN_STRUC(JI,JNLS) = PNEAR%XLIGNIN_STRUC(JI,JNLS)
          ENDDO
          !
          DO JNC=1,IO%NNSOILCARB
            PEK%XSOILCARB(JI,JNC) = PNEAR%XSOILCARB(JI,JNC) 
          ENDDO
          !
        ELSEIF(IO%CRESPSL=='DIF') THEN
           !
           DO JNL=1,IO%NNLITTER
              PEK%XSURFACE_LITTER(JI,JNL)=PNEAR%XSURFACE_LITTER(JI,JNL)
           ENDDO
           !
           DO JNL=1,IO%NNLITTER
              DO JL=1,INL
                 IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF.AND.PNEAR%XSOILDIF_LITTER(JI,JL,JNL)==XUNDEF)THEN
                   PEK%XSOILDIF_LITTER(JI,JL,JNL) = 0.0
                 ELSE
                   PEK%XSOILDIF_LITTER(JI,JL,JNL) = PNEAR%XSOILDIF_LITTER(JI,JL,JNL)
                 ENDIF
              ENDDO
           ENDDO
           !
           PEK%XSURFACE_LIGNIN_STRUC(JI) = PNEAR%XSURFACE_LIGNIN_STRUC(JI)
           !
           DO JL=1,INL
              IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF.AND.PNEAR%XSOILDIF_LIGNIN_STRUC(JI,JL)==XUNDEF)THEN
                PEK%XSOILDIF_LIGNIN_STRUC(JI,JL) = 0.0
               ELSE
                PEK%XSOILDIF_LIGNIN_STRUC(JI,JL) = PNEAR%XSOILDIF_LIGNIN_STRUC(JI,JL)
               ENDIF
           ENDDO
           !
           DO JNC=1,IO%NNSOILCARB
              DO JL=1,INL
                 IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF.AND.PNEAR%XSOILDIF_CARB(JI,JL,JNC)==XUNDEF)THEN
                   PEK%XSOILDIF_CARB(JI,JL,JNC) = 0.0
                 ELSE
                   PEK%XSOILDIF_CARB(JI,JL,JNC) = PNEAR%XSOILDIF_CARB(JI,JL,JNC)
                 ENDIF
              ENDDO
           ENDDO
           !
        ENDIF
        !
        !* Canopy air specific humidity
        !
        IF(IO%CPHOTO/='NON'.OR.ISIZE_LMEB_PATCH>0)THEN
          PEK%XQC(JI) = PNEAR%XQC(JI)
        ENDIF
        !
        !* MEB Prognostic or Semi-prognostic variables
        !
        IF (ISIZE_LMEB_PATCH>0) THEN
        !
        !* liquid water retained on litter
          PEK%XWRL(JI) = PNEAR%XWRL(JI)
        !
        !* ice retained on litter
          PEK%XWRLI(JI) = PNEAR%XWRLI(JI)
        !
        !* snow retained on the foliage (as for WR)
          PEK%XWRVN(JI) = 0.
        !
        !* canopy vegetation temperature
          PEK%XTV(JI) = PNEAR%XTV(JI)
        !
        !* litter temperature
          PEK%XTL(JI) = PNEAR%XTL(JI)
        !
        !* canopy air temperature
          PEK%XTC(JI) = PNEAR%XTC(JI)
        !
        ENDIF
        !
        IF(IO%LFIRE)THEN
          PEK%XFIREIND      (JI)=0.0
          PEK%XMOISTLIT_FIRE(JI)=PEK%XWG(JI,1)+PEK%XWGI(JI,1)
          PEK%XTEMPLIT_FIRE (JI)=PEK%XTG(JI,1)
        ENDIF
        !
        IF(IO%LSOILGAS)THEN
          DO JL=1,INL
             PEK%XSGASO2 (JI,JL) = PNEAR%XSGASO2 (JI,JL)
             PEK%XSGASCO2(JI,JL) = PNEAR%XSGASCO2(JI,JL)
             PEK%XSGASCH4(JI,JL) = PNEAR%XSGASCH4(JI,JL)
          ENDDO
        ENDIF       
      !
      ENDIF ! end of emerging or luluccf-driven changing pfts distribution
      !
    ENDDO  ! end of grid-cell loop
    !
  ENDDO ! end of patch loop
!
!-------------------------------------------------------------------------------
!
!*ISBA-DF case : Update water content and conserv
!
  IF(IO%CISBA=='DIF')THEN
    CALL LANDUSE_HYDRO(IO, S, NK, NP, NPE, TLU, KI)
  ENDIF
!
!-------------------------------------------------------------------------------
!
  IF(IO%CPHOTO=='NIT'.OR.IO%CPHOTO=='NCB')THEN           
    CALL LANDUSE_BIOMASS(IG, IO, S, NP, NPE, TLU, ILUOUT,  &
                         KI, ZBIO_GRID_OLD, ZBIO_GRID_NEW, &
                         ZLULCC_HARVEST_GRID               )
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*ISBA-CC case : Update biomass and carbon stocks and conserv
!
  IF(IO%CPHOTO=='NCB'.AND.(IO%CRESPSL=='CNT'.OR.IO%CRESPSL=='DIF'))THEN    
!   
     IF(IO%CRESPSL=='CNT')THEN
       CALL LANDUSE_CARBON(IO, S, NP, NPE, TLU, KI,          &
                           ZLITTER_GRID_OLD,ZCSOIL_GRID_OLD, &
                           ZLITTER_GRID_NEW,ZCSOIL_GRID_NEW, &
                           ZLULCC_HARVEST_GRID               )
     ELSEIF(IO%CRESPSL=='DIF')THEN
       CALL LANDUSE_CARBON_DIF(IO, S, NP, NPE, TLU, KI,          &
                               ZLITTER_GRID_OLD,ZCSOIL_GRID_OLD, &
                               ZLITTER_GRID_NEW,ZCSOIL_GRID_NEW, &
                               ZLULCC_HARVEST_GRID               )             
     ENDIF
!
     ZBUDGET(:) = (ZBIO_GRID_NEW(:)-ZBIO_GRID_OLD(:))*XPCCO2*XKGTOG+(ZLITTER_GRID_NEW(:)-ZLITTER_GRID_OLD(:)) &
                + (ZCSOIL_GRID_NEW(:)-ZCSOIL_GRID_OLD(:))+(ZLULCC_HARVEST_GRID(:)+S%XCCONSRV(:))*XKGTOG
!
     GSTOP=.FALSE.
     DO JI=1,KI
        IF(ABS(ZBUDGET(JI))>1.0E-10)THEN
          WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(ILUOUT,*)'INIT_ISBA_LANDUSE: NO CARBON CONSERVATION IN AT LEAST ONE GRID CELL'
          WRITE(ILUOUT,*)'LON = ',IG%XLON(JI),' LAT =',IG%XLAT(JI),'RESIDUE =',ZBUDGET(JI),'gC/m2'
          WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          GSTOP=.TRUE.
        ENDIF  
     ENDDO
     IF(GSTOP) CALL ABOR1_SFX('INIT_ISBA_LANDUSE: INCONSISTENCY IN CARBON BUDGET, SEE LISTING_OFF')
!
!
! *  Managing land-use scheme for carbon cycle
!
     IF(S%TTIME%TDATE%MONTH/=1.AND.S%TTIME%TDATE%DAY/=1)THEN  
       WRITE(ILUOUT,*)'INIT_ISBA_LANDUSE: LAND USE MANAGING CAN BE ONLY ANNUAL FOR NOW'
       CALL ABOR1_SFX('INIT_ISBA_LANDUSE: LAND USE MANAGING CAN BE ONLY ANNUAL FOR NOW')
     ENDIF
!
     DO JP=1,INP
        PEK => NPE%AL(JP)
        OLD => TLU%AL(JP)
        CALL LANDUSE_CARBON_MANAGING(OLD%VEGTYPE_PATCH, OLD%LULCC_HARVEST,     &  ! prev patch and vegtype distribs & carbon transferred to litter pools
                                     PEK%XEXPORT_DECADAL, PEK%XEXPORT_CENTURY, &  ! flux of Cant used
                                     PEK%XCSTOCK_DECADAL, PEK%XCSTOCK_CENTURY, &  ! pool of Cant
                                     PEK%XFLUATM, PEK%XFANTATM, PEK%XFLUANT    )  ! Flux of carbon due to LULUCCF
     ENDDO
!
!    kgC/m2 by year to kgC/m2/s
!
     ZNDAYS=366.*XDAY
!   
     IF(((MOD(S%TTIME%TDATE%YEAR,4)==0).AND.(MOD(S%TTIME%TDATE%YEAR,100)/=0)).OR.(MOD(S%TTIME%TDATE%YEAR,400)==0))THEN
        ZNDAYS=366.*XDAY
     ENDIF
!
     DO JP = 1,INP
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
        DO JI=1,PK%NSIZE_P
           PEK%XFLURES (JI) = PEK%XFLURES (JI)/ZNDAYS
           PEK%XFLUANT (JI) = PEK%XFLUANT (JI)/ZNDAYS
           PEK%XFLUATM (JI) = PEK%XFLUATM (JI)/ZNDAYS
           PEK%XFANTATM(JI) = PEK%XFANTATM(JI)/ZNDAYS
        ENDDO
     ENDDO
!
  ENDIF               
!
!-------------------------------------------------------------------------------
!           
ENDIF ! End of Land-use Land Cover Change case
!
IF(IO%CPHOTO=='NCB'.AND.(IO%CRESPSL=='CNT'.OR.IO%CRESPSL=='DIF'))THEN
! 
! Coupling surface-atmosphere for CO2 flux
! Already account for patch fraction
  DO JP=1,INP
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           S%XFLUTOATM(IMASK)=S%XFLUTOATM(IMASK)+PEK%XFLUATM(JI)+PEK%XFANTATM(JI)
     ENDDO
  ENDDO
!
ENDIF
!
!-----------------------------------------------------------
! Nudging case
!-----------------------------------------------------------
!
INTIME=3
!
IF(IO%CNUDG_WG=='DAY'.OR.IO%LNUDG_SWE)THEN
  SELECT CASE (S%TTIME%TDATE%MONTH)
     CASE(4,6,9,11)
      INTIME=30
     CASE(1,3,5,7:8,10,12)
      INTIME=31
     CASE(2)
      INTIME=29
  END SELECT
ENDIF
!
IF(IO%CNUDG_WG/='DEF')THEN
  !
  ZWORK(:,:)=XUNDEF
  DO JP=1,INP
     PK => NP%AL(JP)
     CALL UNPACK_SAME_RANK(PK%NR_P,PK%XNUDG_WGTOT(:,1,1),ZWORK(:,JP))
  ENDDO
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     !
     GLULU=ANY((PEK%XWG(:,1)/=XUNDEF).AND.(PK%XNUDG_WGTOT(:,1,1)==XUNDEF))
     !
     IF(GLULU)THEN  ! If any new patch has appeared, change the XNUDG_WGTOT climatology
       !
       WRITE(*,*)'--------------------------------------------------------------'
       WRITE(*,*)'!!!              Patches Distribution has changed          !!!' 
       WRITE(*,*)'!!!  Land-use nudging update computation are performed     !!!'
       WRITE(*,*)'--------------------------------------------------------------'
       !
       DO JI=1,PK%NSIZE_P
          !
          ! Where there is a change
          IF (PEK%XWG(JI,1)/=XUNDEF.AND.PK%XNUDG_WGTOT(JI,1,1)==XUNDEF) THEN
             !
             IMASK = PK%NR_P(JI)
             CALL ATTRIBUTE_CLOSEST_VEGTYPE_NUDGING(INP,NVEGTYPE,ZPATCH_OLD(IMASK,:),ZWORK(IMASK,:),JP,JP_NEAR) 
             !
             !* Total water content
             !
             DO JL=1,INL
                DO JT=1,INTIME
                   PK%XNUDG_WGTOT(JI,JL,JT) = NP%AL(JP_NEAR)%XNUDG_WGTOT(JI,JL,JT)
                ENDDO
             ENDDO
             !
             DO JL=1,INL
                DO JT=1,INTIME
                   IF(PK%XNUDG_WGTOT(JI,JL,JT)/=XUNDEF.AND.PK%XNUDG_WGTOT(JI,JL,JT)>K%XWSAT(IMASK,JL))THEN
                      PK%XNUDG_WGTOT(JI,JL,JT) = K%XWSAT(IMASK,JL)
                   ENDIF
                ENDDO
             ENDDO
             !
             ! Correct (complete) water content profile in the new soil layers
             !
             IF(IO%CISBA=='DIF')THEN
               CALL LANDUSE_HYDRO_NUDGING(IO, NK, NP, KI)                             
             ENDIF
             !
          ENDIF ! If a new patch arises
          !
       ENDDO ! JI
       !      
     ENDIF ! If there is any change
     !
  ENDDO ! JP
  !
ENDIF ! If CNUDG_WG/='DEF'
! 
IF(IO%LNUDG_SWE)THEN
  !
  ZWORK(:,:)=XUNDEF
  DO JP=1,INP
     PK => NP%AL(JP)
     CALL UNPACK_SAME_RANK(PK%NR_P,PK%XNUDG_SWE(:,1),ZWORK(:,JP))
  ENDDO
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     !
     GLULU=ANY((PEK%XWG(:,1)/=XUNDEF).AND.(PK%XNUDG_SWE(:,1)==XUNDEF))
     !
     IF(GLULU)THEN  ! If any new patch has appeared, change the XNUDG_SWE climatology
       !
       WRITE(*,*)'--------------------------------------------------------------'
       WRITE(*,*)'!!!              Patches Distribution has changed          !!!' 
       WRITE(*,*)'!!!  Land-use nudging update computation are performed     !!!'
       WRITE(*,*)'--------------------------------------------------------------'
       !
       DO JI=1,KI
          !
          ! Where there is a change
          IF(PEK%XWG(JI,1)/=XUNDEF.AND.PK%XNUDG_SWE(JI,1)==XUNDEF) THEN  
            IMASK = PK%NR_P(JI)
            !
            CALL ATTRIBUTE_CLOSEST_VEGTYPE_NUDGING(INP,NVEGTYPE,ZPATCH_OLD(IMASK,:),ZWORK(IMASK,:),JP,JP_NEAR) 
            !
            !* Total snow water content
            !
            DO JT=1,INTIME
               PK%XNUDG_SWE(JI,JT) = NP%AL(JP_NEAR)%XNUDG_SWE(JI,JT)
            ENDDO        
            !
          ENDIF ! If a new patch arises
          !
       ENDDO ! JI
       !
     ENDIF ! If there is any change
     !
  ENDDO ! JP
!
ENDIF ! If LNUDG_SWE_PATCH
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_ISBA_LANDUSE
