!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_VEG (DTCO, UG, U, USS, GDM, GRM, TOP, TG, &
                              HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_VEG* monitor for averaging and interpolations of physiographic fields
!!                   for natural covers of TEB
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2010
!!
!!    J.Escobar   11/2013   Add USE MODI_PGD_TEB_GREENROOF
!!    V. Masson   04/2014   Adds Irrigation
!!    P. Samuelsson 02/2014 Introduced dummy variable in call to READ_NAM_PGD_ISBA for MEB
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
!
USE MODD_PGD_GRID,          ONLY : NL
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_PGD_TEB_GREENROOF
USE MODI_PGD_TEB_GARDEN_PAR
USE MODI_PGD_TEB_IRRIG
USE MODI_CONVERT_COVER_TEB_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                  :: ILUOUT    ! output listing logical unit
INTEGER                  :: JLAYER    ! loop counter
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: YISBA            ! ISBA option
 CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo-transfert function for DIF
 CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil layer thickness for DIF
REAL,    DIMENSION(:,:,:), ALLOCATABLE :: ZDG       ! ground layers
INTEGER, DIMENSION(:,:),   ALLOCATABLE :: IWG_LAYER ! number of layers for DIF option
!
! Not used in TEB garden
!
 CHARACTER(LEN=28)        :: YSOC_TOP      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YSOC_SUB      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YPERM         ! file name for permafrost distribution
 CHARACTER(LEN=28)        :: YGW               ! file name for groundwater map
 CHARACTER(LEN=6)         :: YSOCFILETYPE  ! organic carbon data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE ! permafrost distribution data file type
 CHARACTER(LEN=6)         :: YGWFILETYPE      ! groundwater distribution data file type
REAL                     :: XUNIF_SOC_TOP ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_PERM    ! uniform permafrost distribution
REAL                     :: XUNIF_GW         ! uniform groundwater distribution
LOGICAL                  :: LIMP_SOC      ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_PERM     ! Imposed maps of permafrost distribution
LOGICAL                  :: GMEB          ! Multi-energy balance (MEB)
LOGICAL                  :: LIMP_GW          ! Imposed maps of groundwater distribution
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_VEG',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    1.      Reading of namelist NAM_ISBA for general options of vegetation
!             --------------------------------------------------------------
!
GDM%TGDO%NGROUND_LAYER = 0
GDM%TVG%CISBA         = '   '
GDM%TVG%CPEDOTF       = '   '
GDM%TVG%CPHOTO        = '   '
!
 CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                         &
                       YISBA, YPEDOTF, YPHOTO,  GTR_ML, ZRM_PATCH,              &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,             &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,             &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,         &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,   &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, GMEB,       &                     
                       YGW, YGWFILETYPE, XUNIF_GW, LIMP_GW,                     &                     
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,               &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,     &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,        &
                       XUNIF_FERT                                               )  
!
GDM%TGDO%NGROUND_LAYER = IGROUND_LAYER
GDM%TVG%CISBA         = YISBA
GDM%TVG%CPEDOTF       = YPEDOTF
GDM%TVG%CPHOTO        = YPHOTO
GDM%TVG%LTR_ML        = GTR_ML
!
!-------------------------------------------------------------------------------
!
!*    2.      Coherence of options
!             --------------------
!
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',GDM%TVG%CISBA,'2-L','3-L','DIF')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',GDM%TVG%CPEDOTF,'CH78','CO84')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',GDM%TVG%CPHOTO,'NON','AGS','LAI','AST','LST','NIT','NCB')
  !
  IF (GDM%TVG%CPHOTO=='NCB') THEN
    GDM%TVG%CPHOTO = 'NIT'
    WRITE(ILUOUT,*) '****************************************************************'
    WRITE(ILUOUT,*) '* FOR GARDENS, AGS OPTION HAS BEEN CHANGED FROM "NCB" TO "NIT" *'
    WRITE(ILUOUT,*) '****************************************************************'
  END IF
!
  SELECT CASE (GDM%TVG%CISBA)
    CASE ('2-L')
      GDM%TGDO%NGROUND_LAYER = 2
      GDM%TVG%CPEDOTF       ='CH78'       
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',GDM%TVG%CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'          
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('3-L')
      GDM%TGDO%NGROUND_LAYER = 3
      GDM%TVG%CPEDOTF       ='CH78'         
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',GDM%TVG%CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'        
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('DIF')
      IF(GDM%TGDO%NGROUND_LAYER==NUNDEF)THEN
        IF(TOP%LECOCLIMAP)THEN
          GDM%TGDO%NGROUND_LAYER=NOPTIMLAYER
        ELSE
          WRITE(ILUOUT,*) '****************************************'
          WRITE(ILUOUT,*) '* Number of ground layer not specified *'
          WRITE(ILUOUT,*) '****************************************'
          CALL ABOR1_SFX('PGD_TEB_GARDEN: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
        ENDIF
      ENDIF
! 
      ALLOCATE(GDM%TGDO%XSOILGRID(GDM%TGDO%NGROUND_LAYER))
      GDM%TGDO%XSOILGRID(:)=XUNDEF
      GDM%TGDO%XSOILGRID(:)=ZSOILGRID(1:GDM%TGDO%NGROUND_LAYER) 
      IF(ALL(ZSOILGRID(:)==XUNDEF))THEN
        IF(TOP%LECOCLIMAP) &
                GDM%TGDO%XSOILGRID(1:GDM%TGDO%NGROUND_LAYER)=XOPTIMGRID(1:GDM%TGDO%NGROUND_LAYER)
      ELSEIF(COUNT(GDM%TGDO%XSOILGRID/=XUNDEF)/=GDM%TGDO%NGROUND_LAYER)THEN
        WRITE(ILUOUT,*) '********************************************************'
        WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
        WRITE(ILUOUT,*) '********************************************************'
        CALL ABOR1_SFX('PGD_TEB_GARDEN: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA')            
      ENDIF
!
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* Option CISBA            = ',GDM%TVG%CISBA
      WRITE(ILUOUT,*) '* Pedo transfert function = ',GDM%TVG%CPEDOTF    
      WRITE(ILUOUT,*) '* Number of soil layers   = ',GDM%TGDO%NGROUND_LAYER
      IF(TOP%LECOCLIMAP)THEN
        WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',GDM%TGDO%XSOILGRID(1:GDM%TGDO%NGROUND_LAYER)
      ENDIF
      WRITE(ILUOUT,*) '*****************************************' 

  END SELECT
!
  SELECT CASE (GDM%TVG%CPHOTO)
    CASE ('AGS','LAI','AST','LST')
      GDM%TVG%NNBIOMASS = 1
    CASE ('NIT')
      GDM%TVG%NNBIOMASS = 3
  END SELECT
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ',GDM%TVG%CPHOTO,'               *'
  WRITE(ILUOUT,*) '* the number of biomass pools is set to ', GDM%TVG%NNBIOMASS
  WRITE(ILUOUT,*) '*****************************************'
!
!-------------------------------------------------------------------------------
!
!*    3.      Sand fraction
!             -------------
!
ALLOCATE(GDM%TGDP%XSAND(TG%NDIM,GDM%TGDO%NGROUND_LAYER))
!
IF(LIMP_SAND)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'sand fraction','TWN',YSAND,YSANDFILETYPE,XUNIF_SAND,GDM%TGDP%XSAND(:,1))
ENDIF
!
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  GDM%TGDP%XSAND(:,JLAYER) = GDM%TGDP%XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    4.      Clay fraction
!             -------------
!
ALLOCATE(GDM%TGDP%XCLAY(TG%NDIM,GDM%TGDO%NGROUND_LAYER))
!
IF(LIMP_CLAY)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'clay fraction','TWN',YCLAY,YCLAYFILETYPE,XUNIF_CLAY,GDM%TGDP%XCLAY(:,1))
ENDIF
!
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  GDM%TGDP%XCLAY(:,JLAYER) = GDM%TGDP%XCLAY(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    5.      Subgrid runoff 
!             --------------
!
ALLOCATE(GDM%TGDP%XRUNOFFB(TG%NDIM))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid runoff','TWN',YRUNOFFB,YRUNOFFBFILETYPE,XUNIF_RUNOFFB,GDM%TGDP%XRUNOFFB(:))
!
!-------------------------------------------------------------------------------
!
!*    6.      Drainage coefficient
!             --------------------
!
ALLOCATE(GDM%TGDP%XWDRAIN(TG%NDIM))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid drainage','TWN',YWDRAIN,YWDRAINFILETYPE,XUNIF_WDRAIN,GDM%TGDP%XWDRAIN(:))
!
!-------------------------------------------------------------------------------
!
!*    7.      Interpolation of GARDEN physiographic fields
!             --------------------------------------------
!
GDM%DTGD%NTIME = 12
 CALL PGD_TEB_GARDEN_PAR(DTCO, UG, U, USS, TG, GDM, &
                         HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of greenroofs
!             ------------------
!
IF (TOP%LGREENROOF) CALL PGD_TEB_GREENROOF(DTCO, UG, U, USS, GRM, TG, &
                                           HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Irrigation of gardens and greenroofs
!             ------------------------------------
!
 CALL PGD_TEB_IRRIG(DTCO, UG, U, USS, TG, GDM%TIR, &
                   HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Case of urban hydrology
!             -----------------------
!
IF (TOP%LHYDRO) print*," CALL PGD_TEB_URBHYDRO(HPROGRAM,LECOCLIMAP)"
!
!-------------------------------------------------------------------------------
!
!*   10.      GARDEN diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ------------------------------------------------------------------------------------
!
IF (TOP%LECOCLIMAP) THEN
  ALLOCATE(ZDG(TG%NDIM,GDM%TGDO%NGROUND_LAYER,1))
  ALLOCATE(IWG_LAYER(TG%NDIM,1))
  CALL CONVERT_COVER_TEB_VEG(DTCO,GDM%TVG,GDM%TVG%CISBA,1,TOP%XCOVER,TOP%LCOVER,'   ',&
          'GRD',PSOILGRID=GDM%TGDO%XSOILGRID,PDG=ZDG,KWG_LAYER=IWG_LAYER)
  !
  ALLOCATE(GDM%TGDP%XDG(TG%NDIM,GDM%TGDO%NGROUND_LAYER))
  GDM%TGDP%XDG(:,:) = ZDG(:,:,1)
  IF (GDM%TVG%CISBA=='DIF') THEN
    ALLOCATE(GDM%TGDP%NWG_LAYER(TG%NDIM))
    GDM%TGDP%NWG_LAYER(:) = IWG_LAYER(:,1)
  ELSE
    ALLOCATE(GDM%TGDP%NWG_LAYER(0))
  END IF
END IF
!-------------------------------------------------------------------------------
!!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_VEG
