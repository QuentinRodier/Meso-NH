!SFX_LIC Copyright 2009-2018 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GARDEN_PGD_n (DTCO, U, OCH_BIO_FLUX, HPARAMBVOC, G, PGARDEN, &
                                  TOP, IO, S, K, P, PEK, DTV, PHV, PEKHV, DTHV, T, GB,  &
                                  HPROGRAM, HINIT, OPATCH1, KI, KVERSION, KBUGFIX, PCO2, PRHOA)
!#############################################################
!
!!****  *INIT_TEB_GARDEN_PGD_n* - routine to initialize ISBA
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
!&
!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!  11/2013 (B. Decharme) No exp profile with DIF
!!  01/2016 (E Redon/A Lemonsu) New fields for urban trees
!!  16/02/2018 (P. Wautelet): bug correction: allocate some work arrays to 0,1,1 instead of 0,0,1 (crash with XLF)
!!       2018 (J.Piannezze) : add MEGAN
!!  02/2019 (A. Druel)    Changes for irrigation
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!      ------------
!
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
USE MODD_SSO_n,          ONLY : SSO_t, SSO_INIT
USE MODD_SFX_GRID_n,     ONLY : GRID_t
USE MODD_TEB_OPTION_n,   ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_DATA_ISBA_n,    ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n,      ONLY : GR_BIOG_t
!
USE MODD_AGRI_n,         ONLY : AGRI_t
USE MODD_AGRI,           ONLY : LAGRIP, LIRRIGMODE, NVEG_IRR, XTHRESHOLD
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_DATA_COVER_PAR
USE MODD_SURF_PAR,       ONLY: XUNDEF, NUNDEF

USE MODD_SGH_PAR,        ONLY: XF_DECAY
!
USE MODI_READ_PREP_GARDEN_SNOW
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_VEG_PGD
USE MODI_READ_PGD_TEB_GARDEN_n
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_FROM_DATA_TEB_VEG_n
USE MODI_INIT_VEG_PGD_n
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_ABOR1_SFX
USE MODI_AV_PGD
!
USE MODE_TEB_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!      -------------------------
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
LOGICAL,              INTENT(IN) :: OCH_BIO_FLUX
CHARACTER(LEN=*), INTENT(IN) :: HPARAMBVOC
TYPE(GRID_t),         INTENT(INOUT) :: G
REAL, DIMENSION(:),   INTENT(IN) :: PGARDEN
TYPE(TEB_OPTIONS_t),  INTENT(INOUT) :: TOP
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_K_t),       INTENT(INOUT) :: K
TYPE(ISBA_P_t),       INTENT(INOUT) :: P, PHV
TYPE(ISBA_PE_t),      INTENT(INOUT) :: PEK, PEKHV
!
TYPE(DATA_ISBA_t),    INTENT(INOUT) :: DTV, DTHV
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(GR_BIOG_t),      INTENT(INOUT) :: GB
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,             INTENT(IN)  :: OPATCH1 ! flag to read PGD fields in the file
INTEGER,             INTENT(IN)  :: KI! number of points
INTEGER,             INTENT(IN)  :: KVERSION  ! version number of the file being read
INTEGER,             INTENT(IN)  :: KBUGFIX
REAL,     DIMENSION(KI),    INTENT(IN)  :: PCO2! CO2 concentration (kg/m3)
REAL,     DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
!
!
!
!*       0.2   Declarations of local variables
!      -------------------------------
!
TYPE(SSO_t)  :: YSS
TYPE(AGRI_t) :: YAG
!
INTEGER      :: JILU     ! loop increment
INTEGER      :: ILUOUT   ! unit of output listing file
!
INTEGER      :: IDECADE  ! decade of simulation
!
INTEGER      :: JVEG, JI ! loop counter on vegtypes
!
REAL, DIMENSION(KI)       :: ZF
REAL, DIMENSION(KI)       :: ZLAI_GARDEN
REAL, DIMENSION(0)  :: ZTDEEP_CLI, ZGAMMAT_CLI
REAL, DIMENSION(KI) :: ZSUM_LOW_VEGTYPES, ZSUM_HIGH_VEGTYPES
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CALL SSO_INIT(YSS)
!
!*       1.     Reading of snow configuration:
!               ------------------------------
!
!* initialization of snow scheme (TSNOW defined in MODD_TEB_GARDEN_n)
!
IF (HINIT=='PRE') THEN
  CALL READ_PREP_GARDEN_SNOW(HPROGRAM,PEK%TSNOW%SCHEME,PEK%TSNOW%NLAYER)
!
  IF (PEK%TSNOW%SCHEME.NE.'3-L' .AND. PEK%TSNOW%SCHEME.NE.'CRO' .AND. IO%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GARDEN_n: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Allocation of Physiographic fields
!               ----------------------------------
!
!* allocation of urban green area variables
!
 CALL ALLOCATE_TEB_VEG_PGD(PEK, S, K, P, OPATCH1, KI, NVEGTYPE, IO%NGROUND_LAYER, TOP%LGARDEN, .FALSE. )  
 IF (TOP%CURBTREE/='NONE' ) &
 CALL ALLOCATE_TEB_VEG_PGD(PEKHV, S, K, PHV, OPATCH1, KI, NVEGTYPE, IO%NGROUND_LAYER, TOP%LGARDEN, .TRUE. )  
!
!
IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
!-------------------------------------------------------------------------------
IF (OPATCH1) THEN
!-------------------------------------------------------------------------------
!
!
!*       3.     Read in the file of the primary data
!               ------------------------------------
!
  CALL READ_PGD_TEB_GARDEN_n(OCH_BIO_FLUX, HPARAMBVOC, DTCO, DTV, DTHV, T, GB, U, &
                             IO, K, G%NDIM, TOP, HPROGRAM,KVERSION,KBUGFIX)
!
!
!-------------------------------------------------------------------------------
!
!*       4.     Derives the fractions of each vegtype
!               -------------------------------------
!
  ! Only one vegetation (ISBA) PATCH in TEB; but there can be several TEB patches ()
  !
  ALLOCATE(S%XPATCH(KI,1),P%XPATCH(KI),PHV%XPATCH(KI))
  S%XPATCH  (:,1) = 1.
  P%XPATCH  (:)   = 1.
  PHV%XPATCH(:)   = 1.
  !
  ! Vegetation types fractions for each grid point
  !
  ! S%XVEGTYPE and S%XVEGTYPE_PATCH contains all vegetation types
  ! P%XVEGTYPE_PATCH contains all vegetation types IF no URBTREE option, only bare soil and low vegetation otherwise
  ! PHV%XVEGTYPE_PATCH contains only high vegetation types
  !
  ALLOCATE(S%XVEGTYPE(KI,NVEGTYPE))
  ALLOCATE(S%XVEGTYPE_PATCH(KI,NVEGTYPE,1))
  ALLOCATE(P%XVEGTYPE_PATCH(KI,NVEGTYPE))
  IF (TOP%CURBTREE/='NONE') ALLOCATE(PHV%XVEGTYPE_PATCH(KI,NVEGTYPE))
  !
!
!*       4.1    vegtypes pmrovided from user information on gardens
!               ---------------------------------------------------
!
IF (IO%LPAR) THEN
    ! fields provided by user
    P%XVEGTYPE_PATCH   = DTV%XPAR_VEGTYPE
    IF (TOP%CURBTREE/='NONE') THEN
      PHV%XVEGTYPE_PATCH = DTHV%XPAR_VEGTYPE
      DO JVEG=1,NVEGTYPE
        WHERE (T%XFRAC_LVEG(:) + T%XFRAC_NVEG(:) + T%XFRAC_HVEG(:) >0.)
          S%XVEGTYPE(:,JVEG) = (  P%XVEGTYPE_PATCH(:,JVEG) * (T%XFRAC_LVEG(:) + T%XFRAC_NVEG(:)) &
                                + PHV%XVEGTYPE_PATCH(:,JVEG) * T%XFRAC_HVEG(:) )                 &
                              / (T%XFRAC_LVEG(:) + T%XFRAC_NVEG(:) + T%XFRAC_HVEG(:))
        END WHERE
      END DO
    ELSE
      S%XVEGTYPE = P%XVEGTYPE_PATCH
    END IF
    !
    ! when no garden, assumes bare soil for computational purposes in subsequent routines
    DO JVEG=1,NVEGTYPE
      WHERE (PGARDEN==0)
        S%XVEGTYPE(:,JVEG) = 0.
        S%XVEGTYPE(:,1) = 1.
      END WHERE
    ENDDO

  ELSE
!
!*       4.2    vegtypes provided from ecoclimap
!               ---------------------------------
!
    !classical ecoclimap case
    DO JVEG=1,NVEGTYPE
      ! uses garden fraction as weighting factor for vegtype initialisation
      CALL AV_PGD(DTCO, S%XVEGTYPE(:,JVEG),TOP%XCOVER ,DTCO%XDATA_VEGTYPE(:,JVEG),'GRD','ARI',TOP%LCOVER)
    END DO
!
    ! when no garden, assumes bare soil for computational purposes in subsequent routines
    DO JVEG=1,NVEGTYPE
      WHERE (PGARDEN==0)
        S%XVEGTYPE(:,JVEG) = 0.
        S%XVEGTYPE(:,1) = 1.
      END WHERE
    ENDDO
     P%XVEGTYPE_PATCH(:,:) = S%XVEGTYPE(:,:)
     IF (TOP%CURBTREE/='NONE') THEN
       PHV%XVEGTYPE_PATCH(:,:) = S%XVEGTYPE(:,:)
    ! separates vegtypes according to bare soil & low veg / high veg
       P%XVEGTYPE_PATCH(:,NVT_TEBD) = 0.
       P%XVEGTYPE_PATCH(:,NVT_TRBE) = 0.
       P%XVEGTYPE_PATCH(:,NVT_BONE) = 0.
       P%XVEGTYPE_PATCH(:,NVT_TRBD) = 0.
       P%XVEGTYPE_PATCH(:,NVT_TEBE) = 0.
       P%XVEGTYPE_PATCH(:,NVT_TENE) = 0.
       P%XVEGTYPE_PATCH(:,NVT_BOBD) = 0.
       P%XVEGTYPE_PATCH(:,NVT_BOND) = 0.
       P%XVEGTYPE_PATCH(:,NVT_SHRB) = 0.
       ZSUM_LOW_VEGTYPES(:) = SUM(P%XVEGTYPE_PATCH(:,:), DIM=2)
       DO JVEG=1,NVEGTYPE
         P%XVEGTYPE_PATCH(:,JVEG) = P%XVEGTYPE_PATCH(:,JVEG) / ZSUM_LOW_VEGTYPES(:)
       END DO

       IF (NVT_C3>0) THEN ! ecoclimap
         PHV%XVEGTYPE_PATCH(:,NVT_C3  ) =0.
         PHV%XVEGTYPE_PATCH(:,NVT_IRR ) =0.
         PHV%XVEGTYPE_PATCH(:,NVT_PARK) =0.
       ELSE  ! eco SG
         PHV%XVEGTYPE_PATCH(:,NVT_C3W ) =0.
         PHV%XVEGTYPE_PATCH(:,NVT_C3S ) =0.
         PHV%XVEGTYPE_PATCH(:,NVT_FLTR) =0.
         PHV%XVEGTYPE_PATCH(:,NVT_FLGR) =0.
       END IF
       PHV%XVEGTYPE_PATCH(:,NVT_C4) = 0.
       PHV%XVEGTYPE_PATCH(:,NVT_GRAS) =0.
       PHV%XVEGTYPE_PATCH(:,NVT_TROG) =0.
       PHV%XVEGTYPE_PATCH(:,NVT_NO  ) =0.
       PHV%XVEGTYPE_PATCH(:,NVT_ROCK) =0.
       PHV%XVEGTYPE_PATCH(:,NVT_SNOW) =0.

       ZSUM_HIGH_VEGTYPES(:) = SUM(PHV%XVEGTYPE_PATCH(:,:), DIM=2)
       DO JVEG=1,NVEGTYPE
         WHERE (ZSUM_HIGH_VEGTYPES(:)>0.)
           PHV%XVEGTYPE_PATCH(:,JVEG) = PHV%XVEGTYPE_PATCH(:,JVEG) / ZSUM_HIGH_VEGTYPES(:)
         END WHERE
       END DO
       WHERE (ZSUM_HIGH_VEGTYPES(:)>0.)
         PHV%XVEGTYPE_PATCH(:,NVT_TEBD) = 1. ! default value, withotu impact on physics
       END WHERE
     END IF
  ENDIF
  !
  ! Take care about irrigation fraction if ECOSG and IRRIGATION
  ALLOCATE(S%XVEGTYPE2(KI,NVEGTYPE+NVEG_IRR))
  S%XVEGTYPE2(:,:) = 0.
  IF ( U%LECOSG .AND. ( LIRRIGMODE .OR. LAGRIP ) ) THEN
    !
    IF ( NVEG_IRR == 0 ) CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS - WITH LECOSG AND (LIRRIGMODE OR LAGRIP), NVEG_IRR HAVE TO BE >0')
    !
    IF ( NVEG_IRR /= SIZE(DTV%NPAR_VEG_IRR_USE)) &
      CALL ABOR1_SFX('PCOMPUTE_ISBA_PARAMETER: WHEN LIRRIGMODE or LAGRIP+LECOSG, NVEG_IRR AND NPAR_VEG_IRR_USE HAVE TO BE EQUAL')
    !
    ! IF ECOSG and IRRIGATION, compute fraction of non irrigated vegtype + irrigated vegtype
    DO JVEG=1,NVEGTYPE+NVEG_IRR
      IF ( JVEG <= NVEGTYPE ) THEN
        IF ( ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG ) ) THEN
          WHERE ( DTV%XPAR_IRRIGFRAC(:,JVEG) /= XUNDEF .AND. DTV%XPAR_IRRIGFRAC(:,JVEG) /= 0. )
            S%XVEGTYPE2(:,JVEG) = S%XVEGTYPE(:,JVEG) * (1 - DTV%XPAR_IRRIGFRAC(:,JVEG) )
          ELSEWHERE
            S%XVEGTYPE2(:,JVEG) = S%XVEGTYPE(:,JVEG)
          ENDWHERE
        ELSE
          S%XVEGTYPE2(:,JVEG) = S%XVEGTYPE(:,JVEG)
        ENDIF
      ELSE
        WHERE ( DTV%XPAR_IRRIGFRAC(:,DTV%NPAR_VEG_IRR_USE(JVEG-NVEGTYPE)) /= XUNDEF .AND.        &
                DTV%XPAR_IRRIGFRAC(:,DTV%NPAR_VEG_IRR_USE(JVEG-NVEGTYPE)) /= 0. )
          S%XVEGTYPE2(:,JVEG) = S%XVEGTYPE(:,DTV%NPAR_VEG_IRR_USE(JVEG-NVEGTYPE))                &
                                * DTV%XPAR_IRRIGFRAC(:,DTV%NPAR_VEG_IRR_USE(JVEG-NVEGTYPE))
        ELSEWHERE
          S%XVEGTYPE2(:,JVEG) = 0.
        ENDWHERE
      ENDIF
    ENDDO
    !
  ELSE
    !
    IF ( NVEG_IRR/=0 ) CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS - WITHOUT LECOSG AND (LIRRIGMODE OR LAGRIP), NVEG_IRR HAVE TO BE =0')
    !
    S%XVEGTYPE2(:,:) = S%XVEGTYPE(:,:)
    !
  ENDIF
!
!*       4.3    finalization
!               ------------
!
!
  S%XVEGTYPE_PATCH(:,:,1) = S%XVEGTYPE
  !
  DO JVEG=1,NVEGTYPE
    WHERE (PGARDEN==0)
      S%XVEGTYPE(:,JVEG) = 0.
      S%XVEGTYPE(:,1) = 1.
      S%XVEGTYPE2(:,JVEG) = 0.
      S%XVEGTYPE2(:,1) = 1
    END WHERE
  ENDDO
  !
  ALLOCATE(S%XPATCH(KI,1),P%XPATCH(KI))
  ALLOCATE(S%XVEGTYPE_PATCH(KI,NVEGTYPE+NVEG_IRR,1),P%XVEGTYPE_PATCH(KI,NVEGTYPE+NVEG_IRR))
  S%XPATCH(:,1) = 1.
  P%XPATCH(:) = S%XPATCH(:,1)
  S%XVEGTYPE_PATCH(:,:,1) = S%XVEGTYPE2
  P%XVEGTYPE_PATCH(:,:) = S%XVEGTYPE_PATCH(:,:,1)
  P%NSIZE_P = KI
  ALLOCATE(P%NR_P(KI))
  DO JI = 1,SIZE(P%NR_P)
    P%NR_P(JI) = JI
  ENDDO
  IF (TOP%CURBTREE/='NONE') THEN
    PHV%NSIZE_P = KI
    ALLOCATE(PHV%NR_P(KI))
    PHV%NR_P = P%NR_P
  END IF
!
!-------------------------------------------------------------------------------
!
!*       5.     Fields valid for all TEB patches
!               --------------------------------
!
!
!*       5.1    Physiographic data fields from land cover:
!               -----------------------------------------
!
  IF (.NOT. IO%LPAR) THEN
    ! fields computed by ecoclimap
    IF (TOP%CURBTREE/='NONE' ) &
    CALL CONVERT_PATCH_ISBA(DTCO, DTHV, IO, 1, 1, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE., .FALSE., .FALSE.,'GRD', 1, K, PHV, PEKHV, &
                        .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., &
                        PSOILGRID=IO%XSOILGRID  )   
    CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, 1, 1, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,.FALSE.,.FALSE.,'GRD', 1, K, P, PEK, &
                        .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,&
                        PSOILGRID=IO%XSOILGRID  )   
    ! specific INDIVIDUAL fields being specified by user (only RE25 for the time being)
    IF (IO%CPHOTO/='NON' .AND. DTV%LDATA_RE25(1)) P%XRE25(:) = DTV%XPAR_RE25(:,1)
    IF (IO%CPHOTO/='NON' .AND. DTV%LDATA_RE25(1) .AND. TOP%CURBTREE/='NONE') PHV%XRE25(:) = DTHV%XPAR_RE25(:,1)
  ELSE
!
!*       5.2    OR Physiographic data fields from land cover:
!               --------------------------------------------
!
    ! urban vegetation fields ALL TOGETHER specified by user
    ! All vegetation parameters will be deduced from it 
    IF (TOP%CURBTREE/='NONE' ) &
    CALL INIT_FROM_DATA_TEB_VEG_n(DTHV, K, PHV, PEKHV, IDECADE, .FALSE., .TRUE., .FALSE.,.FALSE.,OHG=TOP%LGARDEN,&
            OHV=.TRUE.)
    CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .TRUE., .FALSE.,.FALSE.,OHG=TOP%LGARDEN,&
            OHV=.FALSE.)
  ENDIF
  !
  ALLOCATE(S%XWSN_WR(0,1,1))
  ALLOCATE(S%XBANDS_WR(0,1,1))  
  ALLOCATE(S%XRHO_WR(0,1,1))
  ALLOCATE(S%XALB_WR(0,1))
  ALLOCATE(S%XHEA_WR(0,1,1))
  ALLOCATE(S%XAGE_WR(0,1,1))
  ALLOCATE(S%XSG1_WR(0,1,1))
  ALLOCATE(S%XSG2_WR(0,1,1)) 
  ALLOCATE(S%XHIS_WR(0,1,1))
  !
END IF
!
!-------------------------------------------------------------------------------
!
!*       6.     Fields varying from one TEB patch to the other
!               ----------------------------------------------
!
!
!*       6.1    Additional Physiographic data fields from land cover:
!               ----------------------------------------------------
!
!
IF (.NOT. IO%LPAR) THEN
    IF (TOP%CURBTREE=='NONE') THEN
      CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, 1, 1, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,.FALSE.,.FALSE.,'GRD', 1, K, P, PEK, &
                        .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.  )   
                    ! LAI fields for low and hight vegetation
     ELSE
      !       
      ! sets number of patches to 2 JUST for this calculation of parameters
      IO%NPATCH=2
      !
                    ! LAI fields for low vegetation
      CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, 1, 1, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,.FALSE.,.FALSE.,'GRD', 1, K, P, PEK, &
                        .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.  )   
                    ! LAI fields for high vegetation
      CALL CONVERT_PATCH_ISBA(DTCO, DTHV, IO, 1, 1, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,.FALSE.,.FALSE.,'GRD', 2, K, PHV, PEKHV, &
                        .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.  )
      !
      ! comes back to only one patch
      IO%NPATCH=1
      !
      ! In case of explicit high vegetation scheme (no big leag), the LAI of tree canopy (alone)
      ! is found by substracting low vegetation LAI to aggregated LAI on the tree vegtype part
      !
      PEKHV%XLAI = MAX(PEKHV%XLAI - PEK%XLAI, 0.)
      !
    END IF
ELSE
!
!*       6.2    OR Additional Physiographic data fields from user :
!               -------------------------------------------------
   !
   IF (TOP%CURBTREE/='NONE') &
   CALL INIT_FROM_DATA_TEB_VEG_n(DTHV, K, PHV, PEKHV, IDECADE, .FALSE., .FALSE., .TRUE., .FALSE., &
        OHG=TOP%LGARDEN, OHV=TOP%CURBTREE/='NONE' )
  !
   CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .FALSE., .TRUE., .FALSE., &
        OHG=TOP%LGARDEN, OHV=TOP%CURBTREE/='NONE' )
   !
   IF (IO%CISBA=='DIF') CALL INIT_IF_DIF(IO%NGROUND_LAYER, PGARDEN, P)
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*       7.     Diagnostic Physiographic fields
!               -------------------------------
!
 CALL INIT_IF_NOVEG(PGARDEN, IO, S, P, PEK)
!
ALLOCATE(K%XVEGTYPE(KI,NVEGTYPE))
K%XVEGTYPE = S%XVEGTYPE
!
ALLOCATE(YSS%XAOSIP(0))
!
 CALL INIT_VEG_PGD_n(YSS, DTV, IO, S, K, K, P, PEK, YAG, KI,    &
     HPROGRAM, 'TOWN  ',ILUOUT, KI, TOP%TTIME%TDATE%MONTH, &
     .TRUE., .FALSE., .FALSE., .TRUE., .TRUE., ZTDEEP_CLI, ZGAMMAT_CLI, &
     .FALSE.,.FALSE., XTHRESHOLD, HINIT, PCO2, PRHOA ) 
! initializes CO2-related fields for high vegetation
 IF (TOP%CURBTREE/='NONE') &
 CALL INIT_VEG_PGD_n(YSS, DTHV, IO, S, K, K, PHV, PEKHV, YAG, KI,    &
     HPROGRAM, 'TOWN  ',ILUOUT, KI, TOP%TTIME%TDATE%MONTH, &
     .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., ZTDEEP_CLI, ZGAMMAT_CLI, &
     .FALSE.,.FALSE., XTHRESHOLD, HINIT, PCO2, PRHOA )
!
! correction copied from garden.F90 :
! in urban areas, dynamical roughness length is influenced by local obstacles due to
! the heterogeneity of the urban fabric (Lemonsu, Redon et al 2021)
!
IF (TOP%CZ0EFF_GD=='LR21') THEN 
  PEK%XZ0(:) = MAX (PEK%XZ0(:), 0.3)
END IF
!
!-------------------------------------------------------------------------------
!
IF(IO%CISBA=='DIF'.AND.IO%LSOC)THEN
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: SUBGRID Soil organic matter'//&
 ' effect (LSOC) NOT YET IMPLEMENTED FOR GARDEN')
ELSEIF (IO%CISBA=='3-L'.AND.IO%CKSAT=='EXP') THEN 
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: topmodel exponential decay not implemented for garden')
ENDIF
!
IF(IO%CKSAT=='SGH' .AND. IO%CISBA/='DIF' .AND. HINIT/='PRE')THEN 
  ZF(:)=MIN(4.0/P%XDG(:,2),XF_DECAY)
  CALL EXP_DECAY_SOIL_FR(IO%CISBA, ZF, P)
ENDIF
!
!-------------------------------------------------------------------------------
!
!       Coherence checks and corrections
!       --------------------------------
!
!
! HTREE > BLD height : blindage
! Height of tree is limited by the building height
!
IF (TOP%CURBTREE/='NONE') THEN
 DO JI = 1,KI
  IF (PHV%XH_TREE(JI).GT.T%XBLD_HEIGHT(JI)) THEN
    !
    PHV%XH_TREE(JI)      = T%XBLD_HEIGHT(JI)
    PHV%XHTRUNK_HVEG(JI) = MIN(PHV%XH_TREE(JI)/2.,PHV%XHTRUNK_HVEG(JI))
    !
  ENDIF
 END DO
!
! HTREE = HTRUNK on a few points : blindage
! Height of trunk is limited not to exceed half the height of tree
!
 DO JI = 1,KI
  IF  (PHV%XHTRUNK_HVEG(JI).GE.PHV%XH_TREE(JI)) THEN
    !
    PHV%XHTRUNK_HVEG(JI)  =  PHV%XH_TREE(JI)/2.
    !
  END IF
 ENDDO
!
! IF H_TREE /= 0 and HTRUNK or WCROWN == 0 abort
!
 DO JI = 1,KI
   IF ((PHV%XH_TREE(JI) .NE. 0.) .AND. &
           ((PHV%XHTRUNK_HVEG(JI) .EQ. 0.) .OR. (PHV%XWCROWN_HVEG(JI) .EQ. 0.))) THEN
       CALL GET_LUOUT(HPROGRAM,ILUOUT)
       WRITE(ILUOUT,*) ''
       WRITE(ILUOUT,*) 'In init_teb_garden_pgdn :'
       WRITE(ILUOUT,*) 'Inconsistency between H_TREE and HTRUNK_HVEG or WCROWN_HVEG'
       WRITE(ILUOUT,*) 'In this point the height of tree exists (>0)'
       WRITE(ILUOUT,*) 'but the height of trunk or'
       WRITE(ILUOUT,*) 'the width of the crown are set to 0 '
       WRITE(ILUOUT,*) ''
       WRITE(ILUOUT,*) 'JI                      : ', JI
       WRITE(ILUOUT,*) 'H_TREE                  : ', PHV%XH_TREE(JI)
       WRITE(ILUOUT,*) 'HTRUNK_HVEG             : ', PHV%XHTRUNK_HVEG(JI)
       WRITE(ILUOUT,*) 'WCROWN_HVEG             : ', PHV%XWCROWN_HVEG(JI)
       WRITE(ILUOUT,*) ''
       CALL ABOR1_SFX('INIT_TEB_GAREDN_PGDN: Inconsistency between H_TREE (>0) and HTRUNK_HVEG or WCROWN_HVEG (=0)')
   ENDIF
 ENDDO
ELSE
 !
 ! No CURBTREE case
 ! Height of tree is limited by the building height 
 !
 DO JI = 1,KI
  IF (P%XH_TREE(JI).GT.T%XBLD_HEIGHT(JI)) THEN
    !
    P%XH_TREE(JI)      = T%XBLD_HEIGHT(JI)
    !
  ENDIF
 ENDDO
END IF
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GARDEN_PGD_n
