!#############################################################
SUBROUTINE INIT_VEG_PGD_n(HPROGRAM, HSURF, KLUOUT, KI, KPATCH, KGROUND_LAYER, KMONTH,        &
                        PVEGTYPE, PPATCH, PVEGTYPE_PATCH, KSIZE_NATURE_P, KR_NATURE_P,  &
                        PRM_PATCH, &
                        ODEEPSOIL, OPHYSDOMC, PTDEEP_CLI, PGAMMAT_CLI, PTDEEP, PGAMMAT, &
                        OAGRIP, PTHRESHOLD, KIRRINUM, OIRRIDAY, OIRRIGATE, PTHRESHOLDSPT, &
                        HPHOTO, HINIT, OTR_ML, KNBIOMASS, PCO2, PRHOA, PABC, PPOI,  &
                        PGMES, PGC, PDMAX, PANMAX, PFZERO, PEPSO, PGAMM, PQDGAMM,   &
                        PQDGMES, PT1GMES, PT2GMES, PAMAX, PQDAMAX, PT1AMAX, PT2AMAX,&
                        PAH, PBH, PTAU_WOOD, PINCREASE, PTURNOVER,                  &
                        KSV, HSV, KBEQ, HSVO, KAEREQ, KSV_CHSBEG, KSV_CHSEND,       &
                        KSV_AERBEG, KSV_AEREND, HCH_NAMES, HAER_NAMES, KDSTEQ,      &
                        KSV_DSTBEG, KSV_DSTEND, KSLTEQ, KSV_SLTBEG, KSV_SLTEND,     &
                        HDSTNAMES, HSLTNAMES, HCHEM_SURF_FILE,                      &
                        PSFDST, PSFDSTM, PSFSLT,                                    &
                        PAOSIP, PAOSIM, PAOSJP, PAOSJM, PHO2IP, PHO2IM, PHO2JP,     &
                        PHO2JM, PZ0, PZ0EFFIP, PZ0EFFIM, PZ0EFFJP, PZ0EFFJM, PZ0REL,&
                        PCLAY, PSAND, HPEDOTF,                                      &
                        PCONDSAT, PMPOTSAT, PBCOEF, PWWILT, PWFC, PWSAT,            &
                        PTAUICE, PCGSAT, PC1SAT, PC2REF, PC3, PC4B, PACOEF, PPCOEF, &
                        PC4REF, PPCPS, PPLVTT, PPLSTT,                              &
                        HSCOND, HISBA, PHCAPSOIL, PCONDDRY, PCONDSLD, HCPSURF,      &
                        PDG, PDROOT, PDG2, PROOTFRAC, PRUNOFFD, PDZG, PDZDIF,       &
                        PSOILWGHT, KWG_LAYER, KLAYER_HORT, KLAYER_DUN, PD_ICE,      &
                        PKSAT_ICE, PALBNIR_DRY, PALBVIS_DRY, PALBUV_DRY,            &
                        PALBNIR_WET, PALBVIS_WET, PALBUV_WET, PBSLAI_NITRO,         &
                        PCE_NITRO, PCNA_NITRO, PCF_NITRO                            )  
!#############################################################
!
!!****  *INIT_VEG_PGD_n_n* - routine to initialize ISBA
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
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM,       ONLY : LCPL_ARP
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,           ONLY : XCPD, XLVTT, XLSTT
USE MODD_SNOW_PAR,       ONLY : XEMISSN
USE MODD_ISBA_PAR,       ONLY : XTAU_ICE
!
USE MODD_SGH_PAR,        ONLY : XICE_DEPH_MAX
!
USE MODI_SURF_PATCH
USE MODI_GET_1D_MASK
USE MODI_CO2_INIT_n
USE MODI_INIT_CHEMICAL_n
USE MODI_OPEN_NAMELIST
USE MODI_CH_INIT_DEP_ISBA_n
USE MODI_CLOSE_NAMELIST
USE MODI_INIT_DST
USE MODI_INIT_SLT
USE MODI_SUBSCALE_Z0EFF
!
USE MODE_SOIL
!
USE MODI_HEATCAPZ
USE MODI_THRMCONDZ
USE MODI_ABOR1_SFX
USE MODI_DIF_LAYER
USE MODI_DRY_WET_SOIL_ALBEDOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=6), INTENT(IN)  :: HSURF     ! Type of surface
INTEGER, INTENT(IN)  :: KLUOUT
!
INTEGER, INTENT(IN)  :: KI
INTEGER, INTENT(IN)  :: KPATCH
INTEGER, INTENT(IN)  :: KGROUND_LAYER
INTEGER, INTENT(IN)  :: KMONTH
!
REAL, DIMENSION(:,:), POINTER :: PVEGTYPE
REAL, DIMENSION(:,:), POINTER :: PPATCH
REAL, DIMENSION(:,:,:), POINTER :: PVEGTYPE_PATCH
INTEGER, DIMENSION(:), POINTER :: KSIZE_NATURE_P
INTEGER, DIMENSION(:,:), POINTER :: KR_NATURE_P
!
REAL, INTENT(IN) :: PRM_PATCH
!
LOGICAL, INTENT(IN) :: ODEEPSOIL
LOGICAL, INTENT(IN) :: OPHYSDOMC
REAL, DIMENSION(:), INTENT(IN) :: PTDEEP_CLI
REAL, DIMENSION(:), INTENT(IN) :: PGAMMAT_CLI
REAL, DIMENSION(:), POINTER :: PTDEEP
REAL, DIMENSION(:), POINTER :: PGAMMAT
!
LOGICAL, INTENT(IN) :: OAGRIP
REAL, DIMENSION(:), INTENT(IN) :: PTHRESHOLD
INTEGER, DIMENSION(:,:), POINTER :: KIRRINUM
LOGICAL, DIMENSION(:,:), POINTER :: OIRRIDAY
LOGICAL, DIMENSION(:,:), POINTER :: OIRRIGATE
REAL, DIMENSION(:,:), POINTER :: PTHRESHOLDSPT
!
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO
 CHARACTER(LEN=3), INTENT(IN) :: HINIT
LOGICAL, INTENT(IN) :: OTR_ML
INTEGER, INTENT(IN) :: KNBIOMASS
REAL, DIMENSION(:), INTENT(IN) :: PCO2
REAL, DIMENSION(:), INTENT(IN) :: PRHOA
REAL, DIMENSION(:), POINTER :: PABC
REAL, DIMENSION(:), POINTER :: PPOI
REAL, DIMENSION(:,:), INTENT(IN) :: PGMES
REAL, DIMENSION(:,:), INTENT(IN) :: PGC
REAL, DIMENSION(:,:), INTENT(IN):: PDMAX
REAL, DIMENSION(:,:), POINTER :: PANMAX
REAL, DIMENSION(:,:), POINTER :: PFZERO
REAL, DIMENSION(:,:), POINTER :: PEPSO
REAL, DIMENSION(:,:), POINTER :: PGAMM
REAL, DIMENSION(:,:), POINTER :: PQDGAMM
REAL, DIMENSION(:,:), POINTER :: PQDGMES
REAL, DIMENSION(:,:), POINTER :: PT1GMES
REAL, DIMENSION(:,:), POINTER :: PT2GMES
REAL, DIMENSION(:,:), POINTER :: PAMAX
REAL, DIMENSION(:,:), POINTER :: PQDAMAX
REAL, DIMENSION(:,:), POINTER :: PT1AMAX
REAL, DIMENSION(:,:), POINTER :: PT2AMAX
REAL, DIMENSION(:,:), POINTER :: PAH
REAL, DIMENSION(:,:), POINTER :: PBH
REAL, DIMENSION(:,:), POINTER :: PTAU_WOOD
REAL, DIMENSION(:,:,:), POINTER :: PINCREASE
REAL, DIMENSION(:,:,:), POINTER :: PTURNOVER
!
INTEGER,                          INTENT(IN) :: KSV      ! number of scalars
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN) :: HSV      ! name of all scalar variables
INTEGER,                         INTENT(OUT) :: KBEQ     ! number of chemical variables
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HSVO          ! name of scalar species without # and @
INTEGER,                         INTENT(OUT) :: KAEREQ  ! number of aerosol variables
INTEGER,                         INTENT(OUT) :: KSV_CHSBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_CHSEND  ! last  chemical var.
INTEGER,                         INTENT(OUT) :: KSV_AERBEG  ! first aerosol var.
INTEGER,                         INTENT(OUT) :: KSV_AEREND  ! last  aerosol var.
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HCH_NAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HAER_NAMES     
INTEGER,                         INTENT(OUT) :: KDSTEQ     ! number of chemical variables
INTEGER,                         INTENT(OUT) :: KSV_DSTBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_DSTEND  ! last  chemical var.
INTEGER,                         INTENT(OUT) :: KSLTEQ     ! number of chemical variables
INTEGER,                         INTENT(OUT) :: KSV_SLTBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_SLTEND  ! last  chemical var.
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HDSTNAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HSLTNAMES
!
 CHARACTER(LEN=28), INTENT(OUT) :: HCHEM_SURF_FILE
!
REAL, DIMENSION(:,:,:), POINTER :: PSFDST
REAL, DIMENSION(:,:,:), POINTER :: PSFDSTM
REAL, DIMENSION(:,:,:), POINTER :: PSFSLT
!
REAL, DIMENSION(:), INTENT(IN) :: PAOSIP
REAL, DIMENSION(:), INTENT(IN) :: PAOSIM
REAL, DIMENSION(:), INTENT(IN) :: PAOSJP
REAL, DIMENSION(:), INTENT(IN) :: PAOSJM
REAL, DIMENSION(:), INTENT(IN) :: PHO2IP
REAL, DIMENSION(:), INTENT(IN) :: PHO2IM
REAL, DIMENSION(:), INTENT(IN) :: PHO2JP
REAL, DIMENSION(:), INTENT(IN) :: PHO2JM
REAL, DIMENSION(:,:), INTENT(IN) :: PZ0
REAL, DIMENSION(:,:), POINTER :: PZ0EFFIP
REAL, DIMENSION(:,:), POINTER :: PZ0EFFIM
REAL, DIMENSION(:,:), POINTER :: PZ0EFFJP
REAL, DIMENSION(:,:), POINTER :: PZ0EFFJM
REAL, DIMENSION(:), POINTER :: PZ0REL
!
REAL, DIMENSION(:,:), INTENT(IN) :: PCLAY
REAL, DIMENSION(:,:), INTENT(IN) :: PSAND
 CHARACTER(LEN=4), INTENT(IN) :: HPEDOTF
REAL, DIMENSION(:,:,:), POINTER :: PCONDSAT
REAL, DIMENSION(:,:), POINTER :: PMPOTSAT
REAL, DIMENSION(:,:), POINTER :: PBCOEF
REAL, DIMENSION(:,:), POINTER :: PWWILT
REAL, DIMENSION(:,:), POINTER :: PWFC
REAL, DIMENSION(:,:), POINTER :: PWSAT
REAL, DIMENSION(:), POINTER :: PTAUICE
REAL, DIMENSION(:), POINTER :: PCGSAT
REAL, DIMENSION(:,:), POINTER :: PC1SAT
REAL, DIMENSION(:,:), POINTER :: PC2REF
REAL, DIMENSION(:,:,:), POINTER :: PC3
REAL, DIMENSION(:), POINTER :: PC4B
REAL, DIMENSION(:), POINTER :: PACOEF
REAL, DIMENSION(:), POINTER :: PPCOEF
REAL, DIMENSION(:,:), POINTER :: PC4REF
!
REAL, DIMENSION(:,:), POINTER :: PPCPS
REAL, DIMENSION(:,:), POINTER :: PPLVTT
REAL, DIMENSION(:,:), POINTER :: PPLSTT
!
 CHARACTER(LEN=4), INTENT(IN) :: HSCOND
 CHARACTER(LEN=3), INTENT(IN) :: HISBA
REAL, DIMENSION(:,:), POINTER :: PHCAPSOIL
REAL, DIMENSION(:,:), POINTER :: PCONDDRY
REAL, DIMENSION(:,:), POINTER :: PCONDSLD
 CHARACTER(LEN=3), INTENT(IN) :: HCPSURF
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDG
REAL, DIMENSION(:,:), INTENT(IN) :: PDROOT
REAL, DIMENSION(:,:), INTENT(IN) :: PDG2
REAL, DIMENSION(:,:,:), INTENT(IN) :: PROOTFRAC
REAL, DIMENSION(:,:), POINTER :: PRUNOFFD
REAL, DIMENSION(:,:,:), POINTER :: PDZG
REAL, DIMENSION(:,:,:), POINTER :: PDZDIF
REAL, DIMENSION(:,:,:), POINTER :: PSOILWGHT
INTEGER, DIMENSION(:,:), INTENT(IN) :: KWG_LAYER
INTEGER, INTENT(OUT) :: KLAYER_HORT
INTEGER, INTENT(OUT) :: KLAYER_DUN
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PD_ICE
REAL, DIMENSION(:,:), POINTER :: PKSAT_ICE
!
REAL, DIMENSION(:), POINTER :: PALBNIR_DRY
REAL, DIMENSION(:), POINTER :: PALBVIS_DRY
REAL, DIMENSION(:), POINTER :: PALBUV_DRY
REAL, DIMENSION(:), POINTER :: PALBNIR_WET
REAL, DIMENSION(:), POINTER :: PALBVIS_WET
REAL, DIMENSION(:), POINTER :: PALBUV_WET
!
REAL, DIMENSION(:,:), POINTER :: PBSLAI_NITRO
REAL, DIMENSIOn(:,:), INTENT(IN) :: PCE_NITRO
REAL, DIMENSIOn(:,:), INTENT(IN) :: PCNA_NITRO
REAL, DIMENSIOn(:,:), INTENT(IN) :: PCF_NITRO
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JPATCH  ! loop counter on tiles
INTEGER :: JILU,JP, JMAXLOC    ! loop increment
INTEGER :: JLAYER  ! loop counter on layers
!
INTEGER :: ICH     ! unit of input chemistry file
INTEGER :: ISIZE
!
REAL, DIMENSION(SIZE(PCO2))       :: ZCO2  ! CO2 concentration  (kg/kg)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_n',0,ZHOOK_HANDLE)
!
!*       2.4    Fraction of each tile
!               ---------------------
!
ALLOCATE(PPATCH         (KI,KPATCH))
ALLOCATE(PVEGTYPE_PATCH (KI,NVEGTYPE,KPATCH))
ALLOCATE(KSIZE_NATURE_P (KPATCH))
ALLOCATE(KR_NATURE_P    (KI,KPATCH))
!
 CALL SURF_PATCH(KPATCH,PVEGTYPE,PPATCH,PVEGTYPE_PATCH)
!
!*       2.5    Masks for tiles
!               ---------------
!
IF (PRM_PATCH/=0.) THEN
  !
  WRITE(KLUOUT,*) " REMOVE PATCH below 5 % add to dominant patch " 
  ! remove small fraction of PATCHES and add to MAIN PATCH
  DO JP = 1,KI
    !1) find most present patch maximum value 
    JMAXLOC = MAXVAL(MAXLOC(PPATCH(JP,:)))
    !2) FIND small value of cover 
    DO JPATCH = 1,KPATCH
      IF ( PPATCH(JP,JPATCH)<PRM_PATCH ) THEN
        PPATCH(JP,JMAXLOC) = PPATCH(JP,JMAXLOC) + PPATCH(JP,JPATCH)
        PPATCH(JP,JPATCH) = 0.0
       ENDIF
    ENDDO
  ENDDO
  !
ENDIF
!
DO JPATCH=1,KPATCH
  KSIZE_NATURE_P(JPATCH) = COUNT(PPATCH(:,JPATCH) > 0.0)
ENDDO
!
KR_NATURE_P(:,:) = 0
DO JPATCH=1,KPATCH
  CALL GET_1D_MASK(KSIZE_NATURE_P(JPATCH),KI,PPATCH(:,JPATCH),KR_NATURE_P(:KSIZE_NATURE_P(JPATCH),JPATCH))
ENDDO
!
!
!*       2.6    Miscellaneous fields for ISBA:
!               -----------------------------
!
!* default value for:
! lateral water flux, deep soil temperature climatology and its relaxation time-scale
!
ALLOCATE(PTDEEP (KI))
ALLOCATE(PGAMMAT(KI))
PTDEEP (:) = XUNDEF
PGAMMAT(:) = XUNDEF
!
IF (ODEEPSOIL) THEN
   DO JILU = 1, KI
      PTDEEP (JILU) = PTDEEP_CLI (KMONTH)
      PGAMMAT(JILU) = 1. / PGAMMAT_CLI(KMONTH)
   END DO
   !
   WRITE(KLUOUT,*)' LDEEPSOIL = ',ODEEPSOIL,' LPHYSDOMC = ',OPHYSDOMC
   WRITE(KLUOUT,*)' XTDEEP    = ',MINVAL(PTDEEP(:)),MAXVAL(PTDEEP(:))
   WRITE(KLUOUT,*)' XGAMMAT   = ',MINVAL(PGAMMAT(:)),MAXVAL(PGAMMAT(:))
ENDIF
!
!
!*       2.7    Irrigation
!               ----------
!
IF (OAGRIP) THEN
   ALLOCATE(KIRRINUM(KI,KPATCH))
   ALLOCATE(OIRRIDAY(KI,KPATCH))
   ALLOCATE(OIRRIGATE(KI,KPATCH))
   ALLOCATE(PTHRESHOLDSPT(KI,KPATCH))
   !
   KIRRINUM (:,:) = 1
   OIRRIDAY (:,:) = .FALSE.                          
   OIRRIGATE(:,:) = .FALSE.                          
   !
   DO JILU = 1, KI
      DO JPATCH = 1, KPATCH
         PTHRESHOLDSPT(JILU,JPATCH) = PTHRESHOLD(KIRRINUM(JILU,JPATCH))
      END DO
   END DO
ELSE
   ALLOCATE(KIRRINUM(0,0))
   ALLOCATE(OIRRIDAY(0,0))
   ALLOCATE(OIRRIGATE(0,0))
   ALLOCATE(PTHRESHOLDSPT(0,0))
ENDIF
!
!
!*       2.8    Additional fields for ISBA-AGS:
!               ------------------------------                        
!
IF(HPHOTO /= 'NON' .AND. HINIT == 'ALL') THEN
  IF (OTR_ML) THEN
    ISIZE = 10
  ELSE
    ISIZE = 3
  ENDIF
  ALLOCATE(PABC(ISIZE))
  ALLOCATE(PPOI(ISIZE))
  PABC(:) = 0.
  PPOI(:) = 0.          
  ZCO2(:) = PCO2(:) / PRHOA(:)
  ALLOCATE(PANMAX        (KI,KPATCH))
  ALLOCATE(PFZERO        (KI,KPATCH))
  ALLOCATE(PEPSO         (KI,KPATCH))
  ALLOCATE(PGAMM         (KI,KPATCH))
  ALLOCATE(PQDGAMM       (KI,KPATCH))
  ALLOCATE(PQDGMES       (KI,KPATCH))
  ALLOCATE(PT1GMES       (KI,KPATCH))
  ALLOCATE(PT2GMES       (KI,KPATCH))
  ALLOCATE(PAMAX         (KI,KPATCH))
  ALLOCATE(PQDAMAX       (KI,KPATCH))
  ALLOCATE(PT1AMAX       (KI,KPATCH))
  ALLOCATE(PT2AMAX       (KI,KPATCH))
  ALLOCATE(PAH           (KI,KPATCH))
  ALLOCATE(PBH           (KI,KPATCH))
  ALLOCATE(PTAU_WOOD     (KI,KPATCH))
  ALLOCATE(PINCREASE     (KI,KNBIOMASS,KPATCH))
  ALLOCATE(PTURNOVER     (KI,KNBIOMASS,KPATCH))
  CALL CO2_INIT_n(HPHOTO, KSIZE_NATURE_P, KR_NATURE_P, PVEGTYPE_PATCH, &
                  ZCO2, PGMES, PGC, PDMAX, PABC, PPOI, PANMAX, &
                  PFZERO, PEPSO, PGAMM, PQDGAMM, PQDGMES,      &
                  PT1GMES, PT2GMES, PAMAX, PQDAMAX,            &
                  PT1AMAX, PT2AMAX, PAH, PBH, PTAU_WOOD,       &
                  PINCREASE, PTURNOVER                         )
ELSE
  ALLOCATE(PABC(0))
  ALLOCATE(PPOI(0))
  ALLOCATE(PANMAX        (0,0))
  ALLOCATE(PFZERO        (0,0))
  ALLOCATE(PEPSO         (0,0))
  ALLOCATE(PGAMM         (0,0))
  ALLOCATE(PQDGAMM       (0,0))
  ALLOCATE(PQDGMES       (0,0))
  ALLOCATE(PT1GMES       (0,0))
  ALLOCATE(PT2GMES       (0,0))
  ALLOCATE(PAMAX         (0,0))
  ALLOCATE(PQDAMAX       (0,0))
  ALLOCATE(PT1AMAX       (0,0))
  ALLOCATE(PT2AMAX       (0,0))
  ALLOCATE(PAH           (0,0))
  ALLOCATE(PBH           (0,0))
  ALLOCATE(PTAU_WOOD     (0,0))
  ALLOCATE(PINCREASE     (0,0,0))
  ALLOCATE(PTURNOVER     (0,0,0))  
END IF
!
!-------------------------------------------------------------------------------
!
!        3.  Initialize Chemical Deposition
!            ------------------------------
!
!        3.1 Chemical gazes
!            --------------
!
    !* for the time being, chemistry on vegetation works only for
    ! ISBA on nature tile (not for gardens), because subroutine INIT_CHEMICAL_n
    ! contains explicitely modules from ISBAn. It should be cleaned in a future
    ! version.
IF (HSURF=='NATURE') THEN
 CALL INIT_CHEMICAL_n(KLUOUT, KSV, HSV, KBEQ, HSVO, KAEREQ,           &
                     KSV_CHSBEG, KSV_CHSEND, KSV_AERBEG, KSV_AEREND, &
                     HCH_NAMES, HAER_NAMES, KDSTEQ, KSV_DSTBEG,      &
                     KSV_DSTEND, KSLTEQ, KSV_SLTBEG, KSV_SLTEND,     &
                     HDSTNAMES=HDSTNAMES, HSLTNAMES=HSLTNAMES        )
END IF
!
IF (KSV /= 0) THEN
  !
  IF (HSURF=='NATURE' .AND. KBEQ > 0) THEN
    !* for the time being, chemistry deposition on vegetation works only for
    ! ISBA on nature tile (not for gardens), because subroutine CH_INIT_DEP_ISBA_n
    ! contains explicitely modules from ISBAn. It should be cleaned in a future
    ! version.
    CALL OPEN_NAMELIST(HPROGRAM, ICH, HFILE=HCHEM_SURF_FILE)
    CALL CH_INIT_DEP_ISBA_n(ICH, KLUOUT, HSVO, KI)
    CALL CLOSE_NAMELIST(HPROGRAM, ICH)
  END IF
  !
  IF (KDSTEQ >=1) THEN
    ALLOCATE (PSFDST (KI, KDSTEQ, KPATCH))  !Output array
    ALLOCATE (PSFDSTM(KI, KDSTEQ, KPATCH))  !Output array
    PSFDST(:,:,:)  = 0.
    PSFDSTM(:,:,:) = 0.     
    CALL INIT_DST(HPROGRAM,KSIZE_NATURE_P,KR_NATURE_P, &
                  KPATCH,PVEGTYPE_PATCH)    
  ELSE
    ALLOCATE(PSFDST (0,0,0))
    ALLOCATE(PSFDSTM(0,0,0))
  END IF
  !
  IF (KSLTEQ >=1) THEN
    ALLOCATE (PSFSLT(KI,KSLTEQ,KPATCH))  !Output array
    CALL INIT_SLT(HPROGRAM)   
  ELSE
    ALLOCATE(PSFSLT(0,0,0))
  END IF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Orographic roughness length
!               ---------------------------
!
ALLOCATE(PZ0EFFIP(KI,KPATCH))
ALLOCATE(PZ0EFFIM(KI,KPATCH))
ALLOCATE(PZ0EFFJP(KI,KPATCH))
ALLOCATE(PZ0EFFJM(KI,KPATCH))
ALLOCATE(PZ0REL  (KI))
!
IF (SIZE(PAOSIP)>0)                                      &
 CALL SUBSCALE_Z0EFF(PAOSIP,PAOSIM,PAOSJP,PAOSJM,         &
                    PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0,     &
                    PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM, &
                    PZ0REL                               ) 
!
!-------------------------------------------------------------------------------
!
!*       5.1     Soil hydraulic characteristics:
!                -------------------------------
!
ALLOCATE(PCONDSAT (KI,KGROUND_LAYER,KPATCH))
ALLOCATE(PMPOTSAT (KI,KGROUND_LAYER))
ALLOCATE(PBCOEF   (KI,KGROUND_LAYER))
ALLOCATE(PWWILT   (KI,KGROUND_LAYER)) ! wilting point
ALLOCATE(PWFC     (KI,KGROUND_LAYER)) ! field capacity
ALLOCATE(PWSAT    (KI,KGROUND_LAYER)) ! saturation
ALLOCATE(PTAUICE  (KI))
!        
DO JLAYER=1,KGROUND_LAYER
   PBCOEF  (:,JLAYER) = BCOEF_FUNC     (PCLAY(:,JLAYER),PSAND(:,JLAYER),HPEDOTF)
   PMPOTSAT(:,JLAYER) = MATPOTSAT_FUNC (PCLAY(:,JLAYER),PSAND(:,JLAYER),HPEDOTF)
   DO JPATCH=1,KPATCH
      PCONDSAT(:,JLAYER,JPATCH) = HYDCONDSAT_FUNC(PCLAY(:,JLAYER),PSAND(:,JLAYER),HPEDOTF)
   ENDDO   
   PWSAT (:,JLAYER) = WSAT_FUNC (PCLAY(:,JLAYER),PSAND(:,JLAYER),HPEDOTF)
   PWWILT(:,JLAYER) = WWILT_FUNC(PCLAY(:,JLAYER),PSAND(:,JLAYER),HPEDOTF)
END DO
!
IF (HISBA=='2-L' .OR. HISBA=='3-L') THEN
  !  field capacity at hydraulic conductivity = 0.1mm/day
  PWFC  (:,:) = WFC_FUNC  (PCLAY(:,:),PSAND(:,:),HPEDOTF)
ELSE IF (HISBA=='DIF') THEN
  !  field capacity at water potential = 0.33bar        
  PWFC  (:,:) = W33_FUNC  (PCLAY(:,:),PSAND(:,:),HPEDOTF)
END IF
!
PTAUICE(:) = XTAU_ICE
!
IF (HISBA=='2-L' .OR. HISBA=='3-L') THEN
  ALLOCATE(PCGSAT (KI))
  ALLOCATE(PC1SAT (KI,KPATCH))
  ALLOCATE(PC2REF (KI,KPATCH))
  ALLOCATE(PC3    (KI,2,KPATCH))
  ALLOCATE(PC4B   (KI))
  ALLOCATE(PACOEF (KI))
  ALLOCATE(PPCOEF (KI))
  ALLOCATE(PC4REF (KI,KPATCH))
  PCGSAT(:)  = CGSAT_FUNC(PCLAY(:,1),PSAND(:,1))
  PC4B(:)    = C4B_FUNC(PCLAY(:,1))
  !
  PACOEF(:)  = ACOEF_FUNC(PCLAY(:,1))
  PPCOEF(:)  = PCOEF_FUNC(PCLAY(:,1))
  !
  DO JPATCH=1,KPATCH
    PC1SAT(:,JPATCH) = C1SAT_FUNC(PCLAY(:,1))
    PC2REF(:,JPATCH) = C2REF_FUNC(PCLAY(:,1))         
    PC4REF(:,JPATCH) = C4REF_FUNC(PCLAY(:,1),PSAND(:,1),       &
                                  PDG(:,2,            JPATCH), &
                                  PDG(:,KGROUND_LAYER,JPATCH)  )
    PC3     (:,1,JPATCH) = C3_FUNC(PCLAY(:,1))
    PC3     (:,2,JPATCH) = C3_FUNC(PCLAY(:,2))

  END DO
  !
ELSE IF (HISBA=='DIF') THEN
  !
  ALLOCATE(PCGSAT (0))
  ALLOCATE(PC1SAT (0,0))
  ALLOCATE(PC2REF (0,0))
  ALLOCATE(PC3    (0,0,0))
  ALLOCATE(PC4B   (0))
  ALLOCATE(PC4REF (0,0))
  ALLOCATE(PACOEF (0))
  ALLOCATE(PPCOEF (0))
  !
END IF
!
!*       5.2     Soil thermal characteristics:
!               --------------------------------
!
ALLOCATE(PPCPS (KI,KPATCH))
ALLOCATE(PPLVTT(KI,KPATCH))
ALLOCATE(PPLSTT(KI,KPATCH))
PPCPS (:,:) = XCPD
PPLVTT(:,:) = XLVTT
PPLSTT(:,:) = XLSTT
!
!CSCOND used in soil.F90 and soildif.F90
IF (HSCOND=='PL98'.OR.HISBA=='DIF') THEN
  ALLOCATE(PHCAPSOIL(KI,KGROUND_LAYER))
  ! 
  CALL HEATCAPZ(PSAND,PWSAT,PHCAPSOIL)
  !
  IF (HSCOND=='PL98') THEN
    !
    ALLOCATE(PCONDDRY (KI,KGROUND_LAYER))
    ALLOCATE(PCONDSLD (KI,KGROUND_LAYER))
    ! 
    CALL THRMCONDZ(PSAND,PWSAT,PCONDDRY,PCONDSLD)
    !
  ELSE
    !
    ALLOCATE(PCONDDRY (0,0))
    ALLOCATE(PCONDSLD (0,0))
    !
  ENDIF
  !
ELSE
  ALLOCATE(PHCAPSOIL(0,0))
  ALLOCATE(PCONDDRY (0,0))
  ALLOCATE(PCONDSLD (0,0))
END IF
!
!-------------------------------------------------------------------------------
!CPSURF used in drag.F90
!CPL_ARP used in drag.F90 and e_budget.F90
IF(HCPSURF=='DRY'.AND.LCPL_ARP) THEN
  CALL ABOR1_SFX('CCPSURF=DRY must not be used with LCPL_ARP')
ENDIF
!
!*       6.1    Initialize hydrology
!               --------------------
!
ALLOCATE(PRUNOFFD (KI,KPATCH))
PRUNOFFD(:,:)=XUNDEF
!
IF (HISBA == 'DIF') THEN
!  
  ALLOCATE(PDZG       (KI,KGROUND_LAYER,KPATCH))
  ALLOCATE(PDZDIF     (KI,KGROUND_LAYER,KPATCH))
  ALLOCATE(PSOILWGHT  (KI,KGROUND_LAYER,KPATCH))
  CALL DIF_LAYER(KI, KGROUND_LAYER, KPATCH, KSIZE_NATURE_P, &
                 PPATCH, PDG, PDROOT, PDG2, PROOTFRAC,      &
                 KWG_LAYER, PDZG, PDZDIF, PSOILWGHT,        &
                 PRUNOFFD, KLAYER_HORT, KLAYER_DUN  )
!
ELSE
!    
  ALLOCATE(PDZG       (0,0,0))
  ALLOCATE(PDZDIF     (0,0,0))
  ALLOCATE(PSOILWGHT  (0,0,0))
  DO JPATCH=1,KPATCH
    WHERE(PPATCH(:,JPATCH)>0.0)
      PRUNOFFD(:,JPATCH) = PDG(:,2,JPATCH)
    ENDWHERE
  END DO
!  
  KLAYER_DUN=2
  KLAYER_HORT=2
!   
ENDIF
!
!Horton (also used by the flooding sheme)
! 
ALLOCATE(PKSAT_ICE(KI,KPATCH))
!
IF(HISBA/='DIF')THEN
  PD_ICE   (:,:)=MIN(PDG(:,2,:),PD_ICE(:,:))
  PD_ICE   (:,:)=MAX(XICE_DEPH_MAX,PD_ICE(:,:))
  PKSAT_ICE(:,:)=PCONDSAT(:,1,:)
ELSE
  PD_ICE   (:,:)=0.0
  PKSAT_ICE(:,:)=0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       8.     Physiographic Radiative fields:  
!               ------------------------------
!
!
!* dry and wet bare soil albedos
!
ALLOCATE(PALBNIR_DRY  (KI))
ALLOCATE(PALBVIS_DRY  (KI))
ALLOCATE(PALBUV_DRY   (KI))
ALLOCATE(PALBNIR_WET  (KI))
ALLOCATE(PALBVIS_WET  (KI))
ALLOCATE(PALBUV_WET   (KI))
!
 CALL DRY_WET_SOIL_ALBEDOS(PSAND(:,1),PCLAY(:,1),                 &
                          PVEGTYPE,                              &
                          PALBNIR_DRY,PALBVIS_DRY,PALBUV_DRY,    &
                          PALBNIR_WET,PALBVIS_WET,PALBUV_WET     )  
!
!
!
!*       2.9    Nitrogen version for isbaAgs
!               ------------------------------                        
!
IF (HPHOTO=='NIT' .OR. HPHOTO=='NCB') THEN
  ALLOCATE(PBSLAI_NITRO            (KI,KPATCH              ))
  WHERE ((PCE_NITRO (:,:)*PCNA_NITRO(:,:)+PCF_NITRO (:,:)) /= 0. )
      PBSLAI_NITRO(:,:) = 1. / (PCE_NITRO (:,:)*PCNA_NITRO(:,:)+PCF_NITRO (:,:))
  ELSEWHERE
      PBSLAI_NITRO(:,:) = XUNDEF
  ENDWHERE
ELSE
  ALLOCATE(PBSLAI_NITRO (0,0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VEG_PGD_n
