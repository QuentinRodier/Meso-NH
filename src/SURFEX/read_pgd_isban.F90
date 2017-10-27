!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_ISBA_n (CHI, DTCO, DTI, DTZ, DGU, GB, IG, I, &
                                  UG, U, SV,GCP, &
                                  HPROGRAM,OLAND_USE)
!     #########################################
!
!!****  *READ_PGD_ISBA_n* - routine to initialise ISBA physiographic variables 
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne  12/2004 : add type of photosynthesis
!!      B. Decharme      2008 : add XWDRAIN
!!      B. Decharme   06/2009 : add topographic index statistics
!!      A.L. Gibelin 04/2009 : dimension NBIOMASS for ISBA-A-gs
!!      B. Decharme  07/2012  : files of data for permafrost area and for SOC top and sub soil
!!                   11/2013  : same for groundwater distribution
!!                   11/2014  : Read XSOILGRID as a series of real 
!!      P. Samuelsson 10/2014 : MEB
!!      M. Moge      02/2015 READ_SURF // + MPPDB_CHECK
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SV_n, ONLY : SV_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
USE MODI_READ_SURF
USE MODI_READ_GRID
USE MODI_READ_LCOVER
USE MODI_READ_PGD_ISBA_PAR_n
USE MODI_READ_PGD_TSZ0_PAR_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
USE MODI_READ_LECOCLIMAP
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_LUOUT
USE MODI_PACK_SAME_RANK
USE MODI_GET_SURF_MASK_n
!
#ifdef MNH_PARALLEL
USE MODE_MPPDB
!
#endif
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SV_t), INTENT(INOUT) :: SV
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
LOGICAL,           INTENT(IN)  :: OLAND_USE ! 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: ILU    ! expected physical size of full surface array
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: IRESP  ! Error code after redding
INTEGER :: JLAYER ! loop counter on layers
INTEGER :: IVERSION, IBUGFIX   ! surface version
!
INTEGER :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_NATURE'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'NATURE',IG%NDIM)
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Dimension initializations:
!               -------------------------
!
!* soil scheme
!
YRECFM='ISBA'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%CISBA,IRESP)
!
IF (IVERSION>=7) THEN
  !
  !* Pedo-transfert function
  !
  YRECFM='PEDOTF'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%CPEDOTF,IRESP)
  !
ELSE
  I%CPEDOTF = 'CH78'
ENDIF
!
!* type of photosynthesis
!
YRECFM='PHOTO'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%CPHOTO,IRESP)
!
!* new radiative transfert
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  YRECFM='TR_ML'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LTR_ML,IRESP)
  !
ELSE 
  I%LTR_ML = .FALSE.
ENDIF
!
!* threshold to remove little fractions of patches
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  !
  YRECFM='RM_PATCH'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XRM_PATCH,IRESP)
  !
ELSE 
  I%XRM_PATCH = 0.0
ENDIF
!
!* number of soil layers
!
YRECFM='GROUND_LAYER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%NGROUND_LAYER,IRESP)
!
!* Reference grid for DIF
!
IF(I%CISBA=='DIF') THEN
  ALLOCATE(I%XSOILGRID(I%NGROUND_LAYER))
  I%XSOILGRID=XUNDEF
  IF (IVERSION>=8) THEN
     DO JLAYER=1,I%NGROUND_LAYER
        WRITE(YLVL,'(I4)') JLAYER
        YRECFM='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSOILGRID(JLAYER),IRESP)
     ENDDO    
  ELSEIF (IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSOILGRID,IRESP,HDIR='-')
  ELSE
    I%XSOILGRID(1:I%NGROUND_LAYER)=XOPTIMGRID(1:I%NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(I%XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
IF (IVERSION>=6) THEN
  YRECFM='NBIOMASS'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%NNBIOMASS,IRESP)
ELSE
  SELECT CASE (I%CPHOTO)
    CASE ('AGS','LAI','AST','LST')
      I%NNBIOMASS = 1
    CASE ('NIT')
      I%NNBIOMASS = 3
    CASE ('NCB')
      I%NNBIOMASS = 6
  END SELECT
ENDIF
!
!* number of tiles
!
YRECFM='PATCH_NUMBER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%NPATCH,IRESP)
!
!* logical vector indicating for which patches MEB should be applied
!
ALLOCATE(I%LMEB_PATCH(I%NPATCH))
!
IF (IVERSION>=8) THEN
!
   YRECFM='MEB_PATCH'
   CALL READ_SURF(HPROGRAM,YRECFM,I%LMEB_PATCH(:),IRESP,HDIR='-')
!
   ISIZE_LMEB_PATCH = COUNT(I%LMEB_PATCH(:))
!
   IF (ISIZE_LMEB_PATCH>0)THEN
      YRECFM='FORC_MEASURE'
      CALL READ_SURF(HPROGRAM,YRECFM,I%LFORC_MEASURE,IRESP)
      YRECFM='MEB_LITTER'
      CALL READ_SURF(HPROGRAM,YRECFM,I%LMEB_LITTER,IRESP)
      YRECFM='MEB_GNDRES'
      CALL READ_SURF(HPROGRAM,YRECFM,I%LMEB_GNDRES,IRESP)

   ELSE
      I%LFORC_MEASURE=.FALSE.
      I%LMEB_LITTER  =.FALSE.           
      I%LMEB_GNDRES  =.FALSE.           
   ENDIF
!
ELSE
   I%LMEB_PATCH(:)=.FALSE.
   I%LFORC_MEASURE=.FALSE.
   I%LMEB_LITTER  =.FALSE.
   I%LMEB_GNDRES  =.FALSE.
ENDIF
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!
!*       3.1    Cover classes :
!               -------------
!
ALLOCATE(I%LCOVER(JPCOVER))
 CALL READ_LCOVER(&
                  HPROGRAM,I%LCOVER)
!
ALLOCATE(I%XCOVER(IG%NDIM,COUNT(I%LCOVER)))
#ifdef MNH_PARALLEL
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',I%XCOVER(:,:),I%LCOVER,IRESP,HDIR='H')
 CALL MPPDB_CHECK_SURFEX3D(I%XCOVER,"READ_PGD_ISBA_n after READ_SURF:XCOVER",PRECISION,ILUOUT,'NATURE',SIZE(I%XCOVER,2))
#else
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',I%XCOVER(:,:),I%LCOVER,IRESP)
#endif
!
!*       3.2    Orography :
!               ---------
!
!
ALLOCATE(I%XZS(IG%NDIM))
YRECFM='ZS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XZS(:),IRESP)
!
!
!* latitude, longitude, mesh size, and heading of JP axis (deg from N clockwise)
!
ALLOCATE(IG%XLAT       (IG%NDIM))
ALLOCATE(IG%XLON       (IG%NDIM))
ALLOCATE(IG%XMESH_SIZE (IG%NDIM))
ALLOCATE(I%XZ0EFFJPDIR(IG%NDIM))
 CALL READ_GRID(&
                HPROGRAM,IG%CGRID,IG%XGRID_PAR,IG%XLAT,IG%XLON,IG%XMESH_SIZE,IRESP,I%XZ0EFFJPDIR)
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX2D(IG%XLAT,"READ_PGD_ISBA_n after READ_GRID:XLAT",PRECISION,ILUOUT,'NATURE')
 CALL MPPDB_CHECK_SURFEX2D(IG%XLON,"READ_PGD_ISBA_n after READ_GRID:XLON",PRECISION,ILUOUT,'NATURE')
 CALL MPPDB_CHECK_SURFEX2D(IG%XMESH_SIZE,"READ_PGD_ISBA_n after READ_GRID:XMESH_SIZE",PRECISION,ILUOUT,'NATURE')
#endif
!
!* clay fraction : attention, seul un niveau est present dans le fichier
!* on rempli tout les niveaux de  XCLAY avec les valeurs du fichiers
!
ALLOCATE(I%XCLAY(IG%NDIM,I%NGROUND_LAYER))
YRECFM='CLAY'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XCLAY(:,1),IRESP)
DO JLAYER=2,I%NGROUND_LAYER
  I%XCLAY(:,JLAYER)=I%XCLAY(:,1)
END DO
!
!* sand fraction
!
ALLOCATE(I%XSAND(IG%NDIM,I%NGROUND_LAYER))
YRECFM='SAND'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSAND(:,1),IRESP)
DO JLAYER=2,I%NGROUND_LAYER
  I%XSAND(:,JLAYER)=I%XSAND(:,1)
END DO
!
!* Soil organic carbon profile
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='SOCP'
   CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LSOCP,IRESP)
ELSE
   I%LSOCP=.FALSE.
ENDIF
!
IF(I%LSOCP)THEN
!  
  ALLOCATE(I%XSOC (IG%NDIM,I%NGROUND_LAYER))
!
  YRECFM='SOC_TOP'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSOC(:,1),IRESP)
  YRECFM='SOC_SUB'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSOC(:,2),IRESP)
!
  DO JLAYER=2,I%NGROUND_LAYER
    I%XSOC (:,JLAYER)=I%XSOC (:,2)
  END DO
!
ELSE
!  
  ALLOCATE(I%XSOC (0,1))
!
ENDIF
!
!* permafrost distribution
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='PERMAFROST'
   CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LPERM,IRESP)
ELSE
   I%LPERM=.FALSE.
ENDIF
!
IF(I%LPERM)THEN
!  
  ALLOCATE(I%XPERM (IG%NDIM))
!
  YRECFM='PERM'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XPERM(:),IRESP)
!
ELSE
!  
  ALLOCATE(I%XPERM (0))
!
ENDIF
!
!* groundwater distribution
!
IF (IVERSION>=8) THEN
   YRECFM='GWKEY'
   CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LGW,IRESP)
ELSE
   I%LGW=.FALSE.
ENDIF
!
IF(I%LGW)THEN
!  
  ALLOCATE(I%XGW (IG%NDIM))
!
  YRECFM='GWFRAC'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XGW(:),IRESP)
  WHERE(I%XGW(:)==XUNDEF)I%XGW(:)=0.0
!
ELSE
!  
  ALLOCATE(I%XGW (0))
!
ENDIF
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='NO'
   CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LNOF,IRESP)
ELSE
   I%LNOF = .FALSE.
ENDIF
!
!SOILNOX
!
IF (CHI%LCH_NO_FLUX) THEN
  !
  IF (I%LNOF) THEN
    !
    ALLOCATE(I%XPH(IG%NDIM))
    YRECFM='PH'
    CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XPH(:),IRESP)
    !
    ALLOCATE(I%XFERT(IG%NDIM))
    YRECFM='FERT'
    CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XFERT(:),IRESP)
    !
  ELSE
    CALL ABOR1_SFX("READ_PGD_ISBAn: WITH LCH_NO_FLUX=T, PH AND FERT FIELDS ARE GIVEN AT PGD STEP")
  ENDIF
  !
ELSE
  ALLOCATE(I%XPH (0))
  ALLOCATE(I%XFERT(0))
END IF
!
!* subgrid-scale orography parameters to compute dynamical roughness length
!
ALLOCATE(I%XAOSIP(IG%NDIM))
YRECFM='AOSIP'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XAOSIP,IRESP)
!
ALLOCATE(I%XAOSIM(IG%NDIM))
YRECFM='AOSIM'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XAOSIM,IRESP)

ALLOCATE(I%XAOSJP(IG%NDIM))
YRECFM='AOSJP'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XAOSJP,IRESP)
!
ALLOCATE(I%XAOSJM(IG%NDIM))
YRECFM='AOSJM'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XAOSJM,IRESP)
!
ALLOCATE(I%XHO2IP(IG%NDIM))
YRECFM='HO2IP'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XHO2IP,IRESP)
!
ALLOCATE(I%XHO2IM(IG%NDIM))
YRECFM='HO2IM'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XHO2IM,IRESP)
!
ALLOCATE(I%XHO2JP(IG%NDIM))
YRECFM='HO2JP'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XHO2JP,IRESP)
!
ALLOCATE(I%XHO2JM(IG%NDIM))
YRECFM='HO2JM'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XHO2JM,IRESP)
!
!* orographic parameter to compute effective surface of energy exchanges
!
ALLOCATE(I%XSSO_SLOPE(IG%NDIM))
YRECFM='SSO_SLOPE'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSSO_SLOPE,IRESP)
!
!* orographic standard deviation for subgrid-scale orographic drag
!
ALLOCATE(I%XSSO_STDEV(IG%NDIM))
YRECFM='SSO_STDEV'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XSSO_STDEV(:),IRESP)
!
!* orographic runoff coefficient
!
ALLOCATE(I%XRUNOFFB(IG%NDIM))
YRECFM='RUNOFFB'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(I%XWDRAIN(IG%NDIM))
IF (IVERSION<=3) THEN
  I%XWDRAIN = 0.
ELSE
  YRECFM='WDRAIN'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XWDRAIN,IRESP)
ENDIF
!
!* topographic index statistics
!
IF(I%CRUNOFF=='SGH ' .AND. IVERSION>=5) THEN 
!
  YRECFM='CTI'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LCTI,IRESP)        
!
  IF (.NOT.I%LCTI) CALL ABOR1_SFX("READ_PGD_ISBA_n:WITH CRUNOFF=SGH, CTI MAPS MUST BE GIVEN TO PGD")
  !
  ALLOCATE(I%XTI_MIN(IG%NDIM))
  ALLOCATE(I%XTI_MAX(IG%NDIM))
  ALLOCATE(I%XTI_MEAN(IG%NDIM))
  ALLOCATE(I%XTI_STD(IG%NDIM))
  ALLOCATE(I%XTI_SKEW(IG%NDIM))
!
  YRECFM='TI_MIN'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XTI_MIN,IRESP)
!
  YRECFM='TI_MAX'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XTI_MAX,IRESP)
!
  YRECFM='TI_MEAN'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XTI_MEAN,IRESP)
!
  YRECFM='TI_STD'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XTI_STD,IRESP)
!
  YRECFM='TI_SKEW'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%XTI_SKEW,IRESP)
!
ELSE
!
  ALLOCATE(I%XTI_MIN(0))
  ALLOCATE(I%XTI_MAX(0))
  ALLOCATE(I%XTI_MEAN(0))
  ALLOCATE(I%XTI_STD(0))
  ALLOCATE(I%XTI_SKEW(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* biogenic chemical emissions
!
IF (CHI%LCH_BIO_FLUX) THEN
  ALLOCATE(ZWORK(U%NSIZE_FULL,1))
  !
  CALL END_IO_SURF_n(HPROGRAM)
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                      HPROGRAM,'FULL  ','SURF  ','READ ')
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  ALLOCATE(IMASK(IG%NDIM))
  ILU=0
  CALL GET_SURF_MASK_n(DTCO, U, &
                       'NATURE',IG%NDIM,IMASK,ILU,ILUOUT)
  ALLOCATE(GB%XISOPOT(IG%NDIM))
  ALLOCATE(GB%XMONOPOT(IG%NDIM))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_ISOPOT'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),GB%XISOPOT(:))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_MONOPOT'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),GB%XMONOPOT(:))
  !
  CALL END_IO_SURF_n(HPROGRAM)
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                      HPROGRAM,'NATURE','ISBA  ','READ ')
  !
  DEALLOCATE(ZWORK)
ELSE
  ALLOCATE(GB%XISOPOT (0))
  ALLOCATE(GB%XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
 CALL READ_LECOCLIMAP(&
                      HPROGRAM,I%LECOCLIMAP)
!
 CALL READ_PGD_ISBA_PAR_n(DTCO, U, &
                          DTI, IG, I,GCP, &
                          HPROGRAM,IG%NDIM,OLAND_USE)
IF (U%CNATURE == 'TSZ0') CALL READ_PGD_TSZ0_PAR_n(&
                                                  DTZ, &
                                                  HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_ISBA_n
