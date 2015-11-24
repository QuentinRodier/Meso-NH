!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_ISBA_n(HPROGRAM,OLAND_USE)
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne  12/2004 : add type of photosynthesis
!!      B. Decharme      2008 : add XWDRAIN
!!      B. Decharme   06/2009 : add topographic index statistics
!!      A.L. Gibelin 04/2009 : dimension NBIOMASS for ISBA-A-gs
!!      B. Decharme  07/2012  : files of data for permafrost area and for SOC top and sub soil
!!      M. Moge      02/2015 READ_SURF
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SURF_ATM_n, ONLY : CNATURE, NSIZE_FULL
USE MODD_ISBA_n, ONLY : NPATCH, TTIME, XCOVER, XZS, CISBA, CPEDOTF,  &
                          CPHOTO, LTR_ML, CRUNOFF, XCLAY, XSAND,     &
                          XSOC, LSOCP, LNOF, XRM_PATCH,              &                          
                          NGROUND_LAYER, NNBIOMASS,                  &
                          XAOSIP, XAOSIM, XAOSJP, XAOSJM,            &
                          XHO2IP, XHO2IM, XHO2JP, XHO2JM,            &
                          XSSO_SLOPE, XSSO_STDEV, XRUNOFFB,          &
                          XZ0EFFJPDIR, LCOVER, LECOCLIMAP, LCTI,     &
                          XWDRAIN, XTI_MIN, XTI_MAX, XTI_MEAN,       &
                          XTI_STD, XTI_SKEW, XSOILGRID, XPH, XFERT,  &
                          LPERM, XPERM  
USE MODD_ISBA_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR, NDIM
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
USE MODD_GR_BIOG_n,   ONLY : XISOPOT, XMONOPOT
USE MODD_CH_ISBA_n,   ONLY : LCH_BIO_FLUX, LCH_NO_FLUX
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
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
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
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER :: ILU    ! expected physical size of full surface array
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: IVERSION, IBUGFIX   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_NATURE'
 CALL GET_TYPE_DIM_n('NATURE',NDIM)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Dimension initializations:
!               -------------------------
!
!* soil scheme
!
YRECFM='ISBA'
 CALL READ_SURF(HPROGRAM,YRECFM,CISBA,IRESP)
!
IF (IVERSION>=7) THEN
  !
  !* Pedo-transfert function
  !
  YRECFM='PEDOTF'
  CALL READ_SURF(HPROGRAM,YRECFM,CPEDOTF,IRESP)
  !
ELSE
  CPEDOTF = 'CH78'
ENDIF
!
!* type of photosynthesis
!
YRECFM='PHOTO'
 CALL READ_SURF(HPROGRAM,YRECFM,CPHOTO,IRESP)
!
!* new radiative transfert
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  YRECFM='TR_ML'
  CALL READ_SURF(HPROGRAM,YRECFM,LTR_ML,IRESP)
  !
ELSE 
  LTR_ML = .FALSE.
ENDIF
!
!* threshold to remove little fractions of patches
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  !
  YRECFM='RM_PATCH'
  CALL READ_SURF(HPROGRAM,YRECFM,XRM_PATCH,IRESP)
  !
ELSE 
  XRM_PATCH = 0.0
ENDIF
!
!* number of soil layers
!
YRECFM='GROUND_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,NGROUND_LAYER,IRESP)
!
!* Reference grid for DIF
!
IF(CISBA=='DIF') THEN
  ALLOCATE(XSOILGRID(NGROUND_LAYER))
  XSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    CALL READ_SURF(HPROGRAM,YRECFM,XSOILGRID,IRESP,HDIR='-')
  ELSE
    XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
IF (IVERSION>=6) THEN
  YRECFM='NBIOMASS'
  CALL READ_SURF(HPROGRAM,YRECFM,NNBIOMASS,IRESP)
ELSE
  SELECT CASE (CPHOTO)
    CASE ('AGS','LAI','AST','LST')
      NNBIOMASS = 1
    CASE ('NIT')
      NNBIOMASS = 3
    CASE ('NCB')
      NNBIOMASS = 6
  END SELECT
ENDIF
!
!* number of tiles
!
YRECFM='PATCH_NUMBER'
 CALL READ_SURF(HPROGRAM,YRECFM,NPATCH,IRESP)
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!
!*       3.1    Cover classes :
!               -------------
!
ALLOCATE(LCOVER(JPCOVER))
 CALL READ_LCOVER(HPROGRAM,LCOVER)
!
ALLOCATE(XCOVER(NDIM,JPCOVER))
 CALL READ_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP,HDIR='H')
!
!*       3.2    Orography :
!               ---------
!
!
ALLOCATE(XZS(NDIM))
YRECFM='ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XZS(:),IRESP)
!
!
!* latitude, longitude, mesh size, and heading of JP axis (deg from N clockwise)
!
ALLOCATE(XLAT       (NDIM))
ALLOCATE(XLON       (NDIM))
ALLOCATE(XMESH_SIZE (NDIM))
ALLOCATE(XZ0EFFJPDIR(NDIM))
 CALL READ_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP,XZ0EFFJPDIR)
!
!* clay fraction : attention, seul un niveau est present dans le fichier
!* on rempli tout les niveaux de  XCLAY avec les valeurs du fichiers
!
ALLOCATE(XCLAY(NDIM,NGROUND_LAYER))
YRECFM='CLAY'
 CALL READ_SURF(HPROGRAM,YRECFM,XCLAY(:,1),IRESP)
DO JLAYER=2,NGROUND_LAYER
  XCLAY(:,JLAYER)=XCLAY(:,1)
END DO
!
!* sand fraction
!
ALLOCATE(XSAND(NDIM,NGROUND_LAYER))
YRECFM='SAND'
 CALL READ_SURF(HPROGRAM,YRECFM,XSAND(:,1),IRESP)
DO JLAYER=2,NGROUND_LAYER
  XSAND(:,JLAYER)=XSAND(:,1)
END DO
!
!* Soil organic carbon profile
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
   YRECFM='SOCP'
   CALL READ_SURF(HPROGRAM,YRECFM,LSOCP,IRESP)
ELSE
   LSOCP=.FALSE.
ENDIF
!
IF(LSOCP)THEN
!  
  ALLOCATE(XSOC (NDIM,NGROUND_LAYER))
!
  YRECFM='SOC_TOP'
  CALL READ_SURF(HPROGRAM,YRECFM,XSOC(:,1),IRESP)
  YRECFM='SOC_SUB'
  CALL READ_SURF(HPROGRAM,YRECFM,XSOC(:,2),IRESP)
!
  DO JLAYER=2,NGROUND_LAYER
    XSOC (:,JLAYER)=XSOC (:,2)
  END DO
!
ELSE
!  
  ALLOCATE(XSOC (0,1))
!
ENDIF
!
!* permafrost distribution
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
   YRECFM='PERMAFROST'
   CALL READ_SURF(HPROGRAM,YRECFM,LPERM,IRESP)
ELSE
   LPERM=.FALSE.
ENDIF
!
IF(LPERM)THEN
!  
  ALLOCATE(XPERM (NDIM))
!
  YRECFM='PERM'
  CALL READ_SURF(HPROGRAM,YRECFM,XPERM(:),IRESP)
!
ELSE
!  
  ALLOCATE(XPERM (0))
!
ENDIF
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='NO'
   CALL READ_SURF(HPROGRAM,YRECFM,LNOF,IRESP)
ELSE
   LNOF = .FALSE.
ENDIF
!
!SOILNOX
!
IF (LCH_NO_FLUX) THEN
  !
  IF (LNOF) THEN
    !
    ALLOCATE(XPH(NDIM))
    YRECFM='PH'
    CALL READ_SURF(HPROGRAM,YRECFM,XPH(:),IRESP)
    !
    ALLOCATE(XFERT(NDIM))
    YRECFM='FERT'
    CALL READ_SURF(HPROGRAM,YRECFM,XFERT(:),IRESP)
    !
  ELSE
    CALL ABOR1_SFX("READ_PGD_ISBAn: WITH LCH_NO_FLUX=T, PH AND FERT FIELDS ARE GIVEN AT PGD STEP")
  ENDIF
  !
ELSE
  ALLOCATE(XPH (0))
  ALLOCATE(XFERT(0))
END IF
!
!* subgrid-scale orography parameters to compute dynamical roughness length
!
ALLOCATE(XAOSIP(NDIM))
YRECFM='AOSIP'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSIP,IRESP)
!
ALLOCATE(XAOSIM(NDIM))
YRECFM='AOSIM'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSIM,IRESP)

ALLOCATE(XAOSJP(NDIM))
YRECFM='AOSJP'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSJP,IRESP)
!
ALLOCATE(XAOSJM(NDIM))
YRECFM='AOSJM'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSJM,IRESP)
!
ALLOCATE(XHO2IP(NDIM))
YRECFM='HO2IP'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2IP,IRESP)
!
ALLOCATE(XHO2IM(NDIM))
YRECFM='HO2IM'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2IM,IRESP)
!
ALLOCATE(XHO2JP(NDIM))
YRECFM='HO2JP'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2JP,IRESP)
!
ALLOCATE(XHO2JM(NDIM))
YRECFM='HO2JM'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2JM,IRESP)
!
!* orographic parameter to compute effective surface of energy exchanges
!
ALLOCATE(XSSO_SLOPE(NDIM))
YRECFM='SSO_SLOPE'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_SLOPE,IRESP)
!
!* orographic standard deviation for subgrid-scale orographic drag
!
ALLOCATE(XSSO_STDEV(NDIM))
YRECFM='SSO_STDEV'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_STDEV(:),IRESP)
!
!* orographic runoff coefficient
!
ALLOCATE(XRUNOFFB(NDIM))
YRECFM='RUNOFFB'
 CALL READ_SURF(HPROGRAM,YRECFM,XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(XWDRAIN(NDIM))
IF (IVERSION<=3) THEN
  XWDRAIN = 0.
ELSE
  YRECFM='WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,XWDRAIN,IRESP)
ENDIF
!
!* topographic index statistics
!
IF(CRUNOFF=='SGH ' .AND. IVERSION>=5) THEN 
!
  YRECFM='CTI'
  CALL READ_SURF(HPROGRAM,YRECFM,LCTI,IRESP)        
!
  IF (.NOT.LCTI) CALL ABOR1_SFX("READ_PGD_ISBA_n:WITH CRUNOFF=SGH, CTI MAPS MUST BE GIVEN TO PGD")
  !
  ALLOCATE(XTI_MIN(NDIM))
  ALLOCATE(XTI_MAX(NDIM))
  ALLOCATE(XTI_MEAN(NDIM))
  ALLOCATE(XTI_STD(NDIM))
  ALLOCATE(XTI_SKEW(NDIM))
!
  YRECFM='TI_MIN'
  CALL READ_SURF(HPROGRAM,YRECFM,XTI_MIN,IRESP)
!
  YRECFM='TI_MAX'
  CALL READ_SURF(HPROGRAM,YRECFM,XTI_MAX,IRESP)
!
  YRECFM='TI_MEAN'
  CALL READ_SURF(HPROGRAM,YRECFM,XTI_MEAN,IRESP)
!
  YRECFM='TI_STD'
  CALL READ_SURF(HPROGRAM,YRECFM,XTI_STD,IRESP)
!
  YRECFM='TI_SKEW'
  CALL READ_SURF(HPROGRAM,YRECFM,XTI_SKEW,IRESP)
!
ELSE
!
  ALLOCATE(XTI_MIN(0))
  ALLOCATE(XTI_MAX(0))
  ALLOCATE(XTI_MEAN(0))
  ALLOCATE(XTI_STD(0))
  ALLOCATE(XTI_SKEW(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* biogenic chemical emissions
!
IF (LCH_BIO_FLUX) THEN
  ALLOCATE(ZWORK(NSIZE_FULL,1))
  !
  CALL END_IO_SURF_n(HPROGRAM)
  CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','READ ')
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  ALLOCATE(IMASK(NDIM))
  ILU=0
  CALL GET_SURF_MASK_n('NATURE',NDIM,IMASK,ILU,ILUOUT)
  ALLOCATE(XISOPOT(NDIM))
  ALLOCATE(XMONOPOT(NDIM))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_ISOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),XISOPOT(:))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_MONOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),XMONOPOT(:))
  !
  CALL END_IO_SURF_n(HPROGRAM)
  CALL INIT_IO_SURF_n(HPROGRAM,'NATURE','ISBA  ','READ ')
  !
  DEALLOCATE(ZWORK)
ELSE
  ALLOCATE(XISOPOT (0))
  ALLOCATE(XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
 CALL READ_LECOCLIMAP(HPROGRAM,LECOCLIMAP)
!
 CALL READ_PGD_ISBA_PAR_n(HPROGRAM,NDIM,OLAND_USE)
IF (CNATURE == 'TSZ0') CALL READ_PGD_TSZ0_PAR_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_ISBA_n
