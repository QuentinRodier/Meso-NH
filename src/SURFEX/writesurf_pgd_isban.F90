!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_ISBA_n (DGU, &
                                        DTI, DTZ, IG, I, U, &
                                       HPROGRAM)
!     ################################################
!
!!****  *WRITESURF_PGD_ISBA_n* - writes ISBA physiographic fields
!!                        
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
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!!      B. Decharme  06/2009 : add topographic index statistics
!!      A.L. Gibelin 04/2009 : dimension NBIOMASS for ISBA-A-gs
!!      B. Decharme  07/2011 : delete argument HWRITE
!!      B. Decharme  07/2012 : files of data for permafrost area and for SOC top and sub soil
!!                   11/2013 : same for groundwater distribution
!!                   11/2014 : Write XSOILGRID as a series of real 
!!      P. Samuelsson 10/2014 : MEB
!!      M. Moge      02/2015 parallelization using WRITE_LCOVER
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODE_WRITE_SURF_COV, ONLY : WRITE_SURF_COV
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
USE MODI_WRITESURF_PGD_ISBA_PAR_n
USE MODI_WRITESURF_PGD_TSZ0_PAR_n
!
USE MODI_WRITE_SURF_FIELD2D
USE MODI_WRITE_SURF_FIELD3D
!
USE MODI_WRITE_LCOVER
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
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JJ, JLAYER
INTEGER :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!
!* soil scheme option
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Pedo-transfert function
!
YRECFM='PEDOTF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%CPEDOTF,IRESP,HCOMMENT=YCOMMENT)
!
!* type of photosynthesis
!
YRECFM='PHOTO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%CPHOTO,IRESP,HCOMMENT=YCOMMENT)
!
!* new radiative transfert
!
YRECFM='TR_ML'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LTR_ML,IRESP,HCOMMENT=YCOMMENT)
!
!* threshold to remove little fractions of patches
!
YRECFM='RM_PATCH'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XRM_PATCH,IRESP,HCOMMENT=YCOMMENT)

!* number of soil layers
!
YRECFM='GROUND_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(I%CISBA=='DIF') THEN
  DO JLAYER=1,I%NGROUND_LAYER
     WRITE(YLVL,'(I4)') JLAYER     
     YRECFM='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Depth of ISBA soilgrid layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XSOILGRID(JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO 
ENDIF
!
!* number of biomass pools
!
YRECFM='NBIOMASS'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%NNBIOMASS,IRESP,HCOMMENT=YCOMMENT)
!
!* number of tiles
!
YRECFM='PATCH_NUMBER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%NPATCH,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if fields are computed from ecoclimap or not
!
YRECFM='ECOCLIMAP'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LECOCLIMAP,IRESP,HCOMMENT=YCOMMENT)
!
!* logical vector indicating for which patches MEB should be applied
!
YRECFM='MEB_PATCH'
YCOMMENT='(LOGICAL LIST)'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LMEB_PATCH(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
ISIZE_LMEB_PATCH = COUNT(I%LMEB_PATCH(:))
!
IF (ISIZE_LMEB_PATCH>0)THEN
!
!* flag indicating if forcing is from observed measurements or not
!
   YRECFM='FORC_MEASURE'
   YCOMMENT=YRECFM
   CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LFORC_MEASURE,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if litter layer is used or not
!
   YRECFM='MEB_LITTER'
   YCOMMENT=YRECFM
   CALL WRITE_SURF(DGU, U, &
                HPROGRAM,YRECFM,I%LMEB_LITTER,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if ground resistance is used or not
!
   YRECFM='MEB_GNDRES'
   YCOMMENT=YRECFM
   CALL WRITE_SURF(DGU, U, &
                HPROGRAM,YRECFM,I%LMEB_GNDRES,IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
 CALL WRITE_LCOVER(DGU,U,HPROGRAM,I%LCOVER)
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF_COV(DGU, U, &
                     HPROGRAM,'COVER',I%XCOVER(:,:),I%LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(DGU, U, &
                 HPROGRAM,IG%CGRID,IG%XGRID_PAR,IG%XLAT,IG%XLON,IG%XMESH_SIZE,IRESP,I%XZ0EFFJPDIR)
!
!
!* clay fraction
!
!
YRECFM='CLAY'
YCOMMENT='X_Y_CLAY'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!* sand fraction
!
YRECFM='SAND'
YCOMMENT='X_Y_SAND'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!* soil organic carbon
!
YRECFM='SOCP'
YCOMMENT=''
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LSOCP,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LSOCP)THEN
  !        
  YCOMMENT='X_Y_SOC'
  YRECFM='SOC_TOP'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XSOC(:,1),IRESP,HCOMMENT=YCOMMENT)
  YRECFM='SOC_SUB'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XSOC(:,2),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!* permafrost distribution
!
YRECFM='PERMAFROST'
YCOMMENT=''
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LPERM,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LPERM)THEN
  YCOMMENT='X_Y_PERM'
  YRECFM='PERM'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XPERM(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!* groundwater distribution
!
YRECFM='GWKEY'
YCOMMENT=''
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LGW,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LGW)THEN
  YCOMMENT='X_Y_GWFRAC'
  YRECFM='GWFRAC'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XGW(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!SOILNOX
!
YRECFM='NO'
YCOMMENT=''
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LNOF,IRESP,HCOMMENT=YCOMMENT)
!
IF (I%LNOF) THEN
  !
  YRECFM='PH'
  YCOMMENT='X_Y_PH'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XPH(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FERT'
  YCOMMENT='X_Y_FERT'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XFERT(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!* subgrid-scale orography parameters to compute dynamical roughness length
!
YRECFM='AOSIP'
YCOMMENT='X_Y_AOSIP'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XAOSIP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSIM'
YCOMMENT='X_Y_AOSIM'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XAOSIM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJP'
YCOMMENT='X_Y_AOSJP'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XAOSJP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJM'
YCOMMENT='X_Y_AOSJM'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XAOSJM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2IP'
YCOMMENT='X_Y_HO2IP'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XHO2IP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2IM'
YCOMMENT='X_Y_HO2IM'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XHO2IM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JP'
YCOMMENT='X_Y_HO2JP'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XHO2JP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JM'
YCOMMENT='X_Y_HO2JM'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XHO2JM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SSO_SLOPE'
YCOMMENT='X_Y_SSO_SLOPE (-)'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XSSO_SLOPE,IRESP,HCOMMENT=YCOMMENT)
!
!* orographic runoff coefficient
!
YRECFM='RUNOFFB'
YCOMMENT='X_Y_RUNOFFB'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!
!* subgrid drainage coefficient
!
YRECFM='WDRAIN'
YCOMMENT='X_Y_WDRAIN'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
!* topographic index statistics
!
YRECFM='CTI'
YCOMMENT=''
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%LCTI,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LCTI)THEN
!
YRECFM='TI_MIN'
YCOMMENT='X_Y_TI_MIN'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XTI_MIN,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_MAX'
YCOMMENT='X_Y_TI_MAX'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XTI_MAX,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_MEAN'
YCOMMENT='X_Y_TI_MEAN'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XTI_MEAN,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_STD'
YCOMMENT='X_Y_TI_STD'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XTI_STD,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_SKEW'
YCOMMENT='X_Y_TI_SKEW'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XTI_SKEW,IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (I%LECOCLIMAP .AND. ASSOCIATED(I%XDG)) THEN
        ! note XDG is not associated only in the zoom_pgd step. This is not a
        ! problem because an initialization of the model is redone just after.
        ! In all other cases, the fileds are associated and initialized.
  !
  !* Soil depth for each patch
  !
  YRECFM='ECO_DG'
  YCOMMENT='soil depth from ecoclimap'
  YCOMMENTUNIT='M'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
        HPROGRAM,I%XDG,1,SIZE(I%XDG,2),YRECFM,YCOMMENT,YCOMMENTUNIT)
  !* Total soil depth for moisture
  !
  IF (I%CISBA=='DIF') THEN
    YRECFM='ECO_WG_L'
    YCOMMENT='Number of soil layers for moisture in ISBA-DIF'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(DGU, U, &
            HPROGRAM,FLOAT(I%NWG_LAYER(:,:)),YRECFM,YCOMMENT,YCOMMENTUNIT)
  END IF
END IF!
!-------------------------------------------------------------------------------
 CALL WRITESURF_PGD_ISBA_PAR_n(DGU, U, &
                               DTI, &
                               HPROGRAM)
IF (U%CNATURE=='TSZ0') CALL WRITESURF_PGD_TSZ0_PAR_n(DGU, U, &
                                                     DTZ, &
                                                     HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_ISBA_n
