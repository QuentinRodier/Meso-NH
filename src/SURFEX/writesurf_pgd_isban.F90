!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_ISBA_n(HPROGRAM)
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!!      B. Decharme  06/2009 : add topographic index statistics
!!      A.L. Gibelin 04/2009 : dimension NBIOMASS for ISBA-A-gs
!!      B. Decharme  07/2011 : delete argument HWRITE
!!      M. Moge      02/2015 parallelization using WRITE_LCOVER
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : CNATURE
USE MODD_ISBA_n, ONLY : NPATCH, NGROUND_LAYER, NNBIOMASS, CISBA,&
                        CPEDOTF, CPHOTO, LTR_ML, XRM_PATCH,     &
                        XCLAY, XSAND, XSOC,                     &                          
                        XAOSIP, XAOSIM, XAOSJP, XAOSJM,         &
                        XHO2IP, XHO2IM, XHO2JP, XHO2JM,         &
                        XSSO_SLOPE,                             &
                        XRUNOFFB, XWDRAIN,                      &
                        XTI_MIN, XTI_MAX, XTI_MEAN, XTI_STD,    &
                        XTI_SKEW, XZS,XCOVER,                   &
                        XZ0EFFJPDIR,                            &
                        LCOVER, LECOCLIMAP, LCTI, LSOCP, LNOF,  &
                        XSOILGRID, XPH, XFERT, LPERM, XPERM,    &
                        XDG, NWG_LAYER
!
USE MODD_ISBA_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JL     ! loop counter
!
!-------------------------------------------------------------------------------
!
!
!* soil scheme option
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Pedo-transfert function
!
YRECFM='PEDOTF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CPEDOTF,IRESP,HCOMMENT=YCOMMENT)
!
!* type of photosynthesis
!
YRECFM='PHOTO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CPHOTO,IRESP,HCOMMENT=YCOMMENT)
!
!* new radiative transfert
!
YRECFM='TR_ML'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LTR_ML,IRESP,HCOMMENT=YCOMMENT)
!
!* threshold to remove little fractions of patches
!
YRECFM='RM_PATCH'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRM_PATCH,IRESP,HCOMMENT=YCOMMENT)

!* number of soil layers
!
YRECFM='GROUND_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(CISBA=='DIF') THEN
  YRECFM='SOILGRID'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOILGRID,IRESP,HCOMMENT=YCOMMENT,HDIR='-')
ENDIF
!
!* number of biomass pools
!
YRECFM='NBIOMASS'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NNBIOMASS,IRESP,HCOMMENT=YCOMMENT)
!
!* number of tiles
!
YRECFM='PATCH_NUMBER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NPATCH,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if fields are computed from ecoclimap or not
!
YRECFM='ECOCLIMAP'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LECOCLIMAP,IRESP,HCOMMENT=YCOMMENT)
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
CALL WRITE_LCOVER(HPROGRAM,LCOVER)
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP,XZ0EFFJPDIR)
!
!
!* clay fraction
!
!
YRECFM='CLAY'
YCOMMENT='X_Y_CLAY'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!* sand fraction
!
YRECFM='SAND'
YCOMMENT='X_Y_SAND'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!* soil organic carbon
!
YRECFM='SOCP'
YCOMMENT=''
 CALL WRITE_SURF(HPROGRAM,YRECFM,LSOCP,IRESP,HCOMMENT=YCOMMENT)
!
IF(LSOCP)THEN
  !        
  YCOMMENT='X_Y_SOC'
  YRECFM='SOC_TOP'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOC(:,1),IRESP,HCOMMENT=YCOMMENT)
  YRECFM='SOC_SUB'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOC(:,2),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!* permafrost distribution
!
YRECFM='PERMAFROST'
YCOMMENT=''
 CALL WRITE_SURF(HPROGRAM,YRECFM,LPERM,IRESP,HCOMMENT=YCOMMENT)
!
IF(LPERM)THEN
  YCOMMENT='X_Y_PERM'
  YRECFM='PERM'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XPERM(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!SOILNOX
!
YRECFM='NO'
YCOMMENT=''
 CALL WRITE_SURF(HPROGRAM,YRECFM,LNOF,IRESP,HCOMMENT=YCOMMENT)
!
IF (LNOF) THEN
  !
  YRECFM='PH'
  YCOMMENT='X_Y_PH'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XPH(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FERT'
  YCOMMENT='X_Y_FERT'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XFERT(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!* subgrid-scale orography parameters to compute dynamical roughness length
!
YRECFM='AOSIP'
YCOMMENT='X_Y_AOSIP'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSIP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSIM'
YCOMMENT='X_Y_AOSIM'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSIM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJP'
YCOMMENT='X_Y_AOSJP'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSJP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='AOSJM'
YCOMMENT='X_Y_AOSJM'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XAOSJM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2IP'
YCOMMENT='X_Y_HO2IP'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2IP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2IM'
YCOMMENT='X_Y_HO2IM'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2IM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JP'
YCOMMENT='X_Y_HO2JP'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2JP,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HO2JM'
YCOMMENT='X_Y_HO2JM'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHO2JM,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SSO_SLOPE'
YCOMMENT='X_Y_SSO_SLOPE (-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSSO_SLOPE,IRESP,HCOMMENT=YCOMMENT)
!
!* orographic runoff coefficient
!
YRECFM='RUNOFFB'
YCOMMENT='X_Y_RUNOFFB'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!
!* subgrid drainage coefficient
!
YRECFM='WDRAIN'
YCOMMENT='X_Y_WDRAIN'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
!* topographic index statistics
!
YRECFM='CTI'
YCOMMENT=''
 CALL WRITE_SURF(HPROGRAM,YRECFM,LCTI,IRESP,HCOMMENT=YCOMMENT)
!
IF(LCTI)THEN
!
YRECFM='TI_MIN'
YCOMMENT='X_Y_TI_MIN'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_MIN,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_MAX'
YCOMMENT='X_Y_TI_MAX'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_MAX,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_MEAN'
YCOMMENT='X_Y_TI_MEAN'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_MEAN,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_STD'
YCOMMENT='X_Y_TI_STD'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_STD,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='TI_SKEW'
YCOMMENT='X_Y_TI_SKEW'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_SKEW,IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (LECOCLIMAP .AND. ASSOCIATED(XDG)) THEN
        ! note XDG is not associated only in the zoom_pgd step. This is not a
        ! problem because an initialization of the model is redone just after.
        ! In all other cases, the fileds are associated and initialized.
!
!* Soil depth for each patch
!
YRECFM='ECO_DG'
YCOMMENT='soil depth from ecoclimap'
YCOMMENTUNIT='M'
CALL WRITE_SURF_FIELD3D(HPROGRAM,XDG,1,SIZE(XDG,2),YRECFM,YCOMMENT,YCOMMENTUNIT)
!* Total soil depth for moisture
!
  IF (CISBA=='DIF') THEN
    YRECFM='ECO_WG_L'
    YCOMMENT='Number of soil layers for moisture in ISBA-DIF'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,FLOAT(NWG_LAYER(:,:)),YRECFM,YCOMMENT,YCOMMENTUNIT)
  END IF
END IF
!
!-------------------------------------------------------------------------------
 CALL WRITESURF_PGD_ISBA_PAR_n(HPROGRAM)
IF (CNATURE=='TSZ0') CALL WRITESURF_PGD_TSZ0_PAR_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_ISBA_n
