!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GREENROOF_n(HPROGRAM,HPATCH)
!     ##################################
!
!!****  *READ_TEB_GREENROOF_n* - routine to initialise ISBA variables
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
!!    based on read_teb_greenroofn
!!
!!    AUTHOR
!!    ------
!!	C. de Munck & A. Lemonsu *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CO2V_PAR,          ONLY : XANFMINIT, XCONDCTMIN
USE MODD_TEB_VEG_n,         ONLY : CPHOTO, CRESPSL, NNBIOMASS
USE MODD_TEB_GREENROOF_n,   ONLY : NLAYER_GR,                          &
                                   XTG, XWG, XWGI, XWR, XLAI, TSNOW,   &
                                   XRESA, XANFM, XAN, XLE, XANDAY,     &
                                   XBSLAI, XBIOMASS, XRESP_BIOMASS
!                                
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_SNOW_PAR,          ONLY : XZ0SN
!
USE MODI_READ_SURF
!
USE MODI_READ_GR_SNOW
!
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current TEB patch identificator
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU                             ! 1D physical dimension
INTEGER           :: IRESP                           ! Error code after redding
INTEGER           :: IWORK                           ! Work integer
INTEGER           :: JLAYER, JNBIOMASS               ! loop counter on layers
 CHARACTER(LEN=30) :: YRECFM                          ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK             ! 2D array to write data in file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n('TOWN  ',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!
!* soil temperatures
!
IWORK = NLAYER_GR
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XTG(:,JLAYER) = ZWORK
END DO
!
!
!* soil liquid water content
!
DO JLAYER=1,NLAYER_GR
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XWG(:,JLAYER) = ZWORK
END DO
!
!* soil ice water content
!
DO JLAYER=1,NLAYER_GR
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XWGI(:,JLAYER) = ZWORK
END DO
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GR_WR'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XWR(:),IRESP)
!
!* Leaf Area Index
!
IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  YRECFM = HPATCH//'GR_LAI'
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,XLAI(:),IRESP)
END IF
!
!* snow mantel
!
 CALL READ_GR_SNOW(HPROGRAM,'GR',HPATCH,ILU,1,TSNOW  )! GROO:GreenROOf 
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!* aerodynamical resistance
!
YRECFM = HPATCH//'GR_RESA'
YRECFM=ADJUSTL(YRECFM)
XRESA(:) = 100.
 CALL READ_SURF(HPROGRAM,YRECFM,XRESA(:),IRESP)
!
XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (CPHOTO/='NON') THEN
  XAN(:)    = 0.
  XANDAY(:) = 0.
  XANFM(:)  = XANFMINIT
  XLE(:)    = 0.
END IF
!
IF (CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
  XBIOMASS(:,:)      = 0.
  XRESP_BIOMASS(:,:) = 0.
ELSEIF (CPHOTO=='LAI' .OR. CPHOTO=='LST') THEN
  XBIOMASS(:,1)      = XBSLAI(:) * XLAI(:)
  XRESP_BIOMASS(:,:) = 0.
ELSEIF (CPHOTO=='NIT') THEN
  XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    CALL READ_SURF(HPROGRAM,YRECFM,XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    CALL READ_SURF(HPROGRAM,YRECFM,XRESP_BIOMASS(:,JNBIOMASS),IRESP)
  END DO
ENDIF
!
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_GREENROOF_n
