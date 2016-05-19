!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_INI_ELEC_n
!     ######################
!
INTERFACE
      SUBROUTINE INI_ELEC_n (KLUOUT, HELEC, HCLOUD, HLUOUT, HINIFILE, &
                             PTSTEP, PZZ,                             &
                             PDXX, PDYY, PDZZ, PDZX, PDZY             )
!
INTEGER,           INTENT(IN) :: KLUOUT   ! Logical unit number for prints
CHARACTER (LEN=4), INTENT(IN) :: HELEC    ! atmospheric electricity scheme
CHARACTER (LEN=4), INTENT(IN) :: HCLOUD   ! microphysics scheme
CHARACTER (LEN=*), INTENT(IN) :: HLUOUT   ! name for output-listing
                                          !  of nested models
CHARACTER (LEN=*), INTENT(IN) :: HINIFILE ! name of the initial file
REAL,              INTENT(IN) :: PTSTEP   ! Time STEP  
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZZ     ! height z
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX    ! metric coefficient dxx
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDYY    ! metric coefficient dyy
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ    ! metric coefficient dzz
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZX    ! metric coefficient dzx
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZY    ! metric coefficient dzy
!
END SUBROUTINE INI_ELEC_n
END INTERFACE
END MODULE MODI_INI_ELEC_n
!
!     ################################################################
      SUBROUTINE INI_ELEC_n(KLUOUT, HELEC, HCLOUD, HLUOUT, HINIFILE, &
                            PTSTEP, PZZ,                             &
                            PDXX, PDYY, PDZZ, PDZX, PDZY             )
!     ################################################################
!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize the variables
!     of the atmospheric electricity scheme
!
!!    METHOD
!!    ------
!!      The initialization of the scheme is performed as follows :
!!   
!!    EXTERNAL
!!    --------
!!      CLEANLIST_ll : deaalocate a list
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!  	C. Barthe     * Laboratoire de l'Atmosphère et des Cyclones *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     09/11/09
!!      M. Chong     13/05/11  Add computation of specific parameters for solving
!!                             the electric field equation (elements of tri-diag
!!                             matrix) 
!!      J.-P. Pinty  13/04/12  Add elec_trid to initialise the tridiagonal syst.
!!      J.-P. Pinty  01/07/12  Add a non-homogeneous Neuman fair-weather 
!!                             boundary condition at the top
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODE_IO_ll
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_DIM_n, ONLY : NIMAX_ll, NJMAX_ll
USE MODD_ELEC_DESCR
USE MODD_ELEC_n, ONLY : XRHOM_E, XAF_E, XCF_E, XBFY_E
USE MODD_CONF_n, ONLY : NRR
USE MODD_PARAMETERS, ONLY : JPVEXT, JPHEXT
USE MODD_CST
USE MODD_CONF, ONLY : CEQNSYS
USE MODD_DYN
USE MODD_REF
USE MODD_TIME
USE MODD_GET_n, ONLY : CGETRCT,CGETRRT, CGETRST, CGETRGT, CGETRHT, CGETCLOUD, &
                       CGETSVM
USE MODD_PRECIP_n, ONLY : XINPRR, XACPRR, XINPRS, XACPRS, XINPRG, XACPRG, &
                          XINPRH, XACPRH, XINPRC, XACPRC, XINPRR3D, XEVAP3D
USE MODD_CLOUDPAR_n, ONLY : NSPLITR
USE MODD_REF_n, ONLY : XRHODJ, XTHVREF
USE MODD_GRID_n, ONLY : XMAP, XDXHAT, XDYHAT
USE MODD_DYN_n, ONLY : XRHOM, XTRIGSX, XTRIGSY, XAF, XCF, XBFY, XDXHATM, &
                       XDYHATM, NIFAXX, NIFAXY
USE MODD_LBC_n, ONLY : CLBCX, CLBCY

USE MODI_ELEC_TRID
USE MODI_INI_CLOUD
USE MODI_INI_PARAM_ELEC
USE MODI_INI_RAIN_ICE_ELEC
USE MODI_INI_FIELD_ELEC
USE MODI_INI_FLASH_GEOM_ELEC
USE MODI_READ_PRECIP_FIELD
!
!
IMPLICIT NONE
!
!*       0.1   declarations of dummy arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! Logical unit number for prints
CHARACTER (LEN=4),  INTENT(IN)  :: HELEC    ! atmospheric electricity scheme
CHARACTER (LEN=4),  INTENT(IN)  :: HCLOUD   ! microphysics scheme
CHARACTER (LEN=*),  INTENT(IN)  :: HLUOUT   ! name for output-listing
                                            !  of nested models
CHARACTER (LEN=*), INTENT(IN)  :: HINIFILE ! name of the initial file
REAL,              INTENT(IN)  :: PTSTEP   ! Time STEP 
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZZ     ! height z
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX    ! metric coefficient dxx
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDYY    ! metric coefficient dyy
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ    ! metric coefficient dzz
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZX    ! metric coefficient dzx
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZY    ! metric coefficient dzy
!
!
!*       0.2   declarations of local variables
!
INTEGER :: IRESP   ! Return code of FM routines 
INTEGER :: ILUOUT  ! Logical unit number of output-listing
!
INTEGER :: IIU     ! Upper dimension in x direction (local)
INTEGER :: IJU     ! Upper dimension in y direction (local)
INTEGER :: IKU     ! Upper dimension in z direction
INTEGER :: IKB, IKE
INTEGER :: JK      ! Loop vertical index
INTEGER :: IINFO_ll ! Return code of // routines
INTEGER :: IINTVL   ! Number of intervals to integrate the kernels
REAL    :: ZFDINFTY ! Factor used to define the "infinite" diameter
!
REAL :: ZRHO00     ! Surface reference air density
REAL :: ZDZMIN
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDZ    ! mesh size
CHARACTER (LEN=3) :: YEQNSYS
!
!
!-------------------------------------------------------------------------------
!
!*       0.    PROLOGUE
!              --------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IKU = SIZE(PZZ,3)
!
!-------------------------------------------------------------------------------
!
!*       1.    ALLOCATE Module MODD_PRECIP_n
!              -----------------------------
!
IF (HCLOUD(1:3) == 'ICE') THEN
  ALLOCATE( XINPRR(IIU,IJU) )
  ALLOCATE( XINPRR3D(IIU,IJU,IKU) )
  ALLOCATE( XEVAP3D(IIU,IJU,IKU) )
  ALLOCATE( XACPRR(IIU,IJU) )
  XINPRR(:,:) = 0.0
  XACPRR(:,:) = 0.0
  XINPRR3D(:,:,:) = 0.0
  XEVAP3D(:,:,:) = 0.0
  ALLOCATE( XINPRC(IIU,IJU) )
  ALLOCATE( XACPRC(IIU,IJU) )
  XINPRC(:,:) = 0.0
  XACPRC(:,:) = 0.0
  ALLOCATE( XINPRS(IIU,IJU) )
  ALLOCATE( XACPRS(IIU,IJU) )
  XINPRS(:,:) = 0.0
  XACPRS(:,:) = 0.0
  ALLOCATE( XINPRG(IIU,IJU) )
  ALLOCATE( XACPRG(IIU,IJU) )
  XINPRG(:,:) = 0.0
  XACPRG(:,:) = 0.0
END IF
!
IF (HCLOUD == 'ICE4') THEN
  ALLOCATE( XINPRH(IIU,IJU) )
  ALLOCATE( XACPRH(IIU,IJU) )
  XINPRH(:,:) = 0.0
  XACPRH(:,:) = 0.0
ELSE
  ALLOCATE( XINPRH(0,0) )
  ALLOCATE( XACPRH(0,0) )
END IF
!
IF(SIZE(XINPRR) == 0) RETURN
!
!
!-------------------------------------------------------------------------------
!
!*       2.    Initialize MODD_PRECIP_n variables
!              -----------------------------------
!
CALL READ_PRECIP_FIELD (HINIFILE, HLUOUT,                                    &
                        CGETRCT, CGETRRT, CGETRST, CGETRGT, CGETRHT,         &
                        XINPRC, XACPRC, XINPRR, XINPRR3D, XEVAP3D,           &
                        XACPRR, XINPRS, XACPRS, XINPRG, XACPRG, XINPRH, XACPRH)
!
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZE THE PARAMETERS 
!*             FOR THE MICROPHYSICS AND THE ELECTRICITY
!              ----------------------------------------
!
!*       3.1    Compute the minimun vertical mesh size
!
ALLOCATE( ZDZ(IIU,IJU,IKU) )
ZDZ(:,:,:) = 0.
!
IKB = 1 + JPVEXT
IKE = SIZE(PZZ,3) - JPVEXT
!
DO JK = IKB, IKE
  ZDZ(:,:,JK) = PZZ(:,:,JK+1) - PZZ(:,:,JK)
END DO
ZDZMIN = MIN_ll (ZDZ,IINFO_ll,1,1,IKB,NIMAX_ll+2*JPHEXT,NJMAX_ll+2*JPHEXT,IKE )
!
DEALLOCATE(ZDZ)
!
!
IF (HELEC(1:3) == 'ELE') THEN
!
!
!*       3.2    initialize the parameters for the mixed-phase microphysics 
!*              and the electrification
!
  CALL INI_RAIN_ICE_ELEC (KLUOUT, PTSTEP, ZDZMIN, NSPLITR, HCLOUD, &
                          IINTVL, ZFDINFTY)
!
!
!*       3.3    initialize the electrical parameters
!
  ZRHO00 = XP00 / (XRD * XTHVREFZ(IKB))
!
  CALL INI_PARAM_ELEC (HINIFILE, HLUOUT, CGETSVM, ZRHO00, NRR, IINTVL, &
                       ZFDINFTY, IIU, IJU, IKU)
!
!
!*       3.4    initialize the parameters for the electric field
!
  IF (LINDUCTIVE .OR. ((.NOT. LOCG) .AND. LELEC_FIELD)) THEN
    CALL INI_FIELD_ELEC (PDXX, PDYY, PDZZ, PDZX, PDZY, PZZ)
  END IF
!
!
!*       3.5    initialize the parameters for the lightning flashes
!
  IF (.NOT. LOCG) THEN
    IF (LFLASH_GEOM) THEN
      CALL INI_FLASH_GEOM_ELEC
    ELSE
      PRINT *,' INI_LIGHTNING_ELEC NOT YET DEVELOPPED'
      STOP
    END IF
  END IF
!
ELSE IF (HELEC /= 'NONE') THEN
  WRITE(ILUOUT,FMT=*) "INI_ELEC_n IS NOT YET DEVELOPPED FOR CELEC=",HELEC
  STOP
END IF
!
!*       3.6    initialize the parameters for the resolution of the electric field
!
YEQNSYS = CEQNSYS
CEQNSYS = 'LHE'
! Force any CEQNSYS (DUR, MAE, LHE) to LHE to obtain a unique set of coefficients
!    for the flat laplacian operator and Return to the original CEQNSYS

ALLOCATE (XRHOM_E(SIZE(XRHOM)))
ALLOCATE (XAF_E(SIZE(XAF)))
ALLOCATE (XCF_E(SIZE(XCF)))
ALLOCATE (XBFY_E(SIZE(XBFY,1),SIZE(XBFY,2),SIZE(XBFY,3)))
!
CALL ELEC_TRID (HLUOUT,CLBCX,CLBCY,                          &
           XMAP,XDXHAT,XDYHAT,XDXHATM,XDYHATM,XRHOM_E,XAF_E, &
           XCF_E,XTRIGSX,XTRIGSY,NIFAXX,NIFAXY,              &
           XRHODJ,XTHVREF,PZZ,XBFY_E,XEPOTFW_TOP)
!
CEQNSYS=YEQNSYS
!
!-------------------------------------------------------------------------------
!
! 
END SUBROUTINE INI_ELEC_n
