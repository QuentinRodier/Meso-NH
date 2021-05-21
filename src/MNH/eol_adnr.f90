!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #######################
       MODULE MODI_EOL_ADNR
!     #######################
!
INTERFACE
!
SUBROUTINE EOL_ADNR(PDXX, PDYY, PDZZ, &
                    PRHO_M, PUT_M,    &
                    PFX_RG            )
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ   ! mesh size
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHO_M           ! dry Density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT_M            ! Wind speed at mass point
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFX_RG           ! Aerodynamic force (cartesian mesh..
                                                            ! .. x axis, global
                                                            ! ..frame)
!
!
END SUBROUTINE EOL_ADNR
!
END INTERFACE
!
END MODULE MODI_EOL_ADNR
!
!     ###################################################################
        SUBROUTINE EOL_ADNR(PDXX, PDYY, PDZZ, &
                            PRHO_M, PUT_M,    &
                            PFX_RG            )
!     ###################################################################
!
!!****  *MODI_EOL_ADNR* -
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several methods are available. One of the models is the Non-
!!       Rotating Actuator Disk Non-Rotating model (ADNR). It allows to 
!!       compute aerodynamic forces according the wind speed and the 
!!       caracteristics of the wind turbine. 
!!
!!**  METHOD
!!    ------
!!       The actuator disc flow model, in this routine, is computed without
!!       rotation. It consists in applying a thrust force over the disc drawn 
!!       by the blades. This aerodynamic force acts against the wind to disturb
!!       the flow.
!!
!!    REFERENCE
!!    ---------
!!     PA. Joulin PhD Thesis. 2020.
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 01/2017
!!     Modification 19/10/20 (PA. Joulin) Updated for a main version
!!
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
! To work with ADNR
USE MODD_EOL_ADNR
!
USE MODD_EOL_SHARED_IO, ONLY: CINTERP
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT
USE MODI_EOL_MATHS,     ONLY: INTERP_LIN8NB
! To know the grid
USE MODD_GRID_n,        ONLY: XXHAT,XYHAT,XZS,XZZ
USE MODE_ll,            ONLY: GET_INDICE_ll
USE MODD_PARAMETERS,    ONLY: JPVEXT
! To play with MPI
USE MODD_VAR_ll,        ONLY: NMNH_COMM_WORLD, IP
use MODD_PRECISION,     only: MNHREAL_MPI
USE MODD_MPIF,          ONLY: MPI_SUM
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
! Meso-NH
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PDXX,PDYY,PDZZ  ! Mesh size
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PRHO_M          ! Dry Density * Jacobian
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PUT_M           ! Wind speed at mass point
! Wind turbine aerodynamic
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PFX_RG          ! Aerodynamic force (cartesian mesh, x axis, RG frame) [N]
!
!*       0.2   Declarations of local variables :
!
! Indicies
INTEGER          :: IIB,IJB,IKB    ! Begin of a CPU domain
INTEGER          :: IIE,IJE,IKE    ! End of a CPU domain
INTEGER          :: IKU            ! Vertical size of the domain
INTEGER          :: JI, JJ, JK     ! Loop index
INTEGER          :: JROT           ! Wind turbine index
!
! Wind
REAL             :: ZRHO_I                  ! Interpolated density [kg/m3]
REAL             :: ZUT_I                   ! Interpolated wind speed U (RG) [m/s] 
REAL, DIMENSION(SIZE(PUT_M,1),SIZE(PUT_M,2),SIZE(PUT_M,3)) :: ZZH ! True heigth to interpolate 8N
!
! Wind turbine
REAL, DIMENSION(TFARM%NNB_TURBINES) :: ZTHRUT    ! Thrust [N]
! Geometry
REAL               :: ZY_DIST      ! Distance Hub - Cell on Y [m]
REAL               :: ZZ_DIST      ! Distance Hub - Cell on Z [m]
REAL               :: ZR_DIST      ! Radial distance Hub - Cell [m] 
REAL, DIMENSION(3) :: ZPOS         ! Element position [m]
!
!Numerical
INTEGER             :: IINFO                   ! code info return
!
!*       0.3     Implicit arguments
!
! A. From MODD_EOL_ADNR:
!TYPE(FARM)                      :: TFARM
!TYPE(TURBINE)                   :: TTURBINE
!REAL, DIMENSION(:), ALLOCATABLE :: XA_INDU          ! Induction factor [-]
!REAL, DIMENSION(:), ALLOCATABLE :: XCT_D            ! Adapted thrust coef (for U_d) [-]
!
!
! B. From MODD_EOL_SHARED_IO:
! for NAM_EOL_ADNR:
!CHARACTER(LEN=100)              :: CFARM_CSVDATA    ! File to read, with farm data
!CHARACTER(LEN=100)              :: CTURBINE_CSVDATA ! File to read, turbine data
!CHARACTER(LEN=3)                :: CINTERP          ! Interpolation method for wind speed
! for outputs
!REAL, DIMENSION(:), ALLOCATABLE :: XTHRUT           ! Thrust [N]
!
!
!-------------------------------------------------------------------------------
!
!*      1.     INITIALIZATIONS
!              ---------------
!
!*       1.1     Subdomain (CPU) indices
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE) ! Get begin and end domain index (CPU)
IKU = SIZE(PUT_M,3)                 ! Top of the domain end index
IKB=1+JPVEXT                        ! Vertical begin index
IKE=IKU-JPVEXT                      ! Vertical end index
!
!*       1.2    Induction factor and adapted thrust coef (that will use U_disc)
!
DO JROT=1,TFARM%NNB_TURBINES 
 XA_INDU(JROT) = 0.5*(1-(1-TFARM%XCT_INF(JROT))**0.5)
 XCT_D(JROT)   = 4*XA_INDU(JROT)/(1-XA_INDU(JROT))
END DO
!
CALL PRINTMER_CPU1('ADNR : At ', 1.3)
!*       1.3    Inits
!
ZTHRUT(:)  = 0.
XTHRUT(:)  = 0.
!
CALL PRINTMER_CPU1('ADNR : At ', 1.4)
!*       1.4    Vertical coordinate in case of interpolation
!
IF (CINTERP=='8NB') THEN
 DO JK=1,IKU-1
  ZZH(:,:,JK) = (0.5*(XZZ(:,:,JK)+XZZ(:,:,JK+1))-XZS(:,:))
 END DO
 ZZH(:,:,IKU) = 2*ZZH(:,:,IKU-1) - ZZH(:,:,IKU-2)
END IF
!
!-------------------------------------------------------------------------------
!
!*      2.     COMPUTES THRUST FORCE THAT ACTS ON THE ROTOR DUE TO THE WIND
!              ------------------------------------------------------------
!
!*       2.1    Finding the position of wind turbines
!
! Loop over domain
DO JK=IKB,IKE
 DO JJ=IJB,IJE
  DO JI=IIB,IIE
   ! Loop over wind turbines
   DO JROT=1,TFARM%NNB_TURBINES 
    ! X axis position test :
    IF (TFARM%XPOS_X(JROT) >= XXHAT(JI) .AND. &
        TFARM%XPOS_X(JROT) <  XXHAT(JI) + PDXX(JI,JJ,JK)) THEN
     ! YZ plane distances calculations 
     ZY_DIST = TFARM%XPOS_Y(JROT)-XYHAT(JJ)
     ZZ_DIST = TTURBINE%XH_HEIGHT-(XZZ(JI,JJ,JK)-XZS(JI,JJ))
     ZR_DIST = (ZY_DIST**2 + ZZ_DIST**2 )**(1./2.)
     !
     ! Disc position test 
     IF (ZR_DIST <= TTURBINE%XR_MAX) THEN     
!
!*       2.2    Interpolating the wind
!
      ZPOS(1) = XXHAT(JI)
      ZPOS(2) = XYHAT(JJ)
      ZPOS(3) = XZZ(JI,JJ,JK)-XZS(JI,JJ)
      SELECT CASE(CINTERP)
       CASE('CLS')
        ZUT_I  = PUT_M(JI,JJ,JK)
        ZRHO_I = PRHO_M(JI,JJ,JK)
       CASE('8NB')
        ZUT_I  = INTERP_LIN8NB(ZPOS(:),JI,JJ,JK,PUT_M,ZZH)
        ZRHO_I = INTERP_LIN8NB(ZPOS(:),JI,JJ,JK,PRHO_M,ZZH)
      END SELECT
!
!*       2.3    Calculating the thrust of a cell wind->rotor
!
      PFX_RG(JI,JJ,JK) = PFX_RG(JI,JJ,JK)                   &
                         + 0.5*ZRHO_I*XCT_D(JROT)           &
                         *PDYY(JI,JJ,JK)*PDZZ(JI,JJ,JK)     &
                         *ZUT_I**2
!
!*       2.4    Calculating the thrust of the rotor wind->rotor 
!                in a pseudo hub coordinate system (-)
!
      ZTHRUT(JROT)     = ZTHRUT(JROT)                        & 
                         - 0.5*ZRHO_I*XCT_D(JROT)           &
                         *PDYY(JI,JJ,JK)*PDZZ(JI,JJ,JK)     &
                         *ZUT_I**2
!
     END IF ! Disc position test
    END IF ! X axis position test
   END DO ! WT loop
  END DO ! X domain loop
 END DO ! Y domain loop
END DO ! Z domain loop
!
!*       2.4    Bottom and top boundaries
!
PFX_RG(:,:,IKB-1) = PFX_RG(:,:,IKB)
PFX_RG(:,:,IKE+1) = PFX_RG(:,:,IKE)
!
!*       3.     SHARING THE DATAS OVER THE CPUS
!               -------------------------------
!
CALL MPI_ALLREDUCE(ZTHRUT, XTHRUT, SIZE(XTHRUT),     &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
!
!
!
END SUBROUTINE EOL_ADNR
