!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_ALM
!     #######################
!
INTERFACE
!
SUBROUTINE EOL_ALM(KTCOUNT, PTSTEP,        &
                   PDXX, PDYY, PDZZ,       &
                   PRHO_M,                 &
                   PUT_M, PVT_M, PWT_M,    &
                   PFX_RG, PFY_RG, PFZ_RG  )

!
INTEGER,                  INTENT(IN)    :: KTCOUNT           ! iteration count
REAL,                     INTENT(IN)    :: PTSTEP            ! timestep except
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ    ! mesh size
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHO_M            ! dry Density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT_M,PVT_M,PWT_M ! Wind speed at mass point
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFX_RG            ! Aerodynamic force ..
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFY_RG            ! .. cartesian mesh ..
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFZ_RG            ! .. global frame)
!
!
END SUBROUTINE EOL_ALM
!
END INTERFACE
!
END MODULE MODI_EOL_ALM
!
!     ###################################################################
        SUBROUTINE EOL_ALM(KTCOUNT, PTSTEP,        &
                           PDXX, PDYY, PDZZ,       &
                           PRHO_M,                 &
                           PUT_M, PVT_M, PWT_M,    &
                           PFX_RG, PFY_RG, PFZ_RG  )
!     ###################################################################
!
!!****  *MODI_EOL_ALM* -
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several methods are available. One of the models is the Actuator 
!!       Line Method (ALM). It allows to compute aerodynamic forces according 
!!       the wind speed and the caracteristics of the wind turbine. 
!! 
!!**  METHOD
!!    ------
!!      The ALM consists in modeling each blade by one line divided into blade
!!      element points (Sørensen and Shen, 2002). These points are applying 
!!      aerodynamic forces into the flow. 
!!      Each point carries a two-dimensional (2D) airfoil, and its characteris-
!!      tics, as lift and drag coefficients. Knowing these coefficients, and 
!!      the angle of attack, the lift and drag forces can be evaluated.
!!
!!    REFERENCE
!!    ---------
!!     PA. Joulin PhD Thesis. 2020.
!!      
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original     24/01/17
!!    Modification 14/10/20 (PA. Joulin) Updated for a main version
!  P. Wautelet 23/07/2021: replace non-standard FLOAT function by REAL function
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
! To work with wind turbines
USE MODD_EOL_ALM
USE MODD_EOL_KINE_ALM
!
USE MODD_EOL_SHARED_IO, ONLY: CINTERP
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTORQT, XPOWT
!
USE MODI_EOL_MATHS
USE MODI_EOL_READER,    ONLY: GET_AIRFOIL_ID
USE MODI_EOL_PRINTER,   ONLY: PRINT_TSPLIT
USE MODI_EOL_ERROR,     ONLY: EOL_WTCFL_ERROR
! Math
USE MODD_CST,           ONLY: XPI
! To know the grid 
USE MODD_GRID_n,        ONLY: XXHAT,XYHAT,XZS,XZZ
USE MODE_ll,            ONLY: GET_INDICE_ll
USE MODD_PARAMETERS,    ONLY: JPVEXT
! MPI stuffs
USE MODD_VAR_ll,        ONLY: NMNH_COMM_WORLD
USE MODD_PRECISION,     ONLY: MNHREAL_MPI
USE MODD_MPIF,          ONLY: MPI_SUM
USE MODE_SUM_ll,        ONLY: MIN_ll
USE MODD_VAR_ll,        ONLY: IP
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,                  INTENT(IN)    :: KTCOUNT           ! iteration count
REAL,                     INTENT(IN)    :: PTSTEP            ! timestep except
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ    ! mesh size
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHO_M            ! dry Density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT_M,PVT_M,PWT_M ! Wind speed at mass point
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFX_RG            ! Aerodynamic force ..
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFY_RG            ! .. cartesian mesh ..
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PFZ_RG            ! .. global frame)
!
!
!*       0.2   Declarations of local variables :
!
! Indicies Compteurs
INTEGER   :: IIB,IJB,IKB        ! Begin of a CPU domain
INTEGER   :: IIE,IJE,IKE        ! End of a CPU domain
INTEGER   :: IKU                ! Vertical size of the domain
INTEGER   :: JI, JJ, JK         ! Loop index
INTEGER   :: JROT, JBLA, JBELT  ! Rotor, blade, and blade element indicies
!
! Averages variables over all sub-timestep (if Time splitting) 
REAL, DIMENSION(TFARM%NNB_TURBINES,TTURBINE%NNB_BLADES,TBLADE%NNB_BLAELT)   :: ZAOA_ATS      ! Angle of attack of an element, hub frame [rad]
REAL, DIMENSION(TFARM%NNB_TURBINES,TTURBINE%NNB_BLADES,TBLADE%NNB_BLAELT)   :: ZFLIFT_ATS    ! Aerodynamic lift force, parallel to Urel [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TTURBINE%NNB_BLADES,TBLADE%NNB_BLAELT)   :: ZFDRAG_ATS    ! Aerodynamic drag force, perpendicular to Urel [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TTURBINE%NNB_BLADES,TBLADE%NNB_BLAELT,3) :: ZFAERO_RE_ATS ! Aerodynamic force (lift+drag) in RE [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TTURBINE%NNB_BLADES,TBLADE%NNB_BLAELT,3) :: ZFAERO_RG_ATS ! Aerodynamic force (lift+drag) in RG [N]


! -- Wind -- 
REAL                :: ZRHO_I                  ! Interpolated density [kg/m3]
REAL                :: ZUT_I                   ! Interpolated wind speed U (RG) [m/s] 
REAL                :: ZVT_I                   ! Interpolated wind speed V (RG) [m/s]
REAL                :: ZWT_I                   ! Interpolated wind speed W (RG) [m/s]
REAL, DIMENSION(3)  :: ZWIND_VEL_RG            ! Wind velocity in RG frame [m/s]
REAL, DIMENSION(3)  :: ZWIND_VEL_RE            ! Wind velocity in RE frame [m/s]
REAL, DIMENSION(3)  :: ZWINDREL_VEL_RE         ! Relative wind velocity in RE frame [m/s]
REAL                :: ZWINDREL_VEL            ! Norm of the relative wind velocity [m/s]
REAL, DIMENSION(SIZE(PUT_M,1),SIZE(PUT_M,2),SIZE(PUT_M,3)) :: ZZH ! True heigth to interpolate 8NB
!
! -- Wind turbine --
INTEGER             :: INB_WT, INB_B, INB_BELT ! Total numbers
REAL                :: ZRAD                    ! Blade radius [m]
INTEGER             :: IAID                    ! Airfoil index [-]
!
! -- Aero -- 
REAL                :: ZAOA                    ! Attack angle of an element [rad]
REAL                :: ZCDRAG                  ! Drag coefficient of an element []
REAL                :: ZCLIFT                  ! Lift coefficient of an element []
REAL                :: ZFDRAG                  ! Drag force of an element, parallel to Urel [N]
REAL                :: ZFLIFT                  ! Lift force of an element, perpendicular to Urel [N]
REAL, DIMENSION(3)  :: ZFAERO_RE               ! Aerodynamic force (lift+drag) in RE [N]
REAL, DIMENSION(3)  :: ZFAERO_RG               ! Aerodynamic force (lift+drag) in RG [N]
! Tip loss
REAL                :: ZFTIPL                  ! tip loss function
REAL                :: ZPHI                    ! angle twist+pitch+aa
!
! Thrust, Torque and Power
REAL, DIMENSION(3)  :: ZFAERO_RH               ! Aerodynamic force (lift+drag) in RH [N] (thrust/torque)
REAL, DIMENSION(3)  :: ZDIST_HBELT_RH          ! Distance between blade element and hub, in RH [m]
REAL, DIMENSION(3)  :: ZDIST_HBELT_RG          ! Distances between blade element and hub, in RG [m]
REAL, DIMENSION(3)  :: Z3D_TORQT               ! Full torque force (3D) of the wind turbine [N]
!
! -- Time spliting --
INTEGER             :: KTSUBCOUNT, INBSUBCOUNT ! sub iteration count
REAL                :: ZTSUBSTEP               ! sub timestep 
REAL                :: ZMAXTSTEP               ! Max value for timestep to respect WTCFL criteria
!
! -- Numerical --  
INTEGER             :: IINFO                   ! code info return
!
!
!*       0.3     Implicit arguments
!
! A. From MODD_EOL_ALM
!TYPE(FARM)                                :: TFARM
!TYPE(TURBINE)                             :: TTURBINE
!TYPE(BLADE)                               :: TBLADE
!TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE  :: TAIRFOIL 
!
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XELT_RAD      ! Blade elements radius [m]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XAOA_GLB      ! Angle of attack of an element [rad]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XFLIFT_GLB    ! Lift force, parallel to Urel [N]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XFDRAG_GLB    ! Drag force, perpendicular to Urel [N]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE     :: XFAERO_RE_GLB ! Aerodyn. force (lift+drag) in RE [N]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE     :: XFAERO_RG_GLB ! Aerodyn. force (lift+drag) in RG [N]
!
!INTEGER                                   :: NNB_BLAELT        ! Number of blade elements
!LOGICAL                                   :: LTIMESPLIT        ! Flag to apply Time splitting method
!LOGICAL                                   :: LTIPLOSSG         ! Flag to apply Glauert's tip loss correction
!LOGICAL                                   :: LTECOUTPTS        ! Flag to get Tecplot file output of element points
!
! B. From MODD_EOL_SHARED_IO:
! for namelist NAM_EOL_ALM
!CHARACTER(LEN=100)                        :: CFARM_CSVDATA     ! Farm file to read 
!CHARACTER(LEN=100)                        :: CTURBINE_CSVDATA  ! Turbine file to read  
!CHARACTER(LEN=100)                        :: CBLADE_CSVDATA    ! Blade file to read  
!CHARACTER(LEN=100)                        :: CAIRFOIL_CSVDATA  ! Airfoil file to read  
!CHARACTER(LEN=3)                          :: CINTERP           ! Interpolation method for wind speed
! for output
!REAL, DIMENSION(:),       ALLOCATABLE     :: XTHRUT        ! Thrust [N]
!REAL, DIMENSION(:),       ALLOCATABLE     :: XTORQT        ! Torque [Nm]
!REAL, DIMENSION(:),       ALLOCATABLE     :: XPOWT         ! Power [W]
!
!
!-------------------------------------------------------------------------------
!
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
!*       1.2     Some usefull integers
!
INB_WT   = TFARM%NNB_TURBINES
INB_B    = TTURBINE%NNB_BLADES
INB_BELT = TBLADE%NNB_BLAELT
!
!*       1.3     Vertical coordinate in case of interpolation
!
IF (CINTERP=='8NB') THEN
 DO JK=1,IKU-1
  ZZH(:,:,JK) = (0.5*(XZZ(:,:,JK)+XZZ(:,:,JK+1))-XZS(:,:))
 END DO
 ZZH(:,:,IKU) = 2*ZZH(:,:,IKU-1) - ZZH(:,:,IKU-2)
END IF
!
!*       1.4     Set to zeros at each MNH time steps
!
! Averaged variables (over time splitting)
ZAOA_ATS(:,:,:)             = 0.
ZFLIFT_ATS(:,:,:)           = 0.
ZFDRAG_ATS(:,:,:)           = 0.
ZFAERO_RE_ATS(:,:,:,:)      = 0.
ZFAERO_RG_ATS(:,:,:,:)      = 0.
!
! Global variables (seen by all CPU) 
XAOA_GLB(:,:,:)             = 0.
XFLIFT_GLB(:,:,:)           = 0.
XFDRAG_GLB(:,:,:)           = 0.
XFAERO_RE_GLB(:,:,:,:)      = 0.
XFAERO_RG_GLB(:,:,:,:)      = 0.
!
XTHRUT(:)                   = 0.
XTORQT(:)                   = 0.
!
!
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTES WTCFL CRITERIA
!               -----------------------
!
!*       2.1     Computing the highest timestep acceptable
ZMAXTSTEP = ABS( MIN(MIN_ll(PDXX(:,:,:),IINFO),&
                     MIN_ll(PDYY(:,:,:),IINFO),&
                     MIN_ll(PDZZ(:,:,:),IINFO))&
                /(MAXVAL(TFARM%XOMEGA(:))*TTURBINE%XR_MAX))
!
IF (.NOT.LTIMESPLIT) THEN
!*       2.2     Checking conditions
! If time step too high : abort
 IF (PTSTEP > ZMAXTSTEP) THEN
  CALL EOL_WTCFL_ERROR(ZMAXTSTEP)
! If time step ok, continue
 ELSE
  INBSUBCOUNT = 1
  ZTSUBSTEP   = PTSTEP/INBSUBCOUNT
 END IF
ELSE 
!*       2.3     Timesplitting : new sub-timestep
 INBSUBCOUNT  = INT(PTSTEP/ZMAXTSTEP) + 1
 ZTSUBSTEP    = PTSTEP/INBSUBCOUNT
 CALL PRINT_TSPLIT(INBSUBCOUNT, ZTSUBSTEP)
END IF
!
!*       2.4     Start looping over sub-timesteps
DO KTSUBCOUNT=1,INBSUBCOUNT
!
!
!-------------------------------------------------------------------------------
!
!*       3.     KINEMATICS COMPUTATIONS
!               -----------------------
!
 CALL EOL_KINE_ALM(KTCOUNT, KTSUBCOUNT, ZTSUBSTEP, PTSTEP)
!
!
!-------------------------------------------------------------------------------
!
!*       4.     COMPUTES AERODYNAMIC FORCES THAT ACTS ON THE BLADES DUE TO THE WIND
!               --------------------------------------------------------------
!
!*       4.1     Finding the position of wind turbines
!
! Loop over domain 
 DO JK=IKB,IKE
  DO JJ=IJB,IJE
   DO JI=IIB,IIE
    ! Loop over wind turbines
    DO JROT=1, INB_WT
     DO JBLA=1, INB_B
      DO JBELT=1, INB_BELT
       ! Position test
       IF (XPOS_ELT_RG(JROT,JBLA,JBELT,1) >= XXHAT(JI) .AND. &
           XPOS_ELT_RG(JROT,JBLA,JBELT,1) <  XXHAT(JI) + PDXX(JI,JJ,JK)) THEN
!
        IF (XPOS_ELT_RG(JROT,JBLA,JBELT,2) >= XYHAT(JJ) .AND. &
            XPOS_ELT_RG(JROT,JBLA,JBELT,2) <  XYHAT(JJ) + PDYY(JI,JJ,JK)) THEN
!
         IF (XPOS_ELT_RG(JROT,JBLA,JBELT,3) >= XZZ(JI,JJ,JK) .AND. &
             XPOS_ELT_RG(JROT,JBLA,JBELT,3) <  XZZ(JI,JJ,JK) + PDZZ(JI,JJ,JK)) THEN
!
!*       4.2     Extracting the wind
!
          SELECT CASE(CINTERP)
           CASE('CLS')
            ZUT_I  = PUT_M(JI,JJ,JK)
            ZVT_I  = PVT_M(JI,JJ,JK)
            ZWT_I  = PWT_M(JI,JJ,JK)
            ZRHO_I = PRHO_M(JI,JJ,JK)
           CASE('8NB')
            ZUT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JBLA,JBELT,:),&
                                   JI,JJ,JK,PUT_M,ZZH)
            ZVT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JBLA,JBELT,:),&
                                   JI,JJ,JK,PVT_M,ZZH)
            ZWT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JBLA,JBELT,:),&
                                   JI,JJ,JK,PWT_M,ZZH)
            ZRHO_I = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JBLA,JBELT,:),&
                                   JI,JJ,JK,PRHO_M,ZZH)
          END SELECT
          ZWIND_VEL_RG(1) = ZUT_I
          ZWIND_VEL_RG(2) = ZVT_I
          ZWIND_VEL_RG(3) = ZWT_I
!
!*       4.3     Calculating the wind in RE frame
!
          ZWIND_VEL_RE(:) = MATMUL(XMAT_RE_RG(JROT,JBLA,JBELT,:,:), ZWIND_VEL_RG(:))
!
!*       4.4     Calculating the relative wind speed in RE frame + norm
!
          ZWINDREL_VEL_RE(:) = ZWIND_VEL_RE(:) - XTVEL_ELT_RE(JROT,JBLA,JBELT,:)
          ZWINDREL_VEL       = NORM(ZWINDREL_VEL_RE)
!
!*       4.5     Calculating the angle of attack
!
          ZAOA   = ATAN2(ZWINDREL_VEL_RE(1), ZWINDREL_VEL_RE(2))      
!
!*       4.6     Getting aerodynamic coefficients from tabulated data
!
          ZRAD   = XELT_RAD(JROT,JBLA,JBELT)            ! Radius of the element
          IAID   = GET_AIRFOIL_ID(TTURBINE,TBLADE,TAIRFOIL,ZRAD)   ! ID of the airfoil   
          ZCLIFT = INTERP_SPLCUB(ZAOA*180/XPI,      &
                                 TAIRFOIL(IAID)%XAA,&
                                 TAIRFOIL(IAID)%XCL)
          ZCDRAG = INTERP_SPLCUB(ZAOA*180/XPI,      &
                                 TAIRFOIL(IAID)%XAA,&
                                 TAIRFOIL(IAID)%XCD)
!
!*       4.7     Tip loss correction (Glauert)
!
          IF (LTIPLOSSG) THEN
           ZPHI   = + ZAOA                                      &
                    - TFARM%XBLA_PITCH(JROT)                    &
                    - XTWIST_ELT(JROT,JBLA,JBELT)
           IF (ZPHI > 0.0) THEN
            ZFTIPL   = (2.0/XPI)*ACOS(MIN(                       &
                        1.0, EXP(-(TTURBINE%NNB_BLADES/2.0)      &
                       *(TTURBINE%XR_MAX-ZRAD)/(ZRAD*SIN(ZPHI)))))
           ELSE
            ZFTIPL = 1.0
           END IF
           ZCLIFT = ZFTIPL*ZCLIFT
           ZCDRAG = ZFTIPL*ZCDRAG
          END IF
!
!*       4.8     Computing aerodynamic forces in relative frame
!                  that act on blades (wind->blade)
          ZFLIFT = 0.5*ZRHO_I*XSURF_ELT(JROT,JBLA,JBELT)*ZCLIFT*ZWINDREL_VEL**2
          ZFDRAG = 0.5*ZRHO_I*XSURF_ELT(JROT,JBLA,JBELT)*ZCDRAG*ZWINDREL_VEL**2
!
!*       4.9     Evaluating the aerodynamiques forces in RE frame
!                  that act on blades (wind->blade)
          ZFAERO_RE(1) = SIN(ZAOA)*ZFDRAG + COS(ZAOA)*ZFLIFT
          ZFAERO_RE(2) = COS(ZAOA)*ZFDRAG - SIN(ZAOA)*ZFLIFT
          ZFAERO_RE(3) = .0 ! 2D flow around arifoil assumption
!
!*       4.10     Evaluating the aerodynamiques forces in RG frame
!                  that act on blades (wind->blade)
          ZFAERO_RG(:) = MATMUL(XMAT_RG_RE(JROT,JBLA,JBELT,:,:), ZFAERO_RE(:))
!
!*       4.11     Adding it to the cell of Meso-NH
          PFX_RG(JI,JJ,JK) = PFX_RG(JI,JJ,JK) + ZFAERO_RG(1) / REAL(INBSUBCOUNT)
          PFY_RG(JI,JJ,JK) = PFY_RG(JI,JJ,JK) + ZFAERO_RG(2) / REAL(INBSUBCOUNT)
          PFZ_RG(JI,JJ,JK) = PFZ_RG(JI,JJ,JK) + ZFAERO_RG(3) / REAL(INBSUBCOUNT)
!
!*       4.12     Storing mean values over one full MNH timestep
!               (all the sub-timesteps values are averaged)
          ZAOA_ATS(JROT,JBLA,JBELT)       = ZAOA_ATS(JROT,JBLA,JBELT)        &
                                          + ZAOA         / REAL(INBSUBCOUNT)
          ZFLIFT_ATS(JROT,JBLA,JBELT)     = ZFLIFT_ATS(JROT,JBLA,JBELT)      &
                                          + ZFLIFT       / REAL(INBSUBCOUNT)
          ZFDRAG_ATS(JROT,JBLA,JBELT)     = ZFDRAG_ATS(JROT,JBLA,JBELT)      &
                                          + ZFDRAG       / REAL(INBSUBCOUNT)
          ZFAERO_RE_ATS(JROT,JBLA,JBELT,:)= ZFAERO_RE_ATS(JROT,JBLA,JBELT,:) &
                                          + ZFAERO_RE(:) / REAL(INBSUBCOUNT)
          ZFAERO_RG_ATS(JROT,JBLA,JBELT,:)= ZFAERO_RG_ATS(JROT,JBLA,JBELT,:) &
                                          + ZFAERO_RG(:) / REAL(INBSUBCOUNT)
!
         ! End of position tests 
         END IF
        END IF
       END IF
      ! End of wind turbine loops  
      END DO
     END DO
    END DO
   ! End of domain loops
   END DO
  END DO
 END DO
! End of sub-time loop
END DO 
!
!
!*       4.13     Top and bottom conditions
PFX_RG(:,:,IKB-1) = PFX_RG(:,:,IKB)
PFX_RG(:,:,IKE+1) = PFX_RG(:,:,IKE)
!
PFY_RG(:,:,IKB-1) = PFY_RG(:,:,IKB)
PFY_RG(:,:,IKE+1) = PFY_RG(:,:,IKE)
!
PFZ_RG(:,:,IKB-1) = PFZ_RG(:,:,IKB)
PFZ_RG(:,:,IKE+1) = PFZ_RG(:,:,IKE)
!
!
!-------------------------------------------------------------------------------
!
!*       5.     SHARING THE DATAS OVER THE CPUS
!               -------------------------------
CALL MPI_ALLREDUCE(ZAOA_ATS,      XAOA_GLB,      SIZE(XAOA_GLB),     &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFLIFT_ATS,    XFLIFT_GLB,    SIZE(XFLIFT_GLB),   &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFDRAG_ATS,    XFDRAG_GLB,    SIZE(XFDRAG_GLB),   &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFAERO_RE_ATS, XFAERO_RE_GLB, SIZE(XFAERO_RE_GLB),&
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFAERO_RG_ATS, XFAERO_RG_GLB, SIZE(XFAERO_RG_GLB),&
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
!
!
!-------------------------------------------------------------------------------
!
!*       6.     COMPUTING THRUST, TORQUE AND POWER
!               ---------------------------------
!
IF(IP == 1) THEN
 DO JROT=1,TFARM%NNB_TURBINES
  DO JBLA=1, TTURBINE%NNB_BLADES
   DO JBELT=1, TBLADE%NNB_BLAELT
!
!*       6.1     Preliminaries
! Aerodynamic load (wind->blade) in RH
    ZFAERO_RH(:)      = MATMUL(XMAT_RH_RG(JROT,:,:), &
                        XFAERO_RG_GLB(JROT,JBLA,JBELT,:))
! Distance between element and hub in RG 
    ZDIST_HBELT_RG(:) = XPOS_ELT_RG(JROT,JBLA,JBELT,:) - XPOS_HUB_RG(JROT,:)
! Distance between element and hub in RH
    ZDIST_HBELT_RH(:) = MATMUL(XMAT_RH_RG(JROT,:,:),ZDIST_HBELT_RG(:))
!
!*       6.2     Thrust (wind->rotor): in RH
    XTHRUT(JROT)      = XTHRUT(JROT) + ZFAERO_RH(3)    ! Only Z component
!*       6.3     Torque (wind->rotor) in RH
    Z3D_TORQT         = CROSS(ZDIST_HBELT_RH(:),ZFAERO_RH(:))
    XTORQT(JROT)      = XTORQT(JROT) + Z3D_TORQT(3)    ! Only Z component 
   END DO
  END DO
!
!*       6.4     Power (wind->rotor)
  XPOWT(JROT) = XTORQT(JROT) * TFARM%XOMEGA(JROT)
 END DO
END IF
!
!
END SUBROUTINE EOL_ALM
