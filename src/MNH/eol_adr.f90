!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_ADR
!     #######################
!
INTERFACE
!
SUBROUTINE EOL_ADR(KTCOUNT, PTSTEP,        &
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
END SUBROUTINE EOL_ADR
!
END INTERFACE
!
END MODULE MODI_EOL_ADR
!
!     ###################################################################
        SUBROUTINE EOL_ADR(KTCOUNT, PTSTEP,        &
                           PDXX, PDYY, PDZZ,       &
                           PRHO_M,                 &
                           PUT_M, PVT_M, PWT_M,    &
                           PFX_RG, PFY_RG, PFZ_RG  )
!     ###################################################################
!
!!****  *MODI_EOL_ADR* -
!!
!!**  PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several methods are available. One of the models is the Actuator 
!!       Disc with Rotation (ADR). It allows to compute aerodynamic forces 
!!       according the wind speed and the caracteristics of the wind turbine. 
!! 
!!**  METHOD
!!    ------
!!      The ADR consists in modeling the rotor by a disc, drawn by the blades 
!!      (Mikkelsen 2003). The disc is discretized in a cyldrical frame. Each 
!!      element apply an aerodynamic force into the flow. Each element carries 
!!      a two-dimensional (2D) airfoil, and its characteristics, as lift and 
!!      drag coefficients. Knowing these coefficients, and the angle of attack, 
!!      the lift and drag forces can be evaluated.
!!
!!**  REFERENCE
!!    ---------
!!     PA. Joulin PhD Thesis. 2020.
!!     H. Toumi Master Thesis. 2022.
!!
!!**  AUTHOR
!!    ------
!!    H. Toumi                    *IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/22
!!    Modification 05/04/23 (PA. Joulin) Updated for a main version
!!
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
! To work with wind turbines
USE MODD_EOL_ADR
USE MODD_EOL_KINE_ADR
!
USE MODD_EOL_SHARED_IO, ONLY: CINTERP, LTECOUTPTS, LTIPLOSSG
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTORQT, XPOWT
USE MODD_EOL_SHARED_IO, ONLY: XELT_RAD
USE MODD_EOL_SHARED_IO, ONLY: XAOA_GLB, XFLIFT_GLB, XFDRAG_GLB, XFAERO_RG_GLB
!
USE MODI_EOL_MATHS
USE MODI_EOL_READER,    ONLY: GET_AIRFOIL_ID_ADR
USE MODI_EOL_PRINTER,   ONLY: PRINT_TSPLIT
USE MODI_EOL_DEBUGGER
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
!*       0.2   Declarations of local variables :
!
! Indicies Compteurs
INTEGER   :: IIB,IJB,IKB        ! Begin of a CPU domain
INTEGER   :: IIE,IJE,IKE        ! End of a CPU domain
INTEGER   :: IKU                ! Vertical size of the domain
INTEGER   :: JI, JJ, JK         ! Loop index
INTEGER   :: JROT, JAZI, JRAD   ! Rotor, azimutal, and radial element indicies
!
! Aggregated variables 
REAL, DIMENSION(TFARM%NNB_TURBINES,TBLADE%NNB_AZIELT,TBLADE%NNB_RADELT)   :: ZAOA_ELT      ! Angle of attack of an element, hub frame [rad]
REAL, DIMENSION(TFARM%NNB_TURBINES,TBLADE%NNB_AZIELT,TBLADE%NNB_RADELT)   :: ZFLIFT_ELT    ! Aerodynamic lift force, parallel to Urel [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TBLADE%NNB_AZIELT,TBLADE%NNB_RADELT)   :: ZFDRAG_ELT    ! Aerodynamic drag force, perpendicular to Urel [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TBLADE%NNB_AZIELT,TBLADE%NNB_RADELT,3) :: ZFAERO_RA_ELT ! Aerodynamic force (lift+drag) in RA [N]
REAL, DIMENSION(TFARM%NNB_TURBINES,TBLADE%NNB_AZIELT,TBLADE%NNB_RADELT,3) :: ZFAERO_RG_ELT ! Aerodynamic force (lift+drag) in RG [N]

!
! -- Wind -- 
REAL                :: ZRHO_I                  ! Interpolated density [kg/m3]
REAL                :: ZUT_I                   ! Interpolated wind speed U (RG) [m/s] 
REAL                :: ZVT_I                   ! Interpolated wind speed V (RG) [m/s]
REAL                :: ZWT_I                   ! Interpolated wind speed W (RG) [m/s]
REAL, DIMENSION(3)  :: ZWIND_VEL_RG            ! Wind velocity in RG frame [m/s]
REAL, DIMENSION(3)  :: ZWIND_VEL_RA            ! Wind velocity in RE frame [m/s]
REAL, DIMENSION(3)  :: ZWINDREL_VEL_RA         ! Relative wind velocity in RE frame [m/s]
REAL                :: ZWINDREL_VEL            ! Norm of the relative wind velocity [m/s]
REAL, DIMENSION(SIZE(PUT_M,1),SIZE(PUT_M,2),SIZE(PUT_M,3)) :: ZZH ! True heigth to interpolate 8NB
!
! -- Wind turbine --
INTEGER             :: INB_WT, INB_B           ! Total numbers of turbines and blades
INTEGER             :: INB_AELT, INB_RELT      ! Total numbers of azimutal and radial elt 
REAL                :: ZRAD                    ! Blade radius [m]
INTEGER             :: IAID                    ! Airfoil index [-]
!
! -- Aero -- 
REAL                :: ZAOA                    ! Attack angle of an element [rad]
REAL                :: ZCDRAG                  ! Drag coefficient of an element []
REAL                :: ZCLIFT                  ! Lift coefficient of an element []
REAL                :: ZFDRAG                  ! Drag force of an element, parallel to Urel [N]
REAL                :: ZFLIFT                  ! Lift force of an element, perpendicular to Urel [N]
REAL, DIMENSION(3)  :: ZFAERO_RA               ! Aerodynamic force (lift+drag) in RA [N]
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
! -- Numerical --  
INTEGER             :: IINFO                   ! code info return
!
!
!*       0.3     Implicit arguments
!
! A. From MODD_EOL_ADR
!TYPE(FARM)                                :: TFARM
!TYPE(TURBINE)                             :: TTURBINE
!TYPE(BLADE)                               :: TBLADE
!TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE  :: TAIRFOIL 
!
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XELT_AZI       ! Elements azimut [rad]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: XFAERO_RA_GLB  ! Aerodyn. force (lift+drag) in RA [N], global
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFAERO_BLA_BLEQ ! Blade Eq Aerodyn. force (lift+drag) in RA [N]
!REAL, DIMENSION(:,:),     ALLOCATABLE :: XAOA_BLEQ       ! Blade Eq. AoA 

! B. From MODD_EOL_SHARED_IO:
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XELT_RAD      ! Blade elements radius [m]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XAOA_GLB      ! Angle of attack of an element [rad]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XFLIFT_GLB    ! Lift force, parallel to Urel [N]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE     :: XFDRAG_GLB    ! Drag force, perpendicular to Urel [N]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE     :: XFAERO_RE_GLB ! Aerodyn. force (lift+drag) in RE [N]
!REAL, DIMENSION(:,:,:,:), ALLOCATABLE     :: XFAERO_RG_GLB ! Aerodyn. force (lift+drag) in RG [N]
!
! for namelist NAM_EOL_ALM
!CHARACTER(LEN=3)   :: CINTERP           ! Interpolation method for wind speed
!LOGICAL            :: LTIPLOSSG         ! Flag to apply Glauert's tip loss correction
!LOGICAL            :: LTECOUTPTS        ! Flag to get Tecplot file output of element points
!LOGICAL            :: LCSVOUTFRM        ! Flag to get CSV files output of frames
!
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
INB_AELT = TBLADE%NNB_AZIELT
INB_RELT = TBLADE%NNB_RADELT
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
ZAOA_ELT(:,:,:)             = 0.
ZFLIFT_ELT(:,:,:)           = 0.
ZFDRAG_ELT(:,:,:)           = 0.
ZFAERO_RA_ELT(:,:,:,:)      = 0.
ZFAERO_RG_ELT(:,:,:,:)      = 0.
! Global variables (seen by all CPU) 
XAOA_GLB(:,:,:)             = 0.
XFLIFT_GLB(:,:,:)           = 0.
XFDRAG_GLB(:,:,:)           = 0.
XFAERO_RA_GLB(:,:,:,:)      = 0.
XFAERO_RG_GLB(:,:,:,:)      = 0.
!
XFAERO_BLEQ_RA_GLB(:,:,:)   = 0.
XAOA_BLEQ_GLB(:,:)          = 0.
!
XTHRUT(:)                   = 0.
XTORQT(:)                   = 0.
!
!------------------------------------------------------------------------------
!
!*       2.     KINEMATICS COMPUTATIONS
!               -----------------------
!
 CALL EOL_KINE_ADR(KTCOUNT, PTSTEP)
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
     DO JAZI=1, INB_AELT
      DO JRAD=1, INB_RELT

       ! Position test
       IF (XPOS_ELT_RG(JROT,JAZI,JRAD,1) >= XXHAT(JI) .AND. &
           XPOS_ELT_RG(JROT,JAZI,JRAD,1) <  XXHAT(JI) + PDXX(JI,JJ,JK)) THEN
!
        IF (XPOS_ELT_RG(JROT,JAZI,JRAD,2) >= XYHAT(JJ) .AND. &
            XPOS_ELT_RG(JROT,JAZI,JRAD,2) <  XYHAT(JJ) + PDYY(JI,JJ,JK)) THEN
!
         IF (XPOS_ELT_RG(JROT,JAZI,JRAD,3) >= XZZ(JI,JJ,JK) .AND. &
             XPOS_ELT_RG(JROT,JAZI,JRAD,3) <  XZZ(JI,JJ,JK) + PDZZ(JI,JJ,JK)) THEN
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
            ZUT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JAZI,JRAD,:),&
                                   JI,JJ,JK,PUT_M,ZZH)
            ZVT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JAZI,JRAD,:),&
                                   JI,JJ,JK,PVT_M,ZZH)
            ZWT_I  = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JAZI,JRAD,:),&
                                   JI,JJ,JK,PWT_M,ZZH)
            ZRHO_I = INTERP_LIN8NB(XPOS_ELT_RG(JROT,JAZI,JRAD,:),&
                                   JI,JJ,JK,PRHO_M,ZZH)
          END SELECT
          ZWIND_VEL_RG(1) = ZUT_I
          ZWIND_VEL_RG(2) = ZVT_I
          ZWIND_VEL_RG(3) = ZWT_I
!
!          PRINT*, 'JROT= ', JROT, 'JAZI= ', JAZI, 'JRAD= ', JRAD
!          PRINT*, 'WIND_VEL_RG(1)= ', ZWIND_VEL_RG(1),'WIND_VEL_RG(2)= ', ZWIND_VEL_RG(2), 'WIND_VEL_RG(3)= ', ZWIND_VEL_RG(3)
!*       4.3     Calculating the wind in RA frame
!
          ZWIND_VEL_RA(:) = MATMUL(XMAT_RA_RG(JROT,JAZI,JRAD,:,:), ZWIND_VEL_RG(:))
!          PRINT*, 'WIND_VEL_RA(1)= ', ZWIND_VEL_RA(1),'WIND_VEL_RA(2)= ', ZWIND_VEL_RA(2), 'WIND_VEL_RA(3)= ', ZWIND_VEL_RA(3)
!
!*       4.4     Calculating the relative wind speed in RE frame + norm
!
          ZWINDREL_VEL_RA(:) = ZWIND_VEL_RA(:) - XTVEL_ELT_RA(JROT,JAZI,JRAD,:)
          ZWINDREL_VEL       = NORM(ZWINDREL_VEL_RA)
!
!          PRINT*, 'WINDREL_VEL_RA(1)= ', ZWINDREL_VEL_RA(1),'WINDREL_VEL_RA(2)= ', ZWINDREL_VEL_RA(2)
!          PRINT*, 'WINDREL_VEL_RA(3)= ', ZWINDREL_VEL_RA(3)
!          PRINT*, 'WINDREL_VEL ', ZWINDREL_VEL
!*       4.5     Calculating the angle of attack
!
          ZAOA   = ATAN2(-ZWINDREL_VEL_RA(3), ZWINDREL_VEL_RA(2))      
!
!          PRINT*, 'AOA= ', ZAOA
!*       4.6     Getting aerodynamic coefficients from tabulated data
!
          ZRAD   = XELT_RAD(JROT,JAZI,JRAD)                       ! Radius of the element
!          PRINT*, 'RAD= ', ZRAD  
          IAID   = GET_AIRFOIL_ID_ADR(TTURBINE,TBLADE,TAIRFOIL,ZRAD)   ! ID of the airfoil   
          ZCLIFT = INTERP_SPLCUB(ZAOA*180/XPI,      &
                                 TAIRFOIL(IAID)%XAA,&
                                 TAIRFOIL(IAID)%XCL)
          ZCDRAG = INTERP_SPLCUB(ZAOA*180/XPI,      &
                                 TAIRFOIL(IAID)%XAA,&
                                 TAIRFOIL(IAID)%XCD)

!          PRINT*, 'CLIFT= ', ZCLIFT  
!          PRINT*, 'CDRAG= ', ZCDRAG  

!
!*       4.7     Tip loss correction (Glauert)
!
          IF (LTIPLOSSG) THEN
           ZPHI   = + ZAOA                                      &
                    - TFARM%XBLA_PITCH(JROT)                    &
                    - XTWIST_ELT(JROT,JRAD)
           IF (ZPHI > 0.0) THEN
            ZFTIPL   = (2.0/XPI)*ACOS(MIN(                       &
                        1.0, EXP(-(TTURBINE%NNB_BLADES/2.0)      &
                       *(TTURBINE%XR_MAX-ZRAD)/(ZRAD*SIN(ZPHI)))))
           ! PRINT*, 'JROT= ', JROT, 'JAZI= ', JAZI, 'JROT= ', JRAD
           ! PRINT*, 'RAD= ', ZRAD, 'PHI= ', ZPHI, 'FTIPL=', ZFTIPL
           ! PRINT*, 'EXP= ', EXP(-(TTURBINE%NNB_BLADES/2.0)*(TTURBINE%XR_MAX-ZRAD)/(ZRAD*SIN(ZPHI))) 
           ! PRINT*, 'ACOS= ', ACOS(MIN(1.0, EXP(-(TTURBINE%NNB_BLADES/2.0)*(TTURBINE%XR_MAX-ZRAD)/(ZRAD*SIN(ZPHI)))))

           ELSE
            ZFTIPL = 1.0
           END IF
           ZCLIFT = ZFTIPL*ZCLIFT
           ZCDRAG = ZFTIPL*ZCDRAG
          END IF
!
!*       4.8     Computing aerodynamic forces in relative frame
!                  that act on blades (wind->blade)
          ZFLIFT = 0.5*ZRHO_I*XSURF_APP_ELT(JROT,JRAD)*ZCLIFT*ZWINDREL_VEL**2
          ZFDRAG = 0.5*ZRHO_I*XSURF_APP_ELT(JROT,JRAD)*ZCDRAG*ZWINDREL_VEL**2
!          PRINT*, 'LIFT= ', ZFLIFT, 'DRAG= ', ZFDRAG
!
!*       4.9     Evaluating the aerodynamiques forces in RE frame
!                  that act on blades (wind->blade)
          ZFAERO_RA(1) = .0 
          ZFAERO_RA(2) =   COS(ZAOA)*ZFDRAG - SIN(ZAOA)*ZFLIFT
          ZFAERO_RA(3) = - SIN(ZAOA)*ZFDRAG - COS(ZAOA)*ZFLIFT
!
!          PRINT*, 'FAERO_RA(1)= ', ZFAERO_RA(1), 'FAERO_RA(2)= ', ZFAERO_RA(2)
!*       4.10     Evaluating the aerodynamiques forces in RG frame
!                  that act on blades (wind->blade)
          ZFAERO_RG(:) = MATMUL(XMAT_RG_RA(JROT,JAZI,JRAD,:,:), ZFAERO_RA(:))
!
!*       4.11     Adding it to the cell of Meso-NH
          PFX_RG(JI,JJ,JK) = PFX_RG(JI,JJ,JK) + ZFAERO_RG(1) 
          PFY_RG(JI,JJ,JK) = PFY_RG(JI,JJ,JK) + ZFAERO_RG(2) 
          PFZ_RG(JI,JJ,JK) = PFZ_RG(JI,JJ,JK) + ZFAERO_RG(3) 
!          PRINT*, 'FX= ', PFX_RG(JI,JJ,JK), 'FY= ', PFY_RG(JI,JJ,JK), 'FZ= ', PFZ_RG(JI,JJ,JK) 
!

          ZAOA_ELT(JROT,JAZI,JRAD)    =   ZAOA
          ZFLIFT_ELT(JROT,JAZI,JRAD)   =   ZFLIFT
          ZFDRAG_ELT(JROT,JAZI,JRAD)   =   ZFDRAG
          ZFAERO_RA_ELT(JROT,JAZI,JRAD,:)   =  ZFAERO_RA(:) 
          ZFAERO_RG_ELT(JROT,JAZI,JRAD,:)   =  ZFAERO_RG(:) 


          !PRINT*, '---------------------------------------------------- '
          !IF (IP==1) THEN 

          !PRINT*, 'TCOUNT= ', KTCOUNT, 'STEP= ', PTSTEP
          !PRINT*, 'JROT= ', JROT, 'JAZI= ', JAZI, 'JRAD= ', JRAD
          !PRINT*, 'WIND_VEL_RG(1)= ', ZWIND_VEL_RG(1),'WIND_VEL_RG(2)= ', ZWIND_VEL_RG(2), 'WIND_VEL_RG(3)= ', ZWIND_VEL_RG(3)
          !PRINT*, 'WIND_VEL_RA(1)= ', ZWIND_VEL_RA(1),'WIND_VEL_RA(2)= ', ZWIND_VEL_RA(2), 'WIND_VEL_RA(3)= ', ZWIND_VEL_RA(3)
          !PRINT*, 'WINDREL_VEL_RA(1)= ', ZWINDREL_VEL_RA(1)
          !PRINT*, 'WINDREL_VEL_RA(2)= ', ZWINDREL_VEL_RA(2)
          !PRINT*, 'WINDREL_VEL_RA(3)= ', ZWINDREL_VEL_RA(3)
          !PRINT*, 'WINDREL_VEL ', ZWINDREL_VEL
          !PRINT*, 'AOA= ', ZAOA
          !PRINT*, 'RAD= ', ZRAD  
          !PRINT*, 'CLIFT= ', ZCLIFT, 'CDRAG= ', ZCDRAG  
          !PRINT*, 'LIFT= ', ZFLIFT, 'DRAG= ', ZFDRAG
          !PRINT*, 'FAERO_RA(1)= ', ZFAERO_RA(1), 'FAERO_RA(2)= ', ZFAERO_RA(2)
          !PRINT*, 'FX= ', PFX_RG(JI,JJ,JK), 'FY= ', PFY_RG(JI,JJ,JK), 'FZ= ', PFZ_RG(JI,JJ,JK) 
         ! END IF

          !PRINT*, 'FAERO(1)= ', ZFAERO(JRAD,1) , 'FAERO(1)= ', ZFAERO(JRAD,2), 'FAERO(1)= ', ZFAERO(JRAD,3)  
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
CALL MPI_ALLREDUCE(ZAOA_ELT,      XAOA_GLB,      SIZE(XAOA_GLB),     &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFLIFT_ELT,    XFLIFT_GLB,    SIZE(XFLIFT_GLB),   &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFDRAG_ELT,    XFDRAG_GLB,    SIZE(XFDRAG_GLB),   &
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFAERO_RA_ELT, XFAERO_RA_GLB, SIZE(XFAERO_RA_GLB),&
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
CALL MPI_ALLREDUCE(ZFAERO_RG_ELT, XFAERO_RG_GLB, SIZE(XFAERO_RG_GLB),&
        MNHREAL_MPI,MPI_SUM,NMNH_COMM_WORLD,IINFO)
!
!
!PRINT*, 'AOA_GLB= ', XAOA_GLB 
!PRINT*, 'FLIFT_GLB= ', XFLIFT_GLB 
!PRINT*, 'FDRAG_GLB= ', XFDRAG_GLB 
!PRINT*, 'FAERO_RA_GLB= ', XFAERO_RA_GLB 
!PRINT*, 'FAERO_RG_GLB= ', XFAERO_RG_GLB 
!-------------------------------------------------------------------------------
!
!*       6.     COMPUTING THRUST, TORQUE AND POWER
!               ---------------------------------
!
IF(IP == 1) THEN
 DO JROT=1,TFARM%NNB_TURBINES

  DO JRAD=1, TBLADE%NNB_RADELT
   DO JAZI=1, TBLADE%NNB_AZIELT
!
!*       6.1     Preliminaries
! Mean AOA on one blade 
    XAOA_BLEQ_GLB(JROT,JRAD)    = XAOA_BLEQ_GLB(JROT,JRAD) + XAOA_GLB(JROT,JAZI,JRAD)
! Aerodynamic load (wind->blade) in RA
    XFAERO_BLEQ_RA_GLB(JROT,JRAD,:) = XFAERO_BLEQ_RA_GLB(JROT,JRAD,:) &
                                    + XFAERO_RA_GLB(JROT,JAZI,JRAD,:)/INB_B
!   PRINT*, '---------------------------------------------------- '
!   PRINT*, 'JROT= ', JROT, 'JAZI= ', JAZI, 'JRAD= ', JRAD
!   PRINT*, 'FAERO_RA(1)= ', XFAERO_RA_BLA(JROT,JRAD,1),'FAERO_RA(2)= ', XFAERO_RA_BLA(JROT,JRAD,2)
!   PRINT*, 'FAERO_RA(3)= ', XFAERO_RA_BLA(JROT,JRAD,3)
!   PRINT*, 'AOA= ', XAOA_BLA(JROT,JRAD)
   END DO
  END DO


  DO JAZI=1, TBLADE%NNB_AZIELT
   DO JRAD=1, TBLADE%NNB_RADELT
! Aerodynamic load (wind->blade) in RH
    ZFAERO_RH(:)      = MATMUL(XMAT_RH_RG(JROT,:,:), &
                        XFAERO_RG_GLB(JROT,JAZI,JRAD,:))
! Distance between element and hub in RG 
    ZDIST_HBELT_RG(:) = XPOS_ELT_RG(JROT,JAZI,JRAD,:) - XPOS_HUB_RG(JROT,:)
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
END SUBROUTINE EOL_ADR
