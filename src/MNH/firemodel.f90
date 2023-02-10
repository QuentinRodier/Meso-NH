!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!###################
MODULE MODI_FIRE_MODEL
!###################

INTERFACE


SUBROUTINE FIRE_GRADPHI( PLSPHI, PGRADLSPHIX, PGRADLSPHIY )
  
  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:,:),      INTENT(IN)  ::  PLSPHI       ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PGRADLSPHIY  ! Grad of Phi on y direction

END SUBROUTINE FIRE_GRADPHI


SUBROUTINE FIRE_PROPAGATE( PLSPHI, PBMAP, PFMIGNITION, PFMWALKIG, PGRADLSPHIX, PGRADLSPHIY, PATMDT, PFIRERW )

  IMPLICIT NONE

  !! Level Set related
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)  ::  PLSPHI       ! Level Set function
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)  ::  PBMAP       ! Burning map
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)  ::  PFMIGNITION  ! Ignition map
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)  ::  PFMWALKIG    ! Walking ignition map

  !! Gradient of LS
  REAL,  DIMENSION(:,:,:),      INTENT(IN)    :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),      INTENT(IN)    :: PGRADLSPHIY  ! Grad of Phi on y direction

  !! Others
  REAL,  DIMENSION(:,:,:),      INTENT(IN)    :: PFIRERW      ! Rate of spread with wind
  REAL,                       INTENT(IN)    :: PATMDT       ! Atm time step

END SUBROUTINE FIRE_PROPAGATE


SUBROUTINE FIRE_NOWINDROS( PFIREFUELMAP, PFMR0, PFMRFA, PFMWF0, PFMR00, PFMFUELTYPE, &
                           PFIRETAU, PFLUXPARAMH, PFLUXPARAMW, PFMASE, PFMAWC )
  
  IMPLICIT NONE

  !! Fuel map
  REAL,  DIMENSION(:,:,:,:),    INTENT(IN)      ::  PFIREFUELMAP  ! Fuel map

  !! Rate of spread and factors
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMR0          ! Rate of spread without wind (R0)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMRFA         ! Radiant factor (A)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMWF0         ! Vertical flame velocity (v0)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMR00         ! Flame thickness speed factor (r0)
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)   ::  PFMFUELTYPE    ! Fuel type
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)     ::  PFIRETAU      ! Residence time
  REAL,  DIMENSION(:,:,:,:),    INTENT(OUT)     ::  PFLUXPARAMH    ! params sensible heat flux model
  REAL,  DIMENSION(:,:,:,:),    INTENT(OUT)     ::  PFLUXPARAMW    ! params latent heat flux model
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)     ::  PFMASE        ! Available sensible energy (J/m2)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)     ::  PFMAWC        ! Available water content (kg/m2)

END SUBROUTINE FIRE_NOWINDROS


SUBROUTINE FIRE_GETWIND( PUT, PVT, PWT, PGRADLSPHIX, PGRADLSPHIY, PFIREWIND, KTCOUNT, PATMDT, PFMGRADOROX, PFMGRADOROY )

  IMPLICIT NONE

  !! Atm wind
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PUT              ! U Wind on atm grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PVT              ! V Wind on atm grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PWT              ! W Wind on atm grid
  !! Grad of Phi
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIX     ! Grad Phi on x direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIY     ! Grad Phi on y direction
  !! Wind on fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)      ::  PFIREWIND       ! Wind Value on fire grid in -grad Phi direction
  !
  INTEGER,                  INTENT(IN)      ::  KTCOUNT          ! Iteration number
  REAL   ,                  INTENT(IN)      ::  PATMDT          ! Atmospheric Time step
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROX     ! Orography gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROY     ! Orography gradient on y direction (dz/dy) [m/m]

END SUBROUTINE FIRE_GETWIND


SUBROUTINE FIRE_RATEOFSPREAD( PFMFUELTYPE, PFMR0, PFMRFA, PFMWF0, PFMR00, PFIREWIND, &
                              PGRADLSPHIX, PGRADLSPHIY, PFMGRADOROX, PFMGRADOROY, PFIRERW )

  IMPLICIT NONE

  !! Wind on fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFIREWIND       ! Wind on fire grid on -grad(phi) direction

  !! Rate of spread and factors
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMR0            ! Rate of spread without wind (R0)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMRFA           ! Radiant factor (A)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMWF0           ! Vertical flame velocity (v0)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMR00           ! Flame thickness speed factor (r0)

  !! Fuel type
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMFUELTYPE      ! Fuel type

  !! Slope related
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIX     ! Grad Phi on x direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIY     ! Grad Phi on y direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROX     ! Orography gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROY     ! Orography gradient on y direction (dz/dy) [m/m]

  !! ROS
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)   ::  PFIRERW          ! Rate of spread with wind and slope (R)

END SUBROUTINE FIRE_RATEOFSPREAD


SUBROUTINE FIRE_HEATFLUXES( PLSPHI, PBMAP, PFIRETAU, PATMDT, PFLUXPARAMH, PFLUXPARAMW, PFMFLUXHDH, PFMFLUXHDW, PFMASE, PFMAWC )
  
  IMPLICIT NONE

  !! LS related
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PLSPHI         ! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PBMAP         ! Burning map

  !! Heat Flux param
  REAL,  DIMENSION(:,:,:,:), INTENT(INOUT) ::  PFLUXPARAMH   ! Sensible heat flux parameters
  REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) ::  PFLUXPARAMW   ! Latent heat flux parameters

  !! Heat Flux out
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMFLUXHDH    ! Surface sensible heat flux (W/m2), fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMFLUXHDW     ! Surface water flux (kg/m2/s), fire grid

  !! Available energy
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PFMASE        ! Available sensible energy (J/m2)
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PFMAWC        ! Available water content (kg/m2)

  !! others
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFIRETAU      ! Residence time and fluxes parameters map
  REAL,                     INTENT(IN)    ::  PATMDT        ! Atm time step

END SUBROUTINE FIRE_HEATFLUXES


SUBROUTINE FIRE_VERTICALFLUXDISTRIB( PFMFLUXHDH, PFMFLUXHDW, PRTHS, PRRS, PSFTS, PEXNREF, PRHODJ, PRT, PRHODREF )

  IMPLICIT NONE

  !! Heat Flux in
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFMFLUXHDH    ! Surface sensible heat flux (W/m2), fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFMFLUXHDW     ! Surface water flux (kg/m2/s), fire grid

  !! Sources
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PRTHS          ! Potential temperature increment (K kg/s)
  REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) ::  PRRS          ! Water content increment (kg/s)
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PSFTS          ! smoke flux (kg/kg/m2)

  !! Others
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PEXNREF        ! Exner function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PRHODJ        ! density times atm cell volume rho*J
  REAL,  DIMENSION(:,:,:,:),  INTENT(IN)    ::  PRT            ! Water content (kg/kg)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PRHODREF      ! reference profile of density

END SUBROUTINE FIRE_VERTICALFLUXDISTRIB


SUBROUTINE FIRE_READFUEL( TPFILE, PFIREFUELMAP, PFMIGNITION, PFMWALKIG )

  USE MODD_IO,   ONLY: TFILEDATA
  
  IMPLICIT NONE

  TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE       ! Synchronous output file
  REAL,  DIMENSION(:,:,:,:), INTENT(OUT)   :: PFIREFUELMAP ! Fuel map
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)   :: PFMIGNITION  ! Ignition map
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)   :: PFMWALKIG    ! Walking Ignition map

END SUBROUTINE FIRE_READFUEL


SUBROUTINE FIRE_READBMAP( TPFILE, PBMAP )

  USE MODD_IO,   ONLY: TFILEDATA

  IMPLICIT NONE

  TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE ! Synchronous output file
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)   :: PBMAP  ! Ignition map

END SUBROUTINE FIRE_READBMAP


SUBROUTINE FIRE_RK( PLSPHI, PLSPHI1, PGRADLSPHIX, PGRADLSPHIY, PFIRERW, PFIREDT )

  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PLSPHI       ! Level Set function at time t
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)    ::  PLSPHI1      ! Level Set function at time t+dtfire

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PGRADLSPHIY  ! Grad of Phi on y direction
  
  !! others
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PFIRERW      ! Rate of spread with wind
  REAL,                     INTENT(IN)    :: PFIREDT      ! Fire time step dtfire

END SUBROUTINE FIRE_RK


SUBROUTINE FIRE_WENO_1( PLSPHI2D, PGRADLSPHIX2D, PGRADLSPHIY2D, PGRADMASKX, PGRADMASKY )
  
  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:), INTENT(IN)  ::  PLSPHI2D       ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:),  INTENT(OUT) ::  PGRADLSPHIX2D ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:),  INTENT(OUT)  ::  PGRADLSPHIY2D  ! Grad of Phi on y direction

  !! others
  REAL,  DIMENSION(:,:),  INTENT(IN)  ::  PGRADMASKX    ! mask value x
  REAL,  DIMENSION(:,:),  INTENT(IN)  ::  PGRADMASKY    ! mask value y

END SUBROUTINE FIRE_WENO_1


SUBROUTINE FIRE_GRADMASK( PLSPHI2D, PGRADMASKX, PGRADMASKY, KMASKORDER )
  
  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:), INTENT(IN)  ::  PLSPHI2D     ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:),  INTENT(OUT)  ::  PGRADMASKX  ! mask value x
  REAL,  DIMENSION(:,:),  INTENT(OUT)  ::  PGRADMASKY  ! mask value y

  !! others
  INTEGER,              INTENT(IN)  ::  KMASKORDER  ! Difference order

END SUBROUTINE FIRE_GRADMASK


SUBROUTINE FIRE_WENO_3( PLSPHI2D, PGRADLSPHIX2D, PGRADLSPHIY2D, PGRADMASKX, PGRADMASKY )
  
  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:), INTENT(IN)  ::  PLSPHI2D       ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:), INTENT(OUT)  :: PGRADLSPHIX2D  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:), INTENT(OUT)  :: PGRADLSPHIY2D  ! Grad of Phi on y direction

  !! others
  REAL,  DIMENSION(:,:), INTENT(IN)  :: PGRADMASKX      ! mask value x
  REAL,  DIMENSION(:,:), INTENT(IN)  :: PGRADMASKY      ! mask value y

END SUBROUTINE FIRE_WENO_3


SUBROUTINE FIRE_LSDIFFU( PLSPHI, PLSDIFFUX, PLSDIFFUY )

  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:,:), INTENT(IN)  ::  PLSPHI     ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),  INTENT(OUT)  ::  PLSDIFFUX  ! Laplacian of Phi on x direction
  REAL,  DIMENSION(:,:,:),  INTENT(OUT)  ::  PLSDIFFUY  ! Laplacian of Phi on y direction

END SUBROUTINE FIRE_LSDIFFU


SUBROUTINE FIRE_ROSDIFFU( PFIRERW )

  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:,:), INTENT(INOUT)  ::  PFIRERW ! ROS

END SUBROUTINE FIRE_ROSDIFFU


SUBROUTINE FIRE_SUBGRIDSURFACE( PLSPHI2D, PSURFRATIO2D )
  
  IMPLICIT NONE

  REAL,  DIMENSION(:,:), INTENT(IN)  ::  PLSPHI2D       ! Level Set function in 2D array
  REAL,  DIMENSION(:,:), INTENT(OUT) ::  PSURFRATIO2D  ! Surface ratio in 2D array

END SUBROUTINE FIRE_SUBGRIDSURFACE


SUBROUTINE FIRE_QUANDRANTSURFACE( PPHI1, PPHI2, PPHI3, PPHI4, PSURFRATIO2D )
  
  IMPLICIT NONE

  !! LS value
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI1         ! Phi at south west point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI2         ! Phi at south east point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI3         ! Phi at north east point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI4         ! Phi at north west point

  !! bruning area 
  REAL,DIMENSION(:,:),    INTENT(INOUT) ::  PSURFRATIO2D  ! Subgrid burning surface for cell

END SUBROUTINE FIRE_QUANDRANTSURFACE


SUBROUTINE FIRE_LS_RECONSTRUCTION_FROM_BMAP( PLSPHI, PBMAP, PATMDT )

  IMPLICIT NONE

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PLSPHI     ! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)  ::  PBMAP     ! Burning map
  REAL,                     INTENT(IN)  ::  PATMDT    ! Atm time step

END SUBROUTINE FIRE_LS_RECONSTRUCTION_FROM_BMAP


SUBROUTINE FIRE_GRAD_OROGRAPHY( PZS, PFMGRADOROX, PFMGRADOROY )

  IMPLICIT NONE

  REAL,  DIMENSION(:,:),     INTENT(IN)  ::  PZS           ! MNH orography (atm resolution) [m]
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PFMGRADOROX   ! Orography gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PFMGRADOROY   ! Orography gradient on y direction (dz/dy) [m/m]

END SUBROUTINE FIRE_GRAD_OROGRAPHY


FUNCTION FIRE_SURF_68( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )

  IMPLICIT NONE

  REAL,    INTENT(IN)     ::  PPHI1 ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4 ! Phi4

  REAL                  ::  PSURF ! Surface ratio

END FUNCTION FIRE_SURF_68


FUNCTION FIRE_SURF_70( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )

  IMPLICIT NONE

  REAL,    INTENT(IN)     ::  PPHI1 ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4 ! Phi4

  REAL                  ::  PSURF ! Surface ratio
END FUNCTION FIRE_SURF_70


FUNCTION FIRE_SURF_22( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )

  IMPLICIT NONE

  REAL,    INTENT(IN)     ::  PPHI1 ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4 ! Phi4

  REAL                  ::  PSURF ! Surface ratio
END FUNCTION FIRE_SURF_22


FUNCTION FIRE_SURF_28( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )

  IMPLICIT NONE

  REAL,    INTENT(IN)     ::  PPHI1 ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3 ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4 ! Phi4

  REAL                  ::  PSURF ! Surface ratio
END FUNCTION FIRE_SURF_28


! ************************ deprecated ************************
FUNCTION FGET_I( PLINDEX, PMINDEX ) RESULT( POUTINDEX )

  IMPLICIT NONE

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                   ::  POUTINDEX           ! i atm index

END FUNCTION FGET_I


FUNCTION FGET_J( PLINDEX, PMINDEX ) RESULT( POUTINDEX )

  IMPLICIT NONE

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                    ::  POUTINDEX           ! i atm index

END FUNCTION FGET_J


FUNCTION FGET_K( PLINDEX, PMINDEX ) RESULT( POUTINDEX )

  IMPLICIT NONE

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                   ::  POUTINDEX           ! i atm index

END FUNCTION FGET_K
! ************************************************************


END INTERFACE
END MODULE MODI_FIRE_MODEL

SUBROUTINE FIRE_GRADPHI( PLSPHI, PGRADLSPHIX, PGRADLSPHIY )
  !!****  *FIRE_GRADPHI* - Fire model computation of Level set function gradient
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute gradient on x and y direcctions for level set function
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       WENO1 or WENO3
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !! 
  !!    PhD
  !!      [a] A. Costes PhD [2021]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France/Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  !
  USE MODD_FIRE
  USE MODI_FIRE_MODEL, ONLY: FIRE_GRADMASK, FIRE_WENO_1, FIRE_WENO_3
  USE MODD_FIELD_n, ONLY : XLSPHI2D, XGRADLSPHIX2D, XGRADLSPHIY2D, XGRADMASKX, XGRADMASKY
  !
  USE MODE_MPPDB
  USE MODD_TIME_n, ONLY : TDTCUR
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PLSPHI       ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PGRADLSPHIY  ! Grad of Phi on y direction
  !

  !*       0.2   declarations of local variables

  INTEGER :: IIU, IJU             ! atm mesh bounds
  INTEGER :: ILU, IMU             ! fire mesh bounds
  INTEGER :: IKU                  ! fire 3rd dimension bounds
  INTEGER :: IA,IB,II,IJ,IK,IL,IM ! Index for conversions
  ! loop
  INTEGER :: JI,JJ,JK             ! index for atm mesh loop i,j,k
  INTEGER :: JL,JM                ! index for fire mesh loop l,m

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Allocate 2D array with fire grid bounds

  ! get atm mesh bounds
  IIU = SIZE(PLSPHI,1)
  IJU = SIZE(PLSPHI,2)
  IKU = SIZE(PLSPHI,3) ! NREFINX * NREFINY

  ! fire mesh bounds
  ILU = IIU*NREFINX
  IMU = IJU*NREFINY

  ! Default values
  PGRADLSPHIX(:,:,:) = 0.
  PGRADLSPHIY(:,:,:) = 0.

  !*      2. Convert LS function Phi from 3d to 2d format

  ! get l and m to find PHI2D(l,m) = PHI3D(i,j,k)
  DO JK = 1, IKU
    ! b = (k-1) \ NREFINX + 1 where \ means euclidian division
    ! as k,1 and NREFINX are integers, (k-1)/NREFINX is an integer division
    IB = (JK - 1) / NREFINX + 1
    ! a = k - (b-1)*NREFINX
    IA = JK - (IB - 1) * NREFINX
    !
    DO JJ = 1, IJU
      ! m = (j-1)*NREFINY + b
      IM = (JJ - 1) * NREFINY + IB
      !
      DO JI = 1, IIU
        ! l = (i-1)*NREFINX + a
        IL = (JI - 1) * NREFINX + IA
        !  PHI2D(l,m) = PHI3D(i,j,k)
        XLSPHI2D(IL,IM) = PLSPHI(JI,JJ,JK)
      END DO
    END DO
  END DO
  !*      3. Compute gradients on 2D grid

  SELECT CASE(NFIRE_WENO_ORDER)
  CASE(1)
    ! Compute mask with 2nd order difference
    CALL FIRE_GRADMASK( XLSPHI2D, XGRADMASKX, XGRADMASKY, 2 )
    CALL FIRE_WENO_1( XLSPHI2D, XGRADLSPHIX2D, XGRADLSPHIY2D, XGRADMASKX, XGRADMASKY )
  !
  CASE(3)
    ! Compute mask with 2nd order difference
    CALL FIRE_GRADMASK( XLSPHI2D, XGRADMASKX, XGRADMASKY, 2 )
    CALL FIRE_WENO_3( XLSPHI2D, XGRADLSPHIX2D, XGRADLSPHIY2D, XGRADMASKX, XGRADMASKY )
    !
  !
  CASE DEFAULT
    CALL FIRE_GRADMASK( XLSPHI2D, XGRADMASKX, XGRADMASKY, 2 )
    CALL FIRE_WENO_1( XLSPHI2D, XGRADLSPHIX2D, XGRADLSPHIY2D, XGRADMASKX, XGRADMASKY )
  END SELECT

  !*      4. Convert gradients from 2d to 3d format

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  DO JM = 1, IMU
    ! j = ceil(m/NREFINY)
    IJ = CEILING(REAL(JM) / REAL(NREFINY))
    ! b = m - (j-1) * NREFINY
    IB = JM - (IJ - 1) * NREFINY
    !
    DO JL = 1, ILU
      ! i = ceil(l/NREFINX)
      II = CEILING(REAL(JL) / REAL(NREFINX))
      ! a = l - (i-1) * NREFINX
      IA = JL - (II - 1) * NREFINX
      ! k = (b-1) * NREFINX + a
      IK = (IB - 1) * NREFINX + IA
      !  GRAD3D(i,j,k) = GRAD2D(l,m)
      PGRADLSPHIX(II,IJ,IK) = XGRADLSPHIX2D(JL,JM)
      PGRADLSPHIY(II,IJ,IK) = XGRADLSPHIY2D(JL,JM)
    END DO
  END DO

END SUBROUTINE FIRE_GRADPHI


SUBROUTINE FIRE_PROPAGATE( PLSPHI, PBMAP, PFMIGNITION, PFMWALKIG, PGRADLSPHIX, PGRADLSPHIY, PATMDT, PFIRERW)
  !!****  *FIRE_PROPAGATE* - propagate fire in time
  !!
  !!    PURPOSE
  !!    -------
  !!    Use RK scheme to propagate fire un time
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!    PhD
  !!      [a] A. Costes PhD [2021]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  USE MODI_FIRE_MODEL, ONLY: FIRE_RK, FIRE_LS_RECONSTRUCTION_FROM_BMAP
  USE MODD_LUNIT_n, ONLY: TLUOUT
  
  USE MODE_MPPDB
  
  IMPLICIT NONE
  
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PLSPHI       ! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PBMAP       ! Burning map
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMIGNITION  ! Ignition map
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMWALKIG    ! Walking ignition map

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),      INTENT(IN)  :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),      INTENT(IN)  :: PGRADLSPHIY  ! Grad of Phi on y direction
  !
  REAL,  DIMENSION(:,:,:),      INTENT(IN)  :: PFIRERW      ! Rate of spread with wind
  !
  REAL,                       INTENT(IN)  :: PATMDT       ! Atm time step
  !

  !*       0.2   declarations of local variables

  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZLSPHI1     ! Level Set function at n+1
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRKK1       ! Runge Kutta 4 increment k1
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRKK2       ! Runge Kutta 4 increment k2
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRKK3       ! Runge Kutta 4 increment k3
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRKK4       ! Runge Kutta 4 increment k4
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRK4TMPPHI  ! Runge Kutta 4 tmp phi field
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRK4GRADX   ! Runge Kutta 4 tmp grad phi x
  REAL,  DIMENSION(SIZE(PLSPHI,1), SIZE(PLSPHI,2), SIZE(PLSPHI,3))  ::  ZRK4GRADY   ! Runge Kutta 4 tmp grad phi y

  INTEGER :: INBITEFIRE                                   ! # iteration in the propagation loop (ie. splitted atm time step)
  REAL    :: ZDTFIRE                                      ! Fire time step
  INTEGER :: JI

  !*      1. Update Ignition
  !*      1.1 Ignition map

  WHERE (PFMIGNITION <= TDTCUR%XTIME )
    ! update level set function where ignition starts at
    PLSPHI = 1.
    ! update bmap arrival time with ignition times
    PBMAP = PFMIGNITION
  END WHERE

  !*      1.2 Walking ignition map at time t

  CALL FIRE_LS_RECONSTRUCTION_FROM_BMAP( PLSPHI, PFMWALKIG, 0.)

  !*      2. Compute # iteration in fire loop

  INBITEFIRE = INT(PATMDT * MAXVAL(PFIRERW) / (XCFLMAXFIRE * MAX(XFIREMESHSIZE(1), XFIREMESHSIZE(2)))) + 1
  IF (INBITEFIRE .NE. 1) THEN
    WRITE(UNIT=TLUOUT%NLU,FMT=*) 'INFO BLAZE : INBITEFIRE is not 1 but is ', INBITEFIRE
  END IF
  !   Compute fire time step
  ZDTFIRE = PATMDT / REAL(INBITEFIRE)

  !*      3. Compute time integration

  DO JI=1,INBITEFIRE
      ! Call Runge kutta time integration
      CALL FIRE_RK( PLSPHI, ZLSPHI1, PGRADLSPHIX, PGRADLSPHIY, PFIRERW, ZDTFIRE )
      ! get overshoots
      WHERE (ZLSPHI1 > 1.) ZLSPHI1 = 1.
      WHERE (ZLSPHI1 < 0.) ZLSPHI1 = 0.
      ! Update walking igniton at t+dt
      CALL FIRE_LS_RECONSTRUCTION_FROM_BMAP( PLSPHI, PFMWALKIG, ZDTFIRE )
      ! Update BMAP
      WHERE (ZLSPHI1 >= .5 .AND. PBMAP < 0.)
          ! where fire is passed, ie phi >= 0.5
          ! but where the bmap is not already defined, ie BMap < 0 as default value is -1
          ! Time increment of Bmap is computed by linear interpolation of Phi function crossing 0.5 value.
          PBMAP = TDTCUR%XTIME + ZDTFIRE * ( 0.5 - PLSPHI ) / ( ZLSPHI1 - PLSPHI )
      END WHERE
      ! update phi value
      PLSPHI = ZLSPHI1
  END DO

END SUBROUTINE FIRE_PROPAGATE


SUBROUTINE FIRE_NOWINDROS( PFIREFUELMAP, PFMR0, PFMRFA, PFMWF0, PFMR00, PFMFUELTYPE, PFIRETAU, &
                           PFLUXPARAMH, PFLUXPARAMW, PFMASE, PFMAWC )
  !!****  *FIRE_NOWINDROS* - Fire model computation of rate of spread without wind
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute of rate of spread without wind
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       Balbi model with Balbi fuel with 22 properties :
  !!          1. Fuel type (<0 : Walking ignition, 0 : Unburnable, >0 : burnable)
  !!          2. Rhod
  !!          3. Rhol
  !!          4. Md
  !!          5. Ml
  !!          6. sd
  !!          7. sl
  !!          8. Sigmad
  !!          9. Sigmal
  !!          10. e
  !!          11. Ti
  !!          12. Ta
  !!          13. DeltaH
  !!          14. Deltah
  !!          15. Tau0
  !!          16. stoch
  !!          17. Rhoa
  !!          18. cp
  !!          19. cpa
  !!          20. X0
  !!          21. LAI
  !!          22. r00
  !!      Fuel is set by python script into netcdf file in this order.
  !!      See pyrolib package on PyPi to create your script.
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Santoni et al. 2011
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST, ONLY : XSTEFAN
  USE MODD_CONF, ONLY: CCONF
  USE MODE_MPPDB
  USE MODD_FIRE
  
  IMPLICIT NONE
  
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Fuel map
  REAL,  DIMENSION(:,:,:,:),    INTENT(IN)      ::  PFIREFUELMAP  ! Fuel map

  !! Rate of spread and factors
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMR0          ! Rate of spread without wind (R0)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMRFA         ! Radiant factor (A)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMWF0         ! Vertical flame velocity (v0)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)      ::  PFMR00         ! Flame thickness speed factor (r0)
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)   ::  PFMFUELTYPE    ! Fuel type
  REAL,  DIMENSION(:,:,:),      INTENT(INOUT)   ::  PFIRETAU      ! Residence time
  REAL,  DIMENSION(:,:,:,:),    INTENT(OUT)     ::  PFLUXPARAMH    ! params sensible heat flux model
  REAL,  DIMENSION(:,:,:,:),    INTENT(OUT)     ::  PFLUXPARAMW    ! params latent heat flux model
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)     ::  PFMASE        ! Available sensible energy (J/m2)
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)     ::  PFMAWC        ! Available water content (kg/m2)
  !

  !*       0.2   declarations of local variables

  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZBETAD, ZBETAL      ! betad, betal : Packing ratio dry, living fuel
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZSD, ZSL            ! Sd, Sl
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZNU                 ! nu
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZA                  ! a
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZA0                 ! A0
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZXSI                ! Xi
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZTF                 ! Tn : nominal radiant temperature
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZV00                ! v00
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZR00                ! R00
  ! get fuel in 3d array to allow mask use
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELRHOD           ! Rhod
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELRHOL           ! Rhol
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELMD             ! Md
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELML             ! Ml
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELSD             ! sd
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELSL             ! sl
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELSIGMAD         ! Sigmad
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELSIGMAL         ! Sigmal
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELE              ! e
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELTI             ! Ti
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELTA             ! Ta
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELDELTAH         ! DeltaH
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELDH             ! Deltah
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELTAU0           ! Tau0
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELSTOCH          ! stoch
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELRHOA           ! Rhoa
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELCP             ! cp
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELCPA            ! cpa
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELX0             ! X0
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELLAI            ! LAI
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZFUELR00            ! r00

  ! Work array
  REAL, DIMENSION(SIZE(PFMR0,1),SIZE(PFMR0,2),SIZE(PFMR0,3)) :: ZWORK, ZWORK2, ZWORK3  ! Tmp array

  ! combustion efficiency parameters
  REAL, PARAMETER :: ZEC = 0.5
  REAL, PARAMETER :: ZES = 0.1

  ! loop, tests
  INTEGER :: JI, JJ

  !*      1. Get fuel in 3D array

  PFMFUELTYPE = PFIREFUELMAP(:,:,:,1)            ! Fuel type
  ZFUELRHOD   = PFIREFUELMAP(:,:,:,2)            ! Rhod
  ZFUELRHOL   = PFIREFUELMAP(:,:,:,3)            ! Rhol
  ZFUELMD     = PFIREFUELMAP(:,:,:,4)            ! Md
  ZFUELML     = PFIREFUELMAP(:,:,:,5)            ! Ml
  ZFUELSD     = PFIREFUELMAP(:,:,:,6)            ! sd
  ZFUELSL     = PFIREFUELMAP(:,:,:,7)            ! sl
  ZFUELSIGMAD = PFIREFUELMAP(:,:,:,8)            ! Sigmad
  ZFUELSIGMAL = PFIREFUELMAP(:,:,:,9)            ! Sigmal
  ZFUELE      = PFIREFUELMAP(:,:,:,10)           ! e
  ZFUELTI     = PFIREFUELMAP(:,:,:,11)           ! Ti
  ZFUELTA     = PFIREFUELMAP(:,:,:,12)           ! Ta
  ZFUELDELTAH = PFIREFUELMAP(:,:,:,13)           ! DeltaH
  ZFUELDH     = PFIREFUELMAP(:,:,:,14)           ! Deltah
  ZFUELTAU0   = PFIREFUELMAP(:,:,:,15)           ! Tau0
  ZFUELSTOCH  = PFIREFUELMAP(:,:,:,16)           ! stoch
  ZFUELRHOA   = PFIREFUELMAP(:,:,:,17)           ! Rhoa
  ZFUELCP     = PFIREFUELMAP(:,:,:,18)           ! cp
  ZFUELCPA    = PFIREFUELMAP(:,:,:,19)           ! cpa
  ZFUELX0     = PFIREFUELMAP(:,:,:,20)           ! X0
  ZFUELLAI    = PFIREFUELMAP(:,:,:,21)           ! LAI
  ZFUELR00    = PFIREFUELMAP(:,:,:,22)           ! r00

  !*      1. Compute unburnable area

  WHERE (PFMFUELTYPE == 0.)
    PFMR0 = 0.
    PFMR00 = 0.
    PFMRFA = 0.
    PFMWF0 = 0.
  END WHERE

  !*      2. Compute Walking ignition

  !! *********** deprecated ***********
  WHERE (PFMFUELTYPE < 0.)
    PFMR0 = -1. * PFMFUELTYPE
    PFMR00 = 0.
    PFMRFA = 0.
    PFMWF0 = 0.
  END WHERE
  !! **********************************

  !*      3. Compute R0, A, Wf0, r00

  WHERE (PFMFUELTYPE > 0.)
    ! betad = Sigmad / (e * Rhod)
    ZBETAD = ZFUELSIGMAD / (ZFUELE * ZFUELRHOD)
    ! betal = Sigmal / (e * Rhol)
    ZBETAL = ZFUELSIGMAL / (ZFUELE * ZFUELRHOL)
    ! Sd = sd * e * betad
    ZSD = ZFUELSD * ZFUELE * ZBETAD
    ! Sl = sl * e * betal
    ZSL = ZFUELSL * ZFUELE * ZBETAL
    ! nu = min(Sd/LAI,1)
    ZNU = MIN(ZSD/ZFUELLAI,1.)
    ! a = Deltah / (cp * (Ti - Ta))
    ZA = ZFUELDH / (ZFUELCP * (ZFUELTI - ZFUELTA))
    ! r0 = sd / r00
    PFMR00 = ZFUELSD * ZFUELR00
    ! A0 = X0 * DeltaH / (4. * cp *(Ti - Ta))
    ZA0 = ZFUELX0 * ZFUELDELTAH / (4. * ZFUELCP * (ZFUELTI - ZFUELTA))
    ! xsi = (Ml - Md) * (Sl/Sd) * (Deltah/DeltaH)
    ZXSI = (ZFUELML - ZFUELMD) * (ZSL * ZFUELDH) / (ZSD * ZFUELDELTAH)
    ! A = nu * A0 * (1. - xsi) / (1. + a*Md)
    PFMRFA = ZNU * ZA0 * (1. - ZXSI) / (1. + ZA * ZFUELMD)
    ! Tn = Ta + (DeltaH * (1. - X0)*(1-xsi)/(cpa*(1 + stoch)))
    ZTF = ZFUELTA + (ZFUELDELTAH * (1. - ZFUELX0) * (1. - ZXSI) / (ZFUELCPA * (1. + ZFUELSTOCH)))
    ! R00 = B * T^4 / (cp * (Ti - Ta))
    ZR00 = XSTEFAN * ZTF**4 / (ZFUELCP * (ZFUELTI - ZFUELTA))
    ! V00 = 2 * LAI * (1. + stoch) * Tn * Rhod / (rhoa * Ta * Tau0)
    ZV00 = 2. * ZFUELLAI * (1. + ZFUELSTOCH) * ZTF * ZFUELRHOD / (ZFUELRHOA * ZFUELTA * ZFUELTAU0)
    ! v0 = nu * V00
    PFMWF0 = ZNU * ZV00
    ! R0 = e * R00 / (Sigmad * (1. + a*Md)) * (Sd/(Sd+Sl))^2
    PFMR0 = ZFUELE * ZR00 / (ZFUELSIGMAD * (1. + ZA * ZFUELMD)) * (ZSD / (ZSD + ZSL))**2
  END WHERE

  !*      4. Compute Residence time

  ! Compute residence time
  ! Tau = Tau0 / sd
  WHERE (ZFUELSD /= 0) PFIRETAU = ZFUELTAU0 / ZFUELSD

  !*      5. Compute Sensible heat flux parameters maps if needed

  ZWORK(:,:,:)  = 0.
  ZWORK2(:,:,:) = 0.
  ZWORK3(:,:,:) = 0.

  ! Compute Sensible heat flux model param map if needed
  SELECT CASE (CHEAT_FLUX_MODEL)
  CASE('CST')
    ! Nominal injection value is needed
    ! INBPARAMSENSIBLE = 1
    ! phih = Ec * (1 - X0) * sigmad * DeltaH / (Tau * (1. + Md))
    ! Ec is combustion efficiency and set to 0.5
    WHERE (PFIRETAU > 0) ZWORK = XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (PFIRETAU * (1. + ZFUELMD))
    PFLUXPARAMH(:,:,:,1) = ZWORK(:,:,:)
    !
    ! Available Sensible Energy
    IF (.NOT. LRESTA_ASE) THEN
      WHERE (PFIRETAU > 0) PFMASE = XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (1. + ZFUELMD)
    END IF

  CASE('EXP')
    ! Exponential injection
    ! Nominal injection value is needed
    ! Characteristic time value is needed
    ! INBPARAMSENSIBLE = 2
    ! phie = Ec * (1 - X0) * sigmad * DeltaH / (Taue * (1. + Md))
    ! taue = - tauf / ln(1 - FERR)
    ! tauf = flamming residence time
    ! FERR = Flamming Energy Release Ratio (namelist parameter), 0.5 <= FERR < 1
    ! Ec is combustion efficiency and set to 0.5
    WHERE (PFIRETAU > 0)
      ! taue
      ZWORK  = - PFIRETAU / LOG(1. - XFERR)
      ! phie
      ZWORK2 = XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (ZWORK * (1. + ZFUELMD))
    END WHERE
    PFLUXPARAMH(:,:,:,1) = ZWORK2(:,:,:)
    PFLUXPARAMH(:,:,:,2) = ZWORK(:,:,:)
    !
    ! Available Sensible Energy
    IF (.NOT. LRESTA_ASE) THEN
      WHERE (PFIRETAU > 0) PFMASE = XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (1. + ZFUELMD)
    END IF

  CASE('EXS')
    ! Exponential + smoldering injection
    ! Nominal injection value is needed
    ! Characteristic time value is needed
    ! Smoldering injection value si needed
    ! INBPARAMSENSIBLE = 3
    ! phie = Ec * (1 - X0) * sigmad * DeltaH / (Taue * (1. + Md))
    ! taue = - tauf / ln(1 - FERR)
    ! phis = 0.006 * phih where phih is the nominal flux of CST model
    ! tauf = flamming residence time
    ! FERR = Flamming Energy Release Ratio (namelist parameter), 0.5 <= FERR < 1
    ! Ec is combustion efficiency and set to 0.5
    ! Es is smoldering energy consumption set to 0.1
    WHERE (PFIRETAU > 0)
      ! taue
      ZWORK  = - PFIRETAU / LOG(1. - XFERR)
      ! phie
      ZWORK2 = XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (ZWORK * (1. + ZFUELMD))
      ! phis
      ZWORK3 = .006 * XFLXCOEFTMP * ZEC * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (PFIRETAU * (1. + ZFUELMD))
    END WHERE
    PFLUXPARAMH(:,:,:,1) = ZWORK2(:,:,:)
    PFLUXPARAMH(:,:,:,2) = ZWORK(:,:,:)
    PFLUXPARAMH(:,:,:,3) = ZWORK3(:,:,:)
    !
    ! Available Sensible Energy
    IF (.NOT. LRESTA_ASE) THEN
      WHERE (PFIRETAU > 0) PFMASE = XFLXCOEFTMP * (ZEC + ZES) * (1. - ZFUELX0) * ZFUELSIGMAD * ZFUELDELTAH / (1. + ZFUELMD)
    END IF
  END SELECT

  !*      5. Compute Sensible heat flux parameters maps if needed

  ZWORK(:,:,:)  = 0.
  ZWORK2(:,:,:) = 0.

  ! Compute Latent heat flux model param map if needed
  SELECT CASE (CLATENT_FLUX_MODEL)
  CASE('CST')
    ! Nominal injection value is needed
    ! INBPARAMLATENT = 1
    ! phiw = (sigmad * Md + sigmal * Ml) / Tau
    WHERE (PFIRETAU > 0) ZWORK = (ZFUELSIGMAD * ZFUELMD + ZFUELSIGMAL * ZFUELML) / PFIRETAU
    PFLUXPARAMW(:,:,:,1) = ZWORK(:,:,:)
    !
    ! Available water content
    IF (.NOT. LRESTA_AWC) THEN
      WHERE(PFIRETAU > 0) PFMAWC =  ZFUELSIGMAD * ZFUELMD + ZFUELSIGMAL * ZFUELML
    END IF

  CASE('EXP')
    ! Exponential injection
    ! Nominal injection value is needed
    ! Characteristic time value is needed
    ! INBPARAMSENSIBLE = 2
    ! phie = (sigmad * Md + sigmal * Ml) / Tau
    ! taue = - tauf / ln(1 - FERR)
    ! tauf = flamming residence time
    ! FERR = Flamming Energy Release Ratio (namelist parameter), 0.5 <= FERR < 1
    ! Ec is combustion efficiency and set to 0.5
    WHERE (PFIRETAU > 0)
      ! taue
      ZWORK  = - PFIRETAU / LOG(1. - XFERR)
      ! phie
      ZWORK2 = (ZFUELSIGMAD * ZFUELMD + ZFUELSIGMAL * ZFUELML) / ZWORK
    END WHERE
    PFLUXPARAMW(:,:,:,1) = ZWORK2(:,:,:)
    PFLUXPARAMW(:,:,:,2) = ZWORK(:,:,:)
    !
    ! Available water content
    IF (.NOT. LRESTA_AWC) THEN
      WHERE(PFIRETAU > 0) PFMAWC =  ZFUELSIGMAD * ZFUELMD + ZFUELSIGMAL * ZFUELML
    END IF
  END SELECT

END SUBROUTINE FIRE_NOWINDROS


SUBROUTINE FIRE_GETWIND( PUT, PVT, PWT, PGRADLSPHIX, PGRADLSPHIY, PFIREWIND, KTCOUNT, PATMDT, PFMGRADOROX, PFMGRADOROY )
  !!****  *FIRE_GETWIND* - Compute horizontal wind on fire mesh
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute horizontal wind on fire mesh. Horizontal interpolation and temporal filtering
  !!
  !!**  METHOD
  !!    ------
  !!    
  !!    See A. Costes PhD [2021], Chapter 2, Section 3.1.a for interpolation
  !!    See A. Costes PhD [2021], Chapter 2, Section 3.1.b for temporal filtering (EWAM recommended as WLIM is deprecated)
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_GRID_n,  ONLY : XXHAT,XYHAT
  USE MODI_SHUMAN,  ONLY : MXF, MYF
  USE MODD_CONF  ,  ONLY : CCONF
  USE MODD_FIELD_n, ONLY : XFMHWS, XFMWINDU, XFMWINDV, XFMWINDW
  !
  USE MODD_FIRE
  !
  USE MODE_MPPDB

  ! tmp use
  USE MODD_TIME_n,     ONLY : TDTCUR
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------
  !! Atm wind
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PUT          ! U Wind on atm grid
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PVT          ! V Wind on atm grid
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PWT          ! W Wind on atm grid

  !! Grad of Phi
  REAL,  DIMENSION(:,:,:), INTENT(IN)  ::  PGRADLSPHIX ! Grad Phi on x direction
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PGRADLSPHIY ! Grad Phi on y direction

  !! Wind on fire grid
  REAL,  DIMENSION(:,:,:),  INTENT(OUT) ::  PFIREWIND   ! Wind Value on fire grid in -grad Phi direction
  
  !! others
  INTEGER,                INTENT(IN)  ::  KTCOUNT      ! Iteration number
  REAL   ,                INTENT(IN)  ::  PATMDT      ! Atmospheric Time step
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PFMGRADOROX ! Orography gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),  INTENT(IN)  ::  PFMGRADOROY ! Orography gradient on y direction (dz/dy) [m/m]

  !*       0.2   declarations of local variables

  REAL,   DIMENSION(SIZE(PUT,1),SIZE(PUT,2),2)    :: ZUM           ! U wind on mass grid for first 2 levels
  REAL,   DIMENSION(SIZE(PVT,1),SIZE(PVT,2),2)    :: ZVM           ! V wind on mass grid for first 2 levels

  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZGRADNORM           ! Norm of grad phi with slope
  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZNX                 ! Unit normal vector n on x direction
  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZNY                 ! Unit normal vector n on y direction
  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZNZ                 ! Unit normal vector n on z direction

  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZWINDTMPU           ! Wind U on fire grid
  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZWINDTMPV           ! Wind V on fire grid
  REAL,   DIMENSION(SIZE(PFIREWIND,1),SIZE(PFIREWIND,2),SIZE(PFIREWIND,3))    :: ZWINDTMPW           ! Wind W on fire grid

  REAL,   DIMENSION(SIZE(PUT,1),SIZE(PUT,2))    :: ZIU1,ZIU2,ZIU3,ZIU4                                ! U on atm mesh corners
  REAL,   DIMENSION(SIZE(PUT,1),SIZE(PUT,2))    :: ZIV1,ZIV2,ZIV3,ZIV4                                ! V on atm mesh corners
  REAL,   DIMENSION(SIZE(PUT,1),SIZE(PUT,2))    :: ZIW1,ZIW2,ZIW3,ZIW4                                ! W on atm mesh corners

  REAL :: ZALPHAEWAM       ! EWAM alpha constant

  INTEGER :: IKU, IIU, IJU
  INTEGER :: IL, IM
  ! loop, tests
  INTEGER :: JI, JJ, JK
  !----------------------------------------------------------------------------------------------
  !

  !*      1. Compute fire front normal vector

  ZGRADNORM = SQRT(PGRADLSPHIX**2 + PGRADLSPHIY**2 + (PGRADLSPHIX * PFMGRADOROX + PGRADLSPHIY * PFMGRADOROY)**2)

  ! Normalize wind projection vectors on -gradphi vector
  WHERE (ZGRADNORM > 0.)
    ZNX = -1. *  PGRADLSPHIX / ZGRADNORM
    ZNY = -1. *  PGRADLSPHIY / ZGRADNORM
    ZNZ = -1. * (PGRADLSPHIX * PFMGRADOROX + PGRADLSPHIY * PFMGRADOROY) / ZGRADNORM
  ELSEWHERE
    ZNX = 0.
    ZNY = 0.
    ZNZ = 0.
  END WHERE

  IF (LINTERPWIND) THEN
    !*      1. Horizontal interpolation fo wind
    !*      1.1 get wind on atm corners
    ! get atm array size
    IIU = SIZE(PUT,1)
    IJU = SIZE(PUT,2)
    IKU = SIZE(PFIREWIND,3)
    ! Default value
    ZIU1(:,:) = 0.
    ZIU2(:,:) = 0.
    ZIU3(:,:) = 0.
    ZIU4(:,:) = 0.
    ZIV1(:,:) = 0.
    ZIV2(:,:) = 0.
    ZIV3(:,:) = 0.
    ZIV4(:,:) = 0.
    ZIW1(:,:) = 0.
    ZIW2(:,:) = 0.
    ZIW3(:,:) = 0.
    ZIW4(:,:) = 0.
    ! interpol sw corner
    ZIU1(2:IIU-1,2:IJU-1) = .5 *   (PUT(2:IIU-1,1:IJU-2,2) + PUT(2:IIU-1,2:IJU-1,2))
    ZIV1(2:IIU-1,2:IJU-1) = .5 *   (PVT(1:IIU-2,2:IJU-1,2) + PVT(2:IIU-1,2:IJU-1,2))
    ZIW1(2:IIU-1,2:IJU-1) = .125 * &
                            (PWT(1:IIU-2,1:IJU-2,2) + PWT(2:IIU-1,1:IJU-2,2) + PWT(2:IIU-1,2:IJU-1,2) + PWT(1:IIU-2,2:IJU-1,2) + &
                             PWT(1:IIU-2,1:IJU-2,3) + PWT(2:IIU-1,1:IJU-2,3) + PWT(2:IIU-1,2:IJU-1,3) + PWT(1:IIU-2,2:IJU-1,3))
    ! interpol se corner
    ZIU2(2:IIU-1,2:IJU-1) = .5 *   (PUT(3:IIU  ,1:IJU-2,2) + PUT(3:IIU  ,2:IJU-1,2))
    ZIV2(2:IIU-1,2:IJU-1) = .5 *   (PVT(3:IIU  ,2:IJU-1,2) + PVT(2:IIU-1,2:IJU-1,2))
    ZIW2(2:IIU-1,2:IJU-1) = .125 * &
                            (PWT(2:IIU-1,1:IJU-2,2) + PWT(3:IIU  ,1:IJU-2,2) + PWT(3:IIU  ,2:IJU-1,2) + PWT(2:IIU-1,2:IJU-1,2) + &
                             PWT(2:IIU-1,1:IJU-2,3) + PWT(3:IIU  ,1:IJU-2,3) + PWT(3:IIU  ,2:IJU-1,3) + PWT(2:IIU-1,2:IJU-1,3))
    ! interpol ne corner
    ZIU3(2:IIU-1,2:IJU-1) = .5 *   (PUT(3:IIU  ,3:IJU  ,2) + PUT(3:IIU  ,2:IJU-1,2))
    ZIV3(2:IIU-1,2:IJU-1) = .5 *   (PVT(3:IIU  ,3:IJU  ,2) + PVT(2:IIU-1,3:IJU  ,2))
    ZIW3(2:IIU-1,2:IJU-1) = .125 * &
                            (PWT(2:IIU-1,2:IJU-1,2) + PWT(3:IIU  ,2:IJU-1,2) + PWT(3:IIU  ,3:IJU  ,2) + PWT(2:IIU-1,3:IJU  ,2) + &
                             PWT(2:IIU-1,2:IJU-1,3) + PWT(3:IIU  ,2:IJU-1,3) + PWT(3:IIU  ,3:IJU  ,3) + PWT(2:IIU-1,3:IJU  ,3))
    ! interpol nw corner
    ZIU4(2:IIU-1,2:IJU-1) = .5 *   (PUT(2:IIU-1,3:IJU  ,2) + PUT(2:IIU-1,2:IJU-1,2))
    ZIV4(2:IIU-1,2:IJU-1) = .5 *   (PVT(1:IIU-2,3:IJU  ,2) + PVT(2:IIU-1,3:IJU  ,2))
    ZIW4(2:IIU-1,2:IJU-1) = .125 * &
                            (PWT(1:IIU-2,2:IJU-1,2) + PWT(2:IIU-1,2:IJU-1,2) + PWT(2:IIU-1,3:IJU  ,2) + PWT(1:IIU-2,3:IJU  ,2) + &
                             PWT(1:IIU-2,2:IJU-1,3) + PWT(2:IIU-1,2:IJU-1,3) + PWT(2:IIU-1,3:IJU  ,3) + PWT(1:IIU-2,3:IJU  ,3))
    
    !*      1.2 Interpol on fire grid
    DO JK = 1, IKU
      ! compute index position of grid cell
      IM = (JK - 1) / NREFINX + 1
      IL = JK - (IM - 1) * NREFINX
      ! Interpol
      ZWINDTMPU(:,:,JK) = (IM * (IL * ZIU3 + (NREFINX + 1 - IL) * ZIU4) + &
                          (NREFINY + 1 - IM) * (IL * ZIU2 + (NREFINX + 1 - IL) * ZIU1)) / &
                          REAL((NREFINX + 1) * (NREFINY + 1))
      ZWINDTMPV(:,:,JK) = (IM * (IL * ZIV3 + (NREFINX + 1 - IL) * ZIV4) + &
                          (NREFINY + 1 - IM) * (IL * ZIV2 + (NREFINX + 1 - IL) * ZIV1)) / &
                          REAL((NREFINX + 1) * (NREFINY + 1))
      ZWINDTMPW(:,:,JK) = (IM * (IL * ZIW3 + (NREFINX + 1 - IL) * ZIW4) + &
                          (NREFINY + 1 - IM) * (IL * ZIW2 + (NREFINX + 1 - IL) * ZIW1)) / &
                          REAL((NREFINX + 1) * (NREFINY + 1))
    END DO
    !
  ELSE
    !*      2. No interpolation. Share wind for each fire cell
    !*      2.1 Get wind on mass grid

    ZUM = MXF(PUT(:,:,1:2))
    ZVM = MYF(PVT(:,:,1:2))

    !*      2.2 Get wind on fire grid

    IKU = SIZE(PFIREWIND,3)

    ! Share wind for each fire cell in atm cell
    DO JK = 1, IKU
      ZWINDTMPU(:,:,JK) = ZUM(:,:,1)
      ZWINDTMPV(:,:,JK) = ZVM(:,:,1)
      ZWINDTMPW(:,:,JK) = .5*(PWT(:,:,2)+PWT(:,:,3))
    END DO
  END IF

  !*      4. Wind filtering

  IF (LWINDFILTER) THEN
    SELECT CASE(CWINDFILTER)
    CASE('EWAM')
      IF (KTCOUNT <= 1 .AND. .NOT. LRESTA_EWAM) THEN
        ! set first value of u and v filtered wind field
        ! u filetered = u from MNH
        XFMWINDU = ZWINDTMPU
        ! v filetered = v from MNH
        XFMWINDV = ZWINDTMPV
        ! w filetered = v from MNH
        XFMWINDW = ZWINDTMPW
      ELSE
        ZALPHAEWAM = 2. / (1. + CEILING(XEWAMTAU/PATMDT))
        ! filter u
        XFMWINDU = XFMWINDU + ZALPHAEWAM*(ZWINDTMPU - XFMWINDU)
        ! filter v
        XFMWINDV = XFMWINDV + ZALPHAEWAM*(ZWINDTMPV - XFMWINDV)
        ! filter w
        XFMWINDW = XFMWINDW + ZALPHAEWAM*(ZWINDTMPW - XFMWINDW)
      END IF

    CASE('WLIM')
      ! initialize HWS to MNH wind
      IF (KTCOUNT <= 1 .AND. .NOT. LRESTA_WLIM) THEN
        XFMHWS = ZWINDTMPU * ZNX + ZWINDTMPV * ZNY + ZWINDTMPW * ZNZ
      END IF
    END SELECT
  ELSE

    ! u filetered = u from MNH
    XFMWINDU = ZWINDTMPU
    ! v filetered = v from MNH
    XFMWINDV = ZWINDTMPV
    ! w filetered = w from MNH
    XFMWINDW = ZWINDTMPW

  END IF

  ! Compute scalar product
  PFIREWIND = XFMWINDU * ZNX + XFMWINDV * ZNY + XFMWINDW * ZNZ

  IF (LWINDFILTER) THEN
    SELECT CASE(CWINDFILTER)
    CASE('EWAM')
      ! HWS filetered from PFIREWIND
      XFMHWS = PFIREWIND

    CASE('WLIM')
      ! filter HWS
      WHERE(XFMHWS <= XWLIMUTH)
        ! do not change HWS
        XFMHWS = PFIREWIND
      ELSEWHERE
        XFMHWS = MIN(PFIREWIND,XFMHWS + .005*(XWLIMUTMAX - PFIREWIND))
      END WHERE
    END SELECT
    ! update PFIREWIND with filtered value
    PFIREWIND = XFMHWS
  ELSE
    ! HWS filetered from PFIREWIND
    XFMHWS = PFIREWIND
  END IF

END SUBROUTINE FIRE_GETWIND


SUBROUTINE FIRE_RATEOFSPREAD( PFMFUELTYPE, PFMR0, PFMRFA, PFMWF0, PFMR00, PFIREWIND, &
                              PGRADLSPHIX, PGRADLSPHIY, PFMGRADOROX, PFMGRADOROY, PFIRERW )
  !!****  *FIRE_NOWINDROS* - Fire model computation of horizontal wind on fire mesh
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute horizontal wind on fire mesh
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    Use Balbi rate of spread parameterization to compute ROS with wind and slope.
  !!    Wind and slope contributions to ROS are optional and can be selected through NWINDSLOPECPLMODE parameter.
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !! 
  !!    Santoni et al. 2011
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODI_FIRE_MODEL, ONLY: FIRE_ROSDIFFU
  USE MODE_MPPDB
  USE MODD_TIME_n, ONLY : TDTCUR

  IMPLICIT NONE
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Wind on fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFIREWIND       ! Wind on fire grid on -grad phi direction

  !! Rate of spread and factors
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMR0            ! Rate of spread without wind (R0)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMRFA           ! Radiant factor (A)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMWF0           ! Vertical flame velocity (v0)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMR00           ! Flame thickness speed factor (r0)
  
  !! Fuel type
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMFUELTYPE      ! Fuel type
  
  !! Slope related
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIX     ! Grad Phi on x direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PGRADLSPHIY     ! Grad Phi on y direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROX     ! Orography gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),    INTENT(IN)      ::  PFMGRADOROY     ! Orography gradient on y direction (dz/dy) [m/m]
  
  !! other
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)   ::  PFIRERW          ! Rate of spread with wind (R)

  !*       0.2   declarations of local variables

  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZTANGAMMA     ! tan of Flame tilt angle (tan(gamma))
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZGEOMFACTOR  ! Geometric factor
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZRTEMP       ! Work ROS
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZTANALPHA     ! tan(alpha) : slope angle in spread direction
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZGRADNORM     ! norm of tilde(n)
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZGRADDIRX     ! tilde(n) on x direction
  REAL,  DIMENSION(SIZE(PFIRERW,1),SIZE(PFIRERW,2),SIZE(PFIRERW,3))    ::  ZGRADDIRY     ! tilde(n) on y direction

  ! loop, tests
  INTEGER :: JI, JJ
  !----------------------------------------------------------------------------------------------
  !

  !*      1. Compute flame tilt angle alpha

  ! Slope contributes to tilt angle
  ! Compute normale in horizontal plane
  ZGRADNORM = SQRT(PGRADLSPHIX**2 + PGRADLSPHIY**2)

  ! Normalize slope projection vectors on -gradphi vector
  WHERE (ZGRADNORM > 0.)
      ZGRADDIRX = -1. *  PGRADLSPHIX / ZGRADNORM
      ZGRADDIRY = -1. * PGRADLSPHIY / ZGRADNORM
  ELSEWHERE
      ZGRADDIRX = 0.
      ZGRADDIRY = 0.
  END WHERE
  ! tilde(n) dot h in the horizontal plane
  ZTANALPHA = ZGRADDIRX * PFMGRADOROX + ZGRADDIRY * PFMGRADOROY

  !*      2. Compute tan(gamma)

  SELECT CASE(NWINDSLOPECPLMODE)
  CASE(0)
    ! Slope and wind do contribute to tilt angle
    WHERE (PFMWF0 > 0)
      ZTANGAMMA = ZTANALPHA + PFIREWIND/PFMWF0
    ELSEWHERE
      ZTANGAMMA = ZTANALPHA
    END WHERE

  CASE(1)
    ! Slope does not contribute to tilt angle but wind does
    WHERE (PFMWF0 > 0)
      ZTANGAMMA = PFIREWIND/PFMWF0
    ELSEWHERE
      ZTANGAMMA = 0.
    END WHERE

  CASE(2)
    ! Slope do contribute to tilt angle but not wind
    ZTANGAMMA = ZTANALPHA
  END SELECT

  !*      3. Compute ROS

  WHERE (ZTANGAMMA > 0)
      ! geom factor = r0 * (1. + sin(gamma) - cos(gamma)) / cos(gamma)
      ! strictly equivalent to r0 * (sqrt(1-tan(gamma)^2) + tan(gamma) - 1) which is much more computationaly efficient
      ZGEOMFACTOR = PFMR00 * (SQRT(1. + ZTANGAMMA**2) + ZTANGAMMA - 1.)
      ! Rt = R0 + A * geomfactor - r0 / cos(gamma)
      ZRTEMP = PFMR0 + PFMRFA * ZGEOMFACTOR - PFMR00 * SQRT(1. + ZTANGAMMA**2)
      ! Rw = 0.5 * (Rt + sqrt(Rt**2 + 4. * r0 * R0 / cos(gamma)))
      PFIRERW = .5 * (ZRTEMP + SQRT(ZRTEMP**2 + 4. * PFMR00 * PFMR0 * SQRT(1. + ZTANGAMMA**2)))
  ELSEWHERE
      PFIRERW = PFMR0
  END WHERE

  SELECT CASE(NWINDSLOPECPLMODE)
  CASE(0,2)
    ! Slope projection
    ! tilde(R) = R / sqrt(1+tan(alpha)^2)
    PFIRERW = PFIRERW / SQRT(1. + ZTANALPHA**2)
  END SELECT

  ! ROS diffusion
  CALL FIRE_ROSDIFFU( PFIRERW )

  ! protection
  WHERE(PFMR0 <= 0.) PFIRERW = 0.
  SELECT CASE(NWINDSLOPECPLMODE)
  CASE(0,2)
    WHERE(PFIRERW < PFMR0 / SQRT(1. + ZTANALPHA**2)) PFIRERW = PFMR0 / SQRT(1. + ZTANALPHA**2)

  CASE(1)
    WHERE(PFIRERW < PFMR0) PFIRERW = PFMR0
  END SELECT

  ! Walking Ignition
  WHERE(PFMFUELTYPE < 0) PFIRERW = -1. * PFMFUELTYPE

END SUBROUTINE FIRE_RATEOFSPREAD


SUBROUTINE FIRE_HEATFLUXES( PLSPHI, PBMAP, PFIRETAU, PATMDT, PFLUXPARAMH, PFLUXPARAMW, PFMFLUXHDH, PFMFLUXHDW, PFMASE, PFMAWC )
  !!****  *FIRE_HEATFLUXES* - Fire model computation of heat fluxes
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute sensible and latent heat fluxes
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    See A. Costes PhD [2021], chapter 2 Section 3.3
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  USE MODI_FIRE_MODEL, ONLY: FIRE_SUBGRIDSURFACE
  USE MODD_FIELD_n, ONLY : XLSPHI2D, XSURFRATIO2D
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PLSPHI         ! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PBMAP         ! Burning map
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFIRETAU      ! Residence time and fluxes parameters map
  REAL,                     INTENT(IN)    ::  PATMDT        ! Atm time step
  ! Heat Flux param
  REAL,  DIMENSION(:,:,:,:), INTENT(INOUT) ::  PFLUXPARAMH   ! Sensible heat flux parameters
  REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) ::  PFLUXPARAMW   ! Latent heat flux parameters
  ! Heat Flux out
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMFLUXHDH    ! Surface sensible heat flux (W/m2), fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFMFLUXHDW     ! Surface water flux (kg/m2/s), fire grid

  ! Available energy
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PFMASE        ! Available sensible energy (J/m2)
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PFMAWC        ! Available water content (kg/m2)

  !*       0.2   declarations of local variables
  ! Heat Flux
  REAL,  DIMENSION(SIZE(PBMAP,1),SIZE(PBMAP,2),SIZE(PBMAP,3))    ::  ZSURFRATIO      ! Burning surface ratio 3D

  REAL,  DIMENSION(SIZE(PBMAP,1),SIZE(PBMAP,2),SIZE(PBMAP,3))    ::  ZWORK,ZWORK2,ZWORK3    ! Work array

  INTEGER     :: INBPARAMSENSIBLE, INBPARAMLATENT
  INTEGER     :: IIU,IJU,IKU     ! Atmospheric mesh used for fire mesh wind computation
  REAL        :: ZMAXARRIVALTIME      ! t+dt/2 as time integration of LS function is already performed

  REAL, PARAMETER :: ZMINENERGY = 1.0E-6

  ! loop, tests
  INTEGER :: JI, JJ, JK, JL, JM
  INTEGER :: II, IJ, IK, IA, IB, IL, IM
  !----------------------------------------------------------------------------------------------
  !
  !*      1. Get time and indexes

  ZMAXARRIVALTIME = TDTCUR%XTIME + PATMDT

  ! Get atm mesh concerned by routine
  IIU = SIZE(PBMAP,1)
  IJU = SIZE(PBMAP,2)
  IKU = SIZE(PBMAP,3)

  !*      2. get surface ratio for each fire cell

  !*      2.1 LSPHI 3D -> 2D

  ! get l and m to find PHI2D(l,m) = PHI3D(i,j,k)
  DO JK = 1, IKU
    ! b = (k-1) \ NREFINX + 1 where \ means euclidian division
    ! as k,1 and NREFINX are integers, (k-1)/NREFINX is an integer division
    IB = (JK - 1) / NREFINX + 1
    ! a = k - (b-1)*NREFINX
    IA = JK - (IB - 1) * NREFINX
    !
    DO JJ = 1, IJU
      ! m = (j-1)*NREFINY + b
      IM = (JJ - 1) * NREFINY + IB
      !
      DO JI = 1, IIU
        ! l = (i-1)*NREFINX + a
        IL = (JI - 1) * NREFINX + IA
        !  PHI2D(l,m) = PHI3D(i,j,k)
        XLSPHI2D(IL,IM) = PLSPHI(JI,JJ,JK)
      END DO
    END DO
  END DO
  !*      2.2 get surface ratio 2D

  ! -----------------------------------------------------------------------------
  !                    OLD METHOD WITHOUT RECONSTRUCTION
  ! -----------------------------------------------------------------------------
  IF (LSGBAWEIGHT) THEN
    !
    DO JM = 2, IJU*NREFINY - 1
      DO JL = 2, IIU*NREFINX - 1
        XSURFRATIO2D(JL,JM) = 9. / 16. * XLSPHI2D(JL,JM) &
                            + 3. / 32. * (XLSPHI2D(JL-1,JM) + XLSPHI2D(JL,JM-1) + XLSPHI2D(JL+1,JM) + XLSPHI2D(JL,JM+1)) &
                            + 1. / 64. * (XLSPHI2D(JL-1,JM-1) + XLSPHI2D(JL+1,JM-1) + XLSPHI2D(JL+1,JM+1) + XLSPHI2D(JL-1,JM+1))
      END DO
    END DO
  ! -----------------------------------------------------------------------------
  ELSE

    CALL FIRE_SUBGRIDSURFACE( XLSPHI2D, XSURFRATIO2D )

  END IF

  !*      2.3 convert surface ratio 2D to 3D

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  DO JM = 1, IJU*NREFINY
    ! j = ceil(m/NREFINY)
    IJ = CEILING(REAL(JM) / REAL(NREFINY))
    ! b = m - (j-1) * NREFINY
    IB = JM - (IJ - 1) * NREFINY
    !
    DO JL = 1, IIU*NREFINX
      ! i = ceil(l/NREFINX)
      II = CEILING(REAL(JL) / REAL(NREFINX))
      ! a = l - (i-1) * NREFINX
      IA = JL - (II - 1) * NREFINX
      ! k = (b-1) * NREFINX + a
      IK = (IB - 1) * NREFINX + IA
      !  GRAD3D(i,j,k) = GRAD2D(l,m)
      ZSURFRATIO(II,IJ,IK) = XSURFRATIO2D(JL,JM)
    END DO
  END DO

  !*      3. Sensible Heat flux

  ! init fire resolution flux
  PFMFLUXHDH(:,:,:) = 0.

  !*      3.1 Injection value function of models

  SELECT CASE (CHEAT_FLUX_MODEL)
  CASE('CST')
    ! Nominal value injection everywhere
    ! Effective injection if energy is Available
    PFMFLUXHDH(:,:,:) = ZSURFRATIO(:,:,:) * PFLUXPARAMH(:,:,:,1)
    
  CASE('EXP')
    ! NBPARAM
    ! INBPARAMSENSIBLE = 2
    ! get heat flux params
    ! Phie
    ZWORK(:,:,:)  = PFLUXPARAMH(:,:,:,1)
    ! Taue
    ZWORK2(:,:,:) = PFLUXPARAMH(:,:,:,2)
    !
    WHERE( PBMAP >= 0 .AND. ZWORK2 > 0)
      PFMFLUXHDH = ZSURFRATIO * ZWORK * EXP(-1. * (TDTCUR%XTIME - PBMAP) / ZWORK2)
    ELSEWHERE
      PFMFLUXHDH = ZSURFRATIO * ZWORK
    END WHERE
    ! Minimal flux injection 0.001*Phie
    WHERE(PFMFLUXHDH < .01 * ZWORK) PFMFLUXHDH = ZSURFRATIO * .01 * ZWORK

  CASE('EXS')
    ! NBPARAM
    ! INBPARAMSENSIBLE = 3
    ! get heat flux params
    ! Phie
    ZWORK(:,:,:)  = PFLUXPARAMH(:,:,:,1)
    ! Taue
    ZWORK2(:,:,:) = PFLUXPARAMH(:,:,:,2)
    ! phis
    ZWORK3(:,:,:) = PFLUXPARAMH(:,:,:,3)
    !
    WHERE( PBMAP >= 0 .AND. ZWORK2 > 0)
      PFMFLUXHDH = ZSURFRATIO * (ZWORK * EXP(-1. * (TDTCUR%XTIME - PBMAP) / ZWORK2) + ZWORK3)
    ELSEWHERE
      PFMFLUXHDH = ZSURFRATIO * (ZWORK + ZWORK3)
    END WHERE

  END SELECT

  !*      3.2 Injection limitation function of Available energy
  ! Check if heat flux if below Available sensible energy
  WHERE ( PFMASE < ZMINENERGY )
    PFMFLUXHDH = 0.
  END WHERE

  !
  WHERE ( PFMASE < PFMFLUXHDH * PATMDT)
    PFMFLUXHDH = PFMASE / PATMDT
  END WHERE

  ! Remove injected energy from Available energy
  PFMASE(:,:,:) = PFMASE(:,:,:) - PFMFLUXHDH(:,:,:) * PATMDT

  !*      4. Latente Heat flux

  ! init fire resolution flux
  PFMFLUXHDW(:,:,:) = 0.

  !*      4.1 CST model

  SELECT CASE (CLATENT_FLUX_MODEL)
  CASE('CST')
    ! Nominal value injection everywhere
    ! Effective injection if energy is Available
    PFMFLUXHDW(:,:,:) = ZSURFRATIO(:,:,:) * PFLUXPARAMW(:,:,:,1)

  CASE('EXP')
    ! NBPARAM
    ! INBPARAMLATENT = 2
    ! get latent flux params
    ! Phie
    ZWORK(:,:,:)  = PFLUXPARAMW(:,:,:,1)
    ! Taue
    ZWORK2(:,:,:) = PFLUXPARAMW(:,:,:,2)
    !
    WHERE( PBMAP >= 0 .AND. ZWORK2 > 0)
      PFMFLUXHDW = ZSURFRATIO * ZWORK * EXP(-1. * (TDTCUR%XTIME - PBMAP) / ZWORK2)
    ELSEWHERE
      PFMFLUXHDW = ZSURFRATIO * ZWORK
    END WHERE
    ! Minimal flux injection 0.001*Phie
    WHERE(PFMFLUXHDW < .01 * ZWORK) PFMFLUXHDW = ZSURFRATIO * .01 * ZWORK
  END SELECT

  ! Check if heat flux if below Available sensible energy
  WHERE ( PFMAWC < ZMINENERGY )
    PFMFLUXHDW = 0.
  END WHERE

  !
  WHERE ( PFMAWC < PFMFLUXHDW * PATMDT)
    PFMFLUXHDW = PFMAWC / PATMDT
  END WHERE

  ! Remove injected energy from Available energy
  PFMAWC(:,:,:) = PFMAWC(:,:,:) - PFMFLUXHDW(:,:,:) * PATMDT

END SUBROUTINE FIRE_HEATFLUXES


SUBROUTINE FIRE_VERTICALFLUXDISTRIB( PFMFLUXHDH, PFMFLUXHDW, PRTHS, PRRS, PSFTS, PEXNREF, PRHODJ, PRT, PRHODREF )
  !!****  *FIRE_VERTICALFLUXDISTRIB* - Fire model vertical distribution of heat fluxes
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute vertical distribution of sensible and latent heat fluxes
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    See A. Costes PhD [2021]
  !!
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_CST
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  !USE MODI_FIRE_MODEL
  USE MODD_GRID_n, ONLY : XZS, XZZ
  USE MODD_NSV
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  ! Heat Flux in
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFMFLUXHDH    ! Surface sensible heat flux (W/m2), fire grid
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PFMFLUXHDW     ! Surface water flux (kg/m2/s), fire grid

  ! Sources
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PRTHS          ! Potential temperature increment (K kg/s)
  REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) ::  PRRS          ! Water content increment (kg/s)
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT) ::  PSFTS          ! smoke flux (kg/kg/m2/s)

  ! Other fields
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PEXNREF        ! Exner function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PRHODJ        ! density times atm cell volume rho*J
  REAL,  DIMENSION(:,:,:,:),  INTENT(IN)    ::  PRT            ! Water content (kg/kg)
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PRHODREF      ! reference profile of density

  !*       0.2   declarations of local variables
  ! Heat Flux
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2))                 ::  ZFLUXATMH   ! Sensible heat flux on atm grid
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2))                 ::  ZFLUXATMW   ! Latent heat flux on atm grid
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2),SIZE(PRTHS,3))   ::  ZCPH        ! Cph (only gaz considered for virtual temperatures)
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2),SIZE(PRTHS,3))   ::  ZRVT        ! water vapor mixing ratio
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2),SIZE(PRTHS,3))   ::  ZFLUXCOEF   ! distributed coefficient
  REAL,  DIMENSION(SIZE(PRTHS,1),SIZE(PRTHS,2),SIZE(PRTHS,3))   ::  ZHZ         ! Flux point height
  !

  INTEGER :: IKB, IKE, IKU
  INTEGER :: IZMAX      ! index of max injection height
  REAL    :: ZZMAXLOCAL ! local max injection height
  REAL    :: ZINJECTTMP ! total injecction before correction

  ! loop, tests
  INTEGER :: JI, JJ, JK, JKK
  INTEGER :: JSV
  !----------------------------------------------------------------------------------------------
  !
  !*      1. Set Flux on atm grid

  ! Mean sensible heat flux from fire grid to atm grid
  ZFLUXATMH(:,:) =  SUM(PFMFLUXHDH(:,:,:),DIM=3) / (NREFINX * NREFINY)

  ! Mean latent heat flux from fire grid to atm grid
  ZFLUXATMW(:,:) =  SUM(PFMFLUXHDW(:,:,:),DIM=3) / (NREFINX * NREFINY)

  !*      2. Compute Cph

  ! check humidity
  IF(SIZE(PRT,4) /= 0) THEN
    ZRVT(:,:,:) = PRT(:,:,:,1)
  ELSE
    ZRVT(:,:,:) = 0.
  END IF

  ! todo: add liquid and solid contribution to Cph
  !       see dyn_sources.f90 for example
  ! Cph = Cpd + Cpv * rv
  ZCPH = XCPD + XCPV * ZRVT

  !*      3. Compute distributed fluxes

  ! top index
  IKU = SIZE(PRTHS,3)
  IKE = IKU - 1
  ! bottom physical index
  IKB = 2

  ! Ensure zf > 0
  IF (XFLUXZEXT <= 0) XFLUXZEXT = 1.

  ! get height instead of altitude
  DO JK=1,IKU
    DO JJ=1,SIZE(PRTHS,2)
      DO JI=1,SIZE(PRTHS,1)
        ZHZ(JI,JJ,JK) = XZZ(JI,JJ,JK) - XZZ(JI,JJ,IKB)
      END DO
    END DO
  END DO

  ! compute distribution
  DO JJ=1,SIZE(PRTHS,2)
    DO JI=1,SIZE(PRTHS,1)
      ! Ensure 0 < zmax < ztop-1 domain
      ZZMAXLOCAL = MAX(1E-1,MIN(ZHZ(JI,JJ,IKE),XFLUXZMAX))
      DO JK=IKB,IKU
        IF (ZHZ(JI,JJ,JK) < ZZMAXLOCAL) THEN
          ! Flux distribution coef
          ! Coef = (exp(-z(k)/zf) - exp(- min(z(k+1),zmax)/zf)) / ((1 - exp(-zmax/zf))*(z(k+1)-z(k)))
          ! sensible
          ZFLUXCOEF(JI,JJ,JK) = (EXP(-1.*ZHZ(JI,JJ,JK)/XFLUXZEXT) - EXP(-1.*MIN(ZHZ(JI,JJ,JK+1),ZZMAXLOCAL)/XFLUXZEXT)) / &
                                ((1. - EXP(-1.*ZZMAXLOCAL/XFLUXZEXT))*(ZHZ(JI,JJ,JK+1)-ZHZ(JI,JJ,JK)))
        ELSE
          ZFLUXCOEF(JI,JJ,JK) = 0.
        END IF
      END DO
    END DO
  END DO

  !*      4. Set theta and rv sources

  ! sensible
  ! RTHS += rho*J*Psi_h / (Pi_ref * Cph)
  DO JJ=1,SIZE(PRTHS,2)
    DO JI=1,SIZE(PRTHS,1)
      DO JK=IKB,IKU
        PRTHS(JI,JJ,JK) = PRTHS(JI,JJ,JK) + PRHODJ(JI,JJ,JK) * ZFLUXATMH(JI,JJ) * ZFLUXCOEF(JI,JJ,JK) &
                          / (PEXNREF(JI,JJ,JK) * ZCPH(JI,JJ,JK))
      END DO
    END DO
  END DO

  ! latent
  ! RRS += rho*J*Psi_w / rho_ref
  IF(SIZE(PRT,4) /= 0) THEN
    DO JJ=1,SIZE(PRTHS,2)
      DO JI=1,SIZE(PRTHS,1)
        DO JK=IKB,IKU
          PRRS(JI,JJ,JK,1)  = PRRS(JI,JJ,JK,1)  + PRHODJ(JI,JJ,JK) * ZFLUXATMW(JI,JJ) * ZFLUXCOEF(JI,JJ,JK) / PRHODREF(JI,JJ,JK)
        END DO
      END DO
    END DO
  END IF


  !*      5. Set smoke source
  ! :tmp: smoke flux is proportional to sensible heat flux
  DO JSV=1,NSV_FIRE
    PSFTS(:,:,NSV_FIREBEG-1+JSV) = ZFLUXATMH / 1E5
  END DO

END SUBROUTINE FIRE_VERTICALFLUXDISTRIB


SUBROUTINE FIRE_READFUEL( TPFILE, PFIREFUELMAP, PFMIGNITION, PFMWALKIG )
  !!****  *FIRE_READFUEL* - Fire model read FuelMap.nc file
  !!
  !!    PURPOSE
  !!    -------
  !!    Read FuelMap.nc file to get fuel properties
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIELD,            ONLY: NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NOTLISTED, TFIELDMETADATA, TYPEREAL
  USE MODD_FIRE,             ONLY: NFIREENTRIES
  USE MODD_IO,               ONLY: TFILEDATA
  USE MODE_IO_FIELD_READ,    ONLY: IO_Field_read
  USE MODE_IO_FILE,          ONLY: IO_File_close, IO_File_open
  USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST
  USE MODD_LUNIT_n,          ONLY: TLUOUT
  USE MODD_PARAMETERS,       ONLY: NMNHNAMELGTMAX, NUNITLGTMAX
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE         ! Synchronous output file
  REAL,  DIMENSION(:,:,:,:), INTENT(OUT)   :: PFIREFUELMAP   ! Fuel map
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)   :: PFMIGNITION    ! Ignition map
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)   :: PFMWALKIG      ! Walking Ignition map

  !*       0.2   declarations of local variables

  CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(NFIREENTRIES), PARAMETER ::  &
    CFIREFUELMAP_NAME = [                                               &
      'Fuel_type', 'rhod     ', 'rhol     ', 'Md       ', 'Ml       ',  &
      'sd       ', 'sl       ', 'sigmad   ', 'sigmal   ', 'e        ',  &
      'Ti       ', 'Ta       ', 'DeltaH   ', 'Deltah   ', 'tau0     ',  &
      'stoch    ', 'rhoa     ', 'cp       ', 'cpa      ', 'X0       ',  &
      'LAI      ', 'r00      ' ]

  CHARACTER(LEN=NUNITLGTMAX), DIMENSION(NFIREENTRIES), PARAMETER ::         &
    CFIREFUELMAP_UNITS = [                                                  &
      '1         ', 'kg m-3    ', 'kg m-3    ', '-         ', '-         ', &
      'm-1       ', 'm-1       ', 'kg m-2    ', 'kg m-2    ', 'm         ', &
      'K         ', 'K         ', 'J kg-1    ', 'J kg-1    ', 's m-1     ', &
      '-         ', 'kg m-3    ', 'J K-1 kg-1', 'J K-1 kg-1', '-         ', &
      '-         ', '-         ' ]

  ! Comments not stored (not very useful and not necessary for checks at reading)
  ! CHARACTER(LEN=NCOMMENTLGTMAX), DIMENSION(NFIREENTRIES), PARAMETER :: CFIREFUELMAP_COMMENT

  CHARACTER(LEN=6) :: YFUELNAME
  INTEGER                               :: ILUOUT          ! Logical unit number for the output listing
  TYPE(TFILEDATA), POINTER              :: TFUELFILE       ! FuelMap file
  TYPE(TFIELDMETADATA)                  :: TZFIELD         ! Field type
  ! loop, tests
  INTEGER :: JFUEL
  !----------------------------------------------------------------------------------------------
  !
  TFUELFILE => NULL()
  !*      1. Get logical number for Outputlisting

  ILUOUT = TLUOUT%NLU

  WRITE(UNIT=ILUOUT,FMT=*) '************************************'
  WRITE(UNIT=ILUOUT,FMT=*) '**** Fire model fuel extraction ****'
  WRITE(UNIT=ILUOUT,FMT=*) '************************************'

  !*      2. Open file

  CALL IO_FILE_ADD2LIST( TFUELFILE, 'FuelMap', 'MNH', 'READ', HFORMAT = 'NETCDF4', KLFITYPE = 2 )
  CALL IO_File_open( TFUELFILE )

  !*      3. Read fuels properties

  DO JFUEL = 1, NFIREENTRIES
    TZFIELD = TFIELDMETADATA( &
      CMNHNAME   = TRIM( CFIREFUELMAP_NAME(JFUEL) ), &
      CSTDNAME   = '',        &
      CLONGNAME  = TRIM( CFIREFUELMAP_NAME(JFUEL) ), &
      CUNITS     = TRIM( CFIREFUELMAP_UNITS(JFUEL) ),        &
      CDIR       = 'XY',      &
      NGRID      = 4,         &
      NTYPE      = TYPEREAL,  &
      NDIMS      = 3,         &
      NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NOTLISTED ], &
      LTIMEDEP   = .FALSE.    )

    WRITE(UNIT=ILUOUT,FMT=*) 'Extract ', TRIM( CFIREFUELMAP_NAME(JFUEL) )

    ! Import data from file
    CALL IO_Field_read( TFUELFILE, TZFIELD, PFIREFUELMAP(:,:,:,JFUEL) )
  END DO

  !*      4. read ignition map file

  WRITE(UNIT=ILUOUT,FMT=*) 'Extract Ignition'

  TZFIELD = TFIELDMETADATA(       &
    CMNHNAME   = 'Ignition',      &
    CSTDNAME   = '',              &
    CLONGNAME  = 'Ignition time', &
    CUNITS     = 's',             &
    CDIR       = 'XY',            &
    NGRID      = 4,               &
    NTYPE      = TYPEREAL,        &
    NDIMS      = 3,               &
    NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NOTLISTED ], &
    LTIMEDEP   = .FALSE.,         &
    CCOMMENT   = 'Ignition map'   )

  ! Import data from file
  CALL IO_Field_read( TFUELFILE, TZFIELD, PFMIGNITION )

  !*      5. read walking ignition map file

  WRITE(UNIT=ILUOUT,FMT=*) 'Extract Walking Ignition'

  TZFIELD = TFIELDMETADATA(               &
    CMNHNAME   = 'WalkingIgnition',       &
    CSTDNAME   = '',                      &
    CLONGNAME  = 'Walking ignition time', &
    CUNITS     = 's',                     &
    CDIR       = 'XY',                    &
    NGRID      = 4,                       &
    NTYPE      = TYPEREAL,                &
    NDIMS      = 3,                       &
    NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NOTLISTED ], &
    LTIMEDEP   = .FALSE.,                 &
    CCOMMENT   = 'WalkingIgnition map'    )

  ! Import data from file
  CALL IO_Field_read( TFUELFILE, TZFIELD, PFMWALKIG )

  !*      6. close file

  CALL IO_File_close( TFUELFILE )
  WRITE(UNIT=ILUOUT,FMT=*) '*************** Done ***************'

END SUBROUTINE FIRE_READFUEL


SUBROUTINE FIRE_READBMAP(TPFILE, PBMAP)
  !!****  *FIRE_NOWINDROS* - Fire model read bmap file
  !!
  !!    PURPOSE
  !!    -------
  !!    Read Bmap file
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE,             ONLY: CBMAPFILE
  USE MODD_IO,               ONLY: TFILEDATA
  USE MODE_IO_FIELD_READ,    ONLY: IO_Field_read
  USE MODE_IO_FILE,          ONLY: IO_File_close, IO_File_open
  USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST
  USE MODD_LUNIT_n,          ONLY: TLUOUT
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  TYPE(TFILEDATA),        INTENT(IN)    :: TPFILE    ! Synchronous output file
  REAL,  DIMENSION(:,:,:),  INTENT(OUT)   :: PBMAP     ! Bmap

  !*       0.2   declarations of local variables

  INTEGER                               :: ILUOUT               ! Logical unit number for the output listing
  TYPE(TFILEDATA), POINTER              :: TFUELFILE            ! FuelMap file
  !----------------------------------------------------------------------------------------------
  !
  TFUELFILE => NULL()
  !*      1. Get logical number for Outputlisting

  ILUOUT = TLUOUT%NLU

  WRITE(UNIT=ILUOUT,FMT=*) '************************************'
  WRITE(UNIT=ILUOUT,FMT=*) '**** Fire model BMap extraction ****'
  WRITE(UNIT=ILUOUT,FMT=*) '************************************'

  !*      2. Open file

  CALL IO_FILE_ADD2LIST( TFUELFILE, CBMAPFILE, 'MNH', 'READ', HFORMAT = 'NETCDF4', KLFITYPE = 2 )
  CALL IO_File_open( TFUELFILE )

  !*      4. read Burning map file

  WRITE(UNIT=ILUOUT,FMT=*) 'Extract BMap'

  ! Import data from file
  CALL IO_Field_read( TFUELFILE, 'BMAP', PBMAP )

  !*      5. Close file

  CALL IO_File_close( TFUELFILE )

  WRITE(UNIT=ILUOUT,FMT=*) '*************** Done ***************'

END SUBROUTINE FIRE_READBMAP


SUBROUTINE FIRE_RK( PLSPHI, PLSPHI1, PGRADLSPHIX, PGRADLSPHIY, PFIRERW, PFIREDT )
  !!****  *FIRE_RK * - routine to call the specialized advection routines for phi
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
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    27/11/2019
  !!
  !*       0.    DECLARATIONS
  !              ------------
  !
  USE MODE_ll
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
  USE MODD_PARAMETERS,  ONLY : JPVEXT
  USE MODD_CONF,        ONLY : NHALO
  USE MODD_LUNIT_n,        ONLY: TLUOUT
  USE MODD_PRECISION,      ONLY: MNHTIME
  !
  USE MODI_GET_HALO
  USE MODE_MPPDB
  USE MODE_MSG
  USE MODD_FIRE
  USE MODI_FIRE_MODEL, ONLY: FIRE_GRADPHI, FIRE_LSDIFFU
  use MODE_MNH_TIMING,  ONLY : SECOND_MNH2
  !
  IMPLICIT NONE
  !
  !*       0.1   Declarations of dummy arguments :
  !
  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)     ::  PLSPHI       ! Level Set function at time t
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PLSPHI1      ! Level Set function at time t+dtfire

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PGRADLSPHIX  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PGRADLSPHIY  ! Grad of Phi on y direction
  !
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PFIRERW      ! Rate of spread with wind
  !
  REAL,                     INTENT(IN)    :: PFIREDT      ! Fire time step dtfire
  !
  !*       0.2   declarations of local variables
  !
  REAL, DIMENSION(SIZE(PLSPHI,1),SIZE(PLSPHI,2),SIZE(PLSPHI,3)) :: ZLSPHIRK ! Intermediate Guesses inside the RK loop
  !
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE                         :: ZGRADPHIXRK ! RK loop gradient of phi in x direction
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE                         :: ZGRADPHIYRK ! RK loop gradient of phi in y direction
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE                         :: ZPHIDIFFRKX ! RK loop laplacian of phi in x direction
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE                         :: ZPHIDIFFRKY ! RK loop laplacian of phi in y direction

  REAL, DIMENSION(:,:), ALLOCATABLE :: ZBUT ! Butcher array coefficients
                                            ! at the RK sub time step
  REAL, DIMENSION(:),   ALLOCATABLE :: ZBUTS! Butcher array coefficients
                                            ! at the end of the RK loop

  TYPE(LIST_ll), POINTER      :: TZFIELDFIRE_ll ! list of fields to exchange
  INTEGER                     :: INBVAR
  INTEGER           :: ILUOUT         ! logical unit
  INTEGER :: IIU, IJU, IKU ! array sizes

  ! Momentum tendencies due to advection
  INTEGER :: ISPL                ! Number of RK splitting loops
  INTEGER :: JI, JS              ! Loop index
  !
  INTEGER                     :: IINFO_ll    ! return code of parallel routine
  TYPE(LIST_ll), POINTER      :: TZFIELDS4_ll ! list of fields to exchange
  !
  REAL          :: XPRECISION
  REAL(KIND=MNHTIME), DIMENSION(2) :: ZGRADTIME1, ZGRADTIME2
  !-------------------------------------------------------------------------------
  !
  !*       0.     INITIALIZATION
  !          --------------------
  !
  ILUOUT=TLUOUT%NLU
  !
  IIU=SIZE(PLSPHI,1)
  IJU=SIZE(PLSPHI,2)
  IKU=SIZE(PLSPHI,3)
  !
  SELECT CASE (NFIRE_RK_ORDER)
  CASE(1)
    ISPL = 1

  CASE(2)
    ISPL = 2

  CASE(3)
    ISPL = 3

  CASE(4)
    ISPL = 4

  CASE(5)
    ISPL = 5

  CASE(6)
    ISPL = 6

  CASE DEFAULT
    WRITE(ILUOUT,'(A/A)') 'ERROR BLAZE-FIRE : ERROR: UNKNOWN NFIRE_RK_ORDER'
    !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','FIREMODEL','')
  END SELECT
  !
  ALLOCATE(ZBUT(ISPL-1,ISPL-1))
  ALLOCATE(ZBUTS(ISPL))
  !
  SELECT CASE (NFIRE_RK_ORDER)
  CASE(1)
    ZBUTS = (/ 1. /)

  CASE(2)
    ZBUTS     = (/ 0. , 1. /)
    ZBUT(1,1)   = 3./4.

  CASE(3)
    ZBUTS     = (/ 1./6. , 1./6. , 2./3. /)
    ZBUT(1,1) = 1.
    ZBUT(1,2) = 0.
    ZBUT(2,1) = 1./4.
    ZBUT(2,2) = 1./4.

  CASE(4)
    ZBUTS     = (/ 1./6. , 1./3. , 1./3. , 1./6./)
    ZBUT      = 0.
    ZBUT(1,1) = 1./2.
    ZBUT(2,2) = 1./2.
    ZBUT(3,3) = 1.

  CASE(5)
    ZBUTS     = (/ 1./4. , 0. , 0. , 0. , 3./4. /)
    ZBUT      = 0.
    ZBUT(1,1) = 1./7.
    ZBUT(2,2) = 3./16.
    ZBUT(3,3) = 1./3.
    ZBUT(4,4) = 2./3.

  CASE(6)
    ZBUTS= (/ 7./90. , 0. , 16./45. , 2./15. , 16./45. , 7./90. /)
    ZBUT= 0.
    ZBUT(1,1) = 1./4.
    ZBUT(2,1) = 1./8.
    ZBUT(2,2) = 1./8.
    ZBUT(3,1) = 0
    ZBUT(3,2) = -1./2.
    ZBUT(3,3) = 1
    ZBUT(4,1) = 3./16.
    ZBUT(4,2) = 0
    ZBUT(4,3) = 0
    ZBUT(4,4) = 9./16.
    ZBUT(5,1) = -3./7.
    ZBUT(5,2) = 2./7.
    ZBUT(5,3) = 12./7.
    ZBUT(5,4) = -12./7.
    ZBUT(5,5) = 8./7.
  END SELECT
  !
  ALLOCATE(ZGRADPHIXRK(SIZE(PLSPHI,1),SIZE(PLSPHI,2),SIZE(PLSPHI,3),ISPL))
  ALLOCATE(ZGRADPHIYRK(SIZE(PLSPHI,1),SIZE(PLSPHI,2),SIZE(PLSPHI,3),ISPL))
  ALLOCATE(ZPHIDIFFRKX(SIZE(PLSPHI,1),SIZE(PLSPHI,2),SIZE(PLSPHI,3),ISPL))
  ALLOCATE(ZPHIDIFFRKY(SIZE(PLSPHI,1),SIZE(PLSPHI,2),SIZE(PLSPHI,3),ISPL))
  !
  PLSPHI1 = PLSPHI
  !
  !*       2.     Wind guess before RK loop
  !          -------------------------------
  ZLSPHIRK = PLSPHI

  NULLIFY(TZFIELDFIRE_ll)
  CALL ADD3DFIELD_ll(TZFIELDFIRE_ll, ZLSPHIRK, 'MODEL_n::ZLSPHIRK')
  !
  ZGRADPHIXRK(:,:,:,:) = 0.
  ZGRADPHIYRK(:,:,:,:) = 0.
  ZPHIDIFFRKX(:,:,:,:) = 0.
  ZPHIDIFFRKY(:,:,:,:) = 0.
  !
  !*       3.     BEGINNING of Runge-Kutta loop
  !          -----------------------------------
  !
  DO JS = 1, ISPL
    CALL UPDATE_HALO_ll( TZFIELDFIRE_ll, IINFO_ll )
    !*       4.     Advection with WENO
    !          -------------------------
    
    IF (JS > 1) THEN
      CALL SECOND_MNH2( ZGRADTIME1 )
      CALL FIRE_GRADPHI( ZLSPHIRK, ZGRADPHIXRK(:,:,:,JS), ZGRADPHIYRK(:,:,:,JS) )
      CALL SECOND_MNH2( ZGRADTIME2 )
      XGRADPERF = XGRADPERF + ZGRADTIME2 - ZGRADTIME1
      XPROPAGPERF = XPROPAGPERF - (ZGRADTIME2 - ZGRADTIME1)
    ELSE
      ! Use already computed gradients
      ZGRADPHIXRK(:,:,:,JS) = PGRADLSPHIX(:,:,:)
      ZGRADPHIYRK(:,:,:,JS) = PGRADLSPHIY(:,:,:)
    END IF
    
    CALL FIRE_LSDIFFU( ZLSPHIRK, ZPHIDIFFRKX(:,:,:,JS), ZPHIDIFFRKY(:,:,:,JS) )
    
    NULLIFY(TZFIELDS4_ll)
  
    CALL ADD3DFIELD_ll( TZFIELDS4_ll, ZGRADPHIXRK(:,:,:,JS), 'MODEL_n::ZGRADPHIXRK' )
    CALL ADD3DFIELD_ll( TZFIELDS4_ll, ZGRADPHIYRK(:,:,:,JS), 'MODEL_n::ZGRADPHIYRK' )
    CALL ADD3DFIELD_ll( TZFIELDS4_ll, ZPHIDIFFRKX(:,:,:,JS), 'MODEL_n::ZPHIDIFFRKX' )
    CALL ADD3DFIELD_ll( TZFIELDS4_ll, ZPHIDIFFRKY(:,:,:,JS), 'MODEL_n::ZPHIDIFFRKY' )
    CALL UPDATE_HALO_ll( TZFIELDS4_ll, IINFO_ll )
    CALL CLEANLIST_ll( TZFIELDS4_ll )
  
    IF ( JS /= ISPL ) THEN
      ZLSPHIRK = PLSPHI
      DO JI = 1, JS
        !
        ! Intermediate guesses inside the RK loop
        !
        ZLSPHIRK(:,:,:) = ZLSPHIRK(:,:,:) + ZBUT(JS,JI) *  PFIREDT * (  &
                          PFIRERW * SQRT(ZGRADPHIXRK(:,:,:,JS)**2 + ZGRADPHIYRK(:,:,:,JS)**2) + &
                          XLSDIFFUSION * (ZPHIDIFFRKX(:,:,:,JS) + ZPHIDIFFRKY(:,:,:,JS)))
      
      END DO

      WHERE(ZLSPHIRK > 1) ZLSPHIRK = 1.
      ! diffusion protection
      WHERE(PFIRERW == 0.) ZLSPHIRK = 0.

    ELSE
      !
      ! Guesses at the end of the RK loop
      !
      DO JI = 1, ISPL
        PLSPHI1(:,:,:) = PLSPHI1(:,:,:) + ZBUTS(JI) *  PFIREDT * ( &
                         PFIRERW * SQRT(ZGRADPHIXRK(:,:,:,JI)**2 + ZGRADPHIYRK(:,:,:,JI)**2) + &
                         XLSDIFFUSION * (ZPHIDIFFRKX(:,:,:,JS) + ZPHIDIFFRKY(:,:,:,JS)))
      END DO
      ! diffusion protection
      WHERE(PFIRERW == 0.) PLSPHI1 = 0.

    END IF
    ! End of the RK loop
  END DO
  !
  DEALLOCATE(ZBUT, ZBUTS, ZGRADPHIXRK, ZGRADPHIYRK,ZPHIDIFFRKX,ZPHIDIFFRKY)
  CALL CLEANLIST_ll( TZFIELDFIRE_ll )

END SUBROUTINE FIRE_RK


SUBROUTINE FIRE_WENO_1( PLSPHI2D, PGRADLSPHIX2D, PGRADLSPHIY2D, PGRADMASKX, PGRADMASKY )
  !!****  *FIRE_WENO_1* - Fire model computation of Level set function gradient
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute gradient on x and y direcctions for level set function
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       WENO1
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:),    INTENT(IN)  ::  PLSPHI2D     ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADLSPHIX2D  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADLSPHIY2D  ! Grad of Phi on y direction
  !
  REAL,  DIMENSION(:,:),      INTENT(IN)  :: PGRADMASKX    ! mask value x
  REAL,  DIMENSION(:,:),      INTENT(IN)  :: PGRADMASKY    ! mask value y

  !*       0.2   declarations of local variables

  INTEGER :: ILU, IMU
  INTEGER :: JL, JM

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Compute 1st order upwind gradient

  PGRADLSPHIX2D = 0.
  PGRADLSPHIY2D = 0.

  ILU = SIZE(PLSPHI2D,1)
  IMU = SIZE(PLSPHI2D,2)

  ! dY = (Y(i) - Y(i-1)) / dx if Y(i+1) <= Y(i-1)
  ! dY = (Y(i+1) - Y(i)) / dx if Y(i+1) > Y(i-1)
  PGRADLSPHIX2D(2:ILU-1,:) = (PLSPHI2D(3:ILU,:) -PLSPHI2D(2:ILU-1,:)) / XFIREMESHSIZE(1) * (.5 + SIGN(.5,PGRADMASKX(2:ILU-1,:)))+&
                            (PLSPHI2D(2:ILU-1,:) - PLSPHI2D(1:ILU-2,:)) / XFIREMESHSIZE(1) * (.5 - SIGN(.5,PGRADMASKX(2:ILU-1,:)))
  !
  PGRADLSPHIY2D(:,2:IMU-1) = (PLSPHI2D(:,3:IMU) -PLSPHI2D(:,2:IMU-1)) / XFIREMESHSIZE(2) * (.5 + SIGN(.5,PGRADMASKY(:,2:IMU-1)))+&
                            (PLSPHI2D(:,2:IMU-1) - PLSPHI2D(:,1:IMU-2)) / XFIREMESHSIZE(2) * (.5 - SIGN(.5,PGRADMASKY(:,2:IMU-1)))
  !

END SUBROUTINE FIRE_WENO_1


SUBROUTINE FIRE_GRADMASK( PLSPHI2D, PGRADMASKX, PGRADMASKY, KMASKORDER )
  !!****  *FIRE_GRADMASK* - Fire model computation of Level set function gradient
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute gradient on x and y direcctions for level set function
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       Centered 2nd/4th order scheme
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:),    INTENT(IN)    ::  PLSPHI2D    ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADMASKX    ! mask value x
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADMASKY    ! mask value y
  INTEGER,                  INTENT(IN)  :: KMASKORDER    ! Difference order
  !

  !*       0.2   declarations of local variables

  INTEGER :: ILU, IMU
  INTEGER :: JL, JM

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Default value

  ILU = SIZE(PLSPHI2D,1)
  IMU = SIZE(PLSPHI2D,2)

  PGRADMASKX(:,:) = 0.
  PGRADMASKY(:,:) = 0.

  SELECT CASE(KMASKORDER)
  CASE(2)
    ! Compute 2nd order centered difference for mask value
    PGRADMASKX(2:ILU-1,:) = PLSPHI2D(3:ILU,:) - PLSPHI2D(1:ILU-2,:)
    PGRADMASKY(:,2:IMU-1) = PLSPHI2D(:,3:IMU) - PLSPHI2D(:,1:IMU-2)

  CASE(4)
    ! Compute 4nd order centered difference for mask value
    PGRADMASKX(3:ILU-2,:) = 8.*(PLSPHI2D(4:ILU-1,:) - PLSPHI2D(2:ILU-3,:)) - (PLSPHI2D(5:ILU,:) - PLSPHI2D(1:ILU-4,:))
    PGRADMASKY(:,3:IMU-2) = 8.*(PLSPHI2D(:,4:IMU-1) - PLSPHI2D(:,2:IMU-3)) - (PLSPHI2D(:,5:IMU) - PLSPHI2D(:,1:IMU-4))
    ! Compute 2nd order on bounds
    ! west
    PGRADMASKX(2,:) = PLSPHI2D(3,:) - PLSPHI2D(1,:)
    ! east
    PGRADMASKX(ILU-1,:) = PLSPHI2D(ILU,:) - PLSPHI2D(ILU-2,:)
    ! south
    PGRADMASKY(:,2) = PLSPHI2D(:,3) - PLSPHI2D(:,1)
    ! north
    PGRADMASKY(:,IMU-1) = PLSPHI2D(:,IMU) - PLSPHI2D(:,IMU-2)

  CASE DEFAULT
    ! Compute 2nd order centered difference for mask value
    PGRADMASKX(2:ILU-1,:) = PLSPHI2D(3:ILU,:) - PLSPHI2D(1:ILU-2,:)
    PGRADMASKY(:,2:IMU-1) = PLSPHI2D(:,3:IMU) - PLSPHI2D(:,1:IMU-2)
  END SELECT

END SUBROUTINE FIRE_GRADMASK


SUBROUTINE FIRE_WENO_3( PLSPHI2D, PGRADLSPHIX2D, PGRADLSPHIY2D, PGRADMASKX, PGRADMASKY )
  !!****  *FIRE_WENO_3* - Fire model computation of Level set function gradient
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute gradient on x and y direcctions for level set function
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       WENO3
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:),    INTENT(IN)    ::  PLSPHI2D     ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADLSPHIX2D  ! Grad of Phi on x direction
  REAL,  DIMENSION(:,:),      INTENT(OUT)  :: PGRADLSPHIY2D  ! Grad of Phi on y direction
  !
  REAL,  DIMENSION(:,:),      INTENT(IN)  :: PGRADMASKX    ! mask value x
  REAL,  DIMENSION(:,:),      INTENT(IN)  :: PGRADMASKY    ! mask value y

  !*       0.2   declarations of local variables

  ! Phi reconstruction
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZPHINEG    ! Phi(i+1/2)-
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZPHIPOS    ! Phi(i+1/2)+

  ! Intermediate reconstruction
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZPHINEG1, ZPHINEG2    ! Phi(i+1/2)- (1) and (2)
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZPHIPOS1, ZPHIPOS2    ! Phi(i+1/2)+ (1) and (2)

  ! Smoothness indicator
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZBETA1NEG, ZBETA2NEG    ! beta1-, beta2-
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZBETA1POS, ZBETA2POS    ! beta1+, beta2+

  ! Weno weights
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZOMEGA1NEG, ZOMEGA2NEG    ! omega1-, omega2-
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))  :: ZOMEGA1POS, ZOMEGA2POS    ! omega1+, omega2+

  ! standard weights
  REAL, PARAMETER :: ZGAMMA1 = 1./3.
  REAL, PARAMETER :: ZGAMMA2 = 2./3.
  !
  REAL, PARAMETER :: ZEPS = 1.0E-15

  INTEGER :: ILU, IMU
  INTEGER :: JL, JM

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Default value for fields

  ! Phi reconstruction
  ZPHINEG = 0.
  ZPHIPOS = 0.

  ! Intermediate reconstruction
  ZPHINEG1 = 0.
  ZPHINEG2 = 0.
  ZPHIPOS1 = 0.
  ZPHIPOS2 = 0.

  ! Smoothness indicator
  ZBETA1NEG = 0.
  ZBETA2NEG = 0.
  ZBETA1POS = 0.
  ZBETA2POS = 0.

  ! Weno weights
  ZOMEGA1NEG = 0.
  ZOMEGA2NEG = 0.
  ZOMEGA1POS = 0.
  ZOMEGA2POS = 0.

  ! Out gradients
  PGRADLSPHIX2D = 0.
  PGRADLSPHIY2D = 0.

  ! get array sizes
  ILU = SIZE(PLSPHI2D,1)
  IMU = SIZE(PLSPHI2D,2)

  !*      2. Compute gradient for x direction
  !*      2.1 Compute phi(i+1/2)-

  ! phi(i+1/2)- (1) = 3/2 * phi_i - 1/2 * phi(i-1)
  ZPHINEG1(2:ILU-1,:) = -.5 * PLSPHI2D(1:ILU-2,:) + 1.5 * PLSPHI2D(2:ILU-1,:)
  ! phi(i+1/2)- (2) = 1/2 * phi_i + 1/2 * phi(i+1)
  ZPHINEG2(2:ILU-1,:) = .5  * PLSPHI2D(2:ILU-1,:) + .5  * PLSPHI2D(3:ILU,:)

  ! Overshot smoothing
  WHERE (ZPHINEG1 > 1.) ZPHINEG1 = 1.
  WHERE (ZPHINEG1 < 0.) ZPHINEG1 = 0.
  ! ZPHINEG2 doesn't need smoothing because overshoot can not exist

  !*      2.2 Compute phi(i-1/2)+

  ! phi(i-1/2)+ (1) = 3/2 * phi_i - 1/2 * phi(i+1)
  ZPHIPOS1(2:ILU-1,:) = -.5 * PLSPHI2D(3:ILU,:)   + 1.5 * PLSPHI2D(2:ILU-1,:)
  ! phi(i-1/2)+ (2) = 1/2 * phi_i + 1/2 * phi(i-1)
  ZPHIPOS2(2:ILU-1,:) = .5  * PLSPHI2D(2:ILU-1,:) + .5  * PLSPHI2D(1:ILU-2,:)

  ! Overshot smoothing
  WHERE (ZPHIPOS1 > 1.) ZPHIPOS1 = 1.
  WHERE (ZPHIPOS1 < 0.) ZPHIPOS1 = 0.
  ! ZPHIPOS2 doesn't need smoothing because overshoot can not exist

  !*      2.3 Compute beta-

  ! beta(1)- = (phi_i - phi_(i-1))^2
  ZBETA1NEG(2:ILU-1,:) = (PLSPHI2D(2:ILU-1,:) - PLSPHI2D(1:ILU-2,:))**2
  ! beta(2)- = (phi_(i+1) - phi_i)^2
  ZBETA2NEG(2:ILU-1,:) = (PLSPHI2D(3:ILU,:)   - PLSPHI2D(2:ILU-1,:))**2

  !*      2.4 Compute beta+

  ! beta(1)- = (phi_i - phi_(i+1))^2
  ZBETA1POS(2:ILU-1,:) = (PLSPHI2D(2:ILU-1,:) - PLSPHI2D(3:ILU,:))**2
  ! beta(2)- = (phi_(i-1) - phi_i)^2
  ZBETA2POS(2:ILU-1,:) = (PLSPHI2D(1:ILU-2,:) - PLSPHI2D(2:ILU-1,:))**2

  !*      2.5 Compute omega-

  ZOMEGA1NEG(2:ILU-1,:) = ZGAMMA1 / (ZEPS + ZBETA1NEG(2:ILU-1,:))**2
  ZOMEGA2NEG(2:ILU-1,:) = ZGAMMA2 / (ZEPS + ZBETA2NEG(2:ILU-1,:))**2

  !*      2.6 Compute omega+

  ZOMEGA1POS(2:ILU-1,:) = ZGAMMA1 / (ZEPS + ZBETA1POS(2:ILU-1,:))**2
  ZOMEGA2POS(2:ILU-1,:) = ZGAMMA2 / (ZEPS + ZBETA2POS(2:ILU-1,:))**2

  !*      2.7 Compute reconstructions for phi+ and phi-

  ! phi(i+1/2)-
  ZPHINEG(2:ILU-1,:) = (ZOMEGA1NEG(2:ILU-1,:) * ZPHINEG1(2:ILU-1,:)  + &
                        ZOMEGA2NEG(2:ILU-1,:) * ZPHINEG2(2:ILU-1,:)) / &
                        (ZOMEGA1NEG(2:ILU-1,:) + ZOMEGA2NEG(2:ILU-1,:))
  !
  ! phi(i-1/2)+
  ZPHIPOS(2:ILU-1,:) = (ZOMEGA1POS(2:ILU-1,:) * ZPHIPOS1(2:ILU-1,:)  + &
                        ZOMEGA2POS(2:ILU-1,:) * ZPHIPOS2(2:ILU-1,:)) / &
                        (ZOMEGA1POS(2:ILU-1,:) + ZOMEGA2POS(2:ILU-1,:))
  !

  ! Overshot smoothing
  WHERE (ZPHINEG > 1.) ZPHINEG = 1.
  WHERE (ZPHIPOS > 1.) ZPHIPOS = 1.
  WHERE (ZPHINEG < 0.) ZPHINEG = 0.
  WHERE (ZPHIPOS < 0.) ZPHIPOS = 0.

  !*      2.8 Compute upwind gradient

  ! - fluxes if mask < 0, ie phi_(i+1) < phi_(i-1)
  ! fire spread from left to right
  ! + fluxes if mask > 0, ie phi_(i+1) > phi_(i-1)
  ! fire spread from right to left

  PGRADLSPHIX2D(3:ILU-2,:) = ((ZPHINEG(3:ILU-2,:) - ZPHINEG(2:ILU-3,:)) * (.5 - SIGN(.5,PGRADMASKX(3:ILU-2,:))) + &
                              (ZPHIPOS(4:ILU-1,:) - ZPHIPOS(3:ILU-2,:)) * (.5 + SIGN(.5,PGRADMASKX(3:ILU-2,:))))/ &
                              XFIREMESHSIZE(1)
  !

  !*      3. Set default values for y direction

  ! Phi reconstruction
  ZPHINEG = 0.
  ZPHIPOS = 0.

  ! Intermediate reconstruction
  ZPHINEG1 = 0.
  ZPHINEG2 = 0.
  ZPHIPOS1 = 0.
  ZPHIPOS2 = 0.

  ! Smoothness indicator
  ZBETA1NEG = 0.
  ZBETA2NEG = 0.
  ZBETA1POS = 0.
  ZBETA2POS = 0.

  ! Weno weights
  ZOMEGA1NEG = 0.
  ZOMEGA2NEG = 0.
  ZOMEGA1POS = 0.
  ZOMEGA2POS = 0.

  !*      4. Compute gradient for y direction

  !*      4.1 Compute phi(i+1/2)-

  ! phi(i+1/2)- (1) = 3/2 * phi_i - 1/2 * phi(i-1)
  ZPHINEG1(:,2:IMU-1) = -.5 * PLSPHI2D(:,1:IMU-2) + 1.5 * PLSPHI2D(:,2:IMU-1)
  ! phi(i+1/2)- (2) = 1/2 * phi_i + 1/2 * phi(i+1)
  ZPHINEG2(:,2:IMU-1) = .5  * PLSPHI2D(:,2:IMU-1) + .5  * PLSPHI2D(:,3:IMU)

  ! Overshot smoothing
  WHERE (ZPHINEG1 > 1.) ZPHINEG1 = 1.
  WHERE (ZPHINEG1 < 0.) ZPHINEG1 = 0.
  ! ZPHINEG2 doesn't need smoothing because overshoot can not exist

  !*      4.2 Compute phi(i-1/2)+

  ! phi(i-1/2)+ (1) = 3/2 * phi_i - 1/2 * phi(i+1)
  ZPHIPOS1(:,2:IMU-1) = -.5 * PLSPHI2D(:,3:IMU)   + 1.5 * PLSPHI2D(:,2:IMU-1)
  ! phi(i-1/2)+ (2) = 1/2 * phi_i + 1/2 * phi(i-1)
  ZPHIPOS2(:,2:IMU-1) = .5  * PLSPHI2D(:,2:IMU-1) + .5  * PLSPHI2D(:,1:IMU-2)

  ! Overshot smoothing
  WHERE (ZPHIPOS1 > 1.) ZPHIPOS1 = 1.
  WHERE (ZPHIPOS1 < 0.) ZPHIPOS1 = 0.
  ! ZPHIPOS2 doesn't need smoothing because overshoot can not exist

  !*      4.3 Compute beta-

  ! beta(1)- = (phi_i - phi_(i-1))^2
  ZBETA1NEG(:,2:IMU-1) = (PLSPHI2D(:,2:IMU-1) - PLSPHI2D(:,1:IMU-2))**2
  ! beta(2)- = (phi_(i+1) - phi_i)^2
  ZBETA2NEG(:,2:IMU-1) = (PLSPHI2D(:,3:IMU)   - PLSPHI2D(:,2:IMU-1))**2

  !*      4.4 Compute beta+

  ! beta(1)- = (phi_i - phi_(i+1))^2
  ZBETA1POS(:,2:IMU-1) = (PLSPHI2D(:,2:IMU-1) - PLSPHI2D(:,3:IMU))**2
  ! beta(2)- = (phi_(i-1) - phi_i)^2
  ZBETA2POS(:,2:IMU-1) = (PLSPHI2D(:,1:IMU-2) - PLSPHI2D(:,2:IMU-1))**2

  !*      4.5 Compute omega-

  ZOMEGA1NEG(:,2:IMU-1) = ZGAMMA1 / (ZEPS + ZBETA1NEG(:,2:IMU-1))**2
  ZOMEGA2NEG(:,2:IMU-1) = ZGAMMA2 / (ZEPS + ZBETA2NEG(:,2:IMU-1))**2

  !*      4.6 Compute omega+

  ZOMEGA1POS(:,2:IMU-1) = ZGAMMA1 / (ZEPS + ZBETA1POS(:,2:IMU-1))**2
  ZOMEGA2POS(:,2:IMU-1) = ZGAMMA2 / (ZEPS + ZBETA2POS(:,2:IMU-1))**2

  !*      4.7 Compute reconstructions for phi+ and phi-

  ! phi(i+1/2)-
  ZPHINEG(:,2:IMU-1) = (ZOMEGA1NEG(:,2:IMU-1) * ZPHINEG1(:,2:IMU-1)  + &
                        ZOMEGA2NEG(:,2:IMU-1) * ZPHINEG2(:,2:IMU-1)) / &
                        (ZOMEGA1NEG(:,2:IMU-1) + ZOMEGA2NEG(:,2:IMU-1))
  !
  ! phi(i-1/2)+
  ZPHIPOS(:,2:IMU-1) = (ZOMEGA1POS(:,2:IMU-1) * ZPHIPOS1(:,2:IMU-1)  + &
                        ZOMEGA2POS(:,2:IMU-1) * ZPHIPOS2(:,2:IMU-1)) / &
                        (ZOMEGA1POS(:,2:IMU-1) + ZOMEGA2POS(:,2:IMU-1))
  !

  ! Overshot smoothing
  WHERE (ZPHINEG > 1.) ZPHINEG = 1.
  WHERE (ZPHIPOS > 1.) ZPHIPOS = 1.
  WHERE (ZPHINEG < 0.) ZPHINEG = 0.
  WHERE (ZPHIPOS < 0.) ZPHIPOS = 0.

  !*      4.8 Compute upwind gradient

  ! - fluxes if mask < 0, ie phi_(i+1) < phi_(i-1)
  ! fire spread from left to right
  ! + fluxes if mask > 0, ie phi_(i+1) > phi_(i-1)
  ! fire spread from right to left

  PGRADLSPHIY2D(:,3:IMU-2) = ((ZPHINEG(:,3:IMU-2) - ZPHINEG(:,2:IMU-3)) * (.5 - SIGN(.5,PGRADMASKY(:,3:IMU-2))) + &
                              (ZPHIPOS(:,4:IMU-1) - ZPHIPOS(:,3:IMU-2)) * (.5 + SIGN(.5,PGRADMASKY(:,3:IMU-2))))/ &
                              XFIREMESHSIZE(2)

END SUBROUTINE FIRE_WENO_3


SUBROUTINE FIRE_LSDIFFU( PLSPHI, PLSDIFFUX, PLSDIFFUY )
  !!****  *FIRE_LSDIFFU* - Fire model computation of Level set diffusion
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute diffusion on x and y direcctions for level set function
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       Centered 2nd order scheme
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  USE MODD_FIELD_n, ONLY : XLSPHI2D, XLSDIFFUX2D, XLSDIFFUY2D
  !
  USE MODD_FIRE
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  USE MODD_TIME_n, ONLY : TDTCUR
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)    ::  PLSPHI     ! Level Set function

  !! Gradient of LS function
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PLSDIFFUX  ! Laplacian of Phi on x direction
  REAL,  DIMENSION(:,:,:),      INTENT(OUT)  :: PLSDIFFUY  ! Laplacian of Phi on y direction
  !

  !*       0.2   declarations of local variables

  INTEGER :: IIU, IJU             ! atm mesh bounds
  INTEGER :: ILU, IMU             ! fire mesh bounds
  INTEGER :: IKU                  ! fire 3rd dimension bounds
  INTEGER :: IA,IB,II,IJ,IK,IL,IM ! Index for conversions
  ! loop
  INTEGER :: JI,JJ,JK             ! index for atm mesh loop i,j,k
  INTEGER :: JL,JM                ! index for fire mesh loop l,m

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Allocate 2D array with fire grid bounds

  ! get atm mesh bounds
  IIU = SIZE(PLSPHI,1)
  IJU = SIZE(PLSPHI,2)
  IKU = SIZE(PLSPHI,3) ! NREFINX * NREFINY

  ILU = SIZE(XLSPHI2D,1)
  IMU = SIZE(XLSPHI2D,2)

  ! Default values
  PLSDIFFUX(:,:,:) = 0.
  PLSDIFFUY(:,:,:) = 0.

  !*      2. Convert LS function Phi from 3d to 2d format

  ! get l and m to find PHI2D(l,m) = PHI3D(i,j,k)
  DO JK = 1, IKU
    ! b = (k-1) \ NREFINX + 1 where \ means euclidian division
    ! as k,1 and NREFINX are integers, (k-1)/NREFINX is an integer division
    IB = (JK - 1) / NREFINX + 1
    ! a = k - (b-1)*NREFINX
    IA = JK - (IB - 1) * NREFINX
    !
    DO JJ = 1, IJU
      ! m = (j-1)*NREFINY + b
      IM = (JJ - 1) * NREFINY + IB
      !
      DO JI = 1, IIU
        ! l = (i-1)*NREFINX + a
        IL = (JI - 1) * NREFINX + IA
        !  PHI2D(l,m) = PHI3D(i,j,k)
        XLSPHI2D(IL,IM) = PLSPHI(JI,JJ,JK)
      END DO
    END DO
  END DO

  !*      3. Compute laplacian on 2D grid

  XLSDIFFUX2D(2:ILU-1,:) = (XLSPHI2D(3:ILU,:) - 2.*XLSPHI2D(2:ILU-1,:) + XLSPHI2D(1:ILU-2,:)) / XFIREMESHSIZE(1)
  XLSDIFFUY2D(:,2:IMU-1) = (XLSPHI2D(:,3:IMU) - 2.*XLSPHI2D(:,2:IMU-1) + XLSPHI2D(:,1:IMU-2)) / XFIREMESHSIZE(2)

  !*      4. Convert laplacian from 2d to 3d format

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  DO JM = 1, IMU
    ! j = ceil(m/NREFINY)
    IJ = CEILING(REAL(JM) / REAL(NREFINY))
    ! b = m - (j-1) * NREFINY
    IB = JM - (IJ - 1) * NREFINY
    !
    DO JL = 1, ILU
      ! i = ceil(l/NREFINX)
      II = CEILING(REAL(JL) / REAL(NREFINX))
      ! a = l - (i-1) * NREFINX
      IA = JL - (II - 1) * NREFINX
      ! k = (b-1) * NREFINX + a
      IK = (IB - 1) * NREFINX + IA
      !  GRAD3D(i,j,k) = GRAD2D(l,m)
      PLSDIFFUX(II,IJ,IK) = XLSDIFFUX2D(JL,JM)
      PLSDIFFUY(II,IJ,IK) = XLSDIFFUY2D(JL,JM)
    END DO
  END DO

END SUBROUTINE FIRE_LSDIFFU


SUBROUTINE FIRE_ROSDIFFU( PFIRERW )
  !!****  *FIRE_ROSDIFFU* - Fire model computation of ROS diffusion
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute diffusion on x and y direcctions for rate of spread
  !!
  !!**  METHOD
  !!    ------
  !!
  !!       Centered 2nd order scheme
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      24/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_PARAMETERS
  USE MODD_CST
  USE MODD_FIELD_n, ONLY : XFIRERW2D
  !
  USE MODD_FIRE
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  USE MODD_TIME_n, ONLY : TDTCUR
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  !! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(INOUT)  ::  PFIRERW     ! ROS
  !

  !*       0.2   declarations of local variables

  INTEGER :: IIU, IJU             ! atm mesh bounds
  INTEGER :: ILU, IMU             ! fire mesh bounds
  INTEGER :: IKU                  ! fire 3rd dimension bounds
  INTEGER :: IA,IB,II,IJ,IK,IL,IM ! Index for conversions
  ! loop
  INTEGER :: JI,JJ,JK             ! index for atm mesh loop i,j,k
  INTEGER :: JL,JM                ! index for fire mesh loop l,m

  !----------------------------------------------------------------------------------------------
  !

  !*      1. Allocate 2D array with fire grid bounds

  ! get atm mesh bounds
  IIU = SIZE(PFIRERW,1)
  IJU = SIZE(PFIRERW,2)
  IKU = SIZE(PFIRERW,3) ! NREFINX * NREFINY

  ILU = SIZE(XFIRERW2D,1)
  IMU = SIZE(XFIRERW2D,2)

  !*      2. Convert LS function Phi from 3d to 2d format

  ! get l and m to find PHI2D(l,m) = PHI3D(i,j,k)
  DO JK = 1, IKU
    ! b = (k-1) \ NREFINX + 1 where \ means euclidian division
    ! as k,1 and NREFINX are integers, (k-1)/NREFINX is an integer division
    IB = (JK - 1) / NREFINX + 1
    ! a = k - (b-1)*NREFINX
    IA = JK - (IB - 1) * NREFINX
    !
    DO JJ = 1, IJU
      ! m = (j-1)*NREFINY + b
      IM = (JJ - 1) * NREFINY + IB
      !
      DO JI = 1, IIU
        ! l = (i-1)*NREFINX + a
        IL = (JI - 1) * NREFINX + IA
        !  PHI2D(l,m) = PHI3D(i,j,k)
        XFIRERW2D(IL,IM) = PFIRERW(JI,JJ,JK)
      END DO
    END DO
  END DO

  !*      3. Compute laplacian on 2D grid

  ! sub cycle 1
  XFIRERW2D(2:ILU-1,2:IMU-1) = XFIRERW2D(2:ILU-1,2:IMU-1) + XROSDIFFUSION * (&
                              (XFIRERW2D(3:ILU,2:IMU-1) - 2.*XFIRERW2D(2:ILU-1,2:IMU-1) + XFIRERW2D(1:ILU-2,2:IMU-1)) &
                              / XFIREMESHSIZE(1) + &
                              (XFIRERW2D(2:ILU-1,3:IMU) - 2.*XFIRERW2D(2:ILU-1,2:IMU-1) + XFIRERW2D(2:ILU-1,1:IMU-2)) &
                              / XFIREMESHSIZE(2))
  ! sub cycle 2
  XFIRERW2D(3:ILU-2,3:IMU-2) = XFIRERW2D(3:ILU-2,3:IMU-2) + XROSDIFFUSION * (&
                              (XFIRERW2D(4:ILU-1,3:IMU-2) - 2.*XFIRERW2D(3:ILU-2,3:IMU-2) + XFIRERW2D(2:ILU-3,3:IMU-2)) &
                              / XFIREMESHSIZE(1) + &
                              (XFIRERW2D(3:ILU-2,4:IMU-1) - 2.*XFIRERW2D(3:ILU-2,3:IMU-2) + XFIRERW2D(3:ILU-2,2:IMU-3)) &
                              / XFIREMESHSIZE(2))
  !

  !*      4. Convert laplacian from 2d to 3d format

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  DO JM = 1, IMU
    ! j = ceil(m/NREFINY)
    IJ = CEILING(REAL(JM) / REAL(NREFINY))
    ! b = m - (j-1) * NREFINY
    IB = JM - (IJ - 1) * NREFINY
    !
    DO JL = 1, ILU
      ! i = ceil(l/NREFINX)
      II = CEILING(REAL(JL) / REAL(NREFINX))
      ! a = l - (i-1) * NREFINX
      IA = JL - (II - 1) * NREFINX
      ! k = (b-1) * NREFINX + a
      IK = (IB - 1) * NREFINX + IA
      !  GRAD3D(i,j,k) = GRAD2D(l,m)
      PFIRERW(II,IJ,IK) = XFIRERW2D(JL,JM)
    END DO
  END DO

END SUBROUTINE FIRE_ROSDIFFU


SUBROUTINE FIRE_SUBGRIDSURFACE( PLSPHI2D, PSURFRATIO2D )
  !!****  *FIRE_SUBGRIDSURFACE* - Fire model computation of subgrid burning area
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute subgrid burning area
  !!
  !!**  METHOD
  !!    ------
  !! 
  !!    EFFR: Explicit Fire Front reconstruction
  !!    WA: Weighted Averaged
  !!    See Costes et al. [2021] for more details
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Costes et al [2021]
  !!    
  !!    Technical reports
  !!      [a] 19S52, A. Costes [2019]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  USE MODI_FIRE_MODEL, ONLY: FIRE_QUANDRANTSURFACE
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,  DIMENSION(:,:),    INTENT(IN)    ::  PLSPHI2D             ! Level Set function in 2D array
  REAL,  DIMENSION(:,:),    INTENT(OUT)    ::  PSURFRATIO2D         ! Surface ratio in 2D array

  !*       0.2   declarations of local variables
  !
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHISW     ! Phi value at South West point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHIS     ! Phi value at South point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHISE     ! Phi value at South East point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHIE     ! Phi value at East point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHINE     ! Phi value at North East point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHIN     ! Phi value at North point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHINW     ! Phi value at North West point
  REAL,  DIMENSION(SIZE(PLSPHI2D,1),SIZE(PLSPHI2D,2))      ::  ZPHIW     ! Phi value at West point

  INTEGER    ::  ZCI       ! Crossing identifier

  INTEGER :: ILU, IMU
  !----------------------------------------------------------------------------------------------
  !
  !*      1.  Default values and sizes

  PSURFRATIO2D(:,:) = 0.

  ILU = SIZE(PLSPHI2D,1)
  IMU = SIZE(PLSPHI2D,2)

  ZPHISW(:,:) = 0.
  ZPHIS(:,:)  = 0.
  ZPHISE(:,:) = 0.
  ZPHIE(:,:)  = 0.
  ZPHINE(:,:) = 0.
  ZPHIN(:,:)  = 0.
  ZPHINW(:,:) = 0.
  ZPHIW(:,:)  = 0.

  !*      2.  Interpolation of corners for south west quadrant

  ! South west interpolation
  ZPHISW(2:ILU-1,2:IMU-1) = 0.25 * (PLSPHI2D(1:ILU-2,1:IMU-2) + PLSPHI2D(2:ILU-1,1:IMU-2) + &
                                    PLSPHI2D(1:ILU-2,2:IMU-1) + PLSPHI2D(2:ILU-1,2:IMU-1))
  ! South interpolation
  ZPHIS(2:ILU-1,2:IMU-1)  = 0.5  * (PLSPHI2D(2:ILU-1,1:IMU-2) + PLSPHI2D(2:ILU-1,2:IMU-1))

  ! West interpolation
  ZPHIW(2:ILU-1,2:IMU-1)  = 0.5  * (PLSPHI2D(1:ILU-2,2:IMU-1) + PLSPHI2D(2:ILU-1,2:IMU-1))

  CALL FIRE_QUANDRANTSURFACE( ZPHISW, ZPHIS, PLSPHI2D, ZPHIW, PSURFRATIO2D )

  !*      3.  Interpolation of corners for south east quadrant

  ! South east interpolation
  ZPHISE(2:ILU-1,2:IMU-1) = 0.25 * (PLSPHI2D(2:ILU-1,1:IMU-2) + PLSPHI2D(3:ILU  ,1:IMU-2) + &
                                    PLSPHI2D(2:ILU-1,2:IMU-1) + PLSPHI2D(3:ILU  ,2:IMU-1))

  ! East interpolation
  ZPHIE(2:ILU-1,2:IMU-1)  = 0.5  * (PLSPHI2D(2:ILU-1,2:IMU-1) + PLSPHI2D(3:ILU  ,2:IMU-1))

  CALL FIRE_QUANDRANTSURFACE( ZPHIS, ZPHISE, ZPHIE, PLSPHI2D, PSURFRATIO2D )

  !*      4.  Interpolation of corners for North east quadrant

  ! North east interpolation
  ZPHINE(2:ILU-1,2:IMU-1) = 0.25 * (PLSPHI2D(2:ILU-1,2:IMU-1) + PLSPHI2D(3:ILU  ,2:IMU-1) + &
                                    PLSPHI2D(2:ILU-1,3:IMU  ) + PLSPHI2D(3:ILU  ,3:IMU  ))

  ! North interpolation
  ZPHIN(2:ILU-1,2:IMU-1)  = 0.5  * (PLSPHI2D(2:ILU-1,2:IMU-1) + PLSPHI2D(2:ILU-1,3:IMU  ))

  CALL FIRE_QUANDRANTSURFACE( PLSPHI2D, ZPHIE, ZPHINE, ZPHIN, PSURFRATIO2D )

  !*      5.  Interpolation of corners for North west quadrant

  ! North west corner
  ZPHINW(2:ILU-1,2:IMU-1) = 0.25 * (PLSPHI2D(1:ILU-2,2:IMU-1) + PLSPHI2D(2:ILU-1,2:IMU-1) + &
                                    PLSPHI2D(1:ILU-2,3:IMU  ) + PLSPHI2D(2:ILU-1,3:IMU  ))

  CALL FIRE_QUANDRANTSURFACE( ZPHIW, PLSPHI2D, ZPHIN, ZPHINW, PSURFRATIO2D )

END SUBROUTINE FIRE_SUBGRIDSURFACE


SUBROUTINE FIRE_QUANDRANTSURFACE( PPHI1, PPHI2, PPHI3, PPHI4, PSURFRATIO2D )
  !!****  *FIRE_QUANDRANTSURFACE* - Fire model computation of subgrid burning area for quadrant
  !!
  !!    PURPOSE
  !!    -------
  !!    Computation of subgrid burning area for quadrant with EFFR method
  !!    This method is also used in pyrolib and is tested for accuracy.
  !!    If you change this subroutine, please change the same part in pyrolib.
  !!
  !!**  METHOD
  !!    ------
  !!    See Costes et al [2021]
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    Costes et al [2021]
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  USE MODI_FIRE_MODEL, ONLY: FIRE_SURF_68,FIRE_SURF_70,FIRE_SURF_22,FIRE_SURF_28
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI1             ! Phi at south west point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI2             ! Phi at south east point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI3             ! Phi at north east point
  REAL,DIMENSION(:,:),    INTENT(IN)     ::  PPHI4             ! Phi at north west point
  REAL,DIMENSION(:,:),    INTENT(INOUT) ::  PSURFRATIO2D      ! Subgrid burning surface for cell

  !*       0.2   declarations of local variables
  ! Intersections
  INTEGER    ::  ZD1       ! Intersection quantity for south border
  INTEGER    ::  ZD2       ! Intersection quantity for east border
  INTEGER    ::  ZD3       ! Intersection quantity for north border
  INTEGER    ::  ZD4       ! Intersection quantity for west border

  REAL     ::  ZPHI1       ! Phi at south west point
  REAL     ::  ZPHI2       ! Phi at south east point
  REAL     ::  ZPHI3       ! Phi at north east point
  REAL     ::  ZPHI4       ! Phi at north west point
  REAL    ::  ZQUADSURF   ! Subgrid burning surface for quadrant

  !
  INTEGER    ::  ZCI       ! Crossing identifier

  INTEGER :: ILU, IMU
  INTEGER :: JL, JM
  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Default values and sizes

  ILU = SIZE(PPHI1,1)
  IMU = SIZE(PPHI1,2)

  !*      2.  Check full or empty cells

  ! loop on 2d ire grid
  DO JM = 2, IMU-1
    DO JL = 1,ILU-1

      ! get corners values
      ZPHI1 = PPHI1(JL,JM)
      ZPHI2 = PPHI2(JL,JM)
      ZPHI3 = PPHI3(JL,JM)
      ZPHI4 = PPHI4(JL,JM)

      ! Full quadrant in fire when phi1,2,3,4 >= .5
      IF(ZPHI1 >= .5 .AND. ZPHI2 >= .5 .AND. ZPHI3 >= .5 .AND. ZPHI4 >= .5 ) THEN
        ZQUADSURF = 1.
        PSURFRATIO2D(JL,JM) = PSURFRATIO2D(JL,JM) + .25 * ZQUADSURF
        CYCLE
      END IF

      ! No fire in quadrant when phi1,2,3,4 < 5
      IF(ZPHI1 <  .5 .AND. ZPHI2 <  .5 .AND. ZPHI3 <  .5 .AND. ZPHI4 <  .5 ) THEN
        ZQUADSURF = 0.
        PSURFRATIO2D(JL,JM) = PSURFRATIO2D(JL,JM) + .25 * ZQUADSURF
        CYCLE
      END IF

      !*      3.  Compute crossing values

      ZD1 = NINT( SIGN(.5,ZPHI1 - .5) - SIGN(.5,ZPHI2 - .5))
      ZD2 = NINT( SIGN(.5,ZPHI2 - .5) - SIGN(.5,ZPHI3 - .5))
      ZD3 = NINT( SIGN(.5,ZPHI4 - .5) - SIGN(.5,ZPHI3 - .5))
      ZD4 = NINT( SIGN(.5,ZPHI1 - .5) - SIGN(.5,ZPHI4 - .5))

      !*      4.  Compute trinary identifier

      ! CI = 3^0 * (1+d1) + 3^1 * (1+d2) + 3^2 * (1+d3) + 3^3 * (1+d4)
      ZCI =       1 + ZD1  +  3 * (1. + ZD2) +&
            9 * (1. + ZD3) + 27 * (1. + ZD4)

      !*      5.  Compute surface for each intersection case

      SELECT CASE(ZCI)
      CASE(68)
        ! South west triangle burning
        ZQUADSURF = FIRE_SURF_68(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(12)
        ! Complementary case #68
        ZQUADSURF = 1. - FIRE_SURF_68(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(70)
        ! Southern trapeze burning
        ZQUADSURF = FIRE_SURF_70(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(10)
        ! Complementary case #70
        ZQUADSURF = 1. - FIRE_SURF_70(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(22)
        ! North west triangle burning
        ZQUADSURF = FIRE_SURF_22(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(58)
        ! Complementary case #22
        ZQUADSURF = 1. - FIRE_SURF_22(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(42)
        ! North west triangle burning
        ! 22 case eqn with Phi2 / Phi4 permutation
        ZQUADSURF = FIRE_SURF_22(ZPHI1,ZPHI4,ZPHI3,ZPHI2)

      CASE(38)
        ! Complementary case #42
        ZQUADSURF = 1. - FIRE_SURF_22(ZPHI1,ZPHI4,ZPHI3,ZPHI2)

      CASE(50)
        ! Western trapeze burning
        ! Eqn 70 with 2 - 4 permutation
        ZQUADSURF = FIRE_SURF_70(ZPHI1,ZPHI4,ZPHI3,ZPHI2)

      CASE(30)
        ! Complementary case #50
        ZQUADSURF = 1. - FIRE_SURF_70(ZPHI1,ZPHI4,ZPHI3,ZPHI2)

      CASE(28)
        ! North east triangle burning
        ZQUADSURF = FIRE_SURF_28(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(52)
        ! Complementary case #46
        ZQUADSURF = 1. - FIRE_SURF_28(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE(24)
        ! North west + South east triangles burning
        ! #22 + #42
        ZQUADSURF = FIRE_SURF_22(ZPHI1,ZPHI2,ZPHI3,ZPHI4) + FIRE_SURF_22(ZPHI1,ZPHI4,ZPHI3,ZPHI2)

      CASE(56)
        ! South west + North east triangles burning
        ! #68 + #46
        ZQUADSURF = FIRE_SURF_68(ZPHI1,ZPHI2,ZPHI3,ZPHI4) + FIRE_SURF_28(ZPHI1,ZPHI2,ZPHI3,ZPHI4)

      CASE DEFAULT
        WRITE(*,*) 'CASE DEFAULT for ', JL, JM
        ZQUADSURF = 0.
      END SELECT

    !*      6.  Add quadrant subgrid area to cell subgrid area

      PSURFRATIO2D(JL,JM) = PSURFRATIO2D(JL,JM) + .25 * ZQUADSURF

    END DO
  END DO

END SUBROUTINE FIRE_QUANDRANTSURFACE


FUNCTION FIRE_SURF_68( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )
  !!****  *FIRE_SURF_68* Compute surface ratio for cases :
  !!
  !!      - 68 : SW triangle with Phi1, Phi2 and Phi4
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/12/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,    INTENT(IN)     ::  PPHI1             ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4             ! Phi4

  REAL                  ::  PSURF             ! Surface ratio

  !*      1.  Compute surface value

  PSURF = (.5 - PPHI1)**2 / (2. * (PPHI2 - PPHI1) * (PPHI4 - PPHI1))

END FUNCTION FIRE_SURF_68


FUNCTION FIRE_SURF_70( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )
  !!****  *FIRE_SURF_70* Compute surface ratio for cases :
  !!
  !!      - 70 : S trapeze
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/12/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,    INTENT(IN)     ::  PPHI1             ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4             ! Phi4

  REAL                  ::  PSURF             ! Surface ratio

  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute surface value

  PSURF = .5 * ((.5 -PPHI1) / (PPHI4 - PPHI1) + (.5 - PPHI2) / (PPHI3 - PPHI2))

END FUNCTION FIRE_SURF_70


FUNCTION FIRE_SURF_22( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )
  !!****  *FIRE_SURF_22* Compute surface ratio for cases :
  !!
  !!      - 22 : NW triangle
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/12/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  USE MODE_MPPDB

  IMPLICIT NONE

  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,    INTENT(IN)     ::  PPHI1             ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4             ! Phi4

  REAL                  ::  PSURF             ! Surface ratio

  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute surface value

  PSURF = (PPHI4 - .5)**2 / (-2. * (PPHI4 - PPHI1) * (PPHI3 - PPHI4))

END FUNCTION FIRE_SURF_22


FUNCTION FIRE_SURF_28( PPHI1, PPHI2, PPHI3, PPHI4 ) RESULT( PSURF )
  !!****  *FIRE_SURF_28* Compute surface ratio for cases :
  !!
  !!      - 28 : NE triangle
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/12/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  REAL,    INTENT(IN)     ::  PPHI1             ! Phi1
  REAL,    INTENT(IN)     ::  PPHI2             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI3             ! Phi2
  REAL,    INTENT(IN)     ::  PPHI4             ! Phi4

  REAL                  ::  PSURF             ! Surface ratio

  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute surface value

  PSURF = (PPHI3 - .5)**2 / (2. * (PPHI3 - PPHI2) * (PPHI3 - PPHI4))

END FUNCTION FIRE_SURF_28


SUBROUTINE FIRE_LS_RECONSTRUCTION_FROM_BMAP( PLSPHI, PBMAP, PATMDT )
  !!****  *FIRE_LS_RECONSTRUCTION_FROM_BMAP* - Fire model level set reconstruction from burning map
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute level set function from bmap
  !!
  !!**  METHOD
  !!    ------
  !!
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!    A. Costes PhD [2021], Chapter 2, Section 3.3.a
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      29/10/19
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !
  USE MODD_FIRE
  USE MODD_TIME_n, ONLY : TDTCUR
  !USE MODI_FIRE_MODEL
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments

  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PLSPHI     ! Level Set function
  REAL,  DIMENSION(:,:,:),    INTENT(IN)  ::  PBMAP     ! Burning map
  REAL,                     INTENT(IN)  ::  PATMDT    ! Atm time step

  !*       0.2   declarations of local variables

  REAL :: ZLAMBDA                                     ! Sigmoide parameter
  REAL :: ZFITPARAM(4)                                ! Fitted parameters for lambda(dxf)
  REAL :: ZFMMESHSIZE                                 ! Mean of fire mesh size


  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Lambda model parameters

  ! lambda = a * exp(-b * (dxf - c)) + d
  ZFITPARAM(1) = 2.13574296   ! a
  ZFITPARAM(2) = 0.21070849   ! b
  ZFITPARAM(3) = -8.61319574  ! c
  ZFITPARAM(4) = 0.06448332   ! d

  ZFMMESHSIZE = .5 * (XFIREMESHSIZE(1) + XFIREMESHSIZE(2))

  !*      2.  Compute lambda value fron exponential fitting for fire mesh size

  ZLAMBDA = ZFITPARAM(1) * EXP(-1. * ZFITPARAM(2) * (ZFMMESHSIZE - ZFITPARAM(3))) + ZFITPARAM(4)

  !*      3.  Compute sigmoide for bmap cells at t+dt

  WHERE(PBMAP >= 0)
    PLSPHI = 1. / (1. + EXP(-1. * ZLAMBDA * (TDTCUR%XTIME + PATMDT - PBMAP)))
  END WHERE

END SUBROUTINE FIRE_LS_RECONSTRUCTION_FROM_BMAP


SUBROUTINE FIRE_GRAD_OROGRAPHY( PZS, PFMGRADOROX, PFMGRADOROY )
  !!****  *FIRE_GRAD_OROGRAPHY* - Fire model computation of orography gradient
  !!
  !!    PURPOSE
  !!    -------
  !!    Compute orography gradient
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    Compute orography gradient on atm mesh and then interpolate on fire grid (like the interpolation of horizontal wind)
  !!
  !!    EXTERNAL
  !!    --------
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
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      14/10/21
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !
  USE MODD_FIRE,  ONLY: XFIREMESHSIZE, NREFINX, NREFINY
  !
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*      0.1  Declarations of arguments
  !!          -------------------------

  REAL,  DIMENSION(:,:),      INTENT(IN)  ::  PZS           ! MNH orography (atm resolution) [m]
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PFMGRADOROX   ! Orographic gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(:,:,:),    INTENT(OUT)  ::  PFMGRADOROY   ! Orographic gradient on y direction (dz/dy) [m/m]

  !*       0.2 Declarations of local variables
  !!           -------------------------------
  REAL,  DIMENSION(SIZE(PZS,1),SIZE(PZS,2))      ::  ZATMGRADOROX          ! MNH gradient on x direction (dz/dx) [m/m]
  REAL,  DIMENSION(SIZE(PZS,1),SIZE(PZS,2))      ::  ZATMGRADOROY          ! MNH gradient on y direction (dz/dy) [m/m]
  REAL,  DIMENSION(SIZE(PZS,1)+1,SIZE(PZS,2)+1)  ::  ZATMCORNERX           ! MNH gradient on x direction interpolated on atm. corners (dz/dx) [m/m]
  REAL,  DIMENSION(SIZE(PZS,1)+1,SIZE(PZS,2)+1)  ::  ZATMCORNERY           ! MNH gradient on y direction interpolated on atm. corners (dz/dy) [m/m]

  ! size of meshes
  INTEGER ::  IIU, IJU  ! atm mesh bounds
  INTEGER ::  IKU       ! fire 3rd dimension bounds
  REAL    ::  ZDX, ZDY  ! atm delta x and delta y
  INTEGER ::  IM, IL    ! fire mesh absolute index

  ! loops
  INTEGER ::  JI,JJ     ! atm grid index
  INTEGER ::  JK,JL,JM  ! fire grid index

  !----------------------------------------------------------------------------------------------
  !

  !!
  !*      1. Get bounds and domain settings
  !!         ==============================

  ! domain max indexes
  IIU = SIZE(PFMGRADOROX,1)
  IJU = SIZE(PFMGRADOROX,2)
  IKU = SIZE(PFMGRADOROX,3) ! NREFINX * NREFINY

  ! retrieve atm mesh size (dx = dxf * NREFINX)
  ZDX = XFIREMESHSIZE(1) * REAL(NREFINX)
  ZDY = XFIREMESHSIZE(2) * REAL(NREFINY)

  !!
  !*      2. Compute orographic gradient on atm mesh
  !!         =======================================

  !*      2.1 Main loop
  !!          ---------
  ! orographic gradient noted h_{i,j}^{x,a} in comments, z_{i,j} is ZS
  ! 2nd order 2d FD scheme for main loop
  ! h_{i,j}^{x,a} = (z_{i+1,j-1} - z_{i-1,j-1} + 2*(z_{i+1,j}-z_{i-1,j}) + z_{i+1,j+1}-z_{i-1,j+1}) / (8 * dx)
  ! h_{i,j}^{y,a} = (z_{i-1,j+1} - z_{i-1,j-1} + 2*(z_{i,j+1}-z_{i,j-1}) + z_{i+1,j+1}-z_{i+1,j-1}) / (8 * dx)
  DO JJ = 2, IJU-1
    DO JI = 2, IIU-1
      ZATMGRADOROX(JI,JJ) = (PZS(JI+1,JJ-1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI+1,JJ)-PZS(JI-1,JJ)) &
                          + PZS(JI+1,JJ+1)-PZS(JI-1,JJ+1)) / (8. * ZDX)
      ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ+1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ+1)-PZS(JI,JJ-1)) &
                          + PZS(JI+1,JJ+1)-PZS(JI+1,JJ-1)) / (8. * ZDY)
    END DO
  END DO

  !*      2.1 West boundary
  !!          -------------
  JI = 1
  DO JJ = 2, IJU-1
    ! uncentered scheme
    ZATMGRADOROX(JI,JJ) = (PZS(JI+1,JJ-1)-PZS(JI,JJ-1) + 2.*(PZS(JI+1,JJ)-PZS(JI,JJ)) + PZS(JI+1,JJ+1)-PZS(JI,JJ+1)) / (4. * ZDX)
    ! do not use JI-1 points
    ZATMGRADOROY(JI,JJ) = (2.*(PZS(JI,JJ+1)-PZS(JI,JJ-1)) + PZS(JI+1,JJ+1)-PZS(JI+1,JJ-1)) / (6. * ZDY)
  END DO

  !*      2.2 East boundary
  !!          -------------
  JI = IIU
  DO JJ = 2, IJU-1
    ! uncentered scheme
    ZATMGRADOROX(JI,JJ) = (PZS(JI,JJ-1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ)-PZS(JI-1,JJ)) + PZS(JI,JJ+1)-PZS(JI-1,JJ+1)) / (4. * ZDX)
    ! do not use JI+1 points
    ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ+1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ+1)-PZS(JI,JJ-1))) / (6. * ZDY)
  END DO

  !*      2.3 South boundary
  !!          --------------
  JJ = 1
  DO JI = 2, IIU-1
    ! do not use JJ-1 points
    ZATMGRADOROX(JI,JJ) = (2.*(PZS(JI+1,JJ)-PZS(JI-1,JJ)) + PZS(JI+1,JJ+1)-PZS(JI-1,JJ+1)) / (6. * ZDX)
    ! uncentered scheme
    ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ+1)-PZS(JI-1,JJ) + 2.*(PZS(JI,JJ+1)-PZS(JI,JJ)) + PZS(JI+1,JJ+1)-PZS(JI+1,JJ)) / (4. * ZDY)
  END DO

  !*      2.4 North boundary
  !!          --------------
  JJ = IJU
  DO JI = 2, IIU-1
    ! do not use JJ+1 points
    ZATMGRADOROX(JI,JJ) = (PZS(JI+1,JJ-1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI+1,JJ)-PZS(JI-1,JJ))) / (6. * ZDX)
    ! uncentered scheme
    ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ)-PZS(JI,JJ-1)) + PZS(JI+1,JJ)-PZS(JI+1,JJ-1)) / (4. * ZDY)
  END DO

  !*      2.5 Corners
  !!          -------
  ! SW
  JI = 1
  JJ = 1
  ZATMGRADOROX(JI,JJ) = (2.*(PZS(JI+1,JJ)-PZS(JI,JJ)) + PZS(JI+1,JJ+1)-PZS(JI,JJ+1)) / (3. * ZDX)
  ZATMGRADOROY(JI,JJ) = (2.*(PZS(JI,JJ+1)-PZS(JI,JJ)) + PZS(JI+1,JJ+1)-PZS(JI+1,JJ)) / (3. * ZDY)
  ! SE
  JI = IIU
  JJ = 1
  ZATMGRADOROX(JI,JJ) = (2.*(PZS(JI,JJ)-PZS(JI-1,JJ)) + PZS(JI,JJ+1)-PZS(JI-1,JJ+1)) / (3. * ZDX)
  ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ+1)-PZS(JI-1,JJ) + 2.*(PZS(JI,JJ+1)-PZS(JI,JJ))) / (3. * ZDY)
  ! NW
  JI = 1
  JJ = IJU
  ZATMGRADOROX(JI,JJ) = (PZS(JI+1,JJ-1)-PZS(JI,JJ-1) + 2.*(PZS(JI+1,JJ)-PZS(JI,JJ))) / (3. * ZDX)
  ZATMGRADOROY(JI,JJ) = (2.*(PZS(JI,JJ)-PZS(JI,JJ-1)) + PZS(JI+1,JJ)-PZS(JI+1,JJ-1)) / (3. * ZDY)
  ! NE
  JI = IIU
  JJ = IJU
  ZATMGRADOROX(JI,JJ) = (PZS(JI,JJ-1)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ)-PZS(JI-1,JJ))) / (3. * ZDX)
  ZATMGRADOROY(JI,JJ) = (PZS(JI-1,JJ)-PZS(JI-1,JJ-1) + 2.*(PZS(JI,JJ)-PZS(JI,JJ-1))) / (3. * ZDY)

  !!
  !*      3. Interpolate on atmospheric grid corners
  !!         =======================================

  !*      3.1 Main loop
  !!          ---------

  DO JJ = 2, IJU
    DO JI = 2, IIU
      ZATMCORNERX(JI,JJ) = .25 * (ZATMGRADOROX(JI-1,JJ) + ZATMGRADOROX(JI,JJ) + ZATMGRADOROX(JI-1,JJ-1) + ZATMGRADOROX(JI,JJ-1))
      ZATMCORNERY(JI,JJ) = .25 * (ZATMGRADOROY(JI-1,JJ) + ZATMGRADOROY(JI,JJ) + ZATMGRADOROY(JI-1,JJ-1) + ZATMGRADOROY(JI,JJ-1))
    END DO
  END DO

  !*      3.1 West boundary
  !!          -------------
  JI = 1
  DO JJ = 2, IJU
    ZATMCORNERX(JI,JJ) = .25 * (3. * ZATMGRADOROX(JI,JJ) - ZATMGRADOROX(JI+1,JJ) + &
                                3. * ZATMGRADOROX(JI,JJ-1) - ZATMGRADOROX(JI+1,JJ-1))
    ZATMCORNERY(JI,JJ) = .25 * (3. * ZATMGRADOROY(JI,JJ) - ZATMGRADOROY(JI+1,JJ) + &
                                3. * ZATMGRADOROY(JI,JJ-1) - ZATMGRADOROY(JI+1,JJ-1))
  END DO

  !*      3.2 East boundary
  !!          -------------
  JI = IIU+1
  DO JJ = 2, IJU
    ZATMCORNERX(JI,JJ) = .25 * (3. * ZATMGRADOROX(JI-1,JJ) - ZATMGRADOROX(JI-2,JJ) + &
                                3. * ZATMGRADOROX(JI-1,JJ-1) - ZATMGRADOROX(JI-2,JJ-1))
    ZATMCORNERY(JI,JJ) = .25 * (3. * ZATMGRADOROY(JI-1,JJ) - ZATMGRADOROY(JI-2,JJ) + &
                                3. * ZATMGRADOROY(JI-1,JJ-1) - ZATMGRADOROY(JI-2,JJ-1))
  END DO

  !*      3.3 South boundary
  !!          --------------
  JJ = 1
  DO JI = 2, IIU
    ZATMCORNERX(JI,JJ) = .25 * (3. * ZATMGRADOROX(JI,JJ) - ZATMGRADOROX(JI,JJ+1) + &
                                3. * ZATMGRADOROX(JI-1,JJ) - ZATMGRADOROX(JI-1,JJ+1))
    ZATMCORNERY(JI,JJ) = .25 * (3. * ZATMGRADOROY(JI,JJ) - ZATMGRADOROY(JI,JJ+1) + &
                                3. * ZATMGRADOROY(JI-1,JJ) - ZATMGRADOROY(JI-1,JJ+1))
  END DO

  !*      3.4 North boundary
  !!          --------------
  JJ = IJU+1
  DO JI = 2, IIU
    ZATMCORNERX(JI,JJ) = .25 * (3. * ZATMGRADOROX(JI,JJ-1) - ZATMGRADOROX(JI,JJ-2) + &
                                3. * ZATMGRADOROX(JI-1,JJ-1) - ZATMGRADOROX(JI-1,JJ-2))
    ZATMCORNERY(JI,JJ) = .25 * (3. * ZATMGRADOROY(JI,JJ-1) - ZATMGRADOROY(JI,JJ-2) + &
                                3. * ZATMGRADOROY(JI-1,JJ-1) - ZATMGRADOROY(JI-1,JJ-2))
  END DO

  !*      3.5 Corners
  !!          -------
  ! SW
  JI = 1
  JJ = 1
  ZATMCORNERX(JI,JJ) = .5 * (2. * ZATMCORNERX(JI+1,JJ) - ZATMCORNERX(JI+2,JJ) + 2. * ZATMCORNERX(JI,JJ+1) - ZATMCORNERX(JI,JJ+2))
  ZATMCORNERY(JI,JJ) = .5 * (2. * ZATMCORNERY(JI+1,JJ) - ZATMCORNERY(JI+2,JJ) + 2. * ZATMCORNERY(JI,JJ+1) - ZATMCORNERY(JI,JJ+2))
  ! SE
  JI = IIU+1
  JJ = 1
  ZATMCORNERX(JI,JJ) = .5 * (2. * ZATMCORNERX(JI-1,JJ) - ZATMCORNERX(JI-2,JJ) + 2. * ZATMCORNERX(JI,JJ+1) - ZATMCORNERX(JI,JJ+2))
  ZATMCORNERY(JI,JJ) = .5 * (2. * ZATMCORNERY(JI-1,JJ) - ZATMCORNERY(JI-2,JJ) + 2. * ZATMCORNERY(JI,JJ+1) - ZATMCORNERY(JI,JJ+2))
  ! NW
  JI = 1
  JJ = IJU+1
  ZATMCORNERX(JI,JJ) = .5 * (2. * ZATMCORNERX(JI+1,JJ) - ZATMCORNERX(JI+2,JJ) + 2. * ZATMCORNERX(JI,JJ-1) - ZATMCORNERX(JI,JJ-2))
  ZATMCORNERY(JI,JJ) = .5 * (2. * ZATMCORNERY(JI+1,JJ) - ZATMCORNERY(JI+2,JJ) + 2. * ZATMCORNERY(JI,JJ-1) - ZATMCORNERY(JI,JJ-2))
  ! NE
  JI = IIU+1
  JJ = IJU+1
  ZATMCORNERX(JI,JJ) = .5 * (2. * ZATMCORNERX(JI-1,JJ) - ZATMCORNERX(JI-2,JJ) + 2. * ZATMCORNERX(JI,JJ-1) - ZATMCORNERX(JI,JJ-2))
  ZATMCORNERY(JI,JJ) = .5 * (2. * ZATMCORNERY(JI-1,JJ) - ZATMCORNERY(JI-2,JJ) + 2. * ZATMCORNERY(JI,JJ-1) - ZATMCORNERY(JI,JJ-2))

  !*      3. Interpolate on fire grid
  !!         ========================

  ! same method as for wind interpolation

  DO JK = 1, IKU
    ! compute index position of grid cell
    IM = (JK - 1) / NREFINX + 1
    IL = JK - (IM - 1) * NREFINX
    ! interpolate for each atm cell
    DO JJ = 1, IJU
      DO JI = 1, IIU
        PFMGRADOROX(JI,JJ,JK) = (IM * (IL * ZATMCORNERX(JI+1,JJ+1) + (NREFINX + 1 - IL) * ZATMCORNERX(JI,JJ+1)) + &
                            (NREFINY + 1 - IM) * (IL * ZATMCORNERX(JI+1,JJ) + (NREFINX + 1 - IL) * ZATMCORNERX(JI,JJ))) / &
                            REAL((NREFINX + 1) * (NREFINY + 1))
        PFMGRADOROY(JI,JJ,JK) = (IM * (IL * ZATMCORNERY(JI+1,JJ+1) + (NREFINX + 1 - IL) * ZATMCORNERY(JI,JJ+1)) + &
                            (NREFINY + 1 - IM) * (IL * ZATMCORNERY(JI+1,JJ) + (NREFINX + 1 - IL) * ZATMCORNERY(JI,JJ))) / &
                            REAL((NREFINX + 1) * (NREFINY + 1))
      END DO
    END DO
  END DO

END SUBROUTINE FIRE_GRAD_OROGRAPHY

!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                             DEPRECATED CONTENT
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! -----------------------------------------------------------------------------
!! Following function are defined to get 3d index (i,j,k) from 2d (l,m)
!! but are not used in Blaze for now
!! -----------------------------------------------------------------------------
FUNCTION FGET_I(PLINDEX,PMINDEX) RESULT(POUTINDEX)
  !!****  *FGET_I* Compute i atm index from l,m fire index :
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/07/21
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE, ONLY : NREFINX,NREFINY
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                    ::  POUTINDEX             ! i atm index

  !*       0.2   declarations of local variables

  INTEGER                   :: IA, IB, II, IJ, IK
  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute atm index

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)

  ! i = ceil(l/NREFINX)
  POUTINDEX = CEILING(REAL(PLINDEX) / REAL(NREFINX))

END FUNCTION FGET_I


FUNCTION FGET_J(PLINDEX,PMINDEX) RESULT(POUTINDEX)
  !!****  *FGET_J* Compute j atm index from l,m fire index :
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/07/21
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE, ONLY : NREFINX,NREFINY
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                    ::  POUTINDEX             ! i atm index

  !*       0.2   declarations of local variables

  INTEGER                   :: IA, IB, II, IJ, IK
  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute atm index

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  ! j = ceil(m/NREFINY)
  POUTINDEX = CEILING(REAL(PMINDEX) / REAL(NREFINY))

END FUNCTION FGET_J


FUNCTION FGET_K(PLINDEX,PMINDEX) RESULT(POUTINDEX)
  !!****  *FGET_J* Compute j atm index from l,m fire index :
  !!
  !!
  !!    AUTHOR
  !!    ------
  !!    A. Costes (Météo-France-Cerfacs)
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/07/21
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !!             ============
  !
  USE MODD_FIRE, ONLY : NREFINX,NREFINY
  USE MODE_MPPDB
  !
  IMPLICIT NONE
  !
  !*       0.1  Declarations of arguments
  !!        -------------------------

  INTEGER,    INTENT(IN)     ::  PLINDEX             ! l fire index
  INTEGER,    INTENT(IN)     ::  PMINDEX             ! m fire index

  INTEGER                    ::  POUTINDEX             ! i atm index

  !*       0.2   declarations of local variables

  INTEGER                   :: IA, IB, II, IJ
  !----------------------------------------------------------------------------------------------
  !

  !*      1.  Compute atm index

  ! get i,j and k to find GRAD3D(i,j,k) = GRAD2D(l,m)
  IJ = CEILING(REAL(PMINDEX) / REAL(NREFINY))
  ! b = m - (j-1) * NREFINY
  IB = PMINDEX - (IJ - 1) * NREFINY
  ! i = ceil(l/NREFINX)
  II = CEILING(REAL(PLINDEX) / REAL(NREFINX))
  ! a = l - (i-1) * NREFINX
  IA = PLINDEX - (II - 1) * NREFINX
  ! k = (b-1) * NREFINX + a
  POUTINDEX = (IB - 1) * NREFINX + IA

END FUNCTION FGET_K
!! -----------------------------------------------------------------------------
