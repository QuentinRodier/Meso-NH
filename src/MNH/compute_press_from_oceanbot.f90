!MNH_LIC Copyright 2021-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################################
      MODULE MODI_COMPUTE_PRESS_FROM_OCEANBOT
!     #####################################
INTERFACE COMPUTE_PRESS_FROM_OCEANBOT
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D(PRHO,PZFLUX,PSURF2D, &
                                                   PFLUX,PMASS)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO      ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:),   INTENT(IN)  :: PSURF2D   ! bottom pressure
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! press at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! press at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D
!
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT1D(PRHO,PZFLUX,PSURF, &
                                                   PFLUX,PMASS)
!
REAL, DIMENSION(:), INTENT(IN)  :: PRHO      ! virtual potential temperature
REAL, DIMENSION(:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL,               INTENT(IN)  :: PSURF  ! botttom press function
REAL, DIMENSION(:), INTENT(OUT) :: PFLUX  ! press at flux points
REAL, DIMENSION(:), INTENT(OUT) :: PMASS  ! press at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT1D
!
END INTERFACE 
END MODULE MODI_COMPUTE_PRESS_FROM_OCEANBOT
!     ######################################
      MODULE MODI_COMPUTE_PRESS_FROM_OCEANBOT3D
!     ######################################
INTERFACE
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D(PRHO,PZFLUX,PSURF2D, &
                                                   PFLUX,PMASS)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO     ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:),   INTENT(IN) :: PSURF2D! bot press function
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! bot press at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! bot press at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D
!
END INTERFACE
END MODULE MODI_COMPUTE_PRESS_FROM_OCEANBOT3D
!     ########################################################################
      SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D(PRHO,PZFLUX,PSURF2D,PFLUX,PMASS)
!     ########################################################################
!
!!****  *COMPUTE_EXNER_FROM_OCEANBOT3D* - computation of hydrostatic 
!!                                      pressure from ocean bottom
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    
!!  1 The local pressure is computed at flux points by integration of the hydrostatic
!!    relation from ground (PSURF2D) to top.
!!
!!    dP= -g rho  dz
!!
!!  2 The pressure at mass level is computed as follows and linearly 
!!    extrapolated for the uppest non-physical level:
!!
!!      ~           P(k+1)-P(k)
!!     P(k) = -----------------------
!!                lnP(k+1)-lnP(k)
!!         
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CST       : contains physical constants
!!         XG  : gravity constant
!!      Module MODD_PARAMETERS
!!         JPVEXT
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      JLR
!!    MODIFICATIONS
!!    -------------
!!      Original    Fev2021
!-------------------------------------------------------------------------------
!
!* 0.    DECLARATIONS
!        ------------
!
USE MODD_CST, ONLY : XG
USE MODD_PARAMETERS, ONLY : JPVEXT
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO      ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:)  , INTENT(IN)  :: PSURF2D! bottom pressuren
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! pres at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! pres at mass points
!
!*       0.2   Declaration of local variables
!              ------------------------------
INTEGER :: IKB,IKU,JK
!-------------------------------------------------------------------------------
!*       1.    INITIALIZATIONS
!              ---------------
IKB=JPVEXT+1
IKU=SIZE(PZFLUX,3)
!-------------------------------------------------------------------------------
!*       2.   COMPUTATION OF PRESSURE AT FLUX POINTS
!             ------------------------------------------------
PFLUX(:,:,IKB)=PSURF2D(:,:)
!
 DO JK=IKB+1,IKU
  PFLUX(:,:,JK)=PFLUX(:,:,JK-1) + XG*PRHO(:,:,JK-1)*(PZFLUX(:,:,JK-1)-PZFLUX(:,:,JK))
 END DO
 DO JK=IKB-1,1,-1
  PFLUX(:,:,JK)=PFLUX(:,:,JK+1) + XG*PRHO(:,:,JK)  *(PZFLUX(:,:,JK+1)-PZFLUX(:,:,JK))
 END DO
!-------------------------------------------------------------------------------
!                     
!*     3.   COMPUTATION OF Pressure AT MASS POINTS
!             --------------------------------------------
 PMASS(:,:,1:IKU-1)=(PFLUX(:,:,1:IKU-1)+PFLUX(:,:,2:IKU))*.5
! (PFLUX(:,:,1:IKU-1)-PFLUX(:,:,2:IKU))            &
!                      /(LOG(PFLUX(:,:,1:IKU-1))-LOG(PFLUX(:,:,2:IKU)))
!
!accute extrapolation not possible as level IKU is in atmosphere. Assume rho_air=1.2
 PMASS(:,:,IKU)=  PMASS(:,:,IKU-1) - XG*1.2 * ( PZFLUX(:,:,IKU)-PZFLUX(:,:,IKU-1) )
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT3D
!     ######################################################################
      SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT1D(PRHO,PZFLUX,PSURF,PFLUX,PMASS)
!     ######################################################################
!
!!****  *COMPUTE_PRESS_FROM_OCEANBOT1D* - computation of hydrostatic press eq
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
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/03/96
!-------------------------------------------------------------------------------
!
!* 0.    DECLARATIONS
!        ------------
!
USE MODI_COMPUTE_PRESS_FROM_OCEANBOT3D
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PRHO      ! virtual potential temperature
REAL, DIMENSION(:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL,               INTENT(IN)  :: PSURF  ! ground Exner function
REAL, DIMENSION(:), INTENT(OUT) :: PFLUX  ! Exner function at flux points
REAL, DIMENSION(:), INTENT(OUT) :: PMASS  ! Exner function at mass points
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL, DIMENSION(1,1,SIZE(PZFLUX))  :: ZRHO      ! virtual potential temperature
REAL, DIMENSION(1,1,SIZE(PZFLUX))  :: ZZFLUX    ! altitude of flux points
REAL, DIMENSION(1,1)               :: ZPSURF  ! ground Exner function
REAL, DIMENSION(1,1,SIZE(PZFLUX))  :: ZPFLUX  ! Exner function at flux points
REAL, DIMENSION(1,1,SIZE(PZFLUX))  :: ZPMASS  ! Exner function at mass points                                    
!
!-------------------------------------------------------------------------------
!
ZRHO(1,1,:)=PRHO(:)
ZZFLUX(1,1,:)=PZFLUX(:)
ZPSURF(1,1)=PSURF
!
CALL COMPUTE_PRESS_FROM_OCEANBOT3D(ZRHO,ZZFLUX,ZPSURF,ZPFLUX,ZPMASS)
!
PFLUX(:)=ZPFLUX(1,1,:)
PMASS(:)=ZPMASS(1,1,:)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANBOT1D

