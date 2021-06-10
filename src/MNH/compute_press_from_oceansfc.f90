!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################################
      MODULE MODI_COMPUTE_PRESS_FROM_OCEANSFC
!     ##################################
INTERFACE COMPUTE_PRESS_FROM_OCEANSFC
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D(PRHO,PZFLUX,PTOP2D, &
                                                PFLUX,PMASS)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO      ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:)  , INTENT(IN)  :: PTOP2D ! 2D Pressure at domain top
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! Pressure at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! Pressure at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D
!
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC1D(PRHO,PZFLUX,PTOP, &
                                                PFLUX,PMASS)
!
REAL, DIMENSION(:), INTENT(IN)  :: PRHO      ! density
REAL, DIMENSION(:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL,               INTENT(IN)  :: PTOP   ! Pressure at domain top 
REAL, DIMENSION(:), INTENT(OUT) :: PFLUX  ! Pressure at flux points
REAL, DIMENSION(:), INTENT(OUT) :: PMASS  ! Pressure at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC1D
END INTERFACE
END MODULE MODI_COMPUTE_PRESS_FROM_OCEANSFC
!     ####################################
      MODULE MODI_COMPUTE_PRESS_FROM_OCEANSFC3D
!     ####################################
INTERFACE
            SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D(PRHO,PZFLUX,PTOP2D, &
                                                PFLUX,PMASS)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO      ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:)  , INTENT(IN)  :: PTOP2D ! Pressure at top domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! Pressure at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! Pressure at mass points
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D
END INTERFACE
END MODULE MODI_COMPUTE_PRESS_FROM_OCEANSFC3D
!     ####################################################################
      SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D(PRHO,PZFLUX,PTOP2D,PFLUX,PMASS)
!     ####################################################################
!
!!****  *COMPUTE_PRESS_FROM_OCEANSFC3D* - computation of hydrostatic
!!                                 from model top
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!  1 The local Exner function in computed by integration of the hydrostatic
!!    relation from top (PEXNTOP2D) to bottom.
!!
!!    dPI= -g/(Cpd thetav) dz
!!
!!  2 The Exner function at mass level is computed as follows and linearly
!!    extrapolated for the uppest non-physical level:
!!
!!      ~           PI(k+1)-PI(k)
!!     PI(k) = -----------------------
!!                lnPI(k+1)-lnPI(k)
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB : verbosity level for output-listing
!!         LTHINSHELL : logical for thinshell approximation
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_CST       : contains physical constants
!!         XG  : gravity constant
!!         XCPD: specific heat for dry air at constant pressure
!!         XRD : gas constant for dry air
!!      Module MODD_PARAMETERS
!!         JPVEXT,JPHEXT
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!  JLR
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/01/21
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
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHO   ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL, DIMENSION(:,:)  , INTENT(IN)  :: PTOP2D ! Pressure at domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFLUX  ! Pressure at flux points
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PMASS  ! Pressure at mass points
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IKB,IKE,IKU,JK
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATIONS
!              ---------------
!
IKB=JPVEXT+1
IKE=SIZE(PZFLUX,3)-JPVEXT
IKU=SIZE(PZFLUX,3)
!-------------------------------------------------------------------------------
!*       2.   COMPUTATION OF PRESSURE AT FLUX POINTS
!             ------------------------------------------------
PFLUX(:,:,IKE+1)=PTOP2D(:,:)
!
 DO JK=IKE,1,-1
  PFLUX(:,:,JK) = PFLUX(:,:,JK+1) + XG*PRHO(:,:,JK)*(PZFLUX(:,:,JK+1)-PZFLUX(:,:,JK))
 END DO
 DO JK=IKE+2,SIZE(PFLUX,3)
    PFLUX(:,:,JK) = PFLUX(:,:,JK-1) + XG*PRHO(:,:,JK-1)*(PZFLUX(:,:,JK-1)-PZFLUX(:,:,JK))
 END DO
!-------------------------------------------------------------------------------
!
!*       3.   COMPUTATION OF PRESSURE AT MASS POINTS
!             --------------------------------------------
 PMASS(:,:,1:IKU-1)=(PFLUX(:,:,1:IKU-1)-PFLUX(:,:,2:IKU))            &
                     /(LOG(PFLUX(:,:,1:IKU-1))-LOG(PFLUX(:,:,2:IKU)))
!accute extrapolation not possible as level IKU is in atmosphere. Assume rho_air=1.2 
 PMASS(:,:,IKU)=  PMASS(:,:,IKU-1) - XG*1.2 * ( PZFLUX(:,:,IKU)-PZFLUX(:,:,IKU-1) )
!-------------------------------------------------------------------------------
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC3D
!     ##################################################################
      SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC1D(PRHOD,PZFLUX,PTOP,PFLUX,PMASS)
!     ##################################################################
!
!!****  *COMPUTE_PRESS_FROM_OCEANSFC1D* - computation of hydrostatic
!!                                 Pressure from model top
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
!!      JLR
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/01/21
!-----------------------------------------------------------------------
!
!* 0.    DECLARATIONS
!        ------------
!
USE MODI_COMPUTE_PRESS_FROM_OCEANSFC3D
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PRHOD     ! virtual potential temperature
REAL, DIMENSION(:), INTENT(IN)  :: PZFLUX    ! altitude of flux points
REAL,               INTENT(IN)  :: PTOP   ! top pressure
REAL, DIMENSION(:), INTENT(OUT) :: PFLUX  ! Pressure at flux points
REAL, DIMENSION(:), INTENT(OUT) :: PMASS  ! Pressure at mass points
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
!JUAN
REAL, DIMENSION(3,1,SIZE(PZFLUX))  :: ZRHOD     ! virtual potential temperature
REAL, DIMENSION(3,1,SIZE(PZFLUX))  :: ZZFLUX    ! altitude of flux points
REAL, DIMENSION(3,1)               :: ZPTOP   ! top pressure
REAL, DIMENSION(3,1,SIZE(PZFLUX))  :: ZPFLUX  ! Pressure at flux points
REAL, DIMENSION(3,1,SIZE(PZFLUX))  :: ZPMASS  ! Pressure at mass points
INTEGER                            :: JI        ! loop index in I
!JUAN
!
!-------------------------------------------------------------------------------
!
!JUAN
DO JI=1,3
ZRHOD(JI,1,:)=PRHOD(:)
ZZFLUX(JI,1,:)=PZFLUX(:)
ZPTOP(JI,1)=PTOP
END DO
!JUAN

!
CALL COMPUTE_PRESS_FROM_OCEANSFC3D(ZRHOD,ZZFLUX,ZPTOP,ZPFLUX,ZPMASS)
!
PFLUX(:)=ZPFLUX(2,1,:)
PMASS(:)=ZPMASS(2,1,:)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE COMPUTE_PRESS_FROM_OCEANSFC1D
