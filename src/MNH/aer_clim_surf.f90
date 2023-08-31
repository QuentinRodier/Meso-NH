!MNH_LIC Copyright 1995-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_AER_CLIM_SURF
!     ##########################
!
INTERFACE
!
    SUBROUTINE AER_CLIM_SURF( PLAT, PLON, PAESEA, PAELAN, PAEURB, PAEDES )
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PLAT, PLON ! arrays of latitude-longitude
!
REAL, DIMENSION (:),     INTENT(OUT)  :: PAESEA, PAELAN, PAEURB, PAEDES
!
END SUBROUTINE AER_CLIM_SURF
!
END INTERFACE
!
END MODULE MODI_AER_CLIM_SURF
!
!
!   ###################################################################
    SUBROUTINE AER_CLIM_SURF(PLAT, PLON, PAESEA, PAELAN, PAEURB, PAEDES )
!   ###################################################################
!
!!****  *INI_RADIATIONS * - initialisation for ECMWF radiation scheme in the MesoNH framework
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
!!  	V. Masson : extract the Aerosol initialization by surface types from ini_radiation_ecmwf.f90 routine
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2023
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!MESO-NH modules
!
USE MODE_ll
USE MODD_CONF,    ONLY : LCARTESIAN
USE MODD_PARAM_n, ONLY : CSURF
!
USE MODI_MNHGET_SURF_PARAM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PLAT, PLON ! arrays of latitude-longitude
!
REAL, DIMENSION (:),     INTENT(OUT)  :: PAESEA, PAELAN, PAEURB, PAEDES
!
!
!*       0.2   declarations of local variables
!
INTEGER :: JI, JJ, IIJ  ! loop index
!
INTEGER :: IIB           ! I index value of the first inner mass point
INTEGER :: IJB           ! J index value of the first inner mass point
INTEGER :: IIE           ! I index value of the last inner mass point
INTEGER :: IJE           ! J index value of the last inner mass point
INTEGER :: IIU           ! array size for the first  index
INTEGER :: IJU           ! array size for the second index
!
REAL, DIMENSION(:,:),ALLOCATABLE :: ZLON          ! longitude
!
! Variables for aerosols and ozone climatologies set up
LOGICAL, DIMENSION (:,:),ALLOCATABLE  :: GAFRICA, GASIA, GAUSTRALIA
REAL, DIMENSION (:,:),   ALLOCATABLE  :: ZDESERT ! desert fraction
REAL, DIMENSION (:,:),   ALLOCATABLE  :: ZSEA   ! sea fraction
REAL, DIMENSION (:,:),   ALLOCATABLE  :: ZTOWN  ! town fraction
REAL, DIMENSION (:,:),   ALLOCATABLE  :: ZBARE  ! bare soil fraction
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*       0.1  INITIALIZATIONS
!
!
!*       0.2  COMPUTES THE PHYSICAL SUBDOMAIN BOUNDS
!
CALL GET_DIM_EXT_ll ('B',IIU,IJU)
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
!-------------------------------------------------------------------------------
!
!*       7.     INITIALIZES THE ECMWF RADIATION PACKAGE 
!	        ------------------------------------------------
!
! AEROSOLS from SURFACE FRACTIONS
!
    !* deserts are only considered over Africa, southern Asia, Australia
    !* longitude between -180 and 180 for geographical tests
    !  Only bare soil fractions larger than 0.5 are supposed to contribute to
    !  desert aerosols
    !
    ALLOCATE(ZSEA      (IIU,IJU))
    ALLOCATE(ZTOWN     (IIU,IJU))
    ALLOCATE(ZBARE     (IIU,IJU))
    IF (CSURF=='EXTE') THEN
      CALL MNHGET_SURF_PARAM_n(PSEA=ZSEA,PTOWN=ZTOWN,PBARE=ZBARE)
    ELSE
      ZSEA (:,:) = 1.
      ZTOWN(:,:) = 0.
      ZBARE(:,:) = 0.
    END IF


    ALLOCATE(ZDESERT   (IIU,IJU))
    ZDESERT(:,:) = 0.

    IF (.NOT.LCARTESIAN) THEN
      !* deserts are only considered over Africa, southern Asia, Australia
      ALLOCATE(ZLON      (IIU,IJU))
      ALLOCATE(GAFRICA   (IIU,IJU))
      ALLOCATE(GASIA     (IIU,IJU))
      ALLOCATE(GAUSTRALIA(IIU,IJU))    
      !* longitude between -180 and 180 for geographical tests
      ZLON = PLON(:,:) - NINT(PLON/360.)*360.
      GAFRICA   (:,:) = PLAT(:,:) > -36.086389  .AND. PLAT(:,:) < 36.010556 &
                  .AND. ZLON(:,:) > -73.18      .AND. ZLON(:,:) < 34.158611
      GASIA     (:,:) = PLAT(:,:) > 4.358056    .AND. PLAT(:,:) < 55.335278 &
                  .AND. ZLON(:,:) > -123.157778 .AND. ZLON(:,:) <-34.285556
      GAUSTRALIA(:,:) = PLAT(:,:) > -39.561389  .AND. PLAT(:,:) < -10.251667 &
                  .AND. ZLON(:,:) > -155.041944 .AND. ZLON(:,:) < -111.405556
      !
      !  Only bare soil fractions larger than 0.5 are supposed to contribute to
      !  desert aerosols
      !
      WHERE (GAFRICA(:,:) .OR. GASIA(:,:) .OR. GAUSTRALIA(:,:)) &
      ZDESERT(:,:) = MAX( 2.*(ZBARE(:,:)-0.5) , 0.)
      !
      !
    ELSE
      !
      ZDESERT(:,:) = MAX( 2.*(ZBARE(:,:)-0.5) , 0.)
      !
    ENDIF
    !
    !* fills sea, town, desert and land surface covers for aerosols distributions
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        PAESEA(IIJ) = ZSEA(JI,JJ)
        PAEURB(IIJ) = ZTOWN(JI,JJ)
        PAEDES(IIJ) = ZDESERT(JI,JJ)
        PAELAN(IIJ) = MAX( 1.- PAESEA(IIJ) - PAEURB(IIJ) - PAEDES(IIJ) , 0.)
      END DO
    END DO
    IF (ALLOCATED(ZLON)) DEALLOCATE(ZLON)
    IF (ALLOCATED(GAFRICA))     DEALLOCATE(GAFRICA)
    IF (ALLOCATED(GASIA))     DEALLOCATE(GASIA)
    IF (ALLOCATED(GAUSTRALIA))     DEALLOCATE(GAUSTRALIA)
    IF (ALLOCATED(ZDESERT))     DEALLOCATE(ZDESERT)

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AER_CLIM_SURF
