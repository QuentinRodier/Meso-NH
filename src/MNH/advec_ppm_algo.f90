!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 adiab 2007/03/27 10:07:52
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_ADVEC_PPM_ALGO
!     ##########################
!
INTERFACE
!
      SUBROUTINE  ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, KGRID, PFIELDT, &
                                 PRHODJ, PTSTEP, &
                                 PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1,PRHOZ2,&
                                 PSRC, KTCOUNT, PCRU, PCRV, PCRW)
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HMET_ADV_SCHEME
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PFIELDT      ! variable at t
INTEGER,                INTENT(IN)  :: KGRID        ! C grid localisation
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCRU, PCRV, PCRW ! Courant numbers 
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHODJ  ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOX1, PRHOX2
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOY1, PRHOY2
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOZ1, PRHOZ2
REAL,                   INTENT(IN)  :: PTSTEP  ! Time step 
INTEGER,                INTENT(IN)  :: KTCOUNT ! iteration count
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSRC    ! source term after advection
!
END SUBROUTINE  ADVEC_PPM_ALGO
!
END INTERFACE
!
END MODULE MODI_ADVEC_PPM_ALGO
!
!
!     ##########################################################################
      SUBROUTINE  ADVEC_PPM_ALGO(HMET_ADV_SCHEME, HLBCX, HLBCY, KGRID, PFIELDT, &
                                 PRHODJ, PTSTEP, &
                                 PRHOX1, PRHOX2, PRHOY1, PRHOY2, PRHOZ1,PRHOZ2,&
                                 PSRC, KTCOUNT, PCRU, PCRV, PCRW)
!     ##########################################################################
!!
!!****  *ADVEC_PPM_ALGO* - interface for 3D advection with PPM type scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODULE MODD_ARGSLIST
!!         HALO2LIST_ll : type for a list of "HALO2_lls"
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!
!USE MODE_ll
!
!USE MODD_CONF
!USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!USE MODI_ADVEC_4TH_ORDER_AUX
USE MODI_SHUMAN
USE MODI_PPM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
CHARACTER (LEN=6),               INTENT(IN) :: HMET_ADV_SCHEME
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PFIELDT      ! variable at t
INTEGER,                INTENT(IN)  :: KGRID        ! C grid localisation
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCRU, PCRV, PCRW ! Courant numbers 
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHODJ  ! density
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOX1, PRHOX2
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOY1, PRHOY2
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHOZ1, PRHOZ2
REAL,                   INTENT(IN)  :: PTSTEP  ! Time step 
INTEGER,                INTENT(IN)  :: KTCOUNT ! iteration count
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSRC    ! source term after advection
!
!TYPE(HALO2_ll), OPTIONAL, POINTER   :: TPHALO2 ! halo2 for the field at t
!
!*       0.2   Declarations of local variables :
!
!INTEGER:: IW,IE,IS,IN,IT,IB,IWF,IEF,ISF,INF   ! Coordinate of 4th order diffusion area
!
!-------------------------------------------------------------------------------
!
! The scalar PFIELDT is first advected by U*dt first in X, then the resulting
! field is a advected in Y and finally in Z direction. The advection steps are
! stored in PSRC which is finally passed back to the model as a source after
! advection. 
!
!*       0.     INITIAL STEP
!               ------------
!
PSRC = PFIELDT
!
SELECT CASE (HMET_ADV_SCHEME)
!
! unlimited scheme (Skamarock notation)
!
CASE('PPM_00')
!
   IF (MODULO(KTCOUNT,2) .EQ. 0) THEN ! JUANTEST50
!
!*       1.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_X(HLBCX, KGRID, PSRC, PCRU, PRHODJ, PTSTEP)
      PSRC = PSRC / PRHOX1
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_Y(HLBCY, KGRID, PSRC, PCRV, PRHOX1, PTSTEP)
      PSRC = PSRC / PRHOY1
!
!*       3.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_Z(KGRID, PSRC, PCRW, PRHOY1, PTSTEP)
      PSRC = PSRC / PRHOZ1
!
   ELSE
!
!
!*       1.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_Z(KGRID, PSRC, PCRW, PRHODJ, PTSTEP)
      PSRC = PSRC / PRHOZ2
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_Y(HLBCY, KGRID, PSRC, PCRV, PRHOZ2, PTSTEP)
      PSRC = PSRC / PRHOY2
!
!*       3.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = PPM_S0_X(HLBCX, KGRID, PSRC, PCRU, PRHOY2, PTSTEP)
      PSRC = PSRC / PRHOX2
!
   END IF
!
! classic (Colella) monotonic scheme
!
CASE('PPM_01')
!
   IF (MODULO(KTCOUNT,2) .EQ. 0) THEN
!
!*       1.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHODJ) - &
           PPM_01_X(HLBCX, KGRID, PSRC, PCRU, PRHODJ, PTSTEP)
      PSRC = PSRC / PRHOX1
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHOX1) - &
           PPM_01_Y(HLBCY, KGRID, PSRC, PCRV, PRHOX1, PTSTEP)
      PSRC = PSRC / PRHOY1
!
!*       3.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHOY1) - &
           PPM_01_Z(KGRID, PSRC, PCRW, PRHOY1, PTSTEP)
      PSRC = PSRC / PRHOZ1
!
   ELSE
!
!*       1.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHODJ) - &
           PPM_01_Z(KGRID, PSRC, PCRW, PRHODJ, PTSTEP)
      PSRC = PSRC / PRHOZ2
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHOZ2) - &
           PPM_01_Y(HLBCY, KGRID, PSRC, PCRV, PRHOZ2, PTSTEP)
      PSRC = PSRC / PRHOY2
!
!*       3.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = (PSRC * PRHOY2) - &
           PPM_01_X(HLBCX, KGRID, PSRC, PCRU, PRHOY2, PTSTEP)
      PSRC = PSRC / PRHOX2
!
   END IF
!
! monotonic scheme (Skamarock notation)
!
CASE('PPM_02')
!
   IF (MODULO(KTCOUNT,2) .EQ. 0) THEN
!
!*       1.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_X(HLBCX, KGRID, PSRC, PCRU, PRHODJ, PRHOX1, PTSTEP)
      PSRC = PSRC / PRHOX1
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_Y(HLBCY, KGRID, PSRC, PCRV, PRHOX1, PRHOY1, PTSTEP)
      PSRC = PSRC / PRHOY1
!
!*       3.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_Z(KGRID, PSRC, PCRW, PRHOY1, PRHOZ1, PTSTEP)
      PSRC = PSRC / PRHOZ1
!
   ELSE
!
!*       1.     ADVECTION IN Z DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_Z(KGRID, PSRC, PCRW, PRHODJ, PRHOZ2, PTSTEP)
      PSRC = PSRC / PRHOZ2
!
!*       2.     ADVECTION IN Y DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_Y(HLBCY, KGRID, PSRC, PCRV, PRHOZ2, PRHOY2, PTSTEP)
      PSRC = PSRC / PRHOY2
!
!*       3.     ADVECTION IN X DIRECTION
!               ------------------------
!
      PSRC = PPM_S1_X(HLBCX, KGRID, PSRC, PCRU, PRHOY2, PRHOX2, PTSTEP)
      PSRC = PSRC / PRHOX2
!
   END IF
!
END SELECT
!
!*       4.     CREATE THE FORCING TERM TO PASS BACK TO THE MODEL
!               -------------------------------------------------
!
! PSRC now contains the field advected in one dt. 
! To create a forcing term
! compatible to the rest of the model forcings, we need to substract the
! initial field, devide by dt and muliplty by RHODJ
!
PSRC = (PSRC - PFIELDT)*PRHODJ/PTSTEP
!
END SUBROUTINE ADVEC_PPM_ALGO
