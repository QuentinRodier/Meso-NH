!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!#######################
MODULE MODI_EOL_KINE_ALM
!
INTERFACE !----------------------------------------
!
SUBROUTINE EOL_KINE_ALM(KTCOUNT,KTSUBCOUNT,PTSUBSTEP,PTSTEP)
!
 INTEGER, INTENT(IN) :: KTCOUNT      ! iteration count
 INTEGER, INTENT(IN) :: KTSUBCOUNT   ! sub iteration count
 REAL,    INTENT(IN) :: PTSUBSTEP    ! sub timestep 
 REAL,    INTENT(IN) :: PTSTEP       ! timestep 
!
END SUBROUTINE EOL_KINE_ALM
!
END INTERFACE !------------------------------------
!
END MODULE MODI_EOL_KINE_ALM 
!#######################
!
!
!
!
!###################################################################
SUBROUTINE EOL_KINE_ALM(KTCOUNT,KTSUBCOUNT,PTSUBSTEP,PTSTEP)
!
!!****  *EOL_KINEMATICS * -
!!
!!    PURPOSE
!!    -------
!!      Compute positions, oritentations and velocities of all the
!!      elements of the wind turbine
!! 
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     Original        04/2017
!!     Modification    10/11/20 (PA. Joulin) Updated for a main version
!  P. Wautelet 19/07/2021: replace double precision by real to allow MNH_REAL=4 compilation
!  PA. Joulin     04/2023: add csv output for 3d frames
!!
!!---------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!*       0.1    Modules
USE MODD_EOL_KINE_ALM
USE MODD_EOL_ALM
USE MODI_EOL_PRINTER
USE MODI_EOL_MATHS
USE MODD_EOL_SHARED_IO, ONLY: LTECOUTPTS,LCSVOUTFRM
USE MODD_TIME_n, ONLY : TDTCUR
USE MODD_CST,    ONLY : XPI
!
IMPLICIT NONE 
!
!*       0.2   Declarations of dummy arguments :
!
INTEGER, INTENT(IN)                     :: KTCOUNT          ! iteration count
INTEGER, INTENT(IN)                     :: KTSUBCOUNT       ! sub iteration count
REAL,    INTENT(IN)                     :: PTSUBSTEP        ! sub timestep 
REAL,    INTENT(IN)                     :: PTSTEP           ! timestep 
!
!*       0.3 Local variables
REAL, DIMENSION(3,3) :: ZORI_MAT_X, ZORI_MAT_Y, ZORI_MAT_Z
REAL, DIMENSION(3)   :: ZADD_TO_POS
!
REAL, DIMENSION(3)   :: ZDIST_TOWO_TELT_RG ! Distance between tower elmt and tower base
REAL, DIMENSION(3)   :: ZDIST_TOWO_NELT_RG ! Distance between nacelle and base of tower
REAL, DIMENSION(3)   :: ZDIST_NAC_HUB_RG   ! Distance between hub and base of nacelle
REAL, DIMENSION(3)   :: ZDIST_HUB_BLA_RG   ! Distance between blade and base of hub
REAL, DIMENSION(3)   :: ZDIST_BLA_ELT_RG   ! Distance between blade and elements
!
REAL, DIMENSION(3)   :: ZPOS_ELTLE_RE      ! Leading Edge (LE) position, in RE
REAL, DIMENSION(3)   :: ZPOS_ELTLE_RG      ! Leading Edge (LE) position, in RG
REAL, DIMENSION(3)   :: ZPOS_ELTTE_RE      ! Trailing Edge (TE) position, in RE
REAL, DIMENSION(3)   :: ZPOS_ELTTE_RG      ! Trailing Edge (TE) position, in RG
!
REAL                             :: ZTIME                              ! TIME 
INTEGER                          :: JROT, JBLA, JTELT, JNELT, JBELT    ! Loop control
INTEGER                          :: INB_WT, INB_B, INB_BELT            ! Total numbers
INTEGER                          :: INB_TELT, INB_NELT                 ! Total numbers
INTEGER                          :: ITECOUT                            ! Unit number for Tecplot file
INTEGER                          :: ICSVOUT                            ! Unit number for TOW.csv file
!
!
!---------------------------------------------------------------
!
!*       1.    PRELIMINARIES
!              ------------- 
!
!*       1.1 Some useful integers
INB_WT   = TFARM%NNB_TURBINES
INB_B    = TTURBINE%NNB_BLADES
INB_TELT = 2
INB_NELT = 2
INB_BELT  = TBLADE%NNB_BLAELT
!
!*       1.2 Sub-time computation
ZTIME = TDTCUR%xtime+(KTSUBCOUNT)*PTSUBSTEP
!
!*       1.3 Tecplotfile : opening + headers
IF (LTECOUTPTS) THEN
 CALL OPEN_TECOUT_ALM(ITECOUT, KTCOUNT, KTSUBCOUNT)
END IF
IF (LCSVOUTFRM) THEN
 CALL OPEN_3DCSV_ALM(ICSVOUT, KTCOUNT, KTSUBCOUNT)
END IF
!
!
!---------------------------------------------------------------
!
!*       2.    COMPUTATIONS
!              ------------ 
!
DO JROT=1, INB_WT
!
! ---- TOWER ---- 
!
!* T.0 Update origin positions in RG (Base position + floating)
 XPOS_TOWO_RG(JROT,:)   = XPOS_REF(:) + XPOSINI_TOWO_RG(JROT,:) &
                                      + XTVEL_TOWO_RG(JROT,:)*ZTIME
!
!* T.1 Update orientation
 CALL GET_ORI_MAT_X(XANGINI_TOW_RG(JROT,1) + XRVEL_RT_RG(JROT,1)*ZTIME, ZORI_MAT_X)
 CALL GET_ORI_MAT_Y(XANGINI_TOW_RG(JROT,2) + XRVEL_RT_RG(JROT,2)*ZTIME, ZORI_MAT_Y)
 CALL GET_ORI_MAT_Z(XANGINI_TOW_RG(JROT,3) + XRVEL_RT_RG(JROT,3)*ZTIME, ZORI_MAT_Z)
!  Compute orientation matrix
 XMAT_RG_RT(JROT,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
!
!* T.2 Update positions in RG
 DO JTELT=1, INB_TELT
  XPOS_TELT_RG(JROT,JTELT,:) = XPOS_TOWO_RG(JROT,:) &
                             + MATMUL(XMAT_RG_RT(JROT,:,:),XPOS_TELT_RT(JROT,JTELT,:))
 END DO
!
!* T.3 Update structural velocities
 DO JTELT=1, INB_TELT
 ! Rotation of tower already in RG
 ! Translation of elements
  ZDIST_TOWO_TELT_RG(:)         = XPOS_TELT_RG(JROT,JTELT,:) - XPOS_TOWO_RG(JROT,:)
  XTVEL_TELT_RG(JROT,JTELT,:)   = XTVEL_TOWO_RG(JROT,:) &
                                + CROSS(XRVEL_RT_RG(JROT,:),ZDIST_TOWO_TELT_RG(:))
 ENDDO
!
!* T.4 Print in tecplot file
 IF (LTECOUTPTS) THEN
  DO JTELT=1, INB_TELT
   CALL PRINT_TECOUT(ITECOUT, XPOS_TELT_RG(JROT,JTELT,:))
  END DO
 END IF
!
!* T.5 Print in 3D csv file
 IF (LCSVOUTFRM) THEN
  CALL PRINT_3DFRM(ICSVOUT,'TOW',XMAT_RG_RT(JROT,:,:), XPOS_TOWO_RG(JROT,:))
 END IF
!
!
! ---- NACELLE ----
!
!* N.0 Update origin positions in RG
 XPOS_NACO_RG(JROT,:) = XPOS_TELT_RG(JROT,INB_TELT,:) &
                      + MATMUL(XMAT_RG_RT(JROT,:,:),XPOSINI_NACO_RT(JROT,:))
!
!* N.1 Update orientation
 CALL GET_ORI_MAT_X(XANGINI_NAC_RT(JROT,1) + XRVEL_RN_RT(JROT,1)*ZTIME, ZORI_MAT_X)
 CALL GET_ORI_MAT_Y(XANGINI_NAC_RT(JROT,2) + XRVEL_RN_RT(JROT,2)*ZTIME, ZORI_MAT_Y)
 CALL GET_ORI_MAT_Z(XANGINI_NAC_RT(JROT,3) + XRVEL_RN_RT(JROT,3)*ZTIME, ZORI_MAT_Z)
!
! Orientation matrix
 XMAT_RT_RN(JROT,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
 XMAT_RG_RN(JROT,:,:) = MATMUL(XMAT_RG_RT(JROT,:,:), XMAT_RT_RN(JROT,:,:))
!
!* N.2 Update positions in RG
 DO JNELT=1, INB_NELT
  XPOS_NELT_RG(JROT,JNELT,:) = XPOS_NACO_RG(JROT,:) &
                             + MATMUL(XMAT_RG_RN(JROT,:,:),XPOS_NELT_RN(JROT,JNELT,:))
 END DO
!
!* N.3 Update structural velocities
 ! Rotation of nacelle in RG
  XRVEL_RN_RG(JROT,:) = MATMUL(XMAT_RG_RT(JROT,:,:),XRVEL_RN_RT(JROT,:)) &
                            + XRVEL_RT_RG(JROT,:)
 DO JNELT=1, INB_NELT
 ! Translation of elements in RG
  ZDIST_TOWO_NELT_RG(:)       = XPOS_NELT_RG(JROT,JNELT,:) - XPOS_TOWO_RG(JROT,:)
  XTVEL_NELT_RG(JROT,JNELT,:) = XTVEL_TOWO_RG(JROT,:) & 
                              + CROSS(XRVEL_RN_RG(JROT,:),ZDIST_TOWO_NELT_RG(:))
 END DO
!
!* N.4 Print in tecplot file
 IF (LTECOUTPTS) THEN
  DO JNELT=1, INB_NELT
   CALL PRINT_TECOUT(ITECOUT, XPOS_NELT_RG(JROT,JNELT,:))
  END DO
 END IF
!
!* N.5 Print in 3D csv file
 IF (LCSVOUTFRM) THEN
  CALL PRINT_3DFRM(ICSVOUT,'NAC',XMAT_RG_RN(JROT,:,:), XPOS_NACO_RG(JROT,:))
 END IF
!
!
! ---- HUB ----
!
!* H.1 Update positions
 XPOS_HUB_RG(JROT,:)  = XPOS_NELT_RG(JROT,INB_NELT,:) &
                      + MATMUL(XMAT_RG_RN(JROT,:,:),XPOSINI_HUB_RN(JROT,:))
!
!* H.2 Update orientation
 CALL GET_ORI_MAT_X(XANGINI_HUB_RN(JROT,1) + XRVEL_RH_RN(JROT,1)*ZTIME, ZORI_MAT_X)
 CALL GET_ORI_MAT_Y(XANGINI_HUB_RN(JROT,2) + XRVEL_RH_RN(JROT,2)*ZTIME, ZORI_MAT_Y)
 CALL GET_ORI_MAT_Z(XANGINI_HUB_RN(JROT,3) + XRVEL_RH_RN(JROT,3)*ZTIME, ZORI_MAT_Z)
! Orientation matrix 
 XMAT_RN_RH(JROT,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
 XMAT_RG_RH(JROT,:,:) = MATMUL(XMAT_RG_RN(JROT,:,:), XMAT_RN_RH(JROT,:,:))
 XMAT_RH_RG(JROT,:,:) = TRANSPOSE(XMAT_RG_RH(JROT,:,:))
!
!* H.3 Update structural velocities
! Rotation of hub in RG
 XRVEL_RH_RG(JROT,:) = MATMUL(XMAT_RG_RH(JROT,:,:),XRVEL_RH_RN(JROT,:)) &
                     + XRVEL_RN_RG(JROT,:)
! Translation of hub in RG
 ZDIST_NAC_HUB_RG(:)  = XPOS_HUB_RG(JROT,:) - XPOS_NELT_RG(JROT,INB_NELT,:)
 XTVEL_HUB_RG(JROT,:) = XTVEL_NELT_RG(JROT,INB_NELT,:) + CROSS(XRVEL_RH_RG(JROT,:),ZDIST_NAC_HUB_RG(:))
!
!* H.4 Print in tecplot file
 IF (LTECOUTPTS) THEN
  CALL PRINT_TECOUT(ITECOUT, XPOS_HUB_RG(JROT,:))
 END IF
!
!* H.5 Print in 3D csv file
 IF (LCSVOUTFRM) THEN
  CALL PRINT_3DFRM(ICSVOUT,'HUB',XMAT_RG_RH(JROT,:,:), XPOS_HUB_RG(JROT,:))
 END IF
!
!
! ---- BLADES ----
!
 DO JBLA=1, INB_B
!* B.1 Update positions
  XPOS_BLA_RG(JROT,JBLA,:)  = XPOS_HUB_RG(JROT,:) &
                            + MATMUL(XMAT_RG_RH(JROT,:,:),XPOSINI_BLA_RH(JROT,JBLA,:))
!
!* B.2 Update orientation
  CALL GET_ORI_MAT_X(XANGINI_BLA_RH(JROT,JBLA,1) + XRVEL_RB_RH(JROT,JBLA,1)*ZTIME, ZORI_MAT_X)
  CALL GET_ORI_MAT_Y(XANGINI_BLA_RH(JROT,JBLA,2) + XRVEL_RB_RH(JROT,JBLA,2)*ZTIME, ZORI_MAT_Y)
  CALL GET_ORI_MAT_Z(XANGINI_BLA_RH(JROT,JBLA,3) + XRVEL_RB_RH(JROT,JBLA,3)*ZTIME, ZORI_MAT_Z)
! Orientation matrix
  XMAT_RH_RB(JROT,JBLA,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
  XMAT_RG_RB(JROT,JBLA,:,:) = MATMUL(XMAT_RG_RH(JROT,:,:), XMAT_RH_RB(JROT,JBLA,:,:))
!
!* B.3 Update structural velocities
! Rotation of blade in RG
  XRVEL_RB_RG(JROT,JBLA,:) = XRVEL_RH_RG(JROT,:) &
                           + MATMUL(XMAT_RG_RB(JROT,JBLA,:,:),XRVEL_RB_RH(JROT,JBLA,:))
! Translation of blade in RG
  ZDIST_HUB_BLA_RG(:) = XPOS_BLA_RG(JROT,JBLA,:) - XPOS_HUB_RG(JROT,:)
  XTVEL_BLA_RG(JROT,JBLA,:) = XTVEL_HUB_RG(JROT,:) &
                            + CROSS(XRVEL_RB_RG(JROT,JBLA,:),ZDIST_HUB_BLA_RG(:))
!
!* B.4 Print in tecplot file
  IF (LTECOUTPTS) THEN
   CALL PRINT_TECOUT(ITECOUT, XPOS_BLA_RG(JROT,JBLA,:))
  END IF
!
!* B.5 Print in 3D csv file
  IF (LCSVOUTFRM) THEN
   CALL PRINT_3DFRM(ICSVOUT,'BLA',XMAT_RG_RB(JROT,JBLA,:,:), XPOS_BLA_RG(JROT,JBLA,:))
  END IF
!
! ---- ELEMENTS ----
!* E.0 Positioning sections (cuts) in RG
  DO JBELT=1, INB_BELT+1
   XPOS_SEC_RG(JROT,JBLA,JBELT,:)  = XPOS_BLA_RG(JROT,JBLA,:) &
                                   + MATMUL(XMAT_RG_RB(JROT,JBLA,:,:),XPOS_SEC_RB(JROT,JBLA,JBELT,:))
  ENDDO
!
!
  DO JBELT=1, INB_BELT
!* E.1 Positioning sections centers (application points) in RG
   XPOS_ELT_RG(JROT,JBLA,JBELT,:)  = XPOS_BLA_RG(JROT,JBLA,:) &
                                   + MATMUL(XMAT_RG_RB(JROT,JBLA,:,:),XPOS_ELT_RB(JROT,JBLA,JBELT,:))
!* E.2 Update orientation
   CALL GET_ORI_MAT_X(XANGINI_ELT_RB(JROT,JBLA,JBELT,1) &
                      + XRVEL_RE_RB(JROT,JBLA,JBELT,1)*ZTIME, ZORI_MAT_X)
   CALL GET_ORI_MAT_Y(XANGINI_ELT_RB(JROT,JBLA,JBELT,2) &
                      + XRVEL_RE_RB(JROT,JBLA,JBELT,2)*ZTIME, ZORI_MAT_Y)
   CALL GET_ORI_MAT_Z(XANGINI_ELT_RB(JROT,JBLA,JBELT,3) &
                      + XRVEL_RE_RB(JROT,JBLA,JBELT,3)*ZTIME, ZORI_MAT_Z)
! Orientation matrix
   XMAT_RB_RE(JROT,JBLA,JBELT,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y,ZORI_MAT_Z))
   XMAT_RG_RE(JROT,JBLA,JBELT,:,:) = MATMUL(XMAT_RG_RB(JROT,JBLA,:,:), XMAT_RB_RE(JROT,JBLA,JBELT,:,:))
   XMAT_RE_RG(JROT,JBLA,JBELT,:,:) = TRANSPOSE(XMAT_RG_RE(JROT,JBLA,JBELT,:,:))
!
!* E.3 Update structural velocities
! Rotation of elements in RG
   XRVEL_RE_RG(JROT,JBLA,JBELT,:) = XRVEL_RB_RG(JROT,JBLA,:) &
                                  + MATMUL(XMAT_RG_RE(JROT,JBLA,JBELT,:,:),&
                                    XRVEL_RE_RB(JROT,JBLA,JBELT,:))
! Translation of elements in RG
   ZDIST_BLA_ELT_RG(:) = XPOS_ELT_RG(JROT,JBLA,JBELT,:) - XPOS_BLA_RG(JROT,JBLA,:)
   XTVEL_ELT_RG(JROT,JBLA,JBELT,:) = XTVEL_BLA_RG(JROT,JBLA,:) &
                                   + CROSS(XRVEL_RE_RG(JROT,JBLA,JBELT,:),ZDIST_BLA_ELT_RG(:))
   XTVEL_ELT_RE(JROT,JBLA,JBELT,:) = MATMUL(XMAT_RE_RG(JROT,JBLA,JBELT,:,:),&
                                            XTVEL_ELT_RG(JROT,JBLA,JBELT,:))
!
!* E.4 Print in tecplot file
   IF (LTECOUTPTS) THEN
    CALL PRINT_TECOUT(ITECOUT, XPOS_ELT_RG(JROT,JBLA,JBELT,:))
   END IF
!
!* E.5 Print in 3D csv file
   IF (LCSVOUTFRM) THEN
    CALL PRINT_3DFRM(ICSVOUT,'ELT',XMAT_RG_RE(JROT,JBLA,JBELT,:,:), XPOS_ELT_RG(JROT,JBLA,JBELT,:))
   END IF
!
! ---- Leading Edge and Trailing Edge ----
! It is just to have a tecplot more beautiful
! For the moment, they are useless for computations
   IF (LTECOUTPTS) THEN
! LE.1 Update Positions
   ZPOS_ELTLE_RE(1)  = 0d0
   ZPOS_ELTLE_RE(2)  = - 1d0/4d0 * XCHORD_ELT(JROT,JBLA,JBELT)
   ZPOS_ELTLE_RE(3)  = 0d0
   ZPOS_ELTTE_RE(1)  = 0d0
   ZPOS_ELTTE_RE(2)  = + 3d0/4d0 * XCHORD_ELT(JROT,JBLA,JBELT)
   ZPOS_ELTTE_RE(3)  = 0d0
!
   ZPOS_ELTLE_RG(:)  = XPOS_ELT_RG(JROT,JBLA,JBELT,:)          &
                     + MATMUL(XMAT_RG_RE(JROT,JBLA,JBELT,:,:), &
                       ZPOS_ELTLE_RE(:))
   ZPOS_ELTTE_RG(:)  = XPOS_ELT_RG(JROT,JBLA,JBELT,:)          &
                     + MATMUL(XMAT_RG_RE(JROT,JBLA,JBELT,:,:), &
                       ZPOS_ELTTE_RE(:))
                                
!* LE.2 Print in tecplot file
    CALL PRINT_TECOUT(ITECOUT, ZPOS_ELTLE_RG(:))
    CALL PRINT_TECOUT(ITECOUT, ZPOS_ELTTE_RG(:))
   END IF
!
!
  END DO ! Blade element loop
 END DO ! Blade loop
END DO ! Rotor loop
!
! Closing tec file
IF (LTECOUTPTS) THEN
 CLOSE(ITECOUT)
END IF
IF (LCSVOUTFRM) THEN
 CLOSE(ICSVOUT)
END IF
!
END SUBROUTINE EOL_KINE_ALM
