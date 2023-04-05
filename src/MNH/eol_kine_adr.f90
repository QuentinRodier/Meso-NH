!MNH_LIC Copyright 2017-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!#######################
MODULE MODI_EOL_KINE_ADR
!
INTERFACE !----------------------------------------
!
SUBROUTINE EOL_KINE_ADR(KTCOUNT,PTSTEP)
!
 INTEGER, INTENT(IN) :: KTCOUNT      ! iteration count
 REAL,    INTENT(IN) :: PTSTEP       ! timestep 
!
END SUBROUTINE EOL_KINE_ADR
!
END INTERFACE !------------------------------------
!
END MODULE MODI_EOL_KINE_ADR 
!#######################
!
!
!
!
!###################################################################
SUBROUTINE EOL_KINE_ADR(KTCOUNT,PTSTEP)
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
!!
!!---------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!*       0.1    Modules
USE MODD_EOL_KINE_ADR
USE MODD_EOL_ADR
USE MODI_EOL_MATHS
USE MODI_EOL_PRINTER
USE MODD_TIME_n, ONLY : TDTCUR
USE MODD_CST,    ONLY : XPI
USE MODD_EOL_SHARED_IO, ONLY: LTECOUTPTS,LCSVOUTFRM
!
IMPLICIT NONE 
!
!*       0.2   Declarations of dummy arguments :
!
INTEGER, INTENT(IN)                     :: KTCOUNT          ! iteration count
REAL,    INTENT(IN)                     :: PTSTEP           ! timestep 
!
!*       0.3 Local variables
REAL, DIMENSION(3,3) :: ZORI_TMAT_X, ZORI_TMAT_Y, ZORI_TMAT_Z
REAL, DIMENSION(3,3) :: ZORI_MAT_X, ZORI_MAT_Y, ZORI_MAT_Z
REAL, DIMENSION(3)   :: ZADD_TO_POS
!
REAL, DIMENSION(3)   :: ZDIST_TOWO_TELT_RG ! Distance between tower elmt and tower base
REAL, DIMENSION(3)   :: ZDIST_TOWO_NELT_RG ! Distance between nacelle and base of tower
REAL, DIMENSION(3)   :: ZDIST_NAC_HUB_RG   ! Distance between hub and base of nacelle
REAL, DIMENSION(3)   :: ZDIST_HUB_CYL_RG   ! Distance between cylindrical base and base of hub
REAL, DIMENSION(3)   :: ZDIST_CYL_ELT_RG   ! Distance between cylindrical base and elements
!
REAL                             :: ZTIME                              ! TIME 
INTEGER                          :: JROT, JTELT, JNELT, JAZI ,JRAD     ! Loop control
INTEGER                          :: INB_WT,INB_B,INB_AELT, INB_RELT         ! Total numbers
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
INB_WT    = TFARM%NNB_TURBINES
INB_B     = TTURBINE%NNB_BLADES
INB_TELT  = 2
INB_NELT  = 2 
INB_AELT  = TBLADE%NNB_AZIELT
INB_RELT  = TBLADE%NNB_RADELT

!
!*       1.2 Sub-time computation
ZTIME = TDTCUR%xtime
!
!*       1.3 Tecplotfile : opening + headers
IF (LTECOUTPTS) THEN
 CALL OPEN_TECOUT_ADR(ITECOUT, KTCOUNT)
END IF
IF (LCSVOUTFRM) THEN
 CALL OPEN_3DCSV_ADR(ICSVOUT, KTCOUNT)
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
  CALL PRINT_3DFRM(ICSVOUT,'TOW',XMAT_RG_RT(JROT,:,:),XPOS_TOWO_RG(JROT,:))
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
  CALL PRINT_3DFRM(ICSVOUT,'NAC',XMAT_RG_RN(JROT,:,:),XPOS_NACO_RG(JROT,:))
 END IF
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
  CALL PRINT_3DFRM(ICSVOUT,'HUB',XMAT_RG_RH(JROT,:,:),XPOS_HUB_RG(JROT,:))
 END IF
!
! ---- CYLINDRICAL ----
!* C.1 Update positions
 XPOS_CYL_RG(JROT,:)  = XPOS_HUB_RG(JROT,:) &
                      + MATMUL(XMAT_RG_RH(JROT,:,:),XPOSINI_CYL_RH(JROT,:))
!
!* C.2 Update orientation
 CALL GET_ORI_MAT_X(XANGINI_CYL_RH(JROT,1) + XRVEL_RC_RH(JROT,1)*ZTIME, ZORI_MAT_X)
 CALL GET_ORI_MAT_Y(XANGINI_CYL_RH(JROT,2) + XRVEL_RC_RH(JROT,2)*ZTIME, ZORI_MAT_Y)
 CALL GET_ORI_MAT_Z(XANGINI_CYL_RH(JROT,3) + XRVEL_RC_RH(JROT,3)*ZTIME, ZORI_MAT_Z)
! Orientation matrix 
 XMAT_RH_RC(JROT,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
 XMAT_RG_RC(JROT,:,:) = MATMUL(XMAT_RG_RH(JROT,:,:), XMAT_RH_RC(JROT,:,:))
!
!* C.3 Update structural velocities
! Rotation of cylindrical in RG
 XRVEL_RC_RG(JROT,:) = MATMUL(XMAT_RG_RC(JROT,:,:),XRVEL_RC_RH(JROT,:)) &
                     + XRVEL_RH_RG(JROT,:)
! Translation of hub in RG
 ZDIST_HUB_CYL_RG(:)  = XPOS_CYL_RG(JROT,:) - XPOS_HUB_RG(JROT,:)
 XTVEL_CYL_RG(JROT,:) = XTVEL_HUB_RG(JROT,:) + CROSS(XRVEL_RC_RG(JROT,:),ZDIST_HUB_CYL_RG(:))
!
!* C.4 Print in tecplot file
 IF (LTECOUTPTS) THEN
  CALL PRINT_TECOUT(ITECOUT, XPOS_CYL_RG(JROT,:))
 END IF
!
!* C.5 Print in 3D csv file
 IF (LCSVOUTFRM) THEN
  CALL PRINT_3DFRM(ICSVOUT,'CYL',XMAT_RG_RC(JROT,:,:),XPOS_CYL_RG(JROT,:))
 END IF
!
! ---- ANNULAR ELEMENTS ----
!* A.0 Positioning sections (cuts) in RG
 DO JAZI=1, INB_AELT
  DO JRAD=1, INB_RELT+1
   XPOS_SEC_RG(JROT,JAZI,JRAD,:)  = XPOS_CYL_RG(JROT,:) &
                                   + MATMUL(XMAT_RG_RC(JROT,:,:),GET_VEC_CART(XPOS_SEC_RC(JROT,JAZI,JRAD,:)))
  END DO

  DO JRAD=1, INB_RELT
!* A.1 Positioning sections centers (application points) in RG
   XPOS_ELT_RG(JROT,JAZI,JRAD,:)  = XPOS_CYL_RG(JROT,:) &
                                   + MATMUL(XMAT_RG_RC(JROT,:,:),GET_VEC_CART(XPOS_ELT_RC(JROT,JAZI,JRAD,:)))
!* A.2 Update orientation
!    A.2.1 Orientation in elements transition ref. frame
   CALL GET_ORI_MAT_X(XANGINI_ELT_RC(JROT,JAZI,JRAD,1) &
                      + XRVEL_RA_RC(JROT,JAZI,JRAD,1)*ZTIME, ZORI_TMAT_X)
   CALL GET_ORI_MAT_Y(XANGINI_ELT_RC(JROT,JAZI,JRAD,2) &
                      + XRVEL_RA_RC(JROT,JAZI,JRAD,2)*ZTIME, ZORI_TMAT_Y)
   CALL GET_ORI_MAT_Z(XANGINI_ELT_RC(JROT,JAZI,JRAD,3) &
                      + XRVEL_RA_RC(JROT,JAZI,JRAD,3)*ZTIME, ZORI_TMAT_Z)
! Orientation matrix
   XMAT_RC_RTA(JROT,JAZI,JRAD,:,:) = MATMUL(ZORI_TMAT_X, MATMUL(ZORI_TMAT_Y, ZORI_TMAT_Z))
   XMAT_RG_RTA(JROT,JAZI,JRAD,:,:) = MATMUL(XMAT_RG_RC(JROT,:,:), XMAT_RC_RTA(JROT,JAZI,JRAD,:,:))
   XMAT_RTA_RG(JROT,JAZI,JRAD,:,:) = TRANSPOSE(XMAT_RG_RTA(JROT,JAZI,JRAD,:,:))
!
!    A.2.2 Orientation in annular elements ref. frame

   CALL GET_ORI_MAT_X(XANGINI_ELT_RA(JROT,JAZI,JRAD,1), ZORI_MAT_X)
   CALL GET_ORI_MAT_Y(XANGINI_ELT_RA(JROT,JAZI,JRAD,2), ZORI_MAT_Y)
   CALL GET_ORI_MAT_Z(XANGINI_ELT_RA(JROT,JAZI,JRAD,3), ZORI_MAT_Z)

   XMAT_RTA_RA(JROT,JAZI,JRAD,:,:) = MATMUL(ZORI_MAT_X, MATMUL(ZORI_MAT_Y, ZORI_MAT_Z))
   XMAT_RG_RA(JROT,JAZI,JRAD,:,:) = MATMUL(XMAT_RG_RTA(JROT,JAZI,JRAD,:,:), XMAT_RTA_RA(JROT,JAZI,JRAD,:,:))
   XMAT_RA_RG(JROT,JAZI,JRAD,:,:) = TRANSPOSE(XMAT_RG_RA(JROT,JAZI,JRAD,:,:))
!

!* A.3 Update structural velocities
! Rotation of elements in RG
   XRVEL_RA_RG(JROT,JAZI,JRAD,:) = XRVEL_RC_RG(JROT,:) &
                                  + MATMUL(XMAT_RG_RA(JROT,JAZI,JRAD,:,:),&
                                    XRVEL_RA_RC(JROT,JAZI,JRAD,:))
! Translation of elements in RG
   ZDIST_CYL_ELT_RG(:) = XPOS_ELT_RG(JROT,JAZI,JRAD,:) - XPOS_CYL_RG(JROT,:)
   XTVEL_ELT_RG(JROT,JAZI,JRAD,:) = XTVEL_CYL_RG(JROT,:) &
                                   + CROSS(XRVEL_RA_RG(JROT,JAZI,JRAD,:),ZDIST_CYL_ELT_RG(:))
   XTVEL_ELT_RA(JROT,JAZI,JRAD,:) = MATMUL(XMAT_RA_RG(JROT,JAZI,JRAD,:,:),&
                                           XTVEL_ELT_RG(JROT,JAZI,JRAD,:))
!
!* A.4 Print in tecplot file
   IF (LTECOUTPTS) THEN
    CALL PRINT_TECOUT(ITECOUT, XPOS_ELT_RG(JROT,JAZI,JRAD,:))
   END IF
!
   IF (LCSVOUTFRM) THEN
    CALL PRINT_3DFRM(ICSVOUT,'ELT',XMAT_RG_RA(JROT,JAZI,JRAD,:,:),XPOS_ELT_RG(JROT,JAZI,JRAD,:))
   END IF
  END DO ! Radial loop
 END DO ! Azimutal loop
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
END SUBROUTINE EOL_KINE_ADR
