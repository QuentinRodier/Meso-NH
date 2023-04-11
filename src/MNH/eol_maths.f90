!MNH_LIC Copyright 2018-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!****  *MODI_EOL_MATHS* -
!!
!!    PURPOSE
!!    -------
!!     Contains all mathematical functions for EOL computations.
!!    
!!    AUTHOR
!!    ------
!!     PA. Joulin               *CNRM & IFPEN*
!!
!!
!!    MODIFICATIONS
!!    -------------
!!     Original     24/01/17
!!     P. Wautelet  19/07/2021: replace double precision by real to allow MNH_REAL=4 compilation
!!     H. Toumi     04/23 : add ADR functions   
!!
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_MATHS
!     #######################
!
INTERFACE
!
FUNCTION CROSS(PA, PB)
        REAL, DIMENSION(3)             :: CROSS
        REAL, DIMENSION(3), INTENT(IN) :: PA, PB
END FUNCTION CROSS
!
FUNCTION NORM(PA)
        REAL                           :: NORM
        REAL, DIMENSION(3), INTENT(IN) :: PA
END FUNCTION NORM
!
SUBROUTINE GET_ORI_MAT_X(PTHETA, PORI_MAT_X)
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_X  ! Matrix
END SUBROUTINE GET_ORI_MAT_X
!
SUBROUTINE GET_ORI_MAT_Y(PTHETA, PORI_MAT_Y)
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_Y  ! Matrix
END SUBROUTINE GET_ORI_MAT_Y
!
SUBROUTINE GET_ORI_MAT_Z(PTHETA, PORI_MAT_Z)
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_Z  ! Matrix
END SUBROUTINE GET_ORI_MAT_Z
!
FUNCTION GET_VEC_CYL(VEC_CART)
        REAL, DIMENSION(3), INTENT(IN)     :: VEC_CART    ! cartesian vector
        REAL, DIMENSION(3)                 :: GET_VEC_CYL ! cylindrical vector
END FUNCTION GET_VEC_CYL
!
FUNCTION GET_VEC_CART(VEC_CYL)
        REAL, DIMENSION(3), INTENT(IN)     :: VEC_CYL      ! cartesian vector
        REAL, DIMENSION(3)                 :: GET_VEC_CART ! cylindrical vector
END FUNCTION GET_VEC_CART
!
FUNCTION INTERP_SPLCUB(PAV, PX, PY)
        REAL                           :: INTERP_SPLCUB ! interface
        REAL,               INTENT(IN) :: PAV  ! Abscissa where spline is to be evaluate
        REAL, DIMENSION(:), INTENT(IN) :: PX, PY
END FUNCTION INTERP_SPLCUB
!
FUNCTION INTERP_LIN8NB(PPOS, KI, KJ, KK, PVAR, PZH)
        REAL                               :: INTERP_LIN8NB ! interface
        REAL, DIMENSION(3),     INTENT(IN) :: PPOS          ! Position where we want to evaluate
        INTEGER,                INTENT(IN) :: KI, KJ, KK    ! Meso-NH cell index
        REAL, DIMENSION(:,:,:), INTENT(IN) :: PVAR,PZH      ! Variable to interpolate 
END FUNCTION INTERP_LIN8NB
!
END INTERFACE
!
END MODULE MODI_EOL_MATHS
!-------------------------------------------------------------------
!
!!****  *EOL_MATHS* -
!!
!!    PURPOSE
!!    -------
!!    Some usefull tools for wind turbine study 
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     04/2018      Original
!!
!!---------------------------------------------------------------
!#########################################################
FUNCTION CROSS(PA, PB)
! Vectorial product 3D : PA * PB
!
        REAL, DIMENSION(3) :: CROSS
        REAL, DIMENSION(3), INTENT(IN) :: PA, PB
!
        CROSS(1) = PA(2) * PB(3) - PA(3) * PB(2)
        CROSS(2) = PA(3) * PB(1) - PA(1) * PB(3)
        CROSS(3) = PA(1) * PB(2) - PA(2) * PB(1)
!
END FUNCTION CROSS
!#########################################################
!
!#########################################################
FUNCTION NORM(PA)
! Eulerian norm of 3D vector : 
!
        REAL                           :: NORM
        REAL, DIMENSION(3), INTENT(IN) :: PA
!
        NORM  = SQRT( PA(1)**2 + PA(2)**2 + PA(3)**2 )
!
END FUNCTION NORM
!
!
!#########################################################
SUBROUTINE GET_ORI_MAT_X(PTHETA, PORI_MAT_X)
! Rotation matrix of PTHETA angle around X
!
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_X  ! Matrix
!
        PORI_MAT_X (1,1) = 1d0
        PORI_MAT_X (1,2) = 0d0
        PORI_MAT_X (1,3) = 0d0
        PORI_MAT_X (2,1) = 0d0 
        PORI_MAT_X (2,2) = +COS(PTHETA)
        PORI_MAT_X (2,3) = -SIN(PTHETA)
        PORI_MAT_X (3,1) = 0d0
        PORI_MAT_X (3,2) = +SIN(PTHETA)
        PORI_MAT_X (3,3) = +COS(PTHETA)
!
END SUBROUTINE GET_ORI_MAT_X
!#########################################################
!
!#########################################################
SUBROUTINE GET_ORI_MAT_Y(PTHETA, PORI_MAT_Y)
! Rotation matrix of PTHETA angle around Y
!
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_Y  ! Matrix
!
        PORI_MAT_Y (1,1) = +COS(PTHETA)
        PORI_MAT_Y (1,2) = 0d0
        PORI_MAT_Y (1,3) = +SIN(PTHETA)
        PORI_MAT_Y (2,1) = 0d0 
        PORI_MAT_Y (2,2) = 1d0
        PORI_MAT_Y (2,3) = 0d0
        PORI_MAT_Y (3,1) = -SIN(PTHETA)
        PORI_MAT_Y (3,2) = 0d0
        PORI_MAT_Y (3,3) = +COS(PTHETA)
!
END SUBROUTINE GET_ORI_MAT_Y
!#########################################################
!
!#########################################################
SUBROUTINE GET_ORI_MAT_Z(PTHETA, PORI_MAT_Z)
! Rotation matrix of PTHETA angle around Z
!
        REAL, INTENT(IN)                   :: PTHETA      ! Angle
        REAL, DIMENSION(3,3), INTENT(OUT)  :: PORI_MAT_Z  ! Matrix
!
        PORI_MAT_Z (1,1) = +COS(PTHETA)
        PORI_MAT_Z (1,2) = -SIN(PTHETA)
        PORI_MAT_Z (1,3) = 0d0
        PORI_MAT_Z (2,1) = +SIN(PTHETA)
        PORI_MAT_Z (2,2) = +COS(PTHETA)
        PORI_MAT_Z (2,3) = 0d0
        PORI_MAT_Z (3,1) = 0d0
        PORI_MAT_Z (3,2) = 0d0
        PORI_MAT_Z (3,3) = 1d0
!
END SUBROUTINE GET_ORI_MAT_Z
!#########################################################
!
!#########################################################
FUNCTION GET_VEC_CYL(VEC_CART)
! Obtain cylindrical coordinates from a cartesian vector
!
        REAL, DIMENSION(3), INTENT(IN)     :: VEC_CART    ! cartesian vector
        REAL, DIMENSION(3)                 :: GET_VEC_CYL     ! cylindrical vector
!
        GET_VEC_CYL(1) = SQRT(VEC_CART(1)**2+VEC_CART(2)**2)
        GET_VEC_CYL(2) = ATAN2(VEC_CART(2),VEC_CART(1))
        GET_VEC_CYL(3) = VEC_CART(3)
!
END FUNCTION GET_VEC_CYL
!#########################################################
!
!#########################################################
FUNCTION GET_VEC_CART(VEC_CYL)
! Obtain cylindrical coordinates from a cartesian vector
!
        REAL, DIMENSION(3), INTENT(IN)     :: VEC_CYL    ! cartesian vector
        REAL, DIMENSION(3)                 :: GET_VEC_CART     ! cylindrical vector
!
        GET_VEC_CART(1) = VEC_CYL(1)*COS(VEC_CYL(2))
        GET_VEC_CART(2) = VEC_CYL(1)*SIN(VEC_CYL(2))
        GET_VEC_CART(3) = VEC_CYL(3)
!
END FUNCTION GET_VEC_CART
!#########################################################
!
!#########################################################
FUNCTION INTERP_SPLCUB(PAV, PX, PY)
! adapted from https://ww2.odu.edu/~agodunov/computing/programs/book2/Ch01/spline.f90
!
IMPLICIT NONE
REAL,               INTENT(IN) :: PAV  ! Abscissa where spline is to be evaluate
REAL, DIMENSION(:), INTENT(IN) :: PX, PY
!
INTEGER                        :: INBVAL                         ! Nb points of data
REAL, ALLOCATABLE              :: ZCOEF1(:),ZCOEF2(:),ZCOEF3(:)  ! Coefficients
!
INTEGER                        :: II, IJ, IBOT, ITOP, IMID
REAL                           :: ZH
REAL                           :: PDX
REAL                           :: INTERP_SPLCUB ! function
!
! --------- Intialisations ---------
INBVAL = SIZE(PX)
ALLOCATE(ZCOEF1(INBVAL))
ALLOCATE(ZCOEF2(INBVAL))
ALLOCATE(ZCOEF3(INBVAL))
!
! --------- Calculs des coefficients --------- 
!
! - Check size of input data
IF (INBVAL < 2 ) THEN
 RETURN
END IF 
IF (INBVAL < 3 ) THEN 
 ZCOEF1(1) = (PY(2)-PY(1))/(PX(2)-PX(1))
 ZCOEF2(1) = 0.
 ZCOEF3(1) = 0.
 ZCOEF1(2) = ZCOEF1(1)
 ZCOEF2(2) = 0.
 ZCOEF3(2) = 0.
 RETURN
END IF
!
! - Preliminaries
ZCOEF3(1) = PX(2) - PX(1)
ZCOEF2(2) = (PY(2) - PY(1))/ZCOEF3(1)
DO II = 2, INBVAL-1
 ZCOEF3(II)   = PX(II+1) - PX(II) 
 ZCOEF1(II)   = 2.0 * (ZCOEF3(II-1) + ZCOEF3(II))
 ZCOEF2(II+1) = (PY(II+1) - PY(II))/ZCOEF3(II)
 ZCOEF2(II)   = ZCOEF2(II+1) - ZCOEF2(II)
END DO
!
! - Boundaries
ZCOEF1(1)      = - ZCOEF3(1)
ZCOEF1(INBVAL) = - ZCOEF3(INBVAL-1)
ZCOEF2(1)      = 0.0 
ZCOEF2(INBVAL) = 0.0 
IF (INBVAL /= 3) THEN 
 ZCOEF2(1)      = ZCOEF2(3)/(PX(4)-PX(2)) - ZCOEF2(2)/(PX(3)-PX(1))
 ZCOEF2(INBVAL) = ZCOEF2(INBVAL-1)/(PX(INBVAL)-PX(INBVAL-2)) &
                 -ZCOEF2(INBVAL-2)/(PX(INBVAL-1)-PX(INBVAL-3))
 ZCOEF2(1)      = ZCOEF2(1)*ZCOEF3(1)**2 / (PX(4)-PX(1))
 ZCOEF2(INBVAL) =-ZCOEF2(INBVAL)*ZCOEF3(INBVAL-1)**2/(PX(INBVAL)-PX(INBVAL-3))
END IF
!
! - Forward elemination
DO II = 2, INBVAL
 ZH = ZCOEF3(II-1)/ZCOEF1(II-1)
 ZCOEF1(II) = ZCOEF1(II) - ZH*ZCOEF3(II-1)
 ZCOEF2(II) = ZCOEF2(II) - ZH*ZCOEF2(II-1)
END DO
!
! - Back substitution 
ZCOEF2(INBVAL) = ZCOEF2(INBVAL)/ZCOEF1(INBVAL)
DO IJ = 1, INBVAL-1
 II = INBVAL-IJ
 ZCOEF2(II) = (ZCOEF2(II) - ZCOEF3(II)*ZCOEF2(II+1))/ZCOEF1(II)
END DO
!
! - Spline coefficient calculations 
ZCOEF1(INBVAL) = (PY(INBVAL) - PY(INBVAL-1))/ZCOEF3(INBVAL-1) &
                + ZCOEF3(INBVAL-1)*(ZCOEF2(INBVAL-1) + 2.0*ZCOEF2(INBVAL))
DO II = 1, INBVAL-1
 ZCOEF1(II) = (PY(II+1) - PY(II))/ZCOEF3(II) &
             - ZCOEF3(II)*(ZCOEF2(II+1) + 2.0*ZCOEF2(II))
 ZCOEF3(II) = (ZCOEF2(II+1) - ZCOEF2(II))/ZCOEF3(II)
 ZCOEF2(II) = 3.0*ZCOEF2(II)
END DO
ZCOEF2(INBVAL) = 3.0*ZCOEF2(INBVAL)
ZCOEF3(INBVAL) = ZCOEF3(INBVAL-1)
 
! --------- Spline cubic interpolation ---------

! If the absciss PAV is out of range
! The ordinate will be the limit value (left or right) 
IF (PAV <= PX(1)) THEN 
 INTERP_SPLCUB = PY(1)
 RETURN
END IF
IF (PAV >= PX(INBVAL)) THEN
 INTERP_SPLCUB = PY(INBVAL)
 RETURN
END IF

! Dichotomie research for IBOT, tq : PX(IBOT) <= PAV <= PX(IBOT+1)
IBOT = 1
ITOP = INBVAL +1
DO WHILE (ITOP > IBOT+1)
 IMID = (IBOT + ITOP)/2
 IF (PAV < PX(IMID)) THEN
  ITOP = IMID
 ELSE
  IBOT = IMID
 END IF
END DO

! Evaluation of spline interpolation
PDX = PAV - PX(IBOT)
INTERP_SPLCUB = PY(IBOT)+PDX*(ZCOEF1(IBOT)+PDX*(ZCOEF2(IBOT)+PDX*ZCOEF3(IBOT)))

! Endings
DEALLOCATE(ZCOEF1)
DEALLOCATE(ZCOEF2)
DEALLOCATE(ZCOEF3)

END FUNCTION INTERP_SPLCUB
!#########################################################
!
!#########################################################
FUNCTION INTERP_LIN8NB(PPOS, KI, KJ, KK, PVAR, PZH)
!
USE MODD_GRID_n, ONLY: XDXHAT, XXHATM, XDYHAT, XYHATM
!
REAL                               :: INTERP_LIN8NB  ! Return
REAL, DIMENSION(3),     INTENT(IN) :: PPOS           ! Position where we want to evaluate
INTEGER,                INTENT(IN) :: KI, KJ, KK     ! Meso-NH cell index
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVAR           ! Variable to interpolate 
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZH            ! Vertical height to interpolate 
!
INTEGER  :: IIP, IJP, IKP                 ! Previous cell index : P = i + 1
INTEGER  :: IIN, IJN, IKN                 ! Next cell index : N = i - 1
!
REAL     :: ZUXNN, ZUXNP, ZUXPP, ZUXPN    ! Interpolated variables (VAR) in X plane (VAR = A*POS + B)
!
REAL     :: ZHXNN, ZHXNP, ZHXPP, ZHXPN    ! Interpotaled variables (VAR) in X plane (VAR = A*POS + B)
!
REAL     :: ZUXN, ZUXP                    ! Interpolated variables (VAR) in Y plance (VAR = A*POS + B)
!
!
REAL     :: ZALPHAX, ZALPHAY, ZALPHAZ     ! Interpolated variables (VAR) in Z plane (VAR = A*POS + B)

REAL     :: ZUX                           ! Interpolated variable (VAR) in Z plane (VAR = A*POS + B)
!
! -----------------------------------------------
!
! FINDING 8 NEIGHBOORS 
! -- X axis
IF (PPOS(1) <= XXHATM(KI)) THEN
 IIP = KI - 1
 IIN = KI
ELSE   
 IIP = KI
 IIN = KI + 1
END IF
! -- Y axis
IF (PPOS(2) <= XYHATM(KJ)) THEN
 IJP = KJ - 1
 IJN = KJ
ELSE   
 IJP = KJ
 IJN = KJ + 1
END IF
! -- Z axis
IF (PPOS(3) <= PZH(KI,KJ,KK)) THEN
 IKP = KK - 1
 IKN = KK
ELSE   
 IKP = KK
 IKN = KK + 1
END IF
!
! INTERPOLATION 
! -- Along X
! -- -- Alpha
ZALPHAX = (PPOS(1) -  XXHATM(IIP)) / XDXHAT(IIP)
!!PRINT*, "ZALPHAX = ", ZALPHAX
! -- -- -- Wind
! -- -- Interpolated variable in temporary plane X
ZUXNN = (1-ZALPHAX)*PVAR(IIP,IJN,IKN) + ZALPHAX*PVAR(IIN,IJN,IKN)
ZUXNP = (1-ZALPHAX)*PVAR(IIP,IJN,IKP) + ZALPHAX*PVAR(IIN,IJN,IKP)
ZUXPP = (1-ZALPHAX)*PVAR(IIP,IJP,IKP) + ZALPHAX*PVAR(IIN,IJP,IKP)
ZUXPN = (1-ZALPHAX)*PVAR(IIP,IJP,IKN) + ZALPHAX*PVAR(IIN,IJP,IKN)
! -- -- -- Height
ZHXNN = (1-ZALPHAX)*PZH(IIP,IJN,IKN) + ZALPHAX*PZH(IIN,IJN,IKN)
ZHXNP = (1-ZALPHAX)*PZH(IIP,IJN,IKP) + ZALPHAX*PZH(IIN,IJN,IKP)
ZHXPP = (1-ZALPHAX)*PZH(IIP,IJP,IKP) + ZALPHAX*PZH(IIN,IJP,IKP)
ZHXPN = (1-ZALPHAX)*PZH(IIP,IJP,IKN) + ZALPHAX*PZH(IIN,IJP,IKN)
!
!
! -- Along Y
! -- -- Alpha
ZALPHAY = (PPOS(2) -  XYHATM(IJP)) / XDYHAT(IJP)
!PRINT*, "ZALPHAY = ", ZALPHAY
! -- -- Interpolated variable in temporary plane Y
! -- -- -- Wind
ZUXN = (1-ZALPHAY)*ZUXPN + ZALPHAY*ZUXNN
ZUXP = (1-ZALPHAY)*ZUXPP + ZALPHAY*ZUXNP
! -- -- -- Height
ZHXN = (1-ZALPHAY)*ZHXPN + ZALPHAY*ZHXNN
ZHXP = (1-ZALPHAY)*ZHXPP + ZALPHAY*ZHXNP
!
!
! -- Along Z
! -- -- Alpha Z
ZALPHAZ = (PPOS(3) - ZHXP) / (ZHXN - ZHXP)
!PRINT*, "ZALPHAZ = ", ZALPHAZ
ZUX = (1 - ZALPHAZ)*ZUXP + ZALPHAZ*ZUXN
!
!
INTERP_LIN8NB = ZUX
!
!
!
END FUNCTION INTERP_LIN8NB
!#########################################################
