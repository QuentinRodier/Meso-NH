!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!       #####################
MODULE MODI_IBM_1DINT
  !     #####################
  !
  INTERFACE 
     !
     FUNCTION IBM_1DINT(PLOCATI,PVALUEI,HINTERP) RESULT(PVALUEG)
       !
       REAL,   DIMENSION(:) , INTENT(IN)  :: PLOCATI
       REAL,   DIMENSION(:) , INTENT(IN)  :: PVALUEI
       CHARACTER(LEN=3)     , INTENT(IN)  :: HINTERP
       REAL                               :: PVALUEG
       !
     END FUNCTION IBM_1DINT
     !
  END INTERFACE
  !
END MODULE MODI_IBM_1DINT
!
!     ###########################################################
FUNCTION IBM_1DINT(PLOCATI,PVALUEI,HINTERP) RESULT(PVALUEG)
  !     ###########################################################
  !
  !!****  *IBM_INTER_1DINT* - Classical Lagrange interpolation 1D
  !!
  !!    PURPOSE
  !!    -------
  !     This function interpolates the 1D fields from the image(s) point
  !     to the mirror point associated to each ghost nodes. The interpolation
  !     weighting is based on the Lagrange polynomials between the image point 
  !     F(X,Y,Z) and each N selected nearest nodes F(Xi,Yi,Zi). The number of
  !     nodes is depending on the interpolation order. The direction of the
  !     interpolation is normal to the interface. 
  !
  !!
  !!    METHOD
  !!    ------
  !!
  !          F(X,Y,Z)= sum(i=1toN)sum(j=1toN)sum(k=1toN)[[[Li(x)Lj(y)Lz(k)F(Xi,Yi,Zi)]]]
  !                    where La(B)=prod(l=1toN,l/=a) (B-Bl)/(Bb-Bl)
  !
  !     Three interpolations type is implemented. Each type uses respectively
  !          MIRROR : computation of the mirror of the ghost
  !          IMAGE1 : one image point with an imposed distance to the interface (1.V_cell**1/3)
  !          IMAGE2 : a secund image  with an imposed distance to the interface (2.V_cell**1/3)
  !     
  !!    INDEX
  !!    -----
  !!
  !     PLOCATI(1) (resp. PVALUEI(1)) is the image 1 location (resp. value) === CL0 ===
  !     PLOCATI(2) (resp. PVALUEI(2)) is the image 2 location (resp. value) === CL1 ===
  !     PLOCATI(3) (resp. PVALUEI(3)) is the mirror  location (resp. value) === CL2 ===
  !     PVALUEI(4)  is the bound   value
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
  !!      Franck Auguste       * CERFACS(AE) *
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    01/01/2019
  !!
  !-------------------------------------------------------------------------------
  !
  !**** 0.    DECLARATIONS
  !     ------------------
  !
  ! module
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1   Declaration of arguments
  REAL,   DIMENSION(:) , INTENT(IN)  :: PLOCATI
  REAL,   DIMENSION(:) , INTENT(IN)  :: PVALUEI
  CHARACTER(LEN=3)     , INTENT(IN)  :: HINTERP
  REAL                               :: PVALUEG
  !
  !------------------------------------------------------------------------------
  !
  !       0.2   Declaration of local variables
  !
  REAL :: Z_PLAG_G0,Z_PLAG_I1,Z_PLAG_I2                      
  REAL :: Z_CINT_G0,Z_CINT_I1,Z_CINT_I2         
  REAL :: Z_CINT_GG,Z_CINT_II
  REAL :: Z_PLAG_GG,Z_PLAG_II
  REAL :: ZVALUEMIN,ZVALUEMAX
  !
  !-------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  !------------------------------------------------------------------------------
  !       
  !**** 2. EXECUTIONS
  !     -------------
  !
  IF (HINTERP=='CL3') THEN
     !
     ! Lagrange polynomials
     Z_PLAG_G0 = (-PLOCATI(3)-PLOCATI(1))/(0.         -PLOCATI(1))*&
          (-PLOCATI(3)-PLOCATI(2))/(0.         -PLOCATI(2))
     Z_PLAG_I1 = (-PLOCATI(3)-0.        )/(+PLOCATI(1)-        0.)*&
          (-PLOCATI(3)-PLOCATI(2))/(+PLOCATI(1)-PLOCATI(2))
     Z_PLAG_I2 = (-PLOCATI(3)-0.        )/(+PLOCATI(2)-        0.)*&
          (-PLOCATI(3)-PLOCATI(1))/(+PLOCATI(2)-PLOCATI(1))
     !
     ! Interpolation coeffs
     Z_CINT_G0 = Z_PLAG_G0
     Z_CINT_I1 = Z_PLAG_I1
     Z_CINT_I2 = Z_PLAG_I2
     !
     ! Mirror value computation
     PVALUEG = Z_CINT_G0*PVALUEI(4)+Z_CINT_I1*PVALUEI(1)+Z_CINT_I2*PVALUEI(2)
     !
  ENDIF
  !
  IF (HINTERP=='CL2') THEN
     !
     ! Lagrange polynomials
     Z_PLAG_G0 = (PLOCATI(3)-PLOCATI(1))/(-PLOCATI(3)-PLOCATI(1))*&
          (PLOCATI(3)-PLOCATI(2))/(-PLOCATI(3)-PLOCATI(2))
     Z_PLAG_I1 = (PLOCATI(3)+PLOCATI(3))/(+PLOCATI(1)+PLOCATI(3))*&
          (PLOCATI(3)-PLOCATI(2))/(+PLOCATI(1)-PLOCATI(2))
     Z_PLAG_I2 = (PLOCATI(3)+PLOCATI(3))/(+PLOCATI(2)+PLOCATI(3))*&
          (PLOCATI(3)-PLOCATI(1))/(+PLOCATI(2)-PLOCATI(1))
     !
     ! Interpolation coeffs
     Z_CINT_G0 = 1./(1.+Z_PLAG_G0)*(2.*Z_PLAG_G0)
     Z_CINT_I1 = 1./(1.+Z_PLAG_G0)*(1.*Z_PLAG_I1)
     Z_CINT_I2 = 1./(1.+Z_PLAG_G0)*(1.*Z_PLAG_I2)
     !
     ! Mirror value computation
     PVALUEG = Z_CINT_G0*PVALUEI(4)+Z_CINT_I1*PVALUEI(1)+Z_CINT_I2*PVALUEI(2)
     !
     ! Value limitation
     ZVALUEMIN = +XIBM_IEPS  
     ZVALUEMAX = -XIBM_IEPS
     ZVALUEMIN = MIN(ZVALUEMIN,PVALUEI(1))
     ZVALUEMIN = MIN(ZVALUEMIN,PVALUEI(2))
     ZVALUEMIN = MIN(ZVALUEMIN,PVALUEI(4))
     ZVALUEMAX = MAX(ZVALUEMAX,PVALUEI(1))
     ZVALUEMAX = MAX(ZVALUEMAX,PVALUEI(2))
     ZVALUEMAX = MAX(ZVALUEMAX,PVALUEI(4))
     PVALUEG = MAX(PVALUEG,ZVALUEMIN)
     PVALUEG = MIN(PVALUEG,ZVALUEMAX)
     !
  ENDIF
  !
  IF (HINTERP=='CL1') THEN
     !
     ! Lagrange polynomials
     Z_PLAG_GG = (PLOCATI(3)-PLOCATI(1))/(-PLOCATI(3)-PLOCATI(1))
     Z_PLAG_II = (PLOCATI(3)+PLOCATI(3))/(+PLOCATI(3)+PLOCATI(1))
     ! 
     ! Interpolation coeffs
     Z_CINT_GG = 1./(1.+Z_PLAG_GG)*(2.*Z_PLAG_GG)
     Z_CINT_II = 1./(1.+Z_PLAG_GG)*(1.*Z_PLAG_II)
     !
     ! Mirror value computation
     PVALUEG = Z_CINT_GG*PVALUEI(4)+Z_CINT_II*PVALUEI(1)
     !
  ENDIF
  !
  IF (HINTERP=='CL0') THEN
     !
     PVALUEG = PVALUEI(3)
     !
  ENDIF
  !
  RETURN
  !
END FUNCTION IBM_1DINT
