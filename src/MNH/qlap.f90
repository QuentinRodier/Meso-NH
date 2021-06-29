!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################
      MODULE MODI_QLAP
!     ################
!
INTERFACE
!
      FUNCTION QLAP(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,PTHETAV,PY)  &
               RESULT(PQLAP)
!  
IMPLICIT NONE
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX    ! x-direction LBC type 
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY    ! y-direction LBC type 
!
                                                 ! Metric coefficients:
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! d*xx 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! d*yy 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZX      ! d*zx 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZY      ! d*zy 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZZ      ! d*zz
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHODJ    ! density of reference * J
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTHETAV   ! virtual potential temp. at time t
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PY        ! field components
!
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2),SIZE(PY,3)) :: PQLAP ! final divergence 
!
END FUNCTION QLAP
!
END INTERFACE
!
END MODULE MODI_QLAP
!
!
!
!     #########################################################################
      FUNCTION QLAP(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,PTHETAV,PY)  &
               RESULT(PQLAP)
!     #########################################################################
!
!!****  *QLAP * - compute the complete quasi-laplacien QLAP of a field P 
!!
!!    PURPOSE
!!    -------
!       This function computes the quasi-laplacien QLAP of the scalar field P
!     localized at a mass point, with non-vanishing orography.
!     The result is localized at a mass point and defined by: 
!                    for Durran and MAE anelastic equations
!                                   (                        ( GX_M_U (PY) )  )                 
!                    PQLAP   = GDIV ( rho * CPd * Thetav * J ( GX_M_V (PY) )  )
!                                   (                        ( GX_M_W (PY) )  )  
!                    or for Lipps and Hemler
!                                   (                        ( GX_M_U (PY) )  )                 
!                    PQLAP   = GDIV ( rho                * J ( GX_M_V (PY) )  )
!                                   (                        ( GX_M_W (PY) )  )  
!     Where GX_M_.. are the cartesian components of the gradient of PY and
!     GDIV is the operator acting on a vector AA: 
!                   
!                   GDIV ( AA ) = J * divergence (1/J  AA  ) 
!     
!!**  METHOD
!!    ------
!!      First, we compute the gradients along x, y , z of the P field. The 
!!    result is multiplied by rhod * CPd * Thetav or  rhod depending on the 
!!    chosen anelastic system where the suffixes indicate 
!!    d dry and v for virtual.
!!      Then, the pseudo-divergence ( J * DIV (1/J o ) ) is computed by the 
!!    subroutine GDIV. The result is localized at a mass point.
!!
!!    EXTERNAL
!!    --------
!!      Function GX_M_U : compute the gradient along x 
!!      Function GY_M_V : compute the gradient along y 
!!      Function GZ_M_W : compute the gradient along z 
!!      FUNCTION MXM: compute an average in the x direction for a variable  
!!      at a mass localization
!!      FUNCTION MYM: compute an average in the y direction for a variable  
!!      at a mass localization
!!      FUNCTION MZM: compute an average in the z direction for a variable  
!!      at a mass localization
!!      Subroutine GDIV : compute J times the divergence of 1/J times a vector
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_PARAMETERS: JPHEXT, JPVEXT
!!      Module MODD_CONF: L2D,CEQNSYS
!!      Module MODD_CST : XCPD
!!
!!    REFERENCE
!!    ---------
!!      Pressure solver documentation ( Scientific documentation )
!!
!!    AUTHOR
!!    ------
!!	P. Hereil and J. Stein       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       11/07/94 
!!      Modification   16/03/95 change the argument list of the gradient
!!                     14/01/97 New anelastic equation ( Stein )
!!                     17/12/97 include the case of non-vanishing orography
!!                              at the lbc ( Stein )
!!                     06/12 V.Masson : update_halo due to CONTRAV changes
!!   J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1  
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  F. Auguste       02/21: add IBM
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CST
USE MODI_GDIV
USE MODI_GRADIENT_M
USE MODI_SHUMAN
!
USE MODE_MPPDB
!
USE MODD_IBM_PARAM_n, ONLY: XIBM_LS, LIBM, XIBM_SU
USE MODI_IBM_BALANCE
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX    ! x-direction LBC type 
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY    ! y-direction LBC type 
!
                                                 ! Metric coefficients:
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDXX      ! d*xx 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDYY      ! d*yy 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZX      ! d*zx 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZY      ! d*zy 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDZZ      ! d*zz
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRHODJ    ! density of reference * J
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTHETAV   ! virtual potential temp. at time t
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PY        ! field components
!
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2),SIZE(PY,3)) :: PQLAP ! final divergence 
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2),SIZE(PY,3)) :: ZU ! rho*J*gradient along x
!
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2),SIZE(PY,3)) :: ZV ! rho*J*gradient along y
!
REAL, DIMENSION(SIZE(PY,1),SIZE(PY,2),SIZE(PY,3)) :: ZW ! rho*J*gradient along z
!
INTEGER                          :: IIU,IJU,IKU         ! I,J,K array sizes
INTEGER                          :: JK,JJ,JI            ! vertical loop index
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
INTEGER :: IINFO_ll
INTEGER :: IIB,IIE,IJB,IJE,IKB,IKE
!-------------------------------------------------------------------------------
!
!
!*       1.    COMPUTE THE GRADIENT COMPONENTS
!              -------------------------------
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKU=SIZE(PY,3)
IKE = IKU - JPVEXT
IKB =   1 + JPVEXT
!
ZU = GX_M_U(1,IKU,1,PY,PDXX,PDZZ,PDZX)
CALL MPPDB_CHECK3D(ZU,'QLAP::ZU',PRECISION)
!
IF ( HLBCX(1) /= 'CYCL' .AND. LWEST_ll() ) THEN
  DO JK=2,IKU-1
    DO JJ=1,IJU
      ZU(IIB,JJ,JK)=  (PY(IIB,JJ,JK) - PY(IIB-1,JJ,JK) - 0.5 * (                  &
       PDZX(IIB,JJ,JK)   * (PY(IIB,JJ,JK)-PY(IIB,JJ,JK-1)) / PDZZ(IIB,JJ,JK)      &
      +PDZX(IIB,JJ,JK+1) * (PY(IIB,JJ,JK+1)-PY(IIB,JJ,JK)) / PDZZ(IIB,JJ,JK+1)    &  
                                      )    ) / PDXX(IIB,JJ,JK)
    END DO
  END DO
END IF
CALL MPPDB_CHECK3D(ZU,'QLAP::ZU/W',PRECISION)
!
IF ( HLBCX(1) /= 'CYCL' .AND. LEAST_ll() ) THEN
  DO JK=2,IKU-1
    DO JJ=1,IJU
      ZU(IIE+1,JJ,JK)=  (PY(IIE+1,JJ,JK) - PY(IIE+1-1,JJ,JK) - 0.5 * (                    &
        PDZX(IIE+1,JJ,JK)   * (PY(IIE+1-1,JJ,JK)-PY(IIE+1-1,JJ,JK-1)) / PDZZ(IIE+1-1,JJ,JK)  &
       +PDZX(IIE+1,JJ,JK+1) * (PY(IIE+1-1,JJ,JK+1)-PY(IIE+1-1,JJ,JK)) / PDZZ(IIE+1-1,JJ,JK+1)&  
                                            ) ) / PDXX(IIE+1,JJ,JK)
    END DO
  END DO
END IF
CALL MPPDB_CHECK3D(ZU,'QLAP::ZU/E',PRECISION)
!
IF(.NOT. L2D) THEN 
!
  ZV = GY_M_V(1,IKU,1,PY,PDYY,PDZZ,PDZY)
  CALL MPPDB_CHECK3D(ZV,'QLAP::ZV',PRECISION)
!
  IF ( HLBCY(1) /= 'CYCL' .AND. LSOUTH_ll() ) THEN 
    DO JK=2,IKU-1
      DO JI=1,IIU
        ZV(JI,IJB,JK)=   (PY(JI,IJB,JK) - PY(JI,IJB-1,JK) - 0.5 * (                  &
          PDZY(JI,IJB,JK)   * (PY(JI,IJB,JK)-PY(JI,IJB,JK-1)) / PDZZ(JI,IJB,JK)      &
         +PDZY(JI,IJB,JK+1) * (PY(JI,IJB,JK+1)-PY(JI,IJB,JK)) / PDZZ(JI,IJB,JK+1)    &  
                                            )   ) / PDYY(JI,IJB,JK) 
      END DO
    END DO
  END IF
  CALL MPPDB_CHECK3D(ZV,'QLAP::ZV/S',PRECISION)
  IF ( HLBCY(1) /= 'CYCL' .AND. LNORTH_ll() ) THEN
!
    DO JK=2,IKU-1
      DO JI=1,IIU
        ZV(JI,IJE+1,JK)=    (PY(JI,IJE+1,JK) - PY(JI,IJE+1-1,JK) - 0.5 * (                  &
          PDZY(JI,IJE+1,JK)   * (PY(JI,IJE+1-1,JK)-PY(JI,IJE+1-1,JK-1)) / PDZZ(JI,IJE+1-1,JK)  &
         +PDZY(JI,IJE+1,JK+1) * (PY(JI,IJE+1-1,JK+1)-PY(JI,IJE+1-1,JK)) / PDZZ(JI,IJE+1-1,JK+1)&
                                                )  ) / PDYY(JI,IJE+1,JK) 
      END DO
    END DO
  END IF
 CALL MPPDB_CHECK3D(ZV,'QLAP::ZV/N',PRECISION)
!
ELSE
  ZV=0.
ENDIF
!
IF ( CEQNSYS == 'DUR' .OR. CEQNSYS == 'MAE' ) THEN
  ZU = MXM(PRHODJ * XCPD * PTHETAV) *  ZU
  IF(.NOT. L2D) THEN 
    ZV = MYM(PRHODJ * XCPD * PTHETAV) *  ZV
  END IF
  ZW = MZM(PRHODJ * XCPD * PTHETAV) *  GZ_M_W(1,IKU,1,PY,PDZZ)
ELSEIF ( CEQNSYS == 'LHE' ) THEN 
  ZU = MXM(PRHODJ) * ZU
  IF(.NOT. L2D) THEN 
    ZV = MYM(PRHODJ) * ZV
  ENDIF
  ZW = MZM(PRHODJ) * GZ_M_W(1,IKU,1,PY,PDZZ)
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTE THE DIVERGENCE  
!              ----------------------
!
NULLIFY(TZFIELDS_ll)
CALL ADD3DFIELD_ll( TZFIELDS_ll, ZU, 'QLAP::ZU' )
CALL ADD3DFIELD_ll( TZFIELDS_ll, ZV, 'QLAP::ZV' )
CALL ADD3DFIELD_ll( TZFIELDS_ll, ZW, 'QLAP::ZW' )
CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
CALL CLEANLIST_ll(TZFIELDS_ll)
!
CALL GDIV(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,ZU,ZV,ZW,PQLAP)
!
IF (LIBM)  THEN
   !
   CALL IBM_BALANCE(XIBM_LS,XIBM_SU,ZU,ZV,ZW,PQLAP)
   !
   PQLAP(:,:,IKB-1) = PQLAP(:,:,IKB-1)*XIBM_SU(:,:,IKB,1)
   PQLAP(:,:,IKE+1) = PQLAP(:,:,IKE+1)*XIBM_SU(:,:,IKE,1)
   !
   IF ( HLBCX(1) /= 'CYCL' ) THEN
      IF(LWEST_ll()) PQLAP(IIB-1,:,:) = PQLAP(IIB-1,:,:)*XIBM_SU(IIB,:,:,1)
      IF(LEAST_ll()) PQLAP(IIE+1,:,:) = PQLAP(IIE+1,:,:)*XIBM_SU(IIE,:,:,1)
   ENDIF
   !
   IF ( HLBCY(1) /= 'CYCL' ) THEN
      IF (LSOUTH_ll()) PQLAP(:,IJB-1,:) = PQLAP(:,IJB-1,:)*XIBM_SU(:,IJB,:,1)
      IF (LNORTH_ll()) PQLAP(:,IJE+1,:) = PQLAP(:,IJE+1,:)*XIBM_SU(:,IJE,:,1)
   ENDIF
   !
ENDIF
!
!-------------------------------------------------------------------------------
!
END FUNCTION QLAP
