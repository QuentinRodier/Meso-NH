!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
MODULE MODE_TRIDIAG
IMPLICIT NONE
CONTAINS
SUBROUTINE TRIDIAG(D,PVARM,PA,PTSTEP,PEXPL,PIMPL, &
                                  PRHODJ,PSOURCE,PVARP )
       USE PARKIND1, ONLY : JPRB
       USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!      #################################################
!
!
!!****   *TRIDIAG* - routine to solve a time implicit scheme
!!
!!
!!     PURPOSE
!!     -------
!        The purpose of this routine is to give a field PVARP at t+1, by 
!      solving an implicit tridiagonal system obtained by the 
!      discretization of the vertical turbulent diffusion. It should be noted 
!      that the degree of implicitness can be varied (PIMPL parameter) and the
!      sources of evolution other than the turbulent diffusion can be taken
!      into account through the PSOURCE field. PVARP is localized at a mass 
!      point.
!
!!**   METHOD
!!     ------
!!        First, the Right Hand Side of the implicit equation is computed.
!!     It is build as follows:
!!        ZY = PVARM + PTSTEP*PSOURCE + DIFF_EXPLI
!!     where PVARM is the variable at t-dt, PSOURCE the supplementary sources of
!!     PVAR ( and not PVAR * PRHODJ !!) and  DIFF_EXPLI is the explicit part
!!     of the vertical turbulent diffusion. This operator is spatially 
!!     discretized as the implicit one, thus:
!!        DIFF_EXPLI(k) = - PEXPL / PRHODJ(k) * 
!!                       ( PA(k+1) * (PVARM(k+1) - PVARM(k)  )
!!                        -PA(k)   * (PVARM(k)   - PVARM(k-1)) )
!!     For the first level, only the upper part is considered, the lower one 
!!     is replaced by the turbulent surface flux (taken into account in the 
!!     PSOURCE(ikb) term).
!!        DIFF_EXPLI(ikb) = - PEXPL / PRHODJ(ikb) * 
!!                       ( PA(ikb+1) * (PVARM(ikb+1) - PVARM(ikb))  )
!!     For the last level, only the lower part is considered, the upper one 
!!     is replaced by the turbulent flux which is taken equal to 0 
!!     (taken into account in the PSOURCE(ike) term).
!!
!!        DIFF_EXPLI(ike) = + PEXPL / PRHODJ(ike) * 
!!                       ( PA(ike) * (PVARM(ike) - PVARM(ike-1))  )
!!                      
!!        Then, the classical tridiagonal algorithm is used to invert the 
!!     implicit operator. Its matrix is given by:
!!
!!     ( b(ikb)   c(ikb)      0        0        0         0        0        0  )
!!     (   0      a(ikb+1) b(ikb+1) c(ikb+1)    0  ...    0        0        0  ) 
!!     (   0         0     a(ikb+2) b(ikb+2) c(ikb+2).    0        0        0  ) 
!!      .......................................................................
!!     (   0   ...   0     a(k)     b(k)     c(k)         0   ...  0        0  ) 
!!      .......................................................................
!!     (   0         0        0        0        0 ...a(ike-1) b(ike-1) c(ike-1))
!!     (   0         0        0        0        0 ...     0   a(ike)   b(ike)  )
!!
!!     ikb and ike represent the first and the last inner mass levels of the
!!     model. The coefficients are:
!!         
!!          a(k) = PIMPL * PA(k)/PRHODJ(k)   
!!          b(k) = 1 - PIMPL * PA(k)/PRHODJ(k) - PIMPL * PA(k+1)/PRHODJ(k)
!!          c(k) = PIMPL * PA(k+1)/PRHODJ(k)
!!
!!          for all k /= ikb or ike
!!
!!          b(ikb) = 1 - PIMPL * PA(ikb+1)/PRHODJ(ikb)
!!          c(ikb) = PIMPL * PA(ikb+1)/PRHODJ(ikb)
!!             (discretization of the upper part of the implicit operator)
!!          b(ike) = 1 - PIMPL * PA(ike)/PRHODJ(ike)
!!          a(ike) = PIMPL * PA(ike)/PRHODJ(ike)
!!             (discretization of the lower part of the implicit operator)
!!       Finally, the marginal points are prescribed.
!!
!!       All these computations are purely vertical and vectorizations are 
!!     easely achieved by processing all the verticals in parallel.
!!
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!       MODD_PARAMETERS
!!            JPVEXT_TURB: number of vertical external points
!!
!!     REFERENCE
!!     ---------
!!       Book 1 of Meso-NH documentation (chapter Turbulence)
!!       Press et al: Numerical recipes (1986) Cambridge Univ. Press
!!
!!     AUTHOR
!!     ------
!!       Joan Cuxart       * INM and Meteo-France *   
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        August 29, 1994
!!       Modification :  January 29, 1995 Algorithm written with two 
!!                                        local variables less
!!      (Cuxart, Stein)  August 21, 1995  Bug correction for PRHODJ
!!      (Stein)         November 16, 1995 new version
!!      (Stein)         February 28, 1995 no inversion in the explicit case
!!      (Seity)         February 2012 add possibility to run with reversed 
!!                            vertical levels
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
USE MODD_PARAMETERS, ONLY: JPVEXT_TURB
USE MODD_DIMPHYEX,   ONLY: DIMPHYEX_t
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
TYPE(DIMPHYEX_t),          INTENT(IN)   :: D
REAL, DIMENSION(D%NIJT,D%NKT),    INTENT(IN)  :: PVARM       ! variable at t-1  
REAL, DIMENSION(D%NIJT,D%NKT),    INTENT(IN)  :: PA          ! upper diag. elements
REAL,                      INTENT(IN)  :: PTSTEP      ! Double time step
REAL,                      INTENT(IN)  :: PEXPL,PIMPL ! weights of the temporal scheme
REAL, DIMENSION(D%NIJT,D%NKT),    INTENT(IN)  :: PRHODJ      ! (dry rho)*J
REAL, DIMENSION(D%NIJT,D%NKT),    INTENT(IN)  :: PSOURCE     ! source term of PVAR    
!
REAL, DIMENSION(D%NIJT,D%NKT),    INTENT(OUT) :: PVARP       ! variable at t+1        
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(D%NIJT,D%NKT)  :: ZY ,ZGAM 
                                         ! RHS of the equation, 3D work array
REAL, DIMENSION(D%NIJT)         :: ZBET
                                         ! 2D work array
INTEGER                              :: JIJ,JK            ! loop counter
INTEGER                              :: IKB,IKE       ! inner vertical limits
INTEGER                              :: IKT,IKA,IKU,IKL! array size in k direction
INTEGER                              :: IKTB,IKTE     ! start, end of k loops in physical domain
INTEGER                              :: IIJB, IIJE    ! start, end of ij loops in physical domain

!
! ---------------------------------------------------------------------------
!                                              
!*      1.  COMPUTE THE RIGHT HAND SIDE
!           ---------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('TRIDIAG',0,ZHOOK_HANDLE)
!
IKT=D%NKT
IKTB=D%NKTB
IKTE=D%NKTE
IKB=D%NKB
IKE=D%NKE
IKA=D%NKA
IKU=D%NKU
IKL=D%NKL
IIJB=D%NIJB
IIJE=D%NIJE
!
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZY(:,IKB) = PVARM(:,IKB)  + PTSTEP*PSOURCE(:,IKB) -   &
  PEXPL / PRHODJ(:,IKB) * PA(:,IKB+IKL) * &
  (PVARM(:,IKB+IKL) - PVARM(:,IKB))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!
DO JK=IKTB+1,IKTE-1
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZY(:,JK)= PVARM(:,JK)  + PTSTEP*PSOURCE(:,JK) -               &
      PEXPL / PRHODJ(:,JK) *                                           &
                             ( PVARM(:,JK-IKL)*PA(:,JK)                &
                              -PVARM(:,JK)*(PA(:,JK)+PA(:,JK+IKL))   &
                              +PVARM(:,JK+IKL)*PA(:,JK+IKL)              &
                             ) 
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
END DO
! 
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZY(:,IKE)= PVARM(:,IKE) + PTSTEP*PSOURCE(:,IKE) +               &
  PEXPL / PRHODJ(:,IKE) * PA(:,IKE) * (PVARM(:,IKE)-PVARM(:,IKE-IKL))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!
!
!*       2.  INVERSION OF THE TRIDIAGONAL SYSTEM
!            -----------------------------------
!
IF ( PIMPL > 1.E-10 ) THEN
!
  !
  !  going up
  !
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZBET(:) = 1. - PIMPL * PA(:,IKB+IKL) / PRHODJ(:,IKB)  ! bet = b(ikb)
  PVARP(:,IKB) = ZY(:,IKB) / ZBET(:)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)               
  !
  DO JK = IKB+IKL,IKE-IKL,IKL
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    ZGAM(:,JK) = PIMPL * PA(:,JK) / PRHODJ(:,JK-IKL) / ZBET(:)  
                                                    ! gam(k) = c(k-1) / bet
    ZBET(:)    = 1. - PIMPL * (  PA(:,JK) * (1. + ZGAM(:,JK))  &
                                 + PA(:,JK+IKL)                      &
                                ) / PRHODJ(:,JK)  
                                                    ! bet = b(k) - a(k)* gam(k)  
    PVARP(:,JK)= ( ZY(:,JK) - PIMPL * PA(:,JK) / PRHODJ(:,JK) &
                    * PVARP(:,JK-IKL)                                 &
                   ) / ZBET(:)
                                        ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END DO
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ! special treatment for the last level
  ZGAM(:,IKE) = PIMPL * PA(:,IKE) / PRHODJ(:,IKE-IKL) / ZBET(:) 
                                                    ! gam(k) = c(k-1) / bet
  ZBET(:)    = 1. - PIMPL * (  PA(:,IKE) * (1. + ZGAM(:,IKE))  &
                              ) / PRHODJ(:,IKE)  
                                                    ! bet = b(k) - a(k)* gam(k)  
  PVARP(:,IKE)= ( ZY(:,IKE) - PIMPL * PA(:,IKE) / PRHODJ(:,IKE) &
                                * PVARP(:,IKE-IKL)                      &
                 ) / ZBET(:)
                                       ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
  !
  !  going down
  !
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  DO JK = IKE-IKL,IKB,-1*IKL
  !$mnh_expand_array(JIJ=IIJB:IIJE)
    PVARP(:,JK) = PVARP(:,JK) - ZGAM(:,JK+IKL) * PVARP(:,JK+IKL) 
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END DO
!
ELSE
! 
  DO JK=IKTB,IKTE
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    PVARP(:,JK) = ZY(:,JK)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END DO
!
END IF 
!
!
!*       3.  FILL THE UPPER AND LOWER EXTERNAL VALUES
!            ----------------------------------------
!
!$mnh_expand_array(JIJ=IIJB:IIJE)
PVARP(:,IKA)=PVARP(:,IKB)
PVARP(:,IKU)=PVARP(:,IKE)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG',1,ZHOOK_HANDLE)
END SUBROUTINE TRIDIAG
END MODULE MODE_TRIDIAG
