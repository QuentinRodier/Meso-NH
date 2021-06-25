!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!     #####################
MODULE MODI_IBM_0DINT
  !     #####################
  !
  INTERFACE 
     !
     FUNCTION IBM_0DINT(PDELTAI,PVALUEI,HBOUND1,HBOUND2,PBOUNDI,PIBM_VISC,PIBM_DIVK) RESULT(PVALUEB)
       !
       REAL                 , INTENT(IN) :: PDELTAI
       REAL                 , INTENT(IN) :: PBOUNDI
       REAL,   DIMENSION(:) , INTENT(IN) :: PVALUEI
       CHARACTER(LEN=3)     , INTENT(IN) :: HBOUND1
       CHARACTER(LEN=3)     , INTENT(IN) :: HBOUND2
       REAL                 , INTENT(IN) :: PIBM_VISC
       REAL                 , INTENT(IN) :: PIBM_DIVK
       REAL                              :: PVALUEB
       !
     END FUNCTION IBM_0DINT
     !
  END INTERFACE
  !
END MODULE MODI_IBM_0DINT
!
!     ###########################################################################
FUNCTION IBM_0DINT(PDELTAI,PVALUEI,HBOUND1,HBOUND2,PBOUNDI,PIBM_VISC,PIBM_DIVK) RESULT(PVALUEB)
  !     ###########################################################################
  !
  !****  *IBM_INTER_0DINT* - Computation of the variable value at the interface
  !
  !    PURPOSE
  !    -------
  !     Depending on the boundary condition type (Dirichlet, Neumann, Robin)
  !     the variable value PVALUEB is affected using the values at images point
  !     PVALUEI.
  !
  !
  !    METHOD
  !    ------
  !     
  !    INDEX
  !    -----
  !
  !    IMPLICIT ARGUMENTS
  !    ------------------
  !
  !    REFERENCE
  !    ---------
  !
  !    AUTHOR
  !    ------
  !	
  !      Franck Auguste       * CERFACS(AE) *
  !
  !    MODIFICATIONS
  !    -------------
  !      Original    01/01/2019
  !
  !-------------------------------------------------------------------------------
  !
  !**** 0.    DECLARATIONS
  !     ------------------
  !
  ! module
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  USE MODD_CST
  USE MODD_CTURB
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1   Declaration of arguments
  REAL                 , INTENT(IN) :: PDELTAI
  REAL                 , INTENT(IN) :: PBOUNDI
  REAL,   DIMENSION(:) , INTENT(IN) :: PVALUEI
  CHARACTER(LEN=3)     , INTENT(IN) :: HBOUND1
  CHARACTER(LEN=3)     , INTENT(IN) :: HBOUND2 
  REAL                 , INTENT(IN) :: PIBM_VISC
  REAL                 , INTENT(IN) :: PIBM_DIVK
  REAL                              :: PVALUEB
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2   Declaration of local variables
  !
  REAL :: Z_ROB, Z_PVAL0, Z_DVAL0
  REAL :: Z_VD, Z_RD, Z_RE, Z_RD1, Z_RD2
  !
  !-----------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  !-----------------------------------------------------------------------------
  !       
  !**** 2. EXECUTIONS
  !     -------------
  !
  !
  ! Switch for Neuman,Dirichlet or Robin conditions
  !
  Z_ROB = 0.
  !
  IF (HBOUND1=='DIR') THEN
     Z_ROB = 1.0
  ENDIF
  IF (HBOUND1=='NEU') THEN
     Z_ROB = 0.0
  ENDIF
  IF (HBOUND1=='ROB') THEN
     Z_ROB = 0.5
  ENDIF
  !
  ! Computation of value at the interface
  !
  Z_PVAL0 = 0.
  Z_DVAL0 = 0.
  !
  IF (HBOUND2=='CST') THEN
     Z_PVAL0 = PBOUNDI
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='CN3'.OR.HBOUND2=='CK3') THEN
     Z_PVAL0 = PVALUEI(3)
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='CN1'.OR.HBOUND2=='CK1') THEN
     Z_PVAL0 = PVALUEI(1)
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='CN2'.OR.HBOUND2=='CK2') THEN
     Z_PVAL0 = PVALUEI(2)
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='LN3'.OR.HBOUND2=='LK3') THEN
     Z_PVAL0 = (2.*PVALUEI(3)-1.*PVALUEI(1))
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='LN1'.OR.HBOUND2=='LK1') THEN
     Z_PVAL0 = (2.*PVALUEI(1)-1.*PVALUEI(2))
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='LN2'.OR.HBOUND2=='LK2') THEN
     Z_PVAL0 = (1./4.)*(9.*PVALUEI(3)-6.*PVALUEI(1)+1.*PVALUEI(2))
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='WN3'.OR.HBOUND2=='WK3') THEN
     Z_VD   = PVALUEI(3)
     Z_RE = XIBM_EPSI  
     IF (ABS(Z_VD).GT.XIBM_EPSI) Z_RE = Z_VD/ABS(Z_VD)
     Z_RD1 = 1./XIBM_RUG/9.8
     Z_RD2 = PIBM_VISC/XIBM_VISC
     Z_RD = MIN(Z_RD1,Z_RD2)
     Z_RD = MAX(XIBM_EPSI,Z_RD)
     Z_PVAL0 = Z_RE/XKARMAN*PIBM_VISC*log(1.+9.8*PDELTAI*XKARMAN*Z_RD*(1.-exp(-PDELTAI*XKARMAN*Z_RD/20.)))
     IF (Z_PVAL0*Z_VD.GT.Z_VD**2.)  Z_PVAL0=Z_VD
     IF (Z_PVAL0*Z_VD.LT.XIBM_EPSI**2.)  Z_PVAL0=XIBM_EPSI
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='WN1'.OR.HBOUND2=='WK1') THEN
     Z_VD   = PVALUEI(1)
     Z_RE = XIBM_EPSI  
     IF (ABS(Z_VD).GT.XIBM_EPSI) Z_RE = Z_VD/ABS(Z_VD)
     Z_RD1 = 1./XIBM_RUG/9.8
     Z_RD2 = PIBM_VISC/XIBM_VISC
     Z_RD = MIN(Z_RD1,Z_RD2)
     Z_RD = MAX(XIBM_EPSI,Z_RD)
     Z_PVAL0 = Z_RE/XKARMAN*PIBM_VISC*log(1.+9.8*PDELTAI*XKARMAN*Z_RD*(1.-exp(-PDELTAI*XKARMAN*Z_RD/20.)))
     IF (Z_PVAL0*Z_VD.GT.Z_VD**2.)  Z_PVAL0=Z_VD
     IF (Z_PVAL0*Z_VD.LT.XIBM_EPSI**2.)  Z_PVAL0=XIBM_EPSI
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='WN2'.OR.HBOUND2=='WK2') THEN
     Z_VD   = PVALUEI(2)
     Z_RE = XIBM_EPSI  
     IF (ABS(Z_VD).GT.XIBM_EPSI) Z_RE = Z_VD/ABS(Z_VD)
     Z_RD1 = 1./XIBM_RUG/9.8
     Z_RD2 = PIBM_VISC/XIBM_VISC
     Z_RD = MIN(Z_RD1,Z_RD2)
     Z_RD = MAX(XIBM_EPSI,Z_RD)
     Z_PVAL0 = Z_RE/XKARMAN*PIBM_VISC*log(1.+9.8*PDELTAI*XKARMAN*Z_RD*(1.-exp(-PDELTAI*XKARMAN*Z_RD/20.)))
     IF (Z_PVAL0*Z_VD.GT.Z_VD**2.)  Z_PVAL0=Z_VD
     IF (Z_PVAL0*Z_VD.LT.XIBM_EPSI**2.)  Z_PVAL0=XIBM_EPSI
     Z_DVAL0 = 0.
  ENDIF
  !
  IF (HBOUND2=='CK3'.OR.HBOUND2=='CK1'.OR.HBOUND2=='CK2'.OR.&
       HBOUND2=='LK3'.OR.HBOUND2=='LK1'.OR.HBOUND2=='LK2'.OR.&
       HBOUND2=='WK3'.OR.HBOUND2=='WK1'.OR.HBOUND2=='WK2') THEN
     Z_VD    = Z_PVAL0 
     Z_PVAL0 = Z_PVAL0*(1.-PIBM_DIVK)
     IF (Z_PVAL0*Z_VD.LT.-XIBM_EPSI**2.) Z_PVAL0=XIBM_EPSI
     IF (Z_PVAL0*Z_VD.GT.Z_VD**2.) Z_PVAL0=Z_VD
  ENDIF
  !
  PVALUEB = Z_ROB*Z_PVAL0+(1.-Z_ROB)*(-PDELTAI*Z_DVAL0/2.+PVALUEI(3))
  !
  RETURN
  !
END FUNCTION IBM_0DINT
