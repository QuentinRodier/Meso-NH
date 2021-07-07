!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!##################
 MODULE MODI_BUDGET_FLAGS
!##################
!
INTERFACE
!
SUBROUTINE BUDGET_FLAGS(OUSERV, OUSERC, OUSERR,         &
                        OUSERI, OUSERS, OUSERG, OUSERH  )
!
!
LOGICAL, INTENT(IN) :: OUSERV    ! flag to use water vapor
LOGICAL, INTENT(IN) :: OUSERC    ! flag to use cloud
LOGICAL, INTENT(IN) :: OUSERR    ! flag to use rain
LOGICAL, INTENT(IN) :: OUSERI    ! flag to use ice
LOGICAL, INTENT(IN) :: OUSERS    ! flag to use snow
LOGICAL, INTENT(IN) :: OUSERG    ! flag to use graupel
LOGICAL, INTENT(IN) :: OUSERH    ! flag to use hail
!
END SUBROUTINE BUDGET_FLAGS
!
END INTERFACE
!
END MODULE MODI_BUDGET_FLAGS
!     ###################################################
SUBROUTINE BUDGET_FLAGS(OUSERV, OUSERC, OUSERR,         &
                        OUSERI, OUSERS, OUSERG, OUSERH  )
!     ###################################################
!
!!****  *BUDGET_FLAGS* - routine to initialize the flags to call the budget
!!                       routines
!!                           
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!       
!!     
!!
!!    EXTERNAL
!!    --------
!!      CART_COMPRESS 
!!      MASK_COMPRESS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      None
!!
!!    AUTHOR
!!    ------
!!  	V. Masson        * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/10/02
!  P. Wautelet 06/03/2020: add condition on NRR for hydrometeor variables and cturb for TKE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_CONF,    ONLY: LCHECK
use modd_conf_n,  only: nrr
USE MODD_LES,     only: LLES_CALL
USE MODD_NSV,     ONLY: NSV
use modd_param_n, only: cturb
!
IMPLICIT NONE
!  
!  
!*       0.1   Declarations of arguments :
!
LOGICAL, INTENT(IN) :: OUSERV    ! flag to use water vapor
LOGICAL, INTENT(IN) :: OUSERC    ! flag to use cloud
LOGICAL, INTENT(IN) :: OUSERR    ! flag to use rain
LOGICAL, INTENT(IN) :: OUSERI    ! flag to use ice
LOGICAL, INTENT(IN) :: OUSERS    ! flag to use snow
LOGICAL, INTENT(IN) :: OUSERG    ! flag to use graupel
LOGICAL, INTENT(IN) :: OUSERH    ! flag to use hail

!*       0.2   Declarations of local variables :
!
!-------------------------------------------------------------------------------
!
LBUDGET_U  =   (LBU_ENABLE .AND. LBU_RU  ) .OR.  (LLES_CALL .OR. LCHECK )
LBUDGET_V  =   (LBU_ENABLE .AND. LBU_RV  ) .OR.  (LLES_CALL .OR. LCHECK )
LBUDGET_W  =   (LBU_ENABLE .AND. LBU_RW  ) .OR.  (LLES_CALL .OR. LCHECK )
LBUDGET_TH =   (LBU_ENABLE .AND. LBU_RTH ) .OR.  (LLES_CALL .OR. LCHECK )
LBUDGET_TKE= ( (LBU_ENABLE .AND. LBU_RTKE) .OR.  (LLES_CALL .OR. LCHECK )              ) .AND. CTURB == 'TKEL'
LBUDGET_RV = ( (LBU_ENABLE .AND. LBU_RRV ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERV) ) .AND. NRR >= 1
LBUDGET_RC = ( (LBU_ENABLE .AND. LBU_RRC ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERC) ) .AND. NRR >= 2
LBUDGET_RR = ( (LBU_ENABLE .AND. LBU_RRR ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERR) ) .AND. NRR >= 3
LBUDGET_RI = ( (LBU_ENABLE .AND. LBU_RRI ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERI) ) .AND. NRR >= 4
LBUDGET_RS = ( (LBU_ENABLE .AND. LBU_RRS ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERS) ) .AND. NRR >= 5
LBUDGET_RG = ( (LBU_ENABLE .AND. LBU_RRG ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERG) ) .AND. NRR >= 6
LBUDGET_RH = ( (LBU_ENABLE .AND. LBU_RRH ) .OR. ((LLES_CALL .OR. LCHECK ).AND. OUSERH) ) .AND. NRR >= 7
LBUDGET_SV =   (LBU_ENABLE .AND. LBU_RSV ) .OR. ((LLES_CALL .OR. LCHECK ).AND. NSV>0 )
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE BUDGET_FLAGS
