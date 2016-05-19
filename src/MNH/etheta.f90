!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 turb 2006/05/18 13:07:25
!-----------------------------------------------------------------
!#################
MODULE MODI_ETHETA
!#################
!
INTERFACE
!
FUNCTION ETHETA(KRR,KRRI,PTHLM,PRM,PLOCPEXNM,PATHETA,PSRCM) RESULT(PETHETA)
!
INTEGER                              :: KRR          ! number of moist var.
INTEGER                              :: KRRI         ! number of ice var.
!
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PTHLM     ! Conservative pot. temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::   PRM       ! Mixing ratios, where
!                                      PRM(:,:,:,1) = conservative mixing ratio
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PLOCPEXNM ! Lv(T)/Cp/Exner at time t-1
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PATHETA   ! Atheta
!                                                    
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PSRCM     ! Normalized 2dn_order
                                                    ! moment s'r'c/2Sigma_s2
!
REAL,DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)):: PETHETA ! result
!
!
END FUNCTION ETHETA
!
END INTERFACE
!
END MODULE MODI_ETHETA
!
!   ############################################################################
FUNCTION ETHETA(KRR,KRRI,PTHLM,PRM,PLOCPEXNM,PATHETA,PSRCM) RESULT(PETHETA)
!   ############################################################################
!
!      PURPOSE
!!     -------
!      ETHETA computes the coefficient Etheta in the flottability turbulent
!      flux. This coefficient relates the vertical flux of the virtual potential
!      temperature ( <Thv' W'> ) to the vertical flux of the conservative potential
!      temperature ( <Thl' W'> ).
!
!!**   METHOD
!!     ------
!!
!!     The virtual potential temperature perturbation is linearized in function
!!     of Thl' and Rnp'. The result is
!!        Thv'= ( ZA + ZC * Atheta * 2 * SRC ) Thl' 
!!             +( ZB + ZC * Amoist * 2 * SRC ) Rnp'
!!     From this relation, we can compute the vertical turbulent fluxes.
!!
!!     EXTERNAL
!!     --------
!!
!!        NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!       Module MODD_CST : contains physical constants.
!!         XRV, XRD  : R for water vapor and dry air
!!            
!!     REFERENCE
!!     ---------
!!
!!
!!     AUTHOR
!!     ------
!!       Jean-Marie Carriere      * Meteo-France *
!!
!!     MODIFICATIONS
!!     -------------
!!       Original       20/03/95
!!     
!!       J. Stein       Feb  28, 1996   optimization + Doctorization
!!       J. Stein       Sept 15, 1996   Atheta previously computed
!!       J.-P. Pinty    May  20, 2003   Improve ETHETA expression
!!
!! ----------------------------------------------------------------------
!
!*       0. DECLARATIONS
!           ------------
USE MODD_CST
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments and result
!
!
INTEGER                              :: KRR          ! number of moist var.
INTEGER                              :: KRRI         ! number of ice var.
!
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PTHLM     ! Conservative pot. temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::   PRM       ! Mixing ratios, where
!                                      PRM(:,:,:,1) = conservative mixing ratio
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PLOCPEXNM ! Lv(T)/Cp/Exner at time t-1
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PATHETA   ! Atheta
!                                                    
REAL, DIMENSION(:,:,:),  INTENT(IN)  ::   PSRCM     ! Normalized 2dn_order
                                                    ! moment s'r'c/2Sigma_s2
!
REAL,DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)):: PETHETA ! result
!
!
!
!*       0.2 declarations of local variables
!
REAL,DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)) ::       &
                                        ZA, ZRW
!                ZA = coeft A, ZRW = total mixing ratio rw
REAL                                  :: ZDELTA  ! = Rv/Rd - 1
INTEGER                               :: JRR     ! moist loop counter
!
!---------------------------------------------------------------------------
!
!
!*       1. COMPUTE ETHETA
!           --------------
!
!
IF ( KRR == 0 ) THEN                                ! dry case
  PETHETA(:,:,:) = 1.
ELSE IF ( KRR == 1 ) THEN                           ! only vapor
  ZDELTA = (XRV/XRD) - 1.
  PETHETA(:,:,:) = 1. + ZDELTA*PRM(:,:,:,1)
ELSE                                                ! liquid water & ice present
  ZDELTA = (XRV/XRD) - 1.
  ZRW(:,:,:) = PRM(:,:,:,1)
!
  IF ( KRRI>0 ) THEN  ! rc and ri case
    ZRW(:,:,:) = ZRW(:,:,:) + PRM(:,:,:,3)
    DO JRR=5,KRR
      ZRW(:,:,:) = ZRW(:,:,:) + PRM(:,:,:,JRR)
    ENDDO
    ZA(:,:,:) = 1. + (                                    &  ! Compute A
              (1.+ZDELTA) * (PRM(:,:,:,1) - PRM(:,:,:,2) - PRM(:,:,:,4)) &
              -ZRW(:,:,:)                                                &
                     )  /  (1. + ZRW(:,:,:))
  !
  !   Etheta = ZA + ZC * Atheta  
  !   ZC is computed from line 2 to line 5
  !   - Atheta * 2. * SRC is computed at line 6 
  !
    PETHETA(:,:,:) = ZA(:,:,:)                                                 &
        +( PLOCPEXNM(:,:,:) * ZA(:,:,:)                                        &
               -(1.+ZDELTA) * (PTHLM(:,:,:) + PLOCPEXNM(:,:,:)*(               &
                                                    PRM(:,:,:,2)+PRM(:,:,:,4)))&
                            / (1. + ZRW(:,:,:))                                &
         ) * PATHETA(:,:,:) * 2. * PSRCM(:,:,:)
  ELSE
    DO JRR=3,KRR
      ZRW(:,:,:) = ZRW(:,:,:) + PRM(:,:,:,JRR)
    ENDDO
    ZA(:,:,:) = 1. + (                                    &  ! Compute A
              (1.+ZDELTA) * (PRM(:,:,:,1) - PRM(:,:,:,2)) &
              -ZRW(:,:,:)                                 &
                     )  /  (1. + ZRW(:,:,:))
  !
  !   Etheta = ZA + ZC * Atheta  
  !   ZC is computed from line 2 to line 5
  !   - Atheta * 2. * SRC is computed at line 6 
  !
    PETHETA(:,:,:) = ZA(:,:,:)                                                 &
        +( PLOCPEXNM(:,:,:) * ZA(:,:,:)                                        &
               -(1.+ZDELTA) * (PTHLM(:,:,:) + PLOCPEXNM(:,:,:)*PRM(:,:,:,2))   &
                            / (1. + ZRW(:,:,:))                                &
         ) * PATHETA(:,:,:) * 2. * PSRCM(:,:,:)
  END IF
END IF
!
!---------------------------------------------------------------------------
!
END FUNCTION ETHETA
