!MNH_LIC Copyright 2013-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!      #################################
       MODULE MODI_LIMA_RAIN_FREEZING
!      #################################
!
INTERFACE
   SUBROUTINE LIMA_RAIN_FREEZING (LDCOMPUTE,                                             &
                                  PRHODREF, PT, PLVFACT, PLSFACT,                        &
                                  PRRT, PCRT, PRIT, PCIT, PLBDR,                         &
                                  P_TH_CFRZ, P_RR_CFRZ, P_CR_CFRZ, P_RI_CFRZ, P_CI_CFRZ, &
                                  PA_TH, PA_RR, PA_CR, PA_RI, PA_CI, PA_RG               )
!
LOGICAL, DIMENSION(:),INTENT(IN)    :: LDCOMPUTE
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRHODREF ! Reference Exner function
REAL, DIMENSION(:),   INTENT(IN)    :: PT       !
REAL, DIMENSION(:),   INTENT(IN)    :: PLVFACT  !
REAL, DIMENSION(:),   INTENT(IN)    :: PLSFACT  !
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRRT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PCRT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PRIT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PCIT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDR   ! 
!
REAL, DIMENSION(:),   INTENT(INOUT) :: P_TH_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_RR_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_CR_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_RI_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_CI_CFRZ
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_TH
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RR
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_CR
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RI
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_CI
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RG
!
END SUBROUTINE LIMA_RAIN_FREEZING
END INTERFACE
END MODULE MODI_LIMA_RAIN_FREEZING
!
!     #######################################################################################
      SUBROUTINE LIMA_RAIN_FREEZING (LDCOMPUTE,                                             &
                                     PRHODREF, PT, PLVFACT, PLSFACT,                        &
                                     PRRT, PCRT, PRIT, PCIT, PLBDR,                         &
                                     P_TH_CFRZ, P_RR_CFRZ, P_CR_CFRZ, P_RI_CFRZ, P_CI_CFRZ, &
                                     PA_TH, PA_RR, PA_CR, PA_RI, PA_CI, PA_RG               )
!     #######################################################################################
!
!!    PURPOSE
!!    -------
!!      Compute the rain freezing by contact with an ice crystal
!!
!!
!!    AUTHOR
!!    ------
!!      J.-M. Cohard     * Laboratoire d'Aerologie*
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      S.    Berthet    * Laboratoire d'Aerologie*
!!      B.    Vié        * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             15/03/2018
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST,              ONLY : XTT
USE MODD_PARAM_LIMA,       ONLY : XRTMIN, XCEXVT
USE MODD_PARAM_LIMA_MIXED, ONLY : XICFRR, XEXICFRR, XRCFRI, XEXRCFRI
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL, DIMENSION(:),INTENT(IN)    :: LDCOMPUTE
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRHODREF ! Reference Exner function
REAL, DIMENSION(:),   INTENT(IN)    :: PT       !
REAL, DIMENSION(:),   INTENT(IN)    :: PLVFACT  !
REAL, DIMENSION(:),   INTENT(IN)    :: PLSFACT  !
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRRT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PCRT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PRIT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PCIT    ! 
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDR   ! 
!
REAL, DIMENSION(:),   INTENT(INOUT) :: P_TH_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_RR_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_CR_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_RI_CFRZ
REAL, DIMENSION(:),   INTENT(INOUT) :: P_CI_CFRZ
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_TH
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RR
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_CR
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RI
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_CI
REAL, DIMENSION(:),   INTENT(INOUT) :: PA_RG
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PRRT)) :: ZW1, ZW2 ! work arrays
!
!-------------------------------------------------------------------------------
!
!
!*       1.     PRELIMINARY COMPUTATIONS
!	        ------------------------
!
!
P_TH_CFRZ(:)=0.
P_RR_CFRZ(:)=0.
P_CR_CFRZ(:)=0.
P_RI_CFRZ(:)=0.
P_CI_CFRZ(:)=0.
!
ZW1(:)=0.
ZW2(:)=0.
!
WHERE( (PRIT(:)>XRTMIN(4)) .AND. (PRRT(:)>XRTMIN(3)) .AND. (PT(:)<XTT) .AND. LDCOMPUTE(:) )
!
   ZW1(:) = XICFRR * PRIT(:) * PCRT(:)                    & ! RICFRRG
                                     * PLBDR(:)**XEXICFRR         &
                                     * PRHODREF(:)**(-XCEXVT-1.0)
!
   ZW2(:) = XRCFRI * PCIT(:) * PCRT(:)                    & ! RRCFRIG
                                     * PLBDR(:)**XEXRCFRI         &
                                     * PRHODREF(:)**(-XCEXVT-1.0)
!
   P_RR_CFRZ(:) = - ZW2(:)
   P_CR_CFRZ(:) = - ZW2(:) * (PCRT(:)/PRRT(:))
   P_RI_CFRZ(:) = - ZW1(:)
   P_CI_CFRZ(:) = - ZW1(:) * (PCIT(:)/PRIT(:))
   P_TH_CFRZ(:) = - P_RR_CFRZ(:) * (PLSFACT(:)-PLVFACT(:))
!
END WHERE
!
PA_TH(:) = PA_TH(:) + P_TH_CFRZ(:)
PA_RR(:) = PA_RR(:) + P_RR_CFRZ(:)
PA_CR(:) = PA_CR(:) + P_CR_CFRZ(:)
PA_RI(:) = PA_RI(:) + P_RI_CFRZ(:)
PA_CI(:) = PA_CI(:) + P_CI_CFRZ(:)
PA_RG(:) = PA_RG(:) - P_RR_CFRZ(:) - P_RI_CFRZ(:)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LIMA_RAIN_FREEZING
