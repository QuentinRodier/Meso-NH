!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-------------------------------------------------------------------------------
!      #################################
       MODULE MODI_LIMA_ICE_AGGREGATION_SNOW
!      #################################
!
INTERFACE
   SUBROUTINE LIMA_ICE_AGGREGATION_SNOW (LDCOMPUTE,                      &
                                         PT, PRHODREF,                   &
                                         PRIT, PRST, PCIT, PLBDI, PLBDS, &
                                         P_RI_AGGS, P_CI_AGGS            )
!
LOGICAL, DIMENSION(:),INTENT(IN)    :: LDCOMPUTE
!
REAL, DIMENSION(:),   INTENT(IN)    :: PT
REAL, DIMENSION(:),   INTENT(IN)    :: PRHODREF
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRIT
REAL, DIMENSION(:),   INTENT(IN)    :: PRST
REAL, DIMENSION(:),   INTENT(IN)    :: PCIT
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDI 
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDS 
!
REAL, DIMENSION(:),   INTENT(OUT)   :: P_RI_AGGS
REAL, DIMENSION(:),   INTENT(OUT)   :: P_CI_AGGS
!
END SUBROUTINE LIMA_ICE_AGGREGATION_SNOW
END INTERFACE
END MODULE MODI_LIMA_ICE_AGGREGATION_SNOW
!
!     #######################################################################
      SUBROUTINE LIMA_ICE_AGGREGATION_SNOW (LDCOMPUTE,                      &
                                            PT, PRHODREF,                   &
                                            PRIT, PRST, PCIT, PLBDI, PLBDS, &
                                            P_RI_AGGS, P_CI_AGGS            )
!     #######################################################################
!
!!    PURPOSE
!!    -------
!!      Compute the aggregation of pristine ice on snow/aggregates
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
!  J. Wurtz       03/2022: new snow characteristics
!  B. Vie         03/2022: Add option for 1-moment pristine ice
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST,             ONLY : XTT
USE MODD_PARAM_LIMA,      ONLY : XRTMIN, XCTMIN, XCEXVT, NMOM_I
USE MODD_PARAM_LIMA_COLD, ONLY : XBI, XCCS, XCXS, XCOLEXIS, XAGGS_CLARGE1, XAGGS_CLARGE2, &
                                 XAGGS_RLARGE1, XAGGS_RLARGE2, XFIAGGS, XBS, XNS
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL, DIMENSION(:),INTENT(IN)    :: LDCOMPUTE
!
REAL, DIMENSION(:),   INTENT(IN)    :: PT
REAL, DIMENSION(:),   INTENT(IN)    :: PRHODREF
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRIT
REAL, DIMENSION(:),   INTENT(IN)    :: PRST
REAL, DIMENSION(:),   INTENT(IN)    :: PCIT
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDI 
REAL, DIMENSION(:),   INTENT(IN)    :: PLBDS 
!
REAL, DIMENSION(:),   INTENT(OUT)   :: P_RI_AGGS
REAL, DIMENSION(:),   INTENT(OUT)   :: P_CI_AGGS
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PRIT)) :: ZZW1, ZZW2, ZZW3 ! work arrays
!
!-------------------------------------------------------------------------------
!
!
!*       2.4    Aggregation of r_i on r_s: CIAGGS and RIAGGS
!        ---------------------------------------------------
!
ZZW1(:) = 0.
ZZW2(:) = 0.
ZZW3(:) = 0.
!
P_RI_AGGS(:) = 0.
P_CI_AGGS(:) = 0.
!
!
IF (NMOM_I.EQ.1) THEN 
   WHERE ( (PRIT(:)>XRTMIN(4)) .AND. (PRST(:)>XRTMIN(5)) .AND. LDCOMPUTE(:) )
      ZZW1(:) = XFIAGGS * EXP( XCOLEXIS*(PT(:)-XTT) ) &
                        * PRIT(:)                     &
                        * PLBDS(:)**(1.-0.27-2.)      &
                        * PRHODREF(:)**(-XCEXVT)
!
      P_RI_AGGS(:) = - ZZW1(:)
   END WHERE
ELSE
   WHERE ( (PRIT(:)>XRTMIN(4)) .AND. (PRST(:)>XRTMIN(5)) .AND. LDCOMPUTE(:) )
      ZZW1(:) = (PLBDI(:) / PLBDS(:))**3
      ZZW2(:) = (PCIT(:)*(XNS*PRST(:)*PLBDS(:)**XBS)*EXP(XCOLEXIS*(PT(:)-XTT) )) &
           / (PLBDI(:)**3)
      ZZW3(:) = ZZW2(:)*(XAGGS_CLARGE1+XAGGS_CLARGE2*ZZW1(:))
!
      P_CI_AGGS(:) = - ZZW3(:)
!
      ZZW2(:) = ZZW2(:) / PLBDI(:)**XBI
      ZZW2(:) = ZZW2(:)*(XAGGS_RLARGE1+XAGGS_RLARGE2*ZZW1(:))
!
      P_RI_AGGS(:) = - ZZW2(:)
   END WHERE
END IF
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LIMA_ICE_AGGREGATION_SNOW
