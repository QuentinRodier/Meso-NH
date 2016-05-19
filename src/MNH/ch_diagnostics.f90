!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ########################## 
      MODULE MODI_CH_DIAGNOSTICS
!!    ########################## 
!
INTERFACE
SUBROUTINE CH_DIAGNOSTICS(PCONC, KMI, KVECNPT)
IMPLICIT NONE
INTEGER, INTENT(IN)                      :: KVECNPT
REAL, DIMENSION(:,:), INTENT(IN) :: PCONC ! the chem species
INTEGER, INTENT(IN)              :: KMI
END SUBROUTINE CH_DIAGNOSTICS
END INTERFACE
END MODULE MODI_CH_DIAGNOSTICS
!!
!!    ############################################## 
      SUBROUTINE CH_DIAGNOSTICS(PCONC, KMI, KVECNPT)
!!    ##############################################
!!
!!*** *CH_DIAGNOSTICS*
!!
!!    PURPOSE
!!    -------
!!    calculate all desirable diagnostics
!!
!!**  METHOD
!!    ------
!!    Actually the following variables will be saved:
!!    PCONC, reaction rates, PROD, LOSS, TERMS;
!!    these variables are  calculated by CH_PRODLOSS, CH_TERMS, and CH_GET_RATES
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!    K. Suhre
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 02/03/95
!!    27/07/96 (K. Suhre) restructured
!!    01/12/03 (Gazen)   change Chemical scheme interface!!
!!    EXTERNAL
!!    --------
USE MODI_CH_GET_RATES
USE MODI_CH_PRODLOSS
USE MODI_CH_TERMS
USE MODI_CH_NONZEROTERMS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_CH_MODEL0D,  ONLY: XTSIMUL, NDIAGIO, CDIAGFORMAT, NVERB
USE MODD_CH_M9_n,       ONLY: NREAC, NEQ, NNONZEROTERMS
!!
!-----------------------------------------------------------------------------
!*    0.  DECLARATIONS
!     ----------------
!
IMPLICIT NONE
!
!*    0.1 Declaration of arguments
!     ----------------------------

INTEGER, INTENT(IN)                      :: KVECNPT
REAL, DIMENSION(:,:), INTENT(IN) :: PCONC ! the chem species
INTEGER, INTENT(IN)              :: KMI
!
!*    0.2 Declaration of local variables
!     ----------------------------------
INTEGER :: JI
!REAL, DIMENSION(KVECNPT,NREAC)       :: ZRATE        ! variable to retrieve data
REAL, DIMENSION(:,:),ALLOCATABLE      :: ZRATE        ! variable to retrieve data
!REAL, DIMENSION(KVECNPT,NEQ)         :: ZPROD, ZLOSS ! dito
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZPROD, ZLOSS ! dito
!REAL, DIMENSION(KVECNPT,NEQ,NREAC)   :: ZTERMS       ! dito
REAL, DIMENSION(:,:,:),ALLOCATABLE    :: ZTERMS       ! dito
!INTEGER, DIMENSION(2, NNONZEROTERMS) :: IINDEX       ! indices of non-zero terms
INTEGER, DIMENSION(:,:),ALLOCATABLE   :: IINDEX       ! indices of non-zero terms
!
! Allocate local arrays 
ALLOCATE(ZRATE(KVECNPT,NREAC))
ALLOCATE(ZPROD(KVECNPT,NEQ), ZLOSS(KVECNPT,NEQ))
ALLOCATE(ZTERMS(KVECNPT,NEQ,NREAC))
ALLOCATE(IINDEX(2, NNONZEROTERMS))
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!
IF (NVERB >= 5) PRINT *, "CH_DIAGNOSTICS called at XTSIMUL = ", XTSIMUL
!
! get reaction rates
CALL CH_GET_RATES(ZRATE,KMI,KVECNPT,NREAC)
!
! get PROD and LOSS terms
CALL CH_PRODLOSS(XTSIMUL,PCONC,ZPROD,ZLOSS,KMI,KVECNPT,NEQ)
!
! get individual TERMS (contrib. of reaction i to the evolution of species j)
CALL CH_TERMS(XTSIMUL,PCONC,ZTERMS,KMI,KVECNPT,NEQ,NREAC)
!
! get the indices of the explicit contributions
! (only non-zero terms will be stored)
CALL CH_NONZEROTERMS(KMI,IINDEX, NNONZEROTERMS)
!
! write variables to file
WRITE(NDIAGIO,CDIAGFORMAT) XTSIMUL, &
                               (PCONC(1,JI), JI = 1, NEQ),           &
                               (ZRATE(1,JI), JI = 1, NREAC),         &
                               (ZPROD(1,JI), JI = 1, NEQ),           &
                               (ZLOSS(1,JI), JI = 1, NEQ),           &
                               (ZTERMS(1,IINDEX(1,JI),IINDEX(2,JI)), &
					   JI = 1, NNONZEROTERMS)
!
DEALLOCATE(ZPROD, ZLOSS)
DEALLOCATE(ZTERMS)
DEALLOCATE(IINDEX)
!
END SUBROUTINE CH_DIAGNOSTICS
