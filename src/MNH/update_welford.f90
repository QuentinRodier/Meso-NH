!MNH_LIC Copyright 2022-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_UPDATE_WELFORD
!     #######################
!
INTERFACE
!
SUBROUTINE UPDATE_WELFORD(KCOUNT,PMEAN,PM2,PVALUE)
        INTEGER,   INTENT(IN)                         :: KCOUNT   ! time count    
        REAL,      DIMENSION(:,:,:),   INTENT(INOUT)  :: PMEAN    ! aggregate for mean value
        REAL,      DIMENSION(:,:,:),   INTENT(INOUT)  :: PM2      ! aggregate for var value
        REAL,      DIMENSION(:,:,:),   INTENT(IN)     :: PVALUE   ! variables
END SUBROUTINE UPDATE_WELFORD
!
END INTERFACE
!
END MODULE MODI_UPDATE_WELFORD
!#########################################################
SUBROUTINE UPDATE_WELFORD(KCOUNT,PMEAN,PM2,PVALUE)
!!****  *UPDATE_WELFORD* - Welford Algorithm
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to compute variance on a large number of sample
!       and avoid catastrophic cancellation, leading to negative variance
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!	    E. Jezequel                                       
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    November, 2022         
! ------------------------------------------------------------------------------
!
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)                          :: KCOUNT        ! time count    
REAL,    DIMENSION(:,:,:),    INTENT(INOUT)  :: PMEAN         ! aggregate for mean value
REAL,    DIMENSION(:,:,:),    INTENT(INOUT)  :: PM2           ! aggregate for var value
REAL,    DIMENSION(:,:,:),    INTENT(IN)     :: PVALUE        ! current value
! local variables
REAL, DIMENSION(SIZE(PVALUE,1),SIZE(PVALUE,2),SIZE(PVALUE,3)) ::  ZDELTA, ZDELTA2
!
ZDELTA = PVALUE - PMEAN
PMEAN  = PMEAN  + ZDELTA / KCOUNT
ZDELTA2= PVALUE - PMEAN
PM2    = PM2 + ZDELTA * ZDELTA2
!
END SUBROUTINE UPDATE_WELFORD
!
