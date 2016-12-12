!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_LCOVER(DGU,U,HPROGRAM,OCOVER)
!     ################################
!
!!****  *READ_LCOVER* - routine to write a file for
!!                         physiographic data file of model _n 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to write the list of covers to a file in parallel using MPI
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	M. Moge   *LA - CNRS*	
!!
!!    MODIFICATIONS
!!    -------------
!!      J. Pianezze 08/2016 replacement of MPI_COMM_WOLRD by NMNH_COMM_WORLD
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef MNH_PARALLEL
USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD
#endif
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
LOGICAL, DIMENSION(JPCOVER)    :: OCOVER   ! list of covers
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
INTEGER           :: IRESP          ! Error code after reading
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
LOGICAL, DIMENSION(JPCOVER)    :: GCOVER   ! tmp list of covers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER   :: IINFO
!-------------------------------------------------------------------------------
!
!
!* ascendant compatibility
IF (LHOOK) CALL DR_HOOK('WRITE_LCOVER',0,ZHOOK_HANDLE)
#ifndef NOMPI
#ifdef MNH_PARALLEL
CALL MPI_ALLREDUCE(OCOVER, GCOVER, SIZE(OCOVER),MPI_LOGICAL, MPI_LOR, NMNH_COMM_WORLD, IINFO)
#else
CALL MPI_ALLREDUCE(OCOVER, GCOVER, SIZE(OCOVER),MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, IINFO)
#endif
#endif
OCOVER(:)=GCOVER(:)
YRECFM='COVER_LIST'
YCOMMENT='(LOGICAL LIST)'
CALL WRITE_SURF(DGU,U,HPROGRAM,YRECFM,OCOVER(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
IF (LHOOK) CALL DR_HOOK('WRITE_LCOVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_LCOVER
