!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_LCOVER(HPROGRAM,OCOVER)
!     ################################
!
!!****  *READ_LCOVER* - routine to read a file for
!!                         physiographic data file of model _n 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise the list of covers
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2008
!!      M. Moge     02/2015 parallelization
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_READ_SURF
USE MODI_OLD_NAME
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
INTEGER           :: IRESP          ! Error code after redding
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IVERSION       ! version of surfex file being read
LOGICAL, DIMENSION(:), ALLOCATABLE :: GCOVER ! cover list in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER   :: IINFO
!-------------------------------------------------------------------------------
!
!
!* ascendant compatibility
IF (LHOOK) CALL DR_HOOK('READ_LCOVER',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
IF (IVERSION<=3) THEN
  ALLOCATE(GCOVER(255))
ELSE
  ALLOCATE(GCOVER(JPCOVER))
END IF
 CALL OLD_NAME(HPROGRAM,'COVER_LIST      ',YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,GCOVER(:),IRESP,HDIR='-')
!
#ifndef NOMPI
CALL MPI_ALLREDUCE(GCOVER, OCOVER, SIZE(GCOVER),MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, IINFO)
#endif
DEALLOCATE(GCOVER)
!
IF (LHOOK) CALL DR_HOOK('READ_LCOVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_LCOVER
