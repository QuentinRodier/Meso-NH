!     #########
      SUBROUTINE WRITE_LCOVER(HPROGRAM,OCOVER)
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
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!USE MODD_WATFLUX_n,      ONLY : LCOVER
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
INTEGER           :: IRESP          ! Error code after reading
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
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
CALL MPI_ALLREDUCE(OCOVER, GCOVER, SIZE(OCOVER),MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, IINFO)
#endif
OCOVER(:)=GCOVER(:)
YRECFM='COVER_LIST'
YCOMMENT='(LOGICAL LIST)'
CALL WRITE_SURF(HPROGRAM,YRECFM,OCOVER(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
IF (LHOOK) CALL DR_HOOK('WRITE_LCOVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_LCOVER
