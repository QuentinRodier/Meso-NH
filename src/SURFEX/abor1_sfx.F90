!     #############################################################
      SUBROUTINE ABOR1_SFX(YTEXT)
!     #############################################################
!
!!****  *ABOR1_SFX* - abor1 subroutine
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2008 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC
USE MODD_SURFEX_OMP, ONLY : NBLOCK, NBLOCKTOT
USE MODD_SURF_CONF,  ONLY : CPROGNAME, CSOFTWARE
!
USE MODI_GET_LUOUT
USE MODI_CLOSE_FILE
!      
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),  INTENT(IN)  :: YTEXT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=6)  :: YPROGRAM   
 CHARACTER(LEN=20) :: YSTRING
INTEGER           :: ILUOUT         ! logical unit of output file      
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
#ifdef ARO
#include "abor1.intfb.h"
#endif
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('ABOR1_SFX',0,ZHOOK_HANDLE)
YPROGRAM = CPROGNAME
!      
 CALL GET_LUOUT(YPROGRAM,ILUOUT)
!
IF (YPROGRAM=='ASCII ' .OR. YPROGRAM=='TEXTE ' .OR. YPROGRAM=='BINARY') THEN
   IF ( NPROC>1 .OR. NBLOCKTOT>1 ) &
     WRITE(*,*)"MPI TASK NUMBER = ",NRANK,", OMP THREAD NUMBER = ",NBLOCK
   WRITE(*,*)YTEXT
   YSTRING='LISTING_'//TRIM(CSOFTWARE)//'.txt'
   WRITE(*,*)'-------------------------------------------------------------------------------'
   WRITE(*,*) 'MORE DETAILS ABOUT THE CRASH IN THE OUTPUT LISTING FILE: ', TRIM(YSTRING)
   WRITE(*,*)'-------------------------------------------------------------------------------'   
ENDIF
!
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
WRITE(ILUOUT,*) '--------------------   FATAL ERROR in SURFEX  -----------------------------'
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
WRITE(ILUOUT,*) '-                                                                         -'
WRITE(ILUOUT,*)YTEXT
WRITE(ILUOUT,*) '-                                                                         -'
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
WRITE(ILUOUT,*) '---------------------------------------------------------------------------'
 CALL CLOSE_FILE(YPROGRAM,ILUOUT)
!
#ifdef ARO
call abor1('abort by abor1_sfx')
#else
 CALL ABORT
STOP
#endif
IF (LHOOK) CALL DR_HOOK('ABOR1_SFX',1,ZHOOK_HANDLE)
!
END SUBROUTINE ABOR1_SFX
