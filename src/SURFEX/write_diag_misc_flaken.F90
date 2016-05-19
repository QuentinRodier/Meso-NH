!     #########
      SUBROUTINE WRITE_DIAG_MISC_FLAKE_n(HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MISC_FLAKE* - writes the FLAKE diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!      Original    10/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!USE MODD_FLAKE_n
USE MODD_DIAG_MISC_FLAKE_n,ONLY : LWATER_PROFILE, XZW_PROFILE, XTW_PROFILE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: IZ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_FLAKE_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'WATER ','FLAKE   ','WRITE')
!
!-------------------------------------------------------------------------------
!
IF (LWATER_PROFILE) THEN
!
!*       Miscellaneous fields :
!        ----------------------
DO IZ=1,SIZE(XZW_PROFILE)
   WRITE(YRECFM,'(F5.1)') XZW_PROFILE(IZ)
   YRECFM='TW_'//TRIM(ADJUSTL(YRECFM))
   YCOMMENT='X_Y_'//YRECFM//' (K)'
!
   CALL WRITE_SURF(HPROGRAM,YRECFM,XTW_PROFILE(IZ,:),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
END IF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DIAG_MISC_FLAKE_n
