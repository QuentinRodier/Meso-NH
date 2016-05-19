!     #########
      SUBROUTINE READ_PRECIP_n(HPROGRAM,HINIT)
!     ########################################
!
!!****  *READ_PRECIP_n* - routine to read the restart file for
!!                        precip field
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise the 
!!       precip field. Indeed, when ARPEGE/ALADIN is used, 
!!       the precip field is not initialize at the begin of
!!       a run.
!!
!!
!!**  METHOD
!!    ------
!!      The data are read in the initial surface file :
!!        - 2D data fields
!!          
!!      It does not read the grid definition. This should have been
!!      read already.
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
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n    ,     ONLY : NSIZE_FULL,LINIT_PRECIP
!
USE MODD_SURF_ATM,           ONLY : LRW_PRECIP,LSAVE_PRECIP
USE MODD_DIAG_SURF_ATM_n,    ONLY : XRW_RAIN, XRW_SNOW
!
USE MODI_READ_SURF
!
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=3),  INTENT(IN)  :: HINIT    ! choice of fields to initialize
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

!
INTEGER           :: IRESP      ! Error code after redding
! 
 CHARACTER(LEN=12) :: YRECFM     ! Name of the article to be read
!
INTEGER           :: IVERSION   ! surface version
INTEGER           :: IBUGFIX    ! surface bugfix
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PRECIP_N',0,ZHOOK_HANDLE)
SELECT CASE (HINIT)
!
CASE ('PGD')
!     
     ALLOCATE(XRW_RAIN(0))
     ALLOCATE(XRW_SNOW(0))           
     LRW_PRECIP   =.FALSE.
     LINIT_PRECIP =.FALSE.
     LSAVE_PRECIP =.FALSE.
!     
CASE ('PRE')
!     
     IF(LRW_PRECIP)THEN
        LINIT_PRECIP =.TRUE.
        LSAVE_PRECIP =.TRUE.
        ALLOCATE(XRW_RAIN(NSIZE_FULL))
        ALLOCATE(XRW_SNOW(NSIZE_FULL))
        XRW_RAIN(:)=0.0
        XRW_SNOW(:)=0.0
     ELSE   
        LINIT_PRECIP =.FALSE.
        LSAVE_PRECIP =.FALSE.
        ALLOCATE(XRW_RAIN(0))
        ALLOCATE(XRW_SNOW(0))
     ENDIF
!     
CASE DEFAULT
!
        YRECFM='VERSION'
        CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
        IF (IVERSION<6) THEN
           LRW_PRECIP =.FALSE.
        ELSE
           YRECFM='RW_PRECIP'
           CALL READ_SURF(HPROGRAM,YRECFM,LRW_PRECIP,IRESP)
        ENDIF
!
        IF(LRW_PRECIP)THEN
           LINIT_PRECIP =.TRUE.
           LSAVE_PRECIP =.TRUE.
           ALLOCATE(XRW_RAIN(NSIZE_FULL))
           ALLOCATE(XRW_SNOW(NSIZE_FULL))           
           YRECFM='RW_RAIN'
           CALL READ_SURF(HPROGRAM,YRECFM,XRW_RAIN(:),IRESP)
           YRECFM='RW_SNOW'
           CALL READ_SURF(HPROGRAM,YRECFM,XRW_SNOW(:),IRESP)
        ELSE   
           LINIT_PRECIP =.FALSE.
           LSAVE_PRECIP =.FALSE.
           ALLOCATE(XRW_RAIN(0))
           ALLOCATE(XRW_SNOW(0))
        ENDIF
!        
END SELECT
IF (LHOOK) CALL DR_HOOK('READ_PRECIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PRECIP_n
