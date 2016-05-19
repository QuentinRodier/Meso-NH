!     #########
      SUBROUTINE SOILTEMP_ARP_PAR(HPROGRAM,OTEMP_ARP,KTEMPLAYER_ARP)
!     ##############################################################
!
!!**** *SOILTEMP_ARP_PAR* Impose special pseudo depth for "force-restore"
!!                        multilayer deep temperature
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2009
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_ISBA_n,   ONLY : CISBA, XSODELX
!
USE MODN_SOILTEMP_ARP
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(OUT)   :: OTEMP_ARP
INTEGER,             INTENT(OUT)   :: KTEMPLAYER_ARP
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!

IF (LHOOK) CALL DR_HOOK('SOILTEMP_ARP_PAR',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN

 SODELX(:)      = XUNDEF
 LTEMP_ARP      = .FALSE.
 NTEMPLAYER_ARP = 4
 !
 !-------------------------------------------------------------------------------
 !
 !*    2.      Input value for SODELX variable
 !             -------------------------------
 !
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
 !
 CALL POSNAM(ILUNAM,'NAM_SOILTEMP_ARP',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SOILTEMP_ARP)
 !
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      Consistency
!             -----------
!
IF(LTEMP_ARP.AND.CISBA=='DIF')THEN
   LTEMP_ARP=.FALSE.
   WRITE(ILUOUT,*)'LTEMP_ARP put at False because you use the ISBA-DF scheme'
ENDIF
!
IF(LTEMP_ARP)THEN
  IF(NTEMPLAYER_ARP>NMAX_LAYER)THEN     
    WRITE(ILUOUT,*)'NTEMPLAYER_ARP is too big (>10), NTEMPLAYER_ARP= ',NTEMPLAYER_ARP
    CALL ABOR1_SFX('NTEMPLAYER_ARP is too big (>10)')
  ELSEIF(NTEMPLAYER_ARP<4)THEN
    WRITE(ILUOUT,*)'NTEMPLAYER_ARP must be at least equal to 4, NTEMPLAYER_ARP= ',NTEMPLAYER_ARP
    CALL ABOR1_SFX('NTEMPLAYER_ARP must be at least equal to 4')
  ENDIF
  IF(COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF)>0.AND. &
       COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF)/=NTEMPLAYER_ARP)THEN  
    WRITE(ILUOUT,*)'Number of SODELX imposed values =',COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF),&
                     ' /= NTEMPLAYER_ARP= ',NTEMPLAYER_ARP  
    CALL ABOR1_SFX('SODELX imposed values /= NTEMPLAYER_ARP')
  ENDIF          
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4.      Initialization
!             -------------------------------
!
IF(LTEMP_ARP)THEN
!
  ALLOCATE(XSODELX(NTEMPLAYER_ARP))
!
  IF(ALL(SODELX(:)==XUNDEF))THEN
!          
    XSODELX(1)=0.5
    XSODELX(2)=1.5
    XSODELX(3)=4.5
    XSODELX(4)=13.5
    WRITE(ILUOUT,*)'SODELX default values : ',XSODELX(:)
!    
  ELSE
!          
    XSODELX(:)=SODELX(1:NTEMPLAYER_ARP)
    WRITE(ILUOUT,*)'SODELX imposed to : ',XSODELX(:)
!    
  ENDIF
!
ELSE
!
  ALLOCATE(XSODELX(0))
!
ENDIF
!
OTEMP_ARP     =LTEMP_ARP
KTEMPLAYER_ARP=NTEMPLAYER_ARP
IF (LHOOK) CALL DR_HOOK('SOILTEMP_ARP_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOILTEMP_ARP_PAR
