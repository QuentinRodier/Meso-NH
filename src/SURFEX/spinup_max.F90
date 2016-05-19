!     #########
      SUBROUTINE SPINUP_MAX(PSPINMAX,KNBYEARSPIN,KNBYEARSOLD,KSPIN)
  
!     #######################################################################
!
!
!!****  *SPINUP_MAX*  
!!
!!    PURPOSE
!!    -------
!!    Number of times the accelerated subroutine is called  
!!     
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	R. Alkama           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      03/26/2012
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL,    INTENT(IN)   :: PSPINMAX  ! max number of times the accelerated subroutine
                                   ! is called for each time step in simulation
                                   ! during the acceleration procedure

INTEGER, INTENT(IN)   :: KNBYEARSPIN ! spinup duration in years
                                     ! nbr of years needed to reach the equilibrium
INTEGER, INTENT(IN)   :: KNBYEARSOLD 
INTEGER, INTENT(OUT)  :: KSPIN        
!                                         
!                                         
!
!*      0.2    declarations of local variables
!
!
REAL, PARAMETER  :: ZSPINFRAC = 0.75 ! fraction of KNBYEARSPIN period used to
                                     ! spin up soil at its maximum PSPINMAX
REAL             :: ZSLOPE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('SPINUP_MAX',0,ZHOOK_HANDLE)
!
IF ( KNBYEARSOLD <= (ZSPINFRAC * KNBYEARSPIN))THEN
   !
   KSPIN = NINT(PSPINMAX)
   !
ELSE IF (KNBYEARSOLD < KNBYEARSPIN)THEN
   !
   ZSLOPE  = PSPINMAX / (REAL(KNBYEARSPIN) - ZSPINFRAC * KNBYEARSPIN)
   !
   KSPIN = NINT(PSPINMAX - ZSLOPE * (KNBYEARSOLD - ZSPINFRAC * KNBYEARSPIN))
   !
   KSPIN = MAX(KSPIN,1)
   !
ELSE
   KSPIN = 1
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SPINUP_MAX',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE 
