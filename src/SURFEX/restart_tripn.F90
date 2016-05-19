!     #########
      SUBROUTINE RESTART_TRIP_n (KLUOUT)
!     ################################
!
!!****  *RESTART_TRIP_n*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP river routing restart.
!     
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/05/05 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODE_RW_TRIP
!
USE MODD_TRIP_n,   ONLY : CGROUNDW, LFLOODT, LPRINT_TRIP, XLEN, &
                            XSURF_STO, XGROUND_STO, XFLOOD_STO,  &
                            XFFLOOD, XHFLOOD  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)             :: KLUOUT
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=15), PARAMETER :: YFILE ='TRIP_RESTART.nc'
 CHARACTER(LEN=20)            :: YVNAME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! * Store output in diag file
!
IF (LHOOK) CALL DR_HOOK('RESTART_TRIP_N',0,ZHOOK_HANDLE)
YVNAME = 'SURF_STO'
 CALL WRITE_TRIP(KLUOUT,YFILE,YVNAME,XLEN,XSURF_STO)   
!
IF(CGROUNDW/='DEF')THEN
  YVNAME = 'GROUND_STO'
  CALL WRITE_TRIP(KLUOUT,YFILE,YVNAME,XLEN,XGROUND_STO)
ENDIF
!
IF(LFLOODT)THEN
  YVNAME = 'FLOOD_STO'
  CALL WRITE_TRIP(KLUOUT,YFILE,YVNAME,XLEN,XFLOOD_STO)           
  YVNAME = 'FFLOOD_T'
  CALL WRITE_TRIP(KLUOUT,YFILE,YVNAME,XLEN,XFFLOOD)        
  YVNAME = 'HFLOOD_T'
  CALL WRITE_TRIP(KLUOUT,YFILE,YVNAME,XLEN,XHFLOOD)        
ENDIF
!
IF(LPRINT_TRIP)WRITE(KLUOUT,*)YFILE,' ended successfully !'
IF (LHOOK) CALL DR_HOOK('RESTART_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE RESTART_TRIP_n
