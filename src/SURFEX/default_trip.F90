!     #########
      SUBROUTINE DEFAULT_TRIP(HGROUNDW,HVIT,OFLOODT,ODIAG_CPL,     &
                                OTRIP_DIAG_MISC,ONCPRINT,OPRINT_TRIP,&
                                PTSTEP_COUPLING,PTRIP_TSTEP,PTAUG,   &
                                PVEL,PRATMED                         )       
!     ########################################################################
!
!!****  *DEFAULT_TRIP* - routine to set default values for the configuration for ISBA
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
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
 CHARACTER(LEN=3),  INTENT(OUT) :: HGROUNDW      !Use groundwater scheme
                                                !'DEF' = No groundwater scheme
                                                !'CST' = Constant transfert time
                                                !'VAR' = Textural dependence of transfert time
!                                                
 CHARACTER(LEN=3),  INTENT(OUT) :: HVIT          !Type of stream flow velocity
                                                !'DEF' = constant and uniforme v=0.5m/s
                                                !'VAR' = variable velocity
!                                                
LOGICAL,  INTENT(OUT)          :: OFLOODT         !if true, use TRIP-FLOOD
LOGICAL,  INTENT(OUT)          :: OTRIP_DIAG_MISC !if true, more diag
LOGICAL,  INTENT(OUT)          :: ODIAG_CPL       !Coupling output diag
LOGICAL,  INTENT(OUT)          :: ONCPRINT        !Netcdf read/write messages
LOGICAL,  INTENT(OUT)          :: OPRINT_TRIP     !Water budget messages                                               
!
REAL,     INTENT(OUT)          :: PTSTEP_COUPLING ! Coupling Time Step
REAL,     INTENT(OUT)          :: PTRIP_TSTEP     ! TRIPTime Step
REAL,     INTENT(OUT)          :: PTAUG           ! Constant transfert time value
REAL,     INTENT(OUT)          :: PVEL            ! Constant velocity value
REAL,     INTENT(OUT)          :: PRATMED         ! meandering ratio
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TRIP',0,ZHOOK_HANDLE)
HGROUNDW        = 'DEF'
HVIT            = 'DEF'   
OFLOODT         = .FALSE.
OTRIP_DIAG_MISC = .FALSE.
ODIAG_CPL       = .FALSE. 
ONCPRINT        = .FALSE. 
OPRINT_TRIP     = .FALSE.
!
PTSTEP_COUPLING = 86400.
PTRIP_TSTEP     = 3600.
!
PTAUG           = 30.0
PVEL            = 0.5
!
PRATMED         = 1.4
IF (LHOOK) CALL DR_HOOK('DEFAULT_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_TRIP
