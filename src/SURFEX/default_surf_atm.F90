!     #########
      SUBROUTINE DEFAULT_SURF_ATM(POUT_TSTEP, PCISMIN, PVMODMIN, OALDTHRES,             &
                                    ODRAG_COEF_ARP, OALDZ0H, ONOSOF, ORW_PRECIP,        &
                                    PEDB, PEDC, PEDD, PEDK, PUSURIC, PUSURID, PUSURICL, &
                                    PVCHRNK, PVZ0CM, PRIMAX, PDELTA_MAX, PWINDMIN,      &
                                    OVZIUSTAR0_ARP, PRZHZ0M,                            &
                                    PVZIUSTAR0, ORRGUST_ARP, PRRSCALE, PRRGAMMA,        &
                                    PUTILGUST, OCPL_ARP, OQVNPLUS, OVERTSHIFT,          &
                                    HIMPLICIT_WIND                                      )
!     ########################################################################
!
!!****  *DEFAULT_SURF_ATM* - routine to set default values for the choice of surface schemes
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
REAL,              INTENT(OUT) :: POUT_TSTEP! time step for writing
REAL,              INTENT(OUT) :: PCISMIN   ! minimum wind shear
REAL,              INTENT(OUT) :: PVMODMIN  ! minimum wind module
LOGICAL,           INTENT(OUT) :: OALDTHRES ! flag to activate aladin formulation
LOGICAL,           INTENT(OUT) :: ODRAG_COEF_ARP ! flag to activate aladin formulation for Cd and Ch
LOGICAL,           INTENT(OUT) :: OALDZ0H
LOGICAL,           INTENT(OUT) :: ONOSOF ! flag to deactivate the Subgrid Orography effects on Forcing
LOGICAL,           INTENT(OUT) :: OVERTSHIFT ! flag to deactivate the vertical shift between atmospheric and model orography
REAL,              INTENT(OUT) :: PEDB
REAL,              INTENT(OUT) :: PEDC
REAL,              INTENT(OUT) :: PEDD
REAL,              INTENT(OUT) :: PEDK
REAL,              INTENT(OUT) :: PUSURIC
REAL,              INTENT(OUT) :: PUSURID
REAL,              INTENT(OUT) :: PUSURICL
REAL,              INTENT(OUT) :: PVCHRNK
REAL,              INTENT(OUT) :: PVZ0CM
REAL,              INTENT(OUT) :: PRIMAX
REAL,              INTENT(OUT) :: PDELTA_MAX ! Maximum fraction of the foliage covered by intercepted water
REAL,              INTENT(OUT) :: PWINDMIN   ! Minimum wind speed (canopy)
LOGICAL,           INTENT(OUT) :: OVZIUSTAR0_ARP  ! flag to activate aladin formulation for zoh over sea
REAL,              INTENT(OUT) :: PRZHZ0M
REAL,              INTENT(OUT) :: PVZIUSTAR0
LOGICAL,           INTENT(OUT) :: ORRGUST_ARP     ! flag to activate the correction of CD, CH, CDN due to moist gustiness
REAL,              INTENT(OUT) :: PRRSCALE
REAL,              INTENT(OUT) :: PRRGAMMA
REAL,              INTENT(OUT) :: PUTILGUST
LOGICAL,           INTENT(OUT) :: OCPL_ARP
LOGICAL,           INTENT(OUT) :: OQVNPLUS
LOGICAL,           INTENT(OUT) :: ORW_PRECIP       ! flag used to Read/Write precipitation forcing 
 CHARACTER(LEN=*),  INTENT(OUT) :: HIMPLICIT_WIND   ! wind implicitation option : OLD or NEW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                                    from/into the restart file for ARPEGE/ALADIN run  
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEFAULT_SURF_ATM',0,ZHOOK_HANDLE)
!
POUT_TSTEP = XUNDEF
!
PCISMIN   = 6.7E-5
PVMODMIN  = 0.
OALDTHRES = .FALSE.
!
ODRAG_COEF_ARP = .FALSE.
OALDZ0H = .FALSE.
ONOSOF = .FALSE.
OVERTSHIFT = .TRUE.
ORW_PRECIP = .FALSE.
PEDB = 5.0
PEDC = 5.0
PEDD = 5.0
PEDK = 1.0
PUSURIC  = 1.0
PUSURID  = 0.035
PUSURICL = 4.0
PVCHRNK  = 0.015
PVZ0CM   = 0.0
!
PRIMAX = 0.2
PDELTA_MAX = 1.0
!
PWINDMIN = 1.E-6
!
PRZHZ0M = 1.0
PVZIUSTAR0 = 0.0
OVZIUSTAR0_ARP = .FALSE.
!
ORRGUST_ARP = .FALSE.
PRRSCALE = 1.15E-4
PRRGAMMA = 0.8
PUTILGUST = 0.125
OCPL_ARP=.FALSE.
OQVNPLUS=.FALSE.
!
HIMPLICIT_WIND = 'NEW'
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SURF_ATM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SURF_ATM
