!     #############################################################
      SUBROUTINE STORES_HVAC_AUTOSIZE
!     #############################################################
!
!!****  *STORES_HVAC_AUTOSIZE* - routine to store the HVAC system
!!                               characteristics for further use
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
!!      Original    05/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BEM_n,      ONLY:      LAUTOSIZE,                            &
                                XM_SYS_RAT, XCAP_SYS_RAT, XCAP_SYS_HEAT
USE MODD_DATA_BEM_n, ONLY :     XPAR_M_SYS_RAT,    LDATA_M_SYS_RAT,   &
                                XPAR_CAP_SYS_RAT,  LDATA_CAP_SYS_RAT, &
                                XPAR_CAP_SYS_HEAT, LDATA_CAP_SYS_HEAT,&
                                LDATA_T_SIZE_MIN, LDATA_T_SIZE_MAX
!
!
USE MODI_ABOR1_SFX
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!

IF (LHOOK) CALL DR_HOOK('STORES_HVAC_AUTOSIZE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       8.     Building HVAC automatic sizing:
!               -------------------------------  
!* stores the real systems characteristics in physiographic data 
!  for further use
!
IL = SIZE(XM_SYS_RAT)
!
LDATA_M_SYS_RAT = .TRUE.
ALLOCATE(XPAR_M_SYS_RAT(IL))
XPAR_M_SYS_RAT = XM_SYS_RAT 
!
LDATA_CAP_SYS_RAT = .TRUE.
ALLOCATE(XPAR_CAP_SYS_RAT(IL))
XPAR_CAP_SYS_RAT = XCAP_SYS_RAT
!
LDATA_CAP_SYS_HEAT = .TRUE.
ALLOCATE(XPAR_CAP_SYS_HEAT(IL))
XPAR_CAP_SYS_HEAT = XCAP_SYS_HEAT
!
LAUTOSIZE = .FALSE.
LDATA_T_SIZE_MIN = .FALSE.
LDATA_T_SIZE_MAX = .FALSE.
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('STORES_HVAC_AUTOSIZE',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE STORES_HVAC_AUTOSIZE
