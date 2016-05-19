!     #########
      SUBROUTINE CO2_TEB_GREENROOF_INIT_n(PCO2)
!     #####################
!
!!****  *CO2_TEB_GREENROOF_INIT_n* - routine to initialize ISBA-AGS variables
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
!!      Original    02/2003 
!!      J.C. Calvet 01/2004 Externalization
!!      P Le Moigne 11/2004 cotwoinit changed into cotwoinit_n
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S Lafont    09/2008 Add initialisation of POI and ABC (needed for TORI)
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 04/2009 : Add carbon spinup
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_TEB_VEG_n,      ONLY : CPHOTO
USE MODD_TEB_GREENROOF_n,ONLY : XVEGTYPE,                             &
                                XGMES, XGC, XDMAX, XABC, XPOI, XANMAX,&
                                XFZERO, XEPSO, XGAMM, XQDGAMM,        &
                                XQDGMES, XT1GMES, XT2GMES, XAMAX,     &
                                XQDAMAX, XT1AMAX, XT2AMAX, XAH, XBH
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_COTWOINIT_n
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
REAL, DIMENSION(:), INTENT(IN) :: PCO2 ! air CO2 concentration (kg/kg)
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(XVEGTYPE,1)) :: ZTAU_WOOD
INTEGER :: ILU   ! size of arrays
INTEGER :: JP    ! loop on tiles
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CO2_TEB_GREENROOF_INIT_N',0,ZHOOK_HANDLE)
ILU = SIZE(XVEGTYPE,1)
!
ALLOCATE(XANMAX        (ILU))
ALLOCATE(XFZERO        (ILU))
ALLOCATE(XEPSO         (ILU))
ALLOCATE(XGAMM         (ILU))
ALLOCATE(XQDGAMM       (ILU))
ALLOCATE(XQDGMES       (ILU))
ALLOCATE(XT1GMES       (ILU))
ALLOCATE(XT2GMES       (ILU))
ALLOCATE(XAMAX         (ILU))
ALLOCATE(XQDAMAX       (ILU))
ALLOCATE(XT1AMAX       (ILU))
ALLOCATE(XT2AMAX       (ILU))
ALLOCATE(XAH           (ILU))
ALLOCATE(XBH           (ILU))
!
     CALL COTWOINIT_n(CPHOTO, XVEGTYPE,XGMES,PCO2,XGC,&
            XDMAX,XABC,XPOI,XANMAX, XFZERO,           &
            XEPSO,XGAMM,XQDGAMM,XQDGMES,XT1GMES,      &
            XT2GMES,XAMAX,XQDAMAX,XT1AMAX,            &
            XT2AMAX,XAH,XBH,ZTAU_WOOD                 )  
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_TEB_GREENROOF_INIT_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CO2_TEB_GREENROOF_INIT_n
