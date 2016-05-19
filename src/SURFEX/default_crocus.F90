!     #########
      SUBROUTINE DEFAULT_CROCUS(OSNOWDRIFT,OSNOWDRIFT_SUBLIM,PZ0ICEZ0SNOW,PRHOTHRESHOLD_ICE,&
                 PALBICE1,PALBICE2,PALBICE3,PVAGING_NOGLACIER,PVAGING_GLACIER)  
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for the configuration for Crocus
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
!!	M. Lafaysse   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2012
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
! Logicals to activate / disactivate snowdrift                                          
LOGICAL, INTENT(OUT)          :: OSNOWDRIFT
LOGICAL, INTENT(OUT)          :: OSNOWDRIFT_SUBLIM

! Prescribed ice albedo in 3 spectral bands for glacier simulation with CROCUS scheme.
REAL,  INTENT(OUT) :: PALBICE1,PALBICE2,PALBICE3
!

! Density threshold for ice detection in CROCUS scheme.
REAL,  INTENT(OUT) :: PRHOTHRESHOLD_ICE

! Roughness length ratio between ice and snow
REAL, INTENT(OUT) :: PZ0ICEZ0SNOW

! Parameters for ageing effect on albedo
REAL, INTENT(OUT) :: PVAGING_NOGLACIER,PVAGING_GLACIER
!
!*       0.2   Declarations of local variables
!              -------------------------------
!                                          
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_CROCUS',0,ZHOOK_HANDLE)

OSNOWDRIFT=.TRUE.
OSNOWDRIFT_SUBLIM=.FALSE.

! Roughness length ratio between ice and snow
PZ0ICEZ0SNOW=10.

! 3 bands spectral albedo for glacier ice (CROCUS)
! Default values from Lejeune et al 2009 (Zongo, Bolivia)
PALBICE1=0.38
PALBICE2=0.23
PALBICE3=0.08

! Gerbaux et al 2005 (Saint Sorlin)
! PALBICE1=0.23
! PALBICE2=0.16
! PALBICE3=0.05
!
! Density threshold for ice detection kg.m-3
PRHOTHRESHOLD_ICE=850.

! Parameters for ageing effect on albedo
PVAGING_NOGLACIER=60.
PVAGING_GLACIER=900.


IF (LHOOK) CALL DR_HOOK('DEFAULT_CROCUS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_CROCUS
