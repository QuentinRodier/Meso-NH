!     #########
    SUBROUTINE LAIGAIN(PBSLAI, PLAIMIN, PVEG, PBIOMASS, PLAI, PANDAY)
!   ######################################################################
!!****  *LAIGAIN*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the time change in LAI due to assimilation
!     of CO2. This in turn changes the dry biomass of the canopy.
!              
!!**  METHOD
!!    ------
!     Calvet at al (1998)
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    MODD_CO2V_PAR
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. (1998)
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!      V. Masson   01/03/03 daily assimilation.
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S Lafont    03/2011 PANDAY calcul move to lailoss, nitro_decline
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CO2V_PAR, ONLY : XMC, XMCO2, XPCCO2
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,DIMENSION(:),INTENT(IN)   :: PBSLAI   ! ratio of biomass to LAI
REAL,DIMENSION(:),INTENT(IN)   :: PLAIMIN  ! minimum LAI
REAL,DIMENSION(:),INTENT(IN)   :: PVEG     ! vegetation fraction
!
REAL,DIMENSION(:),INTENT(INOUT):: PANDAY   ! daily net CO2 assimilation (kgCO2 m-2)
REAL,DIMENSION(:),INTENT(INOUT):: PLAI     ! LAI as a function of time:
!                                          ! as a function of growth,
!                                          ! decay, assimilation.
REAL,DIMENSION(:),INTENT(INOUT):: PBIOMASS ! total dry canopy biomass (kgDM m-2)
!
!*      0.2    declarations of local variables
!
REAL :: ZBMCOEF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
! used to compute biomass change (working scalar)
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LAIGAIN',0,ZHOOK_HANDLE)
ZBMCOEF     = XMC/(XMCO2*XPCCO2)
!
!
! Once a day (at midnight), adjust biomass:
! ----------------------------------------
!
WHERE( (PVEG(:)>0) )
!
! change biomass in time due to assimilation of CO2:
! 2011 :this computation have been move to lailoss and nitro_decline
!
!  PBIOMASS(:) = PBIOMASS(:) + PANDAY(:)*ZBMCOEF
!
! make sure biomass doesn't fall below minimum threshold:
!
  PBIOMASS(:) = MAX(PLAIMIN(:)*PBSLAI(:),PBIOMASS(:))
!
! change in LAI in time due to biomass changes:
!
  PLAI(:)     = PBIOMASS(:)/PBSLAI(:)
!
! reset to zero the daily net assimilation for next day:
!
  PANDAY(:)   = 0.
!
END WHERE
IF (LHOOK) CALL DR_HOOK('LAIGAIN',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE LAIGAIN
