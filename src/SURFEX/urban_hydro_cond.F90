!     #########
      SUBROUTINE URBAN_HYDRO_COND( PBCOEF,PWSAT,PCONDSAT,PMPOTSAT, &
                                   PWG,PWGI,KWG_LAYER,PK           )
!     ##########################################################################
!
!     Compute the hydraulic conductivity
!
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CST
!!      
!!    REFERENCE
!!    ---------
!!
!!    Based on hydro_soildif.F90
!!      
!!    AUTHOR
!!    ------
!!	A. Boone          * Meteo-France *
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_ISBA_PAR, ONLY : XWGMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PWSAT, PCONDSAT
!                                    PWSAT        = porosity profile (m3 m-3)
!                                    PCONDSAT     = hydraulic conductivity at saturation (m s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PBCOEF,PMPOTSAT
!                                    PMPOTSAT = matric potential at saturation (m) (BC parameters)
!                                    PBCOEF   = slope of the retention curve (-) (BC parameters)
!
INTEGER, DIMENSION(:), INTENT(IN) :: KWG_LAYER  
!                                    KWG_LAYER = Number of soil moisture layers
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PWG, PWGI
!                                    PWG  = volumetric liquid water content (m3 m-3) 
!                                    PWGI = volumetric ice content (m3 m-3)
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PK
!                                    PK     = hydraulic conductivity (m s-1)
!
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: JJ, JL    ! loop control
INTEGER                                  :: INI, INL
REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZPSI, ZWSAT, ZFRZ
REAL, PARAMETER                          :: ZEICE = 6.0  ! Ice vertical diffusion impedence factor 
REAL                                     :: ZLOG10, ZS, ZLOG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
!    ---------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOILDIF',0,ZHOOK_HANDLE)
!
INI = SIZE(PWG(:,:),1)
INL = SIZE(PWG(:,:),2)
!
ZLOG10 = LOG(10.0)
ZFRZ     (:,:) = 0.0
ZWSAT    (:,:) = XUNDEF
ZPSI     (:,:) = XUNDEF
!
! 1. Calculation of hydraulic conductivity
!    -------------------------------------
!
DO JL=1,INL
  DO JJ=1,INI    
!   Modify soil porosity as ice assumed to become part
!   of solid soil matrix (with respect to liquid flow):
    ZWSAT(JJ,JL) = MAX(XWGMIN, PWSAT(JJ,JL)-PWGI(JJ,JL))   
!   Diffusion coefficient for freezing
    ZFRZ(JJ,JL)  = EXP(ZLOG10*(-ZEICE*(PWGI(JJ,JL)/(PWGI(JJ,JL)+PWG(JJ,JL)))))
!   Matric potential (m): 
!   psi=mpotsat*(w/wsat)**(-bcoef)
    ZS           = MIN(1.0,PWG(JJ,JL)/ZWSAT(JJ,JL))
    ZLOG         = PBCOEF(JJ,JL)*LOG(ZS)
    ZPSI(JJ,JL)  = PMPOTSAT(JJ,JL)*EXP(-ZLOG)
!   Hydraulic conductivity from matric potential (m s-1):
!   k=frz*condsat*(psi/mpotsat)**(-2-3/bcoef)
    ZLOG         = -ZLOG*(2.0+3.0/PBCOEF(JJ,JL))
    PK(JJ,JL)    = ZFRZ(JJ,JL)*PCONDSAT(JJ,JL)*EXP(-ZLOG)
  ENDDO
ENDDO    
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_COND',1,ZHOOK_HANDLE)
!
END SUBROUTINE URBAN_HYDRO_COND 






