!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE VEG( PSW_RAD, PTA, PQA, PPS, PRGL, PLAI, PRSMIN,              &
                  PGAMMA, PF2, PRS                                         )  
!     ####################################################################
!
!!****  *VEG*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the surface stomatal resistance Rs
!         
!     
!!**  METHOD
!!    ------
!
!     First calculates the F coefficients (i.e., f, F1, F2, F3, and F4).
!     
!     Then, we have
!
!     Rs = Rsmin / ( F1 F2 F3 F4 LAI )
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    none
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      13/03/95 
!!     (P.Jabouille)  13/11/96    mininum value for ZF1
!!     (V. Masson)    28/08/98    add PF2 for Calvet (1998) CO2 computations
!!     (V. Masson)    01/03/03    puts PF2 in a separate routine
!!     (A. Boone)     21/1&/11    Rs_max in MODD_ISBA_PAR
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_PAR, ONLY : XRS_MAX
USE MODE_THERMOS
!
!
USE YOMHOOK       ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1      ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)   :: PSW_RAD, PTA, PQA, PPS
!                                     PSW_RAD = incoming solar radiation
!                                     PTA   = near-surface air temperature
!                                     PQA   = near-surface air specific humidity
!                                     PPS   = surface pressure
!
REAL, DIMENSION(:), INTENT(IN)   :: PRGL, PLAI, PRSMIN, PGAMMA
!                                     PRGL   = coefficient in the Rs formulation
!                                     PLAI   = leaf area index
!                                     PRSMIN = minimum surface resistance
!                                     PGAMMA = coef. in the Rs calculation
!
REAL, DIMENSION(:), INTENT(IN)   :: PF2      ! water stress coefficient
REAL, DIMENSION(:), INTENT(OUT)  :: PRS      ! ground stomatal resistance
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSW_RAD)) :: ZF, ZF1, ZF3, ZF4
!                                           temporary factors necessary to 
!                                           calculate the surface stomatao resistance
!
REAL, DIMENSION(SIZE(PSW_RAD)) :: ZQSAT
!                                 ZQSAT = specific humidity at saturation
!
!
!*      0.3    declarations of local parameters:
!
REAL, PARAMETER                :: ZDENOM_MIN  = 1.E-6 ! minimum denominator: 
!                                                     ! numerical factor to prevent division by 0
REAL, PARAMETER                :: ZFACTR_MIN  = 1.E-3 ! minimum value for some parameters
!                                                     ! to prevent from being too small 
REAL, PARAMETER                :: ZRS_MIN     = 1.E-4 ! minimum canopy resistance (s m-1)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     THE 'ZF1' FACTOR
!               ---------------
!                                      This factor measures the influence
!                                      of the photosynthetically active radiation
!
IF (LHOOK) CALL DR_HOOK('VEG',0,ZHOOK_HANDLE)
ZF(:)  = 0.55*2.*PSW_RAD(:) / (PRGL(:)+ ZDENOM_MIN ) / ( PLAI(:)+  ZDENOM_MIN )
ZF1(:) = ( ZF(:) + PRSMIN(:)/XRS_MAX) /( 1. + ZF(:) )
ZF1(:) = MAX( ZF1(:), ZDENOM_MIN  )
!
!-------------------------------------------------------------------------------
!
!*       3.     THE 'ZF3' FACTOR
!               ----------------
!                                      This factor represents the effect of
!                                      vapor pressure deficit of the atmosphere.
!                                      For very humid air, the stomatal resistance
!                                      is a small, whereas it increases as the
!                                      air is drier. 
!
!
ZQSAT(:) = QSAT(PTA(:),PPS(:))
!
ZF3(:)   = MAX( 1. - PGAMMA(:)*( ZQSAT(:) - PQA(:) )*1000. , ZFACTR_MIN )
!
!-------------------------------------------------------------------------------
!
!*       4.     THE 'ZF4' FACTOR
!               ----------------
!                                       This factor introduces an air temperature
!                                       dependance on the surface stomatal resistance
!
ZF4(:) = MAX( 1.0 - 0.0016*(298.15-PTA(:))**2, ZFACTR_MIN )
!
!-------------------------------------------------------------------------------
!
!*       5.     THE SURFACE STOMATAL RESISTANCE
!               -------------------------------
!
! use Jarvis-resistance (in standard ISBA version):
! otherwise use Jacobs/ISBA-Ags method (see routine COTWORES)
!
PRS(:) = PRSMIN(:) / ( PLAI(:)+ ZDENOM_MIN )          &
            / ZF1(:) / PF2(:) /ZF3(:) / ZF4(:)  
!
PRS(:) = MIN( PRS(:), XRS_MAX)
PRS(:) = MAX( PRS(:), ZRS_MIN)
IF (LHOOK) CALL DR_HOOK('VEG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE VEG
