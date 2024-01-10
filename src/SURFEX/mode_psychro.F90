!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODE_PSYCHRO
!##################
!
!!****  *MODE_PSYCHRO* -
!!
!!    PURPOSE
!!    -------
!      
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/04/11
!!      J.Escobar   11/13 :  remove space in ELSEWHERE statement
!!      R. Schoetter 01/18  correction of bugs and optimization
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
interface PE_FROM_PQ
        module procedure PE_FROM_PQ_0D
        module procedure PE_FROM_PQ_1D
end interface PE_FROM_PQ
interface TD_FROM_TQ
        module procedure TD_FROM_TQ_0D
        module procedure TD_FROM_TQ_1D
end interface TD_FROM_TQ
interface RV_FROM_TPTWB
        module procedure RV_FROM_TPTWB_0D
        module procedure RV_FROM_TPTWB_1D
end interface RV_FROM_TPTWB
interface TWB_FROM_TPQ
        module procedure TWB_FROM_TPQ_0D
        module procedure TWB_FROM_TPQ_1D
end interface TWB_FROM_TPQ
INTERFACE ENTH_FN_T_Q
  MODULE PROCEDURE ENTH_FN_T_Q
END INTERFACE ENTH_FN_T_Q
INTERFACE Q_FN_T_ENTH
  MODULE PROCEDURE Q_FN_T_ENTH
END INTERFACE Q_FN_T_ENTH

contains
!PE_FROM_PQ
!----------
function PE_FROM_PQ_0D(PP, PQ) RESULT(PE) 
!arguments and result
REAL, INTENT(IN) :: PP !atmos. pressure (Pa)
REAL, INTENT(IN) :: PQ !specific humidity (kg/kg)
REAL :: PE !water vapour pressure (Pa)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:PE_FROM_PQ_0D',0,ZHOOK_HANDLE)
PE = PQ * PP /(0.622 + 0.378 * PQ)
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:PE_FROM_PQ_0D',1,ZHOOK_HANDLE)
end function PE_FROM_PQ_0D

function PE_FROM_PQ_1D(PP, PQ) RESULT(PE) 
!arguments and result
REAL, DIMENSION(:), INTENT(IN) :: PP !atmos. pressure (Pa)
REAL, DIMENSION(:), INTENT(IN) :: PQ !specific humidity (kg/kg)
REAL, DIMENSION(SIZE(PQ))      :: PE !water vapour pressure (Pa)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:PE_FROM_PQ_1D',0,ZHOOK_HANDLE)
PE(:) = PQ(:) * PP(:) /(0.622 + 0.378 * PQ(:))
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:PE_FROM_PQ_1D',1,ZHOOK_HANDLE)
end function PE_FROM_PQ_1D
!-------------------------

!TD_FROM_TQ
function TD_FROM_TQ_0D(PT, PQ, PP) RESULT(PTD)
USE MODD_CSTS
USE MODD_SURF_PAR, ONLY: XUNDEF
!arguments and result
REAL, INTENT(IN) :: PT !Air Temp. (K)
REAL, INTENT(IN) :: PQ !Specific humidity (kg/kg)
REAL, INTENT(IN) :: PP ! Atmos. pressure (Pa)
REAL :: PTD !Dew Point Air Temp. (K)
!local variables
REAL :: ALPHA
REAL :: ZPE !water vapour pressure
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TD_FROM_TQ_0D',0,ZHOOK_HANDLE)
ZPE = PE_FROM_PQ(PP,PQ)
ALPHA = LOG(ZPE/1000.)
IF ( (PT.GE.XTT).AND.(PT.LE.(93.+XTT)) ) THEN
        PTD = XTT+6.54+14.526*ALPHA+0.7389*ALPHA*ALPHA+0.09486*ALPHA**3 &
              +0.4569*(ZPE/1000.)**0.1984
ELSE IF (PT .LT. XTT) THEN
        PTD = XTT+6.09+12.608*ALPHA+0.4959*ALPHA*ALPHA
ELSE
        PTD = XUNDEF
ENDIF
PTD = MIN(PTD, PT)
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TD_FROM_TQ_0D',1,ZHOOK_HANDLE)
end function TD_FROM_TQ_0D

function TD_FROM_TQ_1D(PT, PQ, PP) RESULT(PTD)
USE MODD_CSTS
USE MODD_SURF_PAR, ONLY: XUNDEF
!arguments and result
REAL, DIMENSION(:), INTENT(IN) :: PT !Air Temp. (K)
REAL, DIMENSION(:), INTENT(IN) :: PQ !Specific humidity (kg/kg)
REAL, DIMENSION(:), INTENT(IN) :: PP !Atmospheric pressure (Pa)
REAL, DIMENSION(SIZE(PQ))      :: PTD !Dew Point Air Temp. (K)
!local variables
REAL, DIMENSION(SIZE(PQ)) :: ALPHA
REAL, DIMENSION(SIZE(PQ)) :: ZPE !water vapour pressure
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TD_FROM_TQ_1D',0,ZHOOK_HANDLE)
ZPE(:) = PE_FROM_PQ(PP(:), PQ(:))
ALPHA(:) = LOG(ZPE(:)/1000.)
WHERE ( (PT(:).GE.XTT) .AND. (PT(:).LE.(93.+XTT)) ) 
        PTD = XTT+6.54+14.526*ALPHA+0.7389*ALPHA*ALPHA+0.09486*ALPHA**3 &
              +0.4569*(ZPE/1000.)**0.1984
      ELSEWHERE (PT .LT. XTT)
        PTD = XTT+6.09+12.608*ALPHA+0.4959*ALPHA*ALPHA
ELSEWHERE
        PTD = XUNDEF
END WHERE
WHERE(PTD(:).GT.PT(:)) PTD(:) = PT(:)
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TD_FROM_TQ_1D',1,ZHOOK_HANDLE)
end function TD_FROM_TQ_1D
!-------------------------

!RV_FROM_TPTWB
function RV_FROM_TPTWB_0D(PT, PP, PTWB) RESULT(PRV)
USE MODE_THERMOS
USE MODD_CSTS
!arguments and result
REAL, INTENT(IN) :: PT !Air temperature (K)
REAL, INTENT(IN) :: PP !Atmos. Pressure (Pa)
REAL, INTENT(IN) :: PTWB !Wet Bulb Temp. (K)
REAL :: PRV !water vapor mixing ratio (kg/kg)
REAL :: ZRVSAT !saturation water vapor mixing ratio
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:RV_FROM_TPTWB_0D',0,ZHOOK_HANDLE)
ZRVSAT = QSAT(PT, PP) / (1.0 - QSAT(PT, PP))
PRV = ((2501. - 2.326*(PTWB-XTT))*ZRVSAT - 1.006*(PT - PTWB)) &
       / (2501. + 1.86*(PT - XTT) -4.186*(PTWB - XTT))
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:RV_FROM_TPTWB_0D',1,ZHOOK_HANDLE)
end function RV_FROM_TPTWB_0D

function RV_FROM_TPTWB_1D(PT, PP, PTWB) RESULT(PRV)
USE MODE_THERMOS
USE MODD_CSTS
!arguments and result
REAL, DIMENSION(:), INTENT(IN) :: PT !Air temperature (K)
REAL, DIMENSION(:),INTENT(IN) :: PP !Atmos. Pressure (Pa)
REAL, DIMENSION(:),INTENT(IN) :: PTWB !Wet Bulb Temp. (K)
REAL, DIMENSION(SIZE(PT)) :: PRV !water vapor mixing ratio (kg/kg)
REAL, DIMENSION(SIZE(PT)) :: ZRVSAT !saturation water vapor mixing ratio
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:RV_FROM_TPTWB_1D',0,ZHOOK_HANDLE)
ZRVSAT = QSAT(PT(:), PP(:)) / (1 - QSAT(PT(:), PP(:)))
PRV(:) = ((2501. - 2.326*(PTWB(:)-XTT))*ZRVSAT(:) - 1.006*(PT(:) - PTWB(:))) &
       / (2501. + 1.86*(PT(:) - XTT) -4.186*(PTWB(:) - XTT))
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:RV_FROM_TPTWB_1D',1,ZHOOK_HANDLE)       
end function RV_FROM_TPTWB_1D
!----------------------------

!TWB_FROM_TPQ
!------------
function TWB_FROM_TPQ_0D(PT, PP, PQ) RESULT(PTWB)
!
USE MODE_THERMOS
USE MODD_CSTS, ONLY : XTT
!
! Robert:
! The original version of this calculation based on an iteration
! has been very time consuming and is therefore replaced by an approximation
! taken from Stull (2011) JAMC
!
! This formula is only valid for a range of temperature
! and humidity common for application of air conditioning
! inside buildings and must not be used for other purposes
!
! This approximation is good for a pressure of 101325 Pa
! and must therefore not be used in the upper atmosphere
! or for very high areas. However, as illustrated in Stull (2011)
! the error made by this assumption is not very large (e.g. for p = 800 hPa)
!
!arguments and results
REAL, INTENT(IN) :: PT !air temperature (K)
REAL, INTENT(IN) :: PQ !humidity content (kg/kg)
REAL, INTENT(IN) :: PP !atmos. pressure (Pa)
!local variables
!
REAL :: ZPE
REAL :: ZRH
REAL :: ZPT
! Output variable
!
REAL :: PTWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TWB_FROM_TPQ_0D',0,ZHOOK_HANDLE)
!
! Calculation of relative humidity
!
ZPE = PE_FROM_PQ(PP,PQ)
ZRH = 100.0*ZPE/PSAT(PT)
!
! Conversion K -> °C
!
ZPT = PT-XTT
!
! If the relative humidity is outside of
! the regression range it is corrected
!
IF (ZRH.LT.10.0) ZRH=10.0
IF (ZRH.GT.99.0) ZRH=99.0
!
! Check whether the temperature lies within the regression limits
!
IF (ZPT.LT.10.0) THEN
!
! Very simple approximation for the rare case
! where climatisation is required and the air
! temperature is below 10 °C
!
  PTWB = PT - 2.0
ELSE
! The regression from Stull (2011) is used
! Wet bulb temperature from air temperature (°C)
! and relative humidity (%)
!
  PTWB = ZPT*ATAN(0.151977*SQRT(ZRH+8.313659))        + &
          ATAN(ZPT+ZRH)                                  - &
          ATAN(ZRH-1.676331)                             + &
          0.00391838*(ZRH)**(3.0/2.0)*ATAN(0.023101*ZRH) - &
          4.686035
! Conversion °C -> K
!
  PTWB = PTWB + XTT
ENDIF
!
IF (PTWB.GT.PT) PTWB=PT
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TWB_FROM_TPQ_0D',1,ZHOOK_HANDLE)
end function TWB_FROM_TPQ_0D

function TWB_FROM_TPQ_1D(PT, PP, PQ) RESULT(PTWB)
!
USE MODE_THERMOS
USE MODD_CSTS, ONLY : XTT
USE MODI_ABOR1_SFX
!
! Robert:
! The original version of this calculation based on an iteration
! has been very time consuming and is therefore replaced by an approximation
! taken from Stull (2011) JAMC
!
! This formula is only valid for a range of temperature
! and humidity common for application of air conditioning
! inside buildings and must not be used for other purposes
!
! This approximation is good for a pressure of 101325 Pa
! and must therefore not be used in the upper atmosphere
! or for very high areas. However, as illustrated in Stull (2011)
! the error made by this assumptions are not too high (e.g. for p = 800 hPa)
!
!arguments and results
REAL, DIMENSION(:), INTENT(IN) :: PT !air temperature (K)
REAL, DIMENSION(:), INTENT(IN) :: PQ !humidity content (kg/kg)
REAL, DIMENSION(:), INTENT(IN) :: PP !atmos. pressure (Pa)
!local variables
REAL, DIMENSION(SIZE(PT)) :: ZPE
REAL, DIMENSION(SIZE(PT)) :: ZRH
REAL, DIMENSION(SIZE(PT)) :: ZPT
! Output variable
REAL, DIMENSION(SIZE(PT)) :: PTWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TWB_FROM_TPQ_1D',0,ZHOOK_HANDLE)
!
! Calculation of relative humidity
ZPE(:) = PE_FROM_PQ(PP(:),PQ(:))
ZRH(:) = 100.0*ZPE(:)/PSAT(PT(:))
!
! Conversion K -> °C
ZPT(:) = PT(:)-XTT
!
! If the relative humidity is outside of
! the regression range it is corrected
WHERE(ZRH.LT.10.0) ZRH=10.0
WHERE(ZRH.GT.99.0) ZRH=99.0
!
! Check whether the temperature lies within the regression limits
IF ((MINVAL(ZPT).LT.10.0)) CALL ABOR1_SFX ("Air temperature outside of regression range")
! Wet bulb temperature from air temperature (°C)
! and relative humidity (%)
!
PTWB(:) = ZPT(:)*ATAN(0.151977*SQRT(ZRH(:)+8.313659))          + &
        ATAN(ZPT(:)+ZRH(:))                                  - &
        ATAN(ZRH(:)-1.676331)                                + &
        0.00391838*(ZRH(:))**(3.0/2.0)*ATAN(0.023101*ZRH(:)) - &
        4.686035
! Conversion °C -> K
PTWB(:) = PTWB(:) + XTT
IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:TWB_FROM_TPQ_1D',1,ZHOOK_HANDLE)
end function TWB_FROM_TPQ_1D
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION ENTH_FN_T_Q(PT,PQ) RESULT(PENTH)
!     ######################################
!
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the enthalpy function
!       of temperature and humidity content
!      
!
!!**  METHOD
!!    ------
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------  
!!      
!!    REFERENCE
!!    ---------
!!      
!!
!!
!!    AUTHOR
!!    ------
!!      
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/04/11 
!!      A. Alias    01/2013   compi. on Bull : must be 1.0E-5 instead of 1.0D-5
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, INTENT(IN)                :: PT     ! Temperature (K)
REAL, INTENT(IN)                :: PQ     ! Humidity content (kg/kg)
REAL                           :: PENTH  ! Enthalpy (J/kg)
!
!*       0.2   Declarations of local variables
!
REAL        :: ZT                          ! Temperature (C)
REAL        :: ZRV                         ! Mixing ratio (kg/kg_da)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
      IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:ENTH_FN_T_Q',0,ZHOOK_HANDLE)
      ZT = PT - 273.15
      ZRV = PQ/(1.0-PQ)
      IF (ZRV.LT.1.0E-5) ZRV = 1.0E-5
      PENTH=1.00484d3*ZT+ZRV*(2.50094d6+1.85895d3*ZT)
!
      IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:ENTH_FN_T_Q',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION ENTH_FN_T_Q
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION Q_FN_T_ENTH(PT,PENTH) RESULT(PQ)
!     ######################################
!
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the humidity content
!       as a function of temperature and enthalpy
!      
!
!!**  METHOD
!!    ------
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------  
!!      
!!    REFERENCE
!!    ---------
!!      
!!
!!
!!    AUTHOR
!!    ------
!!      
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/04/11 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, INTENT(IN)                :: PT     ! Temperature (K)
REAL, INTENT(IN)                :: PENTH  ! Enthalpy (J/kg)
REAL                           :: PQ     ! Humidity content (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL        :: ZT                          ! Temperature (C)
REAL        :: ZRV                         ! Mixing ratio (kg/kg_da)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
      IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:Q_FN_T_ENTH',0,ZHOOK_HANDLE)
!
      ZT = PT - 273.15
!
!    calculate mixing ratio
      ZRV=(PENTH-1.00484d3*ZT)/(2.50094d6+1.85895d3*ZT)
!
!    validity test
      IF (ZRV .LT. 0.0d0) ZRV=1.d-5
!
!    calculate humidity content
      PQ = ZRV/(1.0+ZRV)
!
     IF (LHOOK) CALL DR_HOOK('MODE_PSYCHRO:Q_FN_T_ENTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION Q_FN_T_ENTH

END MODULE MODE_PSYCHRO
