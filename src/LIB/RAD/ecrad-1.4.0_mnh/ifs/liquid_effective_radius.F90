MODULE MODI_LIQUID_EFFECTIVE_RADIUS

INTERFACE

SUBROUTINE LIQUID_EFFECTIVE_RADIUS &
     & (KIDIA, KFDIA, KLON, KLEV, &
     &  PPRESSURE, PTEMPERATURE, PCLOUD_FRAC, PQ_LIQ, PQ_RAIN, &
     &  PCCT_LIMA, PCRT_LIMA,  &                    
     &  PLAND_FRAC, PCCN_LAND, PCCN_SEA, &
     &  PRE_UM)


USE PARKIND1 , ONLY : JPIM, JPRB


! INPUT ARGUMENTS

! *** Array dimensions and ranges
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA    ! Start column to process
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA    ! End column to process
INTEGER(KIND=JPIM),INTENT(IN) :: KLON     ! Number of columns
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV     ! Number of levels

! *** Variables on model levels
REAL(KIND=JPRB),   INTENT(IN) :: PPRESSURE(KLON,KLEV)    ! (Pa)
REAL(KIND=JPRB),   INTENT(IN) :: PTEMPERATURE(KLON,KLEV) ! (K)
REAL(KIND=JPRB),   INTENT(IN) :: PCLOUD_FRAC(KLON,KLEV)
REAL(KIND=JPRB),   INTENT(IN) :: PQ_LIQ(KLON,KLEV)       ! (kg/kg)
REAL(KIND=JPRB),   INTENT(IN) :: PQ_RAIN(KLON,KLEV)      ! (kg/kg)

REAL(KIND=JPRB),  INTENT (IN)   :: PCCT_LIMA(KLON,KLEV) ! cloud water concentration (LIMA)
REAL(KIND=JPRB),  INTENT (IN)   :: PCRT_LIMA(KLON,KLEV) ! rain water concentration (LIMA)

! *** Single-level variables 
REAL(KIND=JPRB),   INTENT(IN) :: PLAND_FRAC(KLON)        ! 1=land, 0=sea
REAL(KIND=JPRB),   INTENT(IN) :: PCCN_LAND(KLON)
REAL(KIND=JPRB),   INTENT(IN) :: PCCN_SEA(KLON)

! OUTPUT ARGUMENT

! Effective radius
REAL(KIND=JPRB),  INTENT(OUT) :: PRE_UM(KLON,KLEV) ! (microns)

END SUBROUTINE LIQUID_EFFECTIVE_RADIUS
END INTERFACE
END MODULE MODI_LIQUID_EFFECTIVE_RADIUS

SUBROUTINE LIQUID_EFFECTIVE_RADIUS &
     & (KIDIA, KFDIA, KLON, KLEV, &
     &  PPRESSURE, PTEMPERATURE, PCLOUD_FRAC, PQ_LIQ, PQ_RAIN, &
     &  PCCT_LIMA, PCRT_LIMA,  &                    
     &  PLAND_FRAC, PCCN_LAND, PCCN_SEA, &
     &  PRE_UM)

! LIQUID_EFFECTIVE_RADIUS
!
! (C) Copyright 2015- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! PURPOSE
! -------
!   Calculate effective radius of liquid clouds
!
! AUTHOR
! ------
!   Robin Hogan, ECMWF (using code extracted from radlswr.F90)
!   Original: 2015-09-24
!
! MODIFICATIONS
! 2021-04-20 (Q. Libois) Compatibility with MNH
!   - embed in module with interface
!
!
! -------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
! USE YOERAD   , ONLY : YRERAD
USE MODD_PARAM_ECRAD_n , ONLY : NRADLP, NAERMACC, NMCVAR, XCCNSEA, XCCNLND, NAERCLD, &
                            &   NACTAERO, LCCNO, LCCNL, XCCNLND, XCCNSEA
USE YOM_YGFL , ONLY : YGFL
!USE YOECLDP  , ONLY : YRECLDP
USE YOERDU   , ONLY : REPLOG, REPSCW
USE YOMLUN   , ONLY : NULERR
USE YOMCST   , ONLY : RD, RPI
USE MODD_PARAM_LIMA , ONLY    : XALPHAR,XNUR,       & ! Raindrop      distribution parameters
                            &   XALPHAC,XNUC          ! Cloud droplet distribution parameters

! -------------------------------------------------------------------

IMPLICIT NONE

! INPUT ARGUMENTS

! *** Array dimensions and ranges
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA    ! Start column to process
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA    ! End column to process
INTEGER(KIND=JPIM),INTENT(IN) :: KLON     ! Number of columns
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV     ! Number of levels

! *** Variables on model levels
REAL(KIND=JPRB),   INTENT(IN) :: PPRESSURE(KLON,KLEV)    ! (Pa)
REAL(KIND=JPRB),   INTENT(IN) :: PTEMPERATURE(KLON,KLEV) ! (K)
REAL(KIND=JPRB),   INTENT(IN) :: PCLOUD_FRAC(KLON,KLEV)
REAL(KIND=JPRB),   INTENT(IN) :: PQ_LIQ(KLON,KLEV)       ! (kg/kg)
REAL(KIND=JPRB),   INTENT(IN) :: PQ_RAIN(KLON,KLEV)      ! (kg/kg)

REAL(KIND=JPRB),  INTENT (IN)   :: PCCT_LIMA(KLON,KLEV) ! cloud water concentration (LIMA)
REAL(KIND=JPRB),  INTENT (IN)   :: PCRT_LIMA(KLON,KLEV) ! rain water concentration (LIMA)

! *** Single-level variables 
REAL(KIND=JPRB),   INTENT(IN) :: PLAND_FRAC(KLON)        ! 1=land, 0=sea
REAL(KIND=JPRB),   INTENT(IN) :: PCCN_LAND(KLON)
REAL(KIND=JPRB),   INTENT(IN) :: PCCN_SEA(KLON)

! OUTPUT ARGUMENT

! Effective radius
REAL(KIND=JPRB),  INTENT(OUT) :: PRE_UM(KLON,KLEV) ! (microns)

! PARAMETERS

! Minimum and maximum effective radius, in microns
REAL(KIND=JPRB), PARAMETER :: PP_MIN_RE_UM =  4.0_JPRB
REAL(KIND=JPRB), PARAMETER :: PP_MAX_RE_UM = 30.0_JPRB

! LOCAL VARIABLES
INTEGER :: IRADLP ! ID of effective radius scheme to use
INTEGER :: NACTIVE_AEROSOL ! Number of active aerosol
REAL(KIND=JPRB) :: ZCCN    ! CCN concentration (units?)

REAL(KIND=JPRB) :: ZSPECTRAL_DISPERSION
REAL(KIND=JPRB) :: ZNTOT_CM3 ! Number conc in cm-3
REAL(KIND=JPRB) :: ZRE_CUBED
REAL(KIND=JPRB) :: ZLWC_GM3, ZRWC_GM3 ! In-cloud liquid, rain content in g m-3
REAL(KIND=JPRB) :: PCCT_CM3 ! Droplet concentration(from lima) in  cm-3
REAL(KIND=JPRB) :: ZAIR_DENSITY_GM3   ! Air density in g m-3
REAL(KIND=JPRB) :: ZRAIN_RATIO        ! Ratio of rain to liquid water content
REAL(KIND=JPRB) :: ZWOOD_FACTOR, ZRATIO

INTEGER :: JL, JK

REAL(KIND=JPRB) :: ZHOOK_HANDLE

! -------------------------------------------------------------------

#include "abor1.intfb.h"

! -------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LIQUID_EFFECTIVE_RADIUS',0,ZHOOK_HANDLE)

! -------------------------------------------------------------------

! Reproduce logic from RADLSWR
NACTIVE_AEROSOL = NACTAERO
IF (NACTAERO == 0 .AND. NAERMACC == 1) NACTIVE_AEROSOL = NMCVAR
IRADLP = NRADLP
IF (NACTIVE_AEROSOL >= 12 .AND. NAERCLD > 0 ) IRADLP=3 

SELECT CASE(IRADLP)
CASE(0)
  ! Very old parameterization as a function of pressure, used in ERA-15
  PRE_UM(KIDIA:KFDIA,:) = 10.0_JPRB &
       &  + (100000.0_JPRB-PPRESSURE(KIDIA:KFDIA,:))*3.5_JPRB
  
CASE(1)
  ! Simple distinction between land (10um) and ocean (13um) by Zhang
  ! and Rossow
  DO JL = KIDIA,KFDIA
    IF (PLAND_FRAC(JL) < 0.5_JPRB) THEN
      PRE_UM(JL,:) = 13.0_JPRB
    ELSE
      PRE_UM(JL,:) = 10.0_JPRB
    ENDIF
  ENDDO
  
CASE(2)
  ! Martin et al. (JAS 1994)
  DO JL = KIDIA,KFDIA
    ! First compute the cloud droplet concentration
    IF (PLAND_FRAC(JL) < 0.5_JPRB) THEN
      ! Sea case
      IF (LCCNO) THEN
        ZCCN = PCCN_SEA(JL)
      ELSE
        ZCCN = XCCNSEA
      ENDIF
      ZSPECTRAL_DISPERSION = 0.77_JPRB
      ! Cloud droplet concentration in cm-3 (activated CCN) over
      ! ocean
      ZNTOT_CM3 = -1.15E-03_JPRB*ZCCN*ZCCN + 0.963_JPRB*ZCCN + 5.30_JPRB
    ELSE
      ! Land case
      IF (LCCNL) THEN 
        ZCCN=PCCN_LAND(JL)
      ELSE  
        ZCCN = XCCNLND
      ENDIF
      ZSPECTRAL_DISPERSION = 0.69_JPRB
      ! Cloud droplet concentration in cm-3 (activated CCN) over
      ! land
      ZNTOT_CM3 = -2.10E-04_JPRB*ZCCN*ZCCN + 0.568_JPRB*ZCCN - 27.9_JPRB
    ENDIF
    
    ZRATIO = (0.222_JPRB/ZSPECTRAL_DISPERSION)**0.333_JPRB
    
    DO JK = 1,KLEV

      ! Only consider cloudy regions
      IF (PCLOUD_FRAC(JL,JK) >= 0.001_JPRB &
           &  .AND. (PQ_LIQ(JL,JK)+PQ_RAIN(JL,JK)) > 0.0_JPRB) THEN

        ! Compute liquid and rain water contents
        ZAIR_DENSITY_GM3 = 1000.0_JPRB * PPRESSURE(JL,JK) &
             &           / (RD*PTEMPERATURE(JL,JK))
        ! In-cloud mean water contents found by dividing by cloud
        ! fraction
        ZLWC_GM3 = ZAIR_DENSITY_GM3 * PQ_LIQ(JL,JK)  / PCLOUD_FRAC(JL,JK)
        ZRWC_GM3 = ZAIR_DENSITY_GM3 * PQ_RAIN(JL,JK) / PCLOUD_FRAC(JL,JK)
      
        ! Wood's (2000, eq. 19) adjustment to Martin et al's
        ! parameterization
        IF (ZLWC_GM3 > REPSCW) THEN
          ZRAIN_RATIO = ZRWC_GM3 / ZLWC_GM3
          ZWOOD_FACTOR = ((1.0_JPRB + ZRAIN_RATIO)**0.666_JPRB) &
               &     / (1.0_JPRB + 0.2_JPRB * ZRATIO*ZRAIN_RATIO)
        ELSE
          ZWOOD_FACTOR = 1.0_JPRB
        ENDIF
      
        ! g m-3 and cm-3 units cancel out with density of water
        ! 10^6/(1000*1000); need a factor of 10^6 to convert to
        ! microns and cubed root is factor of 100 which appears in
        ! equation below
        ZRE_CUBED = (3.0_JPRB * (ZLWC_GM3 + ZRWC_GM3)) &
             &    / (4.0_JPRB*RPI*ZNTOT_CM3*ZSPECTRAL_DISPERSION)
        IF (ZRE_CUBED > REPLOG) THEN
          PRE_UM(JL,JK) = ZWOOD_FACTOR*100.0_JPRB*EXP(0.333_JPRB*LOG(ZRE_CUBED))
          ! Make sure effective radius is bounded in range 4-30 microns
          PRE_UM(JL,JK) = MAX(PP_MIN_RE_UM, MIN(PRE_UM(JL,JK), PP_MAX_RE_UM))
        ELSE
          PRE_UM(JL,JK) = PP_MIN_RE_UM
        ENDIF

      ELSE
        ! Cloud fraction or liquid+rain water content too low to
        ! consider this a cloud
        PRE_UM(JL,JK) = PP_MIN_RE_UM
        

      ENDIF

    ENDDO
    
  ENDDO

CASE(4)

 ! Calculates effective radius from LIMA
  DO JL = KIDIA,KFDIA
    ! First compute the cloud droplet concentration
    
    DO JK = 1,KLEV

      ! Only consider cloudy regions
      IF (PCLOUD_FRAC(JL,JK) >= 0.001_JPRB &
           &  .AND. (PQ_LIQ(JL,JK)+PQ_RAIN(JL,JK)) > 0.0_JPRB) THEN

        ! Compute liquid and rain water contents
        ZAIR_DENSITY_GM3 = 1000.0_JPRB * PPRESSURE(JL,JK) &
             &           / (RD*PTEMPERATURE(JL,JK))
        ! In-cloud mean water contents found by dividing by cloud
        ! fraction
        PCCT_CM3 = PCCT_LIMA (JL, JK) * ZAIR_DENSITY_GM3 * 1.E-9_JPRB
        ZLWC_GM3 = ZAIR_DENSITY_GM3 * PQ_LIQ(JL,JK)  / PCLOUD_FRAC(JL,JK)
        PRE_UM(JL,JK)=100*(4.*ZLWC_GM3*(XNUC+2)**2/(PCCT_CM3*3.*RPI*(XNUC**2+XNUC)))**(1./3.)     
!        print*,"r_eff=",PRE_UM(JL,JK)
        if (PRE_UM(JL,JK)>50) then
                PRE_UM(JL,JK)=50.
        else if (PRE_UM(JL,JK)<1.) then
                PRE_UM(JL,JK)=1.
        end if


      ELSE
        ! Cloud fraction or liquid+rain water content too low to
        ! consider this a cloud
        PRE_UM(JL,JK) = 1.

      ENDIF

    ENDDO
    
  ENDDO
  
  
CASE DEFAULT
  WRITE(NULERR,'(A,I0,A)') 'LIQUID EFFECTIVE RADIUS OPTION IRADLP=',IRADLP,' NOT AVAILABLE'
  CALL ABOR1('ERROR IN LIQUID_EFFECTIVE_RADIUS')
END SELECT

! -------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('LIQUID_EFFECTIVE_RADIUS',1,ZHOOK_HANDLE)
  
END SUBROUTINE LIQUID_EFFECTIVE_RADIUS
