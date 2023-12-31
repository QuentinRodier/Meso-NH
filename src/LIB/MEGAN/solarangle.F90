!-----------------------------------------------------------------------
!   SUBROUTINE: SOLARANGLE
!
!   DESCRIPTION: TO CALCULATE THE SOLAR ZENITH ANGLE.  THIS WILL GIVE
!                SIN(BETA), NOT THE BETA.
!
!   CALL: NONE
!
!   REQUIRE: NONE
!
!   INPUT:
!            1) DAY OF YEAR
!            2) LATITUDE
!            3) HOUR
!
!   OUTPUT: CALCBETA (SOLAR ZENITH ANGLE)
!
!   CREATED BY TAN 11/15/06  (BASED ON XXXX'S PROGRAM)
!
!-----------------------------------------------------------------------
SUBROUTINE SOLARANGLE(KDAY, PSHOUR, PLAT, PSINBETA)

USE MODD_MEGAN

IMPLICIT NONE

! INPUT
INTEGER, DIMENSION(:), INTENT(IN) :: KDAY     ! DOY OR JULIAN DAY
REAL, DIMENSION(:), INTENT(IN) :: PSHOUR      ! SOLAR HOUR
REAL, DIMENSION(:), INTENT(IN) :: PLAT        ! LATITUDE
! OUTPUT
REAL, DIMENSION(:), INTENT(OUT) :: PSINBETA
! LOCAL
!REAL    :: ZBETA                 ! SOLAR ELEVATION ANGLE
REAL    :: ZSINDELTA, ZCOSDELTA, ZA, ZB
! CONSTANTS
INTEGER :: JJ

! CALCULATION
DO JJ = 1,SIZE(KDAY)

  ZSINDELTA = -SIN(0.40907) * COS( 6.28*(KDAY(JJ)+10.)/365. )
  ZCOSDELTA = (1.-ZSINDELTA**2)**0.5

  ZA = SIN( PLAT(JJ) / XRPI180 ) * ZSINDELTA
  ZB = COS( PLAT(JJ) / XRPI180 ) * ZCOSDELTA

  PSINBETA(JJ) = ZA + ZB * COS( 2 * XPI * (PSHOUR(JJ)-12.)/24. )  ! THIS WILL BE TRANSFERED
                                           ! TO GAMMA_P FUNCTION
  !ZBETA = ASIN(PSINBETA(JJ)) * XRPI180    ! THIS IS NOT USED.

ENDDO

END SUBROUTINE SOLARANGLE
!-----------------------------------------------------------------------

