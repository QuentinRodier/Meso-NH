!=======================================================================
!     MODULE SOILNOX_FX
!
!     This module contain functions to assist soil NOx calculation.
!     
!
!     CONTAINS: 1)FERTLZ_ADJ
!               2)VEG_ADJ
!               3)GROWSEASON
!
!     Note:
!
!     Requirement:
!
!
!     Imported from SMOKE-BEIS v3.14 and modified
!          by Tan 07/21/11 for MEGAN v2.10
!
!     Function PRECADJ is moved to MET2MGN
!              PULSETYPE is moved to MET2MGN
!              PRECIPFAC is moved to MET2MGN
!
!     History:
!
!=======================================================================

MODULE MODE_SOILNOX

USE MODI_JULIAN

IMPLICIT NONE

!...  Program I/O parameters

!...  External parameters

CONTAINS

!=======================================================================
!=======================================================================
FUNCTION FERTLZ_ADJ(KDATE, PLAT) RESULT(PFERTLZ_ADJ)

!***********************************************************************
!  DESCRIPTION:
!    This internal function computes a fertilizer adjustment factor
!    for the given date in yyyyddd format. If it is not growing 
!    season, the adjustment factor is 0; otherwise, it ranges from
!    0.0 to 1.0.
!
!  CALL:
!    GROWSEASON
!
!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!***********************************************************************

IMPLICIT NONE
      
!.... Function arguments
INTEGER, INTENT(IN) :: KDATE
REAL, DIMENSION(:), INTENT(IN) :: PLAT
REAL, DIMENSION(SIZE(PLAT)) :: PFERTLZ_ADJ

!.... Local variables
INTEGER, DIMENSION(SIZE(PLAT)) :: IDAY, ILEN

!-----------------------------------------------------------------------------

CALL GROWSEASON(KDATE, PLAT, IDAY, ILEN)

IF (ANY(IDAY(:)<0).OR.ANY(IDAY(:)>366)) THEN
  WRITE(*,*) "MODE_SOILNOX: FERTLZ_ADJ: Invalid date specified"
  STOP
ENDIF

WHERE ( IDAY(:)==0 )
  PFERTLZ_ADJ(:) = 0.
ELSE WHERE( IDAY(:)>=1 .AND. IDAY(:)<30 )
  ! first month of growing season
  PFERTLZ_ADJ(:) = 1.
ELSE WHERE( IDAY(:)>=30 .AND. IDAY(:)<=366 )
  ! later month of growing season
  PFERTLZ_ADJ(:) = 1. + (30.-FLOAT(IDAY(:)))/(FLOAT(ILEN(:)))
END WHERE

END FUNCTION FERTLZ_ADJ
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================
FUNCTION VEG_ADJ(PLAI) RESULT(PVEG_ADJ)

!***********************************************************************
!  DESCRIPTION
!    This internal function computes a vegetation adjustment factor
!    based on LAIv.  See Yienger and Levy 1995
!    VEG_ADJ = (EXP(-0.24*LAIv)+EXP(-0.0525*LAIv))*0.5 
!
!  CALL
!    NONE
!
!  HISTORY:
!***********************************************************************

IMPLICIT NONE

!...  Function arguments
REAL, DIMENSION(:), INTENT(IN) :: PLAI
!
REAL, DIMENSION(SIZE(PLAI)) :: PVEG_ADJ
!
!-----------------------------------------------------------------------------

PVEG_ADJ = (EXP(-0.24*PLAI)+EXP(-0.0525*PLAI))*0.5 

!******************  FORMAT  STATEMENTS   ******************************

END FUNCTION VEG_ADJ
!=======================================================================
!=======================================================================
      

!=======================================================================
!=======================================================================
SUBROUTINE GROWSEASON(KDATE, PLAT, KDAY, KLEN)

!***********************************************************************
!  DESCRIPTION
!    This internal function computes the day of the growing season
!    corresponding to the given date in yyyyddd format.
!
!  CALL
!    JULIAN
!
!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!               Variation of growing season depends on latitude
!               (Guenther)
!***********************************************************************

IMPLICIT NONE

!.......  Function arguments
INTEGER, INTENT(IN) :: KDATE
REAL, DIMENSION(:), INTENT(IN) :: PLAT
!
INTEGER, DIMENSION(:), INTENT(OUT) :: KDAY
INTEGER, DIMENSION(:), INTENT(OUT) :: KLEN

!.......  Local parameters
INTEGER   :: ISEASON_START
INTEGER   :: ISEASON_END

!.......  Local variables
INTEGER, DIMENSION(SIZE(PLAT)) :: ISJULIAN_START, ISJULIAN_END
INTEGER :: ISJULIAN_START0, ISJULIAN_START1, ISJULIAN_START2
INTEGER :: ISJULIAN_END1, ISJULIAN_END2
INTEGER :: IYEAR, IDAY, IDAY_ADD
!
!-----------------------------------------------------------------------------

IYEAR = INT(KDATE/1000.)
IDAY = KDATE - IYEAR*1000.

IF( IDAY.LT.1 .OR. IDAY.GT.366 ) THEN
  WRITE(*,*) "MODE_SOILNOX: GROWSEASON: Invalid date specified"
  STOP 
ENDIF
    
ISJULIAN_START1 = G2J(IYEAR, 0101)
ISJULIAN_END1   = G2J(IYEAR, 0531)
ISJULIAN_START2 = G2J(IYEAR, 1101)
ISJULIAN_END2   = G2J(IYEAR, 1231)

IF ( IDAY.GE.1101 .AND. IDAY.LE.1231 ) THEN
  ISJULIAN_START0 = ISJULIAN_START2
  IDAY_ADD = 0
ELSE IF ( IDAY.GE.0101 .AND. IDAY.LE.0531 ) THEN
  ISJULIAN_START0 = ISJULIAN_START1
  IDAY_ADD = 61
ELSE
  ISJULIAN_START0 = IDAY
  IDAY_ADD = -1
ENDIF

WHERE ( PLAT(:).LT.-60. .OR. PLAT(:).GT.65. )

  ! antarctic start = 0 end = 0, no growing
  KDAY(:) = 0
  KLEN(:) = 0

ELSE WHERE ( PLAT(:).LE.23. .AND. PLAT(:).GE.-23. ) 

  ! tropical regions, year round
  KDAY(:) = IDAY - ISJULIAN_START1 + 1
  KLEN(:) = ISJULIAN_END2 - ISJULIAN_START1 + 1

ELSE WHERE ( PLAT(:).LT.-23. )

! southern hemisphere
    KDAY(:) = IDAY - ISJULIAN_START0 + 1 + IDAY_ADD
    KLEN(:) = 30 + 31 + ISJULIAN_END1 - ISJULIAN_START1 + 1

ELSE WHERE ( PLAT.GT.23. )

  ! northern hemisphere temperate
  ! start= (lat-23)*4.5            189
  ! end = 365 -((lat-23)*3.3)      226

  ISJULIAN_START(:) = INT( (PLAT(:)-23.0)*4.5 )
  ISJULIAN_END  (:) = ISJULIAN_END2 - INT( (PLAT(:)-23.0)*3.3 )

  WHERE ( IDAY.GE.ISJULIAN_START(:) .AND. IDAY.LE.ISJULIAN_END(:) )
    KDAY(:) = IDAY - ISJULIAN_START(:) + 1
  ELSE WHERE
    KDAY(:) = 0
  END WHERE
  KLEN(:) = ISJULIAN_END(:) - ISJULIAN_START(:) + 1

END WHERE

!******************  FORMAT  STATEMENTS   ******************************

END SUBROUTINE GROWSEASON
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================
FUNCTION G2J(KYYYY, KMMDD) RESULT(KG2J)

IMPLICIT NONE

!.......  Function arguments
INTEGER, INTENT(IN) :: KYYYY
INTEGER, INTENT(IN) :: KMMDD

INTEGER :: KG2J

!.......  Local parameters
INTEGER :: IMM
INTEGER :: IDD

IMM  = INT(KMMDD/100.)
IDD  = KMMDD - IMM*100
KG2J = JULIAN(KYYYY, IMM, IDD)

END FUNCTION G2J

!=======================================================================
!=======================================================================
END MODULE MODE_SOILNOX
