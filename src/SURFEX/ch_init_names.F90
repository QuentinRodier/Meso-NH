!     #########
SUBROUTINE CH_INIT_NAMES (KLUOUT,HSV,KBEQ,KBAER,HSVO,&
                            KSV_CHSBEG,KSV_CHSEND, KSV_AERBEG, KSV_AEREND,&
                            OVARSIGI, OVARSIGJ)  
!!    ###########################################
!!
!!*** *CH_INIT_NAMES*
!!
!!    PURPOSE
!!    -------
!!      Read and filter all chemical species into the CSV array
!!     initialize NSV_CHSBEG and  NSV_CHSEND index for the begin and the ending chemical index
!!     
!!
!!    REFERENCE
!!    ---------
!!    
!!    AUTHOR
!!    ------
!!    P. Tulet    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 16/10/01
!!    01/12/03    (D.Gazen) change emissions handling for surf. externalization
!!    01/06/05    (P.Tulet) add aerosols list
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_CHS_AEROSOL
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!

INTEGER,                         INTENT(IN)  :: KLUOUT ! output listing channel
CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN)  :: HSV    ! name of chemical species
                                                       ! with character # (gas chemistry )
                                                       ! and  character @ (aerosols)
INTEGER,                         INTENT(OUT) :: KBEQ     ! number of chemical variables
INTEGER,                         INTENT(OUT) :: KBAER    ! number of aerosol variables
CHARACTER(LEN=*), DIMENSION(SIZE(HSV)),  INTENT(OUT) :: HSVO ! name of scalar species without # and @
INTEGER,                         INTENT(OUT) :: KSV_CHSBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_CHSEND  ! last  chemical var.
INTEGER,                         INTENT(OUT) :: KSV_AERBEG  ! first aerosol var.
INTEGER,                         INTENT(OUT) :: KSV_AEREND  ! last  aerosol var.
LOGICAL,                         INTENT(OUT) :: OVARSIGI, OVARSIGJ ! type of standard deviation
!
!*      0.2    declarations of local variables
INTEGER :: JSV  !! loop  NBEQ
CHARACTER        :: YRC1
CHARACTER(LEN=5) :: YRC2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CH_INIT_NAMES',0,ZHOOK_HANDLE)
KBEQ = 0
KBAER = 0
KSV_CHSBEG = 0
KSV_AERBEG = 0
KSV_CHSEND = 0
KSV_AEREND = 0
OVARSIGI = .FALSE.
OVARSIGJ = .FALSE.
NSOA = 0


DO JSV=1, SIZE(HSV)

  HSVO(JSV) = HSV(JSV)
  YRC1= HSV(JSV)(1:1)
  YRC2 = HSV(JSV)(2:)

  IF (YRC1 == '#') THEN
    HSVO(JSV) = TRIM(YRC2)
    KBEQ = KBEQ + 1
    IF (KBEQ == 1) KSV_CHSBEG=JSV
  ELSE IF (YRC1 == '@') THEN
    HSVO(JSV) = TRIM(YRC2)
    KBAER = KBAER + 1
    IF (KBAER == 1) KSV_AERBEG=JSV
    IF (HSVO(JSV) == "M6I") OVARSIGI = .TRUE.
    IF (HSVO(JSV) == "M6J") OVARSIGJ = .TRUE.
    IF (HSVO(JSV) == "SOA1I") NSOA = 10
  ENDIF

ENDDO

KSV_CHSEND = KSV_CHSBEG + KBEQ -1
KSV_AEREND = KSV_AERBEG + KBAER -1

IF (KBAER .GT. 0) THEN
DO JSV=1, size(HSVO)
   IF (TRIM(HSVO(JSV)) == "M0I") JP_CH_M0i=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "M0J") JP_CH_M0j=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "M6I") JP_CH_M6i=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "M6J") JP_CH_M6j=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "H2OI") JP_CH_H2Oi=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "H2OJ") JP_CH_H2Oj=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "SO4I") JP_CH_SO4i=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "SO4J") JP_CH_SO4j=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "NO3I") JP_CH_NO3i=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "NO3J") JP_CH_NO3j=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "NH3I") JP_CH_NH3i=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "NH3J") JP_CH_NH3j=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "OCI") JP_CH_OCi=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "OCJ") JP_CH_OCj=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "BCI") JP_CH_BCi=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "BCJ") JP_CH_BCj=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "DSTI") JP_CH_DSTi=JSV-KSV_CHSEND
   IF (TRIM(HSVO(JSV)) == "DSTJ") JP_CH_DSTj=JSV-KSV_CHSEND
END DO

END IF
IF (LHOOK) CALL DR_HOOK('CH_INIT_NAMES',1,ZHOOK_HANDLE)

!
END SUBROUTINE CH_INIT_NAMES
