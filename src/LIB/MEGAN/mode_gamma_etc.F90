!=======================================================================
!     MODULE GAMMA
!
!     THIS MODULE CONTAIN FUNCTIONS TO CALCULATE
!     GAMMA_P, GAMMA_T, GAMMA_L, GAMMA_A FOR BVOCS.
!
!     CONTAINS: 1)GAMMA_LAI
!               2)GAMMA_P
!               3)GAMMA_TLD
!               4)GAMMA_TLI
!               5)GAMMA_A
!               6)GAMMA_S
!               7)GAMMA_CO2
!               8)GAMMA_LAIBIDIR
!
!     NOTE:
!
!     REQUIREMENT:
!
!     CALLS: SOLARANGLE
!
!     CREATED BY TAN 11/21/06 FOR MEGAN V2.0
!
!     HISTORY:
!     08/01/07 GUENTHER A. - MOVE TO MEGANV2.02 WITH MODIFICATION TO
!                            CORRECT CALCULATION OF GAMMA_P
!
!=======================================================================

MODULE MODE_GAMMA_ETC
!
USE MODD_MEGAN
!
!USE MODI_SOLARANGLE
USE MODI_INDEX1
!
IMPLICIT NONE

!...  PROGRAM I/O PARAMETERS

!...  EXTERNAL PARAMETERS

CONTAINS
!***********************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     SCIENTIFIC ALGORITHM
!
!             EMISSION = [EF][GAMMA][RHO]
!           WHERE [EF]    = EMISSION FACTOR (UG/M2H)
!                 [GAMMA] = EMISSION ACTIVITY FACTOR (NON-DIMENSION)
!                 [RHO]   = PRODUCTION AND LOSS WITHIN PLANT CANOPIES
!                           (NON-DIMENSINO)
!                 ASSUMPTION: [RHO] = 1 (11/27/06) (SEE PDT_LOT_CP.EXT)
!
!             GAMMA  = [GAMMA_CE][GAMMA_AGE][GAMMA_SM]
!           WHERE [GAMMA_CE]  = CANOPY CORRECTION FACTOR
!                 [GAMMA_AGE] = LEAF AGE CORRECTION FACTOR
!                 [GAMMA_SM]  = SOIL MOISTURE CORRECTION FACTOR
!                 ASSUMPTION: [GAMMA_SM]  = 1 (11/27/06)
!
!             GAMMA_CE = [GAMMA_LAI][GAMMA_P][GAMMA_T]
!           WHERE [GAMMA_LAI] = LEAF AREA INDEX FACTOR
!                 [GAMMA_P]   = PPFD EMISSION ACTIVITY FACTOR
!                 [GAMMA_T]   = TEMPERATURE RESPONSE FACTOR
!
!             EMISSION = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_AGE][GAMMA_SM]
!        DERIVATION:
!             EMISSION = [EF][GAMMA_ETC](1-LDF) + [EF][GAMMA_ETC][LDF][GAMMA_P]
!             EMISSION = [EF][GAMMA_ETC]{ (1-LDF) + [LDF][GAMMA_P] }
!             EMISSION = [EF][GAMMA_ECT]{ (1-LDF) + [LDF][GAMMA_P] }
!           WHERE LDF = LIGHT DEPENDENT FUNCTION (NON-DIMENSION)
!
!     FOR ISOPRENE
!                 ASSUMPTION: LDF = 1 FOR ISOPRENE            (11/27/06)
!
!        FINAL EQUATION
!             EMISSION = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_AGE][GAMMA_SM]
!
!     FOR NON-ISOPRENE
!        FINAL EQUATION
!             EMISSION = [EF][GAMMA_LAI][GAMMA_T][GAMMA_AGE][GAMMA_SM]*
!                        { (1-LDF) + [LDF][GAMMA_P] }
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!=======================================================================
!...  BEGIN MODULE
!=======================================================================


!-----------------------------------------------------------------------
!.....1) CALCULATE GAM_L (GAMMA_LAI)
!-----------------------------------------------------------------------
!                            0.49[LAI]
!             GAMMA_LAI = ----------------    (NON-DIMENSION)
!                         (1+0.2LAI^2)^0.5
!
!     SUBROUTINE GAMMA_LAI RETURNS THE GAMMA_LAI VALUES
!-----------------------------------------------------------------------
SUBROUTINE GAMMA_LAI(PLAI, PGAM_L)

IMPLICIT NONE
! INPUT
REAL,DIMENSION(:),INTENT(IN) :: PLAI
! OUTPUT
REAL,DIMENSION(:),INTENT(OUT) :: PGAM_L

PGAM_L(:) = (0.49*PLAI(:)) / ( (1.+0.2*(PLAI(:)**2))**0.5 )

END SUBROUTINE GAMMA_LAI
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!.....5) CALCULATE GAM_A (GAMMA_AGE)
!-----------------------------------------------------------------------
!
!             GAMMA_AGE = FNEW*ANEW + FGRO*AGRO + FMAT*AMAT + FOLD*AOLD
!           WHERE FNEW = NEW FOLIAGE FRACTION
!                 FGRO = GROWING FOLIAGE FRACTION
!                 FMAT = MATURE FOLIAGE FRACTION
!                 FOLD = OLD FOLIAGE FRACTION
!                 ANEW = RELATIVE EMISSION ACTIVITY FOR NEW FOLIAGE
!                 AGRO = RELATIVE EMISSION ACTIVITY FOR GROWING FOLIAGE
!                 AMAT = RELATIVE EMISSION ACTIVITY FOR MATURE FOLIAGE
!                 AOLD = RELATIVE EMISSION ACTIVITY FOR OLD FOLIAGE
!
!
!             FOR FOLIAGE FRACTION
!             CASE 1) LAIC = LAIP
!             FNEW = 0.0  , FGRO = 0.1  , FMAT = 0.8  , FOLD = 0.1
!
!             CASE 2) LAIP > LAIC
!             FNEW = 0.0  , FGRO = 0.0
!             FMAT = 1-FOLD
!             FOLD = (LAIP-LAIC)/LAIP
!
!             CASE 3) LAIP < LAIC
!             FNEW = 1-(LAIP/LAIC)                       T <= TI
!                  = (TI/T) * ( 1-(LAIP/LAIC) )          T >  TI
!
!             FMAT = LAIP/LAIC                           T <= TM
!                  = (LAIP/LAIC) +
!                      ( (T-TM)/T ) * ( 1-(LAIP/LAIC) )  T >  TM
!
!             FGRO = 1 - FNEW - FMAT
!             FOLD = 0.0
!
!           WHERE
!             TI = 5 + (0.7*(300-TT))                   TT <= 303
!                = 2.9                                  TT >  303
!             TM = 2.3*TI
!
!             T  = LENGTH OF THE TIME STEP (DAYS)
!             TI = NUMBER OF DAYS BETWEEN BUDBREAK AND THE INDUCTION OF
!                  EMISSION
!             TM = NUMBER OF DAYS BETWEEN BUDBREAK AND THE INITIATION OF
!                  PEAK EMISSIONS RATES
!             TT = AVERAGE TEMPERATURE (K) NEAR TOP OF THE CANOPY DURING
!                  CURRENT TIME PERIOD (DAILY AVE TEMP FOR THIS CASE)
!
!
!             FOR RELATIVE EMISSION ACTIVITY
!             CASE 1) CONSTANT
!             ANEW = 1.0  , AGRO = 1.0  , AMAT = 1.0  , AOLD = 1.0
!
!             CASE 2) MONOTERPENES
!             ANEW = 2.0  , AGRO = 1.8  , AMAT = 0.95 , AOLD = 1.0
!
!             CASE 3) SESQUITERPENES
!             ANEW = 0.4  , AGRO = 0.6  , AMAT = 1.075, AOLD = 1.0
!
!             CASE 4) METHANOL
!             ANEW = 3.0  , AGRO = 2.6  , AMAT = 0.85 , AOLD = 1.0
!
!             CASE 5) ISOPRENE
!             ANEW = 0.05 , AGRO = 0.6  , AMAT = 1.125, AOLD = 1.0
!
!     SUBROUTINE GAMMA_A RETURNS GAMMA_A
!-----------------------------------------------------------------------
SUBROUTINE GAMMA_A(KDATE, KTIME, KTSTLEN, HSPC_NAME, PTEMP_D, PLAIARP, PLAIARC, PGAM_A)

IMPLICIT NONE

! INPUT
INTEGER, INTENT(IN) :: KDATE, KTIME, KTSTLEN
CHARACTER(LEN=16), INTENT(IN) :: HSPC_NAME
REAL, DIMENSION(:), INTENT(IN) :: PTEMP_D
REAL, DIMENSION(:), INTENT(IN) :: PLAIARP, PLAIARC
! OUTPUT
REAL,DIMENSION(:),INTENT(OUT) :: PGAM_A

! LOCAL PARAMETERS
REAL :: ZFNEW, ZFGRO, ZFMAT, ZFOLD
REAL :: ZTI, ZTM        ! NUMBER OF DAYS BETWEEN BUDBREAK
                               ! AND INDUCTION OF EMISSION, 
                               ! INITIATION OF PEAK EMISSIONS RATES
INTEGER :: IAINDX         ! RELATIVE EMISSION ACITIVITY INDEX
INTEGER :: ISPCNUM
INTEGER :: JJ

!...  CHOOSE RELATIVE EMISSION ACTIVITY
!--------CODE BY XUEMEI WANG 11/04/2007----------------  
!
ISPCNUM = INDEX1(HSPC_NAME, CMGN_SPC)
IAINDX = NREA_INDEX(ISPCNUM)
!
!---------------------------------------------------        
! LOCAL PARAMETER ARRAYS
DO JJ = 1,SIZE(PLAIARP)
  IF ( PTEMP_D(JJ).LE.303. ) THEN 
    ZTI = 5.0 + 0.7*(300.-PTEMP_D(JJ))
  ELSE 
    ZTI = 2.9
  ENDIF
    ZTM = 2.3 * ZTI
!


!... CALCULATE FOLIAGE FRACTION

!      PRINT*,'LAIP,LAIC, TT=',MINVAL(LAIP), MAXVAL(LAIP),
!     S       MINVAL(LAIC), MAXVAL(LAIC), MINVAL(TT), MAXVAL(TT)

!      WHERE (LAIP .LT. LAIC) 

!        CALCULATE TI AND TM
  IF ( PLAIARP(JJ).EQ.PLAIARC(JJ) ) THEN

    ZFNEW = 0.0
    ZFGRO = 0.1
    ZFMAT = 0.8
    ZFOLD = 0.1

  ELSEIF ( PLAIARP(JJ).GT.PLAIARC(JJ) ) THEN

    ZFNEW = 0.0
    ZFGRO = 0.0
    ZFOLD = ( PLAIARP(JJ)-PLAIARC(JJ) ) / PLAIARP(JJ)
    ZFMAT = 1. - ZFOLD
    
  ELSE

    ZFMAT = PLAIARP(JJ)/PLAIARC(JJ)
    !       CALCULATE FNEW AND FMAT, THEN FGRO AND FOLD
    !       FNEW
    IF ( ZTI.GE.KTSTLEN ) THEN
      ZFNEW = 1.0 - ZFMAT
    ELSE 
      ZFNEW = (ZTI/KTSTLEN) * ( 1. - ZFMAT )
    ENDIF
!       FMAT
    IF ( ZTM.LT.KTSTLEN ) THEN
      ZFMAT = ZFMAT + ( (KTSTLEN-ZTM)/KTSTLEN ) * ( 1.-ZFMAT )
    ENDIF
    
    ZFGRO = 1.0 - ZFNEW - ZFMAT
    ZFOLD = 0.0
  
  ENDIF

    !...  CALCULATE GAMMA_A
  PGAM_A(JJ) = ZFNEW * XANEW(IAINDX) + ZFGRO * XAGRO(IAINDX) +  &
               ZFMAT * XAMAT(IAINDX) + ZFOLD * XAOLD(IAINDX)

ENDDO

END SUBROUTINE GAMMA_A

!-----------------------------------------------------------------------
!.....6) CALCULATE GAM_SMT (GAMMA_SM)
!-----------------------------------------------------------------------
!
!             GAMMA_SM =     1.0   (NON-DIMENSION)
!
!
!     SUBROUTINE GAMMA_S RETURNS THE GAMMA_SM VALUES
!-----------------------------------------------------------------------
SUBROUTINE GAMMA_S( PGAM_S )

IMPLICIT NONE

REAL,DIMENSION(:) :: PGAM_S

PGAM_S = 1.0

END SUBROUTINE GAMMA_S

!-----------------------------------------------------------------------
!.....2) CALCULATE GAM_P (GAMMA_P)
!-----------------------------------------------------------------------
!             GAMMA_P = 0.0         A<=0, A>=180, SIN(A) <= 0.0
!
!             GAMMA_P = SIN(A)[ 2.46*(1+0.0005(PDAILY-400))*PHI - 0.9*PHI^2 ]
!                                   0<A<180, SIN(A) > 0.0
!           WHERE PHI    = ABOVE CANOPY PPFD TRANSMISSION (NON-DIMENSION)
!                 PDAILY = DAILY AVERAGE ABOVE CANOPY PPFD (UMOL/M2S)
!                 A      = SOLAR ANGLE (DEGREE)
!
!                 NOTE: AAA = 2.46*BBB*PHI - 0.9*PHI^2
!                       BBB = (1+0.0005(PDAILY-400))
!                       GAMMA_P = SIN(A)*AAA
!
!                       PAC
!             PHI = -----------
!                   SIN(A)*PTOA
!           WHERE PAC  = ABOVE CANOPY PPFD (UMOL/M2S)
!                 PTOA = PPFD AT THE TOP OF ATMOSPHERE (UMOL/M2S)
!
!             PAC =  SRAD * 4.766 MMMOL/M2-S * 0.5
!
!             PTOA = 3000 + 99*COS[2*3.14-( DOY-10)/365 )]
!           WHERE DOY = DAY OF YEAR
!
!     SUBROUTINE GAMMA_P RETURNS THE GAMMA_P VALUES
!-----------------------------------------------------------------------
!SUBROUTINE GAMMA_P( KDATE, KTIME, PLAT, PLONG, PPFD, PPFD_D, PGAM_P )
!
!IMPLICIT NONE
!
!! INPUT
!INTEGER,INTENT(IN) :: KDATE, KTIME
!
!REAL,DIMENSION(:),INTENT(IN) :: PLAT, PLONG   
!! PHOTOSYNTHETIC PHOTON FLUX DENSITY: INSTANTANEOUS, DAILY
!REAL,DIMENSION(:),INTENT(IN) :: PPFD, PPFD_D   
!! OUTPUT
!REAL,DIMENSION(:),INTENT(OUT) :: PGAM_P          ! GAMMA_P
!
!! LOCAL PARAMETERS
!REAL, DIMENSION(SIZE(PLAT)) :: ZHOUR, ZSINBETA      ! HOUR IS SOLAR HOUR
!INTEGER, DIMENSION(SIZE(PLAT)) :: IDAY    ! DAY IS DOY (JDATE)
!
!REAL :: ZPTOA, ZPHI
!REAL :: ZAAA, ZBBB
!REAL :: ZBETA                           ! SOLAR ZENITH ANGLE
!INTEGER :: JJ
!
!!...  BEGIN ESTIMATING GAMMA_P
!
!!...  CONVERT DATE AND TIME FORMAT TO LOCAL TIME
!! DAY IS JULIAN DAY
!IDAY(:)  =  MOD(KDATE,1000)
!
!! CONVERT FROM XXXXXX FORMAT TO XX.XX (SOLAR HOUR)
!! HOUR = 0 -> 23.XX
!! SOLAR HOUR
!ZHOUR(:) = KTIME/10000. + PLONG(:)/15.
!     
!WHERE ( ZHOUR(:).LT.0. ) 
!  ZHOUR(:) = ZHOUR(:) + 24.0
!  IDAY(:) = IDAY(:) - 1.
!ENDWHERE
!
!! GET SOLAR ELEVATION ANGLE
!CALL SOLARANGLE(IDAY, ZHOUR, PLAT, ZSINBETA)
!
!DO JJ = 1,SIZE(ZSINBETA)
!
!  IF ( ZSINBETA(JJ).LE.0. ) THEN
!
!    PGAM_P(JJ) = 0.
!      
!  ELSE IF ( ZSINBETA(JJ).GT.0. ) THEN
!
!    ZPTOA = 3000.0 + 99.0 *COS(2. * 3.14 * (IDAY(JJ)-10.)/365.)
! 
!    ZPHI = PPFD(JJ) / (ZSINBETA(JJ) * ZPTOA)
! 
!    ZBBB = 1. + 0.0005 * (PPFD_D(JJ)-400. )
!    ZAAA = ( 2.46 * ZBBB * ZPHI ) - ( 0.9 * ZPHI**2 )
! 
!    PGAM_P(JJ) = ZSINBETA(JJ) * ZAAA
!
!    ZBETA = ASIN(ZSINBETA(JJ)) * XRPI180           ! DEGREE
!
!      ! SCREENING THE UNFORCED ERRORS
!      ! IF SOLAR ELEVATION ANGLE IS LESS THAN 1 THEN
!      ! GAMMA_P CAN NOT BE GREATER THAN 0.1.
!    IF ( ZBETA.LT.1.0 .AND. PGAM_P(JJ).GT.0.1 ) THEN
!      PGAM_P(JJ) = 0.0
!    ENDIF
!
!  ELSE
!            
!    WRITE(*,*) "ERROR: SOLAR ANGLE IS INVALID - FATAL ERROR GAMMA_P, STOP"
!    STOP
!
!  ENDIF 
!         ! END LOOP FOR NROWS
!ENDDO      ! END LOOP FOR NCOLS
!
!END SUBROUTINE GAMMA_P
!!-----------------------------------------------------------------------
!
!
!!-----------------------------------------------------------------------
!!.....3) CALCULATE GAM_T (GAMMA_T) FOR ISOPRENE
!!-----------------------------------------------------------------------
!!                          EOPT*CT2*EXP(CT1*X)
!!             GAMMA_T =  ------------------------
!!                        [CT2-CT1*(1-EXP(CT2*X))]
!!           WHERE X      = [ (1/TOPT)-(1/THR) ] / 0.00831
!!                 EOPT   = 1.75*EXP(0.08(TDAILY-297)
!!                 CT1    = 80
!!                 CT2    = 200
!!                 THR    = HOURLY AVERAGE AIR TEMPERATURE (K)
!!                 TDAILY = DAILY AVERAGE AIR TEMPERATURE (K)
!!                 TOPT   = 313 + 0.6(TDAILY-297)
!!
!!                 NOTE: AAA = EOPT*CT2*EXP(CT1*X)
!!                       BBB = [CT2-CT1*(1-EXP(CT2*X))]
!!                       GAMMA_T = AAA/BBB
!!
!!     SUBROUTINE GAMMA_TLD RETURNS THE GAMMA_T VALUE FOR ISOPRENE
!!-----------------------------------------------------------------------
!SUBROUTINE GAMMA_TLD( PTEMP, PTEMP_D, PGAM_T, HSPC_NAME )
!
!IMPLICIT NONE
!
!! INPUT
!REAL,DIMENSION(:),INTENT(IN) :: PTEMP, PTEMP_D   ! DAILY, HOURLY SURFACE TEMPERATURE 
!! OUTPUT
!REAL,DIMENSION(:),INTENT(OUT) :: PGAM_T       ! GAMMA_T
!CHARACTER(LEN=16),INTENT(IN) :: HSPC_NAME
!!
!! LOCAL PARAMETERS
!REAL :: ZEOPT, ZTOPT, ZX, ZAAA, ZBBB
!INTEGER :: ISPCNUM, JJ
!  
!ISPCNUM = INDEX1(HSPC_NAME, CMGN_SPC)
!
!DO JJ = 1,SIZE(PTEMP)
!
!  ZEOPT = XCLEO(ISPCNUM) * EXP(0.08*(PTEMP_D(JJ)-297.))
!  ZTOPT = 313.0 + ( 0.6*(PTEMP_D(JJ)-297.) )
!  ZX = ( (1/ZTOPT)-(1/PTEMP(JJ)) ) / 0.00831
!
!  ZAAA = ZEOPT * XCT2 * EXP(XCTM1(ISPCNUM)*ZX)
!  ZBBB = ( XCT2- XCTM1(ISPCNUM)*( 1.-EXP(XCT2*ZX) ) )
!  PGAM_T(JJ) = ZAAA/ZBBB
!
!ENDDO
!
!END SUBROUTINE GAMMA_TLD
!!-----------------------------------------------------------------------
!
!
!!-----------------------------------------------------------------------
!!.....4) CALCULATE GAM_T (GAMMA_T) FOR NON-ISOPRENE
!!-----------------------------------------------------------------------
!!
!!             GAMMA_T =  EXP[TDP_FCT*(T-TS)]
!!           WHERE TDP_FCT = TEMPERATURE DEPENDENT PARAMETER ('BETA')
!!                 TS     = STANDARD TEMPERATURE (NORMALLY 303K, 30C)
!!
!!     SUBROUTINE GAMMA_TLI RETURNS THE GAMMA_T VALUE FOR NON-ISOPRENE
!!-----------------------------------------------------------------------
!SUBROUTINE GAMMA_TLI(HSPCNAM, PTEMP, PGAM_T)
!
!IMPLICIT NONE
!
!CHARACTER(LEN=16), INTENT(IN) :: HSPCNAM
!REAL,DIMENSION(:), INTENT(IN):: PTEMP
!REAL, DIMENSION(:), INTENT(OUT) :: PGAM_T
!!
!INTEGER :: ISPCNUM                             ! SPECIES NUMBER
!
!!--END OF DECLARATIONS--
!
!ISPCNUM = INDEX1(HSPCNAM, CMGN_SPC)
!!
!PGAM_T = EXP( XTDF_PRM(ISPCNUM) * (PTEMP-XTS) )
!
!END SUBROUTINE GAMMA_TLI
!!-----------------------------------------------------------------------
!
!!=======================================================================
!!-----------------------------------------------------------------------
!!.....7) CALCULATE GAM_CO2(GAMMA_CO2)
!!-----------------------------------------------------------------------
!!
!!             GAMMA_CO2 =     1.0   (NON-DIMENSION)
!!             WHEN CO2 =400PPM
!!
!!     SUBROUTINE GAM_CO2 RETURNS THE GAMMA_CO2 VALUES
!!    XUEMEI WANG-2009-06-22 
!!-----------------------------------------------------------------------
!SUBROUTINE GAMMA_CO2(PCO2, PGAM_CO2)
!
!IMPLICIT NONE
!
!REAL, DIMENSION(:), INTENT(IN) :: PCO2
!REAL, DIMENSION(:), INTENT(OUT) :: PGAM_CO2
!
!REAL :: ZCI
!INTEGER :: JJ
!
!DO JJ = 1,SIZE(PCO2)
!
!  IF ( PCO2(JJ).EQ.400. ) THEN
!    PGAM_CO2(JJ) = 1.0
!  ELSE
!    ZCI = 0.7* PCO2(JJ)
!    PGAM_CO2(JJ) = XISMAX - ((XISMAX*ZCI**XH) /(XCSTAR**XH+ZCI**XH))
!  ENDIF
!
!ENDDO
!
!END SUBROUTINE GAMMA_CO2
!    
!!=======================================================================
!!=======================================================================
!!-----------------------------------------------------------------------
!!.....8) CALCULATE GAMMA_LAIBIDIR(GAM_LAIBIDIR,LAI)
!!-----------------------------------------------------------------------
!!FROM ALEX GUENTHER 2010-01-26
!!IF LAI < 2 THEN
!!GAMMALAIBIDIR= 0.5 * LAI
!!ELSEIF LAI <= 6 THEN
!!GAMMALAIBIDIR= 1 - 0.0625 * (LAI - 2)
!!ELSE
!!GAMMALAIBIDIR= 0.75
!!END IF
!!
!!     SUBROUTINE GAMMA_LAIBIDIR RETURNS THE GAM_LAIBIDIR VALUES
!!    XUEMEI WANG-2010-01-28
!!
!!-----------------------------------------------------------------------
!SUBROUTINE GAMMA_LAIBIDIR(PLAI, PGAM_LAIBIDIR)
!
!IMPLICIT NONE
!
!REAL,DIMENSION(:),INTENT(IN) ::  PLAI
!REAL,DIMENSION(:),INTENT(OUT) :: PGAM_LAIBIDIR
!
!INTEGER :: JJ
!!
!DO JJ = 1,SIZE(PLAI)
!
!  IF ( PLAI(JJ)<2. ) THEN
!    PGAM_LAIBIDIR(JJ) = 0.5 * PLAI(JJ)
!  ELSEIF ( PLAI(JJ).GE.2. .AND. PLAI(JJ).LE.6. ) THEN 
!    PGAM_LAIBIDIR(JJ) = 1. - 0.0625 * ( PLAI(JJ)-2. )
!  ELSE
!    PGAM_LAIBIDIR(JJ) = 0.75
!  ENDIF
!    
!ENDDO
!
!END  SUBROUTINE GAMMA_LAIBIDIR
!!=======================================================================
!
END MODULE MODE_GAMMA_ETC
