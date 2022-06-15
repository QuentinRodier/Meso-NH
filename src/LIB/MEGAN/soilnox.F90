SUBROUTINE SOILNOX(KDATE, KTIME, OSOIL, KSLTYP, PRECADJ, &
                   PLAT, PTA, PSOILM, PSOILT, PLAIC, PCFNO, PCFNOG )

!***********************************************************************
!  DESCRIPTION:
!  
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions 
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Soil Moisture       (MCIP field) SOILM (M**3/M**3) (LSOIL)
!          (ratio of volume of water per volume of soil)
!     Soil Temperature    (MCIP field) SOILT (K)         (LSOIL)
!     Soil Type           (MCIP field) ISLTYP            (LSOIL)
!
!     saturation values for soil types (constants)       (LSOIL)
!     FOR PX Version, the Temperature adjustment factor accounts for wet and dry soils
!                and  the precipitation adjustment factor accounts for saturated soils
!     FOR the non-PX version, the basic algorithm remains with a temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!
!     The following arrays are updated after each call to SOILNOX
!     PULTYPE   type of NO emission pulse 
!     PULSEDATE julian date for the beginning of an NO pulse 
!     PULSETIME        time for the beginning of an NO pulse
!  
!     The calculation are based on the following paper by J.J. Yienger and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet and dry soils with
!       the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet and dry adjustment is 
!       calculated at each grid cell.  A linear interpolation between the wet and dry adjustment
!       factor is made using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture is determined by
!       taking the MCIP soil moisture field and dividing by the saturation value defined for each
!       soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total. 
!       THe types of Pulses as described in YL95 were used to estimate the NO emission
!       rate.  
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender       
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and Nitric Oxide Emissions from Agricultural Processes
!       Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!        Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
!     NO emission pulse type, soil moisture from previous time step, julian date
!     of NO emission pulse start, time of NO emission pulse start,
!     soil type, SOIL TYPES, Land use data
!
!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     FERTILIZER_ADJ computes fertlizer adjustment factor
!     VEG_ADJ        computes vegatation adjustment factor
!     GROWSEASON     computes day of growing season
!     
!  REVISION  HISTORY:
!    10/01 : Prototype by GAP
!    10/03 : modified transition to non growing season for jul-oct of the year
!    08/04 : Converted to SMOKE code style by C. Seppanen
!    07/21/11 : Imported form SMOKE-BEIS v3.14 for MEGAN v2.10
! 
!***********************************************************************

USE MODE_SOILNOX

USE MODD_MEGAN

IMPLICIT NONE

!.........  ARGUMENTS and their descriptions
INTEGER, INTENT(IN) :: KDATE   !  current simulation date (YYYYDDD)
INTEGER, INTENT(IN) :: KTIME   !  current simulation time (HHMMSS)
LOGICAL, INTENT(IN) :: OSOIL              ! true: using PX version of MCIP
!
INTEGER, DIMENSION(:), INTENT(IN) :: KSLTYP      !  soil type
!
REAL, INTENT(IN) :: PRECADJ     !  precip adjustment
!
REAL, DIMENSION(:), INTENT(IN)    :: PLAT        !  Latitude
REAL, DIMENSION(:), INTENT(IN)    :: PTA         !  air temperature (K)
REAL, DIMENSION(:), INTENT(IN)    :: PSOILM      !  soil moisture (m3/m3)
REAL, DIMENSION(:), INTENT(IN)    :: PSOILT      !  soil temperature (K)
REAL, DIMENSION(:), INTENT(IN)    :: PLAIC       !  soil temperature (K)
REAL, DIMENSION(:), INTENT(INOUT) :: PCFNO       !  NO correction factor
REAL, DIMENSION(:), INTENT(INOUT) :: PCFNOG      !  NO correction factor for grass

!.......  Local ARRAYS
! Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
!       PLEIM-XIU LAND-SURFACE AND PBL MODEL (PX-LSM)
! See JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.

!.........  SCRATCH LOCAL VARIABLES and their descriptions:
REAL, DIMENSION(SIZE(PLAT)) :: ZCF           ! NO correction factor
REAL :: ZTAIR         ! surface temperature
REAL :: ZTSOI         ! soil temperature
REAL :: ZCFNOWET, ZCFNODRY, ZRATIO

INTEGER :: JJ, JL      ! counters
INTEGER :: ISOILCAT      ! soil category

!HARACTER(256)  MESG         ! message buffer

!HARACTER(16) :: PROGNAME = 'SOILNOX'   !  program name

!***********************************************************************

!.....  Loop through cells
DO JJ = 1,SIZE(PTA)

  ZTAIR = MIN(PTA(JJ),303.)        ! unit in degree K

  IF ( ZTAIR>268.8690 ) THEN  
    PCFNOG(JJ) = EXP( 0.04686 * ZTAIR - 14.30579 ) ! grass (from BEIS2)
  ELSE
    PCFNOG(JJ) = 0.0
  END IF

!.......  CFNO
  IF( .NOT.OSOIL ) THEN
    ZTSOI = 0.72 * ZTAIR + 82.28
  ELSE
    ZTSOI = PSOILT(JJ)
  ENDIF

  ZTSOI = MIN(MAX(ZTSOI,273.16),303.16)
  ZCFNODRY = (1./3.) * (1./30.) * (ZTSOI-273.16)  ! see YL 1995 Equa 9a p. 11452
  IF ( ZTSOI<=283.16 ) THEN         ! linear cold case
    ZCFNOWET = (ZTSOI-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
  ELSE                             ! exponential case
    ZCFNOWET = EXP(0.103 * (ZTSOI-273.16)) * EXP(-0.103 * 30.0)
  END IF

  IF( .NOT.OSOIL ) THEN

    ZCF(JJ) = 0.5 * ZCFNOWET + 0.5 * ZCFNODRY

  ELSE

    ! soil
    ISOILCAT = KSLTYP(JJ)
    IF( ISOILCAT>0 .AND. ISOILCAT<=NMAXSTYPES ) THEN
      ZRATIO = PSOILM(JJ) / XSATURATION(ISOILCAT)
      ZCF(JJ) = ZRATIO * ZCFNOWET + (1.-ZRATIO) * ZCFNODRY
    ELSE
      ZCF(JJ) = 0.
    END IF
    
  END IF  ! Endif LSOIL

ENDDO

PCFNO(:) = ZCF(:) * FERTLZ_ADJ(KDATE,PLAT) * VEG_ADJ(PLAIC) * PRECADJ

!******************  FORMAT  STATEMENTS   ******************************

END SUBROUTINE SOILNOX

