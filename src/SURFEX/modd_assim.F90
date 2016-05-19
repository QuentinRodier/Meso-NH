!     ##################
      MODULE MODD_ASSIM
!     ##################
!
!!****  *MODD_ASSIM - declaration of keys for assimilation schemes
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	L. Jarlan   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       23/02/05
! 
!       Inclusion of OI constants 21/05/09 (J.-F. Mahfouf)  
!!       Add all assim keys         04/2012  T.Aspelien
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE

!-------------------------------------------------------------------------------
!
! Assimilation Scheme Options:
!
 LOGICAL                               :: LASSIM               ! Assimilation or not
                                                               !'.TRUE.'
                                                               !'.FALSE.'
 LOGICAL                               :: LPRINT               ! Verbose
 LOGICAL                               :: LAROME               ! If reading AROME fields
 LOGICAL                               :: LECSST               ! Use ECMWF SST
 LOGICAL                               :: LAESST               ! SST analysis performed
 LOGICAL                               :: LAESNM               ! Update snow analysis
 LOGICAL                               :: LALADSURF            
 LOGICAL                               :: LREAD_SST_FROM_FILE  ! Read SST from file
 LOGICAL                               :: LWATERTG2            ! Use deep soil temperature as lake temp.
 LOGICAL                               :: LEXTRAP_SEA          ! Extrapolation of sea points
 LOGICAL                               :: LEXTRAP_WATER        ! Extrapolation of inland water  points
 LOGICAL                               :: LEXTRAP_NATURE       ! Extrapolation of nature points
 CHARACTER(LEN=5)                      :: CASSIM_ISBA          ! OI/EKF
 CHARACTER(LEN=5)                      :: CASSIM               ! type of correction
                                                               ! 'PLUS ' (default)
                                                               ! 'AVERA'            
                                                               ! '2DVAR'
 LOGICAL                               :: LPRT                 ! Running VARASSIM in a perturbation mode
 LOGICAL                               :: LSIM                 ! Running VARASSIM in a reading mode
 LOGICAL                               :: LBEV                 ! Running VARASSIM to evolve B
 LOGICAL                               :: LBFIXED
 INTEGER                               :: NOBSTYPE
 INTEGER, PARAMETER                    :: NOBSMAX = 3
 REAL,DIMENSION(NOBSMAX)               :: YERROBS              ! Observational standard deviation
 INTEGER,DIMENSION(NOBSMAX)            :: INCO                 ! Select the type of observations to be assimilated
 INTEGER                               :: IVAR                 ! counter for ctnrl vars
 INTEGER                               :: NVAR                 ! number of cntrl vars
 INTEGER, PARAMETER                    :: NVARMAX = 4
 REAL,DIMENSION(NVARMAX)               :: TPRT_M               ! The perturbation amplitude (max dim)
 REAL,DIMENSION(NVARMAX)               :: XSIGMA_M             ! covariance of background errors if B is fixed (max dim)
!                                                              ! covariance of model errors if B evolving (max dim)
 CHARACTER(LEN=3),DIMENSION(NVARMAX)   :: XVAR_M ! X is ctrl   ! Name of control variables (syntax of surfex in PREP.txt file ) (max dim)
 CHARACTER(LEN=100),DIMENSION(NVARMAX) :: PREFIX_M             ! The prefix of the control variables (in PREP.txt file) (max dim)
 INTEGER,DIMENSION(NVARMAX)            :: INCV                 ! Select the control variables to be used
 REAL                                  :: SCALE_Q              ! scaling factor of Q matrix w.r.t. the initial B
!
! Constants and options of the soil OI analysis
!
 LOGICAL ::  LHUMID,  LIMVEG, LISSEW,   L_SM_WP, LFGEL,      LCLIM,   LLDHMT,  &
             LOBSWG,  LOBS2M 
 INTEGER ::  MINDJ,   NNEBUL, NNEIGT,   NNEIGW,  NR_SM_WP,   NECHGU,  NTVGLA,  &
             NSEAICE, NLISSEW,          IDJ,     ITRAD 
 REAL    ::  ANEBUL,  RCLIMN, RCLIMTP,  RCLIMTS, RCLIMV,     RCLIMWP, RCLIMWS, &
             SCOEFH,  SCOEFT, SEVAP,    SIGH2MO, SIGT2MO,    SNEIGT,  SNEIGW,  &
             SPRECIP, SWFC,   V10MX,    RD1,     RTINER,     WCRIN,   WPMX,    &
             WSMX,    TMERGL, RZHZ0G,   RCLIMCA, RCLISST,    RWPIA,   RWPIB,   &
             RSNSA,   RSNSB,  SALBM,    SALBB,   SEMIB,      SZZ0B,   SMU0,    &
             SICE,    SEMIM,  RA_SM_WP, RSCALDW, SPRECIP2,                     &
             REPSM,   RCDTR,  SIGHP1,   SIGT2MR, SIGH2MR,    RSABR,            &
             RARGR,   GWFC,   EWFC,     GWWILT,  EWWILT,     G1WSAT,  G2WSAT,  &
             REPS1,   REPS2,  REPS3,    ADWR,    SODELX(0:9),                  &
             SIGWGO,  SIGWGB, SIGW2B,   RTHR_QC, SIGWGO_MAX, RSCAL_JAC
!
END MODULE MODD_ASSIM
