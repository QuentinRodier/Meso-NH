!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:18
!-----------------------------------------------------------------
!    ########################
     MODULE MODI_RADIATIONS   
!    ########################
!
INTERFACE 
!
    SUBROUTINE RADIATIONS (OCLOSE_OUT,HFMFILE,HLUOUT,OCLEAR_SKY,OCLOUD_ONLY,&
               KCLEARCOL_TM1,HEFRADL,HEFRADI,HOPWSW,HOPISW,HOPWLW,HOPILW,   &
               PFUDG, KDLON, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB,KSTATM, &
               KRAD_COLNBR,PCOSZEN,PSEA, PCORSOL,                         &
               PDIR_ALB, PSCA_ALB, PEMIS, PCLDFR, PCCO2, PTSRAD, PSTATM,  &
               PTHT, PRT, PPABST, POZON, PAER,PDST_WL, PAER_CLIM, PSVT,   &
               PDTHRAD, PSRFLWD, PSRFSWD_DIR,PSRFSWD_DIF,PRHODREF, PZZ,   &
               PRADEFF, PSWU, PSWD, PLWU, PLWD, PDTHRADSW, PDTHRADLW      )
!
LOGICAL, INTENT(IN)                  :: OCLOSE_OUT! flag indicating that a FM
                                                  ! file is opened during this 
                                                  ! time-step
CHARACTER(LEN=*), INTENT(IN)         :: HFMFILE   ! Name of the output
                                                  ! FM-file
CHARACTER(LEN=*), INTENT(IN)         :: HLUOUT    ! Output-listing name for
                                                  ! model n
LOGICAL, INTENT(IN)                  :: OCLOUD_ONLY! flag for the cloud column
                                                   !    computations only
LOGICAL, INTENT(IN)                  :: OCLEAR_SKY ! 
INTEGER, INTENT(IN)                  :: KDLON   ! number of columns where the
                                                ! radiation calculations are
                                                !         performed
INTEGER, INTENT(IN)                  :: KFLEV   ! number of vertical levels
                                                !    where the radiation
                                                ! calculations are performed
INTEGER, INTENT(IN)                  :: KRAD_DIAG   ! index for the number of
                                                    !  fields in the output
INTEGER, INTENT(IN)                  :: KFLUX   ! number of top and ground 
                                                ! fluxes for the ZFLUX array
INTEGER, INTENT(IN)                  :: KRAD    ! number of satellite radiances
                                                ! for the ZRAD and ZRADCS arrays
INTEGER, INTENT(IN)                  :: KAER    ! number of AERosol classes

INTEGER, INTENT(IN)                  :: KSWB    ! number of SW band  
INTEGER, INTENT(IN)                  :: KSTATM  ! index of the standard 
                                                ! atmosphere level just above
                                                !      the model top
INTEGER, INTENT(IN)                  :: KRAD_COLNBR ! factor by which the memory
                                                    ! is splitted
!
                                               !Choice of :             
CHARACTER (LEN=*), INTENT (IN)       :: HEFRADL!cloud liquid effective radius calculation
CHARACTER (LEN=*), INTENT (IN)       :: HEFRADI!cloud ice effective radius calculation
CHARACTER (LEN=*), INTENT (IN)       :: HOPWSW !cloud water SW optical properties   
CHARACTER (LEN=*), INTENT (IN)       :: HOPISW !ice water SW optical properties 
CHARACTER (LEN=*), INTENT (IN)       :: HOPWLW !cloud water LW optical properties
CHARACTER (LEN=*), INTENT (IN)       :: HOPILW !ice water  LW optical properties
REAL,              INTENT(IN)        :: PFUDG  ! subgrid cloud inhomogenity factor
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN ! COS(zenithal solar angle)
REAL,                     INTENT(IN) :: PCORSOL ! SOLar constant CORrection
REAL, DIMENSION(:,:),     INTENT(IN) :: PSEA    ! Land-sea mask
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIR_ALB! Surface direct ALBedo
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PSCA_ALB! Surface diffuse ALBedo
REAL, DIMENSION(:,:),     INTENT(IN) :: PEMIS   ! Surface IR EMISsivity
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! CLouD FRaction
REAL,                     INTENT(IN) :: PCCO2   ! CO2 content
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD  ! RADiative Surface Temperature
REAL, DIMENSION(:,:),     INTENT(IN) :: PSTATM  ! selected standard atmosphere
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT    ! THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT     ! moist variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST  ! pressure at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT    ! scalar variable ( C2R2 and C1R3  particle) 
!
REAL, DIMENSION(:,:,:),   POINTER    :: POZON   ! OZON field from clim.
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER    ! AERosols optical thickness from clim. 
REAL, DIMENSION(:,:,:,:), POINTER    :: PDST_WL ! AERosols Extinction.by wavelength 
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER_CLIM    ! AERosols optical thickness from clim.                                                 ! note : the vertical dimension of 
                                                ! these fields include the "radiation levels"
                                                ! above domain top 
!
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PRHODREF ![kg/m3] air density
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ      ![m] height of layers
!
INTEGER, DIMENSION(:,:), INTENT(INOUT)  :: KCLEARCOL_TM1 ! trace of cloud/clear col
                                                         ! at the previous radiation step
!                                                 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDTHRAD ! THeta RADiative Tendancy
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PSRFLWD ! Downward SuRFace LW Flux
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PSRFSWD_DIR ! Downward SuRFace SW Flux DIRect 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PSRFSWD_DIF ! Downward SuRFace SW Flux DIFfuse 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PSWU ! upward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PSWD ! downward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PLWU ! upward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PLWD ! downward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PDTHRADSW ! dthrad sw 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PDTHRADLW !  dthradsw
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PRADEFF ! effective radius
!
!
END SUBROUTINE RADIATIONS
!
END INTERFACE
!
END MODULE MODI_RADIATIONS  
!
!   #######################################################################
    SUBROUTINE RADIATIONS (OCLOSE_OUT,HFMFILE,HLUOUT,OCLEAR_SKY,OCLOUD_ONLY,&
               KCLEARCOL_TM1,HEFRADL,HEFRADI,HOPWSW,HOPISW,HOPWLW,HOPILW,   &
               PFUDG, KDLON, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB,KSTATM, &
               KRAD_COLNBR,PCOSZEN,PSEA, PCORSOL,                         &
               PDIR_ALB, PSCA_ALB,PEMIS, PCLDFR, PCCO2, PTSRAD, PSTATM,   &
               PTHT, PRT, PPABST, POZON, PAER, PDST_WL, PAER_CLIM, PSVT,  &
               PDTHRAD, PSRFLWD, PSRFSWD_DIR,PSRFSWD_DIF, PRHODREF, PZZ,  &
               PRADEFF, PSWU, PSWD, PLWU,PLWD, PDTHRADSW, PDTHRADLW       )
!   #######################################################################
!
!!****  *RADIATIONS * - routine to call the SW and LW radiation calculations
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to prepare the temperature, water vapor
!!    liquid water, cloud fraction, ozone profiles for the ECMWF radiation
!!    calculations. There is a great number of available radiative fluxes in
!!    the output, but only the potential temperature radiative tendency and the
!!    SW and LW surface fluxes are provided in the output of the routine.
!!    Two simplified computations are available (switches OCLEAR_SKY and
!!    OCLOUD_ONLY). When OCLOUD_ONLY is .TRUE. the computations are performed
!!    for the cloudy columns only. Furthermore with OCLEAR_SKY being .TRUE.
!!    the clear sky columns are averaged and the computations are made for
!!    the cloudy columns plus a single ensemble-mean clear sky column.
!!
!!**  METHOD
!!    ------
!!      First the temperature, water vapor, liquid water, cloud fraction
!!    and  profile arrays are built using the current model fields and
!!    the standard atmosphere for the upper layer filling.
!!    The standard atmosphere is used between the levels IKUP and
!!    KFLEV where KFLEV is the number of vertical levels for the radiation 
!!    computations.    
!!    The aerosols optical thickness and the ozone fields come directly
!!    from ini_radiation step (climatlogies used) and are already defined for KFLEV. 
!!    Surface parameter ( albedo, emiss ) are also defined from current surface fields.
!!    In the case of clear-sky or cloud-only approximations, the cloudy
!!    columns are selected by testing the vertically integrated cloud fraction
!!    and the radiation computations are performed for these columns plus the
!!    mean clear-sky one. In addition, columns where cloud have disapeared are determined
!!    by saving cloud trace between radiation step and they are also recalculated
!!    in cloud only step. In all case, the sun position correponds to  the centered
!!    time between 2 full radiation steps (determined in physparam).
!!      Then the ECMWF radiation package is called and the radiative
!!    heating/cooling tendancies are reformatted in case of partial
!!    computations.  In case of "cloud-only approximation" the only cloudy
!!    column radiative fields are updated.
!!
!!    EXTERNAL
!!    --------
!!      Subroutine ECMWF_RADIATION_VERS2 : ECMWF interface calling radiation routines
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_TIME : structure of TDTCUR
!!      Module MODD_CST  : constants
!!        XP00 : reference pressure
!!        XCPD : calorific capacity of dry air at constant pressure
!!        XRD  : gas constant for dry air
!!      Module MODD_PARAMETERS : parameters
!!        JPHEXT : Extra columns on the horizontal boundaries
!!        JPVEXT : Extra levels on the vertical boundaries
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine RADIATIONS )
!!
!!    AUTHOR
!!    ------
!!	J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/02/95 
!!      J.Stein     20/12/95 add the array splitting in order to save memory
!!      J.-P. Pinty 19/11/96 change the splitted arrays, specific humidity
!!                           and add the ice phase
!!      J.Stein     22/06/97 use of the absolute pressure
!!      P.Jabouille 31/07/97 impose a zero humidity for dry simulation
!!      V.Masson    22/09/97 case of clear-sky approx. with no clear-sky column
!!      V.Masson    07/11/97 half level pressure defined from averaged Exner
!!                           function
!!      V.Masson    07/11/97 modification of junction between standard atm
!!                           and model for half level variables (top model
!!                           pressure and temperatures are used preferentially
!!                           to atm standard profile for the first point).
!!      P.Jabouille 24/08/98 impose positivity for ZQLAVE
!!      J.-P. Pinty 29/01/98 add storage for diagnostics
!!      J. Stein    18/07/99 add the ORAD_DIAG switch and keep inside the
!!                           subroutine the partial tendencies 
!!
!!      F.Solmon    04/03/01  MAJOR MODIFICATIONS, updated version of ECMWF radiation scheme
!!      P.Jabouille 05/05/03 bug in humidity conversion
!!      Y.Seity     25/08/03  KSWB=6 for SW direct and scattered surface 
!!                            downward fluxes used in surface scheme. 
!!      P. Tulet    01/20/05  climatologic SSA
!!      A. Grini    05/20/05  dust direct effect (optical properties)
!!      V.Masson, C.Lac 08/10 Correction of inversion of Diffuse and direct albedo
!!      B.Aouizerats 2010     Explicit aerosol optical properties
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_FMWRIT
USE MODE_FM
USE MODE_ll
USE MODI_ECMWF_RADIATION_VERS2
USE YOESW    , ONLY : RTAUA    ,RPIZA    ,RCGA

!
USE MODD_TIME
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_RAIN_ICE_DESCR
USE MODD_NSV, ONLY : NSV_C2R2,NSV_C2R2BEG,NSV_C2R2END, &
                     NSV_C1R3,NSV_C1R3BEG,NSV_C1R3END, &
                     NSV_DSTBEG, NSV_DSTEND, &
                     NSV_AERBEG, NSV_AEREND, &
                     NSV_SLTBEG, NSV_SLTEND
!
USE MODE_THERMO

USE MODD_DUST, ONLY: LDUST
USE MODD_SALT, ONLY: LSALT
USE MODD_CH_AEROSOL, ONLY: LORILAM
USE MODD_PARAM_RAD_n, ONLY: CAOP,LRAD_DUST         
USE MODE_DUSTOPT
USE MODE_SALTOPT
USE MODI_AEROOPT_GET
!
#ifdef MNH_PGI
USE MODE_PACK_PGI
#endif
!  
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
LOGICAL, INTENT(IN)                  :: OCLOSE_OUT! flag indicating that a FM
                                                  ! file is opened during this 
                                                  ! time-step
CHARACTER(LEN=*), INTENT(IN)         :: HFMFILE   ! Name of the output
                                                  ! FM-file
CHARACTER(LEN=*), INTENT(IN)         :: HLUOUT    ! Output-listing name for
                                                  ! model n
LOGICAL, INTENT(IN)                  :: OCLOUD_ONLY! flag for the cloud column
                                                   !    computations only
LOGICAL, INTENT(IN)                  :: OCLEAR_SKY ! 
INTEGER, INTENT(IN)                  :: KDLON   ! number of columns where the
                                                ! radiation calculations are
                                                !       performed
INTEGER, INTENT(IN)                  :: KFLEV   ! number of vertical levels
                                                !    where the radiation
                                                ! calculations are performed
INTEGER, INTENT(IN)                  :: KRAD_DIAG  ! index for the number of
                                                   !  fields in the output
INTEGER, INTENT(IN)                  :: KFLUX   ! number of top and ground 
                                                ! fluxes for the ZFLUX array
INTEGER, INTENT(IN)                  :: KRAD    ! number of satellite radiances
                                                ! for the ZRAD and ZRADCS arrays
INTEGER, INTENT(IN)                  :: KAER    ! number of AERosol classes

INTEGER, INTENT(IN)                  :: KSWB    ! number of SW band  
INTEGER, INTENT(IN)                  :: KSTATM  ! index of the standard 
                                                ! atmosphere level just above
                                                !      the model top
INTEGER, INTENT(IN)                  :: KRAD_COLNBR ! factor by which the memory
                                                    ! is splitted
                                                    !
                                               !Choice of :             
CHARACTER (LEN=*), INTENT (IN)       :: HEFRADL ! 
CHARACTER (LEN=*), INTENT (IN)       :: HEFRADI ! 
CHARACTER (LEN=*), INTENT (IN)       :: HOPWSW !cloud water SW optical properties   
CHARACTER (LEN=*), INTENT (IN)       :: HOPISW !ice water SW optical properties 
CHARACTER (LEN=*), INTENT (IN)       :: HOPWLW !cloud water LW optical properties
CHARACTER (LEN=*), INTENT (IN)       :: HOPILW !ice water  LW optical properties
REAL,               INTENT(IN)       :: PFUDG  ! subgrid cloud inhomogenity factor
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN ! COS(zenithal solar angle)
REAL,                     INTENT(IN) :: PCORSOL ! SOLar constant CORrection
REAL, DIMENSION(:,:),     INTENT(IN) :: PSEA    ! Land-sea mask
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIR_ALB! Surface direct ALBedo
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PSCA_ALB! Surface diffuse ALBedo
REAL, DIMENSION(:,:),     INTENT(IN) :: PEMIS   ! Surface IR EMISsivity
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! CLouD FRaction
REAL,                     INTENT(IN) :: PCCO2   ! CO2 content
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD  ! RADiative Surface Temperature
REAL, DIMENSION(:,:),     INTENT(IN) :: PSTATM  ! selected standard atmosphere
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT    ! THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT     ! moist variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST  ! pressure at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT    ! scalar variable ( C2R2 and C1R3  particle)
!
REAL, DIMENSION(:,:,:),   POINTER    :: POZON   ! OZONE field from clim.
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER    ! AERosols optical thickness from clim. 
REAL, DIMENSION(:,:,:,:), POINTER    :: PDST_WL    ! AERosols Extinction by wavelength . 
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER_CLIM    ! AERosols optical thickness from clim.                                                 ! note : the vertical dimension of 
                                                ! these fields iclude the "radiation levels"
                                                !  above domain top.
                                                ! 
                                                 
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PRHODREF ![kg/m3] air density
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ      ![m] height of layers

INTEGER, DIMENSION(:,:), INTENT(INOUT)  :: KCLEARCOL_TM1 ! trace of cloud/clear col
                                                         ! at the previous radiation step
!                                                 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDTHRAD ! THeta RADiative Tendancy
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PSRFLWD ! Downward SuRFace LW Flux
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PSRFSWD_DIR ! Downward SuRFace SW Flux DIRect 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PSRFSWD_DIF ! Downward SuRFace SW Flux DIFfuse 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PSWU ! upward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PSWD ! downward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PLWU ! upward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PLWD ! downward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PDTHRADSW ! dthrad sw 
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PDTHRADLW !  dthradsw
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PRADEFF ! effective radius
!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!
LOGICAL                         :: GNOCL     ! .TRUE. when no cloud is present
                                             !     with OCLEAR_SKY .TRUE.
LOGICAL                         :: GAOP      ! .TRUE. when CAOP='EXPL'
LOGICAL, DIMENSION(KDLON,KFLEV) :: GCLOUD    ! .TRUE. for the cloudy columns
LOGICAL, DIMENSION(KFLEV,KDLON) :: GCLOUDT   ! transpose of the GCLOUD array
LOGICAL, DIMENSION(KDLON)       :: GCLEAR_2D ! .TRUE. for the clear-sky columns
LOGICAL, DIMENSION(KDLON,KFLEV) :: GCLEAR    ! .TRUE. for all the levels of the 
                                             !                clear-sky columns
LOGICAL, DIMENSION(KDLON,KSWB)  :: GCLEAR_SWB! .TRUE. for all the bands of the  
                                             !                clear-sky columns
INTEGER, DIMENSION(:), ALLOCATABLE :: ICLEAR_2D_TM1 !
!
INTEGER :: JI,JJ,JK,JK1,JK2,JKRAD,JALBS! loop indices
!
INTEGER :: IIB           ! I index value of the first inner mass point
INTEGER :: IJB           ! J index value of the first inner mass point
INTEGER :: IKB           ! K index value of the first inner mass point
INTEGER :: IIE           ! I index value of the last inner mass point
INTEGER :: IJE           ! J index value of the last inner mass point
INTEGER :: IKE           ! K index value of the last inner mass point
INTEGER :: IKU           ! array size for the third  index
INTEGER :: IIJ           ! reformatted array index
INTEGER :: IKSTAE        ! level number of the STAndard atmosphere array
INTEGER :: IKUP          ! vertical level above which STAndard atmosphere data
                         ! are filled in
!
INTEGER :: ICLEAR_COL    ! number of    clear-sky columns
INTEGER :: ICLOUD_COL    ! number of    cloudy    columns
INTEGER :: ICLOUD        ! number of levels corresponding of the cloudy columns
INTEGER :: IDIM          ! effective number of columns for which the radiation
                         ! code is run
INTEGER :: INIR          ! index corresponding to NIR fisrt band (in SW)
!
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZTAVE    ! mean-layer temperature
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZPAVE    ! mean-layer pressure
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQSAVE   ! saturation specific humidity
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQVAVE   ! mean-layer specific humidity
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQLAVE   ! Liquid water KG/KG
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQRAVE   ! Rain water  KG/KG
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQIAVE   ! Ice water Kg/KG
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQLWC   ! liquid water content kg/m3
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQRWC   ! Rain water  content kg/m3
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZQIWC   ! ice water content  kg/m3
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCFAVE   ! mean-layer cloud fraction
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZO3AVE   ! mean-layer ozone content 
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZPRES_HL ! half-level pressure
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZT_HL    ! half-level temperature
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZDPRES   ! layer pressure thickness
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCCT_C2R2! Cloud water Concentarion (C2R2)
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCRT_C2R2! Rain water Concentarion (C2R2)
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCIT_C1R3! Ice water Concentarion (C2R2)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZAER     ! aerosol optical thickness
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZALBP    ! spectral surface albedo for direct radiations
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZALBD    ! spectral surface albedo for diffuse radiations 
REAL, DIMENSION (:),  ALLOCATABLE   :: ZEMIS    ! surface LW  emissivity 
REAL, DIMENSION (:), ALLOCATABLE    :: ZEMIW    ! surface LW  WINDOW emissivity
REAL, DIMENSION(:), ALLOCATABLE     :: ZTS      ! reformatted surface PTSRAD array 
REAL, DIMENSION(:), ALLOCATABLE     :: ZLSM     ! reformatted land sea mask
REAL, DIMENSION(:),   ALLOCATABLE   :: ZRMU0    ! Reformatted ZMU0 array
REAL                                :: ZRII0    ! corrected solar constant
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDTLW    ! LW temperature tendency
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDTSW    ! SW temperature tendency
REAL, DIMENSION(:,:), ALLOCATABLE :: ZNFLW_CS ! CLEAR-SKY LW NET FLUXES
REAL, DIMENSION(:,:), ALLOCATABLE :: ZNFLW    ! TOTAL LW NET FLUXES
REAL, DIMENSION(:,:), ALLOCATABLE :: ZNFSW_CS ! CLEAR-SKY SW NET FLUXES
REAL, DIMENSION(:,:), ALLOCATABLE :: ZNFSW    ! TOTAL SW NET FLUXES
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFLUX_TOP_GND_IRVISNIR ! Top and 
                                                            ! Ground radiative FLUXes
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_DOWN ! DowNward SW Flux profiles
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_UP   ! UPward   SW Flux profiles
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFLUX_LW      !          LW Flux profiles
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTLW_CS ! LW Clear-Sky temp. tendency
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTSW_CS ! SW Clear-Sky temp. tendency
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_TOP_GND_IRVISNIR_CS ! Top and
                                                  !  Ground Clear-Sky radiative FLUXes
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZSFSWDIR !surface SW direct flux
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZSFSWDIF !surface SW diffuse flux

REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ALB_VIS, ZPLAN_ALB_NIR
                        ! PLANetary ALBedo in VISible, Near-InfraRed regions
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_TRA_VIS, ZPLAN_TRA_NIR
                        ! PLANetary TRANsmission in VISible, Near-InfraRed regions
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ABS_VIS, ZPLAN_ABS_NIR
                        ! PLANetary ABSorption in VISible, Near-InfraRed regions
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEFCL_LWD, ZEFCL_LWU
                        ! EFective  DOWNward and UPward LW nebulosity (equivalent emissivities)
                        ! undefined if RRTM is used for LW
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLWP, ZFIWP
                        ! Liquid and Ice Water Path
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRADLP, ZRADIP
                        ! Cloud liquid water and ice effective radius
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEFCL_RRTM, ZCLSW_TOTAL
                        ! effective LW nebulosity ( RRTM case) 
                        ! and SW CLoud fraction for mixed phase clouds
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTAU_TOTAL, ZOMEGA_TOTAL, ZCG_TOTAL
                        ! effective optical thickness, single scattering albedo
                        ! and asymetry factor for mixed phase clouds
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_DOWN_CS, ZFLUX_SW_UP_CS
                        ! Clear-Sky  DowNward and UPward   SW Flux profiles
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFLUX_LW_CS
                        ! Thicknes of the mesh
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDZ
!
REAL, DIMENSION(KDLON,KFLEV) :: ZZDTSW ! SW diabatic heating
REAL, DIMENSION(KDLON,KFLEV) :: ZZDTLW ! LW diabatic heating
REAL, DIMENSION(KDLON)       :: ZZTGVIS! SW surface flux in the VIS band
REAL, DIMENSION(KDLON)       :: ZZTGNIR! SW surface flux in the NIR band
REAL, DIMENSION(KDLON)       :: ZZTGIR ! LW surface flux in the IR bands
REAL, DIMENSION(KDLON,SIZE(PSRFSWD_DIR,3)) :: ZZSFSWDIR
!                                      ! SW direct surface flux   
REAL, DIMENSION(KDLON,SIZE(PSRFSWD_DIR,3)) :: ZZSFSWDIF
!                                      ! SW diffuse surface flux   
!
REAL, DIMENSION(KDLON)       :: ZCLOUD ! vertically integrated cloud fraction
!
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3)) :: ZEXNT ! Exner function
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2))    :: ZLWD    ! surface Downward LW flux
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PSRFSWD_DIR,3)) :: ZSWDDIR ! surface
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PSRFSWD_DIR,3)) :: ZSWDDIF ! surface Downward SW diffuse flux
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3),KSWB) :: ZPIZAZ ! Aerosols SSA
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3),KSWB) :: ZTAUAZ ! Aerosols Optical Detph
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3),KSWB) :: ZCGAZ  ! Aerosols Asymetric factor
REAL :: ZZTGVISC    ! downward surface SW flux (VIS band) for clear_sky
REAL :: ZZTGNIRC    ! downward surface SW flux (NIR band) for clear_sky
REAL :: ZZTGIRC     ! downward surface LW flux for clear_sky
REAL, DIMENSION(SIZE(PSRFSWD_DIR,3)) :: ZZSFSWDIRC
!                   ! downward surface SW direct flux for clear sky
REAL, DIMENSION(SIZE(PSRFSWD_DIR,3)) :: ZZSFSWDIFC
!                   ! downward surface SW diffuse flux for clear sky
REAL, DIMENSION(KFLEV) :: ZT_CLEAR  ! ensemble mean clear-sky temperature
REAL, DIMENSION(KFLEV) :: ZP_CLEAR  ! ensemble mean clear-sky temperature
REAL, DIMENSION(KFLEV) :: ZQV_CLEAR ! ensemble mean clear-sky specific humidity
REAL, DIMENSION(KFLEV) :: ZOZ_CLEAR ! ensemble mean clear-sky ozone
REAL, DIMENSION(KFLEV) :: ZHP_CLEAR ! ensemble mean clear-sky half-lev. pression
REAL, DIMENSION(KFLEV) :: ZHT_CLEAR ! ensemble mean clear-sky half-lev. temp.
REAL, DIMENSION(KFLEV) :: ZDP_CLEAR ! ensemble mean clear-sky pressure thickness
REAL, DIMENSION(KFLEV,KAER) :: ZAER_CLEAR  ! ensemble mean clear-sky aerosols optical thickness
REAL, DIMENSION(KSWB)       :: ZALBP_CLEAR ! ensemble mean clear-sky surface albedo (parallel)
REAL, DIMENSION(KSWB)       :: ZALBD_CLEAR ! ensemble mean clear-sky surface albedo (diffuse)
REAL                        :: ZEMIS_CLEAR ! ensemble mean clear-sky surface emissivity
REAL                        :: ZEMIW_CLEAR ! ensemble mean clear-sky LW window
REAL                        :: ZRMU0_CLEAR ! ensemble mean clear-sky MU0
REAL                        :: ZTS_CLEAR   ! ensemble mean clear-sky surface temperature.
REAL                        :: ZLSM_CLEAR  !  ensemble mean clear-sky land sea-mask  
!
!work arrays
REAL, DIMENSION(:),   ALLOCATABLE :: ZWORK1, ZWORK2, ZWORK3, ZWORK
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK4, ZWORK1AER, ZWORK2AER, ZWORK_GRID
LOGICAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2)) :: ZWORKL
!
!  splitted arrays used to split the memory required by the ECMWF_radiation 
!  subroutine, the fields have the same meaning as their complete counterpart
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZALBP_SPLIT, ZALBD_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZEMIS_SPLIT, ZEMIW_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZRMU0_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZCFAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZO3AVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZT_HL_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZPRES_HL_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZTAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZPAVE_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZAER_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDPRES_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZLSM_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQVAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQSAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQLAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQIAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQRAVE_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQRWC_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQLWC_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZQIWC_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDZ_SPLIT
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCCT_C2R2_SPLIT
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCRT_C2R2_SPLIT
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZCIT_C1R3_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZTS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZSFSWDIR_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZSFSWDIF_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZNFLW_CS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZNFLW_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZNFSW_CS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZNFSW_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTLW_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTSW_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_TOP_GND_IRVISNIR_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_DOWN_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_UP_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFLUX_LW_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTLW_CS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZDTSW_CS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_TOP_GND_IRVISNIR_CS_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ALB_VIS_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ALB_NIR_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_TRA_VIS_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_TRA_NIR_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ABS_VIS_SPLIT
REAL, DIMENSION(:),     ALLOCATABLE :: ZPLAN_ABS_NIR_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEFCL_LWD_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEFCL_LWU_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLWP_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFIWP_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRADLP_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZRADIP_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEFCL_RRTM_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZCLSW_TOTAL_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTAU_TOTAL_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZOMEGA_TOTAL_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZCG_TOTAL_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_DOWN_CS_SPLIT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFLUX_SW_UP_CS_SPLIT
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFLUX_LW_CS_SPLIT
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZPIZA_EQ_TMP        !Single scattering albedo of aerosols (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZIR        !Real part of the aerosol refractive index(lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZII        !Imaginary part of the aerosol refractive index (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZCGA_EQ_TMP         !Assymetry factor aerosols            (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTAUREL_EQ_TMP      !tau/tau_{550} aerosols               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZPIZA_DST_TMP        !Single scattering albedo of dust (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZCGA_DST_TMP         !Assymetry factor dust            (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTAUREL_DST_TMP      !tau/tau_{550} dust               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZPIZA_AER_TMP        !Single scattering albedo of aerosol from ORILAM (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZCGA_AER_TMP         !Assymetry factor aerosol from ORILAM            (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTAUREL_AER_TMP      !tau/tau_{550} aerosol from ORILAM               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZPIZA_SLT_TMP        !Single scattering albedo of sea salt (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZCGA_SLT_TMP         !Assymetry factor of sea salt            (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTAUREL_SLT_TMP      !tau/tau_{550} of sea salt               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: PAER_AER      !tau/tau_{550} aerosol from ORILAM               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: PAER_SLT      !tau/tau_{550} sea salt               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: PAER_DST     !tau/tau_{550} dust               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTAU550_EQ_TMP      !tau/tau_{550} aerosols               (lon,lat,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZPIZA_EQ            !Single scattering albedo of aerosols (points,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZCGA_EQ             !Assymetry factor aerosols            (points,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZTAUREL_EQ          !tau/tau_{550} aerosols               (points,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZPIZA_EQ_SPLIT      !Single scattering albedo of aerosols (points,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZCGA_EQ_SPLIT       !Assymetry factor aerosols            (points,lev,wvl)
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZTAUREL_EQ_SPLIT    !tau/tau_{550} aerosols               (points,lev,wvl)
REAL, DIMENSION(KFLEV,KSWB)           :: ZPIZA_EQ_CLEAR      !Single scattering albedo of aerosols (lev,wvl)
REAL, DIMENSION(KFLEV,KSWB)           :: ZCGA_EQ_CLEAR       !Assymetry factor aerosols            (lev,wvl)
REAL, DIMENSION(KFLEV,KSWB)           :: ZTAUREL_EQ_CLEAR    !tau/tau_{550} aerosols               (lev,wvl)
INTEGER                               :: WVL_IDX              !Counter for wavelength

!
INTEGER  :: JI_SPLIT          ! loop on the splitted array
INTEGER  :: INUM_CALL         ! number of CALL of the radiation scheme
INTEGER  :: IDIM_EFF          ! effective number of air-columns to compute
INTEGER  :: IDIM_RESIDUE      ! number of remaining air-columns to compute
INTEGER  :: IBEG, IEND        ! auxiliary indices
!
!
REAL, DIMENSION(SIZE(PDTHRAD,1),SIZE(PDTHRAD,2),SIZE(PDTHRAD,3)) &
     :: ZDTRAD_LW! LW temperature tendency
REAL, DIMENSION(SIZE(PDTHRAD,1),SIZE(PDTHRAD,2),SIZE(PDTHRAD,3)) &
     :: ZDTRAD_SW! SW temperature tendency
INTEGER             :: ILUOUT       ! Logical unit number for output-listing
INTEGER             :: IRESP        ! Return code of FM routines
INTEGER             :: IGRID        ! C-grid indicator in LFIFM file
INTEGER             :: ILENCH       ! Length of comment string in LFIFM file
CHARACTER (LEN=100) :: YCOMMENT     ! comment string in LFIFM file
CHARACTER (LEN=16)  :: YRECFM       ! Name of the desired field in LFIFM file
REAL, DIMENSION(SIZE(PDTHRAD,1),SIZE(PDTHRAD,2),SIZE(PDTHRAD,3)) &
     :: ZSTORE_3D, ZSTORE_3D2! 3D work array for storage
REAL, DIMENSION(SIZE(PDTHRAD,1),SIZE(PDTHRAD,2)) &
     :: ZSTORE_2D   ! 2D work array for storage!
INTEGER                         :: JBAND       ! Solar band index
CHARACTER (LEN=4), DIMENSION(KSWB) :: YBAND_NAME  ! Solar band name
CHARACTER (LEN=2)               :: YDIR        ! Type of the data field
!
INTEGER :: ISWB ! number of SW spectral bands (between radiations and surface schemes)
INTEGER :: JSWB ! loop on SW spectral bands
INTEGER :: JAE  ! loop on aerosol class
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES
!              ----------------------------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)  ! this definition must be coherent with
                                      ! the one used in ini_radiations routine
IKU = SIZE(PTHT,3)
IKB = 1 + JPVEXT
IKE = IKU - JPVEXT
!
IKSTAE = SIZE(PSTATM,1)
IKUP   = IKE-JPVEXT+1
! 
ISWB   = SIZE(PSRFSWD_DIR,3)
!
!-------------------------------------------------------------------------------
!
!*       2.    INITIALIZES THE MEAN-LAYER VARIABLES
!              ------------------------------------
!
ZEXNT(:,:,:)= ( PPABST(:,:,:)/XP00 ) ** (XRD/XCPD)
! 
ALLOCATE(ZTAVE(KDLON,KFLEV))
ALLOCATE(ZQVAVE(KDLON,KFLEV))
ALLOCATE(ZQLAVE(KDLON,KFLEV))
ALLOCATE(ZQIAVE(KDLON,KFLEV))
ALLOCATE(ZCFAVE(KDLON,KFLEV))
ALLOCATE(ZQRAVE(KDLON,KFLEV))
ALLOCATE(ZQLWC(KDLON,KFLEV))
ALLOCATE(ZQIWC(KDLON,KFLEV))
ALLOCATE(ZQRWC(KDLON,KFLEV))
ALLOCATE(ZDZ(KDLON,KFLEV))
!
ZQVAVE(:,:) = 0.0
ZQLAVE(:,:) = 0.0
ZQIAVE(:,:) = 0.0
ZQRAVE(:,:) = 0.0
ZCFAVE(:,:) = 0.0
ZQLWC(:,:) = 0.0
ZQIWC(:,:) = 0.0
ZQRWC(:,:) = 0.0
ZDZ(:,:)=0.0
!
!COMPUTE THE MESH SIZE
DO JK=IKB,IKE
  JKRAD = JK-JPVEXT
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      ZDZ(IIJ,JKRAD)  =  PZZ(JI,JJ,JK+1) - PZZ(JI,JJ,JK)
      ZTAVE(IIJ,JKRAD)  = PTHT(JI,JJ,JK)*ZEXNT(JI,JJ,JK)
    END DO
  END DO
END DO
!
!  Check if the humidity mixing ratio is available
!
IF( SIZE(PRT(:,:,:,:),4) >= 1 ) THEN
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZQVAVE(IIJ,JKRAD) =MAX(0., PRT(JI,JJ,JK,1))
      END DO
    END DO
  END DO
END IF
!
!  Check if the cloudwater mixing ratio is available
!
IF( SIZE(PRT(:,:,:,:),4) >= 2 ) THEN
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZQLAVE(IIJ,JKRAD) = MAX(0.,PRT(JI,JJ,JK,2))
        ZQLWC(IIJ,JKRAD) = MAX(0.,PRT(JI,JJ,JK,2)*PRHODREF(JI,JJ,JK))
        ZCFAVE(IIJ,JKRAD) = PCLDFR(JI,JJ,JK)
      END DO
    END DO
  END DO
END IF
!
!  Check if the rainwater mixing ratio is available
!
IF( SIZE(PRT(:,:,:,:),4) >= 3 ) THEN
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZQRWC(IIJ,JKRAD) = MAX(0.,PRT(JI,JJ,JK,3)*PRHODREF(JI,JJ,JK))
        ZQRAVE(IIJ,JKRAD) = MAX(0.,PRT(JI,JJ,JK,3))
      END DO
    END DO
  END DO
END IF
!
!  Check if the cloudice mixing ratio is available
!
IF( SIZE(PRT(:,:,:,:),4) >= 4 ) THEN
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZQIWC(IIJ,JKRAD) = MAX(0.,PRT(JI,JJ,JK,4)*PRHODREF(JI,JJ,JK))
        ZQIAVE(IIJ,JKRAD) = MAX( PRT(JI,JJ,JK,4)-XRTMIN(4),0.0 )
      END DO
    END DO
  END DO
END IF
!
!  Standard atmosphere extension
!
DO JK=IKUP,KFLEV
  JK1 = (KSTATM-1)+(JK-IKUP)
  JK2 = JK1+1
  ZTAVE(:,JK)  = 0.5*( PSTATM(JK1,3)+PSTATM(JK2,3) )
  ZQVAVE(:,JK) = 0.5*( PSTATM(JK1,5)/PSTATM(JK1,4)+   &
                 PSTATM(JK2,5)/PSTATM(JK2,4)    )
END DO
!
!        2.1 pronostic water concentation fields (C2R2 coupling) 
!
IF( NSV_C2R2 /= 0 ) THEN
  ALLOCATE (ZCCT_C2R2(KDLON, KFLEV))
  ALLOCATE (ZCRT_C2R2(KDLON, KFLEV))
  ZCCT_C2R2(:, :) = 0.
  ZCRT_C2R2 (:,:) = 0.
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZCCT_C2R2 (IIJ,JKRAD) = MAX(0.,PSVT(JI,JJ,JK,NSV_C2R2BEG+1))
        ZCRT_C2R2 (IIJ,JKRAD) = MAX(0.,PSVT(JI,JJ,JK,NSV_C2R2BEG+2))
      END DO
    END DO
  END DO
ELSE 
  ALLOCATE (ZCCT_C2R2(0,0))
  ALLOCATE (ZCRT_C2R2(0,0))
END IF
!
IF( NSV_C1R3 /= 0 ) THEN
  ALLOCATE (ZCIT_C1R3(KDLON, KFLEV))
  ZCIT_C1R3 (:,:) = 0.
  DO JK=IKB,IKE
    JKRAD = JK-JPVEXT
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZCIT_C1R3 (IIJ,JKRAD) = MAX(0.,PSVT(JI,JJ,JK,NSV_C1R3BEG))
      END DO
    END DO
  END DO
ELSE 
  ALLOCATE (ZCIT_C1R3(0,0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZES THE HALF-LEVEL VARIABLES
!  	           ------------------------------------
!
ALLOCATE(ZPRES_HL(KDLON,KFLEV+1))
ALLOCATE(ZT_HL(KDLON,KFLEV+1))
!
DO JK=IKB,IKE+1
  JKRAD = JK-JPVEXT
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      ZPRES_HL(IIJ,JKRAD) = XP00 * (0.5*(ZEXNT(JI,JJ,JK)+ZEXNT(JI,JJ,JK-1)))**(XCPD/XRD)
    END DO
  END DO
END DO
!
!  Standard atmosphere extension
!
!* begining at ikup+1 level allows to use a model domain higher than 50km
!
DO JK=IKUP+1,KFLEV+1
  JK1 = (KSTATM-1)+(JK-IKUP)
  ZPRES_HL(:,JK) = PSTATM(JK1,2)*100.0
END DO
!
!  Surface temperature at the first level
!  and surface radiative temperature
ALLOCATE(ZTS(KDLON))
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZT_HL(IIJ,1) = PTSRAD(JI,JJ)
    ZTS(IIJ) = PTSRAD(JI,JJ)
  END DO
END DO
!
!  Temperature at half levels
!
ZT_HL(:,2:IKE-JPVEXT) = 0.5*(ZTAVE(:,1:IKE-JPVEXT-1)+ZTAVE(:,2:IKE-JPVEXT))
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZT_HL(IIJ,IKE-JPVEXT+1)  =  0.5*PTHT(JI,JJ,IKE  )*ZEXNT(JI,JJ,IKE  ) &
         + 0.5*PTHT(JI,JJ,IKE+1)*ZEXNT(JI,JJ,IKE+1)
  END DO
END DO
!
!  Standard atmosphere extension
!
!* begining at ikup+1 level allows to use a model domain higher than 50km
!
DO JK=IKUP+1,KFLEV+1
  JK1 = (KSTATM-1)+(JK-IKUP)
  ZT_HL(:,JK) = PSTATM(JK1,3)
END DO
!
!mean layer pressure and layer differential pressure (from half level variables)
!
ALLOCATE(ZPAVE(KDLON,KFLEV))
ALLOCATE(ZDPRES(KDLON,KFLEV))
DO JKRAD=1,KFLEV
  ZPAVE(:,JKRAD)=0.5*(ZPRES_HL(:,JKRAD)+ZPRES_HL(:,JKRAD+1))
  ZDPRES(:,JKRAD)=ZPRES_HL(:,JKRAD)-ZPRES_HL(:,JKRAD+1)
END DO
!-----------------------------------------------------------------------
!*       4.    INITIALIZES THE AEROSOLS and OZONE PROFILES from climatlogy
!	           -------------------------------------------
!
!        4.1    AEROSOL optical thickness
!
IF (CAOP=='EXPL') THEN
   GAOP = .TRUE.
ELSE
   GAOP = .FALSE.
ENDIF
!
IF (CAOP=='EXPL') THEN
   ALLOCATE(ZPIZA_EQ_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZCGA_EQ_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZTAUREL_EQ_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))

   ALLOCATE(ZPIZA_DST_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZCGA_DST_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZTAUREL_DST_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB)) 
   ALLOCATE(PAER_DST(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3)))

   ALLOCATE(ZPIZA_AER_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZCGA_AER_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZTAUREL_AER_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(PAER_AER(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3)))

   ALLOCATE(ZPIZA_SLT_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZCGA_SLT_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(ZTAUREL_SLT_TMP(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3),KSWB))
   ALLOCATE(PAER_SLT(SIZE(PAER,1),SIZE(PAER,2),SIZE(PAER,3)))
   

   ALLOCATE(ZII(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),KSWB))
   ALLOCATE(ZIR(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),KSWB))

  ZPIZA_EQ_TMP = 0.
  ZCGA_EQ_TMP = 0.
  ZTAUREL_EQ_TMP = 0.

  ZPIZA_DST_TMP = 0.
  ZCGA_DST_TMP = 0.
  ZTAUREL_DST_TMP = 0

  ZPIZA_SLT_TMP = 0.
  ZCGA_SLT_TMP = 0.
  ZTAUREL_SLT_TMP = 0

  ZPIZA_AER_TMP = 0.
  ZCGA_AER_TMP = 0.
  ZTAUREL_AER_TMP = 0

  PAER_DST=0.
  PAER_SLT=0.
  PAER_AER=0.
  
 IF (LORILAM) THEN
   CALL AEROOPT_GET(                             &
        PSVT(IIB:IIE,IJB:IJE,:,NSV_AERBEG:NSV_AEREND)        &  !I [ppp]  aerosols concentration
        ,PZZ(IIB:IIE,IJB:IJE,:)                   &  !I [m] height of layers
        ,PRHODREF(IIB:IIE,IJB:IJE,:)              &  !I [kg/m3] density of air
        ,ZPIZA_AER_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)   &  !O [-] single scattering albedo of aerosols
        ,ZCGA_AER_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)    &  !O [-] assymetry factor for aerosols
        ,ZTAUREL_AER_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:) &  !O [-] opt.depth(wvl=lambda)/opt.depth(wvl=550nm)
        ,PAER_AER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT)            &  !O [-] optical depth of aerosols at wvl=550nm
        ,KSWB                                    &  !I |nbr] number of shortwave bands
        ,ZIR(IIB:IIE,IJB:IJE,:,:) &  !O [-] opt.depth(wvl=lambda)/opt.depth(wvl=550nm)
        ,ZII(IIB:IIE,IJB:IJE,:,:) &  !O [-] opt.depth(wvl=lambda)/opt.depth(wvl=550nm)
        )
 ENDIF
 IF(LDUST) THEN
   CALL DUSTOPT_GET(                             &
        PSVT(IIB:IIE,IJB:IJE,:,NSV_DSTBEG:NSV_DSTEND)        &  !I [ppp] Dust scalar concentration
        ,PZZ(IIB:IIE,IJB:IJE,:)                   &  !I [m] height of layers
        ,PRHODREF(IIB:IIE,IJB:IJE,:)              &  !I [kg/m3] density of air
        ,ZPIZA_DST_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)   &  !O [-] single scattering albedo of dust
        ,ZCGA_DST_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)    &  !O [-] assymetry factor for dust
        ,ZTAUREL_DST_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:) &  !O [-] opt.depth(wvl=lambda)/opt.depth(wvl=550nm)
        ,PAER_DST(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT)            &  !O [-] optical depth of dust at wvl=550nm
        ,KSWB                                    &  !I |nbr] number of shortwave bands
        )
   DO WVL_IDX=1,KSWB
     PDST_WL(:,:,:,WVL_IDX) = ZTAUREL_DST_TMP(:,:,:,WVL_IDX)* PAER(:,:,:,3)             
   ENDDO
 ENDIF
 IF(LSALT) THEN
   CALL SALTOPT_GET(                             &
        PSVT(IIB:IIE,IJB:IJE,:,NSV_SLTBEG:NSV_SLTEND)        &  !I [ppp] sea salt scalar concentration
        ,PZZ(IIB:IIE,IJB:IJE,:)                   &  !I [m] height of layers
        ,PRHODREF(IIB:IIE,IJB:IJE,:)              &  !I [kg/m3] density of air
        ,ZPIZA_SLT_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)   &  !O [-] single scattering albedo of sea salt
        ,ZCGA_SLT_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:)    &  !O [-] assymetry factor for sea salt
        ,ZTAUREL_SLT_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,:) &  !O [-] opt.depth(wvl=lambda)/opt.depth(wvl=550nm)
        ,PAER_SLT(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT)            &  !O [-] optical depth of sea salt at wvl=550nm
        ,KSWB                                    &  !I |nbr] number of shortwave bands
        )

 ENDIF


 ZTAUREL_EQ_TMP(:,:,:,:)=ZTAUREL_DST_TMP(:,:,:,:)+ZTAUREL_AER_TMP(:,:,:,:)+ZTAUREL_SLT_TMP(:,:,:,:)
 
 PAER(:,:,:,3)=PAER_AER(:,:,:)+PAER_SLT(:,:,:)+PAER_DST(:,:,:)


 WHERE (ZTAUREL_EQ_TMP(:,:,:,:).GT.0.0)
  ZPIZA_EQ_TMP(:,:,:,:)=(ZPIZA_DST_TMP(:,:,:,:)*ZTAUREL_DST_TMP(:,:,:,:)+&
                    ZPIZA_AER_TMP(:,:,:,:)*ZTAUREL_AER_TMP(:,:,:,:)+&
                    ZPIZA_SLT_TMP(:,:,:,:)*ZTAUREL_SLT_TMP(:,:,:,:))/&
                    ZTAUREL_EQ_TMP(:,:,:,:) 
 END WHERE
 WHERE ((ZTAUREL_EQ_TMP(:,:,:,:).GT.0.0).AND.(ZPIZA_EQ_TMP(:,:,:,:).GT.0.0))
  ZCGA_EQ_TMP(:,:,:,:)=(ZPIZA_DST_TMP(:,:,:,:)*ZTAUREL_DST_TMP(:,:,:,:)*ZCGA_DST_TMP(:,:,:,:)+&
                   ZPIZA_AER_TMP(:,:,:,:)*ZTAUREL_AER_TMP(:,:,:,:)*ZCGA_AER_TMP(:,:,:,:)+&
                   ZPIZA_SLT_TMP(:,:,:,:)*ZTAUREL_SLT_TMP(:,:,:,:)*ZCGA_SLT_TMP(:,:,:,:))/&
                   (ZTAUREL_EQ_TMP(:,:,:,:)*ZPIZA_EQ_TMP(:,:,:,:))
 END WHERE

 ZTAUREL_EQ_TMP(:,:,:,:)=max(1.E-8,ZTAUREL_EQ_TMP(:,:,:,:))
 ZCGA_EQ_TMP(:,:,:,:)=max(1.E-8,ZCGA_EQ_TMP(:,:,:,:))
 ZPIZA_EQ_TMP(:,:,:,:)=max(1.E-8,ZPIZA_EQ_TMP(:,:,:,:))
 PAER(:,:,:,3)=max(1.E-8,PAER(:,:,:,3))
 ZPIZA_EQ_TMP(:,:,:,:)=min(0.99,ZPIZA_EQ_TMP(:,:,:,:))


ENDIF      
!
! Computes SSA, optical depth and assymetry factor for clear sky (aerosols)
ZTAUAZ(:,:,:,:) = 0.
ZPIZAZ(:,:,:,:) = 0.
ZCGAZ(:,:,:,:)  = 0.
DO WVL_IDX=1,KSWB
 DO JAE=1,KAER
      !Special optical properties for dust
      IF (CAOP=='EXPL'.AND.(JAE==3)) THEN
      !Ponderation of aerosol optical in case of explicit optical factor
      !ti
        ZTAUAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)= ZTAUAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) + &
                                                 PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE) * &
                                       ZTAUREL_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX) 
      !wi*ti
        ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)= ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) + &
                                  PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE)                * &
                                  ZTAUREL_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX) * &
                                  ZPIZA_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX)
      !wi*ti*gi
        ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) = ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) + &
                                 PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE)                * &
                                 ZTAUREL_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX) * &
                                 ZPIZA_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX)   * &
                                 ZCGA_EQ_TMP(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,WVL_IDX)
      ELSE

      !Ponderation of aerosol optical properties 
      !ti
        ZTAUAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)=ZTAUAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)+&
             PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE) * RTAUA(WVL_IDX,JAE)
      !wi*ti
        ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)=ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)+&
                                               PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE) *&
                                        RTAUA(WVL_IDX,JAE)*RPIZA(WVL_IDX,JAE)
      !wi*ti*gi
        ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) =  ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) +&
                                               PAER(IIB:IIE,IJB:IJE,IKB-JPVEXT:IKE-JPVEXT,JAE)   *&
                        RTAUA(WVL_IDX,JAE)*RPIZA(WVL_IDX,JAE)*RCGA(WVL_IDX,JAE)
           ENDIF
 ENDDO
! assymetry factor:

ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) = ZCGAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)  / &
                                   ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)
! SSA:
ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) = ZPIZAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX) / &
                                    ZTAUAZ(IIB:IIE,IJB:IJE,IKB:IKE,WVL_IDX)
ENDDO
!

!
ALLOCATE(ZAER(KDLON,KFLEV,KAER))
IF (LRAD_DUST.AND.LDUST) THEN
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      ZAER (IIJ,:,:) = PAER_CLIM  (JI,JJ,:,:)
    END DO
  END DO
ELSE
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      ZAER (IIJ,:,:) = PAER  (JI,JJ,:,:)
    END DO
  END DO
END IF
!
ALLOCATE(ZPIZA_EQ(KDLON,KFLEV,KSWB))
ALLOCATE(ZCGA_EQ(KDLON,KFLEV,KSWB))
ALLOCATE(ZTAUREL_EQ(KDLON,KFLEV,KSWB))
IF(CAOP=='EXPL')THEN
    !Transform from vector of type #lon #lat #lev #wvl
    !to vectors of type #points, #levs, #wavelengths
  DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZPIZA_EQ(IIJ,:,:) = ZPIZA_EQ_TMP(JI,JJ,:,:)
    ZCGA_EQ(IIJ,:,:)= ZCGA_EQ_TMP(JI,JJ,:,:)
    ZTAUREL_EQ(IIJ,:,:)=ZTAUREL_EQ_TMP(JI,JJ,:,:)
  END DO
  END DO
  DEALLOCATE(ZPIZA_EQ_TMP)
  DEALLOCATE(ZCGA_EQ_TMP)
  DEALLOCATE(ZTAUREL_EQ_TMP)
  DEALLOCATE(ZPIZA_DST_TMP)
  DEALLOCATE(ZCGA_DST_TMP)
  DEALLOCATE(ZTAUREL_DST_TMP)  
  DEALLOCATE(ZPIZA_AER_TMP)
  DEALLOCATE(ZCGA_AER_TMP)
  DEALLOCATE(ZTAUREL_AER_TMP)
  DEALLOCATE(ZPIZA_SLT_TMP)
  DEALLOCATE(ZCGA_SLT_TMP)
  DEALLOCATE(ZTAUREL_SLT_TMP)
  DEALLOCATE(PAER_DST)
  DEALLOCATE(PAER_AER)
  DEALLOCATE(PAER_SLT)
  DEALLOCATE(ZIR)
  DEALLOCATE(ZII)
END IF


!
!      4.2   OZONE content 
!
ALLOCATE(ZO3AVE(KDLON,KFLEV))
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZO3AVE(IIJ,:)  = POZON (JI,JJ,:)           
  END DO
END DO
!
!-------------------------------------------------------------------------------
!
!*       5.    CALLS THE E.C.M.W.F. RADIATION CODE
!	           -----------------------------------
!
!
!*       5.1   INITIALIZES 2D AND SURFACE FIELDS
!
ALLOCATE(ZRMU0(KDLON))
ALLOCATE(ZLSM(KDLON))
! 
ALLOCATE(ZALBP(KDLON,KSWB))
ALLOCATE(ZALBD(KDLON,KSWB))
!
ALLOCATE(ZEMIS(KDLON))
ALLOCATE(ZEMIW(KDLON))
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZEMIS(IIJ)   = PEMIS(JI,JJ)
    ZRMU0(IIJ)    = PCOSZEN(JI,JJ)
    ZLSM(IIJ)     = 1.0 - PSEA(JI,JJ)  
  END DO
END DO  
!
! spectral albedo
!
IF ( SIZE(PDIR_ALB,3)==1 ) THEN
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      !  sw direct and diffuse albedos
      ZALBP(IIJ,:)  = PDIR_ALB(JI,JJ,1)
      ZALBD(IIJ,:)  = PSCA_ALB(JI,JJ,1)
      !
    END DO
  END DO
ELSE  
  DO JK=1, SIZE(PDIR_ALB,3)
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
         IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
         !  sw direct and diffuse albedos
         ZALBP(IIJ,JK)  = PDIR_ALB(JI,JJ,JK)
         ZALBD(IIJ,JK)  = PSCA_ALB(JI,JJ,JK)
       ENDDO
     END DO
   ENDDO  
END IF
!
!
! LW emissivity
ZEMIW(:)= ZEMIS(:)
!
!solar constant
ZRII0= PCORSOL*XI0
!
!
!
!*       5.2   ACCOUNTS FOR THE CLEAR-SKY APPROXIMATION
!
!  Performs the horizontal average of the fields when no cloud
!
ZCLOUD(:) = SUM( ZCFAVE(:,:),DIM=2 )
!
! MODIF option CLLY      
ALLOCATE ( ICLEAR_2D_TM1(KDLON) )
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ICLEAR_2D_TM1(IIJ) = KCLEARCOL_TM1(JI,JJ)
  END DO
END DO
!
IF(OCLOUD_ONLY .OR. OCLEAR_SKY) THEN
  !
  GCLEAR_2D(:) = .TRUE.
  WHERE( (ZCLOUD(:) > 0.0) .OR. (ICLEAR_2D_TM1(:)==0) )
    GCLEAR_2D(:) = .FALSE.
  END WHERE
  !
  !
  ICLEAR_COL = COUNT( GCLEAR_2D(:) )  ! number of clear sky columns
  !

  IF( ICLEAR_COL == KDLON ) THEN ! No cloud case so only the mean clear-sky
    GCLEAR_2D(1) = .FALSE.       !           column is selected
    ICLEAR_COL = KDLON-1
    GNOCL = .TRUE.
  ELSE
    GNOCL = .FALSE.
  END IF

  GCLEAR(:,:) = SPREAD( GCLEAR_2D(:),DIM=2,NCOPIES=KFLEV )
  ICLOUD_COL = KDLON - ICLEAR_COL     ! number of  cloudy   columns
!
  IF( ICLEAR_COL /=0 ) THEN ! at least one clear-sky column exists
    ZT_CLEAR(:)  = SUM( ZTAVE(:,:) ,DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ZP_CLEAR(:)  = SUM( ZPAVE(:,:) ,DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ZQV_CLEAR(:) = SUM( ZQVAVE(:,:),DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ZOZ_CLEAR(:) = SUM( ZO3AVE(:,:),DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ZDP_CLEAR(:) = SUM( ZDPRES(:,:),DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    DO JK1=1,KAER
      ZAER_CLEAR(:,JK1) = SUM( ZAER(:,:,JK1),DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    END DO
    !Get an average value for the clear column
    IF(CAOP=='EXPL')THEN
       DO WVL_IDX=1,KSWB
          ZPIZA_EQ_CLEAR(:,WVL_IDX) = SUM( ZPIZA_EQ(:,:,WVL_IDX), DIM=1,MASK=GCLEAR(:,:))/FLOAT(ICLEAR_COL)
          ZCGA_EQ_CLEAR(:,WVL_IDX) = SUM( ZCGA_EQ(:,:,WVL_IDX),DIM=1,MASK=GCLEAR(:,:))/FLOAT(ICLEAR_COL)
          ZTAUREL_EQ_CLEAR(:,WVL_IDX) = SUM( ZTAUREL_EQ(:,:,WVL_IDX),DIM=1,MASK=GCLEAR(:,:))/FLOAT(ICLEAR_COL)
       ENDDO
    ENDIF
    
    !
    ZHP_CLEAR(1:KFLEV) =SUM( ZPRES_HL(:,1:KFLEV),DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ZHT_CLEAR(1:KFLEV)  = SUM( ZT_HL(:,1:KFLEV) ,DIM=1,MASK=GCLEAR(:,:) )/FLOAT(ICLEAR_COL)
    ! 
    GCLEAR_SWB(:,:) = SPREAD(GCLEAR_2D(:),DIM=2,NCOPIES=KSWB)
    ZALBP_CLEAR(:) = SUM( ZALBP(:,:),DIM=1,MASK=GCLEAR_SWB(:,:) ) &
         / FLOAT(ICLEAR_COL)
    ZALBD_CLEAR(:) = SUM( ZALBD(:,:),DIM=1,MASK=GCLEAR_SWB(:,:) ) &
         / FLOAT(ICLEAR_COL)
    !
    ZEMIS_CLEAR  = SUM( ZEMIS(:),DIM=1,MASK=GCLEAR_2D(:)) / FLOAT(ICLEAR_COL)
    ZEMIW_CLEAR  = SUM( ZEMIW(:),DIM=1,MASK=GCLEAR_2D(:)) / FLOAT(ICLEAR_COL)
    ZRMU0_CLEAR  = SUM( ZRMU0(:) ,DIM=1,MASK=GCLEAR_2D(:)) / FLOAT(ICLEAR_COL)
    ZTS_CLEAR    = SUM( ZTS(:) ,DIM=1,MASK=GCLEAR_2D(:)) / FLOAT(ICLEAR_COL)
    ZLSM_CLEAR   = SUM( ZLSM(:) ,DIM=1,MASK=GCLEAR_2D(:)) / FLOAT(ICLEAR_COL)  
!
  ELSE ! the first column is chosen, without physical meaning: it will not be
    ! unpacked after the call to the radiation ecmwf routine
    ZT_CLEAR(:)  = ZTAVE(1,:)
    ZP_CLEAR(:)  = ZPAVE(1,:)
    ZQV_CLEAR(:) = ZQVAVE(1,:)
    ZOZ_CLEAR(:) = ZO3AVE(1,:)
    ZDP_CLEAR(:) = ZDPRES(1,:)
    ZAER_CLEAR(:,:) = ZAER(1,:,:)
    IF(CAOP=='EXPL')THEN
       ZPIZA_EQ_CLEAR(:,:)=ZPIZA_EQ(1,:,:)
       ZCGA_EQ_CLEAR(:,:)=ZCGA_EQ(1,:,:)
       ZTAUREL_EQ_CLEAR(:,:)=ZTAUREL_EQ(1,:,:)
    ENDIF
!
    ZHP_CLEAR(1:KFLEV)  = ZPRES_HL(1,1:KFLEV)
    ZHT_CLEAR(1:KFLEV)  = ZT_HL(1,1:KFLEV)
    ZALBP_CLEAR(:) = ZALBP(1,:)
    ZALBD_CLEAR(:) = ZALBD(1,:)
!
    ZEMIS_CLEAR  = ZEMIS(1)
    ZEMIW_CLEAR  = ZEMIW(1) 
    ZRMU0_CLEAR  = ZRMU0(1)
    ZTS_CLEAR    = ZTS(1) 
    ZLSM_CLEAR   = ZLSM(1)  
  END IF
  !
  GCLOUD(:,:) = .NOT.GCLEAR(:,:) ! .true. where the column is cloudy
  GCLOUDT(:,:)=TRANSPOSE(GCLOUD(:,:))
  ICLOUD = ICLOUD_COL*KFLEV
  ALLOCATE(ZWORK1(ICLOUD))
  ALLOCATE(ZWORK2(ICLOUD+KFLEV)) !  allocation for the KFLEV levels of 
                                 !  the ICLOUD cloudy columns
                                 !  and of the single clear_sky one
  !
  ! temperature profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZTAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZT_CLEAR(1:)      !   and the single clear_sky one
  DEALLOCATE(ZTAVE)
  ALLOCATE(ZTAVE(ICLOUD_COL+1,KFLEV))
  ZTAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  ! vapor mixing ratio profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQVAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZQV_CLEAR(1:)      !   and the single clear_sky one
  DEALLOCATE(ZQVAVE)
  ALLOCATE(ZQVAVE(ICLOUD_COL+1,KFLEV))
  ZQVAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  ! mesh size 
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZDZ(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0              !   and the single clear_sky one
  DEALLOCATE(ZDZ)
  ALLOCATE(ZDZ(ICLOUD_COL+1,KFLEV))
  ZDZ(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !
  ! liquid water mixing ratio profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQLAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0              !   and the single clear_sky one
  DEALLOCATE(ZQLAVE)
  ALLOCATE(ZQLAVE(ICLOUD_COL+1,KFLEV))
  ZQLAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !rain 
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQRAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0              !   and the single clear_sky one
  DEALLOCATE(ZQRAVE)
  ALLOCATE(ZQRAVE(ICLOUD_COL+1,KFLEV))
  ZQRAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ! 
  ! ice water mixing ratio profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQIAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
  DEALLOCATE(ZQIAVE)
  ALLOCATE(ZQIAVE(ICLOUD_COL+1,KFLEV))
  ZQIAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !
  ! liquid water mixing ratio profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQLWC(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0              !   and the single clear_sky one
  DEALLOCATE(ZQLWC)
  ALLOCATE(ZQLWC(ICLOUD_COL+1,KFLEV))
  ZQLWC(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !rain 
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQRWC(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0              !   and the single clear_sky one
  DEALLOCATE(ZQRWC)
  ALLOCATE(ZQRWC(ICLOUD_COL+1,KFLEV))
  ZQRWC(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ! 
  ! ice water mixing ratio profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZQIWC(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
  DEALLOCATE(ZQIWC)
  ALLOCATE(ZQIWC(ICLOUD_COL+1,KFLEV))
  ZQIWC(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !
  ! cloud fraction profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZCFAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
  DEALLOCATE(ZCFAVE)
  ALLOCATE(ZCFAVE(ICLOUD_COL+1,KFLEV))
  ZCFAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  ! C2R2 water particle concentration
  !
  IF ( SIZE(ZCCT_C2R2) > 0 )  THEN
    ZWORK1(:) = PACK( TRANSPOSE(ZCCT_C2R2(:,:)),MASK=GCLOUDT(:,:) )
    ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
    ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
    DEALLOCATE(ZCCT_C2R2)
    ALLOCATE(ZCCT_C2R2(ICLOUD_COL+1,KFLEV))
    ZCCT_C2R2 (:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ENDIF
  IF ( SIZE (ZCRT_C2R2) > 0 )  THEN
    ZWORK1(:) = PACK( TRANSPOSE(ZCRT_C2R2(:,:)),MASK=GCLOUDT(:,:) )
    ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
    ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
    DEALLOCATE(ZCRT_C2R2)
    ALLOCATE(ZCRT_C2R2(ICLOUD_COL+1,KFLEV))
    ZCRT_C2R2 (:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ENDIF
  IF ( SIZE (ZCIT_C1R3) > 0)  THEN
    ZWORK1(:) = PACK( TRANSPOSE(ZCIT_C1R3(:,:)),MASK=GCLOUDT(:,:) )
    ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
    ZWORK2(ICLOUD+1:)= 0.0      !   and the single clear_sky one
    DEALLOCATE(ZCIT_C1R3)
    ALLOCATE(ZCIT_C1R3(ICLOUD_COL+1,KFLEV))
    ZCIT_C1R3 (:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ENDIF
  !
  ! ozone content profiles
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZO3AVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZOZ_CLEAR(1:)    !   and the single clear_sky one
  DEALLOCATE(ZO3AVE)
  ALLOCATE(ZO3AVE(ICLOUD_COL+1,KFLEV))
  ZO3AVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZPAVE(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZP_CLEAR(1:)     !   and the single clear_sky one
  DEALLOCATE(ZPAVE)
  ALLOCATE(ZPAVE(ICLOUD_COL+1,KFLEV))
  ZPAVE(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !pressure thickness
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZDPRES(:,:)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD) ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZDP_CLEAR(1:)    !   and the single clear_sky one
  DEALLOCATE(ZDPRES)
  ALLOCATE(ZDPRES(ICLOUD_COL+1,KFLEV))
  ZDPRES(:,:) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  !
  !aerosols
  !
  ALLOCATE(ZWORK1AER(ICLOUD,KAER))
  ALLOCATE(ZWORK2AER(ICLOUD+KFLEV,KAER))
  DO JK=1,KAER
    ZWORK1AER(:,JK) = PACK( TRANSPOSE(ZAER(:,:,JK)),MASK=GCLOUDT(:,:) )
    ZWORK2AER(1:ICLOUD,JK)=ZWORK1AER(:,JK)
    ZWORK2AER(ICLOUD+1:,JK)=ZAER_CLEAR(:,JK)
  END DO
  DEALLOCATE(ZAER)
  ALLOCATE(ZAER(ICLOUD_COL+1,KFLEV,KAER))
  DO JK=1,KAER
    ZAER(:,:,JK) = TRANSPOSE( RESHAPE( ZWORK2AER(:,JK),(/KFLEV,ICLOUD_COL+1/) ) )
  END DO
  DEALLOCATE (ZWORK1AER)
  DEALLOCATE (ZWORK2AER)
  !
  IF(CAOP=='EXPL')THEN
     ALLOCATE(ZWORK1AER(ICLOUD,KSWB))        !New vector with value for all cld. points
     ALLOCATE(ZWORK2AER(ICLOUD+KFLEV,KSWB))  !New vector with value for all cld.points + 1 clr column
     !Single scattering albedo
     DO WVL_IDX=1,KSWB
        ZWORK1AER(:,WVL_IDX) = PACK( TRANSPOSE(ZPIZA_EQ(:,:,WVL_IDX)),MASK=GCLOUDT(:,:) )
        ZWORK2AER(1:ICLOUD,WVL_IDX) = ZWORK1AER(:,WVL_IDX)
        ZWORK2AER(ICLOUD+1:,WVL_IDX) = ZPIZA_EQ_CLEAR(:,WVL_IDX)
     ENDDO
     DEALLOCATE(ZPIZA_EQ)
     ALLOCATE(ZPIZA_EQ(ICLOUD_COL+1,KFLEV,KSWB))
     DO WVL_IDX=1,KSWB
        ZPIZA_EQ(:,:,WVL_IDX) = TRANSPOSE( RESHAPE( ZWORK2AER(:,WVL_IDX),(/KFLEV,ICLOUD_COL+1/) ) )
     ENDDO
     !Assymetry factor
     DO WVL_IDX=1,KSWB
        ZWORK1AER(:,WVL_IDX) = PACK(TRANSPOSE(ZCGA_EQ(:,:,WVL_IDX)), MASK=GCLOUDT(:,:))
        ZWORK2AER(1:ICLOUD,WVL_IDX) = ZWORK1AER(:,WVL_IDX)
        ZWORK2AER(ICLOUD+1:,WVL_IDX) = ZCGA_EQ_CLEAR(:,WVL_IDX)
     ENDDO
     DEALLOCATE(ZCGA_EQ)
     ALLOCATE(ZCGA_EQ(ICLOUD_COL+1,KFLEV,KSWB))
     DO WVL_IDX=1,KSWB
        ZCGA_EQ(:,:,WVL_IDX) = TRANSPOSE(RESHAPE(ZWORK2AER(:,WVL_IDX),(/KFLEV,ICLOUD_COL+1/)))
     ENDDO
     !Relative wavelength-distributed optical depth
     DO WVL_IDX=1,KSWB
        ZWORK1AER(:,WVL_IDX) =  PACK(TRANSPOSE(ZTAUREL_EQ(:,:,WVL_IDX)), MASK=GCLOUDT(:,:))
        ZWORK2AER(1:ICLOUD,WVL_IDX) = ZWORK1AER(:,WVL_IDX)
        ZWORK2AER(ICLOUD+1:,WVL_IDX) = ZTAUREL_EQ_CLEAR(:,WVL_IDX)
     ENDDO
     DEALLOCATE(ZTAUREL_EQ)
     ALLOCATE(ZTAUREL_EQ(ICLOUD_COL+1,KFLEV,KSWB))
     DO WVL_IDX=1,KSWB
        ZTAUREL_EQ(:,:,WVL_IDX) = TRANSPOSE(RESHAPE(ZWORK2AER(:,WVL_IDX),(/KFLEV,ICLOUD_COL+1/)))
     ENDDO
     DEALLOCATE(ZWORK1AER)
     DEALLOCATE(ZWORK2AER)
  ELSE
     DEALLOCATE(ZPIZA_EQ)
     ALLOCATE(ZPIZA_EQ(ICLOUD_COL+1,KFLEV,KSWB))
     DEALLOCATE(ZCGA_EQ)
     ALLOCATE(ZCGA_EQ(ICLOUD_COL+1,KFLEV,KSWB))
     DEALLOCATE(ZTAUREL_EQ)
     ALLOCATE(ZTAUREL_EQ(ICLOUD_COL+1,KFLEV,KSWB))
  ENDIF !Check on LDUST
  
  ! half-level variables
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZPRES_HL(:,1:KFLEV)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD)  ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZHP_CLEAR(1:)     !   and the single clear_sky one
  DEALLOCATE(ZPRES_HL)
  ALLOCATE(ZPRES_HL(ICLOUD_COL+1,KFLEV+1))
  ZPRES_HL(:,1:KFLEV) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ZPRES_HL(:,KFLEV+1) = PSTATM(IKSTAE,2)*100.0
  !
  ZWORK1(:) = PACK( TRANSPOSE(ZT_HL(:,1:KFLEV)),MASK=GCLOUDT(:,:) )
  ZWORK2(1:ICLOUD) = ZWORK1(1:ICLOUD)  ! fills the ICLOUD_COL cloudy columns
  ZWORK2(ICLOUD+1:)= ZHT_CLEAR(1:)     !   and the single clear_sky one
  DEALLOCATE(ZT_HL)
  ALLOCATE(ZT_HL(ICLOUD_COL+1,KFLEV+1))
  ZT_HL(:,1:KFLEV) = TRANSPOSE( RESHAPE( ZWORK2(:),(/KFLEV,ICLOUD_COL+1/) ) )
  ZT_HL(:,KFLEV+1) = PSTATM(IKSTAE,3)
  !
  ! surface fields
  !
  ALLOCATE(ZWORK3(ICLOUD_COL))
  ALLOCATE(ZWORK4(ICLOUD_COL,KSWB))
  ALLOCATE(ZWORK(KDLON))
  DO JALBS=1,KSWB
    ZWORK(:)  = ZALBP(:,JALBS)
    ZWORK3(:) = PACK( ZWORK(:),MASK=.NOT.GCLEAR_2D(:) )
    ZWORK4(:,JALBS) = ZWORK3(:)
  END DO
  DEALLOCATE(ZALBP)
  ALLOCATE(ZALBP(ICLOUD_COL+1,KSWB))
  ZALBP(1:ICLOUD_COL,:) = ZWORK4(1:ICLOUD_COL,:)
  ZALBP(ICLOUD_COL+1,:) = ZALBP_CLEAR(:)
  !
  DO JALBS=1,KSWB
    ZWORK(:)  = ZALBD(:,JALBS)
    ZWORK3(:) = PACK( ZWORK(:),MASK=.NOT.GCLEAR_2D(:) )
    ZWORK4(:,JALBS) = ZWORK3(:)
  END DO
  DEALLOCATE(ZALBD)
  ALLOCATE(ZALBD(ICLOUD_COL+1,KSWB))
  ZALBD(1:ICLOUD_COL,:) = ZWORK4(1:ICLOUD_COL,:)  
  ZALBD(ICLOUD_COL+1,:) = ZALBD_CLEAR(:)  
  !
  DEALLOCATE(ZWORK4)
  !
  ZWORK3(:) = PACK( ZEMIS(:),MASK=.NOT.GCLEAR_2D(:) )
  DEALLOCATE(ZEMIS)
  ALLOCATE(ZEMIS(ICLOUD_COL+1))
  ZEMIS(1:ICLOUD_COL) = ZWORK3(1:ICLOUD_COL)
  ZEMIS(ICLOUD_COL+1) = ZEMIS_CLEAR
  !
  !
  ZWORK3(:) = PACK( ZEMIW(:),MASK=.NOT.GCLEAR_2D(:) )
  DEALLOCATE(ZEMIW)
  ALLOCATE(ZEMIW(ICLOUD_COL+1))
  ZEMIW(1:ICLOUD_COL) = ZWORK3(1:ICLOUD_COL)
  ZEMIW(ICLOUD_COL+1) = ZEMIW_CLEAR
  ! 
  !
  ZWORK3(:) = PACK( ZRMU0(:),MASK=.NOT.GCLEAR_2D(:) )
  DEALLOCATE(ZRMU0)
  ALLOCATE(ZRMU0(ICLOUD_COL+1))
  ZRMU0(1:ICLOUD_COL) = ZWORK3(1:ICLOUD_COL)
  ZRMU0(ICLOUD_COL+1) = ZRMU0_CLEAR
  !
  ZWORK3(:) = PACK( ZLSM(:),MASK=.NOT.GCLEAR_2D(:) )
  DEALLOCATE(ZLSM)
  ALLOCATE(ZLSM(ICLOUD_COL+1))
  ZLSM(1:ICLOUD_COL) = ZWORK3(1:ICLOUD_COL)
  ZLSM (ICLOUD_COL+1)= ZLSM_CLEAR
  ! 
  ZWORK3(:) = PACK( ZTS(:),MASK=.NOT.GCLEAR_2D(:) )
  DEALLOCATE(ZTS)
  ALLOCATE(ZTS(ICLOUD_COL+1))
  ZTS(1:ICLOUD_COL) = ZWORK3(1:ICLOUD_COL)
  ZTS(ICLOUD_COL+1) = ZTS_CLEAR
  !
  DEALLOCATE(ZWORK1)
  DEALLOCATE(ZWORK2)
  DEALLOCATE(ZWORK3)
  DEALLOCATE(ZWORK)
  !  
  IDIM = ICLOUD_COL +1
!
ELSE
  !
  !*       5.3   RADIATION COMPUTATIONS FOR THE FULL COLUMN NUMBER (KDLON)
  !
  IDIM = KDLON
END IF
!
! initialisation of cloud trace for the next radiation time step
WHERE ( ZCLOUD(:) <= 0.0 )
  ICLEAR_2D_TM1(:) = 1
ELSEWHERE
  ICLEAR_2D_TM1(:) = 0
END WHERE
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    KCLEARCOL_TM1(JI,JJ) = ICLEAR_2D_TM1(IIJ)
  END DO
END DO
! 
!
!*       5.4  VERTICAL grid modification(up-down) for compatibility with ECMWF 
!             radiation vertical grid. ALLOCATION of the outputs.  
!            
!             
ALLOCATE (ZWORK_GRID(SIZE(ZPRES_HL,1),KFLEV+1))
!
!half level pressure
ZWORK_GRID(:,:)=ZPRES_HL(:,:)
DO JKRAD=1, KFLEV+1
  JK1=(KFLEV+1)+1-JKRAD
  ZPRES_HL(:,JKRAD) = ZWORK_GRID(:,JK1)
END DO
!
!half level temperature
ZWORK_GRID(:,:)=ZT_HL(:,:)
DO  JKRAD=1, KFLEV+1
  JK1=(KFLEV+1)+1-JKRAD
  ZT_HL(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
DEALLOCATE(ZWORK_GRID)
!
!mean layer variables
!-------------------------------------
ALLOCATE(ZWORK_GRID(SIZE(ZTAVE,1),KFLEV))
!
!mean layer temperature
ZWORK_GRID(:,:)=ZTAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZTAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!mean layer pressure
ZWORK_GRID(:,:)=ZPAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZPAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!mean layer pressure thickness
ZWORK_GRID(:,:)=ZDPRES(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZDPRES(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!mesh size
ZWORK_GRID(:,:)=ZDZ(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZDZ(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO

!mean layer cloud fraction
ZWORK_GRID(:,:)=ZCFAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZCFAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!mean layer water vapor mixing ratio
ZWORK_GRID(:,:)=ZQVAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQVAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!ice
ZWORK_GRID(:,:)=ZQIAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQIAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!liquid water
ZWORK_GRID(:,:)=ZQLAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQLAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO


!rain water
ZWORK_GRID(:,:)=ZQRAVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQRAVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!ice water content
ZWORK_GRID(:,:)=ZQIWC(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQIWC(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!liquid water content
ZWORK_GRID(:,:)=ZQLWC(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQLWC(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO


!rain water content
ZWORK_GRID(:,:)=ZQRWC(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZQRWC(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO


!C2R2 water particle concentration
!
IF (SIZE(ZCCT_C2R2) > 0) THEN
  ZWORK_GRID(:,:)=ZCCT_C2R2(:,:)
  DO JKRAD=1, KFLEV
    JK1=KFLEV+1-JKRAD
    ZCCT_C2R2(:,JKRAD)=ZWORK_GRID(:,JK1)
  END DO
END IF
IF (SIZE(ZCRT_C2R2) > 0) THEN
  ZWORK_GRID(:,:)=ZCRT_C2R2(:,:)
  DO JKRAD=1, KFLEV
    JK1=KFLEV+1-JKRAD
    ZCRT_C2R2(:,JKRAD)=ZWORK_GRID(:,JK1)
  END DO
END IF
IF (SIZE(ZCIT_C1R3) > 0) THEN
  ZWORK_GRID(:,:)=ZCIT_C1R3(:,:)
  DO JKRAD=1, KFLEV
    JK1=KFLEV+1-JKRAD
    ZCIT_C1R3(:,JKRAD)=ZWORK_GRID(:,JK1)
  END DO
END IF
!
!ozone content 
ZWORK_GRID(:,:)=ZO3AVE(:,:)
DO JKRAD=1, KFLEV
  JK1=KFLEV+1-JKRAD
  ZO3AVE(:,JKRAD)=ZWORK_GRID(:,JK1)
END DO
!
!aerosol optical depth 
DO JI=1,KAER
  ZWORK_GRID(:,:)=ZAER(:,:,JI)
  DO JKRAD=1, KFLEV
    JK1=KFLEV+1-JKRAD
    ZAER(:,JKRAD,JI)=ZWORK_GRID(:,JK1)
  END DO
END DO
IF (CAOP=='EXPL') THEN
!TURN MORE FIELDS UPSIDE DOWN...
!Dust single scattering albedo
DO JI=1,KSWB
   ZWORK_GRID(:,:)=ZPIZA_EQ(:,:,JI)
   DO JKRAD=1,KFLEV
      JK1=KFLEV+1-JKRAD
      ZPIZA_EQ(:,JKRAD,JI)=ZWORK_GRID(:,JK1)
   ENDDO
ENDDO
!Dust asymmetry factor
DO JI=1,KSWB
   ZWORK_GRID(:,:)=ZCGA_EQ(:,:,JI)
   DO JKRAD=1,KFLEV
      JK1=KFLEV+1-JKRAD
      ZCGA_EQ(:,JKRAD,JI)=ZWORK_GRID(:,JK1)
   ENDDO
ENDDO
DO JI=1,KSWB
   ZWORK_GRID(:,:)=ZTAUREL_EQ(:,:,JI)
   DO JKRAD=1,KFLEV
      JK1=KFLEV+1-JKRAD
      ZTAUREL_EQ(:,JKRAD,JI)=ZWORK_GRID(:,JK1)
   ENDDO
ENDDO 

END IF

!
DEALLOCATE(ZWORK_GRID)
!
!mean layer saturation specific humidity
!
ALLOCATE(ZQSAVE(SIZE(ZTAVE,1),SIZE(ZTAVE,2)))
!
WHERE (ZTAVE(:,:) > XTT)
  ZQSAVE(:,:) = QSATW_2D(ZTAVE, ZPAVE)
ELSEWHERE
  ZQSAVE(:,:) = QSATI_2D(ZTAVE, ZPAVE)
END WHERE
!
! allocations for the radiation code outputs
!
ALLOCATE(ZDTLW(IDIM,KFLEV))
ALLOCATE(ZDTSW(IDIM,KFLEV))
ALLOCATE(ZFLUX_TOP_GND_IRVISNIR(IDIM,KFLUX))
ALLOCATE(ZSFSWDIR(IDIM,ISWB))
ALLOCATE(ZSFSWDIF(IDIM,ISWB))
ALLOCATE(ZDTLW_CS(IDIM,KFLEV))
ALLOCATE(ZDTSW_CS(IDIM,KFLEV))
ALLOCATE(ZFLUX_TOP_GND_IRVISNIR_CS(IDIM,KFLUX))
!
!
ALLOCATE(ZFLUX_LW(IDIM,2,KFLEV+1))
ALLOCATE(ZFLUX_SW_DOWN(IDIM,KFLEV+1))
ALLOCATE(ZFLUX_SW_UP(IDIM,KFLEV+1))
ALLOCATE(ZRADLP(IDIM,KFLEV))
IF( KRAD_DIAG >= 1) THEN
  ALLOCATE(ZNFLW(IDIM,KFLEV+1))
  ALLOCATE(ZNFSW(IDIM,KFLEV+1))
ELSE
  ALLOCATE(ZNFLW(0,0))
  ALLOCATE(ZNFSW(0,0))
END IF
! 
IF( KRAD_DIAG >= 2) THEN
  ALLOCATE(ZFLUX_SW_DOWN_CS(IDIM,KFLEV+1))
  ALLOCATE(ZFLUX_SW_UP_CS(IDIM,KFLEV+1))
  ALLOCATE(ZFLUX_LW_CS(IDIM,2,KFLEV+1))
  ALLOCATE(ZNFLW_CS(IDIM,KFLEV+1))
  ALLOCATE(ZNFSW_CS(IDIM,KFLEV+1))
ELSE
  ALLOCATE(ZFLUX_SW_DOWN_CS(0,0))
  ALLOCATE(ZFLUX_SW_UP_CS(0,0))
  ALLOCATE(ZFLUX_LW_CS(0,0,0))
  ALLOCATE(ZNFSW_CS(0,0))
  ALLOCATE(ZNFLW_CS(0,0))
END IF
!
IF( KRAD_DIAG >= 3) THEN
  ALLOCATE(ZPLAN_ALB_VIS(IDIM))
  ALLOCATE(ZPLAN_ALB_NIR(IDIM))
  ALLOCATE(ZPLAN_TRA_VIS(IDIM))
  ALLOCATE(ZPLAN_TRA_NIR(IDIM))
  ALLOCATE(ZPLAN_ABS_VIS(IDIM))
  ALLOCATE(ZPLAN_ABS_NIR(IDIM))
ELSE
  ALLOCATE(ZPLAN_ALB_VIS(0))
  ALLOCATE(ZPLAN_ALB_NIR(0))
  ALLOCATE(ZPLAN_TRA_VIS(0))
  ALLOCATE(ZPLAN_TRA_NIR(0))
  ALLOCATE(ZPLAN_ABS_VIS(0))
  ALLOCATE(ZPLAN_ABS_NIR(0))
END IF
!
IF( KRAD_DIAG >= 4) THEN
  ALLOCATE(ZEFCL_RRTM(IDIM,KFLEV))
  ALLOCATE(ZCLSW_TOTAL(IDIM,KFLEV))
  ALLOCATE(ZTAU_TOTAL(IDIM,KSWB,KFLEV))
  ALLOCATE(ZOMEGA_TOTAL(IDIM,KSWB,KFLEV))
  ALLOCATE(ZCG_TOTAL(IDIM,KSWB,KFLEV))
  ALLOCATE(ZEFCL_LWD(IDIM,KFLEV))
  ALLOCATE(ZEFCL_LWU(IDIM,KFLEV))
  ALLOCATE(ZFLWP(IDIM,KFLEV))
  ALLOCATE(ZFIWP(IDIM,KFLEV))  
  ALLOCATE(ZRADIP(IDIM,KFLEV)) 
ELSE
  ALLOCATE(ZEFCL_RRTM(0,0))
  ALLOCATE(ZCLSW_TOTAL(0,0))
  ALLOCATE(ZTAU_TOTAL(0,0,0))
  ALLOCATE(ZOMEGA_TOTAL(0,0,0))
  ALLOCATE(ZCG_TOTAL(0,0,0))
  ALLOCATE(ZEFCL_LWD(0,0))
  ALLOCATE(ZEFCL_LWU(0,0))
  ALLOCATE(ZFLWP(0,0))
  ALLOCATE(ZFIWP(0,0))
  ALLOCATE(ZRADIP(0,0))
END IF
!
!*       5.6   CALLS THE ECMWF_RADIATION ROUTINES
!
!  mixing ratio -> specific humidity conversion
!

ZQVAVE(:,:) = ZQVAVE(:,:) / (1.+ZQVAVE(:,:))
!
IF( IDIM <= KRAD_COLNBR ) THEN 
!
! there is less than KRAD_COLNBR verticals to be considered therefore
! no split of the arrays is performed
!
   CALL ECMWF_RADIATION_VERS2  ( IDIM ,KFLEV, KRAD_DIAG, KAER,     &      
       ZDZ,HEFRADL,HEFRADI,HOPWSW, HOPISW, HOPWLW, HOPILW,PFUDG,      &
       ZRII0, ZAER , ZALBD, ZALBP, ZPRES_HL, ZPAVE,               &
       PCCO2, ZCFAVE, ZDPRES, ZEMIS, ZEMIW, ZLSM, ZRMU0,          &
       ZO3AVE , ZQVAVE, ZQIAVE ,ZQIWC,ZQLAVE,ZQLWC, ZQSAVE, ZQRAVE,  ZQRWC,  &
       ZT_HL,ZTAVE, ZTS, ZCCT_C2R2, ZCRT_C2R2, ZCIT_C1R3,         &
       ZNFLW_CS, ZNFLW, ZNFSW_CS,ZNFSW,                           &
       ZDTLW, ZDTSW, ZFLUX_TOP_GND_IRVISNIR,                      &
       ZSFSWDIR, ZSFSWDIF,                                        &
       ZFLUX_SW_DOWN, ZFLUX_SW_UP, ZFLUX_LW ,                     &
       ZDTLW_CS, ZDTSW_CS, ZFLUX_TOP_GND_IRVISNIR_CS,             &
       ZFLUX_SW_DOWN_CS, ZFLUX_SW_UP_CS, ZFLUX_LW_CS,             &           
       ZPLAN_ALB_VIS,ZPLAN_ALB_NIR, ZPLAN_TRA_VIS, ZPLAN_TRA_NIR, &
       ZPLAN_ABS_VIS, ZPLAN_ABS_NIR,ZEFCL_LWD, ZEFCL_LWU,         &
       ZFLWP, ZFIWP,ZRADLP, ZRADIP,ZEFCL_RRTM,  ZCLSW_TOTAL,  ZTAU_TOTAL,  &
       ZOMEGA_TOTAL,ZCG_TOTAL,                                    &
       GAOP, ZPIZA_EQ,ZCGA_EQ,ZTAUREL_EQ                       )

ELSE
!
! the splitting of the arrays will be performed
!
  INUM_CALL = CEILING( FLOAT( IDIM ) / FLOAT( KRAD_COLNBR ) )
  IDIM_RESIDUE = IDIM
!
  DO JI_SPLIT = 1 , INUM_CALL
    IDIM_EFF = MIN( IDIM_RESIDUE,KRAD_COLNBR )
    !
    IF( JI_SPLIT == 1 .OR. JI_SPLIT == INUM_CALL ) THEN       
      ALLOCATE(  ZALBP_SPLIT(IDIM_EFF,KSWB))
      ALLOCATE(  ZALBD_SPLIT(IDIM_EFF,KSWB))  
      ALLOCATE(  ZEMIS_SPLIT(IDIM_EFF))
      ALLOCATE(  ZEMIW_SPLIT(IDIM_EFF))
      ALLOCATE(  ZRMU0_SPLIT(IDIM_EFF))
      ALLOCATE(  ZCFAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZO3AVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZT_HL_SPLIT(IDIM_EFF,KFLEV+1))
      ALLOCATE(  ZPRES_HL_SPLIT(IDIM_EFF,KFLEV+1))
      ALLOCATE(  ZDZ_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQLAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQIAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQRAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQLWC_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQIWC_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQRWC_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZQVAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZTAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZPAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZAER_SPLIT( IDIM_EFF,KFLEV,KAER))
      ALLOCATE( ZPIZA_EQ_SPLIT(IDIM_EFF,KFLEV,KSWB))
      ALLOCATE( ZCGA_EQ_SPLIT(IDIM_EFF,KFLEV,KSWB))
      ALLOCATE( ZTAUREL_EQ_SPLIT(IDIM_EFF,KFLEV,KSWB))
      ALLOCATE(  ZDPRES_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZLSM_SPLIT(IDIM_EFF))
      ALLOCATE(  ZQSAVE_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZTS_SPLIT(IDIM_EFF))
      ! output pronostic       
      ALLOCATE(  ZDTLW_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZDTSW_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZFLUX_TOP_GND_IRVISNIR_SPLIT(IDIM_EFF,KFLUX))
      ALLOCATE(  ZSFSWDIR_SPLIT(IDIM_EFF,ISWB))
      ALLOCATE(  ZSFSWDIF_SPLIT(IDIM_EFF,ISWB))
      ALLOCATE(  ZDTLW_CS_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZDTSW_CS_SPLIT(IDIM_EFF,KFLEV))
      ALLOCATE(  ZFLUX_TOP_GND_IRVISNIR_CS_SPLIT(IDIM_EFF,KFLUX))
!
      ALLOCATE(  ZFLUX_LW_SPLIT(IDIM_EFF,2,KFLEV+1))
      ALLOCATE(  ZFLUX_SW_DOWN_SPLIT(IDIM_EFF,KFLEV+1))
      ALLOCATE(  ZFLUX_SW_UP_SPLIT(IDIM_EFF,KFLEV+1))
      ALLOCATE(  ZRADLP_SPLIT(IDIM_EFF,KFLEV))
      IF(KRAD_DIAG >=1) THEN
        ALLOCATE(  ZNFSW_SPLIT(IDIM_EFF,KFLEV+1))
        ALLOCATE(  ZNFLW_SPLIT(IDIM_EFF,KFLEV+1))
      ELSE
        ALLOCATE(  ZNFSW_SPLIT(0,0))
        ALLOCATE(  ZNFLW_SPLIT(0,0))
      END IF
!
      IF( KRAD_DIAG >= 2) THEN      
        ALLOCATE(  ZFLUX_SW_DOWN_CS_SPLIT(IDIM_EFF,KFLEV+1))
        ALLOCATE(  ZFLUX_SW_UP_CS_SPLIT(IDIM_EFF,KFLEV+1))
        ALLOCATE(  ZFLUX_LW_CS_SPLIT(IDIM_EFF,2,KFLEV+1))
        ALLOCATE(   ZNFSW_CS_SPLIT(IDIM_EFF,KFLEV+1))
        ALLOCATE(   ZNFLW_CS_SPLIT(IDIM_EFF,KFLEV+1))
      ELSE
        ALLOCATE(  ZFLUX_SW_DOWN_CS_SPLIT(0,0))
        ALLOCATE(  ZFLUX_SW_UP_CS_SPLIT(0,0))
        ALLOCATE(  ZFLUX_LW_CS_SPLIT(0,0,0))
        ALLOCATE(  ZNFSW_CS_SPLIT(0,0))
        ALLOCATE(  ZNFLW_CS_SPLIT(0,0))
      END IF
!
      IF( KRAD_DIAG >= 3) THEN
        ALLOCATE(  ZPLAN_ALB_VIS_SPLIT(IDIM_EFF))
        ALLOCATE(  ZPLAN_ALB_NIR_SPLIT(IDIM_EFF))
        ALLOCATE(  ZPLAN_TRA_VIS_SPLIT(IDIM_EFF))
        ALLOCATE(  ZPLAN_TRA_NIR_SPLIT(IDIM_EFF))
        ALLOCATE(  ZPLAN_ABS_VIS_SPLIT(IDIM_EFF))
        ALLOCATE(  ZPLAN_ABS_NIR_SPLIT(IDIM_EFF))
      ELSE
        ALLOCATE(  ZPLAN_ALB_VIS_SPLIT(0))
        ALLOCATE(  ZPLAN_ALB_NIR_SPLIT(0))
        ALLOCATE(  ZPLAN_TRA_VIS_SPLIT(0))
        ALLOCATE(  ZPLAN_TRA_NIR_SPLIT(0))
        ALLOCATE(  ZPLAN_ABS_VIS_SPLIT(0))
        ALLOCATE(  ZPLAN_ABS_NIR_SPLIT(0))
      END IF
!
      IF( KRAD_DIAG >= 4) THEN
        ALLOCATE(  ZEFCL_RRTM_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZCLSW_TOTAL_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZTAU_TOTAL_SPLIT(IDIM_EFF,KSWB,KFLEV))
        ALLOCATE(  ZOMEGA_TOTAL_SPLIT(IDIM_EFF,KSWB,KFLEV))
        ALLOCATE(  ZCG_TOTAL_SPLIT(IDIM_EFF,KSWB,KFLEV))
        ALLOCATE(  ZEFCL_LWD_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZEFCL_LWU_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZFLWP_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZFIWP_SPLIT(IDIM_EFF,KFLEV))
        ALLOCATE(  ZRADIP_SPLIT(IDIM_EFF,KFLEV))
      ELSE
        ALLOCATE(  ZEFCL_RRTM_SPLIT(0,0))
        ALLOCATE(  ZCLSW_TOTAL_SPLIT(0,0))
        ALLOCATE(  ZTAU_TOTAL_SPLIT(0,0,0))
        ALLOCATE(  ZOMEGA_TOTAL_SPLIT(0,0,0))
        ALLOCATE(  ZCG_TOTAL_SPLIT(0,0,0))
        ALLOCATE(  ZEFCL_LWD_SPLIT(0,0))
        ALLOCATE(  ZEFCL_LWU_SPLIT(0,0))
        ALLOCATE(  ZFLWP_SPLIT(0,0))
        ALLOCATE(  ZFIWP_SPLIT(0,0))
        ALLOCATE(  ZRADIP_SPLIT(0,0))
      END IF
!
! C2R2 coupling
!
      IF (SIZE (ZCCT_C2R2) > 0)  THEN
        ALLOCATE (ZCCT_C2R2_SPLIT(IDIM_EFF,KFLEV))
      ELSE
        ALLOCATE (ZCCT_C2R2_SPLIT(0,0))
      END IF
!
      IF (SIZE (ZCRT_C2R2) > 0)  THEN
        ALLOCATE (ZCRT_C2R2_SPLIT(IDIM_EFF,KFLEV))
      ELSE
        ALLOCATE (ZCRT_C2R2_SPLIT(0,0))
      END IF
!
      IF (SIZE (ZCIT_C1R3) > 0)  THEN
        ALLOCATE (ZCIT_C1R3_SPLIT(IDIM_EFF,KFLEV))
      ELSE
        ALLOCATE (ZCIT_C1R3_SPLIT(0,0))
      END IF
    END IF
! 
! fill the splitted arrays with their values taken from the full arrays 
!
    IBEG = IDIM-IDIM_RESIDUE+1
    IEND = IBEG+IDIM_EFF-1
!
    ZALBP_SPLIT(:,:) = ZALBP( IBEG:IEND ,:)
    ZALBD_SPLIT(:,:) = ZALBD( IBEG:IEND ,:)
    ZEMIS_SPLIT(:) = ZEMIS ( IBEG:IEND )
    ZEMIW_SPLIT(:) = ZEMIW ( IBEG:IEND )
    ZRMU0_SPLIT(:)    = ZRMU0 ( IBEG:IEND )
    ZCFAVE_SPLIT(:,:) = ZCFAVE( IBEG:IEND ,:)
    ZO3AVE_SPLIT(:,:) = ZO3AVE( IBEG:IEND ,:)
    ZT_HL_SPLIT(:,:)    = ZT_HL( IBEG:IEND ,:)
    ZPRES_HL_SPLIT(:,:) = ZPRES_HL( IBEG:IEND ,:)
    ZQLAVE_SPLIT(:,:) = ZQLAVE( IBEG:IEND , :)
    ZDZ_SPLIT(:,:) = ZDZ( IBEG:IEND , :)
    ZQIAVE_SPLIT(:,:) = ZQIAVE( IBEG:IEND ,:)
    ZQRAVE_SPLIT (:,:) = ZQRAVE (IBEG:IEND ,:)
    ZQLWC_SPLIT(:,:) = ZQLWC( IBEG:IEND , :)
    ZQIWC_SPLIT(:,:) = ZQIWC( IBEG:IEND ,:)
    ZQRWC_SPLIT(:,:) = ZQRWC (IBEG:IEND ,:)
    ZQVAVE_SPLIT(:,:) = ZQVAVE( IBEG:IEND ,:)
    ZTAVE_SPLIT(:,:)  = ZTAVE ( IBEG:IEND ,:)
    ZPAVE_SPLIT(:,:)  = ZPAVE ( IBEG:IEND ,:)
    ZAER_SPLIT (:,:,:)  = ZAER  ( IBEG:IEND ,:,:)
    IF(CAOP=='EXPL')THEN
       ZPIZA_EQ_SPLIT(:,:,:)=ZPIZA_EQ(IBEG:IEND,:,:)
       ZCGA_EQ_SPLIT(:,:,:)=ZCGA_EQ(IBEG:IEND,:,:)
       ZTAUREL_EQ_SPLIT(:,:,:)=ZTAUREL_EQ(IBEG:IEND,:,:)
    ENDIF
    ZDPRES_SPLIT(:,:)  = ZDPRES (IBEG:IEND ,:)
    ZLSM_SPLIT (:)    = ZLSM (IBEG:IEND)
    ZQSAVE_SPLIT (:,:) = ZQSAVE (IBEG:IEND ,:)
    ZTS_SPLIT (:) = ZTS (IBEG:IEND)
!
! C2R2 concentrations
    IF (SIZE (ZCCT_C2R2) > 0)  ZCCT_C2R2_SPLIT(:,:) = ZCCT_C2R2 (IBEG:IEND ,:)
    IF (SIZE (ZCRT_C2R2) > 0)  ZCRT_C2R2_SPLIT(:,:) = ZCRT_C2R2 (IBEG:IEND ,:)  
    IF (SIZE (ZCIT_C1R3) > 0)  ZCIT_C1R3_SPLIT(:,:) = ZCIT_C1R3 (IBEG:IEND ,:)
!
!  CALL the ECMWF radiation with the splitted array
!
   CALL ECMWF_RADIATION_VERS2  ( IDIM_EFF , KFLEV, KRAD_DIAG, KAER,              &    
         ZDZ_SPLIT,HEFRADL,HEFRADI,HOPWSW, HOPISW, HOPWLW, HOPILW,PFUDG,                    &
         ZRII0, ZAER_SPLIT , ZALBD_SPLIT, ZALBP_SPLIT, ZPRES_HL_SPLIT,            &
         ZPAVE_SPLIT,PCCO2, ZCFAVE_SPLIT, ZDPRES_SPLIT, ZEMIS_SPLIT, ZEMIW_SPLIT, &
         ZLSM_SPLIT, ZRMU0_SPLIT,ZO3AVE_SPLIT , ZQVAVE_SPLIT, ZQIAVE_SPLIT ,ZQIWC_SPLIT,      & 
         ZQLAVE_SPLIT,ZQLWC_SPLIT,ZQSAVE_SPLIT, ZQRAVE_SPLIT,ZQRWC_SPLIT,  ZT_HL_SPLIT,      &
         ZTAVE_SPLIT, ZTS_SPLIT, ZCCT_C2R2_SPLIT,ZCRT_C2R2_SPLIT,ZCIT_C1R3_SPLIT, & 
         ZNFLW_CS_SPLIT, ZNFLW_SPLIT, ZNFSW_CS_SPLIT,ZNFSW_SPLIT,                 &          
         ZDTLW_SPLIT, ZDTSW_SPLIT, ZFLUX_TOP_GND_IRVISNIR_SPLIT,                  &
         ZSFSWDIR_SPLIT, ZSFSWDIF_SPLIT,                                          &
         ZFLUX_SW_DOWN_SPLIT, ZFLUX_SW_UP_SPLIT, ZFLUX_LW_SPLIT ,                 &
         ZDTLW_CS_SPLIT, ZDTSW_CS_SPLIT, ZFLUX_TOP_GND_IRVISNIR_CS_SPLIT,         &
         ZFLUX_SW_DOWN_CS_SPLIT, ZFLUX_SW_UP_CS_SPLIT, ZFLUX_LW_CS_SPLIT,         & 
         ZPLAN_ALB_VIS_SPLIT,ZPLAN_ALB_NIR_SPLIT, ZPLAN_TRA_VIS_SPLIT,            &
         ZPLAN_TRA_NIR_SPLIT, ZPLAN_ABS_VIS_SPLIT, ZPLAN_ABS_NIR_SPLIT,           &
         ZEFCL_LWD_SPLIT, ZEFCL_LWU_SPLIT, ZFLWP_SPLIT,ZFIWP_SPLIT,               &
         ZRADLP_SPLIT,ZRADIP_SPLIT,ZEFCL_RRTM_SPLIT, ZCLSW_TOTAL_SPLIT,           &
         ZTAU_TOTAL_SPLIT,ZOMEGA_TOTAL_SPLIT, ZCG_TOTAL_SPLIT,                    &
         GAOP,ZPIZA_EQ_SPLIT,ZCGA_EQ_SPLIT,ZTAUREL_EQ_SPLIT  )

!
! fill the full output arrays with the splitted arrays
!
    ZDTLW( IBEG:IEND ,:)  =  ZDTLW_SPLIT(:,:)  
    ZDTSW( IBEG:IEND ,:)  =  ZDTSW_SPLIT(:,:) 
    ZFLUX_TOP_GND_IRVISNIR( IBEG:IEND ,:)=  ZFLUX_TOP_GND_IRVISNIR_SPLIT(:,:) 
    ZSFSWDIR (IBEG:IEND,:)  = ZSFSWDIR_SPLIT(:,:)
    ZSFSWDIF (IBEG:IEND,:)  = ZSFSWDIF_SPLIT(:,:)
!
    ZDTLW_CS( IBEG:IEND ,:) =  ZDTLW_CS_SPLIT(:,:)
    ZDTSW_CS( IBEG:IEND ,:) =  ZDTSW_CS_SPLIT(:,:)
    ZFLUX_TOP_GND_IRVISNIR_CS( IBEG:IEND ,:) =                     &
         ZFLUX_TOP_GND_IRVISNIR_CS_SPLIT(:,:)
    ZFLUX_LW( IBEG:IEND ,:,:)    =  ZFLUX_LW_SPLIT(:,:,:) 
    ZFLUX_SW_DOWN( IBEG:IEND ,:) =  ZFLUX_SW_DOWN_SPLIT(:,:)
    ZFLUX_SW_UP( IBEG:IEND ,:)   =  ZFLUX_SW_UP_SPLIT(:,:)
    ZRADLP( IBEG:IEND ,:) = ZRADLP_SPLIT(:,:)
    IF( OCLOSE_OUT ) THEN
      IF( KRAD_DIAG >= 1) THEN
        ZNFLW(IBEG:IEND ,:)= ZNFLW_SPLIT(:,:)
        ZNFSW(IBEG:IEND ,:)= ZNFSW_SPLIT(:,:)
        IF( KRAD_DIAG >= 2) THEN
          ZFLUX_SW_DOWN_CS( IBEG:IEND ,:) = ZFLUX_SW_DOWN_CS_SPLIT(:,:)
          ZFLUX_SW_UP_CS( IBEG:IEND ,:)   = ZFLUX_SW_UP_CS_SPLIT(:,:)
          ZFLUX_LW_CS( IBEG:IEND ,:,:)    = ZFLUX_LW_CS_SPLIT(:,:,:)
          ZNFLW_CS(IBEG:IEND ,:)= ZNFLW_CS_SPLIT(:,:)
          ZNFSW_CS(IBEG:IEND ,:)= ZNFSW_CS_SPLIT(:,:)
          IF( KRAD_DIAG >= 3) THEN
            ZPLAN_ALB_VIS( IBEG:IEND ) = ZPLAN_ALB_VIS_SPLIT(:)
            ZPLAN_ALB_NIR( IBEG:IEND ) = ZPLAN_ALB_NIR_SPLIT(:)
            ZPLAN_TRA_VIS( IBEG:IEND ) = ZPLAN_TRA_VIS_SPLIT(:)
            ZPLAN_TRA_NIR( IBEG:IEND ) = ZPLAN_TRA_NIR_SPLIT(:)
            ZPLAN_ABS_VIS( IBEG:IEND ) = ZPLAN_ABS_VIS_SPLIT(:)
            ZPLAN_ABS_NIR( IBEG:IEND ) = ZPLAN_ABS_NIR_SPLIT(:)          
            IF( KRAD_DIAG >= 4) THEN
              ZEFCL_LWD( IBEG:IEND ,:) = ZEFCL_LWD_SPLIT(:,:)
              ZEFCL_LWU( IBEG:IEND ,:)   = ZEFCL_LWU_SPLIT(:,:)
              ZFLWP( IBEG:IEND ,:) = ZFLWP_SPLIT(:,:)
              ZFIWP( IBEG:IEND ,:) = ZFIWP_SPLIT(:,:)
              ZRADIP( IBEG:IEND ,:) = ZRADIP_SPLIT(:,:)
              ZEFCL_RRTM( IBEG:IEND ,:) = ZEFCL_RRTM_SPLIT(:,:)
              ZCLSW_TOTAL( IBEG:IEND ,:) = ZCLSW_TOTAL_SPLIT(:,:)
              ZTAU_TOTAL( IBEG:IEND ,:,:)  = ZTAU_TOTAL_SPLIT(:,:,:)
              ZOMEGA_TOTAL( IBEG:IEND ,:,:)= ZOMEGA_TOTAL_SPLIT(:,:,:)
              ZCG_TOTAL( IBEG:IEND ,:,:)   = ZCG_TOTAL_SPLIT(:,:,:)                
            END IF
          END IF
        END IF
      END IF
    END IF
!
    IDIM_RESIDUE = IDIM_RESIDUE - IDIM_EFF
!
! desallocation of the splitted arrays
!
    IF( JI_SPLIT >= INUM_CALL-1 ) THEN
      DEALLOCATE(  ZALBP_SPLIT )
      DEALLOCATE(  ZALBD_SPLIT )  
      DEALLOCATE(  ZEMIS_SPLIT  )
      DEALLOCATE(  ZEMIW_SPLIT  )
      DEALLOCATE(  ZRMU0_SPLIT      )
      DEALLOCATE(  ZCFAVE_SPLIT     )
      DEALLOCATE(  ZO3AVE_SPLIT     )
      DEALLOCATE(  ZT_HL_SPLIT      )
      DEALLOCATE(  ZPRES_HL_SPLIT   )
      DEALLOCATE(  ZDZ_SPLIT     )
      DEALLOCATE(  ZQLAVE_SPLIT     )
      DEALLOCATE(  ZQIAVE_SPLIT     )
      DEALLOCATE(  ZQVAVE_SPLIT     )
      DEALLOCATE(  ZTAVE_SPLIT      )
      DEALLOCATE(  ZPAVE_SPLIT      )
      DEALLOCATE(  ZAER_SPLIT       )
      DEALLOCATE(  ZDPRES_SPLIT     )
      DEALLOCATE(  ZLSM_SPLIT       )
      DEALLOCATE(  ZQSAVE_SPLIT     )
      DEALLOCATE(  ZQRAVE_SPLIT  )
      DEALLOCATE(  ZQLWC_SPLIT     )
      DEALLOCATE(  ZQRWC_SPLIT     )
      DEALLOCATE(  ZQIWC_SPLIT     )
      DEALLOCATE(   ZCCT_C2R2_SPLIT  )
      DEALLOCATE(   ZCRT_C2R2_SPLIT  )
      DEALLOCATE(   ZCIT_C1R3_SPLIT  )
      DEALLOCATE(  ZTS_SPLIT    )
      DEALLOCATE(   ZNFLW_CS_SPLIT)
      DEALLOCATE(   ZNFLW_SPLIT)
      DEALLOCATE(   ZNFSW_CS_SPLIT)
      DEALLOCATE(   ZNFSW_SPLIT)
      DEALLOCATE(ZDTLW_SPLIT)
      DEALLOCATE(ZDTSW_SPLIT)
      DEALLOCATE(ZFLUX_TOP_GND_IRVISNIR_SPLIT)
      DEALLOCATE(ZSFSWDIR_SPLIT)
      DEALLOCATE(ZSFSWDIF_SPLIT)
      DEALLOCATE(ZFLUX_SW_DOWN_SPLIT)
      DEALLOCATE(ZFLUX_SW_UP_SPLIT)
      DEALLOCATE(ZFLUX_LW_SPLIT)
      DEALLOCATE(ZDTLW_CS_SPLIT)
      DEALLOCATE(ZDTSW_CS_SPLIT)
      DEALLOCATE(ZFLUX_TOP_GND_IRVISNIR_CS_SPLIT)
      DEALLOCATE(ZPLAN_ALB_VIS_SPLIT)
      DEALLOCATE(ZPLAN_ALB_NIR_SPLIT)
      DEALLOCATE(ZPLAN_TRA_VIS_SPLIT)
      DEALLOCATE(ZPLAN_TRA_NIR_SPLIT)
      DEALLOCATE(ZPLAN_ABS_VIS_SPLIT)
      DEALLOCATE(ZPLAN_ABS_NIR_SPLIT)
      DEALLOCATE(ZEFCL_LWD_SPLIT)
      DEALLOCATE(ZEFCL_LWU_SPLIT)
      DEALLOCATE(ZFLWP_SPLIT)
      DEALLOCATE(ZRADLP_SPLIT)
      DEALLOCATE(ZRADIP_SPLIT)
      DEALLOCATE(ZFIWP_SPLIT)
      DEALLOCATE(ZEFCL_RRTM_SPLIT)
      DEALLOCATE(ZCLSW_TOTAL_SPLIT)
      DEALLOCATE(ZTAU_TOTAL_SPLIT)
      DEALLOCATE(ZOMEGA_TOTAL_SPLIT)
      DEALLOCATE(ZCG_TOTAL_SPLIT)
      DEALLOCATE(ZFLUX_SW_DOWN_CS_SPLIT)
      DEALLOCATE(ZFLUX_SW_UP_CS_SPLIT)
      DEALLOCATE(ZFLUX_LW_CS_SPLIT)
      DEALLOCATE(ZPIZA_EQ_SPLIT)
      DEALLOCATE(ZCGA_EQ_SPLIT)
      DEALLOCATE(ZTAUREL_EQ_SPLIT)
    END IF
  END DO
END IF

!
DEALLOCATE(ZTAVE)
DEALLOCATE(ZPAVE)
DEALLOCATE(ZQVAVE)
DEALLOCATE(ZQLAVE)
DEALLOCATE(ZDZ)
DEALLOCATE(ZQIAVE)
DEALLOCATE(ZCFAVE)
DEALLOCATE(ZPRES_HL)
DEALLOCATE(ZT_HL)
DEALLOCATE(ZRMU0) 
DEALLOCATE(ZLSM)
DEALLOCATE(ZQSAVE)
DEALLOCATE(ZAER)
DEALLOCATE(ZPIZA_EQ)
DEALLOCATE(ZCGA_EQ)
DEALLOCATE(ZTAUREL_EQ)
DEALLOCATE(ZDPRES)
DEALLOCATE(ZCCT_C2R2)
DEALLOCATE(ZCRT_C2R2)
DEALLOCATE(ZCIT_C1R3)
!
DEALLOCATE(ZTS)
DEALLOCATE(ZALBP)
DEALLOCATE(ZALBD)
DEALLOCATE(ZEMIS)
DEALLOCATE(ZEMIW)
DEALLOCATE(ZQRAVE)
DEALLOCATE(ZQLWC)
DEALLOCATE(ZQIWC)
DEALLOCATE(ZQRWC)
DEALLOCATE(ICLEAR_2D_TM1)
!
!*       5.6   UNCOMPRESSES THE OUTPUT FIELD IN CASE OF 
!                      CLEAR-SKY APPROXIMATION
!
IF(OCLEAR_SKY .OR. OCLOUD_ONLY) THEN
  ALLOCATE(ZWORK1(ICLOUD))
  ALLOCATE(ZWORK2(ICLOUD+KFLEV)) !       allocation for the KFLEV levels of 
  ALLOCATE(ZWORK4(KFLEV,KDLON))
  ZWORK2(:) = PACK( TRANSPOSE(ZDTLW(:,:)),MASK=.TRUE. )
!
  DO JK=1,KFLEV
    ZWORK4(JK,:) = ZWORK2(ICLOUD+JK)
  END DO
  ZWORK1(1:ICLOUD) = ZWORK2(1:ICLOUD)
  ZZDTLW(:,:) = TRANSPOSE( UNPACK( ZWORK1(:),MASK=GCLOUDT(:,:)   &
       ,FIELD=ZWORK4(:,:) ) )
  !
  ZWORK2(:) = PACK( TRANSPOSE(ZDTSW(:,:)),MASK=.TRUE. )
  DO JK=1,KFLEV
    ZWORK4(JK,:) = ZWORK2(ICLOUD+JK)
  END DO
  ZWORK1(1:ICLOUD) = ZWORK2(1:ICLOUD)
  ZZDTSW(:,:) = TRANSPOSE( UNPACK( ZWORK1(:),MASK=GCLOUDT(:,:)   &
       ,FIELD=ZWORK4(:,:) ) )
  !
  DEALLOCATE(ZWORK1)
  DEALLOCATE(ZWORK2)
  DEALLOCATE(ZWORK4)
  !
  ZZTGVISC   = ZFLUX_TOP_GND_IRVISNIR(ICLOUD_COL+1,5)
  !
  ZZTGVIS(:) = UNPACK( ZFLUX_TOP_GND_IRVISNIR(:,5),MASK=.NOT.GCLEAR_2D(:), &
       FIELD=ZZTGVISC  )
  ZZTGNIRC   = ZFLUX_TOP_GND_IRVISNIR(ICLOUD_COL+1,6)
  !
  ZZTGNIR(:) = UNPACK( ZFLUX_TOP_GND_IRVISNIR(:,6),MASK=.NOT.GCLEAR_2D(:), &
       FIELD=ZZTGNIRC )
  ZZTGIRC    = ZFLUX_TOP_GND_IRVISNIR(ICLOUD_COL+1,4)
  !
  ZZTGIR (:) = UNPACK( ZFLUX_TOP_GND_IRVISNIR(:,4),MASK=.NOT.GCLEAR_2D(:), &
       FIELD=ZZTGIRC  )
  !
  DO JSWB=1,ISWB
    ZZSFSWDIRC(JSWB) = ZSFSWDIR (ICLOUD_COL+1,JSWB)
    !
    ZZSFSWDIR(:,JSWB) =  UNPACK(ZSFSWDIR (:,JSWB),MASK=.NOT.GCLEAR_2D(:), &
         FIELD= ZZSFSWDIRC(JSWB)  ) 
    !
    ZZSFSWDIFC(JSWB) = ZSFSWDIF (ICLOUD_COL+1,JSWB)
    !
    ZZSFSWDIF(:,JSWB) =  UNPACK(ZSFSWDIF (:,JSWB),MASK=.NOT.GCLEAR_2D(:), &
         FIELD= ZZSFSWDIFC(JSWB)  )
  END DO
!
!  No cloud case
!
  IF( GNOCL ) THEN
          IF (SIZE(ZZDTLW,1)>1) THEN
             ZZDTLW(1,:)= ZZDTLW(2,:)
          ENDIF
          IF (SIZE(ZZDTSW,1)>1) THEN
             ZZDTSW(1,:)= ZZDTSW(2,:)
          ENDIF
    ZZTGVIS(1) = ZZTGVISC
    ZZTGNIR(1) = ZZTGNIRC
    ZZTGIR(1)  = ZZTGIRC
    ZZSFSWDIR(1,:) =  ZZSFSWDIRC(:)
    ZZSFSWDIF(1,:) =  ZZSFSWDIFC(:)
  END IF
ELSE
  ZZDTLW(:,:) = ZDTLW(:,:)
  ZZDTSW(:,:) = ZDTSW(:,:)
  ZZTGVIS(:)  = ZFLUX_TOP_GND_IRVISNIR(:,5)
  ZZTGNIR(:)  = ZFLUX_TOP_GND_IRVISNIR(:,6)
  ZZTGIR(:)   = ZFLUX_TOP_GND_IRVISNIR(:,4)
  ZZSFSWDIR(:,:) =  ZSFSWDIR(:,:)
  ZZSFSWDIF(:,:) =  ZSFSWDIF(:,:) 
END IF
!
DEALLOCATE(ZDTLW)
DEALLOCATE(ZDTSW)
DEALLOCATE(ZSFSWDIR)
DEALLOCATE(ZSFSWDIF)
!
!-------------------------------------------------------------------------------
!
!*       6.    COMPUTES THE RADIATIVE SOURCES AND THE DOWNWARD SURFACE FLUXES
!              --------------------------------------------------------------
!
!  Computes the SW and LW radiative tendencies
!  note : tendencies in K/s for MNH   
!
ZDTRAD_LW(:,:,:)=0.0
ZDTRAD_SW(:,:,:)=0.0
DO JK=IKB,IKE
  JKRAD= JK-JPVEXT
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
      IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
      ZDTRAD_LW(JI,JJ,JK) = ZZDTLW(IIJ,JKRAD)/XDAY
      ZDTRAD_SW(JI,JJ,JK) = ZZDTSW(IIJ,JKRAD)/XDAY      
    END DO
  END DO
END DO
!
!  Computes the downward SW and LW surface fluxes + diffuse and direct contribution
!
ZLWD(:,:)=0.
ZSWDDIR(:,:,:)=0.
ZSWDDIF(:,:,:)=0.
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
    ZLWD(JI,JJ) = ZZTGIR(IIJ)
    ZSWDDIR(JI,JJ,:) = ZZSFSWDIR (IIJ,:)
    ZSWDDIF(JI,JJ,:) = ZZSFSWDIF (IIJ,:)
  END DO
END DO
!
!final  THETA_radiative tendency and surface fluxes 
!
IF(OCLOUD_ONLY) THEN
  !
  ZWORKL(:,:) = SUM(PCLDFR(:,:,:),DIM=3) > 0.0
  DO JK = IKB,IKE
    WHERE( ZWORKL(:,:) )
      PDTHRAD(:,:,JK) = (ZDTRAD_LW(:,:,JK)+ZDTRAD_SW(:,:,JK))/ZEXNT(:,:,JK)
    ENDWHERE
  END DO
  !
  WHERE( ZWORKL(:,:) )
    PSRFLWD(:,:) = ZLWD(:,:)
  ENDWHERE
  DO JSWB=1,ISWB
    WHERE( ZWORKL(:,:) )
      PSRFSWD_DIR (:,:,JSWB) = ZSWDDIR(:,:,JSWB)
      PSRFSWD_DIF (:,:,JSWB) = ZSWDDIF(:,:,JSWB)
    END WHERE
  END DO
ELSE
  PDTHRAD(:,:,:) = (ZDTRAD_LW(:,:,:)+ZDTRAD_SW(:,:,:))/ZEXNT(:,:,:)
  PDTHRADSW(:,:,:) = ZDTRAD_SW(:,:,:)/ZEXNT(:,:,:)
  PDTHRADLW(:,:,:) = ZDTRAD_LW(:,:,:)/ZEXNT(:,:,:)
  PSRFLWD(:,:) = ZLWD(:,:)
  DO JSWB=1,ISWB
    PSRFSWD_DIR (:,:,JSWB) = ZSWDDIR(:,:,JSWB)
    PSRFSWD_DIF (:,:,JSWB) = ZSWDDIF(:,:,JSWB)
  END DO
!
!sw and lw fluxes 
!
  DO JK=IKB,IKE
   JKRAD = JK - JPVEXT
   DO JJ=IJB,IJE
    DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          PSWU(JI,JJ,JK) = ZFLUX_SW_UP(IIJ,JKRAD)
          PSWD(JI,JJ,JK) = ZFLUX_SW_DOWN(IIJ,JKRAD)
          PLWU(JI,JJ,JK) = ZFLUX_LW(IIJ,1,JKRAD)
          PLWD(JI,JJ,JK) = -ZFLUX_LW(IIJ,2,JKRAD)
    END DO
   END DO
  END DO
!!!effective radius
  DO JK=IKB,IKE
   JKRAD = JK - JPVEXT
   DO JJ=IJB,IJE
    DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          PRADEFF(JI,JJ,JK) = ZRADLP(IIJ,JKRAD)
    END DO
   END DO
  END DO
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       7.    STORE SOME ADDITIONNAL RADIATIVE FIELDS
!              ---------------------------------------
!
IF( OCLOSE_OUT .AND. (KRAD_DIAG >= 1) ) THEN
  ZSTORE_3D(:,:,:) = 0.0
  ZSTORE_3D2(:,:,:) = 0.0
  ZSTORE_2D(:,:)   = 0.0
  !
  IF( KRAD_DIAG >= 1) THEN
    !
    CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
    WRITE(UNIT=ILUOUT,FMT='(/," STORE ADDITIONNAL RADIATIVE FIELDS:", &
         & " KRAD_DIAG=",I1,/)') KRAD_DIAG
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_SW_DOWN(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YDIR='XY'
    YRECFM   = 'SWF_DOWN'
    YCOMMENT = 'X_Y_Z_SWF_DOWN (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_SW_UP(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'SWF_UP'
    YCOMMENT = 'X_Y_Z_SWF_UP (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = -ZFLUX_LW(IIJ,2,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_DOWN'
    YCOMMENT = 'X_Y_Z_LWF_DOWN (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_LW(IIJ,1,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_UP'
    YCOMMENT = 'X_Y_Z_LWF_UP (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZNFLW(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_NET'
    YCOMMENT = 'X_Y_Z_LWF_NET (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZNFSW(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'SWF_NET'
    YCOMMENT = 'X_Y_Z_SWF_NET (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = ZDTRAD_LW (JI,JJ,JK)*XDAY
        END DO
      END DO
    END DO
    YRECFM   = 'DTRAD_LW'
    YCOMMENT = 'X_Y_Z_DTRAD_LW (K/DAY)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JK=IKB,IKE
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = ZDTRAD_SW (JI,JJ,JK)*XDAY
        END DO
      END DO
    END DO
    YRECFM   = 'DTRAD_SW'
    YCOMMENT = 'X_Y_Z_DTRAD_SW (K/DAY)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR(IIJ,5)
      END DO
    END DO
    YRECFM   = 'RADSWD_VIS'
    YCOMMENT = 'X_Y_RADSWD_VIS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR(IIJ,6)
      END DO
    END DO
    YRECFM   = 'RADSWD_NIR'
    YCOMMENT = 'X_Y_Z_RADSWD_NIR'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR(IIJ,4)
      END DO
    END DO
    YRECFM   = 'RADLWD'
    YCOMMENT = 'X_Y_RADLWD'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  !
  IF( KRAD_DIAG >= 2) THEN
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_SW_DOWN_CS(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YDIR='XY'
    YRECFM   = 'SWF_DOWN_CS'
    YCOMMENT = 'X_Y_Z_SWF_DOWN_CS (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_SW_UP_CS(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'SWF_UP_CS'
    YCOMMENT = 'X_Y_Z_SWF_UP_CS (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = -ZFLUX_LW_CS(IIJ,2,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_DOWN_CS'
    YCOMMENT = 'X_Y_Z_LWF_DOWN (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLUX_LW_CS(IIJ,1,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_UP_CS'
    YCOMMENT = 'X_Y_Z_LWF_UP_CS (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZNFLW_CS(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'LWF_NET_CS'
    YCOMMENT = 'X_Y_Z_SWF_NET_CS (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZNFSW_CS(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'SWF_NET_CS'
    YCOMMENT = 'X_Y_Z_SWF_NET_CS (W/M2)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK-JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZDTSW_CS(IIJ,JKRAD) 
        END DO
      END DO
    END DO
    YRECFM   = 'DTRAD_SW_CS'
    YCOMMENT = 'X_Y_Z_DTRAD_SW_CS (K/DAY)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK-JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZDTLW_CS(IIJ,JKRAD) 
        END DO
      END DO
    END DO
    YRECFM   = 'DTRAD_LW_CS'
    YCOMMENT = 'X_Y_Z_DTRAD_LW_CS (K/DAY)'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR_CS(IIJ,5)
      END DO
    END DO
    YRECFM   = 'RADSWD_VIS_CS'
    YCOMMENT = 'X_Y_RADSWD_VIS_CS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR_CS(IIJ,6)
      END DO
    END DO
    YRECFM   = 'RADSWD_NIR_CS'
    YCOMMENT = 'X_Y_RADSWD_NIR_CS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZFLUX_TOP_GND_IRVISNIR_CS(IIJ,4)
      END DO
    END DO
    YRECFM   = 'RADLWD_CS'
    YCOMMENT = 'X_Y_RADLWD_CS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
  !
  !
  IF( KRAD_DIAG >= 3) THEN
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_ALB_VIS(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_ALB_VIS'
    YCOMMENT = 'X_Y_PLAN_ALB_VIS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_ALB_NIR(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_ALB_NIR'
    YCOMMENT = 'X_Y_PLAN_ALB_NIR'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_TRA_VIS(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_TRA_VIS'
    YCOMMENT = 'X_Y_PLAN_TRA_VIS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_TRA_NIR(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_TRA_NIR'
    YCOMMENT = 'X_Y_PLAN_TRA_NIR'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_ABS_VIS(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_ABS_VIS'
    YCOMMENT = 'X_Y_PLAN_ABS_VIS'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JJ=IJB,IJE
      DO JI=IIB,IIE
        IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
        ZSTORE_2D(JI,JJ) = ZPLAN_ABS_NIR(IIJ)
      END DO
    END DO
    YRECFM   = 'PLAN_ABS_NIR'
    YCOMMENT = 'X_Y_PLAN_ABS_NIR'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_2D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    !
  END IF
!
!
  IF( KRAD_DIAG >= 4) THEN
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZEFCL_LWD(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'EFNEB_DOWN'
    YCOMMENT = 'X_Y_Z_EFNEB_DOWN'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZEFCL_LWU(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'EFNEB_UP'
    YCOMMENT = 'X_Y_Z_EFNEB_UP'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFLWP(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'FLWP'
    YCOMMENT = 'X_Y_Z_FLWP'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZFIWP(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'FIWP'
    YCOMMENT = 'X_Y_Z_FIWP'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZRADLP(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'EFRADL'
    YCOMMENT = 'X_Y_Z_RAD_microm'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
 
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZRADIP(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'EFRADI'
    YCOMMENT = 'X_Y_Z_RAD_microm'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
 !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZCLSW_TOTAL(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'SW_NEB'
    YCOMMENT = 'X_Y_Z_SW_NEB'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZEFCL_RRTM(IIJ,JKRAD)
        END DO
      END DO
    END DO
    YRECFM   = 'RRTM_LW_NEB'
    YCOMMENT = 'X_Y_Z_LW_NEB'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    ! spectral bands
    IF (KSWB==6) THEN
      INIR = 4
    ELSE
      INIR = 2
    END IF

    DO JBAND=1,INIR-1
      WRITE(YBAND_NAME(JBAND),'(A3,I1)') 'VIS', JBAND
    END DO
    DO JBAND= INIR, KSWB
      WRITE(YBAND_NAME(JBAND),'(A3,I1)') 'NIR', JBAND
    END DO
!
    DO JBAND=1,KSWB
      YRECFM   = 'ODAER_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_OD_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZTAUAZ(:,:,:,JBAND),IGRID,ILENCH,YCOMMENT,IRESP)
      YRECFM   = 'SSAAER_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_SSA_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZPIZAZ(:,:,:,JBAND),IGRID,ILENCH,YCOMMENT,IRESP)
      YRECFM   = 'GAER_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_G_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZCGAZ(:,:,:,JBAND),IGRID,ILENCH,YCOMMENT,IRESP)
    ENDDO

    DO JBAND=1,KSWB
      DO JK=IKB,IKE
        JKRAD = JK - JPVEXT
        DO JJ=IJB,IJE
          DO JI=IIB,IIE
            IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
            ZSTORE_3D(JI,JJ,JK) = ZTAU_TOTAL(IIJ,JBAND,JKRAD)
          END DO
        END DO
      END DO
      YRECFM   = 'OTH_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_OTH_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
      !
      DO JK=IKB,IKE
        JKRAD = JK - JPVEXT
        DO JJ=IJB,IJE
          DO JI=IIB,IIE
            IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
            ZSTORE_3D(JI,JJ,JK) = ZOMEGA_TOTAL(IIJ,JBAND,JKRAD)
          END DO
        END DO
      END DO
      YRECFM   = 'SSA_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_SSA_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
      !
      DO JK=IKB,IKE
        JKRAD = JK - JPVEXT
        DO JJ=IJB,IJE
          DO JI=IIB,IIE
            IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
            ZSTORE_3D(JI,JJ,JK) = ZCG_TOTAL(IIJ,JBAND,JKRAD)
          END DO
        END DO
      END DO
      YRECFM   = 'ASF_'//YBAND_NAME(JBAND)
      YCOMMENT = 'X_Y_Z_ASF_'//YBAND_NAME(JBAND)
      IGRID    = 1
      ILENCH   = LEN(YCOMMENT)
      CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
    END DO
  END IF
  !
  !
  IF (KRAD_DIAG >= 5)   THEN
!
! OZONE and AER optical thickness climato  entering the ecmwf_radiation_vers2
! note the vertical grid is re-inversed for graphic !   
    DO JK=IKB,IKE
      JKRAD = KFLEV+1 - JK + JPVEXT               
      DO JJ=IJB,IJE
        DO JI=IIB,IIE 
          IIJ = 1 + (JI-IIB) + (IIE-IIB+1)*(JJ-IJB)
          ZSTORE_3D(JI,JJ,JK) = ZO3AVE(IIJ, JKRAD)
        END DO
      END DO
    END DO
    YDIR='XY'
    YRECFM   = 'O3CLIM'
    YCOMMENT = 'X_Y_Z_O3 Pa/Pa'
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D,IGRID,ILENCH,YCOMMENT,IRESP)
! 
!cumulated optical thickness of aerosols
!cumul begin from the top of the domain, not from the TOA !      
!
!land 
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,1)
        END DO
      END DO
    END DO
!
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
    YDIR='XY'
    YRECFM   = 'CUM_AER_LAND'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
!
! sea
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,2)
        END DO
      END DO
    END DO
!sum
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
!
    YDIR='XY'
    YRECFM   = 'CUM_AER_SEA'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
!
! desert
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,3)
        END DO
      END DO
    END DO
!sum     
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
!    
    YDIR='XY'
    YRECFM   = 'CUM_AER_DES'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
!
! urban
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,4)
        END DO
      END DO
    END DO
!sum      
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
!
    YDIR='XY'
    YRECFM   = 'CUM_AER_URB'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
!
! Volcanoes
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,5)
        END DO
      END DO
    END DO
!sum         
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
!
    YDIR='XY'
    YRECFM   = 'CUM_AER_VOL'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
!
! stratospheric background
    DO JK=IKB,IKE
      JKRAD = JK - JPVEXT
      DO JJ=IJB,IJE
        DO JI=IIB,IIE
          ZSTORE_3D(JI,JJ,JK) = PAER(JI,JJ,JKRAD,6)
        END DO
      END DO
    END DO
!sum      
    ZSTORE_2D (:,:) = 0.
    DO JK=IKB,IKE
      JK1=IKE-JK+IKB 
      ZSTORE_2D(:,:) = ZSTORE_2D(:,:) + ZSTORE_3D(:,:,JK1)
      ZSTORE_3D2(:,:,JK1) = ZSTORE_2D(:,:)  
    END DO
!
    YDIR='XY'
    YRECFM   = 'CUM_AER_STRB'
    YCOMMENT = 'X_Y_Z_CUM_AER_OPT' 
    IGRID    = 1
    ILENCH   = LEN(YCOMMENT)
    CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,YDIR,ZSTORE_3D2,IGRID,ILENCH,YCOMMENT,IRESP)
  ENDIF
END IF
!

DEALLOCATE(ZNFLW_CS)
DEALLOCATE(ZNFLW)
DEALLOCATE(ZNFSW_CS)
DEALLOCATE(ZNFSW)
DEALLOCATE(ZFLUX_TOP_GND_IRVISNIR)
DEALLOCATE(ZFLUX_SW_DOWN)
DEALLOCATE(ZFLUX_SW_UP)
DEALLOCATE(ZFLUX_LW)
DEALLOCATE(ZDTLW_CS)
DEALLOCATE(ZDTSW_CS)
DEALLOCATE(ZFLUX_TOP_GND_IRVISNIR_CS)
DEALLOCATE(ZPLAN_ALB_VIS)
DEALLOCATE(ZPLAN_ALB_NIR)
DEALLOCATE(ZPLAN_TRA_VIS)
DEALLOCATE(ZPLAN_TRA_NIR)
DEALLOCATE(ZPLAN_ABS_VIS)
DEALLOCATE(ZPLAN_ABS_NIR)
DEALLOCATE(ZEFCL_LWD)
DEALLOCATE(ZEFCL_LWU)
DEALLOCATE(ZFLWP)
DEALLOCATE(ZFIWP)
DEALLOCATE(ZRADLP)
DEALLOCATE(ZRADIP)
DEALLOCATE(ZEFCL_RRTM)
DEALLOCATE(ZCLSW_TOTAL)
DEALLOCATE(ZTAU_TOTAL)
DEALLOCATE(ZOMEGA_TOTAL)
DEALLOCATE(ZCG_TOTAL)
DEALLOCATE(ZFLUX_SW_DOWN_CS)
DEALLOCATE(ZFLUX_SW_UP_CS)
DEALLOCATE(ZFLUX_LW_CS)
DEALLOCATE(ZO3AVE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE RADIATIONS

