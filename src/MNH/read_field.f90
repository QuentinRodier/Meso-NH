!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######################
      MODULE MODI_READ_FIELD
!     ######################
!
INTERFACE 
!
      SUBROUTINE READ_FIELD(HINIFILE,HLUOUT,KMASDEV,KIU,KJU,KKU,PTSTEP,      &
            HGETTKET,HGETRVT,HGETRCT,HGETRRT,HGETRIT,HGETCIT,                &
            HGETRST,HGETRGT,HGETRHT,HGETSVT,HGETSRCT,HGETSIGS,HGETCLDFR,     &
            HGETBL_DEPTH,HGETSBL_DEPTH,HGETPHC,HGETPHR,HUVW_ADV_SCHEME,      &
            HTEMP_SCHEME,KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,               &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
            PUM,PVM,PWM,PDUM,PDVM,PDWM,                                                     &
            PUT,PVT,PWT,PTHT,PPABST,PPABSM,PTKET,PRTKEMS,                    &
            PRT,PSVT,PCIT,PDRYMASST,    &
            PSIGS,PSRCT,PCLDFR,PBL_DEPTH,PSBL_DEPTH,PWTHVMF,PPHC,PPHR,       &
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,                                 &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            KFRC,TPDTFRC,PUFRC,PVFRC,PWFRC,PTHFRC,PRVFRC,                    &
            PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC,PPGROUNDFRC,PATC,        &
            KADVFRC,TPDTADVFRC,PDTHFRC,PDRVFRC,                              &
            KRELFRC,TPDTRELFRC, PTHREL, PRVREL,                              &
            PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M,                              &
            PRUS_PRES,PRVS_PRES,PRWS_PRES,PRTHS_CLD,PRRS_CLD,PRSVS_CLD       )
!
USE MODD_TIME ! for type DATE_TIME
!
!
CHARACTER (LEN=*),         INTENT(IN)  :: HINIFILE       
                             ! name of the initial file
CHARACTER (LEN=*),         INTENT(IN)  :: HLUOUT        
                             ! name for output-listing of nested models
INTEGER,                   INTENT(IN)  :: KMASDEV
                             ! version of the input file
INTEGER,                   INTENT(IN)  :: KIU, KJU, KKU   
                             ! array sizes in x, y and z  directions
REAL,                      INTENT(IN)  :: PTSTEP       
                             ! current Time STEP   
! 
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKET,                          &
                                          HGETRVT,HGETRCT,HGETRRT,           &
                                          HGETRIT,HGETRST,HGETRGT,HGETRHT,   & 
                                          HGETCIT,HGETSRCT,                  &
                                          HGETSIGS,HGETCLDFR,HGETBL_DEPTH,   &
                                          HGETSBL_DEPTH,HGETPHC,HGETPHR
CHARACTER (LEN=*), DIMENSION(:),INTENT(IN)  :: HGETSVT
!
! GET indicators to know wether a given  variable should or not be read in the
! FM file at time t-deltat and t
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME ! advection scheme for wind
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME ! advection scheme for wind
!
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN):: KSIZELBYTKE_ll                ! for TKE
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV 
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUM,PVM,PWM     ! U,V,W at t-dt
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PDUM,PDVM,PDWM  ! Difference on U,V,W 
                                                          ! between t+dt and t-dt
REAL, DIMENSION(:,:),      INTENT(OUT) :: PBL_DEPTH       ! BL depth
REAL, DIMENSION(:,:),      INTENT(OUT) :: PSBL_DEPTH      ! SBL depth
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PWTHVMF         ! MassFlux buoyancy flux
!
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUT,PVT,PWT     ! U,V,W at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PTHT,PTKET      ! theta, tke and
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PRTKEMS         ! tke adv source
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPABST          ! pressure at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPABSM          ! pressure at t-1
REAL, DIMENSION(:,:,:,:),  INTENT(OUT) :: PRT,PSVT        ! moist and scalar
                                                          ! variables at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PSRCT           ! turbulent flux
                                                          !  <s'Rc'> at t 
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PCIT            ! ice conc. at t
REAL,                      INTENT(OUT) :: PDRYMASST       ! Md(t)
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PSIGS           ! =sqrt(<s's'>) for the
                                                          ! Subgrid Condensation
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PCLDFR          ! cloud fraction  
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPHC            ! pH value in cloud water  
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPHR            ! pH value in rainwater  
! Larger Scale fields
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLSUM,PLSVM,PLSWM    ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLSTHM,  PLSRVM      ! Mass
! LB fields
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! Forcing fields
INTEGER,              INTENT(IN)  :: KFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTFRC ! date of forcing profs.
REAL, DIMENSION(:,:), INTENT(OUT) :: PUFRC,PVFRC,PWFRC ! forcing variables
REAL, DIMENSION(:,:), INTENT(OUT) :: PTHFRC,PRVFRC
REAL, DIMENSION(:,:), INTENT(OUT) :: PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC
REAL, DIMENSION(:),   INTENT(OUT) :: PPGROUNDFRC
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: PATC
INTEGER,              INTENT(IN)  :: KADVFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTADVFRC ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),   INTENT(OUT) :: PDTHFRC, PDRVFRC
INTEGER,              INTENT(IN)  :: KRELFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTRELFRC ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),   INTENT(OUT) :: PTHREL, PRVREL
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M ! Eddy fluxes
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRUS_PRES, PRVS_PRES, PRWS_PRES
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS_CLD, PRSVS_CLD
!
!
END SUBROUTINE READ_FIELD
!
END INTERFACE
!
END MODULE MODI_READ_FIELD
!     ######spl
      SUBROUTINE READ_FIELD(HINIFILE,HLUOUT,KMASDEV,KIU,KJU,KKU,PTSTEP,      &
            HGETTKET,HGETRVT,HGETRCT,HGETRRT,HGETRIT,HGETCIT,                &
            HGETRST,HGETRGT,HGETRHT,HGETSVT,HGETSRCT,HGETSIGS,HGETCLDFR,     &
            HGETBL_DEPTH,HGETSBL_DEPTH,HGETPHC,HGETPHR,HUVW_ADV_SCHEME,      &
            HTEMP_SCHEME,KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,               &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
            PUM,PVM,PWM,PDUM,PDVM,PDWM,                                                     &
            PUT,PVT,PWT,PTHT,PPABST,PPABSM,PTKET,PRTKEMS,                    &
            PRT,PSVT,PCIT,PDRYMASST,    &
            PSIGS,PSRCT,PCLDFR,PBL_DEPTH,PSBL_DEPTH,PWTHVMF,PPHC,PPHR,       &
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,                                 &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            KFRC,TPDTFRC,PUFRC,PVFRC,PWFRC,PTHFRC,PRVFRC,                    &
            PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC,PPGROUNDFRC,PATC,        &
            KADVFRC,TPDTADVFRC,PDTHFRC,PDRVFRC,                              &
            KRELFRC,TPDTRELFRC, PTHREL, PRVREL,                              &
            PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M,                              &
            PRUS_PRES,PRVS_PRES,PRWS_PRES,PRTHS_CLD,PRRS_CLD, PRSVS_CLD      )
!     ########################################################################
!
!!****  *READ_FIELD* - routine to read prognostic and surface fields
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize  prognostic and 
!     surface fields by reading their  value in initial file or by setting 
!     them to a fixed value.
!
!!**  METHOD
!!    ------
!!      According to the get indicators, the prognostics fields are :
!!          - initialized by reading their value in the LFIFM file 
!!    if the corresponding indicators are equal to 'READ'  
!!          - initialized to zero if the corresponding indicators 
!!    are equal to 'INIT'
!!          -  not initialized if their corresponding indicators 
!!    are equal to 'SKIP'
!!
!!      In case of time step change, all fields at t-dt are (linearly)
!!    interpolated to get a consistant initial state before the segment 
!!    integration  
!!
!!    EXTERNAL
!!    --------
!!      FMREAD   : to read data in LFIFM file
!!      INI_LS   : to initialize larger scale fields
!!      INI_LB   : to initialize "2D" surfacic LB fields 
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_CONF   : NVERB,CCONF,CPROGRAM
!!
!!      Module MODD_CTURB :  XTKEMIN
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (routine READ_FIELD)
!!      
!!
!!    AUTHOR
!!    ------
!!  	V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        15/06/94 
!!      modification    22/11/94  add the pressure function (J.Stein)
!!      modification    22/11/94  add the LS fields         (J.Stein)
!!      modification    06/01/95  add Md(t)                 (J.P.Lafore)
!!                      26/03/95  add EPS var               (J. Cuxart)
!!                      30/06/95  add var related to the Subgrid condensation
!!                                                                   (J.Stein)
!!                      18/08/95  time step change case     (J.P.Lafore)
!!                      01/03/96  add the cloud fraction    (J. Stein)
!!     modification     13/12/95  add fmread of the forcing variables 
!!                                                          (M.Georgelin)
!!     modification     13/02/96  external control of the forcing (J.-P. Pinty)
!!                      11/04/96  add the ice concentration (J.-P. Pinty)
!!                      27/01/97  read ISVR 3D fields of SV (J.-P. Pinty)
!!                      26/02/97  "surfacic" LS fieds  introduction (J.P.Lafore)
!!          (V MASSON)  03/03/97  positivity control for time step change 
!!                      10/04/97  proper treatment of minima for LS-fields (J.P.Lafore)
!!           J. Stein   22/06/97  use the absolute pressure
!!           J. Stein   22/10/97  cleaning + add the LB fields for u,v,w,theta,Rv
!!          P. Bechtold 22/01/98  add SST and surface pressure forcing
!!          V. Ducrocq  14/08/98  //,  remove KIINF,KJINF,KISUP,KJSUP,
!!                                     and introduce INI_LS and INI_LB
!!          J. Stein    22/01/99  add the reading of STORAGE_TYPE to improve
!!                                the START case when the file contains 2
!!                                instants MT
!!          D. Gazen    22/01/01  use MODD_NSV to handle NSV floating indices
!!                                for the current model
!!          V. Masson   01/2004   removes surface (externalization)
!!       J.-P. Pinty    06/05/04  treat NSV_* for C1R3 and ELEC
!!                      05/06     Remove EPS
!!          M. Leriche  04/10     add pH in cloud water and rainwater
!!          M. Leriche  07/10     treat NSV_* for ice phase chemical species
!!          C.Lac       11/11     Suppress all the t-Dt fields
!!          M.Tomasini, 
!!          P. Peyrille   06/12   2D west african monsoon : add reading of ADV forcing and addy fluxes 
!!          C.Lac       03/13     add prognostic supersaturation for C2R2/KHKO
!!          Bosseur & Filippi 07/13 Adds Forefire
!!          M. Leriche  11/14     correct bug in pH initialization
!!          C.Lac       12/14     correction for reproducibility START/RESTA
!!      Modification    01/2016  (JP Pinty) Add LIMA
!!          M. Leriche  02/16     treat gas and aq. chemicals separately
!!          C.Lac        10/16 CEN4TH with RKC4 + Correction on RK loop
!!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODE_FM
USE MODE_IO_ll, ONLY : UPCASE
!
USE MODD_CONF
USE MODD_TIME ! for type DATE_TIME
USE MODD_CST
USE MODD_CTURB
USE MODD_NSV
USE MODD_DUST
USE MODD_SALT
USE MODD_PASPOL
#ifdef MNH_FOREFIRE
USE MODD_FOREFIRE
#endif
USE MODD_CH_AEROSOL
!
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_CH_M9_n,         ONLY: CNAMES, CICNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_PARAM_C2R2,      ONLY: LSUPSAT
!
USE MODD_PARAM_LIMA     , ONLY: NMOD_CCN, LSCAV, LAERO_MASS,                &
                                NMOD_IFN, NMOD_IMM, NINDICE_CCN_IMM, LHHONI
USE MODD_PARAM_LIMA_WARM, ONLY: CLIMA_WARM_NAMES, CAERO_MASS
USE MODD_PARAM_LIMA_COLD, ONLY: CLIMA_COLD_NAMES
!
USE MODE_FMREAD
USE MODI_INI_LS
USE MODI_INI_LB
!
USE MODD_LATZ_EDFLX
USE MODD_2D_FRC
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!
CHARACTER (LEN=*),         INTENT(IN)  :: HINIFILE       
                             ! name of the initial file
CHARACTER (LEN=*),         INTENT(IN)  :: HLUOUT        
                             ! name for output-listing of nested models
INTEGER,                   INTENT(IN)  :: KMASDEV
                             ! version of the input file
INTEGER,                   INTENT(IN)  :: KIU, KJU, KKU   
                             ! array sizes in x, y and z  directions
REAL,                      INTENT(IN)  :: PTSTEP       
                             ! current Time STEP   
! 
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKET,                          &
                                          HGETRVT,HGETRCT,HGETRRT,           &
                                          HGETRIT,HGETRST,HGETRGT,HGETRHT,   & 
                                          HGETCIT,HGETSRCT,                  &
                                          HGETSIGS,HGETCLDFR,HGETBL_DEPTH,   &
                                          HGETSBL_DEPTH,HGETPHC,HGETPHR
CHARACTER (LEN=*), DIMENSION(:),INTENT(IN)  :: HGETSVT
!
! GET indicators to know wether a given  variable should or not be read in the
! FM file at time t-deltat and t
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME ! advection scheme for wind
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME ! advection scheme for wind
!
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN):: KSIZELBYTKE_ll                ! for TKE
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV 
!
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUM,PVM,PWM     ! U,V,W at t-dt
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PDUM,PDVM,PDWM  ! Difference on U,V,W 
                                                          ! between t+dt and t-dt
REAL, DIMENSION(:,:),      INTENT(OUT) :: PBL_DEPTH       ! BL depth
REAL, DIMENSION(:,:),      INTENT(OUT) :: PSBL_DEPTH      ! SBL depth
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PWTHVMF         ! MassFlux buoyancy flux
!
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUT,PVT,PWT     ! U,V,W at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PTHT,PTKET      ! theta, tke and
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PRTKEMS         ! tke adv source
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPABST          ! pressure at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPABSM          ! pressure at t-1
REAL, DIMENSION(:,:,:,:),  INTENT(OUT) :: PRT,PSVT        ! moist and scalar
                                                          ! variables at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PSRCT           ! turbulent flux
                                                          !  <s'Rc'> at t 
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PCIT            ! ice conc. at t
REAL,                      INTENT(OUT) :: PDRYMASST       ! Md(t)
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PSIGS           ! =sqrt(<s's'>) for the
                                                          ! Subgrid Condensation
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PCLDFR          ! cloud fraction  
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPHC            ! pH value in cloud water  
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPHR            ! pH value in rainwater  
!
!
! Larger Scale fields
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLSUM,PLSVM,PLSWM    ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLSTHM,  PLSRVM      ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
!
!       
! Forcing fields
INTEGER,              INTENT(IN)  :: KFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTFRC ! date of forcing profs.
REAL, DIMENSION(:,:), INTENT(OUT) :: PUFRC,PVFRC,PWFRC ! forcing variables
REAL, DIMENSION(:,:), INTENT(OUT) :: PTHFRC,PRVFRC
REAL, DIMENSION(:,:), INTENT(OUT) :: PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC
REAL, DIMENSION(:),   INTENT(OUT) :: PPGROUNDFRC
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: PATC
INTEGER,              INTENT(IN)  :: KADVFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTADVFRC ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),   INTENT(OUT) :: PDTHFRC, PDRVFRC
INTEGER,              INTENT(IN)  :: KRELFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TPDTRELFRC ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),   INTENT(OUT) :: PTHREL, PRVREL
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M ! Eddy fluxes
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRUS_PRES, PRVS_PRES, PRWS_PRES
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS_CLD, PRSVS_CLD
!
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(KIU,KJU,KKU) :: Z3D               ! 3D array used to read  data
                                                  ! in initial file 
REAL, DIMENSION(KIU,KJU,KKU) :: ZWORK             ! to compute supersaturation 
INTEGER             :: IGRID,ILENCH,IRESP         !   File 
CHARACTER (LEN=LEN_HREC)  :: YRECFM                     ! management
CHARACTER (LEN=100) :: YCOMMENT                   ! variables 
CHARACTER (LEN=2)   :: YDIR                       !
INTEGER             :: ILUOUT                     ! Unit number for prints
INTEGER             :: JSV                        ! Loop index for additional
                                                  ! scalar variables
INTEGER             :: ISV                        ! total number of  scalar variables
INTEGER             :: IRR                        !  counter for moist variables
INTEGER             :: JKLOOP,JRR                 ! Loop indexes
INTEGER             :: IIUP,IJUP                  ! size  of working 
                                                  ! window arrays
REAL                :: Z1,Z2                      ! coeff. for the temporal 
                                                  ! interpolation
INTEGER             :: JT                  ! loop index
REAL, DIMENSION(KKU)        :: Z1D    ! forcing working arrays
INTEGER, DIMENSION(3)       :: ITDATE ! date array
CHARACTER(LEN=3)            :: YFRC   ! To mark the different forcing dates
LOGICAL :: GLSOURCE ! switch for the source term (for ini_ls and ini_lb)
CHARACTER (LEN=2)      :: YSTORAGE_TYPE
! ---------------------------------------------------------------------
! Modif PP ADV FRC
REAL, DIMENSION(KIU)        :: X1D    ! forcing working arrays
REAL, DIMENSION(KIU,KJU,KKU):: XDTH3D,XDRV3D
!
CHARACTER(LEN=2)  :: INDICE
INTEGER           :: I
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATION
!              ---------------
!
GLSOURCE=.FALSE.
Z3D = 0.0 
ZWORK = 0.0
!
YRECFM='STORAGE_TYPE' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,YSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP /= 0) YSTORAGE_TYPE='TT'
!
!-------------------------------------------------------------------------------
!
!*       2.    READ PROGNOSTIC VARIABLES
!              -------------------------
!
!*       2.1  Time t:
!
IF (KMASDEV<50) THEN
  YRECFM = 'UM'
ELSE
  YRECFM = 'UT'
ENDIF
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PUT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (KMASDEV<50) THEN
  YRECFM = 'VM'
ELSE
  YRECFM = 'VT'
END IF
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PVT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (KMASDEV<50) THEN
  YRECFM = 'WM'
ELSE
  YRECFM = 'WT'
END IF
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PWT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (KMASDEV<50) THEN
  YRECFM = 'THM'
ELSE
  YRECFM = 'THT'
END IF
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PTHT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (KMASDEV<50) THEN
  YRECFM = 'PABSM'
ELSE
  YRECFM = 'PABST'
END IF
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PPABST,IGRID,ILENCH,YCOMMENT,IRESP)
PPABSM = PPABST
!
SELECT CASE(HGETTKET)                   
  CASE('READ')
    IF (KMASDEV<50) THEN
      YRECFM = 'TKEM'
    ELSE
      YRECFM = 'TKET'
    END IF
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PTKET,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (KMASDEV>50 .AND. (CCONF == 'RESTA')) THEN
      YRECFM = 'TKEMS'
      YDIR='XY'
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PRTKEMS,IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  CASE('INIT')
    PTKET(:,:,:)=XTKEMIN
    PRTKEMS(:,:,:)=0.               
END SELECT 
!
IRR=0
!
SELECT CASE(HGETRVT)             ! vapor
  CASE('READ')
    IRR=IRR+1 
    IF (KMASDEV<50) THEN
      YRECFM = 'RVM'
    ELSE
      YRECFM='RVT'
    END IF
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR)=0.
END SELECT 
!
SELECT CASE(HGETRCT)             ! cloud 
  CASE('READ') 
    IRR=IRR+1
    IF (KMASDEV<50) THEN
      YRECFM = 'RCM'
    ELSE
      YRECFM='RCT'
    END IF
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR) = 0.
END SELECT
!
SELECT CASE(HGETRRT)             ! rain 
  CASE('READ') 
    IRR=IRR+1
    IF (KMASDEV<50) THEN
      YRECFM = 'RRM'
    ELSE
      YRECFM ='RRT'
    END IF 
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR) = 0.
END SELECT
!
SELECT CASE(HGETRIT)             ! cloud ice
  CASE('READ') 
    IRR=IRR+1 
    IF (KMASDEV<50) THEN
      YRECFM = 'RIM'
    ELSE
      YRECFM ='RIT'
    END IF 
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR)=0.
END SELECT
!
SELECT CASE(HGETRST)             ! snow
  CASE('READ')
    IRR=IRR+1 
    IF (KMASDEV<50) THEN
      YRECFM = 'RSM'
    ELSE
      YRECFM ='RST'
    END IF 
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR)=0.
END SELECT
!
SELECT CASE(HGETRGT)             ! graupel
  CASE('READ') 
    IRR=IRR+1 
    IF (KMASDEV<50) THEN
      YRECFM = 'RGM'
    ELSE
      YRECFM ='RGT'
    END IF 
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR)=0.
END SELECT
!
SELECT CASE(HGETRHT)             ! hail
  CASE('READ') 
    IRR=IRR+1 
    IF (KMASDEV<50) THEN
      YRECFM = 'RHM'
    ELSE
      YRECFM ='RHT'
    END IF 
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    PRT(:,:,:,IRR)=Z3D(:,:,:)
  CASE('INIT')
    IRR=IRR+1 
    PRT(:,:,:,IRR)=0.
END SELECT
!
SELECT CASE(HGETCIT)             ! ice concentration
  CASE('READ')
    YRECFM='CIT'
    YDIR='XY'
    IF (SIZE(PCIT) /= 0 )   &
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PCIT,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  CASE('INIT')
    PCIT(:,:,:)=0.
END SELECT
!
!  Scalar Variables Reading : Users, C2R2, C1R3, LIMA, ELEC, Chemical SV
!
YDIR='XY'
ISV= SIZE(PSVT,4)
!
DO JSV = 1, NSV_USER              ! initialize according to the get indicators
  SELECT CASE(HGETSVT(JSV))
    CASE ('READ')
      WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
      PSVT(:,:,:,JSV) = Z3D(:,:,:)
    CASE ('INIT')
      PSVT(:,:,:,JSV) = 0. 
  END SELECT
END DO
!
DO JSV = NSV_C2R2BEG,NSV_C2R2END
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
    IF (LSUPSAT .AND. (HGETRVT == 'READ') ) THEN
      ZWORK(:,:,:) = (PPABST(:,:,:)/XP00 )**(XRD/XCPD)
      ZWORK(:,:,:) = PTHT(:,:,:)*ZWORK(:,:,:)
      ZWORK(:,:,:) = EXP(XALPW-XBETAW/ZWORK(:,:,:)-XGAMW*ALOG(ZWORK(:,:,:)))
      !rvsat
      ZWORK(:,:,:) = (XMV / XMD)*ZWORK(:,:,:)/(PPABST(:,:,:)-ZWORK(:,:,:))
      ZWORK(:,:,:) = PRT(:,:,:,1)/ZWORK(:,:,:) 
      PSVT(:,:,:,NSV_C2R2END ) = ZWORK(:,:,:)
    END IF
  END SELECT
END DO
!
DO JSV = NSV_C1R3BEG,NSV_C1R3END
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
! LIMA variables
!
DO JSV = NSV_LIMA_BEG,NSV_LIMA_END
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
! Nc
    IF (JSV .EQ. NSV_LIMA_NC) THEN
      YRECFM=TRIM(CLIMA_WARM_NAMES(1))//'T'
    END IF
! Nr
    IF (JSV .EQ. NSV_LIMA_NR) THEN
      YRECFM=TRIM(CLIMA_WARM_NAMES(2))//'T'
    END IF
! N CCN free
    IF (JSV .GE. NSV_LIMA_CCN_FREE .AND. JSV .LT. NSV_LIMA_CCN_ACTI) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_FREE + 1)
      YRECFM=TRIM(CLIMA_WARM_NAMES(3))//INDICE//'T'
    END IF
! N CCN acti
    IF (JSV .GE. NSV_LIMA_CCN_ACTI .AND. JSV .LT. NSV_LIMA_CCN_ACTI + NMOD_CCN) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_ACTI + 1)
      YRECFM=TRIM(CLIMA_WARM_NAMES(4))//INDICE//'T'
    END IF
! Scavenging
    IF (JSV .EQ. NSV_LIMA_SCAVMASS) THEN
      YRECFM=TRIM(CAERO_MASS(1))//'T'
    END IF
! Ni
    IF (JSV .EQ. NSV_LIMA_NI) THEN
      YRECFM=TRIM(CLIMA_COLD_NAMES(1))//'T'
    END IF
! N IFN free
    IF (JSV .GE. NSV_LIMA_IFN_FREE .AND. JSV .LT. NSV_LIMA_IFN_NUCL) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_FREE + 1)
      YRECFM=TRIM(CLIMA_COLD_NAMES(2))//INDICE//'T'
    END IF
! N IFN nucl
    IF (JSV .GE. NSV_LIMA_IFN_NUCL .AND. JSV .LT. NSV_LIMA_IFN_NUCL + NMOD_IFN) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_NUCL + 1)
      YRECFM=TRIM(CLIMA_COLD_NAMES(3))//INDICE//'T'
    END IF
! N IMM nucl
    I = 0
    IF (JSV .GE. NSV_LIMA_IMM_NUCL .AND. JSV .LT. NSV_LIMA_IMM_NUCL + NMOD_IMM) THEN
    I = I + 1
    WRITE(INDICE,'(I2.2)')(NINDICE_CCN_IMM(I))
    YRECFM=TRIM(CLIMA_COLD_NAMES(4))//INDICE//'T'
  END IF
! Hom. freez. of CCN
  IF (JSV .EQ. NSV_LIMA_HOM_HAZE) THEN
    YRECFM=TRIM(CLIMA_COLD_NAMES(5))//'T'
  END IF
!
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,YCOMMENT,IRESP)
!
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
DO JSV = NSV_ELECBEG,NSV_ELECEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
DO JSV = NSV_CHGSBEG,NSV_CHGSEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    CNAMES(JSV-NSV_CHGSBEG+1) = UPCASE(CNAMES(JSV-NSV_CHGSBEG+1))
    YRECFM=TRIM(CNAMES(JSV-NSV_CHGSBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_CHACBEG,NSV_CHACEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1) = UPCASE(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))
    YRECFM=TRIM(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_CHICBEG,NSV_CHICEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    CICNAMES(JSV-NSV_CHICBEG+1) = UPCASE(CICNAMES(JSV-NSV_CHICBEG+1))
    YRECFM=TRIM(CICNAMES(JSV-NSV_CHICBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_SLTBEG,NSV_SLTEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_DSTBEG,NSV_DSTEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)  
    CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
END SELECT
END DO

DO JSV = NSV_AERBEG,NSV_AEREND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CAERONAMES(JSV-NSV_AERBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT    
END DO
!
DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)  
    CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
END SELECT
END DO
!
DO JSV = NSV_LGBEG,NSV_LGEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM=TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
DO JSV = NSV_PPBEG,NSV_PPEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    IF ( IRESP ==0 ) THEN
       PSVT(:,:,:,JSV) = Z3D(:,:,:)
    ELSE
       PSVT(:,:,:,JSV) = 0.
    END IF
    WRITE(YRECFM,'(A3,I3.3)')'ATC',JSV
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) THEN
      PATC(:,:,:,JSV-NSV_PPBEG+1) = Z3D(:,:,:)
    ELSE
      PATC(:,:,:,JSV-NSV_PPBEG+1) = 0. 
    ENDIF
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
    PATC(:,:,:,JSV-NSV_PPBEG+1) = 0.
  END SELECT
END DO
!
#ifdef MNH_FOREFIRE
DO JSV = NSV_FFBEG,NSV_FFEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    IF (IRESP == 0) THEN
       PSVT(:,:,:,JSV) = Z3D(:,:,:)
    ELSE
       PSVT(:,:,:,JSV) = 0.
    END IF
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
#endif
!
DO JSV = NSV_CSBEG,NSV_CSEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    WRITE(YRECFM,'(A3,I3.3)')'SVT',JSV
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    IF ( IRESP ==0 ) THEN
       PSVT(:,:,:,JSV) = Z3D(:,:,:)
    ELSE
       PSVT(:,:,:,JSV) = 0.
    END IF
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
DO JSV = NSV_LNOXBEG,NSV_LNOXEND
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    YRECFM='LINOXT'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PSVT(:,:,:,JSV) = Z3D(:,:,:)
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
IF (CCONF == 'RESTA') THEN
  YRECFM = 'US_PRES'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PRUS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM = 'VS_PRES'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PRVS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM = 'WS_PRES'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PRWS_PRES,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM = 'THS_CLD'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PRTHS_CLD,IGRID,ILENCH,YCOMMENT,IRESP)
  DO JRR = 1, SIZE(PRT,4)
   IF (JRR == 1 ) THEN
    YRECFM='RVS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 2 ) THEN
    YRECFM='RCS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 3 ) THEN
    YRECFM='RRS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 4 ) THEN
    YRECFM='RIS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 5 ) THEN
    YRECFM='RSS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 6 ) THEN
    YRECFM='RGS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
   IF (JRR == 7 ) THEN
    YRECFM='RHS_CLD'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRRS_CLD(:,:,:,JRR) = Z3D(:,:,:)
   END IF
  END DO
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
   IF (JSV == NSV_C2R2BEG ) THEN
    YRECFM='RSVS_CLD1'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRSVS_CLD(:,:,:,JSV) = Z3D(:,:,:)
   END IF
   IF (JSV == NSV_C2R2BEG ) THEN
    YRECFM='RSVS_CLD2'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
         YCOMMENT,IRESP)
    PRSVS_CLD(:,:,:,JSV) = Z3D(:,:,:)
   END IF
  END DO
END IF
!
!*       2.1  Time t-dt:
!
IF (CPROGRAM=='MESONH' .AND. HUVW_ADV_SCHEME(1:3)=='CEN' .AND. &
        HTEMP_SCHEME == 'LEFR' ) THEN
  IF (CCONF=='RESTA') THEN
    YRECFM = 'UM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PUM,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM = 'VM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PVM,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM = 'WM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PWM,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM = 'DUM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PDUM,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM = 'DVM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PDVM,IGRID,ILENCH,YCOMMENT,IRESP)
    !
    YRECFM = 'DWM'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PDWM,IGRID,ILENCH,YCOMMENT,IRESP)
  ELSE
    PUM = PUT
    PVM = PVT
    PWM = PWT
  END IF
END IF
!
!*       2.2a  3D LS fields  
!
!
CALL INI_LS(HINIFILE,HLUOUT,HGETRVT,GLSOURCE,PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM)
!
!
!*       2.2b  2D "surfacic" LB fields   
!
!
CALL INI_LB(HINIFILE,HLUOUT,GLSOURCE,ISV,                             &   
     KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,               &
     KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
     KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
     HGETTKET,HGETRVT,HGETRCT,HGETRRT,HGETRIT,HGETRST,                &
     HGETRGT,HGETRHT,HGETSVT,                                         &
     PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
     PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM             ) 
!
!
!*       2.3  Some special variables:
!
YRECFM = 'DRYMASST'                  ! dry mass
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PDRYMASST,IGRID,ILENCH,YCOMMENT,IRESP)
!
SELECT CASE(HGETSRCT)                ! turbulent flux SRC at time t
  CASE('READ')
    YRECFM='SRCT'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    IF( IRESP /= 0 ) THEN
      YRECFM='SRC'
      YDIR='XY'
      CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
    END IF
    PSRCT(:,:,:)=Z3D(:,:,:)
  CASE('INIT')
    PSRCT(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETSIGS)                ! subgrid condensation
  CASE('READ')
    YRECFM='SIGS'
    YDIR='XY' 
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PSIGS,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  CASE('INIT')
    PSIGS(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETPHC)             ! pH in cloud water
  CASE('READ')
    YRECFM='PHC'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PPHC,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  CASE('INIT')
    PPHC(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETPHR)             ! pH in rainwater
  CASE('READ')
    YRECFM='PHR'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PPHR,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
  CASE('INIT')
    PPHR(:,:,:)=0.
END SELECT
!
IRESP=0
IF(HGETCLDFR=='READ') THEN           ! cloud fraction
    YRECFM='CLDFR'
    YDIR='XY' 
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PCLDFR,IGRID,ILENCH,  &
                YCOMMENT,IRESP)
ENDIF
IF(HGETCLDFR=='INIT' .OR. IRESP /= 0) THEN
  IF(SIZE(PRT,4) > 3) THEN
    WHERE(PRT(:,:,:,2)+PRT(:,:,:,4) > 1.E-30)
      PCLDFR(:,:,:) = 1.
    ELSEWHERE
      PCLDFR(:,:,:) = 0.
    ENDWHERE
  ELSE
    WHERE(PRT(:,:,:,2) > 1.E-30)
      PCLDFR(:,:,:) = 1.
    ELSEWHERE
      PCLDFR(:,:,:) = 0.
    ENDWHERE
  ENDIF
ENDIF
!
!* boundary layer depth
!
IF (HGETBL_DEPTH=='READ') THEN
  YRECFM = 'BL_DEPTH'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PBL_DEPTH,IGRID,ILENCH,YCOMMENT,IRESP)
ELSE
  PBL_DEPTH(:,:)=XUNDEF
END IF
!
!* surface boundary layer depth
!
IF (HGETSBL_DEPTH=='READ') THEN
  YRECFM = 'SBL_DEPTH'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PSBL_DEPTH,IGRID,ILENCH,YCOMMENT,IRESP)
ELSE
  PSBL_DEPTH(:,:)=0.
END IF
!
!* Contribution from MAss Flux parameterizations to vert. flux of buoyancy
!
SELECT CASE(HGETTKET)                   
  CASE('READ') 
    YRECFM = 'WTHVMF'
    YDIR='XY'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PWTHVMF,IGRID,ILENCH,YCOMMENT,IRESP)
  CASE('INIT')
    PWTHVMF(:,:,:)=0.
END SELECT 
!-------------------------------------------------------------------------------
!
!*       2.4   READ FORCING VARIABLES
!              ----------------------
!
!
IF ( LFORCING ) THEN
  DO JT=1,KFRC
!
    WRITE (YFRC,'(I3.3)') JT
    YRECFM='DTFRC'//YFRC//'%TDATE' ! array of rank 3 for date
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
    TPDTFRC(JT)%TDATE%YEAR  = ITDATE(1)
    TPDTFRC(JT)%TDATE%MONTH = ITDATE(2)
    TPDTFRC(JT)%TDATE%DAY   = ITDATE(3)
    YRECFM='DTFRC'//YFRC//'%TIME'
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TPDTFRC(JT)%TIME,IGRID,ILENCH, &
                YCOMMENT,IRESP)
!
    YRECFM='UFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PUFRC(:,JT)=Z1D(:)
!
    YRECFM='VFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PVFRC(:,JT)=Z1D(:)
!
    YRECFM='WFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PWFRC(:,JT)=Z1D(:)
!
    YRECFM='THFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PTHFRC(:,JT)=Z1D(:)
!
    YRECFM='RVFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PRVFRC(:,JT)=Z1D(:)
!
    YRECFM='TENDTHFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PTENDTHFRC(:,JT)=Z1D(:)
!
    YRECFM='TENDRVFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PTENDRVFRC(:,JT)=Z1D(:)
!
    YRECFM='GXTHFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PGXTHFRC(:,JT)=Z1D(:)
!
    YRECFM='GYTHFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z1D,IGRID,ILENCH,YCOMMENT,IRESP)
    PGYTHFRC(:,JT)=Z1D(:)
!
    YRECFM='PGROUNDFRC'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PPGROUNDFRC(JT),IGRID,ILENCH,YCOMMENT,IRESP)
!
  END DO
END IF
!
!-------------------------------------------------------------------------------
IF (L2D_ADV_FRC) THEN

  DO JT=1,KADVFRC  

    WRITE (YFRC,'(I3.3)') JT
    YRECFM='DTADV'//YFRC//'%TDATE' ! array of rank 3 for date
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
    TPDTADVFRC(JT)%TDATE%YEAR  = ITDATE(1)
    TPDTADVFRC(JT)%TDATE%MONTH = ITDATE(2)
    TPDTADVFRC(JT)%TDATE%DAY   = ITDATE(3)
    !
    YRECFM='DTADV'//YFRC//'%TIME'
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TPDTADVFRC(JT)%TIME,IGRID,ILENCH, &
	    YCOMMENT,IRESP)
    YRECFM='TH_ADV'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XDTH3D,IGRID,ILENCH,  &
                                                         YCOMMENT,IRESP)
    PDTHFRC(:,:,:,JT)=XDTH3D(:,:,:)
    !
    YRECFM='Q_ADV'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XDRV3D,IGRID,ILENCH,  &
                                                         YCOMMENT,IRESP)
    
    PDRVFRC(:,:,:,JT)=XDRV3D(:,:,:)
  ENDDO
ENDIF
!
IF (L2D_REL_FRC) THEN

  DO JT=1,KRELFRC  

    WRITE (YFRC,'(I3.3)') JT
    YRECFM='DTREL'//YFRC//'%TDATE' ! array of rank 3 for date
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
    TPDTRELFRC(JT)%TDATE%YEAR  = ITDATE(1)
    TPDTRELFRC(JT)%TDATE%MONTH = ITDATE(2)
    TPDTRELFRC(JT)%TDATE%DAY   = ITDATE(3)

    ! Relaxation

    YRECFM='TH_REL'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XDTH3D,IGRID,ILENCH,  &
                                                         YCOMMENT,IRESP)
    
    PTHREL(:,:,:,JT)=XDTH3D(:,:,:)
    !
    YRECFM='Q_REL'//YFRC
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XDRV3D,IGRID,ILENCH,  &
                                                         YCOMMENT,IRESP)

    PRVREL(:,:,:,JT)=XDRV3D(:,:,:)

    ENDDO

ENDIF

!
IF (LUV_FLX) THEN
   IF ( CCONF /= 'START' .OR. CPROGRAM=='SPAWN ' ) THEN 
       YRECFM='VU_FLX'
       YDIR='--'
       CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PVU_FLUX_M,IGRID, &
                   ILENCH,YCOMMENT,IRESP)
   ELSE IF (CCONF == 'START') THEN
       PVU_FLUX_M(:,:,:)=0.
   END IF
ENDIF
!
IF (LTH_FLX) THEN
   IF ( CCONF /= 'START' .OR. CPROGRAM=='SPAWN ' ) THEN 
       YRECFM='VT_FLX'
       YDIR='--'
       CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PVTH_FLUX_M,IGRID, &
                   ILENCH,YCOMMENT,IRESP)
       YRECFM='WT_FLX'
       YDIR='--'
       CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PWTH_FLUX_M,IGRID, &
                   ILENCH,YCOMMENT,IRESP)       
   ELSE IF (CCONF == 'START') THEN
       PWTH_FLUX_M(:,:,:)=0.
       PVTH_FLUX_M(:,:,:)=0.
   END IF
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*       3.    PRINT ON OUTPUT-LISTING
!              ----------------------
!
IF (NVERB >= 10 .AND. .NOT. L1D) THEN
  IIUP = SIZE(PUT,1)
  IJUP = SIZE(PVT,2) 
  CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
! 
  WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PUT values:'
  WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
  DO JKLOOP=1,KKU
    WRITE(ILUOUT,FMT=*) PUT(1,1,JKLOOP),PUT(IIUP/2,IJUP/2,JKLOOP), &
    PUT(IIUP,KJU,JKLOOP),JKLOOP    
  END DO
!
  WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PVT values:'
  WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
  DO JKLOOP=1,KKU
    WRITE(ILUOUT,FMT=*) PVT(1,1,JKLOOP),PVT(IIUP/2,IJUP/2,JKLOOP), &
    PVT(IIUP,IJUP,JKLOOP),JKLOOP    
  END DO
!
  WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PWT values:'
  WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
  DO JKLOOP=1,KKU
    WRITE(ILUOUT,FMT=*) PWT(1,1,JKLOOP),PWT(IIUP/2,IJUP/2,JKLOOP), &
    PWT(IIUP,IJUP,JKLOOP),JKLOOP    
  END DO
!
  WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PTHT values:'
  WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
  DO JKLOOP=1,KKU
    WRITE(ILUOUT,FMT=*) PTHT(1,1,JKLOOP),PTHT(IIUP/2,IJUP/2,JKLOOP), &
    PTHT(IIUP,IJUP,JKLOOP),JKLOOP    
  END DO
!
  IF(SIZE(PTKET,1) /=0) THEN
    WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PTKET values:'
    WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
    DO JKLOOP=1,KKU
      WRITE(ILUOUT,FMT=*) PTKET(1,1,JKLOOP),PTKET(IIUP/2,IJUP/2,JKLOOP), &
      PTKET(IIUP,IJUP,JKLOOP),JKLOOP    
    END DO
  END IF
!
  IF (SIZE(PRT,4) /= 0) THEN
    WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PRT values:'
    DO JRR = 1, SIZE(PRT,4)
      WRITE(ILUOUT,FMT=*) 'JRR = ',JRR
      WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
      DO JKLOOP=1,KKU
        WRITE(ILUOUT,FMT=*) PRT(1,1,JKLOOP,JRR),PRT(IIUP/2,IJUP/2,JKLOOP,JRR), &
        PRT(IIUP,IJUP,JKLOOP,JRR),JKLOOP    
      END DO
    END DO
!
  END IF   
!
  IF (SIZE(PSVT,4) /= 0) THEN
    WRITE(ILUOUT,FMT=*) 'READ_FIELD: Some PSVT values:'
    DO JRR = 1, SIZE(PSVT,4)
      WRITE(ILUOUT,FMT=*) 'JRR = ',JRR
      WRITE(ILUOUT,FMT=*) '(1,1,JK)   (IIU/2,IJU/2,JK)   (IIU,IJU,JK)    JK  '
      DO JKLOOP=1,KKU
        WRITE(ILUOUT,FMT=*) PSVT(1,1,JKLOOP,JRR),PSVT(IIUP/2,IJUP/2,JKLOOP,JRR), &
        PSVT(IIUP,IJUP,JKLOOP,JRR),JKLOOP    
      END DO
    END DO
!
  END IF   
END IF 
!-------------------------------------------------------------------------------
! 
!
END SUBROUTINE READ_FIELD
