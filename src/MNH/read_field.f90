!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!     ######################
      MODULE MODI_READ_FIELD
!     ######################
!
INTERFACE 
!
      SUBROUTINE READ_FIELD(TPINIFILE,KIU,KJU,KKU,                           &
            HGETTKET,HGETRVT,HGETRCT,HGETRRT,HGETRIT,HGETCIT,HGETZWS,        &
            HGETRST,HGETRGT,HGETRHT,HGETSVT,HGETSRCT,HGETSIGS,HGETCLDFR,     &
            HGETBL_DEPTH,HGETSBL_DEPTH,HGETPHC,HGETPHR,HUVW_ADV_SCHEME,      &
            HTEMP_SCHEME,KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,  &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
            PUM,PVM,PWM,PDUM,PDVM,PDWM,                                      &
            PUT,PVT,PWT,PTHT,PPABST,PTKET,PRTKEMS,                           &
            PRT,PSVT,PZWS,PCIT,PDRYMASST,                                    &            
            PSIGS,PSRCT,PCLDFR,PBL_DEPTH,PSBL_DEPTH,PWTHVMF,PPHC,PPHR,       &
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM, PLSZWSM,                        &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            KFRC,TPDTFRC,PUFRC,PVFRC,PWFRC,PTHFRC,PRVFRC,                    &
            PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC,PPGROUNDFRC,PATC,        &
            PTENDUFRC,PTENDVFRC,                                             &
            KADVFRC,TPDTADVFRC,PDTHFRC,PDRVFRC,                              &
            KRELFRC,TPDTRELFRC, PTHREL, PRVREL,                              &
            PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M,                              &
            PRUS_PRES,PRVS_PRES,PRWS_PRES,PRTHS_CLD,PRRS_CLD,PRSVS_CLD       )
!
USE MODD_IO_ll, ONLY : TFILEDATA
USE MODD_TIME ! for type DATE_TIME
!
!
TYPE(TFILEDATA),           INTENT(IN)  :: TPINIFILE    !Initial file
INTEGER,                   INTENT(IN)  :: KIU, KJU, KKU   
                             ! array sizes in x, y and z  directions
! 
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKET,                          &
                                          HGETRVT,HGETRCT,HGETRRT,           &
                                          HGETRIT,HGETRST,HGETRGT,HGETRHT,   & 
                                          HGETCIT,HGETSRCT, HGETZWS,         &
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
INTEGER, INTENT(IN) :: KSIZELBYTKE_ll                ! for TKE
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
REAL, DIMENSION(:,:,:,:),  INTENT(OUT) :: PRT,PSVT        ! moist and scalar
                                                          ! variables at t
REAL, DIMENSION(:,:),      INTENT(INOUT) :: PZWS
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
REAL, DIMENSION(:,:),            INTENT(OUT) :: PLSZWSM              ! significant height of sea waves
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! Forcing fields
INTEGER,                        INTENT(IN)    :: KFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTFRC           ! date of forcing profs.
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PUFRC,PVFRC,PWFRC ! forcing variables
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PTHFRC,PRVFRC
REAL, DIMENSION(:,:),           INTENT(OUT) :: PTENDUFRC,PTENDVFRC
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC
REAL, DIMENSION(:),             INTENT(OUT)   :: PPGROUNDFRC
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PATC
INTEGER,                        INTENT(IN)    :: KADVFRC           ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTADVFRC        ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PDTHFRC, PDRVFRC
INTEGER,                        INTENT(IN)    :: KRELFRC           ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTRELFRC        ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PTHREL, PRVREL
REAL, DIMENSION(:,:,:),         INTENT(OUT)   :: PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M ! Eddy fluxes
REAL, DIMENSION(:,:,:),         INTENT(INOUT) :: PRUS_PRES, PRVS_PRES, PRWS_PRES
REAL, DIMENSION(:,:,:),         INTENT(INOUT) :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:),       INTENT(INOUT) :: PRRS_CLD, PRSVS_CLD
!
!
END SUBROUTINE READ_FIELD
!
END INTERFACE
!
END MODULE MODI_READ_FIELD
!
!     ########################################################################
      SUBROUTINE READ_FIELD(TPINIFILE,KIU,KJU,KKU,                           &
            HGETTKET,HGETRVT,HGETRCT,HGETRRT,HGETRIT,HGETCIT,HGETZWS,        &
            HGETRST,HGETRGT,HGETRHT,HGETSVT,HGETSRCT,HGETSIGS,HGETCLDFR,     &
            HGETBL_DEPTH,HGETSBL_DEPTH,HGETPHC,HGETPHR,HUVW_ADV_SCHEME,      &
            HTEMP_SCHEME,KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,  &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
            PUM,PVM,PWM,PDUM,PDVM,PDWM,                                      &
            PUT,PVT,PWT,PTHT,PPABST,PTKET,PRTKEMS,                           &
            PRT,PSVT,PZWS,PCIT,PDRYMASST,                                    &
            PSIGS,PSRCT,PCLDFR,PBL_DEPTH,PSBL_DEPTH,PWTHVMF,PPHC,PPHR,       &
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,PLSZWSM,                         &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            KFRC,TPDTFRC,PUFRC,PVFRC,PWFRC,PTHFRC,PRVFRC,                    &
            PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC,PPGROUNDFRC,PATC,        &
            PTENDUFRC,PTENDVFRC,                                             &
            KADVFRC,TPDTADVFRC,PDTHFRC,PDRVFRC,                              &
            KRELFRC,TPDTRELFRC, PTHREL, PRVREL,                              &
            PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M,                              &
            PRUS_PRES,PRVS_PRES,PRWS_PRES,PRTHS_CLD,PRRS_CLD,PRSVS_CLD       )
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
!!                   09/2017 Q.Rodier add LTEND_UV_FRC
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!          V. Vionnet  07/17    add blowing snow scheme
!!          P. Wautelet 01/2019  corrected intent of PDUM,PDVM,PDWM (OUT->INOUT)
!  P. Wautelet 13/02/2019: removed PPABSM and PTSTEP dummy arguments (bugfix: PPABSM was intent(OUT))
!!      Bielli S. 02/2019  Sea salt : significant sea wave height influences salt emission; 5 salt modes
!  P. Wautelet 14/03/2019: correct ZWS when variable not present in file
!! M. Leriche  10/06/2019: in restart case read all immersion modes for LIMA
!!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_2D_FRC
USE MODD_CH_AEROSOL
USE MODD_CH_M9_n,         ONLY: CNAMES, CICNAMES
USE MODD_CONF
USE MODD_CONF_n
USE MODD_CST
USE MODD_CTURB
USE MODD_DUST
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_FIELD_n,         only: XZWS_DEFAULT
#ifdef MNH_FOREFIRE
USE MODD_FOREFIRE
#endif
USE MODD_BLOWSNOW
USE MODD_BLOWSNOW_n

USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_IO_ll,           ONLY: TFILEDATA
USE MODD_LATZ_EDFLX
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_LUNIT_N,         ONLY: TLUOUT
USE MODD_NSV
USE MODD_PARAM_C2R2,      ONLY: LSUPSAT
!
USE MODD_PARAM_LIMA     , ONLY: NMOD_CCN, LSCAV, LAERO_MASS,                &
                                NMOD_IFN, NMOD_IMM, NINDICE_CCN_IMM, LHHONI
USE MODD_PARAM_LIMA_COLD, ONLY: CLIMA_COLD_NAMES
USE MODD_PARAM_LIMA_WARM, ONLY: CLIMA_WARM_NAMES, CAERO_MASS
USE MODD_PARAM_n,           ONLY: CSCONV
USE MODD_ADV_n
USE MODD_PASPOL
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_SALT
USE MODD_TIME ! for type DATE_TIME
!
USE MODE_FIELD,           ONLY: TFIELDDATA,TFIELDLIST,FIND_FIELD_ID_FROM_MNHNAME,TYPEDATE,TYPEREAL
USE MODE_FMREAD
USE MODE_IO_ll,           ONLY: UPCASE
USE MODE_MSG
!
USE MODI_INI_LB
USE MODI_INI_LS
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!
TYPE(TFILEDATA),           INTENT(IN)  :: TPINIFILE    !Initial file
INTEGER,                   INTENT(IN)  :: KIU, KJU, KKU   
                             ! array sizes in x, y and z  directions
! 
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKET,                          &
                                          HGETRVT,HGETRCT,HGETRRT,           &
                                          HGETRIT,HGETRST,HGETRGT,HGETRHT,   & 
                                          HGETCIT,HGETSRCT,HGETZWS,          &
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
INTEGER, INTENT(IN) :: KSIZELBYTKE_ll                ! for TKE
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV 
!
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUM,PVM,PWM     ! U,V,W at t-dt
REAL, DIMENSION(:,:,:),    INTENT(INOUT) :: PDUM,PDVM,PDWM  ! Difference on U,V,W
                                                          ! between t+dt and t-dt
REAL, DIMENSION(:,:),      INTENT(OUT) :: PBL_DEPTH       ! BL depth
REAL, DIMENSION(:,:),      INTENT(OUT) :: PSBL_DEPTH      ! SBL depth
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PWTHVMF         ! MassFlux buoyancy flux
!
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PUT,PVT,PWT     ! U,V,W at t
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PTHT,PTKET      ! theta, tke and
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PRTKEMS         ! tke adv source
REAL, DIMENSION(:,:,:),    INTENT(OUT) :: PPABST          ! pressure at t
REAL, DIMENSION(:,:,:,:),  INTENT(OUT) :: PRT,PSVT        ! moist and scalar
                                                          ! variables at t
REAL, DIMENSION(:,:),      INTENT(INOUT) :: PZWS
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
REAL, DIMENSION(:,:),            INTENT(OUT) :: PLSZWSM              ! significant height of sea waves
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
INTEGER,                        INTENT(IN)    :: KFRC              ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTFRC           ! date of forcing profs.
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PUFRC,PVFRC,PWFRC ! forcing variables
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PTHFRC,PRVFRC
REAL, DIMENSION(:,:),           INTENT(OUT) :: PTENDUFRC,PTENDVFRC
REAL, DIMENSION(:,:),           INTENT(OUT)   :: PTENDTHFRC,PTENDRVFRC,PGXTHFRC,PGYTHFRC
REAL, DIMENSION(:),             INTENT(OUT)   :: PPGROUNDFRC
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PATC
INTEGER,                        INTENT(IN)    :: KADVFRC           ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTADVFRC        ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PDTHFRC, PDRVFRC
INTEGER,                        INTENT(IN)    :: KRELFRC           ! number of forcing
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)   :: TPDTRELFRC        ! date of forcing profs.
REAL, DIMENSION(:,:,:,:),       INTENT(OUT)   :: PTHREL, PRVREL
REAL, DIMENSION(:,:,:),         INTENT(OUT)   :: PVTH_FLUX_M,PWTH_FLUX_M,PVU_FLUX_M ! Eddy fluxes
REAL, DIMENSION(:,:,:),         INTENT(INOUT) :: PRUS_PRES, PRVS_PRES, PRWS_PRES
REAL, DIMENSION(:,:,:),         INTENT(INOUT) :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:),       INTENT(INOUT) :: PRRS_CLD, PRSVS_CLD
!
!*       0.2   declarations of local variables
!
INTEGER                      :: I, IID
INTEGER                      :: ILUOUT       ! Unit number for prints
INTEGER                      :: IRESP
INTEGER                      :: ISV          ! total number of  scalar variables
INTEGER                      :: JSV          ! Loop index for additional scalar variables
INTEGER                      :: JKLOOP,JRR   ! Loop indexes
INTEGER                      :: IIUP,IJUP    ! size  of working window arrays
INTEGER                      :: JT           ! loop index
LOGICAL                      :: GLSOURCE     ! switch for the source term (for ini_ls and ini_lb)
CHARACTER(LEN=2)             :: INDICE
CHARACTER(LEN=3)             :: YFRC         ! To mark the different forcing dates
CHARACTER(LEN=15)            :: YVAL
REAL, DIMENSION(KIU,KJU,KKU) :: ZWORK        ! to compute supersaturation
TYPE(TFIELDDATA)             :: TZFIELD
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATION
!              ---------------
!
GLSOURCE=.FALSE.
ZWORK = 0.0
!
!-------------------------------------------------------------------------------
!
!*       2.    READ PROGNOSTIC VARIABLES
!              -------------------------
!
!*       2.1  Time t:
!
IF (TPINIFILE%NMNHVERSION(1)<5) THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT',IID,IRESP)
  TZFIELD = TFIELDLIST(IID)
  TZFIELD%CMNHNAME = 'UM'
  CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PUT)
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT',IID,IRESP)
  TZFIELD = TFIELDLIST(IID)
  TZFIELD%CMNHNAME = 'VM'
  CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PVT)
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('WT',IID,IRESP)
  TZFIELD = TFIELDLIST(IID)
  TZFIELD%CMNHNAME = 'WM'
  CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PWT)
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('THT',IID,IRESP)
  TZFIELD = TFIELDLIST(IID)
  TZFIELD%CMNHNAME = 'THM'
  CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTHT)
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('PABST',IID,IRESP)
  TZFIELD = TFIELDLIST(IID)
  TZFIELD%CMNHNAME = 'PABSM'
  CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PPABST)
ELSE
  CALL IO_READ_FIELD(TPINIFILE,'UT',PUT)
  CALL IO_READ_FIELD(TPINIFILE,'VT',PVT)
  CALL IO_READ_FIELD(TPINIFILE,'WT',PWT)
  CALL IO_READ_FIELD(TPINIFILE,'THT',PTHT)
  CALL IO_READ_FIELD(TPINIFILE,'PABST',PPABST)
ENDIF
!
SELECT CASE(HGETTKET)                   
  CASE('READ')
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('TKET',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'TKEM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTKET)
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'TKET',PTKET)
    END IF
    IF ( ( (TPINIFILE%NMNHVERSION(1)==5 .AND. TPINIFILE%NMNHVERSION(2)>0) .OR. TPINIFILE%NMNHVERSION(1)>5 ) &
        .AND. (CCONF == 'RESTA') .AND. LSPLIT_CFL) THEN
      CALL IO_READ_FIELD(TPINIFILE,'TKEMS',PRTKEMS)
    END IF
  CASE('INIT')
    PTKET(:,:,:)   = XTKEMIN
    PRTKEMS(:,:,:) = 0.
END SELECT 
!
SELECT CASE(HGETZWS)
  CASE('READ')
    CALL IO_READ_FIELD(TPINIFILE,'ZWS',PZWS,IRESP)
    !If the field ZWS is not in the file, set its value to XZWS_DEFAULT
    !ZWS is present in files since MesoNH 5.4.2
    IF ( IRESP/=0 ) THEN
      WRITE (YVAL,'( E15.8 )') XZWS_DEFAULT
      CALL PRINT_MSG(NVERB_WARNING,'IO','READ_FIELD','ZWS not found in file: using default value: '//TRIM(YVAL)//' m')
      PZWS(:,:) = XZWS_DEFAULT
    END IF

  CASE('INIT')
    PZWS(:,:)=0.
END SELECT 
!
SELECT CASE(HGETRVT)             ! vapor
  CASE('READ')
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RVT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RVM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RVT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RVT',PRT(:,:,:,IDX_RVT))
    END IF
  CASE('INIT')
    PRT(:,:,:,IDX_RVT) = 0.
END SELECT 
!
SELECT CASE(HGETRCT)             ! cloud 
  CASE('READ') 
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RCT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RCM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RCT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RCT',PRT(:,:,:,IDX_RCT))
    END IF
  CASE('INIT')
    PRT(:,:,:,IDX_RCT) = 0.
END SELECT
!
SELECT CASE(HGETRRT)             ! rain 
  CASE('READ') 
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RRT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RRM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RRT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RRT',PRT(:,:,:,IDX_RRT))
    END IF 
  CASE('INIT')
    PRT(:,:,:,IDX_RRT) = 0.
END SELECT
!
SELECT CASE(HGETRIT)             ! cloud ice
  CASE('READ') 
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RIT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RIM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RIT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RIT',PRT(:,:,:,IDX_RIT))
    END IF 
  CASE('INIT')
    PRT(:,:,:,IDX_RIT) = 0.
END SELECT
!
SELECT CASE(HGETRST)             ! snow
  CASE('READ')
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RST',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RSM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RST))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RST',PRT(:,:,:,IDX_RST))
    END IF 
  CASE('INIT')
    PRT(:,:,:,IDX_RST) = 0.
END SELECT
!
SELECT CASE(HGETRGT)             ! graupel
  CASE('READ') 
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RGT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RGM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RGT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RGT',PRT(:,:,:,IDX_RGT))
    END IF 
  CASE('INIT')
    PRT(:,:,:,IDX_RGT) = 0.
END SELECT
!
SELECT CASE(HGETRHT)             ! hail
  CASE('READ') 
    IF (TPINIFILE%NMNHVERSION(1)<5) THEN
      CALL FIND_FIELD_ID_FROM_MNHNAME('RHT',IID,IRESP)
      TZFIELD = TFIELDLIST(IID)
      TZFIELD%CMNHNAME = 'RHM'
      CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRT(:,:,:,IDX_RHT))
    ELSE
      CALL IO_READ_FIELD(TPINIFILE,'RHT',PRT(:,:,:,IDX_RHT))
    END IF 
  CASE('INIT')
    PRT(:,:,:,IDX_RHT) = 0.
END SELECT
!
SELECT CASE(HGETCIT)             ! ice concentration
  CASE('READ')
    IF (SIZE(PCIT) /= 0 ) CALL IO_READ_FIELD(TPINIFILE,'CIT',PCIT)
  CASE('INIT')
    PCIT(:,:,:)=0.
END SELECT
!
!  Scalar Variables Reading : Users, C2R2, C1R3, LIMA, ELEC, Chemical SV
!
ISV= SIZE(PSVT,4)
!
IF (NSV_USER>0) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = 1, NSV_USER              ! initialize according to the get indicators
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        WRITE(TZFIELD%CMNHNAME,'(A3,I3.3)')'SVT',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CCOMMENT   = 'X_Y_Z_'//TRIM(TZFIELD%CMNHNAME)
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_C2R2END>=NSV_C2R2BEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'm-3'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
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
END IF
!
IF (NSV_C1R3END>=NSV_C1R3BEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'm-3'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_C1R3BEG,NSV_C1R3END
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
! LIMA variables
!
DO JSV = NSV_LIMA_BEG,NSV_LIMA_END
  SELECT CASE(HGETSVT(JSV))
  CASE ('READ')
    TZFIELD%CSTDNAME   = ''
    WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CUNITS     = 'kg-1'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
! Nc
    IF (JSV .EQ. NSV_LIMA_NC) THEN
      TZFIELD%CMNHNAME   = TRIM(CLIMA_WARM_NAMES(1))//'T'
    END IF
! Nr
    IF (JSV .EQ. NSV_LIMA_NR) THEN
      TZFIELD%CMNHNAME   = TRIM(CLIMA_WARM_NAMES(2))//'T'
    END IF
! N CCN free
    IF (JSV .GE. NSV_LIMA_CCN_FREE .AND. JSV .LT. NSV_LIMA_CCN_ACTI) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_FREE + 1)
      TZFIELD%CMNHNAME   = TRIM(CLIMA_WARM_NAMES(3))//INDICE//'T'
    END IF
! N CCN acti
    IF (JSV .GE. NSV_LIMA_CCN_ACTI .AND. JSV .LT. NSV_LIMA_CCN_ACTI + NMOD_CCN) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_ACTI + 1)
      TZFIELD%CMNHNAME   = TRIM(CLIMA_WARM_NAMES(4))//INDICE//'T'
    END IF
! Scavenging
    IF (JSV .EQ. NSV_LIMA_SCAVMASS) THEN
      TZFIELD%CMNHNAME   = TRIM(CAERO_MASS(1))//'T'
      TZFIELD%CUNITS     = 'kg kg-1'
    END IF
! Ni
    IF (JSV .EQ. NSV_LIMA_NI) THEN
      TZFIELD%CMNHNAME   = TRIM(CLIMA_COLD_NAMES(1))//'T'
    END IF
! N IFN free
    IF (JSV .GE. NSV_LIMA_IFN_FREE .AND. JSV .LT. NSV_LIMA_IFN_NUCL) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_FREE + 1)
      TZFIELD%CMNHNAME   = TRIM(CLIMA_COLD_NAMES(2))//INDICE//'T'
    END IF
! N IFN nucl
    IF (JSV .GE. NSV_LIMA_IFN_NUCL .AND. JSV .LT. NSV_LIMA_IFN_NUCL + NMOD_IFN) THEN
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_NUCL + 1)
      TZFIELD%CMNHNAME   = TRIM(CLIMA_COLD_NAMES(3))//INDICE//'T'
    END IF
! N IMM nucl
    IF (JSV .GE. NSV_LIMA_IMM_NUCL .AND. JSV .LT. NSV_LIMA_IMM_NUCL + NMOD_IMM) THEN
      DO I= 1, NMOD_IMM ! to be supressed
        WRITE(INDICE,'(I2.2)')(NINDICE_CCN_IMM(I)) ! to be supressed
!        WRITE(INDICE,'(I2.2)')(NINDICE_CCN_IMM(JSV - NSV_LIMA_BEG - NSV_LIMA_IMM_NUCL + 1))
        TZFIELD%CMNHNAME   = TRIM(CLIMA_COLD_NAMES(4))//INDICE//'T'
      ENDDO
    END IF
! Hom. freez. of CCN
    IF (JSV .EQ. NSV_LIMA_HOM_HAZE) THEN
      TZFIELD%CMNHNAME   = TRIM(CLIMA_COLD_NAMES(5))//'T'
    END IF
!
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
  CASE ('INIT')
    PSVT(:,:,:,JSV) = 0.
  END SELECT
END DO
!
IF (NSV_ELECEND>=NSV_ELECBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_ELECBEG,NSV_ELECEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        IF (JSV .GT. NSV_ELECBEG .AND. JSV .LT. NSV_ELECEND) THEN
          TZFIELD%CUNITS     = 'C m-3'
          WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        ELSE
          TZFIELD%CUNITS     = 'm-3'
          WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3,A8)')'X_Y_Z_','SVT',JSV,' (nb ions/m3)'
        END IF
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_CHGSEND>=NSV_CHGSBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppbv'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_CHGSBEG,NSV_CHGSEND
    CNAMES(JSV-NSV_CHGSBEG+1) = UPCASE(CNAMES(JSV-NSV_CHGSBEG+1))
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CNAMES(JSV-NSV_CHGSBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A4,I3.3)')'X_Y_Z_','CHIM',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_CHACEND>=NSV_CHACBEG) THEN
  TZFIELD%CSTDNAME   = ''
  !PW TODO: check units
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_CHACBEG,NSV_CHACEND
    CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1) = UPCASE(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CNAMES(JSV-NSV_CHACBEG+NSV_CHGS+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A4,I3.3,A4)')'X_Y_Z_','CHAQ',JSV,' (M)'
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
!***ATTENTION: BUG ? field written with a M suffix, read with a T suffix
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_CHICEND>=NSV_CHICBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_CHICBEG,NSV_CHICEND
    CICNAMES(JSV-NSV_CHICBEG+1) = UPCASE(CICNAMES(JSV-NSV_CHICBEG+1))
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CICNAMES(JSV-NSV_CHICBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_SLTEND>=NSV_SLTBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_SLTBEG,NSV_SLTEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_SLTDEPEND>=NSV_SLTDEPBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_DSTEND>=NSV_DSTBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_DSTBEG,NSV_DSTEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_DSTDEPEND>=NSV_DSTDEPBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_AEREND>=NSV_AERBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_AERBEG,NSV_AEREND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_AERDEPEND>=NSV_AERDEPBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_LGEND>=NSV_LGBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'm'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_LGBEG,NSV_LGEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CLGNAMES(JSV-NSV_LGBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_PPEND>=NSV_PPBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_PPBEG,NSV_PPEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        WRITE(TZFIELD%CMNHNAME,'(A3,I3.3)')'SVT',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CCOMMENT   = 'X_Y_Z_'//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CUNITS     = 'kg kg-1'
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV),IRESP)
        IF (IRESP/=0) THEN
          PSVT(:,:,:,JSV) = 0.
        END IF
        !
        WRITE(TZFIELD%CMNHNAME,'(A3,I3.3)')'ATC',JSV+NSV_PPBEG-1
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)')'X_Y_Z_','ATC',JSV+NSV_PPBEG-1
        TZFIELD%CUNITS     = 'm-3'
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PATC(:,:,:,JSV-NSV_PPBEG+1),IRESP)
        IF (IRESP/=0) THEN
          PATC(:,:,:,JSV-NSV_PPBEG+1) = 0.
        ENDIF
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
        PATC(:,:,:,JSV-NSV_PPBEG+1) = 0.
    END SELECT
  END DO
END IF
!
#ifdef MNH_FOREFIRE
IF (NSV_FFEND>=NSV_FFBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_FFBEG,NSV_FFEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        WRITE(TZFIELD%CMNHNAME,'(A3,I3.3)')'SVT',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CCOMMENT   = 'X_Y_Z_'//TRIM(TZFIELD%CMNHNAME)
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV),IRESP)
        IF (IRESP /= 0) THEN
          PSVT(:,:,:,JSV) = 0.
        END IF
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
#endif
!
IF (NSV_CSEND>=NSV_CSBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_CSBEG,NSV_CSEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        WRITE(TZFIELD%CMNHNAME,'(A3,I3.3)')'SVT',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CCOMMENT   = 'X_Y_Z_'//TRIM(TZFIELD%CMNHNAME)
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV),IRESP)
        IF (IRESP /= 0) THEN
          PSVT(:,:,:,JSV) = 0.
        END IF
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_LNOXEND>=NSV_LNOXBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'ppp'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_LNOXBEG,NSV_LNOXEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = 'LINOXT'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)') 'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
!
IF (NSV_SNWEND>=NSV_SNWBEG) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  DO JSV = NSV_SNWBEG,NSV_SNWEND
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        TZFIELD%CMNHNAME   = TRIM(CSNOWNAMES(JSV-NSV_SNWBEG+1))//'T'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A3,I3.3)') 'X_Y_Z_','SVT',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PSVT(:,:,:,JSV))
      CASE ('INIT')
        PSVT(:,:,:,JSV) = 0.
    END SELECT
  END DO
END IF
IF (NSV_SNW>=1) THEN
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 2
  TZFIELD%LTIMEDEP   = .TRUE.
  DO JSV = 1,NSV_SNW
    SELECT CASE(HGETSVT(JSV))
      CASE ('READ')
        WRITE(TZFIELD%CMNHNAME,'(A10,I3.3)')'SNOWCANO_M',JSV      
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        WRITE(TZFIELD%CCOMMENT,'(A6,A8,I3.3)') 'X_Y_Z_','SNOWCANO',JSV
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,XSNWCANO(:,:,JSV))
      CASE ('INIT')
        XSNWCANO(:,:,JSV) = 0.
    END SELECT
  END DO

END IF

!
IF (CCONF == 'RESTA') THEN
  IF (CTEMP_SCHEME/='LEFR') THEN
    CALL IO_READ_FIELD(TPINIFILE,'US_PRES',PRUS_PRES)
    CALL IO_READ_FIELD(TPINIFILE,'VS_PRES',PRVS_PRES)
    CALL IO_READ_FIELD(TPINIFILE,'WS_PRES',PRWS_PRES)
  END IF
  IF (LSPLIT_CFL) THEN
    CALL IO_READ_FIELD(TPINIFILE,'THS_CLD',PRTHS_CLD)
    DO JRR = 1, SIZE(PRT,4)
      SELECT CASE(JRR)
        CASE (1)
          CALL IO_READ_FIELD(TPINIFILE,'RVS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (2)
          CALL IO_READ_FIELD(TPINIFILE,'RCS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (3)
          CALL IO_READ_FIELD(TPINIFILE,'RRS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (4)
          CALL IO_READ_FIELD(TPINIFILE,'RIS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (5)
          CALL IO_READ_FIELD(TPINIFILE,'RSS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (6)
          CALL IO_READ_FIELD(TPINIFILE,'RGS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE (7)
          CALL IO_READ_FIELD(TPINIFILE,'RHS_CLD',PRRS_CLD(:,:,:,JRR))
        CASE DEFAULT
          CALL PRINT_MSG(NVERB_FATAL,'GEN','READ_FIELD','PRT is too big')
      END SELECT
    END DO
    DO JSV = NSV_C2R2BEG,NSV_C2R2END
      IF (JSV == NSV_C2R2BEG ) THEN
        TZFIELD%CMNHNAME   = 'RSVS_CLD1'
        TZFIELD%CSTDNAME   = ''
        TZFIELD%CLONGNAME  = 'RSVS_CLD1'
        TZFIELD%CUNITS     = '1'
        TZFIELD%CDIR       = 'XY'
        TZFIELD%CCOMMENT   = 'X_Y_Z_RHS_CLD'
        TZFIELD%NGRID      = 1
        TZFIELD%NTYPE      = TYPEREAL
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .TRUE.
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRSVS_CLD(:,:,:,JSV))
      END IF
      IF (JSV == NSV_C2R2BEG ) THEN
        TZFIELD%CMNHNAME   = 'RSVS_CLD2'
        TZFIELD%CSTDNAME   = ''
        TZFIELD%CLONGNAME  = 'RSVS_CLD2'
        TZFIELD%CUNITS     = '1'
        TZFIELD%CDIR       = 'XY'
        TZFIELD%CCOMMENT   = 'X_Y_Z_RHS_CLD'
        TZFIELD%NGRID      = 1
        TZFIELD%NTYPE      = TYPEREAL
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .TRUE.
        CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRSVS_CLD(:,:,:,JSV))
      END IF
    END DO
  END IF
END IF
!
!*       2.1  Time t-dt:
!
IF (CPROGRAM=='MESONH' .AND. HUVW_ADV_SCHEME(1:3)=='CEN' .AND. &
        HTEMP_SCHEME == 'LEFR' ) THEN
  IF (CCONF=='RESTA') THEN
    CALL IO_READ_FIELD(TPINIFILE,'UM', PUM)
    CALL IO_READ_FIELD(TPINIFILE,'VM', PVM)
    CALL IO_READ_FIELD(TPINIFILE,'WM', PWM)
    CALL IO_READ_FIELD(TPINIFILE,'DUM',PDUM)
    CALL IO_READ_FIELD(TPINIFILE,'DVM',PDVM)
    CALL IO_READ_FIELD(TPINIFILE,'DWM',PDWM)
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
CALL INI_LS(TPINIFILE,HGETRVT,GLSOURCE,PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,PLSZWSM)
!
!
!*       2.2b  2D "surfacic" LB fields   
!
!
CALL INI_LB(TPINIFILE,GLSOURCE,ISV,                                   &
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
CALL IO_READ_FIELD(TPINIFILE,'DRYMASST',PDRYMASST) ! dry mass
!
SELECT CASE(HGETSRCT)                ! turbulent flux SRC at time t
  CASE('READ')
    CALL IO_READ_FIELD(TPINIFILE,'SRCT',PSRCT)
  CASE('INIT')
    PSRCT(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETSIGS)                ! subgrid condensation
  CASE('READ')
    CALL IO_READ_FIELD(TPINIFILE,'SIGS',PSIGS)
  CASE('INIT')
    PSIGS(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETPHC)             ! pH in cloud water
  CASE('READ')
    CALL IO_READ_FIELD(TPINIFILE,'PHC',PPHC)
  CASE('INIT')
    PPHC(:,:,:)=0.
END SELECT
!
SELECT CASE(HGETPHR)             ! pH in rainwater
  CASE('READ')
    CALL IO_READ_FIELD(TPINIFILE,'PHR',PPHR)
  CASE('INIT')
    PPHR(:,:,:)=0.
END SELECT
!
IRESP=0
IF(HGETCLDFR=='READ') THEN           ! cloud fraction
  CALL IO_READ_FIELD(TPINIFILE,'CLDFR',PCLDFR,IRESP)
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
  CALL IO_READ_FIELD(TPINIFILE,'BL_DEPTH',PBL_DEPTH)
ELSE
  PBL_DEPTH(:,:)=XUNDEF
END IF
!
!* surface boundary layer depth
!
IF (HGETSBL_DEPTH=='READ') THEN
  CALL IO_READ_FIELD(TPINIFILE,'SBL_DEPTH',PSBL_DEPTH)
ELSE
  PSBL_DEPTH(:,:)=0.
END IF
!
!* Contribution from MAss Flux parameterizations to vert. flux of buoyancy
!
SELECT CASE(HGETTKET)                   
  CASE('READ') 
    IF (CSCONV=='EDKF') THEN 
      CALL IO_READ_FIELD(TPINIFILE,'WTHVMF',PWTHVMF)
    ELSE
      PWTHVMF(:,:,:)=0
    ENDIF
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
!
    TZFIELD%CMNHNAME   = 'DTFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Date of forcing profile '//YFRC
    TZFIELD%NGRID      = 0
    TZFIELD%NTYPE      = TYPEDATE
    TZFIELD%NDIMS      = 0
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,TPDTFRC(JT))
!
    TZFIELD%CMNHNAME   = 'UFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'm s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Zonal component of horizontal forcing wind'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PUFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'VFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'm s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Meridian component of horizontal forcing wind'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PVFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'WFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'm s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Vertical forcing wind'
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PWFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'THFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Forcing potential temperature'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTHFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'RVFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Forcing vapor mixing ratio'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRVFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'TENDTHFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale potential temperature tendency for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTENDTHFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'TENDRVFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'kg kg-1 s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale vapor mixing ratio tendency for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTENDRVFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'GXTHFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K m-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale potential temperature gradient for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PGXTHFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'GYTHFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K m-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale potential temperature gradient for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PGYTHFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'PGROUNDFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'Pa'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Forcing ground pressure'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 0
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PPGROUNDFRC(JT))
!
    TZFIELD%CMNHNAME   = 'TENDUFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'm s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale U tendency for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTENDUFRC(:,JT))
!
    TZFIELD%CMNHNAME   = 'TENDVFRC'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'm s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Large-scale V tendency for forcing'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTENDVFRC(:,JT))
  END DO
END IF
!
!-------------------------------------------------------------------------------
IF (L2D_ADV_FRC) THEN

  DO JT=1,KADVFRC  
    WRITE (YFRC,'(I3.3)') JT
    !
    TZFIELD%CMNHNAME   = 'DTADV'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Date and time of the advecting forcing '//YFRC
    TZFIELD%NGRID      = 0
    TZFIELD%NTYPE      = TYPEDATE
    TZFIELD%NDIMS      = 0
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,TPDTADVFRC(JT))
    !
    TZFIELD%CMNHNAME   = 'TH_ADV'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PDTHFRC(:,:,:,JT))
    !
    TZFIELD%CMNHNAME   = 'Q_ADV'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'kg kg-1 s-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PDRVFRC(:,:,:,JT))
  ENDDO
ENDIF
!
IF (L2D_REL_FRC) THEN

  DO JT=1,KRELFRC  
    WRITE (YFRC,'(I3.3)') JT
    !
    TZFIELD%CMNHNAME   = 'DTREL'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = 'Date and time of the relaxation forcing '//YFRC
    TZFIELD%NGRID      = 0
    TZFIELD%NTYPE      = TYPEDATE
    TZFIELD%NDIMS      = 0
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,TPDTRELFRC(JT))
    !
    ! Relaxation
    TZFIELD%CMNHNAME   = 'TH_REL'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'K'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PTHREL(:,:,:,JT))
    !
    TZFIELD%CMNHNAME   = 'Q_REL'//YFRC
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_READ_FIELD(TPINIFILE,TZFIELD,PRVREL(:,:,:,JT))
  ENDDO
ENDIF
!
IF (LUV_FLX) THEN
  IF ( CCONF /= 'START' .OR. CPROGRAM=='SPAWN ' ) THEN
    CALL IO_READ_FIELD(TPINIFILE,'VU_FLX',PVU_FLUX_M)
  ELSE IF (CCONF == 'START') THEN
    PVU_FLUX_M(:,:,:)=0.
  END IF
ENDIF
!
IF (LTH_FLX) THEN
  IF ( CCONF /= 'START' .OR. CPROGRAM=='SPAWN ' ) THEN
    CALL IO_READ_FIELD(TPINIFILE,'VT_FLX',PVTH_FLUX_M)
    CALL IO_READ_FIELD(TPINIFILE,'WT_FLX',PWTH_FLUX_M)
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
  ILUOUT= TLUOUT%NLU
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

