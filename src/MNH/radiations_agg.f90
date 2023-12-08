!MNH_LIC Copyright 1995-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    ########################
     MODULE MODI_RADIATIONS_AGG
!    ########################
!
INTERFACE

    SUBROUTINE RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG, &
               TPFILE,OCLEAR_SKY,OCLOUD_ONLY,                                  &
               KCLEARCOL_TM1,HEFRADL,HEFRADI,HOPWSW,HOPISW,HOPWLW,HOPILW,      &
               PFUDG, KDLON, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB_OLD,    &
               KSWB_MNH,KLWB_MNH, KSTATM,KRAD_COLNBR,PCOSZEN,PSEA, PCORSOL,    &
               PDIR_ALB, PSCA_ALB,PEMIS, PCLDFR, PCCO2, PTSRAD, PSTATM,        &
               PTHT, PRT, PPABST, POZON, PAER, PDST_WL, PAER_CLIM, PSVT,       &
               PDTHRAD, PSRFLWD, PSRFSWD_DIR,PSRFSWD_DIF, PRHODREF, PZZ,       &
               PRADEFF, PSWU, PSWD, PLWU,PLWD, PDTHRADSW, PDTHRADLW            )
!
USE MODD_IO,          ONLY: TFILEDATA

INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(IN) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(IN) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(IN) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(IN) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain

TYPE(TFILEDATA),  INTENT(IN)         :: TPFILE    ! Output file
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

INTEGER, INTENT(IN)                  :: KSWB_OLD    ! number of SW band ECMWF 
INTEGER, INTENT(IN)                  :: KSWB_MNH    ! number of SW band ECRAD
INTEGER, INTENT(IN)                  :: KLWB_MNH    ! number of LW band ECRAD
INTEGER, INTENT(IN)                  :: KSTATM  ! index of the standard 
                                                ! atmosphere level just above
                                                !      the model top
INTEGER, INTENT(IN)                  :: KRAD_COLNBR ! factor by which the memory
                                                    ! is split
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
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PEMIS   ! Surface IR EMISsivity
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! CLouD FRaction
REAL,                     INTENT(IN) :: PCCO2   ! CO2 content
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD  ! RADiative Surface Temperature
REAL, DIMENSION(:,:),     INTENT(IN) :: PSTATM  ! selected standard atmosphere
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT    ! THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT     ! moist variables at t (humidity, cloud water, rain water, ice water)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST  ! pressure at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT    ! scalar variable ( C2R2 and C1R3  particle)
!
REAL, DIMENSION(:,:,:),   POINTER    :: POZON   ! OZONE field from clim.
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER    ! AERosols optical thickness from clim. 
REAL, DIMENSION(:,:,:,:), POINTER    :: PDST_WL    ! AERosols Extinction by wavelength . 
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER_CLIM    ! AERosols optical thickness from clim.
                                                ! note : the vertical dimension of 
                                                ! these fields include the "radiation levels"
                                                ! above domain top
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

END SUBROUTINE RADIATIONS_AGG
!
END INTERFACE
!
END MODULE MODI_RADIATIONS_AGG
!
!   ############################################################################
    SUBROUTINE RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG, &
               TPFILE,OCLEAR_SKY,OCLOUD_ONLY,                                  &
               KCLEARCOL_TM1,HEFRADL,HEFRADI,HOPWSW,HOPISW,HOPWLW,HOPILW,      &
               PFUDG, KDLON, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB_OLD,    &
               KSWB_MNH,KLWB_MNH, KSTATM,KRAD_COLNBR,PCOSZEN,PSEA, PCORSOL,    &
               PDIR_ALB, PSCA_ALB,PEMIS, PCLDFR, PCCO2, PTSRAD, PSTATM,        &
               PTHT, PRT, PPABST, POZON, PAER, PDST_WL, PAER_CLIM, PSVT,       &
               PDTHRAD, PSRFLWD, PSRFSWD_DIR,PSRFSWD_DIF, PRHODREF, PZZ,       &
               PRADEFF, PSWU, PSWD, PLWU,PLWD, PDTHRADSW, PDTHRADLW            )
!   ############################################################################
!
!!****  *RADIATIONS * - routine to call the SW and LW radiation calculations
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to aggregate columns of the temperature, water vapor
!!    liquid water, cloud fraction, ozone profiles for the ECMWF radiation
!!    calculations. There is a great number of available radiative fluxes in
!!    the output, but only the potential temperature radiative tendency and the
!!    SW and LW surface fluxes are provided in the output of the routine.
!!
!!**  METHOD
!!    ------
!!
!!    All columns are aggregated according to NRAD_AGG * NRAD_AGG points. 
!!    
!!
!!    EXTERNAL
!!    --------
!!      Subroutine RADIATIONS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine RADIATIONS )
!!
!!    AUTHOR
!!    ------
!!	V. Masson        * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/23
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE PARKIND1,         ONLY: JPRB
!
USE MODD_CONF,        ONLY: LCARTESIAN
USE MODD_CST
USE MODD_GRID ,       ONLY: XLAT0, XLON0
USE MODD_GRID_n ,     ONLY: XLAT, XLON, XXHAT, XYHAT, XXHAT_ll, XYHAT_ll
USE MODD_IO,          ONLY: TFILEDATA
USE MODD_LUNIT_n,     ONLY: TLUOUT
USE MODD_LBC_n,       ONLY: CLBCX, CLBCY
USE MODD_PARAMETERS
!
USE MODI_RADIATIONS
USE MODE_ll
use mode_msg
USE MODE_SUM_ll,          ONLY: MIN_ll
USE MODE_SUM2_ll,         ONLY: GMINLOC_ll
USE MODD_VAR_ll,      ONLY: IP
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
!  
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(IN) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(IN) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(IN) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(IN) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain

TYPE(TFILEDATA),  INTENT(IN)         :: TPFILE    ! Output file
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

INTEGER, INTENT(IN)                  :: KSWB_OLD    ! number of SW band ECMWF 
INTEGER, INTENT(IN)                  :: KSWB_MNH    ! number of SW band ECRAD
INTEGER, INTENT(IN)                  :: KLWB_MNH    ! number of LW band ECRAD
INTEGER, INTENT(IN)                  :: KSTATM  ! index of the standard 
                                                ! atmosphere level just above
                                                !      the model top
INTEGER, INTENT(IN)                  :: KRAD_COLNBR ! factor by which the memory
                                                    ! is split
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
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PEMIS   ! Surface IR EMISsivity
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! CLouD FRaction
REAL,                     INTENT(IN) :: PCCO2   ! CO2 content
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD  ! RADiative Surface Temperature
REAL, DIMENSION(:,:),     INTENT(IN) :: PSTATM  ! selected standard atmosphere
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT    ! THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT     ! moist variables at t (humidity, cloud water, rain water, ice water)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST  ! pressure at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT    ! scalar variable ( C2R2 and C1R3  particle)
!
REAL, DIMENSION(:,:,:),   POINTER    :: POZON   ! OZONE field from clim.
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER    ! AERosols optical thickness from clim. 
REAL, DIMENSION(:,:,:,:), POINTER    :: PDST_WL    ! AERosols Extinction by wavelength . 
REAL, DIMENSION(:,:,:,:), POINTER    :: PAER_CLIM    ! AERosols optical thickness from clim.
                                                ! note : the vertical dimension of 
                                                ! these fields include the "radiation levels"
                                                ! above domain top
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
INTEGER :: IIB           ! I index value of the first inner mass point
INTEGER :: IJB           ! J index value of the first inner mass point
INTEGER :: IKB           ! K index value of the first inner mass point
INTEGER :: IIE           ! I index value of the last inner mass point
INTEGER :: IJE           ! J index value of the last inner mass point
INTEGER :: IKE           ! K index value of the last inner mass point
INTEGER :: IIU           ! array size for the first  index
INTEGER :: IJU           ! array size for the second index
INTEGER :: IKU           ! array size for the third  index
INTEGER :: IIMAX         ! last X possible point to consider in aggregation
INTEGER :: IJMAX         ! last Y possible point to consider in aggregation
!
INTEGER :: JIP            ! X packed array index
INTEGER :: JJP            ! Y packed array index
INTEGER :: JI             ! X full array index in current processor
INTEGER :: JJ             ! Y full array index in current processor
INTEGER :: IXORP          ! Index of left X point of packed domain being treated
INTEGER :: IYORP          ! Index of bottom Y point of packed domain being treated
!
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG) :: ZLAT    ! Latitude
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG) :: ZLON    ! Longitude
!
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG) :: ZCOSZEN ! COS(zenithal solar angle)
REAL                                   :: ZCORSOL ! SOLar constant CORrection
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG) :: ZSEA    ! Land-sea mask
!
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PDIR_ALB,3)) :: ZDIR_ALB! Surface direct ALBedo
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSCA_ALB,3)) :: ZSCA_ALB! Surface diffuse ALBedo
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PEMIS,3))    :: ZEMIS   ! Surface IR EMISsivity
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PCLDFR,3))   :: ZCLDFR  ! CLouD FRaction
REAL                                                    :: ZCCO2   ! CO2 content
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG)                  :: ZTSRAD  ! RADiative Surface Temperature
!
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PTHT,3))     :: ZTHT    ! THeta at t
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PRT,3),SIZE(PRT,4)) :: ZRT     ! moist variables at t (humidity, cloud water, rain water, ice water)
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PPABST,3)) :: ZPABST  ! pressure at t
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSVT,3),SIZE(PSVT,4)) :: ZSVT    ! scalar variable ( C2R2 and C1R3  particle)
!
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(POZON,3))    :: ZOZON   ! OZONE field from clim.
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PAER,3),SIZE(PAER,4))    :: ZAER    ! AERosols optical thickness from clim. 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PDST_WL,3),SIZE(PDST_WL,4))    :: ZDST_WL    ! AERosols Extinction by wavelength . 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PAER_CLIM,3),SIZE(PAER_CLIM,4))    :: ZAER_CLIM    ! AERosols optical thickness from clim.
                                                ! note : the vertical dimension of 
                                                ! these fields include the "radiation levels"
                                                ! above domain top
                                                ! 
                                                 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PRHODREF,3))   :: ZRHODREF ![kg/m3] air density
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PZZ,3))   :: ZZZ      ![m] height of layers

INTEGER, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG) :: ICLEARCOL_TM1 ! trace of cloud/clear col
                                                         ! at the previous radiation step
!                                                 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PDTHRAD,3)) :: ZDTHRAD ! THeta RADiative Tendancy
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG)                 :: ZSRFLWD ! Downward SuRFace LW Flux
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSRFSWD_DIR,3)) :: ZSRFSWD_DIR ! Downward SuRFace SW Flux DIRect 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSRFSWD_DIF,3)) :: ZSRFSWD_DIF ! Downward SuRFace SW Flux DIFfuse 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSWU,3)) :: ZSWU ! upward SW Flux 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PSWD,3)) :: ZSWD ! downward SW Flux 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PLWU,3)) :: ZLWU ! upward LW Flux 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PLWD,3)) :: ZLWD ! downward LW Flux 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PDTHRADSW,3)) :: ZDTHRADSW ! dthrad sw 
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PDTHRADLW,3)) :: ZDTHRADLW !  dthradsw
REAL, DIMENSION(KI_RAD_AGG,KJ_RAD_AGG,SIZE(PRADEFF,3)) :: ZRADEFF ! effective radius
!

REAL, DIMENSION(SIZE(PCOSZEN,1),SIZE(PCOSZEN,2)) :: ZZLAT    ! Latitude
REAL, DIMENSION(SIZE(PCOSZEN,1),SIZE(PCOSZEN,2)) :: ZZLON    ! Longitude
!
!
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3)) :: ZDZPABST
INTEGER             :: ILUOUT       ! Logical unit number for output-listing
REAL :: ZMINVAL
INTEGER, DIMENSION(3) :: IMINLOC
INTEGER :: IINFO_ll
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES
!              ----------------------------------------------
!
! full arrays
CALL GET_DIM_EXT_ll ('B',IIU,IJU)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!

! Limit of points to integrate into the aggregation on X and Y limits
IIMAX = IIU ; IF (LEAST_ll()  .AND. CLBCX(2)/='CYCL') IIMAX = IIE
IJMAX = IJU ; IF (LNORTH_ll() .AND. CLBCY(2)/='CYCL') IJMAX = IJE
!
IKU = SIZE(PTHT,3)
IKB = 1 + JPVEXT
IKE = IKU - JPVEXT
!
!
!*       1.1   CHECK PRESSURE DECREASING
!              -------------------------
ZDZPABST(:,:,1:IKU-1) = PPABST(:,:,1:IKU-1) - PPABST(:,:,2:IKU)
ZDZPABST(:,:,IKU) = ZDZPABST(:,:,IKU-1)
!
ZMINVAL=MIN_ll(ZDZPABST,IINFO_ll)
!
IF ( ZMINVAL <= 0.0 ) THEN
   ILUOUT = TLUOUT%NLU
   IMINLOC=GMINLOC_ll( ZDZPABST )
   WRITE(ILUOUT,*) ' radiation.f90 STOP :: SOMETHING WRONG WITH PRESSURE , ZDZPABST <= 0.0 '  
   WRITE(ILUOUT,*) ' radiation :: ZDZPABST ', ZMINVAL,' located at ',   IMINLOC
   FLUSH(unit=ILUOUT)
   call Print_msg( NVERB_FATAL, 'GEN', 'RADIATIONS', 'something wrong with pressure: ZDZPABST <= 0.0' )
END IF
!
!-------------------------------------------------------------------------------
!
! special case of no packing: avoids transfer of data from one array to another (but duplicates the code to call the radaitions routine)
! -------------------------
!
!--------------------
IF (KRAD_AGG==1) THEN
!--------------------

  IF(LCARTESIAN) THEN
    ZZLAT(:,:) = XLAT0  *(XPI/180.)
    ZZLON(:,:) = XLON0 *(XPI/180.)
  ELSE
    ZZLAT = XLAT *(XPI/180.)
    ZZLON = XLON *(XPI/180.)
  END IF
!
  CALL RADIATIONS( TPFILE, IIB, IIE,IJB,IJE,                                                 &
                   OCLEAR_SKY, OCLOUD_ONLY, KCLEARCOL_TM1, HEFRADL, HEFRADI, HOPWSW, HOPISW, &
                   HOPWLW, HOPILW, PFUDG,                                                    &
                   KDLON, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB_OLD, KSWB_MNH, KLWB_MNH, &
                   KSTATM, KRAD_COLNBR, PCOSZEN, PSEA, PCORSOL, ZZLAT, ZZLON,                &
                   PDIR_ALB, PSCA_ALB, PEMIS, PCLDFR, PCCO2, PTSRAD, PSTATM, PTHT, PRT,      &
                   PPABST, POZON, PAER,PDST_WL, PAER_CLIM, PSVT,                             &
                   PDTHRAD, PSRFLWD, PSRFSWD_DIR, PSRFSWD_DIF, PRHODREF, PZZ ,               &
                   PRADEFF, PSWU, PSWD, PLWU, PLWD, PDTHRADSW, PDTHRADLW                     )
  RETURN
!--------------------
END IF
!--------------------
!
!-------------------------------------------------------------------------------
!
!        2. Packs arrays
!           ------------
!
! latitude and longitude definition & packing
!
! Only the middle point of each packed subdomain is kept, there is nopt averaging 
! (to avoid issues at poles and longitude sign change).
!
IF(LCARTESIAN) THEN
  ZLAT(:,:) = XLAT0
  ZLON(:,:) = XLON0
ELSE
  CALL PACK_RAD_AGG_MID(XLAT,ZLAT,.FALSE.)
  CALL PACK_RAD_AGG_MID(XLON,ZLON,.FALSE.)
END IF
!
ZLAT = ZLAT *(XPI/180.)
ZLON = ZLON *(XPI/180.)
!
!
! packing of input fileds
!
CALL PACK_RAD_AGG_2D(PCOSZEN,ZCOSZEN,.TRUE.)
CALL PACK_RAD_AGG_2D(PSEA,ZSEA,.TRUE.)
CALL PACK_RAD_AGG_3D(PDIR_ALB,ZDIR_ALB,.TRUE.)
CALL PACK_RAD_AGG_3D(PSCA_ALB,ZSCA_ALB,.TRUE.)
CALL PACK_RAD_AGG_3D(PEMIS,ZEMIS,.TRUE.)
CALL PACK_RAD_AGG_3D(PCLDFR,ZCLDFR,.FALSE.)
CALL PACK_RAD_AGG_2D(PTSRAD,ZTSRAD,.TRUE.)
CALL PACK_RAD_AGG_3D(PTHT,ZTHT,.FALSE.)
CALL PACK_RAD_AGG_4D(PRT,ZRT,.FALSE.)
CALL PACK_RAD_AGG_3D(PPABST,ZPABST,.FALSE.)
CALL PACK_RAD_AGG_4D(PSVT,ZSVT,.FALSE.)
CALL PACK_RAD_AGG_3D(POZON,ZOZON,.TRUE.)
CALL PACK_RAD_AGG_4D(PAER,ZAER,.TRUE.)
CALL PACK_RAD_AGG_4D(PDST_WL,ZDST_WL,.TRUE.)
CALL PACK_RAD_AGG_4D(PAER_CLIM,ZAER_CLIM,.TRUE.)
CALL PACK_RAD_AGG_3D(PRHODREF,ZRHODREF,.FALSE.)
CALL PACK_RAD_AGG_3D(PZZ,ZZZ,.FALSE.)
CALL PACK_RAD_AGG_I2(KCLEARCOL_TM1,ICLEARCOL_TM1,.TRUE.)
CALL PACK_RAD_AGG_3D(PDTHRAD,ZDTHRAD,.TRUE.)
CALL PACK_RAD_AGG_2D(PSRFLWD,ZSRFLWD,.TRUE.)
CALL PACK_RAD_AGG_3D(PSRFSWD_DIR,ZSRFSWD_DIR,.TRUE.)
CALL PACK_RAD_AGG_3D(PSRFSWD_DIF,ZSRFSWD_DIF,.TRUE.)
CALL PACK_RAD_AGG_3D(PSWU,ZSWU,.TRUE.)
CALL PACK_RAD_AGG_3D(PSWD,ZSWD,.TRUE.)
CALL PACK_RAD_AGG_3D(PLWU,ZLWU,.TRUE.)
CALL PACK_RAD_AGG_3D(PLWD,ZLWD,.TRUE.)
CALL PACK_RAD_AGG_3D(PDTHRADSW,ZDTHRADSW,.TRUE.)
CALL PACK_RAD_AGG_3D(PDTHRADLW,ZDTHRADLW,.TRUE.)
CALL PACK_RAD_AGG_3D(PRADEFF,ZRADEFF,.TRUE.)
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!        3. Call radiation on packed columns
!           --------------------------------
!
      CALL RADIATIONS( TPFILE, 1, KI_RAD_AGG,1,KJ_RAD_AGG,                                       &
                       OCLEAR_SKY, OCLOUD_ONLY, ICLEARCOL_TM1, HEFRADL, HEFRADI, HOPWSW, HOPISW, &
                       HOPWLW, HOPILW, PFUDG,                                                    &
                       KI_RAD_AGG*KJ_RAD_AGG, KFLEV, KRAD_DIAG, KFLUX, KRAD, KAER, KSWB_OLD, KSWB_MNH, KLWB_MNH, &
                       KSTATM, KRAD_COLNBR, ZCOSZEN, ZSEA, PCORSOL, ZLAT, ZLON,                  &
                       ZDIR_ALB, ZSCA_ALB, ZEMIS, ZCLDFR, PCCO2, ZTSRAD, PSTATM, ZTHT, ZRT,      &
                       ZPABST, ZOZON, ZAER,ZDST_WL, ZAER_CLIM, ZSVT,                             &
                       ZDTHRAD, ZSRFLWD, ZSRFSWD_DIR, ZSRFSWD_DIF, ZRHODREF, ZZZ ,               &
                       ZRADEFF, ZSWU, ZSWD, ZLWU, ZLWD, ZDTHRADSW, ZDTHRADLW                     )
!
!-------------------------------------------------------------------------------
!
!        4. Unpacks arrays
!           --------------
!
CALL UNPACK_RAD_AGG_3D(PDTHRAD,ZDTHRAD)
CALL UNPACK_RAD_AGG_2D(PSRFLWD,ZSRFLWD)
CALL UNPACK_RAD_AGG_3D(PSRFSWD_DIR,ZSRFSWD_DIR)
CALL UNPACK_RAD_AGG_3D(PSRFSWD_DIF,ZSRFSWD_DIF)
CALL UNPACK_RAD_AGG_3D(PSWU,ZSWU)
CALL UNPACK_RAD_AGG_3D(PSWD,ZSWD)
CALL UNPACK_RAD_AGG_3D(PLWU,ZLWU)
CALL UNPACK_RAD_AGG_3D(PLWD,ZLWD)
CALL UNPACK_RAD_AGG_3D(PDTHRADSW,ZDTHRADSW)
CALL UNPACK_RAD_AGG_3D(PDTHRADLW,ZDTHRADLW)
CALL UNPACK_RAD_AGG_3D(PRADEFF,ZRADEFF)
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE PACK_RAD_AGG_2D(PFULL,PPACK,OEXCH)
REAL, DIMENSION(:,:), INTENT(IN)  :: PFULL
REAL, DIMENSION(:,:), INTENT(OUT) :: PPACK
LOGICAL                           :: OEXCH
INTEGER :: ICOUNT
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
REAL, DIMENSION(SIZE(PFULL,1),SIZE(PFULL,2)) :: ZFULL

  NULLIFY(TZFIELDS_ll)
  ZFULL = PFULL
  IF (OEXCH) THEN
    CALL ADD2DFIELD_ll( TZFIELDS_ll, ZFULL, 'RADIATION_AGG: PACK_RAD_AGG_2D' )
    CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
    CALL CLEANLIST_ll(TZFIELDS_ll)
  END IF

  PPACK = 0.

  DO JJP=1,KJ_RAD_AGG
    DO JIP=1,KI_RAD_AGG
      ICOUNT = 0
      IXORP = KIOR_RAD_AGG + (JIP-1) * KRAD_AGG
      IYORP = KJOR_RAD_AGG + (JJP-1) * KRAD_AGG
      DO JJ=IYORP,MIN(IYORP+KRAD_AGG-1,IJMAX)
        DO JI=IXORP,MIN(IXORP+KRAD_AGG-1,IIMAX)
          PPACK(JIP,JJP) = PPACK(JIP,JJP) + ZFULL(JI,JJ)
          ICOUNT = ICOUNT + 1
        END DO
      END DO
      PPACK(JIP,JJP) = PPACK(JIP,JJP) / ICOUNT
    END DO
  END DO
END SUBROUTINE PACK_RAD_AGG_2D
!-------------------------------------------------------------------------------
!
SUBROUTINE PACK_RAD_AGG_3D(PFULL,PPACK,OEXCH)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PFULL
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PPACK
LOGICAL                           :: OEXCH
INTEGER :: JK
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
REAL, DIMENSION(SIZE(PFULL,1),SIZE(PFULL,2),SIZE(PFULL,3)) :: ZFULL

NULLIFY(TZFIELDS_ll)
ZFULL = PFULL
IF (OEXCH) THEN
  DO JK=1,SIZE(PFULL,3)
    CALL ADD2DFIELD_ll( TZFIELDS_ll, ZFULL(:,:,JK), 'RADIATION_AGG: PACK_RAD_AGG_3D' )
  END DO
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
END IF

DO JK=1,SIZE(PFULL,3)
  CALL PACK_RAD_AGG_2D(ZFULL(:,:,JK),PPACK(:,:,JK),.FALSE.)
END DO

END SUBROUTINE PACK_RAD_AGG_3D

!-------------------------------------------------------------------------------
!
SUBROUTINE PACK_RAD_AGG_4D(PFULL,PPACK,OEXCH)
REAL, DIMENSION(:,:,:,:), INTENT(IN)  :: PFULL
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: PPACK
LOGICAL                           :: OEXCH
INTEGER :: JK, JL
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
REAL, DIMENSION(SIZE(PFULL,1),SIZE(PFULL,2),SIZE(PFULL,3),SIZE(PFULL,4)) :: ZFULL

NULLIFY(TZFIELDS_ll)
ZFULL = PFULL
IF (OEXCH) THEN
  DO JL=1,SIZE(PFULL,4)
    DO JK=1,SIZE(PFULL,3)
      CALL ADD2DFIELD_ll( TZFIELDS_ll, ZFULL(:,:,JK,JL), 'RADIATION_AGG: PACK_RAD_AGG_4D' )
    END DO
  END DO
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
END IF

DO JL=1,SIZE(PFULL,4)
 DO JK=1,SIZE(PFULL,3)
  CALL PACK_RAD_AGG_2D(ZFULL(:,:,JK,JL),PPACK(:,:,JK,JL),.FALSE.)
 END DO
END DO

END SUBROUTINE PACK_RAD_AGG_4D


!-------------------------------------------------------------------------------
!
SUBROUTINE PACK_RAD_AGG_I2(KFULL,KPACK,OEXCH)
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KFULL
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KPACK
LOGICAL                           :: OEXCH
REAL, DIMENSION(SIZE(KFULL,1),SIZE(KFULL,2)) :: ZFULL
REAL, DIMENSION(SIZE(KPACK,1),SIZE(KPACK,2)) :: ZPACK

ZFULL = FLOAT(KFULL)
CALL PACK_RAD_AGG_2D(ZFULL(:,:),ZPACK(:,:),OEXCH)
!
KPACK=0
WHERE ( ZPACK>=1.-0.5/FLOAT(KRAD_AGG*KRAD_AGG) ) KPACK = 1
! This ensures that the output is coherent with the clear_sky columns definitions in radiations:
! If the averaged value is 1 (allowing calculation error), this means that all columns are clear, so we keep this information as clear
! If any column was not clear, then the averaged value is below one 
! (in fact, necessarily below 1-1/FLOAT(KRAD_AGG**2), because entry values are 0 or 1 ),
! and then the packed column is partly cloudy, so we keep the value zero.
!
END SUBROUTINE PACK_RAD_AGG_I2

!-------------------------------------------------------------------------------
!
SUBROUTINE PACK_RAD_AGG_MID(PFULL,PPACK,OEXCH)
REAL, DIMENSION(:,:), INTENT(IN)  :: PFULL
REAL, DIMENSION(:,:), INTENT(OUT) :: PPACK
LOGICAL                           :: OEXCH

!  DO JJP=1,MIN(KJ_RAD_AGG,IJMAX)
!    DO JIP=1,MIN(KI_RAD_AGG,IIMAX)
  DO JJP=1,KJ_RAD_AGG
    DO JIP=1,KI_RAD_AGG
      IXORP = KIOR_RAD_AGG + (JIP-1) * KRAD_AGG
      IYORP = KJOR_RAD_AGG + (JJP-1) * KRAD_AGG
      PPACK(JIP,JJP) = PFULL(MIN(IXORP + KRAD_AGG/2,IImax),MIN(IYORP + KRAD_AGG/2,Ijmax) ) 
    END DO
  END DO
END SUBROUTINE PACK_RAD_AGG_MID
!
!-------------------------------------------------------------------------------
!
SUBROUTINE UNPACK_RAD_AGG_2D(PFULL,PPACK)
REAL, DIMENSION(:,:), INTENT(OUT)  :: PFULL
REAL, DIMENSION(:,:), INTENT(IN)   :: PPACK

  DO JJP=1,KJ_RAD_AGG
    DO JIP=1,KI_RAD_AGG
      IXORP = KIOR_RAD_AGG + (JIP-1) * KRAD_AGG
      IYORP = KJOR_RAD_AGG + (JJP-1) * KRAD_AGG
      DO JJ=IYORP,MIN(IYORP+KRAD_AGG-1,IJMAX)
        DO JI=IXORP,MIN(IXORP+KRAD_AGG-1,IIMAX)
          PFULL(JI,JJ) = PPACK(JIP,JJP)
        END DO
      END DO
    END DO
  END DO

END SUBROUTINE UNPACK_RAD_AGG_2D
!-------------------------------------------------------------------------------
!
SUBROUTINE UNPACK_RAD_AGG_3D(PFULL,PPACK)
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PFULL
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PPACK
INTEGER :: JK

DO JK=1,SIZE(PFULL,3)
  CALL UNPACK_RAD_AGG_2D(PFULL(:,:,JK),PPACK(:,:,JK))
END DO

END SUBROUTINE UNPACK_RAD_AGG_3D


!-------------------------------------------------------------------------------
!
END SUBROUTINE RADIATIONS_AGG

