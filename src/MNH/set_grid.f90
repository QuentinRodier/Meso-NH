!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     ####################
      MODULE MODI_SET_GRID
!     ####################
!
INTERFACE
!
      SUBROUTINE SET_GRID(KMI,HINIFILE,HLUOUT,                                &
                          KIU,KJU,KKU,KIMAX_ll,KJMAX_ll,                      &
                          PBMX1,PBMX2,PBMX3,PBMX4,PBMY1,PBMY2,PBMY3,PBMY4,    &
                          PBFX1,PBFX2,PBFX3,PBFX4,PBFY1,PBFY2,PBFY3,PBFY4,    &
                          KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,            &
                          HLBCX,HLBCY,                                        &
                          PTSTEP,PSEGLEN,                                     &
                          PLONORI,PLATORI,PLON,PLAT,                          &
                          PXHAT,PYHAT,PDXHAT,PDYHAT, PMAP,                    &
                          PZS,PZZ,PZHAT,OSLEVE,PLEN1,PLEN2,PZSMT,             &
                          PJ,                                                 &
                          TPDTMOD,TPDTCUR,KSTOP,KOUT_NUMB,TPOUTBAKN           )
!
USE MODD_TYPE_DATE
USE MODE_IO_ll, ONLY:TOUTBAK
!
INTEGER,                INTENT(IN)  :: KMI       ! Model index
CHARACTER (LEN=*),      INTENT(IN)  :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN)  :: HLUOUT    ! name for output-listing
                                                 !  of nested models
INTEGER,                INTENT(IN)  :: KIU       ! Upper dimension in x direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KJU       ! Upper dimension in y direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KKU       ! Upper dimension in z direction
                                                 ! for domain arrays
INTEGER,               INTENT(IN)   :: KIMAX_ll  !  Dimensions  in x direction
                                                 ! of the physical domain,
INTEGER,               INTENT(IN)   :: KJMAX_ll  !  Dimensions  in y direction
                                                 ! of the physical domain,
REAL, DIMENSION(:), INTENT(IN) :: PBMX1,PBMX2,PBMX3,PBMX4 ! Mass points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBMY1,PBMY2,PBMY3,PBMY4 ! Mass points in Y-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFX1,PBFX2,PBFX3,PBFX4 ! Flux points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFY1,PBFY2,PBFY3,PBFY4 ! Flux points in Y-direc.
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the inner model domain, relative to outer model
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction resolution RATIO
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between inner model and outer model
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCX   ! type of lateral
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCY   ! boundary conditions
!
REAL,                   INTENT(IN)  :: PTSTEP    ! time step of model KMI
REAL,                   INTENT(INOUT) :: PSEGLEN ! segment duration (in seconds)
!
REAL,                   INTENT(OUT) :: PLONORI   ! Longitude  of the
                                                 ! Origine point of
                                                 ! conformal projection
REAL,                   INTENT(OUT) :: PLATORI   ! Latitude of the
                                                 ! Origine point of
                                                 ! conformal projection
REAL, DIMENSION(:,:),   INTENT(OUT) :: PLON,PLAT ! Longitude and latitude
REAL, DIMENSION(:),     INTENT(OUT) :: PXHAT     ! Position x in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:),     INTENT(OUT) :: PYHAT     ! Position y in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:),     INTENT(OUT) :: PDXHAT    ! horizontal stretching in x
REAL, DIMENSION(:),     INTENT(OUT) :: PDYHAT    ! horizontal stretching in y
REAL, DIMENSION(:,:),   INTENT(OUT) :: PMAP      ! Map factor
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZS       ! orography
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PZZ       ! Height z
REAL, DIMENSION(:),     INTENT(OUT) :: PZHAT     ! Height  level
LOGICAL,                INTENT(OUT) :: OSLEVE    ! flag for SLEVE coordinate
REAL,                   INTENT(OUT) :: PLEN1     ! Decay scale for smooth topography
REAL,                   INTENT(OUT) :: PLEN2     ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZSMT     ! smooth-orography
!
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTMOD   ! date and time of the model
                                                 ! beginning
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTCUR   ! Current date and time
INTEGER,                INTENT(OUT) :: KSTOP     ! number of time steps for
                                                 ! current segment
INTEGER,POINTER,        INTENT(OUT) :: KOUT_NUMB ! number of outputs
TYPE(TOUTBAK),DIMENSION(:),POINTER,INTENT(OUT) :: TPOUTBAKN ! List of outputs and backups
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PJ        ! Jacobian
!
END SUBROUTINE SET_GRID
!
END INTERFACE
!
END MODULE MODI_SET_GRID
!
!
!
!
!
!     #########################################################################
      SUBROUTINE SET_GRID(KMI,HINIFILE,HLUOUT,                                &
                          KIU,KJU,KKU,KIMAX_ll,KJMAX_ll,                      &
                          PBMX1,PBMX2,PBMX3,PBMX4,PBMY1,PBMY2,PBMY3,PBMY4,    &
                          PBFX1,PBFX2,PBFX3,PBFX4,PBFY1,PBFY2,PBFY3,PBFY4,    &
                          KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,            &
                          HLBCX,HLBCY,                                        &
                          PTSTEP,PSEGLEN,                                     &
                          PLONORI,PLATORI,PLON,PLAT,                          &
                          PXHAT,PYHAT,PDXHAT,PDYHAT, PMAP,                    &
                          PZS,PZZ,PZHAT,OSLEVE,PLEN1,PLEN2,PZSMT,             &
                          PJ,                                                 &
                          TPDTMOD,TPDTCUR,KSTOP,KOUT_NUMB,TPOUTBAKN           )
!     #########################################################################
!
!!****  *SET_GRID* - routine to set grid variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to set spatio-temporal grid variables
!
!!**  METHOD
!!    ------
!!
!!      The spatial grid variables are read in initial file :
!!        * The reference latitude (XLAT0), the reference longitude (XLON0) and
!!      the projection parameter (XPRPK) are read if spherical geometry is used.
!!      (LCARTESIAN=.FALSE.) and only at the first call (by INI_MODEL1,i.e. KMI=1),
!!     since it is the same for all nested models.
!!        * The rotation angle (XBETA) is read only at the first call for the
!!     same reason.
!!        * The latitude and longitude of the origine points (PLATORI and PLONORI)
!!     are read for a spherical geometry (LCARTESIAN=.FALSE.).
!!        * The horizontal positions (PXHAT and PYHAT) are always read.
!!
!!      The temporal grid variables are read in initial file :
!!        * The number of time steps for the current segment depends on the time step
!!     PTSTEP and on the segment length PSEGLEN plus one time step of the first
!!     model for all models.
!!        * The time of the beginning of experiment (TDTEXP of type DATE_TIME)
!!     is read only at the first call  by INI_MODEL1 (KMI=1),
!!     since it is the same for all nested models.
!!        * The times of the  beginning of model (TPDTMOD of type DATE_TIME),
!!     of beginning of segment (TPDTSEG  of type DATE_TIME) are read for
!!     all models
!!
!!      Then, the other spatial grid variables are deduced :
!!        * If Cartesian geometry (LCARTESIAN=.TRUE.), SM_GRIDCART computes
!!      the horizontal stretchings (PDXHAT and PDYHAT) the height (PZZ) and the
!!      Jacobian (PJ).
!!        * if Spherical geometry (LCARTESIAN=.FALSE.), SM_GRIDPROJ computes
!!      the horizontal stretchings (PDXHAT and PDYHAT) the height (PZZ), the
!!      Jacobian (PJ), the map factor (PMAP), the latitude (PLAT) and the
!!      longitude (PLON).
!!
!!      and  the other temporal  grid variables are deduced :
!!        The current time (TPDTCUR of type DATE_TIME) is set equal to the time
!!    of beginning of segment.
!!
!!     IF verbose option (NVERB >=5), the time is printed on output-listing
!!    EXTERNAL
!!    --------
!!      FMREAD      : to read data in LFIFM file
!!      FMLOOK      : to retrieve a logical unit number
!!
!!      Module MODE_GRIDPROJ : contains conformal projection routines
!!        SM_GRIDPROJ : to compute some grid variables in case of conformal
!!                       projection
!!        SM_LATLON   : to compute latitude and longitude, giving the
!!                      positions on the grid
!!      Module MODE_GRIDCART : contains  cartesian geometry routines
!!        SM_GRIDCART : to compute some grid_variables in case of cartesian
!!                       geometry
!!      Module MODE_TIME : contains SM_PRINT_TIME routine
!!                         and uses module MODD_TIME (for definition
!!                         of types DATE_TIME and DATE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!      Module MODD_CONF       : contains declaration of configuration variables
!!                              for all models
!!         CCONF      : Configuration for all models ( START, RESTART or POST)
!!         LCARTESIAN :  Logical for cartesian geometry
!!                       .TRUE.  = cartesian geometry
!!         NVERB      : Level of informations on output-listing
!!                          0 for minimum  prints
!!                          5 for intermediate level of prints
!!                         10 for maximum  prints
!!         CSTORAGE_TYPE : type of stored informations ( 2 or one instant)
!!
!!
!!      Module MODD_GRID       : contains spatial  grid variables for all model
!!
!!         XLON0 : Reference longitude for the conformal projection
!!         XLAT0 : Reference latitude
!!         XBETA : Rotation angle
!!         XRPK  : Projection parameter for the conformal projection
!!
!!      Module MODE_TIME      : uses module MODD_TIME (contains temporal grid
!!                            variables for all model
!!                  TDTEXP : Date and time for the experiment beginning
!!                  TDTSEG : Date and time for the segment beginning
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine SET_GRID)
!!
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    30/06/94
!!      J. STEIN    02/01/95  correct the TPDTCUR initialization
!!      J. STEIN    26/01/95  read TPDTCUR in the FM-file
!!      J. STEIN    16/03/95  bug in the TPDTCUR reading
!!      J. STEIN    16/04/95  another bug in the TPDTCUR initialization
!!      J. STEIN    03/01/96  change the temporal grid
!!      J. STEIN P.JABOUILLE 30/04/96 add the storage-type reading
!!      J. STEIN    25/05/96  read RPK only in the non-cartesian case
!!      J.P. LAFORE 03/07/97  gridnesting implementation
!!      V. DUCROCQ   13/08/98  //
!!      J. STEIN    01/02/99  change the orography at the boundary for the
!!                            grid-nesting lbc
!!     V.MASSON 12/10/00 read of the orography in all cases, even if LFLAT=T
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
USE MODE_FM
!              ------------
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_BUDGET
USE MODD_DYN
USE MODD_FMOUT
USE MODD_NESTING
!
USE MODE_GRIDCART
USE MODE_GRIDPROJ
USE MODE_TIME
USE MODE_ll
USE MODI_GATHER_ll  !!!! a mettre dans mode_ll
!
USE MODE_FMREAD
USE MODD_OUT_n, ONLY : OUT_MODEL
USE MODD_VAR_ll, ONLY : IP,NPROC
USE MODD_DYN_n, ONLY : DYN_MODEL
!
IMPLICIT NONE
!
!*       0.1   declarations of argument
!
INTEGER,                INTENT(IN)  :: KMI       ! Model index
CHARACTER (LEN=*),      INTENT(IN)  :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN)  :: HLUOUT    ! name for output-listing
                                                 !  of nested models
INTEGER,                INTENT(IN)  :: KIU       ! Upper dimension in x direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KJU       ! Upper dimension in y direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KKU       ! Upper dimension in z direction
                                                 ! for domain arrays
INTEGER,               INTENT(IN)   :: KIMAX_ll  !  Dimensions  in x direction
                                                 ! of the physical domain,
INTEGER,               INTENT(IN)   :: KJMAX_ll  !  Dimensions  in y direction
                                                 ! of the physical domain,
REAL, DIMENSION(:), INTENT(IN) :: PBMX1,PBMX2,PBMX3,PBMX4 ! Mass points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBMY1,PBMY2,PBMY3,PBMY4 ! Mass points in Y-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFX1,PBFX2,PBFX3,PBFX4 ! Flux points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFY1,PBFY2,PBFY3,PBFY4 ! Flux points in Y-direc.
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the inner model domain, relative to outer model
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction resolution RATIO
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between inner model and outer model
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCX   ! type of lateral
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCY   ! boundary conditions
!
REAL,                   INTENT(IN)  :: PTSTEP    ! time step of model KMI
REAL,                   INTENT(INOUT) :: PSEGLEN ! segment duration (in seconds)
!
REAL,                   INTENT(OUT) :: PLONORI   ! Longitude  of the
                                                 ! Origine point of the
                                                 ! conformal projection
REAL,                   INTENT(OUT) :: PLATORI   ! Latitude of the
                                                 ! Origine point of the
                                                 ! conformal projection
REAL, DIMENSION(:,:),   INTENT(OUT) :: PLON,PLAT ! Longitude and latitude
REAL, DIMENSION(:),     INTENT(OUT) :: PXHAT     ! Position x in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:),     INTENT(OUT) :: PYHAT     ! Position y in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:),     INTENT(OUT) :: PDXHAT    ! horizontal stretching in x
REAL, DIMENSION(:),     INTENT(OUT) :: PDYHAT    ! horizontal stretching in y
REAL, DIMENSION(:,:),   INTENT(OUT) :: PMAP      ! Map factor
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZS       ! orography
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PZZ       ! Height z
REAL, DIMENSION(:),     INTENT(OUT) :: PZHAT     ! Height  level
LOGICAL,                INTENT(OUT) :: OSLEVE    ! flag for SLEVE coordinate
REAL,                   INTENT(OUT) :: PLEN1     ! Decay scale for smooth topography
REAL,                   INTENT(OUT) :: PLEN2     ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZSMT     ! smooth-orography
!
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTMOD   ! date and time of the model
                                                 ! beginning
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTCUR   ! Current date and time
INTEGER,                INTENT(OUT) :: KSTOP     ! number of time steps for
                                                 ! current segment
INTEGER,POINTER,        INTENT(OUT) :: KOUT_NUMB ! number of outputs
TYPE(TOUTBAK),DIMENSION(:),POINTER,INTENT(OUT) :: TPOUTBAKN ! List of outputs and backups
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PJ        ! Jacobian
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZXHAT_ll    !  Position x in the conformal
                                                 ! plane (array on the complete domain)
REAL, DIMENSION(:), ALLOCATABLE   :: ZYHAT_ll    !   Position y in the conformal
                                                 ! plane (array on the complete domain)
REAL                         :: ZXHATM,ZYHATM    ! coordinates of mass point
REAL                         :: ZLATORI, ZLONORI ! lat and lon of left-bottom point
REAL                         :: ZTEMP            ! Intermediate variable
INTEGER                      :: IPOS
INTEGER                :: IGRID,ILENCH,IRESP  !   File
CHARACTER (LEN=16)     :: YRECFM              ! management
CHARACTER (LEN=100)    :: YCOMMENT            ! variables
CHARACTER (LEN=2)      :: YDIR                !
INTEGER, DIMENSION(3)  :: ITDATE           ! date array
CHARACTER (LEN=40)     :: YTITLE                    ! Title for date print
INTEGER                :: ILUOUT                    ! Logical unit number for
                                                    ! output-listing
INTEGER                :: JKLOOP,JOUT,IDX           ! Loop index
INTEGER                :: IIUP,IJUP ,ISUP=1         ! size  of working
                                                    ! window arrays,
                                                    ! supp. time steps
!
INTEGER                :: IMASDEV                   ! masdev of the file
INTEGER                :: IMI                       ! model number for loop
INTEGER                :: IOUT_NUMB                 ! number of outputs
!-------------------------------------------------------------------------------
!
YRECFM='MASDEV'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.    READ GRID  VARIABLES IN INITIAL FILE
!              ------------------------------------
!
!*       1.1   Spatial grid
!
  YRECFM='STORAGE_TYPE'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,CSTORAGE_TYPE,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (IRESP /= 0) CSTORAGE_TYPE='TT'
  !
IF (KMI == 1) THEN
  YRECFM='LON0'     ! this parameter is also useful in the cartesian to
  YDIR='--'        ! compute the sun position for the radiation scheme
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LAT0'     ! this parameter is also useful in the cartesian to
  YDIR='--'        ! compute the Coriolis parameter
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='BETA'     ! this parameter is also useful in the cartesian to
  YDIR='--'           ! rotate the simulatin domain
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
YRECFM='XHAT'
YDIR='XX'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='YHAT'
YDIR='YY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='RPK'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (IMASDEV > 45) THEN
    YRECFM='LONORI'
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
  !
    YRECFM='LATORI'
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  ELSE                     
    CALL FMREAD(HINIFILE,'LONOR',HLUOUT,'--',PLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HINIFILE,'LATOR',HLUOUT,'--',PLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
    ALLOCATE(ZXHAT_ll(KIMAX_ll+ 2 * JPHEXT),ZYHAT_ll(KJMAX_ll+2 * JPHEXT))
    CALL GATHERALL_FIELD_ll('XX',PXHAT,ZXHAT_ll,IRESP) !//
    CALL GATHERALL_FIELD_ll('YY',PYHAT,ZYHAT_ll,IRESP) !//
    ZXHATM = - 0.5 * (ZXHAT_ll(1)+ZXHAT_ll(2))
    ZYHATM = - 0.5 * (ZYHAT_ll(1)+ZYHAT_ll(2))
    CALL SM_LATLON(PLATORI,PLONORI,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    DEALLOCATE(ZXHAT_ll,ZYHAT_ll)
    PLATORI = ZLATORI
    PLONORI = ZLONORI
  END IF
  !
END IF

YRECFM='ZS'
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZHAT'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PZHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
CALL DEFAULT_SLEVE(OSLEVE,PLEN1,PLEN2)
!
IF (IMASDEV<=46) THEN
  PZSMT  = PZS
  OSLEVE = .FALSE.
ELSE
  YRECFM='SLEVE'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,OSLEVE,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='ZSMT'
  YDIR='XY'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF (OSLEVE) THEN
  YRECFM='LEN1'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PLEN1,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LEN2'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,PLEN2,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!*       1.2   Temporal grid
!
IF (KMI == 1) THEN
  YRECFM='DTEXP%TDATE'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
  TDTEXP%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3))
  YRECFM='DTEXP%TIME'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTEXP%TIME,IGRID,ILENCH,           &
             YCOMMENT,IRESP)
END IF
!
YRECFM='DTCUR%TDATE'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TPDTCUR%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3))
YRECFM='DTCUR%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TPDTCUR%TIME,IGRID,ILENCH,           &
            YCOMMENT,IRESP)
!
YRECFM='DTMOD%TDATE'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TPDTMOD%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3))
YRECFM='DTMOD%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TPDTMOD%TIME,IGRID,ILENCH,           &
            YCOMMENT,IRESP)
!
YRECFM='DTSEG%TDATE'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TDTSEG%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3))
YRECFM='DTSEG%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTSEG%TIME,IGRID,ILENCH,           &
            YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       2.    SET OTHER GRID VARIABLES
!              ------------------------
!
!
!*       2.1    Spatial grid
!
IF (LCARTESIAN) THEN
  CALL SM_GRIDCART(HLUOUT,PXHAT,PYHAT,PZHAT,PZS,OSLEVE,PLEN1,PLEN2,PZSMT,PDXHAT,PDYHAT,PZZ,PJ)
ELSE
  CALL SM_GRIDPROJ(HLUOUT,PXHAT,PYHAT,PZHAT,PZS,OSLEVE,PLEN1,PLEN2,PZSMT,PLATORI,PLONORI, &
                   PMAP,PLAT,PLON,PDXHAT,PDYHAT,PZZ,PJ)
END IF
!
!*       2.2    Temporal grid - segment length
!
IF (CPROGRAM /= 'DIAG  ') TDTSEG = TPDTCUR
ISUP = 1     ! 1 corresponds to a last timestep
   ! to obtain the prognostic and diagnostic fields all along this timestep
!
IF ( KMI == 1) PSEGLEN = PSEGLEN + PTSTEP*ISUP ! needed for the gridnesting case to get
                                               ! the same PSEGLEN for all nested models
KSTOP = NINT(PSEGLEN/PTSTEP)
!
!
!*       2.3    Temporal grid - outputs managment
!
! The output/backups times have been read only by model 1
IF (KMI == 1) THEN
DO IMI = 1, NMODEL
  !
  !*       2.3.1  Synchronization between nested models through XBAK_TIME arrays (MODD_FMOUT)
  !
  DO JOUT = 1,JPOUTMAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
      !Value is rounded to nearest timestep
      XBAK_TIME(IMI,JOUT) = NINT(XBAK_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) * DYN_MODEL(IMI)%XTSTEP
      !Output/backup time is propagated to nested models (with higher numbers)
      !PW: TODO: BUG?: what happens if 2 dissociated models?
      DO JKLOOP = IMI+1,NMODEL
        IDX = 1
        !Find first non 'allocated' element
        DO WHILE ( XBAK_TIME(JKLOOP,IDX) >= 0. )
          IDX = IDX + 1
        END DO
        IF (IDX > JPOUTMAX) THEN
          PRINT *,'Error in SET_GRID when treating output list'
          CALL ABORT
          STOP
        END IF
        XBAK_TIME(JKLOOP,IDX) = XBAK_TIME(IMI,JOUT)
      END DO
    END IF
  END DO
  !
  !*       2.3.2 Find duplicated entries
  !
  DO JOUT = 1,JPOUTMAX
    DO JKLOOP = JOUT+1,JPOUTMAX
      IF ( XBAK_TIME(IMI,JKLOOP) == XBAK_TIME(IMI,JOUT) .AND. XBAK_TIME(IMI,JKLOOP) >= 0. ) THEN
        print *,'WARNING: found duplicated backup (removed extra one)'
        XBAK_TIME(IMI,JKLOOP) = -1.
      END IF
    END DO
  END DO
  !
  !*       2.3.3 Sort entries
  !
  DO JOUT = 1,JPOUTMAX
    ZTEMP = XBAK_TIME(IMI,JOUT)
    IF (ZTEMP<0.) ZTEMP = 1e99
    IPOS = -1
    DO JKLOOP = JOUT+1,JPOUTMAX
      IF ( XBAK_TIME(IMI,JKLOOP) < ZTEMP .AND. XBAK_TIME(IMI,JKLOOP) >= 0. ) THEN
        ZTEMP = XBAK_TIME(IMI,JKLOOP)
        IPOS = JKLOOP
      END IF
    END DO
    IF (IPOS >= JOUT) THEN
      XBAK_TIME(IMI,IPOS) = XBAK_TIME(IMI,JOUT)
      XBAK_TIME(IMI,JOUT) = ZTEMP
    END IF
  END DO
  !
  !*       2.3.4 counting the output number of model IMI
  !
  IOUT_NUMB = 0
  DO JOUT = 1,JPOUTMAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
      IOUT_NUMB = IOUT_NUMB + 1
    END IF
  END DO
  !
  OUT_MODEL(IMI)%NOUT_NUMB = IOUT_NUMB
  ALLOCATE(OUT_MODEL(IMI)%TOUTBAKN(IOUT_NUMB))
  !
  IPOS = 0
  DO JOUT = 1,JPOUTMAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
        IPOS = IPOS + 1
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NSTEP = NINT(XBAK_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) + 1
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%XTIME = XBAK_TIME(IMI,JOUT)
    END IF
  END DO
  !
  !*       2.3.5 finding dad output number
  !
  !Security check (if it happens, this part of the code should be exported outside of the IMI loop)
  IF (NDAD(IMI)>IMI) THEN
    print *,'ERROR in SET_GRID'
    STOP
  END IF
  IF (NDAD(IMI) == IMI .OR.  IMI == 1) THEN
    OUT_MODEL(IMI)%TOUTBAKN(:)%NOUTDAD = 0
  ELSE
    DO IPOS = 1,OUT_MODEL(IMI)%NOUT_NUMB
      IDX = 0
      DO JOUT = 1,OUT_MODEL(NDAD(IMI))%NOUT_NUMB
        IF ( OUT_MODEL(NDAD(IMI))%TOUTBAKN(JOUT)%XTIME <= OUT_MODEL(IMI)%TOUTBAKN(IPOS)%XTIME+1.E-6 ) THEN
          IDX = JOUT
        ELSE
          EXIT
        END IF
      END DO
      IF (IDX>0) THEN
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTDAD = IDX
      ELSE
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTDAD = -1
      END IF
    END DO
  END IF
  !
  !
  IF (IP==1) THEN
  PRINT *,'-------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of backups: ',IOUT_NUMB
  PRINT *,'Timestep     Time'
  DO JOUT = 1,IOUT_NUMB
    WRITE(*,'( I9 F12.3 )'  ) OUT_MODEL(IMI)%TOUTBAKN(JOUT)%NSTEP,OUT_MODEL(IMI)%TOUTBAKN(JOUT)%XTIME
  END DO
  PRINT *,'-------------------------'
  END IF
  !
END DO ! IMI=1,NMODEL
!
DEALLOCATE(XBAK_TIME)
!
END IF ! IMI==1
!
KOUT_NUMB => OUT_MODEL(KMI)%NOUT_NUMB
TPOUTBAKN => OUT_MODEL(KMI)%TOUTBAKN
!
!-------------------------------------------------------------------------------
!
!*       3.    PRINT ON OUTPUT-LISTING
!              -----------------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
IF  (NVERB >= 10) THEN
  IIUP = SIZE(PXHAT)
  IJUP = SIZE(PYHAT)
  IF(LCARTESIAN) THEN
    WRITE(ILUOUT,FMT=*) 'SET_GRID : No map projection used.'
  ELSE
    IF (XRPK == 1.) THEN
      WRITE(ILUOUT,FMT=*) 'SET_GRID : Polar stereo used.'
    ELSE IF (XRPK == 0.) THEN
      WRITE(ILUOUT,FMT=*) 'SET_GRID : Mercator used.'
    ELSE
      WRITE(ILUOUT,FMT=*) 'SET_GRID : Lambert used, cone factor=',XRPK
      WRITE(ILUOUT,FMT=*) ' SET_GRID : LON0 = ',XLON0,' LAT0 = ',XLAT0, &
       ' RPK = ',XRPK,' BETA = ',XBETA,' LONORI = ',PLONORI,' LATORI = ',PLATORI
    END IF
  END IF
  WRITE(ILUOUT,FMT=*) ' SET_GRID : Some PXHAT values:'
  WRITE(ILUOUT,FMT=*) ' I= 1        I=IIU/2       I=IIU'
  WRITE(ILUOUT,FMT=*) PXHAT(1),PXHAT(IIUP/2),PXHAT(IIUP)
!
  WRITE(ILUOUT,FMT=*) ' SET_GRID : Some PYHAT values:'
  WRITE(ILUOUT,FMT=*) ' I= 1        I=IIU/2       I=IIU'
  WRITE(ILUOUT,FMT=*) PYHAT(1),PYHAT(IJUP/2),PYHAT(IJUP)
!
  WRITE(ILUOUT,FMT=*) ' SET_GRID : Some PZHAT values:'
  WRITE(ILUOUT,FMT=*) ' I= 1        I=IIU/2       I=IIU'
  WRITE(ILUOUT,FMT=*) PZHAT(1),PZHAT(KKU/2),PZHAT(KKU)
!
  WRITE(ILUOUT,FMT=*) ' SET_GRID : Some PZS values:'
  WRITE(ILUOUT,FMT=*) ' I= 1        I=IIU/2       I=IIU'
  WRITE(ILUOUT,FMT=*) PZS(1,1),PZS(IIUP/2,IJUP/2),PZS(IIUP,IJUP)
!
  YTITLE='CURRENT DATE AND TIME'
  CALL SM_PRINT_TIME(TPDTCUR,HLUOUT,YTITLE)
END IF
IF (NVERB >= 5) THEN
  YTITLE='DATE AND TIME OF EXPERIMENT BEGINNING'
  CALL SM_PRINT_TIME(TDTEXP,HLUOUT,YTITLE)
  YTITLE='DATE AND TIME OF MODEL BEGINNING'
  CALL SM_PRINT_TIME(TPDTMOD,HLUOUT,YTITLE)
END IF
YTITLE='DATE AND TIME OF SEGMENT BEGINNING'
CALL SM_PRINT_TIME(TDTSEG,HLUOUT,YTITLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SET_GRID
