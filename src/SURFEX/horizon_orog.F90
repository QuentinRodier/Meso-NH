!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HORIZON_OROG(U, UG, USS, CSVF, LFSSOSVF)
!     #########################
!
!!*HORIZON_OROG  computes the horizon and SVF from orography
!!
!!
!!    METHOD
!!    ------
!!    See Senkova et al, 2007
!!   
!!    AUTHOR
!!    ------
!!
!!    A.Mary        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    24/03/2015
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t 
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n,  ONLY : SSO_t
!
USE MODD_SURFEX_MPI,     ONLY : NRANK, NPIO
USE MODD_CSTS,           ONLY : XPI
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGDWORK,        ONLY : XHALORADIUS, XFLATRAD
USE MODD_PGD_GRID,       ONLY : NL, CGRID
!
USE MODI_GET_MESH_DIM
USE MODI_GET_GRID_DIM
USE MODI_ABOR1_SFX
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SSO_t), INTENT(INOUT)  :: USS
CHARACTER(LEN=16), INTENT(IN)       :: CSVF  ! formula for SVF computation:
                                             !  'SENKOVA' = Senkova et al. 2007
                                             !  'MANNERS' = Manners et al. 2012
LOGICAL, INTENT(IN) :: LFSSOSVF      ! compute SVF on fractional slopes if possible
!
!*    0.2    Declaration of indexes
!            ----------------------
!
!*    0.3    Declaration of working arrays
!            -----------------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IHALO   ! index of points in halo
REAL, DIMENSION(:), ALLOCATABLE    :: ZHALO   ! distance of points in halo
REAL, DIMENSION(:), ALLOCATABLE    :: ZDX     ! grid mesh size in x direction
REAL, DIMENSION(:), ALLOCATABLE    :: ZDY     ! grid mesh size in y direction
REAL, DIMENSION(:), ALLOCATABLE    :: ZMESH_SIZE
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON    ! longitude of the gridpoints (rad)
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT    ! latitude of the gridpoints (rad)
REAL, DIMENSION(:), ALLOCATABLE    :: ZZS
REAL, DIMENSION(:), ALLOCATABLE    :: ZSLOPE, ZASPECT
!
REAL, DIMENSION(:,:), ALLOCATABLE    :: ZHMINS_DIR
REAL, DIMENSION(:,:), ALLOCATABLE    :: ZHMAXS_DIR
REAL, DIMENSION(:), ALLOCATABLE    :: ZSVF
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFRAC_DIR, ZSLOPE_DIR
!
REAL, DIMENSION(USS%NSECTORS,2)  :: ZSECTORS ! aspect sectors delimitation :
                                             ! sector 1 being centered on North,
                                             ! with clockwise sense
REAL, DIMENSION(USS%NSECTORS) :: ZLHAVG   ! avg local horizon by sector
REAL, DIMENSION(USS%NSECTORS) :: ZLHMIN   ! min local horizon by sector
REAL, DIMENSION(USS%NSECTORS) :: ZLHMAX   ! max local horizon by sector
REAL, DIMENSION(USS%NSECTORS) :: ZHPHI    ! local horizon by sector (as defined by Manners et al. 2012)
!
INTEGER, DIMENSION(USS%NSECTORS) :: ILHAVG   ! counter for averaging
INTEGER, DIMENSION(USS%NSECTORS) :: IC       ! counting index
!
REAL,    DIMENSION(0:359) :: ZLH          ! subgrid local horizon 
LOGICAL, DIMENSION(0:359) :: GLH          ! indicator for ZLH having been filled or not
!
REAL    :: ZCOSLAT   ! local variable
REAL    :: ZSINLAT   ! local variable
REAL    :: ZDIR      ! local variable (direction in halo)
REAL    :: ZDIST     ! local variable (distance in halo)
!
REAL :: ZTPHI        ! local slope (as defined by Manners et al. 2012)
REAL :: ZVPHI        ! local tilted horizon (as defined by Manners et al. 2012)
REAL :: ZSVF0        ! local SVF (as defined by Manners et al. 2012)

REAL :: ZM           ! local subscale M derivatives
REAL :: ZK           ! local subscale K derivatives
REAL :: ZL           ! local subscale L derivatives
REAL :: ZD           ! local subscale D derivatives
                     ! cf. Senkova & Rontu 2013
REAL :: ZASPECT0     ! local direction 
REAL  :: ZSECTOR_ANGLE ! angle of one sector
!
INTEGER :: IIMAX        ! X-size of grid
INTEGER :: IJMAX        ! Y-size of grid
INTEGER :: IRADIUS      ! radius of halo in number of gridpoints
INTEGER :: ICOUNT       ! number of gridpoints actually in halo
INTEGER :: IDIR_TOUCHED ! number of angles "touched" by one gridpoint in halo
INTEGER :: JL           ! loop index on grid meshs
INTEGER :: JI,JJ,JK,II,IJ,IIND,JF  ! indexes
!
LOGICAL :: LLRECT  ! rectangular grid
!
!*    0.4    Declaration of other local variables
!            ------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
IF (LHOOK) CALL DR_HOOK('HORIZON_OROG',0,ZHOOK_HANDLE)
!
! definition of sectors
ZSECTOR_ANGLE = 2.*XPI/USS%NSECTORS
ZSECTORS(1,1) = -ZSECTOR_ANGLE/2. ! first sector is centered on 0.0 rad
ZSECTORS(1,2) = ZSECTORS(1,1) + ZSECTOR_ANGLE ! then rotate by sector
DO JI=2,USS%NSECTORS
  ZSECTORS(JI,1) = ZSECTORS(JI-1,2)
  ZSECTORS(JI,2) = ZSECTORS(JI,1) + ZSECTOR_ANGLE
ENDDO
!
IF ( NRANK==NPIO ) THEN
  !
  ALLOCATE(ZLAT(U%NDIM_FULL),ZLON(U%NDIM_FULL))
  ALLOCATE(ZZS(U%NDIM_FULL))
  ALLOCATE(ZSLOPE(U%NDIM_FULL),ZASPECT(U%NDIM_FULL))
  ALLOCATE(ZFRAC_DIR (U%NDIM_FULL,USS%NSECTORS))
  ALLOCATE(ZSLOPE_DIR(U%NDIM_FULL,USS%NSECTORS))
  !
ELSE
  !
  ALLOCATE(ZLON(0),ZLAT(0),ZZS(0))
  ALLOCATE(ZSLOPE(0),ZASPECT(0))
  ALLOCATE(ZSLOPE_DIR(0,0),ZFRAC_DIR(0,0))
  !
ENDIF
!
CALL GATHER_AND_WRITE_MPI(UG%G%XLAT,ZLAT)
CALL GATHER_AND_WRITE_MPI(UG%G%XLON,ZLON)
CALL GATHER_AND_WRITE_MPI(USS%XAVG_ZS,ZZS)
CALL GATHER_AND_WRITE_MPI(USS%XSLOPE,ZSLOPE)
CALL GATHER_AND_WRITE_MPI(USS%XASPECT,ZASPECT)
CALL GATHER_AND_WRITE_MPI(USS%XSLOPE_DIR,ZSLOPE_DIR)
CALL GATHER_AND_WRITE_MPI(USS%XFRAC_DIR,ZFRAC_DIR)
!
IF (NRANK==NPIO) THEN
  !
  ! deg2radians conversions
  ZLAT(:) = ZLAT(:) * XPI/180.
  ZLON(:) = ZLON(:) * XPI/180.
  !
  ALLOCATE(ZDX(U%NDIM_FULL),ZDY(U%NDIM_FULL))
  ALLOCATE(ZMESH_SIZE(U%NDIM_FULL))
  CALL GET_MESH_DIM(CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,ZDX,ZDY,ZMESH_SIZE)
  DEALLOCATE(ZMESH_SIZE)
  CALL GET_GRID_DIM(CGRID,UG%NGRID_FULL_PAR,UG%XGRID_FULL_PAR,LLRECT,IIMAX,IJMAX)
  !
  ALLOCATE(ZHMINS_DIR(U%NDIM_FULL,USS%NSECTORS))
  ALLOCATE(ZHMAXS_DIR(U%NDIM_FULL,USS%NSECTORS))
  !
  ALLOCATE(ZSVF(U%NDIM_FULL))
  IF (CSVF == 'SENKOVA') THEN
    ZSVF(:) = 1.
  ELSEIF (CSVF == 'MANNERS') THEN
    ZSVF(:) = 0.
  ENDIF
  !
  IRADIUS = NINT(XHALORADIUS/((MINVAL(ZDX(:))+MINVAL(ZDY(:)))/2.))
  ALLOCATE(IHALO((2*IRADIUS+1)**2))
  ALLOCATE(ZHALO((2*IRADIUS+1)**2))
  !
  !
  !----------------------------------------------------------------------------
  !
  !*    2.     Loop on grid points
  !            -------------------
  !
  DO JL=1,U%NDIM_FULL
    !
    ! 2.1 - halo definition inspired from GET_NEAR_MESHES (CONF_PROJ),
    ! but locally only
    IRADIUS = NINT(XHALORADIUS/((ZDX(JL)+ZDY(JL))/2))
    II = MOD(JL-1,IIMAX)+1
    IJ = 1 + (JL-II)/IIMAX
    ICOUNT = 0
    DO JI = -IRADIUS,IRADIUS
      IF (II + JI > 0 .AND. II + JI <= IIMAX) THEN
        DO JJ = -IRADIUS,IRADIUS
          IF (IJ + JJ > 0 .AND. IJ + JJ <= IJMAX) THEN
            IF (JI**2 + JJ**2 < IRADIUS**2) THEN
              ICOUNT = ICOUNT + 1
              IHALO(ICOUNT) = (II+JI) + IIMAX * (IJ+JJ-1)
              ZHALO(ICOUNT) = SQRT(JI*JI*ZDX(JL)*ZDX(JL) + JJ*JJ*ZDY(JL)*ZDY(JL))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    !
    IF (.FALSE.) THEN ! we can compute subscale (NSSO)
      ! pb: how to get lon/lat of SSO subboxes ???
    ELSE ! we compute at mesh scale

      ZCOSLAT = COS(ZLAT(JL))
      ZSINLAT = SIN(ZLAT(JL))
      ZLH(:) = 0.
      GLH(:) = .FALSE.

      ! scan halo, computing horizon angle and direction
      DO JK = 1,ICOUNT

        IF ( ZZS(IHALO(JK))<XUNDEF ) THEN

          ZDIST = ZHALO(JK)

          IF (ZDIST > 0.1) THEN ! distance > 0.1m, not to take self point into account

            ZDIR = ATAN2( SIN(ZLON(JL)-ZLON(IHALO(JK)))*COS(ZLAT(IHALO(JK))), &
                          ZCOSLAT*SIN(ZLAT(IHALO(JK))) - ZSINLAT*COS(ZLAT(IHALO(JK))) * &
                          COS(ZLON(JL)-ZLON(IHALO(JK))) )

            ! - shift to clockwise sense
            ZDIR = 2.*XPI - ZDIR
            ZDIR = MOD(ZDIR, 2*XPI)
            IIND = NINT(ZDIR*180./XPI)

            ! am: comparison between meshsize and r.dTheta, with dTheta = 1°
            ! for the point to be diffused on adjacent angles 
            IDIR_TOUCHED = NINT((ZDX(JL)+ZDY(JL))/2./(ZDIST*XPI/180.))

            DO JI=IIND-IDIR_TOUCHED/2,IIND+IDIR_TOUCHED/2
              IF (JI < 0) THEN
                JJ = JI + 360
              ELSEIF (JI >= 360) THEN
                JJ = JI - 360
              ELSE
                JJ = JI
              ENDIF
              GLH(JJ) = .TRUE.
              IF (ZZS(IHALO(JK)) > ZZS(JL)) THEN
                ZLH(JJ) = MAX(ZLH(JJ), SIN(ATAN((ZZS(IHALO(JK)) - ZZS(JL))/ZDIST)))
              ENDIF
            ENDDO

          ENDIF

        ENDIF

      ENDDO

      ZLHMIN(:) = 1.1
      ZLHMAX(:) = 0.
      ZLHAVG(:) = 0.
      ILHAVG(:) = 0

      DO JI = 0,359

        ! min/max local horizon by sector
        ZASPECT0 = JI*XPI/180.
        IF (ZASPECT0 >= ZSECTORS(USS%NSECTORS,2)) THEN
          ZASPECT0 = ZASPECT0 - 2*XPI
        ENDIF
        IF (GLH(JI)) THEN
          DO JK=1,USS%NSECTORS
            IF (ZASPECT0 >= ZSECTORS(JK,1) .AND. ZASPECT0 < ZSECTORS(JK,2)) THEN
              ZLHAVG(JK) = ZLHAVG(JK) + ZLH(JI)
              ILHAVG(JK) = ILHAVG(JK) + 1
              ZLHMIN(JK) = MIN(ZLHMIN(JK), ZLH(JI))
              ZLHMAX(JK) = MAX(ZLHMAX(JK), ZLH(JI))
              EXIT
            ENDIF
          ENDDO
        ENDIF

      ENDDO

      WHERE (ILHAVG(:)/=0)
        ZLHAVG(:) = ZLHAVG(:)/ILHAVG(:)
      END WHERE
      WHERE (ZLHMIN(:) > 1.) ZLHMIN(:) = 0.

      ! store min and max horizon
      DO JK=1,USS%NSECTORS
        ZHMINS_DIR(JL,JK) = ZLHMIN(JK)
        ZHMAXS_DIR(JL,JK) = ZLHMAX(JK)
      ENDDO

      ! compute SVF
      IF (CSVF == 'SENKOVA') THEN

        IF (LFSSOSVF) THEN
          CALL ABOR1_SFX("HORIZON_OROG: CSVF='SENKOVA' and LFSSOSVF=.TRUE. not compatible")
        ENDIF
        DO JK=1,USS%NSECTORS
          ZSVF(JL) = ZSVF(JL) - ZLHAVG(JK)/USS%NSECTORS
        ENDDO
      
      ELSEIF (CSVF == 'MANNERS') THEN
            
        ZHPHI(:) = XPI/2. - ASIN(ZLHAVG(:))
      
        ! using Manners et al. 2012 formula
        IF (LFSSOSVF .AND. SUM(ZFRAC_DIR(JL,:)) >= 0.99) THEN

          ! compute one tilted SVF by fractional aspect, then average
          DO JF=1,USS%NSECTORS
            ZSVF0 = 0.
            DO JK=1,USS%NSECTORS
              ZTPHI = XPI + ATAN(1./(-TAN(ZSLOPE_DIR(JL, JF))*COS(2.*XPI*(JK-JF)/8.)))
              ZTPHI = MOD(ZTPHI, XPI)
              ZVPHI = MIN(ZHPHI(JK), ZTPHI) + XPI/2. - ZTPHI
              ZSVF0 = ZSVF0 + SIN(ZVPHI)**2/USS%NSECTORS
            ENDDO
            ZSVF(JL) = ZSVF(JL) + USS%XFRAC_DIR(JL, JF)*ZSVF0
          ENDDO

        ELSE

          ! compute @ grid scale
          DO JK=1,USS%NSECTORS
            ZTPHI = XPI + ATAN(1./(-TAN(ZSLOPE(JL))*COS(ZASPECT(JL) - 2.*XPI*(JK-1)/8.)))
            ZTPHI = MOD(ZTPHI, XPI)
            ZVPHI = MIN(ZHPHI(JK), ZTPHI) + XPI/2. - ZTPHI
            !ZVPHI = ZHPHI(JK) ! plane point
            ZSVF(JL) = ZSVF(JL) + SIN(ZVPHI)**2/USS%NSECTORS
          ENDDO

        ENDIF
      
      ENDIF

    ENDIF

  ENDDO

  DEALLOCATE(IHALO,ZHALO)
  DEALLOCATE(ZDX,ZDY)

ELSE
  !
  ALLOCATE(ZHMINS_DIR(0,0),ZHMAXS_DIR(0,0))
  ALLOCATE(ZSVF(0))
  !
ENDIF
!
DEALLOCATE(ZLON,ZLAT,ZZS)
DEALLOCATE(ZSLOPE,ZSLOPE_DIR,ZFRAC_DIR,ZASPECT)
!
CALL READ_AND_SEND_MPI(ZHMINS_DIR,USS%XHMINS_DIR)
CALL READ_AND_SEND_MPI(ZHMAXS_DIR,USS%XHMAXS_DIR)
DEALLOCATE(ZHMINS_DIR,ZHMAXS_DIR)
!
CALL READ_AND_SEND_MPI(ZSVF,USS%XSVF)
DEALLOCATE(ZSVF)
!
IF (LHOOK) CALL DR_HOOK('HORIZON_OROG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE HORIZON_OROG
