!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ##########################
MODULE MODI_STATION_n
!      ##########################
!
INTERFACE
!
      SUBROUTINE STATION_n(PTSTEP,                               &
                           PXHAT, PYHAT, PZ,                     &
                           PU, PV, PW, PTH, PR, PSV, PTKE,       &
                           PTS,PP ) 
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE STATION_n
!
END INTERFACE
!
END MODULE MODI_STATION_n
!
!     ########################################################
      SUBROUTINE STATION_n(PTSTEP,                           &
                       PXHAT, PYHAT, PZ,                     &
                       PU, PV, PW, PTH, PR, PSV, PTKE,       &
                       PTS, PP )
!     ########################################################
!
!
!!****  *STATION_n* - (advects and) stores 
!!                                stations/s in the model
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Pierre TULET / Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/02/2002
!  A. Lemonsu   19/11/2002
!  P. Aumond    01/07/2011: add model levels
!  C. Lac          04/2013: correction on the vertical levels
!  C. Lac          04/2013: add I/J positioning
!  P. Wautelet  28/03/2018: replace TEMPORAL_DIST by DATETIME_DISTANCE
!  P. Wautelet  05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet  13/09/2019: budget: simplify and modernize date/time management
!  R. Schoetter    11/2019: use LCARTESIAN instead of LSTATLAT for multiproc in cartesian
!  P. Wautelet  07/04/2022: rewrite types for stations
!
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CONF
USE MODD_CST
USE MODD_DIAG_IN_RUN
USE MODD_GRID
USE MODD_PARAMETERS
USE MODD_PARAM_n,       ONLY: CRAD
USE MODD_STATION_n
USE MODD_ALLSTATION_n,  ONLY: LDIAG_SURFRAD
USE MODD_SUB_STATION_n
USE MODD_TIME,          ONLY: tdtexp
USE MODD_TIME_n,        ONLY: tdtcur
!
USE MODE_ll
!
USE MODI_WATER_SUM
!
!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
!
INTEGER :: IIB        ! current processor domain sizes
INTEGER :: IJB        ! 
INTEGER :: IIE        !    
INTEGER :: IJE        !   
INTEGER :: IIU        ! 
INTEGER :: IJU        ! 
!
REAL, DIMENSION(SIZE(PXHAT))        :: ZXHATM ! mass point coordinates
REAL, DIMENSION(SIZE(PYHAT))        :: ZYHATM ! mass point coordinates
!
REAL, DIMENSION(SIZE(PSV,1),SIZE(PSV,2),SIZE(PSV,3),SIZE(PSV,4))  :: ZWORK   ! 
!
LOGICAL       :: GSTORE                       ! storage occurs at this time step
!
!
INTEGER :: IN       ! time index
INTEGER :: JSV      ! loop counter
!
REAL    :: ZU_STAT     ! horizontal wind speed at station location (along x)
REAL    :: ZV_STAT     ! horizontal wind speed at station location (along y)
REAL    :: ZGAM        ! rotation between meso-nh base and spherical lat-lon base.
!
INTEGER :: IINFO_ll   ! return code
INTEGER :: IRESP      ! return code
INTEGER :: I          ! loop for stations
INTEGER :: J          ! loop for levels

!
!----------------------------------------------------------------------------
!
!*      2.   PRELIMINARIES
!            -------------
!
!*      2.1  Indices
!            -------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
!
!*      2.2  Interpolations of model variables to mass points
!            ------------------------------------------------
!
IIU=SIZE(PXHAT)
IJU=SIZE(PYHAT)
!
ZXHATM(1:IIU-1)=0.5*PXHAT(1:IIU-1)+0.5*PXHAT(2:IIU  )
ZXHATM(  IIU  )=1.5*PXHAT(  IIU  )-0.5*PXHAT(  IIU-1)
!
ZYHATM(1:IJU-1)=0.5*PYHAT(1:IJU-1)+0.5*PYHAT(2:IJU  )
ZYHATM(  IJU  )=1.5*PYHAT(  IJU  )-0.5*PYHAT(  IJU-1)
!
!----------------------------------------------------------------------------
!
!*      3.4  instant of storage
!            ------------------
!
IF ( TSTATIONS_TIME%XTIME_CUR == XUNDEF ) TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTSTEP - PTSTEP
!
TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTIME_CUR + PTSTEP
!
IF ( TSTATIONS_TIME%XTIME_CUR >= TSTATIONS_TIME%XTSTEP - 1.E-10 ) THEN
     GSTORE = .TRUE.
     TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTIME_CUR - TSTATIONS_TIME%XTSTEP
     TSTATIONS_TIME%N_CUR = TSTATIONS_TIME%N_CUR + 1
     IN = TSTATIONS_TIME%N_CUR
ELSE
     GSTORE = .FALSE.
END IF
!
IF (GSTORE) THEN
#if 0
  tstations_time%tpdates(in)%date%year  = tdtexp%date%year
  tstations_time%tpdates(in)%date%month = tdtexp%date%month
  tstations_time%tpdates(in)%date%day   = tdtexp%date%day
  tstations_time%tpdates(in)%xtime      = tdtexp%xtime + ( in - 1 ) * tstation%step
#else
  tstations_time%tpdates(in) = tdtcur
#endif
END IF
!
!
!----------------------------------------------------------------------------
!
!*      4.   STATION POSITION
!            --------------
!
!*      4.0  initialization of processor test
!            --------------------------------
IF (GSTATFIRSTCALL) THEN
 GSTATFIRSTCALL=.FALSE.
!
 IF (.NOT.(ASSOCIATED(ZTHIS_PROCS))) ALLOCATE(ZTHIS_PROCS(NUMBSTAT))
!
 IF (.NOT.(ASSOCIATED(II)))     ALLOCATE(II(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(IJ)))     ALLOCATE(IJ(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(IV)))     ALLOCATE(IV(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(IU)))     ALLOCATE(IU(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(ZXCOEF))) ALLOCATE(ZXCOEF(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(ZUCOEF))) ALLOCATE(ZUCOEF(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(ZYCOEF))) ALLOCATE(ZYCOEF(NUMBSTAT))
 IF (.NOT.(ASSOCIATED(ZVCOEF))) ALLOCATE(ZVCOEF(NUMBSTAT))

 ZXCOEF(:)  =XUNDEF
 ZUCOEF(:)  =XUNDEF
 ZYCOEF(:)  =XUNDEF
 ZVCOEF(:)  =XUNDEF

 DO I=1,NUMBSTAT
!
  ZTHIS_PROCS(I)=0.
!
!*      4.1  X position
!            ----------
!
  IU(I)=COUNT( PXHAT (:)<=TSTATIONS(I)%XX )
  II(I)=COUNT( ZXHATM(:)<=TSTATIONS(I)%XX )
!
  IF (II(I)<=IIB-1   .AND. LWEST_ll() .AND. .NOT. L1D) TSTATIONS(I)%LERROR=.TRUE.
  IF (II(I)>=IIE     .AND. LEAST_ll() .AND. .NOT. L1D) TSTATIONS(I)%LERROR=.TRUE.
!
!
!*      4.2  Y position
!            ----------
!
  IV(I)=COUNT( PYHAT (:)<=TSTATIONS(I)%XY )
  IJ(I)=COUNT( ZYHATM(:)<=TSTATIONS(I)%XY )
!
  IF (IJ(I)<=IJB-1   .AND. LSOUTH_ll() .AND. .NOT. L1D) TSTATIONS(I)%LERROR=.TRUE.
  IF (IJ(I)>=IJE     .AND. LNORTH_ll() .AND. .NOT. L1D) TSTATIONS(I)%LERROR=.TRUE.
!
!
!*      4.3  Position of station according to processors
!            -------------------------------------------
!
  IF (IU(I)>=IIB .AND. IU(I)<=IIE .AND. IV(I)>=IJB .AND. IV(I)<=IJE) ZTHIS_PROCS(I)=1.
  IF (L1D) ZTHIS_PROCS(I)=1.
!
!
!*      4.4  Computations only on correct processor
!            --------------------------------------
    ZXCOEF(I) = 0.
    ZYCOEF(I) = 0.
    ZUCOEF(I) = 0.
    ZVCOEF(I) = 0.
    IF (ZTHIS_PROCS(I) >0. .AND. .NOT. L1D) THEN
!----------------------------------------------------------------------------
!
!*      6.1  Interpolation coefficient for X
!            -------------------------------
!
       ZXCOEF(I) = (TSTATIONS(I)%XX - ZXHATM(II(I))) / (ZXHATM(II(I)+1) - ZXHATM(II(I)))
!
!
!
!*      6.2  Interpolation coefficient for y
!            -------------------------------
!
       ZYCOEF(I) = (TSTATIONS(I)%XY - ZYHATM(IJ(I))) / (ZYHATM(IJ(I)+1) - ZYHATM(IJ(I)))
!
!-------------------------------------------------------------------
!
!*      7.   INITIALIZATIONS FOR INTERPOLATIONS OF U AND V
!            ---------------------------------------------
!
!*      7.1  Interpolation coefficient for X (for U)
!            -------------------------------
!
       ZUCOEF(I) = (TSTATIONS(I)%XX - PXHAT(IU(I))) / (PXHAT(IU(I)+1) - PXHAT(IU(I)))
!
!
!*      7.2  Interpolation coefficient for y (for V)
!            -------------------------------
!
       ZVCOEF(I) = (TSTATIONS(I)%XY - PYHAT(IV(I))) / (PYHAT(IV(I)+1) - PYHAT(IV(I)))
!
!

    END IF
 ENDDO
END IF
!----------------------------------------------------------------------------
!
!*      8.   DATA RECORDING
!            --------------
!
IF (GSTORE) THEN
  DO I=1,NUMBSTAT
     !
     IF ((ZTHIS_PROCS(I)==1.).AND.(.NOT. TSTATIONS(I)%LERROR)) THEN
       IF (TSTATIONS(I)%NK/= XUNDEF) THEN
         J = TSTATIONS(I)%NK
       ELSE  ! suppose TSTATIONS(I)%XZ /= XUNDEF
        J=1
        DO WHILE ((STATION_INTERP_2D(PZ(:,:,J))-STATION_INTERP_2D(PZ(:,:,2))) &
        < TSTATIONS(I)%XZ)
         J = J + 1
        END DO
        IF (((STATION_INTERP_2D(PZ(:,:,J))-STATION_INTERP_2D(PZ(:,:,2)))-TSTATIONS(I)%XZ)>&
       (TSTATIONS(I)%XZ-(STATION_INTERP_2D(PZ(:,:,J-1))-STATION_INTERP_2D(PZ(:,:,2))))) THEN
         J=J-1
        ENDIF
       END IF
      !
      IF (LCARTESIAN) THEN
        TSTATIONS(I)%XZON (IN)   =   STATION_INTERP_2D_U(PU(:,:,J))
        TSTATIONS(I)%XMER (IN)   =   STATION_INTERP_2D_V(PV(:,:,J))
      ELSE
        ZU_STAT               = STATION_INTERP_2D_U(PU(:,:,J))
        ZV_STAT               = STATION_INTERP_2D_V(PV(:,:,J))
        ZGAM                  = (XRPK * (TSTATIONS(I)%XLON - XLON0) - XBETA)*(XPI/180.)
        TSTATIONS(I)%XZON (IN)   =   ZU_STAT     * COS(ZGAM) + ZV_STAT     * SIN(ZGAM)
        TSTATIONS(I)%XMER (IN)   = - ZU_STAT     * SIN(ZGAM) + ZV_STAT     * COS(ZGAM)
      ENDIF
        TSTATIONS(I)%XW   (IN)   = STATION_INTERP_2D(PW(:,:,J))
        TSTATIONS(I)%XTH  (IN)   = STATION_INTERP_2D(PTH(:,:,J))
        TSTATIONS(I)%XP   (IN)   = STATION_INTERP_2D(PP(:,:,J))
      !
        DO JSV=1,SIZE(PR,4)
         TSTATIONS(I)%XR   (IN,JSV) = STATION_INTERP_2D(PR(:,:,J,JSV))
        END DO
      !
        DO JSV=1,SIZE(PSV,4)
         TSTATIONS(I)%XSV  (IN,JSV) = STATION_INTERP_2D(PSV(:,:,J,JSV))
        END DO
      !
        IF (SIZE(PTKE)>0) TSTATIONS(I)%XTKE  (IN) = STATION_INTERP_2D(PTKE(:,:,J))
        IF (SIZE(PTS) >0) TSTATIONS(I)%XTSRAD(IN) = STATION_INTERP_2D(PTS)
        TSTATIONS(I)%XZS      = STATION_INTERP_2D(PZ(:,:,1+JPVEXT))
      !
        IF (LDIAG_SURFRAD) THEN
          TSTATIONS(I)%XZON10M(IN) = STATION_INTERP_2D(XCURRENT_ZON10M)
          TSTATIONS(I)%XMER10M(IN) = STATION_INTERP_2D(XCURRENT_MER10M)
          TSTATIONS(I)%XT2M   (IN) = STATION_INTERP_2D(XCURRENT_T2M   )
          TSTATIONS(I)%XQ2M   (IN) = STATION_INTERP_2D(XCURRENT_Q2M   )
          TSTATIONS(I)%XHU2M  (IN) = STATION_INTERP_2D(XCURRENT_HU2M  )
          TSTATIONS(I)%XRN    (IN) = STATION_INTERP_2D(XCURRENT_RN    )
          TSTATIONS(I)%XH     (IN) = STATION_INTERP_2D(XCURRENT_H     )
          TSTATIONS(I)%XLE    (IN) = STATION_INTERP_2D(XCURRENT_LE    )
          TSTATIONS(I)%XLEI   (IN) = STATION_INTERP_2D(XCURRENT_LEI   )
          TSTATIONS(I)%XGFLUX (IN) = STATION_INTERP_2D(XCURRENT_GFLUX )
         IF (CRAD /= 'NONE') THEN
          TSTATIONS(I)%XSWD   (IN) = STATION_INTERP_2D(XCURRENT_SWD   )
          TSTATIONS(I)%XSWU   (IN) = STATION_INTERP_2D(XCURRENT_SWU   )
          TSTATIONS(I)%XLWD   (IN) = STATION_INTERP_2D(XCURRENT_LWD   )
          TSTATIONS(I)%XLWU   (IN) = STATION_INTERP_2D(XCURRENT_LWU   )
          TSTATIONS(I)%XSWDIR (IN) = STATION_INTERP_2D(XCURRENT_SWDIR )
          TSTATIONS(I)%XSWDIFF(IN) = STATION_INTERP_2D(XCURRENT_SWDIFF)
          TSTATIONS(I)%XDSTAOD(IN) = STATION_INTERP_2D(XCURRENT_DSTAOD)
         ENDIF
          TSTATIONS(I)%XSFCO2 (IN) = STATION_INTERP_2D(XCURRENT_SFCO2 )
        ENDIF

      !
    END IF
!
!----------------------------------------------------------------------------
!
!*     11.   EXCHANGE OF INFORMATION BETWEEN PROCESSORS
!            ------------------------------------------
!
!*     11.2  data stored
!            -----------
!
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XX   )
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XY   )
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XZ   )
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLON )
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLAT )
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XZON (IN))
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XMER (IN))
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XW   (IN))
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XP   (IN))
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XTH  (IN))
  DO JSV=1,SIZE(PR,4)
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XR   (IN,JSV))
  END DO
  DO JSV=1,SIZE(PSV,4)
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSV  (IN,JSV))
  END DO
  IF (SIZE(PTKE)>0) CALL DISTRIBUTE_STATION(TSTATIONS(I)%XTKE  (IN))
  IF (SIZE(PTS) >0) CALL DISTRIBUTE_STATION(TSTATIONS(I)%XTSRAD(IN))
  CALL DISTRIBUTE_STATION(TSTATIONS(I)%XZS )
  IF (LDIAG_SURFRAD) THEN
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XT2M    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XQ2M    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XHU2M   (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XZON10M (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XMER10M (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XRN     (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XH      (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLE     (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLEI    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XGFLUX  (IN))
   IF (CRAD /= 'NONE') THEN
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSWD    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSWU    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLWD    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XLWU    (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSWDIR  (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSWDIFF (IN))
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XDSTAOD (IN))
   END IF
    CALL DISTRIBUTE_STATION(TSTATIONS(I)%XSFCO2  (IN))
  ENDIF

 ENDDO
  !
END IF
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
FUNCTION STATION_INTERP_2D(PA) RESULT(PB)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PA
REAL                             :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
     JI=1
     JJ=1 
ELSEIF (L1D) THEN
     JI=2
     JJ=2
ELSE
     JI=II(I)
     JJ=IJ(I)
END IF
!
!
IF ((JI .GE. 1).AND. (JI .LE. SIZE(PA,1)) .AND. &
    (JJ .GE. 1).AND. (JJ .LE. SIZE(PA,2))) &
PB = (1.-ZYCOEF(I)) * (1.-ZXCOEF(I)) *  PA(JI,JJ)    + &
     (1.-ZYCOEF(I)) *    (ZXCOEF(I)) *  PA(JI+1,JJ)  + &
     (   ZYCOEF(I)) * (1.-ZXCOEF(I)) *  PA(JI,JJ+1)  + &
     (   ZYCOEF(I)) *    (ZXCOEF(I)) *  PA(JI+1,JJ+1)
!
END FUNCTION STATION_INTERP_2D
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! MODIFS
FUNCTION STATION_INTERP_2D_U(PA) RESULT(PB)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PA
REAL                             :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
     JI=1
     JJ=1
ELSEIF (L1D) THEN
     JI=2
     JJ=2
ELSE
     JI=II(I)
     JJ=IJ(I)
END IF
!
IF ((JI .GE. 1).AND. (JI .LE. SIZE(PA,1)) .AND. &
    (JJ .GE. 1).AND. (JJ .LE. SIZE(PA,2))) &
PB = (1.- ZYCOEF(I)) * (1.-ZUCOEF(I)) * PA(JI  ,JJ  ) &
   + (1.- ZYCOEF(I)) * (   ZUCOEF(I)) * PA(JI+1,JJ  ) &
   + (    ZYCOEF(I)) * (1.-ZUCOEF(I)) * PA(JI  ,JJ+1) &
   + (    ZYCOEF(I)) * (   ZUCOEF(I)) * PA(JI+1,JJ+1)
!
END FUNCTION STATION_INTERP_2D_U
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! MODIFS
FUNCTION STATION_INTERP_2D_V(PA) RESULT(PB)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PA
REAL                             :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSEIF (L1D) THEN
     JI=2
     JJ=2  
ELSE
  JI=II(I)
  JJ=IJ(I)
END IF
!
IF ((JI .GT. 0).AND. (JI .LT. SIZE(PA,1)) .AND. &
    (JJ .GT. 0).AND. (JJ .LT. SIZE(PA,2))) &
PB = (1.- ZVCOEF(I)) * (1.-ZXCOEF(I)) * PA(JI  ,JJ  ) &
   + (1.- ZVCOEF(I)) * (   ZXCOEF(I)) * PA(JI+1,JJ  ) &
   + (    ZVCOEF(I)) * (1.-ZXCOEF(I)) * PA(JI  ,JJ+1) &
   + (    ZVCOEF(I)) * (   ZXCOEF(I)) * PA(JI+1,JJ+1)
!
END FUNCTION STATION_INTERP_2D_V
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE DISTRIBUTE_STATION(PAS)
!
REAL, INTENT(INOUT) :: PAS
!
PAS = PAS * ZTHIS_PROCS(I)
CALL REDUCESUM_ll(PAS,IINFO_ll)
!
END SUBROUTINE DISTRIBUTE_STATION
!----------------------------------------------------------------------------
!
END SUBROUTINE STATION_n
