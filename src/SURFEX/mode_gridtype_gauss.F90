!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!##########################
MODULE MODE_GRIDTYPE_GAUSS
!##########################
!
!############################################################################
!############################################################################
!############################################################################
!
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
      USE PARKIND1  ,ONLY : JPRB
!
      USE MODI_ABOR1_SFX
!
      IMPLICIT NONE
CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRIDTYPE_GAUSS(PGRID_PAR,KNLATI, PLAPO,PLOPO,PCODIL,KNLOPA, &
                                      KL,PLAT,PLON,PLAT_XY,PLON_XY,PMESH_SIZE      )  
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_GAUSS* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      (B. Decharme) 2008 multiples changes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN) :: KNLATI ! number of pseudo-latitudes
REAL,                       INTENT(IN) :: PLAPO  ! latitude of the rotated pole (deg)
REAL,                       INTENT(IN) :: PLOPO  ! logitude of the rotated pole (rad)
REAL,                       INTENT(IN) :: PCODIL ! stretching factor
INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA ! number of pseudo-longitudes
!                                                ! on each pseudo-latitude circle
!                                                ! on pseudo-northern hemisphere
!                                                ! (starting from the rotated pole)
INTEGER,                    INTENT(IN) :: KL     ! number of points used
REAL,   DIMENSION(:),       INTENT(IN) :: PLAT   ! latitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLON   ! longitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLAT_XY! pseudo-latitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLON_XY! pseudo-longitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PMESH_SIZE ! Mesh size
!
REAL, DIMENSION(:), POINTER :: PGRID_PAR         ! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PUT_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
ALLOCATE(PGRID_PAR(5+KNLATI+5*KL))
PGRID_PAR(1) = KNLATI
PGRID_PAR(2) = PLAPO
PGRID_PAR(3) = PLOPO
PGRID_PAR(4) = PCODIL
PGRID_PAR(5:4+KNLATI)= KNLOPA(:)
PGRID_PAR(5+KNLATI) = KL
PGRID_PAR(6+KNLATI:5+KNLATI+KL) = PLAT(:)
PGRID_PAR(6+KNLATI+KL:5+KNLATI+2*KL) = PLON(:)
PGRID_PAR(6+KNLATI+2*KL:5+KNLATI+3*KL) = PLAT_XY(:)
PGRID_PAR(6+KNLATI+3*KL:5+KNLATI+4*KL) = PLON_XY(:)
PGRID_PAR(6+KNLATI+4*KL:5+KNLATI+5*KL) = PMESH_SIZE(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PUT_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_GAUSS(PGRID_PAR,KNLATI,                  &
                                      PLAPO,PLOPO,PCODIL,KNLOPA,KL,      &
                                      PLAT,PLON,PLAT_XY,PLON_XY,PMESH_SIZE)  
!     ####################################################################
!
!!****  *GET_GRIDTYPE_GAUSS* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN) :: PGRID_PAR! parameters defining this grid
INTEGER, INTENT(OUT), OPTIONAL :: KNLATI   ! number of pseudo-latitudes
REAL,    INTENT(OUT), OPTIONAL :: PLAPO    ! latitude of the rotated pole (deg)
REAL,    INTENT(OUT), OPTIONAL :: PLOPO    ! logitude of the rotated pole (deg)
REAL,    INTENT(OUT), OPTIONAL :: PCODIL   ! stretching factor
INTEGER, DIMENSION(:), INTENT(OUT), OPTIONAL :: KNLOPA ! number of pseudo-longitudes
!                                                     ! on each pseudo-latitude circle
!                                                     ! on pseudo-northern hemisphere
!                                                     ! (starting from the rotated pole)
INTEGER, INTENT(OUT), OPTIONAL :: KL   ! number of points
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLAT    ! latitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLON    ! longitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLAT_XY ! pseudo-latitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLON_XY ! pseudo-longitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PMESH_SIZE ! Mesh size
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL
INTEGER :: INLATI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GET_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
IF (PRESENT(KNLATI))  KNLATI = PGRID_PAR(1)
IF (PRESENT(PLAPO))   PLAPO  = PGRID_PAR(2)
IF (PRESENT(PLOPO))   PLOPO  = PGRID_PAR(3)
IF (PRESENT(PCODIL))  PCODIL = PGRID_PAR(4)
!
INLATI = PGRID_PAR(1)
!
IF (PRESENT(KNLOPA)) THEN
  KNLOPA(:) = PGRID_PAR(5:4+INLATI)
END IF
!
IL    = PGRID_PAR(5+INLATI)
!
IF (PRESENT(KL)) THEN
  KL = IL
END IF
!
IF (PRESENT(PLAT)) THEN
  IF (SIZE(PLAT)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLAT') 
  END IF
 PLAT(:) = PGRID_PAR(6+INLATI:5+INLATI+IL)
END IF
!
IF (PRESENT(PLON)) THEN
  IF (SIZE(PLON)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLON') 
  END IF
 PLON(:) = PGRID_PAR(6+INLATI+IL:5+INLATI+2*IL)
END IF
!
IF (PRESENT(PLAT_XY)) THEN
  IF (SIZE(PLAT_XY)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLAT_XY') 
  END IF
 PLAT_XY(:) = PGRID_PAR(6+INLATI+2*IL:5+INLATI+3*IL)
END IF
!
IF (PRESENT(PLON_XY)) THEN
  IF (SIZE(PLON_XY)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLON_XY') 
  END IF
 PLON_XY(:) = PGRID_PAR(6+INLATI+3*IL:5+INLATI+4*IL)
END IF
!
IF (PRESENT(PMESH_SIZE)) THEN
  IF (SIZE(PMESH_SIZE)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PMESH_SIZE') 
  END IF
 PMESH_SIZE(:) = PGRID_PAR(6+INLATI+4*IL:5+INLATI+5*IL)
END IF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GET_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE LATLON_GAUSS(PLON_XY,PLAT_XY,KL,PLOPO,PLAPO,PCODIL,PLON,PLAT)
!     ####################################################################
!
!!****  *LATLON_GAUSS* - computes the coordinates on the real sphere
!!
!!    AUTHOR
!!    ------
!!	F. Taillefer  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                   INTENT(IN) :: KL      ! number of grid points
REAL,                      INTENT(IN) :: PLAPO   ! pole latitude
REAL,                      INTENT(IN) :: PLOPO   ! pole longitude
REAL,                      INTENT(IN) :: PCODIL  ! stretching factor
REAL,  DIMENSION(KL),      INTENT(IN) :: PLAT_XY ! pseudo-latitudes of points  (deg)
REAL,  DIMENSION(KL),      INTENT(IN) :: PLON_XY ! pseudo-longitudes of points (deg)
REAL,  DIMENSION(KL),      INTENT(OUT):: PLAT    ! latitudes of points  (deg)
REAL,  DIMENSION(KL),      INTENT(OUT):: PLON    ! longitudes of points (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
! called 1 variables : on the transformed sphere (rotated ans stretched)
! called 2 variables : on the rotated but no stretched sphere
! called 3 variables : on the real sphere

INTEGER :: JP
REAL :: ZCLO3,ZCONR,ZINTERM,ZLAT1,ZLAT2,ZLAT3,ZLON1,ZLON2,ZLON3
REAL :: ZLATP,ZLONP,ZSLA3,ZSLO3,ZR,ZPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATLON_GAUSS',0,ZHOOK_HANDLE)
ZPI=4.*ATAN(1.)
ZCONR=ZPI/180.

ZLONP=ZCONR*PLOPO
ZLATP=ZCONR*PLAPO

DO JP = 1,KL
  ZLON1=ZCONR*PLON_XY(JP)
  ZLAT1=ZCONR*PLAT_XY(JP)
! move from the stretched to the no stretched sphere
  ZINTERM=1./PCODIL*COS(ZLAT1)/(1.+SIN(ZLAT1))
  ZLAT2=2.*ATAN((1.-ZINTERM)/(1.+ZINTERM))
  ZLON2=ZLON1
! move from the rotated sphere to the real one
!
! calculation of the latitude
  ZSLA3=-COS(ZLAT2)*COS(ZLON2)*COS(ZLATP)+SIN(ZLAT2)*SIN(ZLATP)
  IF (ZSLA3>1. .OR. ZSLA3<-1.) THEN
    WRITE(0,*) 'be carefull --> sinus >1'
    ZSLA3=MIN(1.,MAX(-1.,ZSLA3))
  ENDIF
  ZLAT3=ASIN(ZSLA3)
!
! calculation of the sine and cosine of the longitude
  ZSLO3=(COS(ZLAT2)*COS(ZLON2)*SIN(ZLATP)*SIN(ZLONP)&
      +COS(ZLAT2)*SIN(ZLON2)*COS(ZLONP)&
      +SIN(ZLAT2)*COS(ZLATP)*SIN(ZLONP)) / COS(ZLAT3)  
  ZCLO3=(COS(ZLAT2)*COS(ZLON2)*SIN(ZLATP)*COS(ZLONP)&
      -COS(ZLAT2)*SIN(ZLON2)*SIN(ZLONP)&
      +SIN(ZLAT2)*COS(ZLATP)*COS(ZLONP)) / COS(ZLAT3)  
!
! Conversion from rectangular to polar to get the longitude
  ZR=SQRT(ZCLO3*ZCLO3+ZSLO3*ZSLO3)
  IF (ZCLO3==0.) THEN
    IF (ZSLO3==0.) THEN
      ZLON3=0.
    ELSEIF (ZSLO3>0.) THEN
      ZLON3=ZPI/2.
    ELSE
      ZLON3=-ZPI/2.
    ENDIF
  ELSE
    ZINTERM=ATAN(ZSLO3/ZCLO3)
    IF (ZCLO3>=0.) THEN
      ZLON3=ZINTERM
    ELSEIF (ZSLO3>=0.) THEN
      ZLON3=ZINTERM+ZPI
    ELSE
      ZLON3=ZINTERM-ZPI
    ENDIF
  ENDIF
!
! Conversion from radians to degrees
  PLON(JP)=ZLON3/ZCONR
  IF (PLON(JP)<0.) PLON(JP)=PLON(JP)+360.
  PLAT(JP)=ZLAT3/ZCONR

END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATLON_GAUSS',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------
END SUBROUTINE LATLON_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE COMP_GRIDTYPE_GAUSS(KNLATI,KNLOPA,KL,KTYP,PLAT_XY,PLON_XY)
!     ####################################################################
!
!!****  *COMP_GRIDTYPE_GAUSS* - computes the gaussian grid
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!!        F.Taillefer  10/2007 : adapt gaussian latitudes calculation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE EGGANGLES , ONLY : P_ASIN

IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN) :: KNLATI  ! number of pseudo-latitudes
INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA  ! number of pseudo-longitudes
!                                                 ! on each pseudo-latitude circle
!                                                 ! on pseudo-northern hemisphere
!                                                 ! (starting from the rotated pole)
INTEGER,                    INTENT(IN) :: KL      ! number of points used
INTEGER,                    INTENT(IN) :: KTYP    ! type of transform
REAL,   DIMENSION(KL),   INTENT(INOUT) :: PLAT_XY ! pseudo-latitudes of points  (deg)
REAL,   DIMENSION(KL),   INTENT(INOUT) :: PLON_XY ! pseudo-longitudes of points (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JL ! point loop counter
INTEGER :: JX ! longitude loop counter
INTEGER :: JY ! latitude loop counter
REAL                      :: ZPI, ZRD, ZI
REAL, DIMENSION(KNLATI)   :: ZNLOPA, ZSINLA, ZWG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:COMP_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
ZPI = 4.*ATAN(1.)
ZRD = 180. / ZPI
ZI=0.
IF (KTYP==1) ZI=180.
!
ZNLOPA=FLOAT(KNLOPA)
!
! gaussian latitudes calculation
 CALL LATITUDES_GAUSS(KNLATI,ZSINLA,ZWG)
!
JL=KNLATI/2
DO JY=1,JL
  ZWG(JY)= P_ASIN(ZSINLA(JY))
  ZWG(KNLATI+1-JY)= -P_ASIN(ZSINLA(JY))
END DO
!
JL=0
!* loop on latitudes (from north pole)
DO JY=1,KNLATI
!* loop on longitudes (from 0°, to the east)
  DO JX=1,KNLOPA(JY)
    JL=JL+1
    PLAT_XY(JL) = ZWG(JY)*ZRD
    PLON_XY(JL) = 360. * FLOAT(JX-1) / ZNLOPA(JY) + ZI
  END DO
END DO
!
IF (JL/=KL) THEN
  WRITE(0,*) ' PB in the total number of points of the gaussian grid '
  WRITE(0,*) '   check your namelist and rerun !'
  CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: PB IN THE TOTAL NUMBER OF POINTS')
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:COMP_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE COMP_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GAUSS_GRID_LIMITS(KNLATI,KNLOPA,PXINF,PXSUP,PYINF,PYSUP)
!     ####################################################################
!
!!****  *GAUSS_GRID_LIMITS* - computes the gaussian grid "boxes"
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!!         F. Taillefer  08/2007  pb with the gaussian latitudes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE EGGANGLES , ONLY : P_ASIN

IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                   INTENT(IN) :: KNLATI! number of pseudo-latitudes
INTEGER, DIMENSION(KNLATI),INTENT(IN) :: KNLOPA! number of pseudo-longitudes
!                                              ! on each pseudo-latitude circle
!                                              ! on pseudo-northern hemisphere
!                                              ! (starting from the rotated pole)
REAL,   DIMENSION(:),      INTENT(OUT):: PXINF ! minimum pseudo longitude of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PXSUP ! maximum pseudo longitude of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PYINF ! minimum pseudo latitude  of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PYSUP ! maximum pseudo latitude  of the grid point (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JL ! point loop counter
INTEGER :: JX ! longitude loop counter
INTEGER :: JY ! latitude loop counter
REAL                    :: ZNLATI, ZPI, ZRD
REAL, DIMENSION(KNLATI) :: ZNLOPA, ZSINLA, ZWG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GAUSS_GRID_LIMITS',0,ZHOOK_HANDLE)
ZPI = 4.*ATAN(1.)
ZRD = 180. / ZPI
!
ZNLATI=FLOAT(KNLATI)
ZNLOPA=FLOAT(KNLOPA)
!
! gaussian latitudes calculation
 CALL LATITUDES_GAUSS(KNLATI,ZSINLA,ZWG)
!
JL=KNLATI/2
DO JY=1,JL
  ZWG(JY)= P_ASIN(ZSINLA(JY))*ZRD
  ZWG(KNLATI+1-JY)= -P_ASIN(ZSINLA(JY))*ZRD
END DO
!
JL=0
!
!* loop on latitudes (from north pole)
DO JY=1,KNLATI
!* loop on longitudes (from 0°, to the east)
  DO JX=1,KNLOPA(JY)
    JL=JL+1
    IF (JY==1) THEN
      PYSUP(JL) = 90. 
    ELSE
      PYSUP(JL)= ZWG(JY)+(ZWG(JY-1)-ZWG(JY))/2.
    ENDIF
    IF (JY==KNLATI) THEN
      PYINF(JL) = -90.
    ELSE
      PYINF(JL) = ZWG(JY)-(ZWG(JY)-ZWG(JY+1))/2.
    ENDIF
    PXSUP  (JL) = 360. * (FLOAT(JX)-0.5) / ZNLOPA(JY)
    PXINF  (JL) = 360. * (FLOAT(JX)-1.5) / ZNLOPA(JY)
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GAUSS_GRID_LIMITS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GAUSS_GRID_LIMITS
!############################################################################
  !############################################################################
  !        ####################################################################
  SUBROUTINE XY_GAUSS(PLAPO,PLOPO,PCODIL, &
                              PLAT,PLON,PLAT_XY,PLON_XY)  
    !      ####################################################################
    !
    !!****  *LATLON_GAUSS * - Routine to compute coordinates on a transform sphere
    !!                        from geographical coordinates            
    !!
    !!     PURPOSE
    !!     -------
    !        This routine computes the latitude and longitude a real coordinates 
    !        given array to an arpege model coordinates (rotated stretched)
    !
    !
    !
    !!**   METHOD
    !!     ------
    !!       use of rotations routines (eggmrt) and streching conformal formulae 
    !!       to pass from real sphere (PLAT,PLON) transform sphere (PLAT_XY, PLON_XY)
    !!
    !!     EXTERNAL
    !!     --------
    !!       None
    !!
    !!     REFERENCE
    !!     ---------
    !!         Arpege DOC "Sphere Transphormee" Chapitre 7 version du 4/6/1991
    !!         J-D Gril for GEO_GAUSS 2005
    !!         J-D Gril Doc for EGGANGLES routines (new EGGX) 2005        
    !!       
    !!     AUTHOR
    !!     ------
    !!      J-D Gril
    !!
    !!     MODIFICATION
    !!     ------------
    !!       Original  10/2005
    !
    !-------------------------------------------------------------------------------
    !
    !*     0.     DECLARATIONS
    !             ------------
    !
    USE MODE_GEO_GAUSS,ONLY : GAUSS_TR, GAUSS_RT
    USE EGGANGLES,ONLY : LOLA, ANGLE_DOMAIN
    !
    IMPLICIT NONE
    !
    !*     0.1    Declarations of arguments and results
    !
    REAL,                 INTENT(IN) :: PLOPO
    REAL,                 INTENT(IN) :: PLAPO
    REAL,                 INTENT(IN) :: PCODIL
    REAL,                 INTENT(IN) :: PLAT,PLON

    REAL,                 INTENT(OUT):: PLAT_XY,PLON_XY    
    !
    !*     0.2    Declarations of local variables
    ! 
    TYPE(LOLA)                          :: TZPOLE
    TYPE(LOLA)                          :: TZPTCI,TZPTCO
    !
    REAL :: ZPI
    REAL :: ZDR
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    !--------------------------------------------------------------------------------
    !
    !*     1.     Preliminary calculations
    !             ------------------------
    !
    IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:XY_GAUSS',0,ZHOOK_HANDLE)
    ZPI = 4.*ATAN(1.)
    ZDR = ZPI / 180.
    !
    TZPTCI%LON = ANGLE_DOMAIN(PLON,DOM='0+',UNIT='D') * ZDR
    TZPTCI%LAT = PLAT * ZDR
    TZPOLE%LON = ANGLE_DOMAIN(PLOPO,DOM='0+',UNIT='D') * ZDR
    TZPOLE%LAT = PLAPO * ZDR
    !
    !-------------------------------------------------------------------------------
    !
    !*     2.    Calcul
    !            ------
    !
    TZPTCO = ANGLE_DOMAIN(GAUSS_TR(TZPTCI,TZPOLE,PCODIL),DOM='-+',UNIT='R')
    !     
    !---------------------------------------------------------------------------------
    !
    !*     3.      EXIT
    !              ----
    !
    PLAT_XY = TZPTCO%LAT / ZDR
    PLON_XY = TZPTCO%LON / ZDR
    !
    IF (ABS(PLON_XY-360.)<1.E-4) PLON_XY = 0.
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:XY_GAUSS',1,ZHOOK_HANDLE)
    !---------------------------------------------------------------------------------
  END SUBROUTINE XY_GAUSS
  !---------------------------------------------------------------------------------
  !############################################################################
  !        ####################################################################
  SUBROUTINE MAP_FACTOR_GAUSS(PLAPO,PLOPO,PCODIL, &
                                   PLAT,PLON,PMAP)  
    !      ####################################################################
    !
    !!****  *MAP_FACTOR_GAUSS * - Routine to compute map factor for points
    !!      in real sphere coordinates
    !!
    !!
    !!     PURPOSE
    !!     -------
    !
    !!     REFERENCE
    !!     ---------
    !!         Arpege DOC "Sphere Transphormee" Chapitre 7 version du 4/6/1991
    !!         J-D Gril for GEO_GAUSS 2005
    !!         J-D Gril Doc for EGGANGLES routines (new EGGX) 2005        
    !!       
    !!     AUTHOR
    !!     ------
    !!      J-D Gril
    !!
    !!     MODIFICATION
    !!     ------------
    !!       Original  10/2005
    !!
    !-------------------------------------------------------------------------------
    !
    !*     0.     DECLARATIONS
    !             ------------
    !
    USE EGGANGLES,ONLY : LOLA, ANGLE_DOMAIN
    USE MODE_GEO_GAUSS,ONLY : MAP_FAC
    !
    IMPLICIT NONE
    !
    !*     0.1    Declarations of arguments and results
    !
    REAL,                 INTENT(IN) :: PLAPO  ! latitude of  pole
    REAL,                 INTENT(IN) :: PLOPO  ! longitude of pole
    REAL,                 INTENT(IN) :: PCODIL ! coefficient of dilatation
    REAL, DIMENSION(:),   INTENT(IN) :: PLAT   ! given geographic latitudes
    REAL, DIMENSION(:),   INTENT(IN) :: PLON   ! given geographic longitudes    
    ! 
    REAL, DIMENSION(SIZE(PLAT)),   INTENT(OUT):: PMAP   ! map factor
    !
    !*     0.2    Declarations of local variables
    ! 
    ! 
    TYPE(LOLA)                          :: TZPOLE
    TYPE(LOLA), DIMENSION(SIZE(PLAT))   :: TZPTCI
    !
    REAL :: ZPI
    REAL :: ZDR
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    !-------------------------------------------------------------------------------
    !
    !*     1.     PRELIMINARY CALCULATION
    !             -----------------------
    !
    IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MAP_FACTOR_GAUSS',0,ZHOOK_HANDLE)
    ZPI = 4.*ATAN(1.)
    ZDR = ZPI / 180.
    !
    TZPTCI(:)%LON = ANGLE_DOMAIN(PLON(:),DOM='0+',UNIT='D') * ZDR
    TZPTCI(:)%LAT = PLAT(:) * ZDR
    TZPOLE%LON    = ANGLE_DOMAIN(PLOPO,DOM='0+',UNIT='D') * ZDR
    TZPOLE%LAT    = PLAPO * ZDR
    !    
    !-------------------------------------------------------------------------------
    !
    !*     2.    Calcul
    !            ------
    !
    PMAP(:) = MAP_FAC(TZPOLE,PCODIL,TZPTCI)
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MAP_FACTOR_GAUSS',1,ZHOOK_HANDLE)
    !
    !-------------------------------------------------------------------------------
  END SUBROUTINE MAP_FACTOR_GAUSS
  !-------------------------------------------------------------------------------
  !############################################################################
  !##################################
  SUBROUTINE LATITUDES_GAUSS(N,XMU,W)
  !##################################
  !
  !!****  *LATITUDES_GAUSS* - routine to compute the gaussian latitudes
  !!
  !!    PURPOSE
  !!    -------
  !!       Compute the sinus of the latitudes of the gaussian grid associated
  !!     to a given N number of latitudes
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    AUTHOR
  !!    ------
  !!              *Meteo France*
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    08/2007
  !-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,                       INTENT(IN)  :: N    ! number of latitudes (without pole)
  REAL, DIMENSION (N/2), INTENT(OUT) :: XMU  ! Gauss abscissas
  REAL, DIMENSION (N/2), INTENT(OUT) :: W    ! gaussian weights

  INTEGER                  :: JP1
  PARAMETER (JP1=20001)
  INTEGER                  :: JP2
  PARAMETER (JP2=1000)
  INTEGER                  :: NP, NN, J, ITER
  REAL(KIND=8), DIMENSION (JP1) :: X
  REAL(KIND=8), DIMENSION (JP2) :: XS
  REAL(KIND=8), DIMENSION (JP2) :: XI
  REAL(KIND=8), DIMENSION (JP2) :: AS
  REAL(KIND=8), DIMENSION (JP2) :: AI
  REAL(KIND=8)                  :: G, H, XA, XB, DX, DG
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  !-------------------------------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS',0,ZHOOK_HANDLE)
  NP=20001
  DX=1.D0/(NP-1.D0)
  X(1)=0.
  DO J=2,NP
    X(J)=X(J-1)+DX
  ENDDO
  CALL PDN(X(1),H,DG,N)
  NN=0
  DO J=2,NP
    CALL PDN(X(J),G,DG,N)
    IF(G*H > 0.D0) GO TO 12
    NN=NN+1
    XS(NN)=X(J)
    XI(NN)=X(J-1)
    AS(NN)=G
    AI(NN)=H
 12 CONTINUE
    H=G
  ENDDO

  IF(NN == N/2) GO TO 13
  CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: LATITUDES_GAUSS')
 13 CONTINUE
  DO J=1,NN
   19 CONTINUE
    IF(DABS(AS(J)*1.D0) > DABS(AI(J)*1.D0)) GO TO 21
    XA=XS(J)
    GOTO 22
 21 CONTINUE
    XA=XI(J)
 22 CONTINUE
    ITER=0
    DX=0.
 23 CONTINUE
    ITER=ITER+1
    XA=XA-DX
    IF((XA-XI(J))*(XA-XS(J)) > 0.) GO TO 25
    CALL PDN(XA,G,DG,N)
    DX=G/DG
    IF(DABS(DX*1.D0) > 1.D-15) GO TO 24
    XB=XA-DX
    CALL PDN(XB,G,DG,N)
    XMU(NN+1-J)=XB
    W(NN+1-J)=2.D0/((1.D0-XB*XB)*DG*DG)
    CYCLE
 25 CONTINUE
    XB=(XI(J)+XS(J))*0.5D0
    CALL PDN(XB,G,DG,N)
    IF(G*AS(J) < 0.) GO TO 26
    XS(J)=XB
    AS(J)=G
    GO TO 19
 26 CONTINUE
    XI(J)=XB
    AI(J)=G
    GO TO 19
 24 CONTINUE
    IF(ITER < 30) GO TO 23
  ENDDO
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS',1,ZHOOK_HANDLE)

  !-------------------------------------------------------------------------------
  END SUBROUTINE LATITUDES_GAUSS

  !############################################################################
  !#######################
  SUBROUTINE PDN(X,P,DP,N)
  !#######################
  !
  !-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER             :: JP1
  PARAMETER (JP1=1000)

  INTEGER,INTENT(IN)  :: N
  REAL(KIND=8),INTENT(IN)  :: X
  REAL(KIND=8),INTENT(OUT) :: P, DP

  REAL(KIND=8)                 :: AI
  REAL(KIND=8), DIMENSION(JP1) :: PN

  INTEGER :: J
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  !-------------------------------------------------------------------------------
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PDN',0,ZHOOK_HANDLE)
  PN(1)=X
  PN(2)=(3.D0*X*X-1.D0)*0.5D0

  DO J=3,N
    AI=J
    PN(J)=((2.D0*AI-1.D0)*X*PN(J-1)-(AI-1.D0)*PN(J-2))/AI
  ENDDO

  P=PN(N)
  DP=AI*(X*P-PN(N-1))/(X*X-1.D0)
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PDN',1,ZHOOK_HANDLE)

  !-------------------------------------------------------------------------------
  END SUBROUTINE PDN

  !############################################################################
  !##################################
  SUBROUTINE MESH_SIZE_GAUSS(KL,KNLATI,KNLOPA,PLAPO,PLOPO,PCODIL,&
                               PLAT_XY,PLAT,PLON,PMESH_SIZE)  
  !##################################
  !
  !!****  *MESH_SIZE_GAUSS* - routine to compute the global mesh size
  !!
  !!    PURPOSE
  !!    -------
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    AUTHOR
  !!    ------
  !!              *Meteo France*
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    06/2008
  !-------------------------------------------------------------------------------
  !
  USE MODD_CSTS, ONLY : XRADIUS, XPI
  !
  IMPLICIT NONE
  !
  INTEGER,                    INTENT(IN) :: KL        ! number of point
  INTEGER,                    INTENT(IN) :: KNLATI    ! number of pseudo-latitudes
  INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA ! number of pseudo-longitudes on each
  REAL,                       INTENT(IN) :: PLAPO     ! latitude  of the rotated pole (deg)
  REAL,                       INTENT(IN) :: PLOPO     ! longitude of the rotated pole (deg)
  REAL,                       INTENT(IN) :: PCODIL    ! stretching factor (must be greater than or equal to 1)
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLAT_XY  ! pseudo latitudes
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLAT       ! latitude  (degrees)
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLON       ! longitude (degrees)
  REAL,    DIMENSION(KL),     INTENT(OUT):: PMESH_SIZE ! global mesh size
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: IXX      ! number of points in the latitude circle of each grid point
  INTEGER, DIMENSION(:), ALLOCATABLE :: IYY      ! latitude circle of each grid point
  REAL,    DIMENSION(:), ALLOCATABLE :: ZDLAT  ! 
  REAL,    DIMENSION(:), ALLOCATABLE :: PLAT_XY_C  !  pseudo-latitude for each circle
  REAL,    DIMENSION(:), ALLOCATABLE :: ZXX   ! X-length of each mesh in the gaussian grid
  REAL,    DIMENSION(:), ALLOCATABLE :: ZYY   ! Y-length of each mesh in the gaussian grid
  REAL,    DIMENSION(:), ALLOCATABLE :: ZMAP  ! map factor
  INTEGER :: IL
  INTEGER :: JL       ! loop counter on number of points
  INTEGER :: JLTOT    ! loop counter on total number of points
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MESH_SIZE_GAUSS',0,ZHOOK_HANDLE)
  ALLOCATE(IXX(KL))
  ALLOCATE(IYY(KL))
  ALLOCATE(ZXX(KL))
  ALLOCATE(PLAT_XY_C(KNLATI))
  ALLOCATE(ZDLAT(KNLATI))
  ALLOCATE(ZYY(KL))
  ALLOCATE(ZMAP(KL))
  !
  !        1.0   Length according to the X axis (longitudes)
  !              ------------------------------
  !
  IL=1
  DO JL = 1,KNLATI
    DO JLTOT = 1,KNLOPA(JL)
       IXX(IL)=KNLOPA(JL)
       IYY(IL)=JL
       IL=IL+1
    ENDDO
  ENDDO
  !
  !
  DO JL = 1,KL
     ZXX(JL) = 2.*XPI*XRADIUS*COS(PLAT_XY(JL)*XPI/180.)/FLOAT(IXX(JL))
  ENDDO
  !
  !        2.0   Length according to the Y axis (latitudes)
  !              ------------------------------
  !
  IL=1
  DO JL = 1,KNLATI
     PLAT_XY_C(JL)=PLAT_XY(IL)
     IL=IL+KNLOPA(JL)
  ENDDO
  !
  ZDLAT(1)=90.-PLAT_XY_C(1)+((PLAT_XY_C(1)-PLAT_XY_C(2))/2.)
  DO JL = 2,KNLATI-1
     ZDLAT(JL)=((PLAT_XY_C(JL-1)-PLAT_XY_C(JL))/2.)+((PLAT_XY_C(JL)-PLAT_XY_C(JL+1))/2.)
  ENDDO
  ZDLAT(KNLATI)=((PLAT_XY_C(KNLATI-1)-PLAT_XY_C(KNLATI))/2.)+PLAT_XY_C(KNLATI)+90.
  ZDLAT(:)=ZDLAT(:)*XPI*XRADIUS/180.
  !
  DO JL = 1,KL
     ZYY(JL)=ZDLAT(IYY(JL))
  ENDDO
  !
  !        3.0   grid mesh
  !              ---------
  !
  CALL MAP_FACTOR_GAUSS(PLAPO,PLOPO,PCODIL,PLAT,PLON,ZMAP)
  !
  PMESH_SIZE(:) = ZXX(:) * ZYY(:) * ZMAP(:)**2
  !
  DEALLOCATE(ZXX )
  DEALLOCATE(ZYY )
  DEALLOCATE(ZMAP)
  DEALLOCATE(PLAT_XY_C)
  DEALLOCATE(ZDLAT)
  DEALLOCATE(IXX  )
  DEALLOCATE(IYY  )
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MESH_SIZE_GAUSS',1,ZHOOK_HANDLE)
  !
  !-------------------------------------------------------------------------------
  END SUBROUTINE MESH_SIZE_GAUSS

  !############################################################################
  !############################################################################
  !############################################################################

END MODULE MODE_GRIDTYPE_GAUSS

