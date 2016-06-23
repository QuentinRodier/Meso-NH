!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HOR_EXTRAPOL_SURF_CHEAP(KLUOUT,HCOORTYPE,PLAT_IN,PLON_IN,PFIELD_IN, &
                                         PLAT,PLON,PFIELD,OINTERP)  
!     ###################################################################
!
!!**** *HOR_EXTRAPOL_SURF_CHEAP* extrapolate a surface field
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!       For each point to interpolate, the nearest valid point value is set.
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
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/12/98
!!     V. Masson    01/2004 extrapolation in latitude and longitude
!!     M. Jidane    11/2013 add OpenMP directives
!!     L. Raynaudl  06/2014 cheap version : nearest point looked for in a
!!                          neighbourhood
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XPI, XRADIUS
USE MODN_PREP_SURF_ATM, ONLY : NHALO_PREP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing logical unit
 CHARACTER(LEN=4), INTENT(IN) :: HCOORTYPE
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT_IN  ! input lat. of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON_IN  ! input lon. of each grid mesh.
REAL,   DIMENSION(:,:),  INTENT(IN)     :: PFIELD_IN! input field on grid mesh
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT     ! latitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON     ! longitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(INOUT)  :: PFIELD   ! field on grid mesh
LOGICAL,DIMENSION(:),  INTENT(IN)     :: OINTERP  ! .true. where physical value is needed
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER  :: INO   ! output  array size
!
REAL     :: ZLAT  ! latitude of point to define
REAL     :: ZLON  ! longitude of point to define
REAL     :: ZDIST ! current distance to valid point (in lat/lon grid)
REAL     :: ZFIELD! current found field value
REAL     :: ZNDIST! smallest distance to valid point
REAL     :: ZCOSLA! cosine of latitude
!
INTEGER  :: JI,JII,JJ,ICI,ICJ,ICI_MIN,ICI_MAX,ICJ_MIN,ICJ_MAX,ICOMPT    ! loop index on points
REAL     :: ZLONSC! longitude of valid point
LOGICAL  :: GLALO ! flag true is second coordinate is a longitude or pseudo-lon.
                  !      false if metric coordinates
!
REAL(KIND=JPRB) :: ZRAD ! conversion degrees to radians
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_CHEAP',0,ZHOOK_HANDLE)
!
INO = SIZE(PFIELD,1)
!
WHERE (.NOT. OINTERP(:)) PFIELD(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
GLALO = HCOORTYPE=='LALO'
!
!-------------------------------------------------------------------------------
!
!*    3.     No data point
!            -------------
!
IF (COUNT(PFIELD_IN(:,:)/=XUNDEF)==0 .AND. LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_CHEAP',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD_IN(:,:)/=XUNDEF)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*      4.   Loop on points to define
!            ------------------------
!
ZRAD=XPI/180.0_JPRB
!
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JII,JJ,ZLAT,ZLON,ZFIELD,ZCOSLA,ZLONSC,ZDIST,ZNDIST,ZHOOK_HANDLE_OMP)
DO JI=1,INO

  IF (PFIELD(JI)/=XUNDEF) CYCLE
  IF (.NOT. OINTERP(JI))  CYCLE
!
!*      4.1  initialisation
!            --------------
!
  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_CHEAP OMP',0,ZHOOK_HANDLE_OMP)
  ZNDIST=1.E20
  ZLAT=PLAT(JI)
  ZLON=PLON(JI)
  ZFIELD=PFIELD(JI)
  ZCOSLA=COS(ZLAT*ZRAD)

  ! Locate surrounding input points around the interpolated output point
  ! Size of the neighbourhood is given by NHALO_PREP
  ICI = 1
  DO JJ=SIZE(PLON_IN),1,-1
    IF (PLON_IN(JJ)<=ZLON) THEN
      ICI = JJ
      EXIT
    ENDIF
  ENDDO
  ICJ = 1
  DO JJ=SIZE(PLAT_IN),1,-1
    IF (PLAT_IN(JJ)<=ZLAT) THEN
      ICJ = JJ
      EXIT
    ENDIF
  ENDDO

  ICI_MIN=MAX(ICI-NHALO_PREP,1)
  ICI_MAX=MIN(ICI+NHALO_PREP,SIZE(PLON_IN))
  ICJ_MIN=MAX(ICJ-NHALO_PREP,1)
  ICJ_MAX=MIN(ICJ+NHALO_PREP,SIZE(PLAT_IN))
!
!*      4.2  extrapolation with nearest valid point
!            --------------------------------------

  ! The nearest point is looked for in a neighbourhood
  ! This helps reducing the cost
  ICOMPT=0
  DO JJ=ICJ_MIN,ICJ_MAX
    DO JII=ICI_MIN,ICI_MAX
      IF (PFIELD_IN(JII,JJ)/=XUNDEF) THEN
        ICOMPT=ICOMPT+1
        ZLONSC = PLON_IN(JII)
        IF (GLALO) THEN
          IF (ZLONSC-ZLON> 180.) ZLONSC = ZLONSC - 360.
          IF (ZLONSC-ZLON<-180.) ZLONSC = ZLONSC + 360. 
          ZDIST= (PLAT_IN(JJ)-ZLAT) ** 2 + ((ZLONSC-ZLON)*ZCOSLA) ** 2
        ELSE
          ZDIST= (PLAT_IN(JJ)-ZLAT) ** 2 + (ZLONSC-ZLON) ** 2
        END IF
        IF (ZDIST<=ZNDIST) THEN
          ZFIELD=PFIELD_IN(JII,JJ)
          ZNDIST=ZDIST
        END IF
      END IF
    ENDDO
  ENDDO
  PFIELD(JI) = ZFIELD
  IF (ICOMPT==0) THEN
    WRITE(*,*) 'NO EXTRAPOLATION : INCREASE YOUR HALO_PREP IN NAM_PREP_SURF_ATM'
    CALL ABOR1_SFX("HOR_EXTRAPOL_SURF_CHEAP: INCREASE YOUR HALO_PREP IN NAM_PREP_SURF_ATM")
  ENDIF

  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_CHEAP OMP',1,ZHOOK_HANDLE_OMP)
END DO
!$OMP END PARALLEL DO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_CHEAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE HOR_EXTRAPOL_SURF_CHEAP
