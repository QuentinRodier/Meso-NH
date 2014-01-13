!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_INDEX(KLUOUT,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!     ##############################################################
!
!!**** *GET_MESH_INDEX* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID, ONLY : CGRID, XGRID_PAR, NGRID_PAR
USE MODD_POINT_OVERLAY
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_MESH_INDEX_CONF_PROJ
!
USE MODI_GET_MESH_INDEX_GAUSS
!
USE MODI_GET_MESH_INDEX_IGN
!
USE MODI_GET_MESH_INDEX_LONLAT_REG
!
USE MODI_GET_MESH_INDEX_LONLATVAL
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KLUOUT  ! output listing
REAL,    DIMENSION(:),           INTENT(IN)    :: PLAT    ! latitude of the point
REAL,    DIMENSION(:),           INTENT(IN)    :: PLON    ! longitude of the point
INTEGER, DIMENSION(:),           INTENT(OUT)   :: KINDEX  ! index of the grid mesh where the point is
INTEGER,               OPTIONAL, INTENT(IN)    :: KSSO    ! number of subgrid mesh in each direction
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: KISSOX  ! X index of the subgrid mesh where the point is
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: KISSOY  ! Y index of the subgrid mesh where the point is
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                        :: ISSO
INTEGER, DIMENSION(SIZE(PLAT)) :: IISSOX
INTEGER, DIMENSION(SIZE(PLAT)) :: IISSOY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX',0,ZHOOK_HANDLE)
SELECT CASE (CGRID)
!     
  CASE("CONF PROJ ","LONLAT REG","GAUSS     ","IGN      ","LONLATVAL ")
    IF (PRESENT(KSSO) .AND. PRESENT(KISSOX) .AND. PRESENT(KISSOY)) THEN
      ISSO = KSSO
    ELSE
      ISSO = 0
    ENDIF
    !
    IF (CGRID=="CONF PROJ ") THEN
      CALL GET_MESH_INDEX_CONF_PROJ(NGRID_PAR,SIZE(PLAT),XGRID_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
      XNUM(:)=0
    ENDIF
    IF (CGRID=="LONLAT REG") THEN
      CALL GET_MESH_INDEX_LONLAT_REG(NGRID_PAR,SIZE(PLAT),XGRID_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
      XNUM(:)=0     
    ENDIF
    IF (CGRID=="GAUSS     ") THEN
      CALL GET_MESH_INDEX_GAUSS(NGRID_PAR,SIZE(PLAT),XGRID_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
      XNUM(:)=0
    ENDIF
    IF (CGRID=="IGN       ") &
      CALL GET_MESH_INDEX_IGN(NGRID_PAR,SIZE(PLAT),XGRID_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
    IF (CGRID=="LONLATVAL ") &
      CALL GET_MESH_INDEX_LONLATVAL(NGRID_PAR,SIZE(PLAT),XGRID_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
    !
    IF (PRESENT(KSSO) .AND. PRESENT(KISSOX) .AND. PRESENT(KISSOY)) THEN
      KISSOX = IISSOX
      KISSOY = IISSOY
    ENDIF

  CASE DEFAULT
    WRITE(KLUOUT,*) 'error in physiographic fields computations (routine GET_MESH_INDEX)'
    WRITE(KLUOUT,*) 'It is impossible to retrieve geographical coordinates (latitude, longitude)'
    WRITE(KLUOUT,*) 'for the following grid type: CGRID = ', CGRID
    CALL ABOR1_SFX('GET_MESH_INDEX: IMPOSSIBLE TO RETRIEVE GEOGRAPHICAL COORDINATES')
END SELECT
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX
