!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_GRIDTYPE(HPROGRAM,HGRID,KGRID_PAR,PGRID_PAR,KL)
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Reads in namelist the grid type and parameters.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!!    E. Martin    10/2007  IGN Grids
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_READ_NAM_GRID_CARTESIAN
!
USE MODI_READ_NAM_GRID_CONF_PROJ
!
USE MODI_READ_NAM_GRID_GAUSS
!
USE MODI_READ_NAM_GRID_IGN
!
USE MODI_READ_NAM_GRID_LONLAT_REG
!
USE MODI_READ_NAM_GRID_LONLATVAL
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)   :: HPROGRAM   ! program calling the surface
 CHARACTER(LEN=10), INTENT(IN)   :: HGRID      ! grid type
INTEGER,           INTENT(OUT)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
INTEGER,           INTENT(OUT)  :: KL         ! number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRIDTYPE',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)

!*    1.      Conformal projection grid and regular lat/lon
!             ---------------------------------------------
!
  CASE ('CONF PROJ ','LONLAT REG','CARTESIAN ','GAUSS     ','IGN       ','LONLATVAL ')
    KGRID_PAR = 0
    ALLOCATE(PGRID_PAR(0))
    IF (HGRID=='CONF PROJ ')&
      CALL READ_NAM_GRID_CONF_PROJ(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='CARTESIAN ')&
      CALL READ_NAM_GRID_CARTESIAN(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='LONLAT REG')&
      CALL READ_NAM_GRID_LONLAT_REG(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='GAUSS     ')&
      CALL READ_NAM_GRID_GAUSS(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='IGN       ')&
      CALL READ_NAM_GRID_IGN(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='LONLATVAL ')&
      CALL READ_NAM_GRID_LONLATVAL(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)      
    DEALLOCATE(PGRID_PAR)
    ALLOCATE(PGRID_PAR(KGRID_PAR))
    IF (HGRID=='CONF PROJ ')&
      CALL READ_NAM_GRID_CONF_PROJ(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='CARTESIAN ')&
      CALL READ_NAM_GRID_CARTESIAN(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='LONLAT REG')&
      CALL READ_NAM_GRID_LONLAT_REG(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='GAUSS     ')&
      CALL READ_NAM_GRID_GAUSS(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='IGN       ')&
      CALL READ_NAM_GRID_IGN(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)  
    IF (HGRID=='LONLATVAL ')&
      CALL READ_NAM_GRID_LONLATVAL(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)   
        ! note that all points of the grid will be kept, whatever the surface
        ! type under consideration (e.g. sea points will be kept even for
        ! initialization of continents)
        !

!*    2.      Other cases
!             -----------
!
  CASE DEFAULT
    CALL ABOR1_SFX('READ_NAM_GRIDTYPE: GRID TYPE NOT SUPPORTED, '//HGRID)

END SELECT
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRIDTYPE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_GRIDTYPE
