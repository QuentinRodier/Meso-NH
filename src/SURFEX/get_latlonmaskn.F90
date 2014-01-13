!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_LATLONMASK_n(OLATLONMASK,HGRID,PGRID_PAR,KGRID_PAR)
!     #######################################################
!
!!**** *GET_LATLONMASK_n* get the grid dimensions
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
!!    P. Le Moigne         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2007
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, XGRID_PAR, NGRID_PAR
!      
USE MODI_LATLONMASK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
 CHARACTER(LEN=10), INTENT(OUT)             ::  HGRID      
REAL, DIMENSION(:), POINTER                ::  PGRID_PAR      
INTEGER, INTENT(OUT)                       ::  KGRID_PAR      
LOGICAL, DIMENSION(:,:), INTENT(OUT)       ::  OLATLONMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('GET_LATLONMASK_N',0,ZHOOK_HANDLE)
NGRID_PAR=SIZE(XGRID_PAR)

 CALL LATLONMASK(CGRID,NGRID_PAR,XGRID_PAR,OLATLONMASK)
!
HGRID=CGRID
!
KGRID_PAR=NGRID_PAR
!
ALLOCATE(PGRID_PAR(KGRID_PAR))
!
PGRID_PAR(:)=XGRID_PAR(:)
IF (LHOOK) CALL DR_HOOK('GET_LATLONMASK_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_LATLONMASK_n
