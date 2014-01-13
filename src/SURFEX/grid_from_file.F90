!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GRID_FROM_FILE(HPROGRAM,HFILE,HFILETYPE,OGRID,HGRID,KGRID_PAR,PGRID_PAR,KL)
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
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_GRIDTYPE
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GRID_MODIF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)   :: HPROGRAM   ! program calling the surface
 CHARACTER(LEN=28), INTENT(IN)   :: HFILE      ! file name
 CHARACTER(LEN=6),  INTENT(IN)   :: HFILETYPE  ! file type
LOGICAL,           INTENT(IN)   :: OGRID      ! .true. if grid is imposed by atm. model
 CHARACTER(LEN=10), INTENT(OUT)  :: HGRID      ! type of horizontal grid
INTEGER,           INTENT(OUT)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
INTEGER,           INTENT(OUT)  :: KL         ! number of points on processor
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER           :: ILUOUT ! listing  file  logical unit
INTEGER           :: ILUNAM ! namelist file  logical unit
INTEGER           :: IRESP  ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!------------------------------------------------------------------------------
!
!*       1.    Defaults
!              --------
!
IF (LHOOK) CALL DR_HOOK('GRID_FROM_FILE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!---------------------------------------------------------------------------
!
!*       2.    Opening of the file
!              -------------------
!
 CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
!
!---------------------------------------------------------------------------
!
!*       3.    Number of points in this file
!              -----------------------------
!
 CALL READ_SURF(HFILETYPE,'DIM_FULL  ',KL,IRESP)
!
!---------------------------------------------------------------------------
!
!*       4.    Grid type
!              ---------
!
 CALL READ_SURF(HFILETYPE,'GRID_TYPE',HGRID,IRESP)
!
!---------------------------------------------------------------------------
!
!*       5.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,KL,.FALSE.,HDIR='A')
!
ALLOCATE(PGRID_PAR(KGRID_PAR))
 CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,KL,.TRUE.,PGRID_PAR,IRESP,HDIR='A')
!
!---------------------------------------------------------------------------
!
!*       6.    Closes the file
!              ---------------
!
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
!------------------------------------------------------------------------------
!
!*       7.    Open namelist
!              -------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!------------------------------------------------------------------------------
!
!*       8.    Grid modification
!              -----------------
!
IF (.NOT. OGRID) CALL GRID_MODIF(ILUOUT,ILUNAM,HGRID,KGRID_PAR,PGRID_PAR,KL)
!
!------------------------------------------------------------------------------
!
!*       9.    Close namelist
!              --------------
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
IF (LHOOK) CALL DR_HOOK('GRID_FROM_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GRID_FROM_FILE
