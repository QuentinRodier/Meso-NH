!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE PGD_GRID_SURF_ATM(HPROGRAM,HFILE,HFILETYPE,OGRID)
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
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
!!    Original     13/10/03
!!      M.Moge     10/02/15 change in the input parameters of PGD_GRID_IO_INIT
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,        ONLY : NVERSION, NBUGFIX
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_PGD_GRID,        ONLY : LLATLONMASK, NL
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, XGRID_PAR, NGRID_PAR, &
                                 XLAT, XLON, XMESH_SIZE, XJPDIR
!
USE MODI_PGD_GRID
USE MODI_INI_CSTS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!USE MODI_PGD_GRID_IO_INIT
USE MODI_SURF_VERSION
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE    ! atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE! atmospheric file type
LOGICAL,              INTENT(IN)  :: OGRID    ! .true. if grid is imposed by atm. model
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER :: IRESP ! error return code
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_GRID_SURF_ATM',0,ZHOOK_HANDLE)
CPROGNAME=HPROGRAM
!
!*    1.      Set default constant values 
!             ---------------------------
!
 CALL SURF_VERSION
!
 CALL INI_CSTS
!
!-------------------------------------------------------------------------------
!
!*    2.      Initialisation of output grid
!             -----------------------------
!
 CALL PGD_GRID        (HPROGRAM,HFILE,HFILETYPE,OGRID,CGRID,NGRID_PAR,XGRID_PAR)
! 
!
!-------------------------------------------------------------------------------
!
!
!*    3.      Additional actions for I/O
!
!#ifdef MNH_PARALLEL
! CALL PGD_GRID_IO_INIT(HPROGRAM,NGRID_PAR,XGRID_PAR)
!#else
! CALL PGD_GRID_IO_INIT(HPROGRAM)
!#endif
!we already called PGD_GRID_IO_INIT in subroutine PGD_GRID
!
IF (LHOOK) CALL DR_HOOK('PGD_GRID_SURF_ATM',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE PGD_GRID_SURF_ATM
