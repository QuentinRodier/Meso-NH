!     ###########################################################
      SUBROUTINE PGD_SURF_ATM(HPROGRAM,HFILE,HFILETYPE,OZS)
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
!!      A. Lemonsu      05/2009         Ajout de la clef LGARDEN pour TEB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_PGD_GRID,        ONLY : LLATLONMASK
USE MODD_SURF_ATM_n,      ONLY : CNATURE, CSEA, CWATER, CTOWN,     &
                                 XSEA, XWATER,                     &
                                 NDIM_NATURE, NDIM_SEA,            &
                                 NDIM_TOWN,NDIM_WATER,             &
                                 LECOCLIMAP, LWATER_TO_NATURE,     &
                                 LTOWN_TO_ROCK, LGARDEN, NDIM_FULL,&
                                 NSIZE_FULL
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, XGRID_PAR, NGRID_PAR, XLAT, &
                                 XLON, XMESH_SIZE, XJPDIR   
USE MODD_CH_SURF_n,       ONLY : LCH_EMIS, LRW_CH_EMIS, CCH_EMIS
!
USE MODI_GET_LUOUT
USE MODI_READ_PGD_ARRANGE_COVER
USE MODI_READ_PGD_COVER_GARDEN
USE MODI_INI_DATA_COVER
USE MODI_READ_PGD_SCHEMES
USE MODI_READ_NAM_WRITE_COVER_TEX
USE MODI_WRITE_COVER_TEX_START
USE MODI_WRITE_COVER_TEX_COVER
USE MODI_LATLON_GRID
USE MODI_PUT_PGD_GRID
USE MODI_LATLONMASK
USE MODI_PGD_GAUSS_INDEX
USE MODI_PGD_FRAC
USE MODI_PGD_COVER
USE MODI_PGD_OROGRAPHY
USE MODI_PGD_NATURE
USE MODI_PGD_TOWN
USE MODI_PGD_INLAND_WATER
USE MODI_PGD_SEA
USE MODI_PGD_DUMMY
USE MODI_PGD_CHEMISTRY
USE MODI_PGD_CHEMISTRY_SNAP
USE MODI_WRITE_COVER_TEX_END
USE MODI_INIT_READ_DATA_COVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_NAM_PGD_CHEMISTRY
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE    ! atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE! atmospheric file type
LOGICAL,              INTENT(IN)  :: OZS      ! .true. if orography is imposed by atm. model
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT ! logical unit of output listing file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_SURF_ATM',0,ZHOOK_HANDLE)
CPROGNAME=HPROGRAM
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Set default constant values 
!             ---------------------------
!
 CALL READ_PGD_ARRANGE_COVER(HPROGRAM,LWATER_TO_NATURE,LTOWN_TO_ROCK)
!
 CALL READ_PGD_COVER_GARDEN(HPROGRAM,LGARDEN)
!
 CALL INIT_READ_DATA_COVER(HPROGRAM)
!
 CALL INI_DATA_COVER
!
!*    1.2     surface schemes
 CALL READ_PGD_SCHEMES(HPROGRAM,CNATURE,CSEA,CTOWN,CWATER)
!
!*    1.3     prints all parameters in a Latex file
 CALL READ_NAM_WRITE_COVER_TEX(HPROGRAM)
!
 CALL WRITE_COVER_TEX_START(HPROGRAM)
 CALL WRITE_COVER_TEX_COVER
!-------------------------------------------------------------------------------
!
!*    2.      Grid
!             ----
!
ALLOCATE(XLAT(NSIZE_FULL))
ALLOCATE(XLON(NSIZE_FULL))
ALLOCATE(XMESH_SIZE(NSIZE_FULL))
ALLOCATE(XJPDIR(NSIZE_FULL))
 CALL LATLON_GRID(CGRID,NGRID_PAR,NSIZE_FULL,ILUOUT,XGRID_PAR,XLAT,XLON,XMESH_SIZE,XJPDIR)
!
!
!*    2.3     Stores the grid in the module MODD_PGD_GRID
!
 CALL PUT_PGD_GRID(CGRID,NSIZE_FULL,NGRID_PAR,XGRID_PAR)
!
!*    2.4     mask to limit the number of input data to read
 CALL LATLONMASK      (CGRID,NGRID_PAR,XGRID_PAR,LLATLONMASK)
!
!*    2.5     gaussien grid mesh index
!
IF(CGRID=='GAUSS') CALL PGD_GAUSS_INDEX(HPROGRAM,OZS)
!
!-------------------------------------------------------------------------------
!
!*    3.      surface cover
!             -------------
!
 CALL PGD_FRAC(HPROGRAM,LECOCLIMAP)
IF (LECOCLIMAP) CALL PGD_COVER(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    4.      Orography
!             ---------
!
 CALL PGD_OROGRAPHY(HPROGRAM,XSEA,XWATER,HFILE,HFILETYPE,OZS)
!
!_______________________________________________________________________________
!
!*    5.      Additionnal fields for nature scheme
!             ------------------------------------
!
IF (NDIM_NATURE>0) CALL PGD_NATURE(HPROGRAM,LECOCLIMAP)  
!_______________________________________________________________________________
!
!*    6.      Additionnal fields for town scheme
!             ----------------------------------
!
IF (NDIM_TOWN>0) CALL PGD_TOWN(HPROGRAM,LECOCLIMAP,LGARDEN)  
!_______________________________________________________________________________
!
!*    7.      Additionnal fields for inland water scheme
!             ------------------------------------------
!
IF (NDIM_WATER>0) CALL PGD_INLAND_WATER(HPROGRAM)   
!_______________________________________________________________________________
!
!*    8.      Additionnal fields for sea scheme
!             ---------------------------------
!
IF (NDIM_SEA>0) CALL PGD_SEA(HPROGRAM)  
!
!_______________________________________________________________________________
!
!*    9.      Dummy fields
!             ------------
!
 CALL PGD_DUMMY(HPROGRAM)
!_______________________________________________________________________________
!
!*   10.      Chemical Emission fields
!             ------------------------
!
 CALL READ_NAM_PGD_CHEMISTRY(HPROGRAM,CCH_EMIS)
IF (CCH_EMIS=='SNAP') THEN
  CALL PGD_CHEMISTRY_SNAP(HPROGRAM,LCH_EMIS)
ELSE IF (CCH_EMIS=='AGGR') THEN
  CALL PGD_CHEMISTRY(HPROGRAM,LCH_EMIS)
  LRW_CH_EMIS = .FALSE.
ENDIF
!_______________________________________________________________________________
!
!*   11.     Writing in cover latex file
!            ---------------------------
!
 CALL WRITE_COVER_TEX_END(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('PGD_SURF_ATM',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE PGD_SURF_ATM
