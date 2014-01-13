!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_GRID_EXTERN(HFILETYPE,KLUOUT,HGRIDTYPE,HINTERP_TYPE,KNI)
!     ##########################################################################
!
!!****  *PREP_GRID_EXTERN* - reads EXTERNALIZED Surface grid.
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODI_READ_SURF
USE MODI_PREP_GRID_CONF_PROJ
USE MODI_PREP_GRID_CARTESIAN
USE MODI_PREP_GRID_GAUSS
USE MODI_PREP_GRID_LONLAT_REG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE    ! file type
INTEGER,           INTENT(IN)    :: KLUOUT       ! logical unit of output listing
 CHARACTER(LEN=10),  INTENT(OUT)  :: HGRIDTYPE    ! Grid type
 CHARACTER(LEN=6),  INTENT(OUT)   :: HINTERP_TYPE ! Grid type
INTEGER,           INTENT(OUT)   :: KNI          ! number of points
!
!* 0.2 Declaration of local variables
!      ------------------------------
!
INTEGER :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
!*   1 Type of grid
!      ------------
!
IF (LHOOK) CALL DR_HOOK('PREP_GRID_EXTERN',0,ZHOOK_HANDLE)
 CALL READ_SURF(HFILETYPE,'GRID_TYPE',HGRIDTYPE,IRESP)
!
!-----------------------------------------------------------------------
!
!*   2 Reading of grid
!      ---------------
!
IF (HGRIDTYPE=='CONF PROJ ') THEN
  CALL PREP_GRID_CONF_PROJ(HFILETYPE,HINTERP_TYPE,KNI)
ELSE IF (HGRIDTYPE=='CARTESIAN ') THEN
  CALL PREP_GRID_CARTESIAN(HFILETYPE,HINTERP_TYPE,KNI)
ELSE IF (HGRIDTYPE=='GAUSS     ') THEN
  CALL PREP_GRID_GAUSS(HFILETYPE,HINTERP_TYPE,KNI)
ELSE IF (HGRIDTYPE=='LONLAT REG') THEN
  HGRIDTYPE = 'LATLON    '
  CALL PREP_GRID_LONLAT_REG(HFILETYPE,HINTERP_TYPE,KNI)
ELSE
  WRITE(KLUOUT,*) 'GRIDTYPE "',HGRIDTYPE,'" NOT ACCEPTED AS INPUT FILE FOR FIELD PREPARATION'
  CALL ABOR1_SFX('GRIDTYPE NOT ACCEPTED AS INPUT FILE FOR FIELD PREPARATION, '//HGRIDTYPE)
END IF
IF (LHOOK) CALL DR_HOOK('PREP_GRID_EXTERN',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------------
!
END SUBROUTINE PREP_GRID_EXTERN
