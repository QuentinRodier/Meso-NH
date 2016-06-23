!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE UPDATE_DATA_COVER (DTCO, DTI, IG, I, &
                                    KYEAR)
!     #########################
!
!!**** *INI_DATA_COVER* initializes cover-field correspondance arrays
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!!    P. Samuelsson 10/2014 MEB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_DATA_COVER,     ONLY :   XDATA_LAI, XDATA_H_TREE, &
                                  XDATA_VEG, XDATA_GREEN, XDATA_Z0, XDATA_EMIS_ECO, &
                                  XDATA_LAIGV, XDATA_Z0LITTER, XDATA_H_VEG, XDATA_LAIMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ECOCLIMAP2_LAI
!
USE MODI_INI_DATA_PARAM
USE MODI_FIX_MEB_VEG
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
!
INTEGER,             INTENT(IN)    :: KYEAR        ! new year
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER           :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_COVER',0,ZHOOK_HANDLE)
IF (KYEAR /= DTCO%NYEAR) THEN
  DTCO%NYEAR = KYEAR
  CALL ECOCLIMAP2_LAI(DTCO)
  CALL INI_DATA_PARAM(DTCO%XDATA_VEGTYPE, PSURF=DTCO%XDATA_NATURE, PSURF2=DTCO%XDATA_GARDEN, &
             PLAI=XDATA_LAI, PH_TREE=XDATA_H_TREE, PVEG_OUT=XDATA_VEG,        &
             PGREEN=XDATA_GREEN, PZ0=XDATA_Z0, PEMIS_ECO=XDATA_EMIS_ECO,      &
             PLAIMIN_OUT=XDATA_LAIMIN,                                        &
             PLAIGV_OUT=XDATA_LAIGV, PZ0LITTER=XDATA_Z0LITTER,                &
             PH_VEG=XDATA_H_VEG                                               )
!
  IF (ASSOCIATED(I%LMEB_PATCH)) THEN
    ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
  ELSE
    ISIZE_LMEB_PATCH=0
  END IF
!
  IF (ISIZE_LMEB_PATCH>0)  THEN
    CALL FIX_MEB_VEG(DTI, IG, I, &
                     I%NPATCH)
  ENDIF
!
END IF
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_COVER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------

END SUBROUTINE UPDATE_DATA_COVER
