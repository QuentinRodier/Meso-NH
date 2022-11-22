!MNH_LIC Copyright 2009-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_MEAN_FIELD
!     ##########################
!
INTERFACE
!
SUBROUTINE INI_MEAN_FIELD
!
END SUBROUTINE INI_MEAN_FIELD                  
!
END INTERFACE
!
END MODULE MODI_INI_MEAN_FIELD
!
!     ############################################################
      SUBROUTINE INI_MEAN_FIELD
!     ############################################################
!
!!****  *INI_MEAN_FIELD* - routine to initialize mean variables      
!!
!!    PURPOSE
!!    -------
!      
!!**  METHOD
!!    ------
!    !!      
!!    EXTERNAL
!!    --------   
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!  !!
!!
!!    REFERENCE
!!    ---------
!!       
!!
!!    AUTHOR
!!    ------
!!	P. Aumond      * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/12/09
!!                      10/2016 (C.Lac) Add max values
!!                      02/2021 (T.Nagel) add passive scalar (XSVT) and UW wind component
!!                      05/2021 (PA.Joulin) add wind turbine variables
!!                      11/2022 (E. Jezequel) add covariances
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!
USE MODD_MEAN_FIELD_n
USE MODD_MEAN_FIELD
USE MODD_PARAM_n        
USE MODD_EOL_MAIN, ONLY: LMAIN_EOL, CMETH_EOL, NMODEL_EOL
USE MODD_EOL_SHARED_IO, ONLY: XTHRU_SUM, XTORQ_SUM, XPOW_SUM
USE MODD_EOL_ALM
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
INTEGER :: IMI !Current model index
!
MEAN_COUNT = 0
!
XUM_MEAN  = 0.0
XVM_MEAN  = 0.0
XWM_MEAN  = 0.0
XTHM_MEAN = 0.0
XTEMPM_MEAN = 0.0
IF (CTURB /= 'NONE') XTKEM_MEAN = 0.0
XPABSM_MEAN = 0.0
XSVT_MEAN = 0.0
!
XU2_M2    = 0.0
XV2_M2    = 0.0
XW2_M2    = 0.0
IF (LCOV_FIELD) THEN
  XUV_MEAN  = 0.0
  XUW_MEAN  = 0.0
  XVW_MEAN  = 0.0
  XWTH_MEAN = 0.0
END IF
XTH2_M2   = 0.0
XTEMP2_M2 = 0.0
XPABS2_M2 = 0.0
!
IMI = GET_CURRENT_MODEL_INDEX()
IF (LMAIN_EOL .AND. IMI==NMODEL_EOL) THEN
 SELECT CASE(CMETH_EOL)
  CASE('ADNR') ! Actuator Disc Non-Rotating
   XTHRU_SUM      = 0.0
  CASE('ALM') ! Actuator Line Method
   XAOA_SUM       = 0.0
   XFAERO_RE_SUM  = 0.0
   XTHRU_SUM      = 0.0
   XTORQ_SUM      = 0.0
   XPOW_SUM       = 0.0
 END SELECT
END IF
!
!
XUM_MAX  = -1.E20
XVM_MAX  = -1.E20
XWM_MAX  = -1.E20
XTHM_MAX = 0.0
XTEMPM_MAX = 0.0
IF (CTURB /= 'NONE') XTKEM_MAX = 0.0
XPABSM_MAX = 0.0

END SUBROUTINE INI_MEAN_FIELD
