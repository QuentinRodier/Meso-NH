!     ##################
      MODULE MODN_IDEAL_FLUX
!     ##################
!
!!****  *MODN_IDEAL_FLUX - declaration of keys for 
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/10/10
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_IDEAL_FLUX, ONLY : NFORC_MAX, NFORCF, NFORCT, XSFTH_f=>XSFTH, XSFTQ_f=>XSFTQ, &
                            XSFCO2_f=>XSFCO2, CUSTARTYPE, XUSTAR_f=>XUSTAR, XZ0, XALB, &
                            XEMIS, XTSRAD_t=>XTSRAD, XTIMEF_f=>XTIMEF, XTIMET_t=>XTIMET   
  
IMPLICIT NONE
!
REAL, DIMENSION(NFORC_MAX) :: XSFTH
 CHARACTER(LEN=7) :: CSFTQ ! Unit for the evaporation flux :
                          !'kg/m2/s'
                          !'W/m2   '
REAL, DIMENSION(NFORC_MAX) :: XSFTQ
REAL, DIMENSION(NFORC_MAX) :: XSFCO2
REAL, DIMENSION(NFORC_MAX) :: XUSTAR
REAL, DIMENSION(NFORC_MAX) :: XTSRAD
REAL, DIMENSION(NFORC_MAX) :: XTIMEF
REAL, DIMENSION(NFORC_MAX) :: XTIMET
!
!
NAMELIST/NAM_IDEAL_FLUX/NFORCF, NFORCT, XTIMEF, XTIMET, XSFTH, CSFTQ, XSFTQ, XSFCO2, &
                        CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD
!
END MODULE MODN_IDEAL_FLUX
