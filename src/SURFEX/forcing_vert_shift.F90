!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE FORCING_VERT_SHIFT(PZS_ATM,PZS_SURF,PTA_ATM,PQA_ATM,PPA_ATM,PRHOA_ATM, &
                                       PTA_SURF,PQA_SURF,PPA_SURF,PRHOA_SURF               )  
!      #########################################
!
!
!!****   *FORCING_VERT_SHIFT* - routine to shith atmospheric forcing to another altitude
!!
!!
!!     PURPOSE
!!     -------
!
!!**   METHOD
!!     ------
!!                      
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!       V. Masson
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        07/2003
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
USE MODD_CSTS,    ONLY : XRD, XG, XRV
USE MODD_ATM_CST, ONLY : XCLIM_T_GRAD
!
USE MODE_THERMOS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_ATM     ! orography of atmospheric grid
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_SURF    ! orography of surface     grid
REAL,    DIMENSION(:), INTENT(IN)  :: PTA_ATM     ! temperature at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PQA_ATM     ! humidity    at atmospheric altitude (kg/m3)
REAL,    DIMENSION(:), INTENT(IN)  :: PPA_ATM     ! pressure    at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PRHOA_ATM   ! density     at atmospheric altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PTA_SURF    ! temperature at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PQA_SURF    ! humidity    at surface     altitude (kg/m3)
REAL,    DIMENSION(:), INTENT(OUT) :: PPA_SURF    ! pressure    at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PRHOA_SURF  ! density     at surface     altitude
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQA_ATM    ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQA_SURF   ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PRHOA_ATM)) :: ZRHOA_ATM  ! approximated density
REAL, DIMENSION(SIZE(PRHOA_ATM)) :: ZRHOA_SURF ! approximated density
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FORCING_VERT_SHIFT',0,ZHOOK_HANDLE)
ZQA_ATM = PQA_ATM / PRHOA_ATM
!
!*       1.  climatological gradient for temperature
!            ---------------------------------------
!
PTA_SURF = PTA_ATM + XCLIM_T_GRAD * (PZS_SURF - PZS_ATM)
!
!-------------------------------------------------------------------------------
!
!*       2.  hydrostatism for pressure
!            -------------------------
!
PPA_SURF = PPA_ATM * EXP ( - XG/XRD/(0.5*(PTA_ATM+PTA_SURF)*( 1.+((XRV/XRD)-1.)*ZQA_ATM(:) )) &
                              * (PZS_SURF-PZS_ATM)                                              )  
!
!-------------------------------------------------------------------------------
!
!*       3.  conservation of relative humidity for humidity
!            ----------------------------------------------
!
ZQA_SURF = ZQA_ATM / QSAT(PTA_ATM, PPA_ATM) * QSAT(PTA_SURF,PPA_SURF)
!
!-------------------------------------------------------------------------------
!
!*       4.  estimation of air density from temperature and humidity
!            -------------------------------------------------------
!
ZRHOA_ATM (:) = PPA_ATM (:) / XRD /  PTA_ATM (:) / ( 1.+((XRV/XRD)-1.)*ZQA_ATM (:) )
ZRHOA_SURF(:) = PPA_SURF(:) / XRD /  PTA_SURF(:) / ( 1.+((XRV/XRD)-1.)*ZQA_SURF(:) )
!
PRHOA_SURF(:) = PRHOA_ATM(:) * ZRHOA_SURF(:) / ZRHOA_ATM (:)
!
!-------------------------------------------------------------------------------
!
!*       5.  new humidity in kg/m3
!            ---------------------
!
PQA_SURF = ZQA_SURF * PRHOA_ATM
IF (LHOOK) CALL DR_HOOK('FORCING_VERT_SHIFT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FORCING_VERT_SHIFT
