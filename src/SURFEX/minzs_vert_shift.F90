!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE MINZS_VERT_SHIFT(PZS_MOY,PZS_MIN,PTA_2M,PQA_2M,PPA_2M,PRHOA_2M, &
                                   PTA_2M_MIN,PQA_2M_MIN,PPA_2M_MIN,PRHOA_2M_MIN  )  
!      #########################################
!
!
!!****   *MINZS_VERT_SHIFT* - routine to shift 2m variables to 2m variables 
!!                            above the minimum orography of the grid mesh
!!
!!
!!     PURPOSE
!!     -------
!
!!**   METHOD
!!     ------
!!
!!     Same method like in forcing_vert_shift.F90
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
!!       B. Decharme
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        06/2013
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
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_MOY    ! mean orography of atmospheric grid
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_MIN    ! min orography of atmospheric grid
REAL,    DIMENSION(:), INTENT(IN)  :: PTA_2M     ! temperature at 2m
REAL,    DIMENSION(:), INTENT(IN)  :: PQA_2M     ! humidity    at 2m (kg/m3)
REAL,    DIMENSION(:), INTENT(IN)  :: PPA_2M     ! pressure    at 2m
REAL,    DIMENSION(:), INTENT(IN)  :: PRHOA_2M   ! density     at 2m
!
REAL,    DIMENSION(:), INTENT(OUT) :: PTA_2M_MIN    ! temperature at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PQA_2M_MIN    ! humidity    at surface     altitude (kg/m3)
REAL,    DIMENSION(:), INTENT(OUT) :: PPA_2M_MIN    ! pressure    at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PRHOA_2M_MIN  ! density     at surface     altitude
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PQA_2M  )) :: ZQA_2M       ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PQA_2M  )) :: ZQA_2M_MIN   ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PRHOA_2M)) :: ZRHOA_2M     ! approximated density
REAL, DIMENSION(SIZE(PRHOA_2M)) :: ZRHOA_2M_MIN ! approximated density
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MINZS_VERT_SHIFT',0,ZHOOK_HANDLE)
!
ZQA_2M = PQA_2M / PRHOA_2M
!
!*       1.  climatological gradient for temperature
!            ---------------------------------------
!
PTA_2M_MIN = PTA_2M + XCLIM_T_GRAD * (PZS_MIN - PZS_MOY)
!
!-------------------------------------------------------------------------------
!
!*       2.  hydrostatism for pressure
!            -------------------------
!
PPA_2M_MIN = PPA_2M * EXP ( - XG/XRD/(0.5*(PTA_2M+PTA_2M_MIN)*( 1.+((XRV/XRD)-1.)*ZQA_2M(:) )) &
                              * (PZS_MIN-PZS_MOY)                                              )  
!
!-------------------------------------------------------------------------------
!
!*       3.  conservation of relative humidity for humidity
!            ----------------------------------------------
!
ZQA_2M_MIN = ZQA_2M / QSAT(PTA_2M, PPA_2M) * QSAT(PTA_2M_MIN,PPA_2M_MIN)
!
!-------------------------------------------------------------------------------
!
!*       4.  estimation of air density from temperature and humidity
!            -------------------------------------------------------
!
ZRHOA_2M    (:) = PPA_2M    (:) / XRD /  PTA_2M    (:) / ( 1.+((XRV/XRD)-1.)*ZQA_2M    (:) )
ZRHOA_2M_MIN(:) = PPA_2M_MIN(:) / XRD /  PTA_2M_MIN(:) / ( 1.+((XRV/XRD)-1.)*ZQA_2M_MIN(:) )
!
PRHOA_2M_MIN(:) = PRHOA_2M(:) * ZRHOA_2M_MIN(:) / ZRHOA_2M (:)
!
!-------------------------------------------------------------------------------
!
!*       5.  new humidity in kg/m3
!            ---------------------
!
PQA_2M_MIN = ZQA_2M_MIN * PRHOA_2M_MIN
!
IF (LHOOK) CALL DR_HOOK('MINZS_VERT_SHIFT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MINZS_VERT_SHIFT
