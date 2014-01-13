!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_FLUX_n(HPROGRAM,KI,PRN,PH,PLE,PLEI,PGFLUX,PT2M,PQ2M,   &
                            PHU2M,PZON10M,PMER10M,PSURFLWNET,PSURFSWNET     )  
!     ########################################
!
!!****  *GET_FLUX_n* - routine to get some surface fields
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,        ONLY   : XUNDEF
!
USE MODD_DIAG_SURF_ATM_n, ONLY   : XAVG_RN, XAVG_H, XAVG_LE, XAVG_GFLUX,   &
                                     XAVG_T2M, XAVG_Q2M, XAVG_HU2M,        &
                                     XAVG_ZON10M, XAVG_MER10M,             &
                                     LSURF_BUDGET, N2M, XAVG_LEI,          &
                                     XAVG_LWD, XAVG_LWU, XAVG_SWD, XAVG_SWU  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)     :: HPROGRAM
INTEGER,             INTENT(IN)     :: KI        ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PRN       ! Net radiation at surface    (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PH        ! Sensible heat flux          (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PLE       ! Total Latent heat flux      (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PLEI      ! Solid Latent heat flux      (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PGFLUX    ! Net soil-vegetation flux    (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PT2M      ! Air temperature at 2 meters (K)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQ2M      ! Air humidity at 2 meters    (kg/kg)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PHU2M     ! Air relative humidity at 2 meters (-)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZON10M   ! zonal Wind at 10 meters     (m/s)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PMER10M   ! meridian Wind at 10 meters  (m/s)
!
REAL, DIMENSION(KI),  INTENT(OUT) :: PSURFLWNET   ! LW net at the surface
REAL, DIMENSION(KI),  INTENT(OUT) :: PSURFSWNET   ! SW net at the surface
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_FLUX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (LSURF_BUDGET)      THEN 
        PRN       = XAVG_RN      
        PH        = XAVG_H  
        PLE       = XAVG_LE 
        PLEI      = XAVG_LEI 
        PGFLUX    = XAVG_GFLUX 
        PSURFLWNET=XAVG_LWD-XAVG_LWU
        PSURFSWNET=XAVG_SWD-XAVG_SWU
   ELSE 
        PRN      = XUNDEF
        PH       = XUNDEF
        PLE      = XUNDEF
        PLEI     = XUNDEF
        PGFLUX   = XUNDEF
        PSURFLWNET=XUNDEF
        PSURFSWNET=XUNDEF   
ENDIF           
!
IF (N2M>0)      THEN 
        PT2M      = XAVG_T2M
        PQ2M      = XAVG_Q2M
        PHU2M     = XAVG_HU2M
        PZON10M   = XAVG_ZON10M
        PMER10M   = XAVG_MER10M
   ELSE 
        PT2M     = XUNDEF
        PQ2M     = XUNDEF
        PHU2M    = XUNDEF
        PZON10M  = XUNDEF
        PMER10M  = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_FLUX_N',1,ZHOOK_HANDLE)
!==============================================================================
!
END SUBROUTINE GET_FLUX_n
