!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_GLACIER (PTSTEP,PSR,PSNOWRHO,PSNOWSWE,PGLASTO,PICEFLUX)
!     ########################################################################
!
!!****  *HYDRO_GLACIER*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the ice runoff fluxes over permanent snow area with LGLACIER
!     option
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
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
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/09 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XDAY
USE MODD_ISBA_n,   ONLY : TSNOW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX, XHGLA, XSNOWDMIN, XRHOSMAX_ES
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!                                       KTSTEP = timestep [s]
!
REAL, DIMENSION(:), INTENT(IN)       :: PSR
!                                       PSR      = Snowfall    [kg/m²s]
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWRHO
!                                       PSNOWRHO = Snow density [kg/m3]
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWSWE
!                                       PSNOWSWE = Snow water equivalent [kg/m²]
!
REAL, DIMENSION(:), INTENT(INOUT)    :: PGLASTO
!                                       PGLASTO  = Glacier storage      [kg/m²]
!
REAL, DIMENSION(:), INTENT(OUT)      :: PICEFLUX
!                                       PICEFLUX = Ice flux from the Snowfall reservoir [kg/m²s]
!
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZTAU=365.25 !days
!
REAL, DIMENSION(SIZE(PSR)) :: ZGLASTO,ZSTOMAX,ZFLUX,ZSR,ZSWE
REAL, DIMENSION(SIZE(PSR)) :: ZSNOWD
!
REAL              ::ZRHOSMAX
!
INTEGER :: JWRK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_GLACIER',0,ZHOOK_HANDLE)
!
ZGLASTO (:) = PGLASTO(:)
ZSTOMAX (:) = 0.0
ZFLUX   (:) = 0.0
ZSR     (:) = 0.0
ZSWE    (:) = 0.0
!
PICEFLUX(:) = 0.0
!
!-------------------------------------------------------------------------------
!Ice accumulation only if snow amount is > to 33.3 meters of aged snow
!
IF(TSNOW%SCHEME/='3-L' .AND. TSNOW%SCHEME/='CRO')THEN
  ZRHOSMAX=XRHOSMAX
  ZSWE(:)=PSNOWSWE(:,1)
ELSE
  ZRHOSMAX=XRHOSMAX_ES
  DO JWRK=1,SIZE(PSNOWSWE,2)
     ZSWE  (:) = ZSWE  (:) + PSNOWSWE(:,JWRK)
  END DO
ENDIF
!
WHERE(ZSWE(:)>XHGLA*ZRHOSMAX)
     ZSR(:) = PSR(:)
ELSEWHERE
     ZSR(:) = 0.0
ENDWHERE
!
!Snow storage calculation
!
PGLASTO(:)=(ZGLASTO(:)+PTSTEP*ZSR(:))/(1.0+PTSTEP/(ZTAU*XDAY))
!
!supress numerical artifacs
!
ZSTOMAX(:)=ZSR(:)*PTSTEP+ZGLASTO(:)
!
PGLASTO(:)=MIN(ZSTOMAX(:),PGLASTO(:))
!
!Ice flux calculation                
!
ZFLUX(:)=(ZGLASTO(:)-PGLASTO(:))/PTSTEP+ZSR(:)
!      
!supress numerical artifacs
!
PICEFLUX(:) = MAX(0.0,ZFLUX(:))
PGLASTO (:) = PGLASTO(:) + PICEFLUX(:)-ZFLUX(:)             
!
WHERE(PGLASTO(:)<=1.E-10)PGLASTO(:)=0.0
!
!-------------------------------------------------------------------------------
!Snow pack update
!
IF(TSNOW%SCHEME/='3-L' .AND. TSNOW%SCHEME/='CRO')THEN
!
  WHERE(PSNOWSWE(:,1)<=XHGLA*ZRHOSMAX)PICEFLUX(:)=0.0
  PSNOWSWE(:,1)=PSNOWSWE(:,1)-PICEFLUX(:)*PTSTEP
!
ELSE
!
  WHERE(ZSWE(:)<=XHGLA*ZRHOSMAX)PICEFLUX(:)=0.0
!
! Snow total depth
  ZSNOWD(:) = 0.
  DO JWRK=1,SIZE(PSNOWSWE,2)
     ZSNOWD(:) = ZSNOWD(:) + PSNOWSWE(:,JWRK)/PSNOWRHO(:,JWRK)
  END DO
!
! Flux
  DO JWRK=1,SIZE(PSNOWSWE,2)
     ZFLUX(:) = PICEFLUX(:)*(PSNOWSWE(:,JWRK)/PSNOWRHO(:,JWRK)) &
                /MAX(ZSNOWD(:),0.0001)
     PSNOWSWE(:,JWRK)=PSNOWSWE(:,JWRK)-ZFLUX(:)*PTSTEP
  END DO
!
ENDIF
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HYDRO_GLACIER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE HYDRO_GLACIER
