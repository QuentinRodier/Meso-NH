!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE NUDGING_ISBA_SNOW(IO, KK, PK, PEK, DMK, KYEAR, KMONTH, KDAY, PTSTEP, PSR)                 

!     #####################################################################
!
!!****  *NUDGING_ISBA_SNOW*  
!!
!!    PURPOSE
!!    -------
!!   
!!     Prepares the nudging of the snow
!!
!!    METHOD
!!    -------
!
!     Step 1
!     Select the current day towards which the nudging is applied
!     
!     Step 2
!     Computes the nudging fluxes
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!    J. Colin           * Meteo-France *
!!
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 08/16
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_PE_t, ISBA_P_t, ISBA_K_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_SNOW_PAR, ONLY : XSNOWDMIN, XRHOSMAX_ES, XRHOSMIN_ES
USE MODD_CSTS,     ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
INTEGER,            INTENT(IN)    :: KYEAR  ! Current year
INTEGER,            INTENT(IN)    :: KMONTH ! Current month
INTEGER,            INTENT(IN)    :: KDAY   ! Current day
REAL,               INTENT(IN)    :: PTSTEP ! Time step
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSR    ! Snow rate (in kg/m2/s)
!
!*      0.2    declarations of local parameters
!
REAL, PARAMETER                   :: ZDZMIN  = 0.0001
REAL, PARAMETER                   :: ZSWEMIN = 1.0E-10
!
!*      0.2    declarations of local variables 
!
REAL, DIMENSION(SIZE(PSR))                  :: ZSNOWD_INIT       ! Initial Total depth of snow
REAL, DIMENSION(SIZE(PSR))                  :: ZSNOWD_TOT        ! Total depth of snow (after nudging)
REAL, DIMENSION(SIZE(PSR))                  :: ZSNOWPR           ! Initial Solid precipitation
REAL, DIMENSION(SIZE(PSR))                  :: ZSWE_INIT         ! Initial total snow water content
REAL, DIMENSION(SIZE(PSR))                  :: ZSWE_TOT          ! Total snow water content (after nudging)
REAL, DIMENSION(SIZE(PSR),PEK%TSNOW%NLAYER) :: ZSNOWDZ           ! Depth of snow per layer
REAL, DIMENSION(SIZE(PSR))                  :: ZMASK             ! Mask
REAL, DIMENSION(SIZE(PSR))                  :: ZWORK             ! work array
REAL, DIMENSION(SIZE(PSR))                  :: ZNUDG_SWE         ! Final Nudging value
REAL, DIMENSION(SIZE(PSR))                  :: ZSNOWABLAT_DELTA  ! FLAG =1 if snow ablates

!
INTEGER :: INI,INS ! Loop bounds
INTEGER :: JI,JL   ! Loop indexes

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SNOW',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!        1.     Initialisations 
!               ---------------

INI = SIZE(PSR,1)
INS = PEK%TSNOW%NLAYER   ! total snow layers
!
IF(IO%LNUDG_SWE_MASK)THEN
  ZMASK(:)=KK%XNUDG_MASK(:)
ELSE
  ZMASK(:)=1.0
ENDIF
!
ZSNOWPR(:) = PSR(:)
!
ZSNOWDZ(:,:) = PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:)
!
DMK%XNUDGINCSWE(:) = 0.0
!
ZSNOWABLAT_DELTA(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!       2     Compute the snow nudging
!             ----------------------------
!
IF (PEK%TSNOW%SCHEME=='D95') THEN
  !
  ! If D95
  !
  ZSWE_INIT(:) = PEK%TSNOW%WSNOW(:,1)
  !
  ZWORK(:) = (MAX(0.,PK%XNUDG_SWE(:,KDAY))-PEK%TSNOW%WSNOW(:,1))
  !  
  DO JI=1,INI
    IF ((ZSNOWDZ(JI,1)<=ZDZMIN).AND.(PK%XNUDG_SWE(JI,KDAY)>0.)) THEN            
       PSR(JI) = PSR(JI) + ZMASK(JI)*PK%XNUDG_SWE(JI,KDAY)/PTSTEP
    ELSEIF (ZSNOWDZ(JI,1)>ZDZMIN) THEN
       PEK%TSNOW%WSNOW(JI,1) = MAX(0.,PEK%TSNOW%WSNOW(JI,1)+ZWORK(JI)*ZMASK(JI)*(PTSTEP/IO%XTRELAX_SWE))
    ENDIF                   
  ENDDO
  !
  ZSWE_TOT(:) = PEK%TSNOW%WSNOW(:,1)  
  !
  ! removes very small values due to computation precision and update pronostic
  ! variable (as in hydro_snow)
  !
  WHERE(PEK%TSNOW%WSNOW(:,1) < ZSWEMIN) 
       PEK%TSNOW%WSNOW(:,1) = 0.
  END WHERE
  !
  WHERE ( PEK%TSNOW%WSNOW(:,1) == 0.0 ) 
    PEK%TSNOW%RHO(:,1) = XUNDEF 
    PEK%TSNOW%ALB(:)  = XUNDEF 
  END WHERE
  !
ELSEIF (PEK%TSNOW%SCHEME=='3-L') THEN
  !
  ! If isba_es
  !
  ZSNOWD_INIT(:) = 0.
  ZSWE_INIT  (:) = 0.
  DO JL=1,INS
     DO JI=1,INI
        ZSNOWD_INIT(JI) = ZSNOWD_INIT(JI) + ZSNOWDZ (JI,JL)
        ZSWE_INIT  (JI) = ZSWE_INIT  (JI) + PEK%TSNOW%WSNOW(JI,JL)
     ENDDO
  ENDDO
  !
  ZSWE_TOT  (:) = 0.
  ZSNOWD_TOT(:) = 0.
  !
  ! Remove small values of PK%XNUDG_SWE
  !
  ZNUDG_SWE(:) = PK%XNUDG_SWE(:,KDAY)
  WHERE((PK%XNUDG_SWE(:,KDAY)/XRHOSMAX_ES)<XSNOWDMIN)
        ZNUDG_SWE(:)= 0.0
  END WHERE
  !
  DO JI=1,INI
     !
     IF ((ZSNOWD_INIT(JI) < XSNOWDMIN).AND.(ZNUDG_SWE(JI)>0.)) THEN
        !
        ! If there is no snow and the nudging value is positive, convert the latter in precipitation
        PSR(JI) = PSR(JI) + ZMASK(JI)*ZNUDG_SWE(JI)/PTSTEP
        !
     ELSEIF (ZSNOWD_INIT(JI) >= XSNOWDMIN) THEN 
        !
        ! If there is snow, nudge
        !
        DO JL=1,INS
           !
           ZWORK(JI) = MAX(0.,ZNUDG_SWE(JI)*PEK%TSNOW%WSNOW(JI,JL)/ZSWE_INIT(JI))        
           PEK%TSNOW%WSNOW(JI,JL) = MAX(0.,PEK%TSNOW%WSNOW(JI,JL)+(ZWORK(JI)-PEK%TSNOW%WSNOW(JI,JL)) &
                                  * ZMASK(JI)*(PTSTEP/IO%XTRELAX_SWE))   
           !
           ZSWE_TOT  (JI) = ZSWE_TOT  (JI) + PEK%TSNOW%WSNOW(JI,JL)
           ZSNOWD_TOT(JI) = ZSNOWD_TOT(JI) + (PEK%TSNOW%WSNOW(JI,JL)/PEK%TSNOW%RHO(JI,JL))
           !
        ENDDO
        !
    ENDIF  
    !
  ENDDO
  !
  ! Remove traces of snow, as in snow3L_isba
  !
  WHERE (ZSNOWD_TOT(:)<XSNOWDMIN*1.1)
      ZSNOWABLAT_DELTA(:) = 1.0
      PEK%TSNOW%ALB(:)         = XUNDEF
  ENDWHERE
  !
  ! If snow is ablated, reinitialize snow prognostic variables
  !
  DO JL=1,INS 
      DO JI=1,INI
        PEK%TSNOW%WSNOW(JI,JL) = (1.0-ZSNOWABLAT_DELTA(JI))*PEK%TSNOW%WSNOW(JI,JL)
        PEK%TSNOW%HEAT (JI,JL) = (1.0-ZSNOWABLAT_DELTA(JI))*PEK%TSNOW%HEAT (JI,JL)
        PEK%TSNOW%RHO  (JI,JL) = (1.0-ZSNOWABLAT_DELTA(JI))*PEK%TSNOW%RHO  (JI,JL) + ZSNOWABLAT_DELTA(JI)*XRHOSMIN_ES  
        DMK%XSNOWTEMP  (JI,JL) = (1.0-ZSNOWABLAT_DELTA(JI))*DMK%XSNOWTEMP  (JI,JL) + ZSNOWABLAT_DELTA(JI)*XTT 
        PEK%TSNOW%AGE  (JI,JL) = (1.0-ZSNOWABLAT_DELTA(JI))*PEK%TSNOW%AGE  (JI,JL)
      ENDDO
  ENDDO
  !
ENDIF ! Snow scheme
!
!--------------------------------------------------------------------------------
!
!      3. Compute Nudging Increment (kg/m2)  
!      ------------------------------------
!
DMK%XNUDGINCSWE(:) = (ZSWE_TOT(:)-ZSWE_INIT(:))+(PSR(:)-ZSNOWPR(:))*PTSTEP
!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SNOW',1,ZHOOK_HANDLE)
!
END SUBROUTINE NUDGING_ISBA_SNOW
