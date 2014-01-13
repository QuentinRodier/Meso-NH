!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ############################################################
      SUBROUTINE INIT_IDEAL_FLUX(HPROGRAM,HINIT,                            &
                                   KI,KSV,KSW,                                &
                                   HSV,PCO2,PRHOA,                            &
                                   PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                                   PEMIS,PTSRAD,                              &
                                   HTEST                                      )  
!     ############################################################
!
!!****  *INIT_IDEAL_FLUX * - Prescription of the surface fluxes for the temperature, 
!!    vapor, horizontal components of the wind and the scalar variables.   
!!
!!    PURPOSE
!!    -------
!       Give prescribed values of the surface fluxes for the potential 
!     temperature, the vapor, the horizontal components of the wind and the 
!     scalar variables. These fluxes are unsteady when a diurnal cycle 
!     is taken into account.
!
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
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!	    J. Cuxart and J. Stein       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/01/95 
!!      V. Masson      02/03  split the routine in two (initialization here, and run)
!!      R. Honnert     07/10  allows reading of data in namelist
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IDEAL_FLUX, ONLY : XSFTS, XALB, XEMIS
USE MODN_IDEAL_FLUX
USE MODD_READ_NAMELIST, ONLY : LNAM_READ

USE MODI_DIAG_IDEAL_INIT_n
USE MODD_DIAG_IDEAL_n, ONLY : N2M, LSURF_BUDGET, L2M_MIN_ZS, LRAD_BUDGET, LCOEF      , &
                                LSURF_VARS, XDIAG_TSTEP, LSURF_BUDGETC, LRESET_BUDGETC   
USE MODI_READ_IDEAL_CONF_n
USE MODI_READ_DEFAULT_IDEAL_n
USE MODI_PREP_CTRL_IDEAL
USE MODI_DEFAULT_DIAG_IDEAL
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
! 
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KI),  INTENT(IN)  :: PAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
!
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!*       0.2   declarations of local variables
!
INTEGER           :: ISV    ! number of scalar variables
INTEGER           :: ILUOUT ! unit of output listing fie
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_IDEAL_FLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!----------------------------------------------------------------------------------
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_IDEAL_FLUX: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!----------------------------------------------------------------------------------
IF (LNAM_READ) THEN
 !
 !*       0.1    defaults
 !               --------
 !
 CALL DEFAULT_DIAG_IDEAL(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                         LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP           )  

ENDIF
!----------------------------------------------------------------------------------
!
!*       0.2    configuration
!               -------------
!
 CALL READ_DEFAULT_IDEAL_n(HPROGRAM)
 CALL READ_IDEAL_CONF_n(HPROGRAM)
!
ALLOCATE(XTIMEF_f (NFORCF+1))
ALLOCATE(XSFTH_f  (NFORCF+1))
ALLOCATE(XSFTQ_f  (NFORCF+1))
ALLOCATE(XSFCO2_f (NFORCF+1))
IF (CUSTARTYPE=='USTAR') ALLOCATE(XUSTAR_f (NFORCF+1))
!
ALLOCATE(XTIMET_t (NFORCT+1))
ALLOCATE(XTSRAD_t (NFORCT+1))
!
XTIMEF_f(1:NFORCF) = XTIMEF(1:NFORCF)
XSFTH_f (1:NFORCF) = XSFTH (1:NFORCF)
XSFTQ_f (1:NFORCF) = XSFTQ (1:NFORCF)
XSFCO2_f(1:NFORCF) = XSFCO2(1:NFORCF)
IF (CUSTARTYPE=='USTAR') XUSTAR_f(1:NFORCF) = XUSTAR(1:NFORCF)
!
XTIMET_t(1:NFORCT) = XTIMET(1:NFORCT)
XTSRAD_t(1:NFORCT) = XTSRAD(1:NFORCT)
!
XTIMEF_f(NFORCF+1) = XTIMEF_f(NFORCF)+1
XSFTH_f (NFORCF+1) = XSFTH_f (NFORCF)
XSFTQ_f (NFORCF+1) = XSFTQ_f (NFORCF)
XSFCO2_f(NFORCF+1) = XSFCO2_f(NFORCF)
IF (CUSTARTYPE=='USTAR') XUSTAR_f(NFORCF+1) = XUSTAR_f(NFORCF)
!
XTIMET_t(NFORCT+1) = XTIMET(NFORCT)+1
XTSRAD_t(NFORCT+1) = XTSRAD(NFORCT)
!
!----------------------------------------------------------------------------------
!
!*       0.3    control
!               -------
!
IF (HINIT=='PRE') THEN
   CALL PREP_CTRL_IDEAL(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                          ILUOUT,LSURF_BUDGETC)  
ENDIF
!
!----------------------------------------------------------------------------------
!
!*       3.    HOURLY surface scalar mixing ratio fluxes (NFORCF+1 values per scalar from 00UTC to 24UTC)
!              -----------------------------------------
!
ISV = SIZE(HSV)
!
IF(.NOT. ALLOCATED (XSFTS) )ALLOCATE(XSFTS(NFORCF+1,ISV))
!
!* unit: kg/m2/s
!
XSFTS = 0.
!                                                          
!-------------------------------------------------------------------------------
!
!*       8.    Radiative outputs
!              -----------------
!
PTSRAD   = XTSRAD_t(1)
!
PDIR_ALB = XALB
PSCA_ALB = XALB
PEMIS    = XEMIS
!
!-------------------------------------------------------------------------------
!
!*       9.    Fluxes as diagnostics
!              ---------------------
!
 CALL DIAG_IDEAL_INIT_n(KI,KSW)
IF (LHOOK) CALL DR_HOOK('INIT_IDEAL_FLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IDEAL_FLUX
