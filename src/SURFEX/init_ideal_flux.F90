!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ############################################################
      SUBROUTINE INIT_IDEAL_FLUX (DGL, OREAD_BUDGETC, &
                                  HPROGRAM,HINIT,                            &
                                   KI,KSV,KSW,                                &
                                   HSV,PCO2,PRHOA,                            &
                                   PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                                   PEMIS,PTSRAD,PTSURF,                       &
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
!!      J. Cuxart and J. Stein       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/01/95 
!!      V. Masson      02/03  split the routine in two (initialization here, and run)
!!      R. Honnert     07/10  allows reading of data in namelist
!!      B. Decharme  04/2013 new coupling variables
!!      P. Le Moigne  03/2015 add diagnostics IDEAL case
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_t
!
USE MODD_IDEAL_FLUX, ONLY : XSFTS, XALB, XEMIS
USE MODN_IDEAL_FLUX
USE MODD_READ_NAMELIST, ONLY : LNAM_READ

USE MODI_DIAG_IDEAL_INIT_n
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
!
TYPE(DIAG_IDEAL_t), INTENT(INOUT) :: DGL
!
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
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
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
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
 CALL DEFAULT_DIAG_IDEAL(DGL%N2M,DGL%LSURF_BUDGET,DGL%L2M_MIN_ZS,DGL%LRAD_BUDGET,DGL%LCOEF,DGL%LSURF_VARS,&
                         DGL%LSURF_BUDGETC,DGL%LRESET_BUDGETC,DGL%XDIAG_TSTEP           )  

ENDIF
!----------------------------------------------------------------------------------
!
!*       0.2    configuration
!               -------------
!
 CALL READ_DEFAULT_IDEAL_n(DGL, &
                           HPROGRAM)
 CALL READ_IDEAL_CONF_n(DGL, &
                        HPROGRAM)
!
IF (.NOT.ALLOCATED(XTIMEF_f)) THEN

!$OMP SINGLE
  ALLOCATE(XTIMEF_f (NFORCF+1))
  ALLOCATE(XSFTH_f  (NFORCF+1))
  ALLOCATE(XSFTQ_f  (NFORCF+1))
  ALLOCATE(XSFCO2_f (NFORCF+1))
  IF (CUSTARTYPE=='USTAR') ALLOCATE(XUSTAR_f (NFORCF+1))
!
  ALLOCATE(XTIMET_t (NFORCT+1))
  ALLOCATE(XTSRAD_t (NFORCT+1))
!$OMP END SINGLE
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
    CALL PREP_CTRL_IDEAL(DGL%N2M,DGL%LSURF_BUDGET,DGL%L2M_MIN_ZS,DGL%LRAD_BUDGET,DGL%LCOEF,DGL%LSURF_VARS,&
                          ILUOUT,DGL%LSURF_BUDGETC)  
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
 CALL DIAG_IDEAL_INIT_n(DGL, HPROGRAM, OREAD_BUDGETC, &
                        KI,KSW)
!
ENDIF
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
PTSURF   = PTSRAD
!
!-------------------------------------------------------------------------------
!
!*       9.    Fluxes as diagnostics
!              ---------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_IDEAL_FLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IDEAL_FLUX
