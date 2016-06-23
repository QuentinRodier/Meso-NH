!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CO2_TEB_GREENROOF_INIT_n (I, TGRP, TVG, &
                                           PCO2)
!     #####################
!
!!****  *CO2_TEB_GREENROOF_INIT_n* - routine to initialize ISBA-AGS variables
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2003 
!!      J.C. Calvet 01/2004 Externalization
!!      P Le Moigne 11/2004 cotwoinit changed into cotwoinit_n
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S Lafont    09/2008 Add initialisation of POI and ABC (needed for TORI)
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 04/2009 : Add carbon spinup
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_COTWOINIT_n
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
!
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: TGRP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
REAL, DIMENSION(:), INTENT(IN) :: PCO2 ! air CO2 concentration (kg/kg)
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(TGRP%XVEGTYPE,1)) :: ZTAU_WOOD
INTEGER :: ILU   ! size of arrays
INTEGER :: JP    ! loop on tiles
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CO2_TEB_GREENROOF_INIT_N',0,ZHOOK_HANDLE)
ILU = SIZE(TGRP%XVEGTYPE,1)
!
ALLOCATE(TGRP%XANMAX        (ILU))
ALLOCATE(TGRP%XFZERO        (ILU))
ALLOCATE(TGRP%XEPSO         (ILU))
ALLOCATE(TGRP%XGAMM         (ILU))
ALLOCATE(TGRP%XQDGAMM       (ILU))
ALLOCATE(TGRP%XQDGMES       (ILU))
ALLOCATE(TGRP%XT1GMES       (ILU))
ALLOCATE(TGRP%XT2GMES       (ILU))
ALLOCATE(TGRP%XAMAX         (ILU))
ALLOCATE(TGRP%XQDAMAX       (ILU))
ALLOCATE(TGRP%XT1AMAX       (ILU))
ALLOCATE(TGRP%XT2AMAX       (ILU))
ALLOCATE(TGRP%XAH           (ILU))
ALLOCATE(TGRP%XBH           (ILU))
!
     CALL COTWOINIT_n(I, &
                      TVG%CPHOTO, TGRP%XVEGTYPE,TGRP%XGMES,PCO2,TGRP%XGC,&
            TGRP%XDMAX,TGRP%XABC,TGRP%XPOI,TGRP%XANMAX, TGRP%XFZERO,           &
            TGRP%XEPSO,TGRP%XGAMM,TGRP%XQDGAMM,TGRP%XQDGMES,TGRP%XT1GMES,      &
            TGRP%XT2GMES,TGRP%XAMAX,TGRP%XQDAMAX,TGRP%XT1AMAX,            &
            TGRP%XT2AMAX,TGRP%XAH,TGRP%XBH,ZTAU_WOOD                 )  
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_TEB_GREENROOF_INIT_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CO2_TEB_GREENROOF_INIT_n
