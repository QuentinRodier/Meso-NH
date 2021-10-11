!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_CROCUS(HSNOWDRIFT,OSNOWDRIFT_SUBLIM,OSNOW_ABS_ZENITH,&
                 HSNOWMETAMO,HSNOWRAD,OATMORAD,OSNOWSYTRON,HSNOWFALL,HSNOWCOND,HSNOWHOLD,&
                 HSNOWCOMP,HSNOWZREF, OSNOWCOMPACT_BOOL, OSNOWMAK_BOOL, &
                 OPRODSNOWMAK, OSNOWMAK_PROP, OSNOWTILLER, OSELF_PROD)  
! 
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for the configuration for Crocus
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
!!      M. Lafaysse   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2012
!!	M. Dumont 01/2016 atmotartes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
! Snowdrift scheme and selection of properties of falling snow  
CHARACTER(*), INTENT(OUT)     :: HSNOWDRIFT
! Logicals to activate / disactivate blowing snow sublimation  
LOGICAL, INTENT(OUT)          :: OSNOWDRIFT_SUBLIM
LOGICAL, INTENT(OUT)          :: OSNOW_ABS_ZENITH
LOGICAL, INTENT(OUT)          :: OATMORAD
! Logical to activate / disactivate Sytron                                          
LOGICAL, INTENT(OUT)          :: OSNOWSYTRON
!
! Snow metamorphism scheme and radiative transfer scheme <bber
CHARACTER(*), INTENT(OUT) :: HSNOWMETAMO,HSNOWRAD,HSNOWFALL, HSNOWCOND, HSNOWHOLD, HSNOWCOMP, HSNOWZREF!added HSNOWFALL HSNOWHOLD and HSNOWCOND bber>
!
! Snowmaking option p.spandre 19/11/2013                                          
!
LOGICAL, INTENT(OUT)                :: OSNOWCOMPACT_BOOL
LOGICAL, DIMENSION(:), INTENT(OUT)  :: OPRODSNOWMAK
LOGICAL, INTENT(OUT)                :: OSNOWMAK_BOOL
LOGICAL, INTENT(OUT)                :: OSNOWMAK_PROP
LOGICAL, INTENT(OUT)                :: OSNOWTILLER
LOGICAL, INTENT(OUT)                :: OSELF_PROD
!
!*       0.2   Declarations of local variables
!              -------------------------------
!                                          
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_CROCUS',0,ZHOOK_HANDLE)
!
HSNOWDRIFT        = 'DFLT'
OSNOWDRIFT_SUBLIM = .FALSE.
OSNOW_ABS_ZENITH = .FALSE.
OSNOWSYTRON=.FALSE.
OATMORAD=.FALSE.
! Snowmaking and Grooming default option and pressure default value | P.Spandre     20160211                                    
OSNOWCOMPACT_BOOL=.FALSE.                                    
OSNOWMAK_BOOL =.FALSE.
OPRODSNOWMAK = .FALSE.
OSNOWMAK_PROP =.FALSE.
OSNOWTILLER =.FALSE.
OSELF_PROD =.FALSE.
!
HSNOWMETAMO = 'B92'
HSNOWRAD    = 'B92'
!New multiphysics options Cluzet et al 2016
HSNOWFALL   = 'V12'
HSNOWCOND   = 'Y81'
HSNOWHOLD   = 'B92'
HSNOWCOMP   = 'B92'
HSNOWZREF   = 'CST'
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_CROCUS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_CROCUS
