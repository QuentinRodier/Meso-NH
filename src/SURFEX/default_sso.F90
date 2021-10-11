!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
  !     ########################################################################
      SUBROUTINE DEFAULT_SSO(HROUGH,PFRACZ0,PCOEFBE,ODSV,ODSH,ODSL)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for 
!                        main logical switch for orographic roughness
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
!!      V. Masson  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2010
!!      A. Mary     04/2015    Ororad rewriting
!!      A. Mary     04/2016    Ororad phasing
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
 CHARACTER(LEN=4), INTENT(OUT) :: HROUGH
REAL, INTENT(OUT) :: PFRACZ0
REAL, INTENT(OUT) :: PCOEFBE
LOGICAL, INTENT(OUT) :: ODSV, ODSH, ODSL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! General switch
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SSO',0,ZHOOK_HANDLE)
HROUGH = "BE04"
PFRACZ0 = 2.
PCOEFBE = 2.
ODSV = .FALSE.
ODSH = .FALSE.
ODSL = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_SSO',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_SSO
