!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!     #########
      SUBROUTINE WRITE_DST_CONF(KLUDES,HEMISPARAM)
!     #######################################################
!
!!****  *WRITE_DST_CONF* - routine to write the configuration for DST
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
!!	A. Grini   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_DST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,          INTENT(IN) :: KLUDES     ! logical unit
 CHARACTER(LEN=5), INTENT(IN) :: HEMISPARAM ! Reference to paper where emission parameterization is proposed
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
!
!* writing of namelist
!  -------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DST_CONF',0,ZHOOK_HANDLE)
CEMISPARAM_DST=HEMISPARAM
WRITE(UNIT=KLUDES,NML=NAM_SURF_DST)
IF (LHOOK) CALL DR_HOOK('WRITE_DST_CONF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DST_CONF
