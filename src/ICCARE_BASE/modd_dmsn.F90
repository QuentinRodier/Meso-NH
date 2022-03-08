!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_DMS_n
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet   *LAERO
!!
!!    MODIFICATIONS
!!    -------------
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE DMS_t
!
  REAL, POINTER, DIMENSION(:) :: XDMS        !  contenu en DMS marin (nmole.dm-3)
!
END TYPE DMS_t

 CONTAINS
!
SUBROUTINE DMS_INIT(YDMS)        
TYPE(DMS_t), INTENT(INOUT) :: YDMS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DMS_n:DMS_INIT",0,ZHOOK_HANDLE)
NULLIFY(YDMS%XDMS)
IF (LHOOK) CALL DR_HOOK("MODD_DMS_n:DMS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DMS_INIT


END MODULE MODD_DMS_n
