!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_AGRI_n
!     ##################
!
!!****  *MODD_AGRI_n - declaration of SEEDING date for summer crops 
!!      
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. LE MOIGNE   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       06/2006
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
!-------------------------------------------------------------------------------
TYPE AGRI_t
!                                          
INTEGER, POINTER, DIMENSION (:,:)   :: NIRRINUM       
                                        ! Stage for Irrigation (4 stages)
!
LOGICAL, POINTER,DIMENSION(:,:)     :: LIRRIGATE 
                                        ! True if irrigation performed
!
LOGICAL, POINTER,DIMENSION(:,:)     :: LIRRIDAY 
                                        ! True if irrigation occurs during present day
!                                          
REAL, POINTER, DIMENSION(:,:)       :: XTHRESHOLDSPT 
                                        ! Spatialized threshold

END TYPE AGRI_t
!-------------------------------------------------------------------------------



 CONTAINS

!
!


                                        

SUBROUTINE AGRI_INIT(YAGRI)
TYPE(AGRI_t), INTENT(INOUT) :: YAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YAGRI%NIRRINUM)
  NULLIFY(YAGRI%LIRRIGATE)
  NULLIFY(YAGRI%LIRRIDAY)
  NULLIFY(YAGRI%XTHRESHOLDSPT)
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE AGRI_INIT

!-------------------------------------------------------------------------------
!
END MODULE MODD_AGRI_n
