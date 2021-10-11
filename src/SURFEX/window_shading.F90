!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
SUBROUTINE WINDOW_SHADING(PSHGC_SH, O_SHADE, PALB_WALL,      &
                          PABS_WIN, PABS_WINSH, PALB_WIN, PTRAN_WIN )
!     #############################################################
!
!
!
!!****  *WINDOW_SHADING*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the radiative properties of the window in case of shading
!     devices active
!         
!     
!!**  METHOD
!!     ------
!!     
!!    apply radiative properties coming from namelist input (SHGC_SH)  
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!   EnergyPlus Engineering Reference V7, 2012, p. 217.
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Bueno           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/10
!!      G. Pigeon   09/12 code reorganization to take into account propertie
!!                  from window
!-------------------------------------------------------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
! Declaration of arguments
!
REAL, DIMENSION(:), INTENT(IN) :: PSHGC_SH  !Window + shading solar heat gain coefficient
REAL, DIMENSION(:,:), INTENT(IN) :: O_SHADE ! Fraction of shading elements closed
REAL, DIMENSION(:), INTENT(IN)  :: PALB_WALL !albedo of the wall     
REAL, DIMENSION(:), INTENT(IN)  :: PABS_WIN  !Window absorptivity
REAL, DIMENSION(:,:), INTENT(OUT) :: PABS_WINSH!Window absorptivity after shading
REAL, DIMENSION(:,:), INTENT(OUT) :: PALB_WIN  !Albedo of the ensemble window + shading
REAL, DIMENSION(:,:), INTENT(INOUT) :: PTRAN_WIN !Window transmitivity
!
! Local variables
!
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZTRAN_WIN_SHADING 
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZABS_WINSH_SHADING
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZALB_WIN_SHADING  
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZTRAN_WIN_NOSHADING 
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZABS_WINSH_NOSHADING
REAL, DIMENSION(SIZE(PALB_WIN,1),SIZE(PALB_WIN,2)) :: ZALB_WIN_NOSHADING  
!
INTEGER         :: JJ
INTEGER         :: JCOMP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WINDOW_SHADING',0,ZHOOK_HANDLE)
!
DO JJ=1,SIZE(O_SHADE,1)
   DO JCOMP=1,SIZE(O_SHADE,2)
      !
      ZTRAN_WIN_SHADING (JJ,JCOMP) = PSHGC_SH (JJ)
      ZABS_WINSH_SHADING(JJ,JCOMP) = ZTRAN_WIN_SHADING(JJ,JCOMP) * PABS_WIN(JJ)
      ZALB_WIN_SHADING  (JJ,JCOMP) = PALB_WALL(JJ)
      !
      ZTRAN_WIN_NOSHADING (JJ,JCOMP) = PTRAN_WIN(JJ,JCOMP)
      ZABS_WINSH_NOSHADING(JJ,JCOMP) = PABS_WIN(JJ)
      ZALB_WIN_NOSHADING  (JJ,JCOMP) = 1. - PABS_WIN(JJ) - PTRAN_WIN(JJ,JCOMP)
      !
      PTRAN_WIN (JJ,JCOMP) = O_SHADE(JJ,JCOMP) * ZTRAN_WIN_SHADING   (JJ,JCOMP) + &
                        (1.0-O_SHADE(JJ,JCOMP))* ZTRAN_WIN_NOSHADING (JJ,JCOMP)
      !
      PABS_WINSH(JJ,JCOMP) = O_SHADE(JJ,JCOMP) * ZABS_WINSH_SHADING   (JJ,JCOMP) + &
                        (1.0-O_SHADE(JJ,JCOMP))* ZABS_WINSH_NOSHADING (JJ,JCOMP)
      !
      PALB_WIN  (JJ,JCOMP) = O_SHADE(JJ,JCOMP) * ZALB_WIN_SHADING   (JJ,JCOMP) + &
                        (1.0-O_SHADE(JJ,JCOMP))* ZALB_WIN_NOSHADING (JJ,JCOMP)
      !
      IF ((PABS_WINSH(JJ,JCOMP) + PTRAN_WIN(JJ,JCOMP) + PALB_WIN(JJ,JCOMP)) .GT. 1.) THEN
         PALB_WIN(JJ,JCOMP) = 1. - PABS_WINSH(JJ,JCOMP) - PTRAN_WIN(JJ,JCOMP)
      ENDIF
      !
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WINDOW_SHADING',1,ZHOOK_HANDLE)
!
END SUBROUTINE WINDOW_SHADING
