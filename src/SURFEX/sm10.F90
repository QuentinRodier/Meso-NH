!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE SM10(PZ, PBLD_HEIGHT, PBLD, PL)
!     ###############################################################################
!
!!****  *SM10* computes the shape for the mixing length according to Santiago and Martilli 2010
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010
!!---------------------------------------------------------------
!
!
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ          ! canopy levels        (m)
REAL, DIMENSION(:),   INTENT(IN)  :: PBLD_HEIGHT ! building height      (m)
REAL, DIMENSION(:),   INTENT(IN)  :: PBLD        ! building plane area density (-)
REAL, DIMENSION(:,:), INTENT(OUT) :: PL          ! base profile for mixing length computations  (m)
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZALPHA1 = 2.24 ! Parameter zalpha1 from Santiago and Martilli (2010)
REAL, PARAMETER :: ZALPHA2 = 1.12 ! Parameter zalpha2 from Santiago and Martilli (2010)
!
REAL :: ZD  ! Displacement height (d in Santiago and Martilli, 2010)
REAL :: ZD2 ! Parameter to make mixing length continuous at 1.5 H (d2 in Santiago and Martilli, 2010)
!
INTEGER :: JJ      ! Horizontal loop counter
INTEGER :: JLAYER  ! vertical loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!* Preliminaries:
!
IF (LHOOK) CALL DR_HOOK('SM10',0,ZHOOK_HANDLE)
!
DO JJ = 1, SIZE(PZ,1)
   !
   ! Calculation of the displacement height (d) after Santiago and Martilli (2010)
   !
   ZD = PBLD_HEIGHT(JJ) * (PBLD(JJ))**0.13
   !
   ! Calculation of the d2 parameter required to make the mixing length continuous
   ! at z = 1.5 * PBLD_HEIGHT
   !
   ZD2 = -1.5 * PBLD_HEIGHT(JJ) + 2.0 * ZD
   !
   DO JLAYER = 1, SIZE(PZ,2)
      !
      IF ( PZ(JJ,JLAYER).LE.PBLD_HEIGHT(JJ) ) THEN
         !
         PL(JJ,JLAYER) = ZALPHA1 * ( PBLD_HEIGHT(JJ) - ZD)
         !
      ELSE IF ( (PZ(JJ,JLAYER).GT.PBLD_HEIGHT(JJ)).AND.(PZ(JJ,JLAYER).LE.(1.5*PBLD_HEIGHT(JJ))) ) THEN
         !
         PL(JJ,JLAYER) = ZALPHA1 * ( PZ(JJ,JLAYER) - ZD )
         !
      ELSE IF ( PZ(JJ,JLAYER).GT.(1.5*PBLD_HEIGHT(JJ)) ) THEN     
         !
         PL(JJ,JLAYER) = ZALPHA2 * ( PZ(JJ,JLAYER) - ZD2 )
         !
      ELSE
         CALL ABOR1_SFX("SM10: No rule for calculation of mixing length")
      ENDIF
      !
      ! Check whether mixing length increases with height
      !
      IF (JLAYER.GT.1) THEN
         IF (PL(JJ,JLAYER).LT.PL(JJ,JLAYER-1)) THEN
            CALL ABOR1_SFX("SM10: Urban mixing length decreases with height")
         ENDIF
      ENDIF
      !
   ENDDO
   !
ENDDO
!
! Some checks
!
If (MINVAL(PL).LT. 0.0) THEN
    CALL ABOR1_SFX("SM10: Negative value for urban mixing length")
ENDIF
!
If (MINVAL(PL).LT. MINVAL(PZ)) THEN
    CALL ABOR1_SFX("SM10: Too low value for urban mixing length")
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SM10',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SM10
