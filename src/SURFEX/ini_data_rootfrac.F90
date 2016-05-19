!     #########
      SUBROUTINE INI_DATA_ROOTFRAC( PDG, PROOTDEPTH, PROOT_EXT, PROOT_LIN, PROOTFRAC )

!     ##########################################################################
!
!!****  *INI_DATA_ROOTFRAC*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the soil grid configuration using a reference grid
!     Also compute the root fraction
!         
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone (2000)
!!    Boone et al. (2000)
!!    Habets et al. (2003)
!!    Decharme et al. (2011)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!      new version :
!!	B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/04/03
!!      new version :10/08/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR, ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:,:,:), INTENT(IN) :: PDG         ! depth of base of soil layers (m)
REAL,    DIMENSION(:,:),   INTENT(IN) :: PROOTDEPTH  ! effective root depth         (m)
REAL, DIMENSION(:,:), INTENT(IN)     :: PROOT_EXT
REAL, DIMENSION(:,:), INTENT(IN)     :: PROOT_LIN
!
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PROOTFRAC
!
!*      0.2    declarations of local variables
!
REAL               :: ZLOG1, ZLOG2
REAL               :: ZJACKSON ! Jackson (1996) formulation for cumulative root fraction
REAL               :: ZUNIF    ! linear formulation for cumulative root fraction
!
INTEGER            :: INI,INL,IPATCH
INTEGER            :: JJ,JL,JPATCH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_ROOTFRAC',0,ZHOOK_HANDLE)
!
INI    = SIZE(PDG,1)
INL    = SIZE(PDG,2)
IPATCH = SIZE(PDG,3)
!
!
PROOTFRAC(:,:,:) = XUNDEF
!
DO JPATCH=1,IPATCH
  DO JJ=1,INI
    !
    IF ( PROOTDEPTH(JJ,JPATCH)/=XUNDEF .AND. PROOTDEPTH(JJ,JPATCH)/=0.0 ) THEN 
      !
      DO JL=1,INL                
        ZLOG1    = 100. * LOG(PROOT_EXT(JJ,JPATCH)) * PDG    (JJ,JL,JPATCH)
        ZLOG2    = 100. * LOG(PROOT_EXT(JJ,JPATCH)) * PROOTDEPTH(JJ,JPATCH)
        ZJACKSON = MIN(1.0,(1.0-EXP(ZLOG1))/(1.0-EXP(ZLOG2)))
        ZUNIF    = MIN(1.0,(PDG(JJ,JL,JPATCH)/PROOTDEPTH(JJ,JPATCH))) 
        PROOTFRAC(JJ,JL,JPATCH) =      PROOT_LIN(JJ,JPATCH)  * ZUNIF    &
                                   + (1.0-PROOT_LIN(JJ,JPATCH)) * ZJACKSON
      ENDDO
!       No vegetation case                                    
    ELSE
      PROOTFRAC(JJ,:,JPATCH) = 0.0
    ENDIF
    !
  ENDDO
ENDDO     
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_ROOTFRAC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE INI_DATA_ROOTFRAC
