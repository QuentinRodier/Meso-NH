!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOILGRID(PSOILGRID, PSOILDEPTH, PDG, KWG_LAYER  )

!     ##########################################################################
!
!!****  *SOILGRID*  
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
!!      A. Boone           * Meteo-France *
!!      new version :
!!      B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/04/03
!!      new version :10/08/2011
!!      modif       :   09/2012 soildepth can reach 12m (permafrost)
!!                              bug coef algo
!!    B. Decharme   08/2016  supress adaptative grid because not physical
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR, ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:),   INTENT(IN)    :: PSOILGRID   ! reference soil grid          (m)
REAL,    DIMENSION(:),   INTENT(IN)    :: PSOILDEPTH  ! total soil depth             (m)   
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PDG         ! depth of base of soil layers (m)
INTEGER, DIMENSION(:),   INTENT(OUT)   :: KWG_LAYER   ! last layers for soil moisture
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER     :: ZDIST = 0.3 ! parameter (m)
!
!*      0.3    declarations of local variables
!
REAL,DIMENSION(SIZE(PDG,1)) :: ZREF
!
REAL    :: ZWORK
INTEGER :: INI,INL
INTEGER :: JJ,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
IF (LHOOK) CALL DR_HOOK('SOILGRID',0,ZHOOK_HANDLE)
!
INI    = SIZE(PDG,1)
INL    = SIZE(PDG,2)
!
KWG_LAYER (:) = 0
ZREF      (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       1.     Grid configuration
!               ------------------
!
WHERE(PSOILDEPTH(:)/=XUNDEF)
  PDG(:,1)=MIN(0.01,PSOILGRID(1))
ELSEWHERE
  PDG(:,1)=XUNDEF
ENDWHERE
!
DO JJ=1,INI 
   !
   IF(PSOILDEPTH(JJ)==XUNDEF)THEN
     !
     PDG    (JJ,:) = XUNDEF              
     KWG_LAYER(JJ) = NUNDEF
     !
   ELSE
     !
     DO JL=2,INL
        !
        PDG      (JJ,JL) = PSOILGRID(JL)
        !
        IF(PSOILGRID(JL)-PSOILGRID(JL-1)<=ZDIST)THEN       
          ZWORK = ABS(PSOILGRID(JL)-PSOILDEPTH(JJ))
          IF(ZWORK<=ZREF(JJ))THEN
             KWG_LAYER(JJ) = JL
             ZREF     (JJ) = ZWORK
          ENDIF
        ELSEIF(PSOILDEPTH(JJ)>=(PSOILGRID(JL)*0.3+PSOILGRID(JL-1)*0.7))THEN
          KWG_LAYER(JJ) = JL
        ENDIF
     ENDDO
     !
   ENDIF
ENDDO
!
IF(ANY(KWG_LAYER(:)==0))THEN
  CALL ABOR1_SFX('SOILGRID: WITH CISBA=DIF NWG_LAYER MUST BE DEFINED FOR EACH POINT')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SOILGRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SOILGRID
