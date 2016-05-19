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
!!	A. Boone           * Meteo-France *
!!      new version :
!!	B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/04/03
!!      new version :10/08/2011
!!      modif       :   09/2012 soildepth can reach 12m (permafrost)
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
REAL,    DIMENSION(:),     INTENT(IN)    :: PSOILGRID   ! reference soil grid          (m)
REAL,    DIMENSION(:,:),   INTENT(IN)    :: PSOILDEPTH  ! total soil depth             (m)   
REAL,    DIMENSION(:,:,:), INTENT(OUT)   :: PDG         ! depth of base of soil layers (m)
INTEGER, DIMENSION(:,:),   INTENT(OUT)   :: KWG_LAYER   ! last layers for soil moisture
!
!*      0.2    declarations of local variables
!
REAL,DIMENSION(SIZE(PDG,1),SIZE(PDG,3)) :: ZREF
!
REAL               :: ZWORK
!
INTEGER            :: INI,INL,IPATCH
INTEGER            :: JJ,JL,JPATCH
!
LOGICAL            :: LOPTIMGRID
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
IPATCH = SIZE(PDG,3)
!
KWG_LAYER (:,:) = 0
ZREF      (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       1.     Grid configuration
!               ------------------
!
!*       1.1    Check consistency
!               -----------------
!
LOPTIMGRID=.FALSE.
IF(INL==NOPTIMLAYER) THEN
  IF( ALL(PSOILGRID(1:INL)==XOPTIMGRID(1:NOPTIMLAYER)) ) LOPTIMGRID=.TRUE.
ENDIF
!
!*       1.2    Assign soil layer depths if ECOCLIMAP
!               -------------------------------------
!
IF(LOPTIMGRID)THEN
  !
  !Optimized ECOCLIMAP soil grid
  CALL OPTIMSOILGRID
  !
ELSE
  !
  WHERE(PSOILDEPTH(:,:)/=XUNDEF)
    PDG(:,1,:)=MIN(0.01,PSOILGRID(1))
  ELSEWHERE
    PDG(:,1,:)=XUNDEF
  ENDWHERE
  !
  DO JPATCH=1,IPATCH
    DO JJ=1,INI 
      !
      IF( PSOILDEPTH(JJ,JPATCH)==XUNDEF )THEN
        !
        PDG      (JJ,:,JPATCH) = XUNDEF              
        KWG_LAYER(JJ,  JPATCH) = NUNDEF
        !
      ELSE
        !
        DO JL=2,INL
          !
          PDG      (JJ,JL,JPATCH) = PSOILGRID(JL)
          ! 
          IF ( PSOILGRID(JL)-PSOILGRID(JL-1)<=0.3 ) THEN       
            ZWORK = ABS(PSOILGRID(JL)-PSOILDEPTH(JJ,JPATCH))
            IF(ZWORK<=ZREF(JJ,JPATCH))THEN
              KWG_LAYER(JJ,JPATCH) = JL
              ZREF     (JJ,JPATCH) = ZWORK
            ENDIF
          ELSEIF( PSOILDEPTH(JJ,JPATCH)>=(PSOILGRID(JL)*0.6+PSOILGRID(JL-1)*0.4) )THEN
            KWG_LAYER(JJ,JPATCH) = JL
          ENDIF
        ENDDO
        !
      ENDIF
    ENDDO
  ENDDO
  !
ENDIF
!
IF(ANY(KWG_LAYER(:,:)==0))THEN
  CALL ABOR1_SFX('SOILGRID: WITH CISBA=DIF NWG_LAYER MUST BE DEFINED FOR EACH POINT')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SOILGRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE OPTIMSOILGRID
!
IMPLICIT NONE
!
! declarations of local variables
!
INTEGER, PARAMETER                  :: NDLIM = 13
!
REAL, DIMENSION(NDLIM), PARAMETER   :: ZDLIM = &
      (/1.25,1.50,1.75,2.00,2.25,2.50,2.75,3.00,3.50,4.00,4.50,5.00,5.50/)
!
REAL,DIMENSION(SIZE(PDG,1),SIZE(PDG,3)) :: ZDG_WATER
!
LOGICAL            :: LWORK
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! init
!
IF (LHOOK) CALL DR_HOOK('SOILGRID:OPTIMSOILGRID',0,ZHOOK_HANDLE)
!
ZDG_WATER (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!a. Arranged depth
!
DO JPATCH=1,IPATCH
   DO JJ=1,INI
      IF(PSOILDEPTH(JJ,JPATCH)<1.25)THEN
        ZDG_WATER(JJ,JPATCH)=MIN(1.0,PSOILDEPTH(JJ,JPATCH))
      ELSEIF(PSOILDEPTH(JJ,JPATCH)>=5.50.AND.PSOILDEPTH(JJ,JPATCH)<6.50)THEN
              ZDG_WATER(JJ,JPATCH)=5.50
      ELSEIF(PSOILDEPTH(JJ,JPATCH)>=6.50.AND.PSOILDEPTH(JJ,JPATCH)<10.50)THEN
              ZDG_WATER(JJ,JPATCH)=8.00
      ELSEIF(PSOILDEPTH(JJ,JPATCH)>=10.50.AND.PSOILDEPTH(JJ,JPATCH)<XUNDEF)THEN
              ZDG_WATER(JJ,JPATCH)=12.00              
      ELSE
        DO JL=1,NDLIM-1         
           IF(PSOILDEPTH(JJ,JPATCH)>=ZDLIM(JL).AND.PSOILDEPTH(JJ,JPATCH)<ZDLIM(JL+1))THEN
              ZDG_WATER(JJ,JPATCH)=MERGE(ZDLIM(JL),ZDLIM(JL+1),PSOILDEPTH(JJ,JPATCH)<(0.4*ZDLIM(JL)+0.6*ZDLIM(JL+1)))
           ENDIF
        ENDDO
      ENDIF
   ENDDO
ENDDO      
!
!-------------------------------------------------------------------------------
!b. General cases
!
DO JPATCH=1,IPATCH
  DO JJ=1,INI
    !
    IF(PSOILDEPTH(JJ,JPATCH)==XUNDEF)THEN
      !
      PDG      (JJ,:,JPATCH) = XUNDEF      
      KWG_LAYER(JJ,  JPATCH) = NUNDEF    
      !
    ELSE
      !
      PDG(JJ,:,JPATCH) = PSOILGRID(:)
      !      
      LWORK=(ZDG_WATER(JJ,JPATCH)<=1.0.OR.&
             ZDG_WATER(JJ,JPATCH)==1.5.OR.&
             ZDG_WATER(JJ,JPATCH)==2.0.OR.&
             ZDG_WATER(JJ,JPATCH)==3.0.OR.&
             ZDG_WATER(JJ,JPATCH)==5.0.OR.&
             ZDG_WATER(JJ,JPATCH)==8.0.OR.&
             ZDG_WATER(JJ,JPATCH)==12.0   )
      ! 
      IF (LWORK) THEN    
        DO JL=2,INL      
          ZWORK = ABS(PSOILGRID(JL)-ZDG_WATER(JJ,JPATCH))                 
          IF(ZWORK<=ZREF(JJ,JPATCH))THEN
            KWG_LAYER(JJ,JPATCH)=JL
            ZREF(JJ,JPATCH)=ZWORK
          ENDIF
        ENDDO           
      ENDIF
      !
    ENDIF
    !
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!c. Particular cases
!
WHERE (ZDG_WATER(:,:)==1.25)
  KWG_LAYER(:,:) = 9
  PDG(:,9,:) = ZDG_WATER(:,:)
ELSEWHERE (ZDG_WATER(:,:)==1.75 .OR. ZDG_WATER(:,:)==2.25)
  KWG_LAYER(:,:) = 10
  PDG(:,10,:) = ZDG_WATER(:,:)
  WHERE (ZDG_WATER(:,:)==1.75) PDG(:,9,:) = 1.25
ELSEWHERE (ZDG_WATER(:,:)==2.50 .OR. ZDG_WATER(:,:)==2.75 .OR. ZDG_WATER(:,:)==3.50)
  KWG_LAYER(:,:) = 11
  PDG(:,11,:) = ZDG_WATER(:,:)
ELSEWHERE (ZDG_WATER(:,:)==4.00 .OR. ZDG_WATER(:,:)==4.50 .OR. ZDG_WATER(:,:)==5.50)
  KWG_LAYER(:,:) = 12
  PDG(:,12,:) = ZDG_WATER(:,:)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('SOILGRID:OPTIMSOILGRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE OPTIMSOILGRID
!
!-------------------------------------------------------------------------------
END SUBROUTINE SOILGRID
