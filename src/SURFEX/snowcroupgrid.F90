!     #########
        SUBROUTINE SNOWCROUPGRID(PSNOW, PSNOWDZ, PSNOWDZN, PSNOWRHO,       &
                                   PSNOWHEAT,PSNOWGRAN1, PSNOWGRAN2,         &
                                   PSNOWHIST,PSNOWHMASS,OSNOWFALL, PSNOWBIS, &
                                   PSNOWDZBIS,                               &
                                   PSNOWHEATBIS,PSNOWRHOBIS, PSNOWGRAN1BIS,     &
                                   PSNOWGRAN2BIS,PSNOWHISTBIS,PTSTEP,PSR,    &
                                   PTA,PVMOD                   )  
!
USE MODD_SNOW_PAR, ONLY : XSNOWCRITD
USE MODE_SNOW3L
!
! modifs_EB: transformation de grille uniquement si chute de neige, une couche
! trop petite ou HTN < 3 cm (==>omodifgrid=.T.)
! modifs pour traiter les grains comme les variables d'origine
!
        USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
        USE PARKIND1  ,ONLY : JPRB
!
        IMPLICIT NONE
!
!       0.1 declarations of arguments        
!        
        REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWDZ,PSNOWRHO,      &
                                                   PSNOWDZN, PSNOWHEAT   
!
        REAL, DIMENSION(:,:), INTENT(INOUT)   ::  PSNOWDZBIS,           &
                                  PSNOWHEATBIS,PSNOWRHOBIS, PSNOWGRAN1BIS,   &
                                  PSNOWGRAN2BIS,PSNOWHISTBIS                                     
        REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWGRAN1, PSNOWGRAN2,&
                                                   PSNOWHIST    
!
        REAL, DIMENSION(:), INTENT(INOUT)      :: PSNOW, PSNOWBIS,PSNOWHMASS 
!        
        REAL, DIMENSION(:), INTENT(IN)         :: PSR, PTA, PVMOD
!
        LOGICAL,DIMENSION(:), INTENT(IN)       :: OSNOWFALL 
!
        REAL, INTENT(IN)                       :: PTSTEP 
!
!       0.2 declaration of local variables
!
        REAL, DIMENSION(SIZE(PSNOW))           :: ZSUMHEAT, ZSUMSWE,    &
                                                    ZSNOWMIX_DELTA,       &
                                                    ZSNOWHMASS      
!
        REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) ::ZSNOWHEATN, &
                                                 ZSNOWRHON  
! ajout EB
        REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) ::ZSNOWGRAN1N,   &
                                  ZSNOWGRAN2N,ZSNOWHISTN                                                  
        REAL, DIMENSION(SIZE(PSNOW))           :: ZNDENT, ZNVIEU
!        
        INTEGER                                :: INLVLS
        INTEGER JJ1,JJ2
! ajout EB         
        REAL, DIMENSION(SIZE(PSNOW))           :: PSNOWMIN
        integer JJ3
        logical OMODIFGRID
!
        REAL                                   :: ZTSTEP     
        REAL(KIND=JPRB) :: ZHOOK_HANDLE
!        
!       0.3 initialization
IF (LHOOK) CALL DR_HOOK('SNOWCROUPGRID',0,ZHOOK_HANDLE)
INLVLS = SIZE(PSNOWDZ(:,:),2)
!        
!       1. update grid for snowpack>3cm
!
!
! ajout EB
IF((PSNOWGRAN1(1,3)>80.0).and.(PSNOWGRAN2(1,3)>2.E-2))then
   WRITE(*,*) 'pb1 psnowgran',PSNOWGRAN1(1,3),PSNOWGRAN2(1,3) &
                      , PSNOWRHO(1,3),PSNOWDZ(1,3)  
ENDIF
IF((PSNOWGRAN1BIS(1,3)>80.0).and.(PSNOWGRAN2BIS(1,3)>2.E-2))then
   WRITE(*,*) 'pb1 psnowgranBIS',PSNOWGRAN1BIS(1,3),PSNOWGRAN2BIS(1,3)
ENDIF
!
OMODIFGRID=.FALSE.
!         
DO JJ1=1,SIZE(PSNOW(:))
!
  IF(OSNOWFALL(JJ1).and.OMODIFGRID) THEN
! IF(OSNOWFALL(JJ1)) THEN
    WRITE(*,*) 'ALERTE'
! ajout EB
    OMODIFGRID=.TRUE.
    !
    ZTSTEP=PTSTEP/10.0
    PSNOWHMASS(JJ1)=0     
    DO JJ2=1,10
       PSNOWHMASS(JJ1)=0
       CALL SNOWCROADDSNOW(ZTSTEP,PSR(JJ1),PTA(JJ1),PVMOD(JJ1),&
                PSNOW(JJ1),PSNOWRHO(JJ1,1),PSNOWDZ(JJ1,1),PSNOWHEAT(JJ1,1),&
                ZSNOWHMASS(JJ1),PSNOWGRAN1(JJ1,1),PSNOWGRAN2(JJ1,1),&
                PSNOWHIST(JJ1,1),INLVLS)  
       PSNOWHMASS(JJ1)=PSNOWHMASS(JJ1)+ZSNOWHMASS(JJ1)      
       ZSNOWRHON(JJ1,:)  = PSNOWRHO(JJ1,:)
       ZSNOWHEATN(JJ1,:) = PSNOWHEAT(JJ1,:)      
!ajout EB           
       ZSNOWGRAN1N(JJ1,:) = PSNOWGRAN1(JJ1,:)      
       ZSNOWGRAN2N(JJ1,:) = PSNOWGRAN2(JJ1,:)      
       ZSNOWHISTN(JJ1,:) = PSNOWHIST(JJ1,:)      

       CALL SNOW3LGRID(PSNOWDZN(JJ1,:), PSNOW(JJ1))
       CALL SNOWCROTRANSF_1D(PSNOW(JJ1),PSNOWDZ(JJ1,:),PSNOWDZN(JJ1,:),&
                ZSNOWRHON(JJ1,:),ZSNOWHEATN(JJ1,:),ZSNOWGRAN1N(JJ1,:),     &
                ZSNOWGRAN2N(JJ1,:),ZSNOWHISTN(JJ1,:))            
    END DO
!
  ELSE 
!
    ZSNOWRHON(JJ1,:)  = PSNOWRHOBIS(JJ1,:)
    ZSNOWHEATN(JJ1,:) = PSNOWHEATBIS(JJ1,:)
!ajout EB           
    ZSNOWGRAN1N(JJ1,:) = PSNOWGRAN1BIS(JJ1,:)      
    ZSNOWGRAN2N(JJ1,:) = PSNOWGRAN2BIS(JJ1,:)      
    ZSNOWHISTN(JJ1,:) = PSNOWHISTBIS(JJ1,:)      
! ajout EB         
! on change de grille seulement si il y a une trop petite couche
    PSNOWMIN(JJ1)=PSNOW(JJ1)
!
    DO JJ3=1, INLVLS
      IF (PSNOWDZ(JJ1,JJ3) < PSNOWMIN(JJ1)) PSNOWMIN(JJ1)=PSNOWDZ(JJ1,JJ3)
    ENDDO
!
    IF (PSNOWMIN(JJ1) < MIN(0.001,PSNOW(JJ1)/(2*INLVLS)) &
        .OR.PSR(JJ1) > 0.0) then  
      OMODIFGRID=.TRUE.
!        write (*,*) 'avant recalcul: snowmin=',psnowmin(jj1),'psr=', psr(jj1) 
!       write (*,*) PSNOW(JJ1), PSNOWBIS(JJ1)
!       write (*,*) PSNOWRHO(JJ1,1), PSNOWRHOBIS(JJ1,1)
!       write (*,*) PSNOWHEAT(JJ1,1), PSNOWHEATBIS(JJ1,1)
!       write (*,*) PSNOWDZ(JJ1,1), PSNOWDZBIS(JJ1,1)
      CALL SNOW3LGRID(PSNOWDZN(JJ1,:),PSNOWBIS(JJ1))

      WRITE(*,*) 'psr, psnowmin,psnow', PSR(JJ1), PSNOWMIN(JJ1),PSNOW(JJ1)
!       CALL SNOW3LTRANSF_1D(PSNOWBIS(JJ1),PSNOWDZBIS(JJ1,:),PSNOWDZN(JJ1,:),  &
!                      ZSNOWRHON(JJ1,:),ZSNOWHEATN(JJ1,:),ZSNOWGRAN1N(JJ1,:),  &
!                      ZSNOWGRAN2N(JJ1,:),ZSNOWHISTN(JJ1,:),OSNOW_METAMO)
      CALL SNOWNLTRANSFGRID_1D(PSNOWBIS(JJ1),PSNOWDZBIS(JJ1,:),PSNOWDZN(JJ1,:),  &
                         ZSNOWRHON(JJ1,:),ZSNOWHEATN(JJ1,:),ZSNOWGRAN1N(JJ1,:),  &
                         ZSNOWGRAN2N(JJ1,:),ZSNOWHISTN(JJ1,:))  
!       write(*,*) 'après'
!       write (*,*) PSNOW(JJ1), PSNOWBIS(JJ1)
!       write (*,*) PSNOWRHO(JJ1,1), PSNOWRHOBIS(JJ1,1)
!       write (*,*) PSNOWHEAT(JJ1,1), PSNOWHEATBIS(JJ1,1)
!       write (*,*) PSNOWDZ(JJ1,1), PSNOWDZBIS(JJ1,1)
    ENDIF
!
    PSNOW(JJ1)       = PSNOWBIS(JJ1)  
    PSNOWRHO(JJ1,:)  = PSNOWRHOBIS(JJ1,:)
    PSNOWHEAT(JJ1,:) = PSNOWHEATBIS(JJ1,:) 
    PSNOWDZ(JJ1,:)   = PSNOWDZBIS(JJ1,:)                       
!          
    PSNOWGRAN1(JJ1,:) = PSNOWGRAN1BIS(JJ1,:)
    PSNOWGRAN2(JJ1,:) = PSNOWGRAN2BIS(JJ1,:) 
    PSNOWHIST(JJ1,:)  = PSNOWHISTBIS(JJ1,:)
!
  ENDIF  

  IF((PSNOWGRAN1(1,3)>80.0).AND.(PSNOWGRAN2(1,3)>2.E-2)) THEN
    WRITE(*,*) 'pb2 psnowgran',PSNOWGRAN1(1,3),PSNOWGRAN2(1,3)
  ENDIF
  IF((PSNOWGRAN1BIS(1,3)>80.0).AND.(PSNOWGRAN2BIS(1,3)>2.E-2)) THEN
    WRITE(*,*) 'pb2 psnowgranBIS',PSNOWGRAN1BIS(1,3),PSNOWGRAN2BIS(1,3)
  ENDIF
  IF((ZSNOWGRAN1N(1,3)>80.0).and.(ZSNOWGRAN2N(1,3)>2.E-2)) THEN
    WRITE(*,*) 'pb2 psnowgranN',ZSNOWGRAN1N(1,3),ZSNOWGRAN2N(1,3) &
        , ZSNOWHISTN(1,3),PSNOW(1),inlvls  
  ENDIF
!
END DO       
!
!      2. update grid for thin snowpack<3 cm 
!
!  
ZSUMHEAT(:)       = 0.0
ZSUMSWE(:)        = 0.0
ZSNOWMIX_DELTA(:) = 0.0
ZNDENT(:)         = 0.0
ZNVIEU(:)         = 0.0      
!        
DO JJ1=1, SIZE(PSNOWHEAT,1)
  IF(PSNOW(JJ1)<XSNOWCRITD)THEN
! ajout EB                
    OMODIFGRID=.TRUE.
    DO JJ2=1,INLVLS
      ZSUMHEAT(JJ1)       = ZSUMHEAT(JJ1) + ZSNOWHEATN(JJ1,JJ2)
      ZSUMSWE(JJ1)        = ZSUMSWE(JJ1)  + ZSNOWRHON (JJ1,JJ2)* PSNOWDZ(JJ1,JJ2)                     
      ZSNOWMIX_DELTA(:) = 1.0  
               
      IF(PSNOWGRAN1(JJ1,JJ2)<0)THEN       ! Dendritic snow
         ZNDENT(JJ1)       = ZNDENT(JJ1)+1.0
      ELSE                             ! Non dendritic snow
         ZNVIEU(JJ1)       = ZNVIEU(JJ1)+1.0
      ENDIF
    END DO
  ENDIF
END DO
!
! Average properties for grains : determine which grain type is the most
! important in the snowpack.
!IF(OSNOW_METAMO)THEN
! modifs EB pour changer variables de la subroutine suivante        
! IF((PSNOWGRAN1(1,3)>80.0).and.(PSNOWGRAN2(1,3)>2.E-2))then
!       write(*,*) 'pb2b psnowgran',PSNOWGRAN1(1,3),PSNOWGRAN2(1,3) &
!              , PSNOWHIST(1,3),PSNOW(1),zndent(1),znvieu(1),inlvls
!endif
! ajout EB suppression temporaire de cet appel pour vérif TRANSFGRID 
  CALL SNOW3LAVGRAIN(PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST, &
                      ZSNOWGRAN1N, ZSNOWGRAN2N, ZSNOWHISTN,ZNDENT, ZNVIEU)   
!   IF((ZSNOWGRAN1N(1,3)>80.0).and.(ZSNOWGRAN2N(1,3)>2.E-2))then
!         write(*,*) 'pb2b psnowgranN',ZSNOWGRAN1N(1,3),ZSNOWGRAN2N(1,3) &
!                , ZSNOWHISTN(1,3),PSNOW(1),zndent(1),znvieu(1),inlvls
! endif
!ENDIF
!
!IF(ZSNOWMIX_DELTA(1)>0.)THEN
 !       write(*,*) 'PG1_1',PSNOWGRAN1(1,1),'PG2_1',PSNOWGRAN2(1,1)
!ENDIF
!
DO JJ1=1,INLVLS
   ZSNOWHEATN(:,JJ1) = ZSNOWMIX_DELTA(:)*(ZSUMHEAT(:)/INLVLS)  + &
                        (1.0-ZSNOWMIX_DELTA(:))*ZSNOWHEATN(:,JJ1)  
!
   PSNOWDZN(:,JJ1)   = ZSNOWMIX_DELTA(:)*(PSNOW(:)/INLVLS)     + &
                        (1.0-ZSNOWMIX_DELTA(:))*PSNOWDZN(:,JJ1)  
!
   ZSNOWRHON(:,JJ1)  = ZSNOWMIX_DELTA(:)*(ZSUMSWE(:)/PSNOW(:)) + &
                        (1.0-ZSNOWMIX_DELTA(:))*ZSNOWRHON(:,JJ1)  
ENDDO
!                      
!     3. Update mass (density and thickness) and heat:
!
! ajout EB pour ne faire cet update que pour les couches fines 
  IF((ZSNOWGRAN1N(1,3)>80.0).and.(ZSNOWGRAN2N(1,3)>2.E-2))then
        write(*,*) 'pb3 psnowgran',ZSNOWGRAN1N(1,3),ZSNOWGRAN2N(1,3)
  ENDIF
  IF((PSNOWGRAN1BIS(1,3)>80.0).and.(PSNOWGRAN2BIS(1,3)>2.E-2))then
        write(*,*) 'pb3 psnowgranBIS',PSNOWGRAN1BIS(1,3),PSNOWGRAN2BIS(1,3)
  ENDIF
IF(OMODIFGRID) THEN
! write(*,*) omodifgrid, psnowmin(1), psr(1), zsnowmix_delta(1)        
  PSNOWRHO(:,:)   = ZSNOWRHON(:,:)
  PSNOWDZ(:,:)    = PSNOWDZN(:,:)
  PSNOWHEAT(:,:)  = ZSNOWHEATN(:,:) 

  PSNOWGRAN1(:,:) = ZSNOWGRAN1N(:,:)
  PSNOWGRAN2(:,:) = ZSNOWGRAN2N(:,:) 
  PSNOWHIST(:,:)  = ZSNOWHISTN(:,:)

ENDIF
!
! 
!#############################################################
!#############################################################
!#############################################################
!#############################################################
!
IF (LHOOK) CALL DR_HOOK('SNOWCROUPGRID',1,ZHOOK_HANDLE)
CONTAINS
!
!
        SUBROUTINE SNOWCROADDSNOW(PTSTEP,PSR,PTA,PVMOD,          &
                     PSNOW,PSNOWRHO,PSNOWDZ,PSNOWHEAT,PSNOWHMASS, &
                     PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,KNLVLS)  
!!    PURPOSE
!!    -------
!     Add snow to snowpack 
!     Update mass and heat content of uppermost layer.
!
!
USE MODD_CSTS,     ONLY : XLMTT, XTT, XCI
USE MODD_SNOW_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, XANSMAX,XSNOWCRITD
USE MODD_SNOW_METAMO
!
USE MODE_SNOW3L
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!
REAL, INTENT(IN)                     :: PSR, PTA, PVMOD
!
REAL, INTENT(INOUT)                  :: PSNOW
!
REAL, INTENT (INOUT)                 :: PSNOWRHO, PSNOWDZ, PSNOWHEAT
!
REAL, INTENT(OUT)                    :: PSNOWHMASS
!
REAL, INTENT(INOUT)                   :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!   
INTEGER                               :: KNLVLS  
!
!*      0.2    declarations of local variables
!
INTEGER JJ
!
!
REAL                                :: ZSNOWFALL, ZRHOSNEW,        &
                                         ZSNOW, ZSNOWTEMP,           &
                                         ZSNOWFALL_DELTA, ZSCAP,     &
                                         ZSDEN, ZSPHE, ZDIAMD,ZDIAMV,&
                                         ZDIAMN,ZSPHERD,ZSPHERV,     &
                                         ZSPHERN, ZDENT   
!
! ISBA-ES CROCUS (Pahaut 1976): snowfall density coefficients:
!
REAL, PARAMETER                      :: ZSNOWFALL_A_SN = 109.0  ! kg/m3
REAL, PARAMETER                      :: ZSNOWFALL_B_SN =   6.0  ! kg/(m3 K)
REAL, PARAMETER                      :: ZSNOWFALL_C_SN =  26.0  ! kg/(m7/2 s1/2)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOWCROADDSNOW',0,ZHOOK_HANDLE)
ZRHOSNEW        = XRHOSMIN_ES
ZSNOWFALL       = 0.0
ZSCAP           = 0.0
ZSNOW           = PSNOW
ZSDEN           = 0.0
ZSPHE           = 0.0
!
PSNOWHMASS      = 0.0
!
!
! 1. Add snow into snowpack:
! --------------------------
!
!
! Heat content of newly fallen snow (J/m2):
! NOTE for now we assume the snowfall has
! the temperature of the snow surface upon reaching the snow.
! This is done as opposed to using the air temperature since
! this flux is quite small and has little to no impact
! on the time scales of interest. If we use the above assumption
! then, then the snowfall advective heat flux is zero.
!
ZSNOWTEMP    = XTT
!
ZSCAP         = PSNOWRHO*XCI
ZSNOWTEMP     = XTT + (PSNOWHEAT +                              &
                  XLMTT*PSNOWRHO*PSNOWDZ)/                        &
                  (ZSCAP*MAX(XSNOWDMIN/KNLVLS,PSNOWDZ))  
ZSNOWTEMP     = MIN(XTT, ZSNOWTEMP)
!
!
!
PSNOWHMASS = PSR*(XCI*(ZSNOWTEMP-XTT)-XLMTT)*PTSTEP
!
! Snowfall density: Following CROCUS (Pahaut 1976)
!
ZRHOSNEW     = MAX(XRHOSMIN_ES, ZSNOWFALL_A_SN + ZSNOWFALL_B_SN*(PTA-XTT)+         &
                   ZSNOWFALL_C_SN*SQRT(PVMOD))  
!
ZSDEN      = MAX(MIN(XNDEN1*PVMOD-XNDEN2,XNDEN3),-XGRAN)
ZSPHE      = MIN(MAX(XNSPH1*PVMOD+XNSPH2,XNSPH3),XNSPH4)       
!
! Augment total pack depth:
!
ZSNOWFALL  = PSR*PTSTEP/ZRHOSNEW    ! snowfall thickness (m)
!
!
PSNOW      = PSNOW + ZSNOWFALL
!
! Fresh snowfall changes the snowpack
! density, increases the total liquid water
! equivalent: in uppermost snow layer:
!
    IF(PSNOWGRAN1<0.0)THEN
        PSNOWGRAN1 = (PSNOWDZ*PSNOWRHO*PSNOWGRAN1 + ZSNOWFALL*ZRHOSNEW*ZSDEN)/     &
                      (PSNOWDZ*PSNOWRHO+ZSNOWFALL*ZRHOSNEW)  
        PSNOWGRAN2 = (PSNOWDZ*PSNOWRHO*PSNOWGRAN2 + ZSNOWFALL*ZRHOSNEW*ZSPHE)/     &
                      (PSNOWDZ*PSNOWRHO+ZSNOWFALL*ZRHOSNEW)  
    ELSEIF(PSNOWGRAN1>=0.0)THEN
        ZDIAMD   = -ZSDEN*XDIAET/XGRAN+(1+ZSDEN/XGRAN)*(ZSPHE &
                          *XDIAGF/XGRAN+(1-ZSPHE/XGRAN)*XDIAFP)    
        ZSPHERD  = ZSPHE/XGRAN                
        ZDIAMV   = PSNOWGRAN2
        ZSPHERV  = PSNOWGRAN1/XGRAN
        ZDIAMN   = (ZDIAMD*ZRHOSNEW*ZSNOWFALL+ZDIAMV*PSNOWDZ*PSNOWRHO)/ &
                      (PSNOWDZ*PSNOWRHO+ZSNOWFALL*ZRHOSNEW)  
        ZSPHERN  = (ZSPHERD*ZRHOSNEW*ZSNOWFALL+ZSPHERV*PSNOWDZ*PSNOWRHO)/ &
                      (PSNOWDZ*PSNOWRHO+ZSNOWFALL*ZRHOSNEW)      
        IF(ZDIAMN<ZSPHERN*XDIAGF+(1-ZSPHERN)*XDIAFP)THEN
           ZDENT=(ZDIAMN-(ZSPHERN*XDIAGF+(1-ZSPHERN)*XDIAFP))/ &
                  (XDIAET-(ZSPHERN*XDIAGF+(1-ZSPHERN)*XDIAFP))  
            PSNOWGRAN1 = -XGRAN*ZDENT
            PSNOWGRAN2 =  XGRAN*ZSPHERN  
        ELSE
            PSNOWGRAN1 = XGRAN*ZSPHERN
            PSNOWGRAN2 = ZDIAMN
        ENDIF       
     ENDIF
     PSNOWHIST  = 0.0          
  
   PSNOWRHO   = (PSNOWDZ*PSNOWRHO + ZSNOWFALL*ZRHOSNEW)/     &
                     (PSNOWDZ+ZSNOWFALL)  
   PSNOWDZ    = PSNOWDZ + ZSNOWFALL
!
! Add energy of snowfall to snowpack:
! Update heat content (J/m2) (therefore the snow temperature
! and liquid content):
!
   PSNOWHEAT  = PSNOWHEAT + PSNOWHMASS
IF (LHOOK) CALL DR_HOOK('SNOWCROADDSNOW',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE SNOWCROADDSNOW
!
!
!################################################################################
!################################################################################
!################################################################################
!
!
SUBROUTINE SNOWCROTRANSF_1D(PSNOW,PSNOWDZ,PSNOWDZN,                      &
                                  PSNOWRHO,PSNOWHEAT,PSNOWGRAN1,          &
                                  PSNOWGRAN2, PSNOWHIST)  
!
!!    PURPOSE
!!    -------
!     Snow mass and heat redistibution due to grid thickness
!     configuration resetting. Total mass and heat content
!     of the overall snowpack unchanged/conserved within this routine.
!
!
USE MODD_SNOW_PAR, ONLY : XSNOWCRITD
USE MODD_SNOW_METAMO
USE MODE_SNOW3L
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                  :: PSNOW
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHEAT, PSNOWRHO, PSNOWDZ, &
                                         PSNOWDZN, PSNOWGRAN1, PSNOWGRAN2, &
                                         PSNOWHIST    
!
!*      0.2    declarations of local variables
!
INTEGER :: JJ, KILAYER1, KILAYER2
!
INTEGER :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWRHON,ZSNOWGRAN1,ZSNOWGRAN2,     &
                                         ZSNOWHEATN,ZSNOWHIST                                         
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)-1) :: ZSNOWZO, ZSNOWZN, ZSNOWDDZ, ZDELTA  
! ajout EB
!integer :: jjj
real :: htn_new, htn_old
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOWCROTRANSF_1D',0,ZHOOK_HANDLE)
INLVLS = SIZE(PSNOWDZ(:),1)

ZSNOWGRAN1(:)=PSNOWGRAN1(:)
ZSNOWGRAN2(:)=PSNOWGRAN2(:)
ZSNOWHIST(:)=PSNOWHIST(:)
!ajout EB pour vérifier égalité des HTN

htn_new=0.
htn_old=0.
do jj=1,inlvls
htn_new=htn_new+PSNOWDZ(JJ)
htn_old=htn_old+PSNOWDZN(JJ)
enddo
if (abs(htn_new-htn_old) > 1.E-7) write(*,*) 'difhtn:', htn_new, htn_old
!
!
! 1. Calculate vertical grid depths (m):
! --------------------------------------
!
ZSNOWZO(1)     = PSNOWDZ(1)
ZSNOWZN(1)     = PSNOWDZN(1)
!
DO JJ=2,INLVLS-1
   ZSNOWZO(JJ) = ZSNOWZO(JJ-1) + PSNOWDZ(JJ)
   ZSNOWZN(JJ) = ZSNOWZN(JJ-1) + PSNOWDZN(JJ)
ENDDO
!
! 2. Calculate thickness changes (m):
! -----------------------------------
!
ZSNOWDDZ(:)    = ZSNOWZN(:) - ZSNOWZO(:)
!
! Calculate the delta function:
!
ZDELTA(:)    = 0.0
WHERE(ZSNOWDDZ(:) > 0.0) ZDELTA(:) = 1.0
!
!
! 3. Calculate mass and heat transfers due to grid adjustment/changes:
! --------------------------------------------------------------------
! Do transfers at boundaries first:
! Upper layer:
!
!write(*,*) 'AV_AGREG1', PSNOWGRAN1(1)
IF(ZDELTA(1)==1.0)THEN
        KILAYER1=1
        KILAYER2=2
        CALL SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,    &
                          PSNOWHIST,ZSNOWGRAN1,ZSNOWGRAN2,ZSNOWHIST,            &
                          KILAYER1,KILAYER2,ZSNOWDDZ)  

ENDIF 
!write(*,*) 'AP_AGREG1', PSNOWGRAN1(1)                       
!                        
ZSNOWRHON(1)  = ( PSNOWDZ(1)*PSNOWRHO(1) + ZSNOWDDZ(1)*      &
                    (     ZDELTA(1) *PSNOWRHO(2) +                 &
                     (1.0-ZDELTA(1))*PSNOWRHO(1) ) )               &
                    /PSNOWDZN(1)                  

!
ZSNOWHEATN(1) = PSNOWHEAT(1) + ZSNOWDDZ(1)*                    &
                    ((    ZDELTA(1) *PSNOWHEAT(2)/PSNOWDZ(2)) +  &
                    ((1.0-ZDELTA(1))*PSNOWHEAT(1)/PSNOWDZ(1)) )  
! 
!IF(ZSNOWGRAN1(1)>99.OR.ZSNOWGRAN1(1)<-99.)THEN
!     write(*,*) 'ZG1',ZSNOWGRAN1(1)
!     read(*,*)
!ENDIF           
! Lowest layer:
!
IF(ZDELTA(INLVLS-1)==0.0)THEN
        KILAYER1=INLVLS
        KILAYER2=INLVLS-1
        CALL SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,    &
                          PSNOWHIST,ZSNOWGRAN1,ZSNOWGRAN2,ZSNOWHIST,           &
                          KILAYER1,KILAYER2,ZSNOWDDZ)  
ENDIF 
!
ZSNOWRHON(INLVLS)  = ( PSNOWDZ(INLVLS)*PSNOWRHO(INLVLS) -      &
                           ZSNOWDDZ(INLVLS-1)*                       &
                    (     ZDELTA(INLVLS-1) *PSNOWRHO(INLVLS) +     &
                     (1.0-ZDELTA(INLVLS-1))*PSNOWRHO(INLVLS-1) ) ) &
                    /PSNOWDZN(INLVLS)  
!

ZSNOWHEATN(INLVLS) = PSNOWHEAT(INLVLS) - ZSNOWDDZ(INLVLS-1)*   &
                    ((    ZDELTA(INLVLS-1) *PSNOWHEAT(INLVLS)/     &
                          PSNOWDZ(INLVLS)) +                         &
                    ((1.0-ZDELTA(INLVLS-1))*PSNOWHEAT(INLVLS-1)    &
                          /PSNOWDZ(INLVLS-1)) )  
!
!
! Update interior layer mass and heat :
!
!write(*,*) 'AV_AGREG8', PSNOWGRAN1(8),'ZD7',ZDELTA(7),'ZD9',ZDELTA(9)
DO JJ=2,INLVLS-1
 IF(ZDELTA(JJ-1)==0.0)THEN
         KILAYER1=JJ
         KILAYER2=JJ-1
         CALL SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,    &
                          PSNOWHIST,ZSNOWGRAN1,ZSNOWGRAN2,ZSNOWHIST,             &
                          KILAYER1,KILAYER2,ZSNOWDDZ)  
!           if (zsnowgran1(2) > 900.or. psnowgran1(2)>900.) then
!                   write (*,*) 'agr',PSNOWGRAN1(2),PSNOWGRAN1(2), &
!                   zSNOWGRAN1(2),zSNOWGRAN1(2), KILAYER1,KILAYER2
!                   write(*,*) 'dzold:',(psnowdz(jjj),jjj=1, INLVLS)
!                   write(*,*) 'dznew:',(psnowdzn(jjj),jjj=1, INLVLS)
!                   stop
!           endif
 ENDIF
!
!code initial vincent IF(OSNOW_METAMO.AND.ZDELTA(JJ+1)==1.0)THEN
!code initial vincent         KILAYER1=JJ
!code initial vincent         KILAYER2=JJ+1
!code initial vincent         CALL SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,    &
!code initial vincent                        PSNOWHIST,ZSNOWGRAN1,ZSNOWGRAN2,ZSNOWHIST,            &
!code initial vincent                        KILAYER1,KILAYER2,ZSNOWDDZ)
!code initial vincent ENDIF

!plm
 IF(ZDELTA(JJ)==1.0)THEN
         KILAYER1=JJ-1
         KILAYER2=JJ
         CALL SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,    &
                          PSNOWHIST,ZSNOWGRAN1,ZSNOWGRAN2,ZSNOWHIST,            &
                          KILAYER1,KILAYER2,ZSNOWDDZ)  
 ENDIF
!plm
!
 ZSNOWRHON(JJ)  = ( PSNOWDZ(JJ)*PSNOWRHO(JJ)                            &
                      - ZSNOWDDZ(JJ-1)*                                   &
                    (     ZDELTA(JJ-1) *PSNOWRHO(JJ) +                    &
                     (1.0-ZDELTA(JJ-1))*PSNOWRHO(JJ-1) )                  &
                      + ZSNOWDDZ(JJ)*                                     &
                    (     ZDELTA(JJ)   *PSNOWRHO(JJ+1) +                  &
                     (1.0-ZDELTA(JJ))  *PSNOWRHO(JJ) ) )                  &
                    /PSNOWDZN(JJ)  
                   
!
 ZSNOWHEATN(JJ) = PSNOWHEAT(JJ)                                         &
                      - ZSNOWDDZ(JJ-1)*                                   &
                    ((    ZDELTA(JJ-1) *PSNOWHEAT(JJ)                     &
                          /PSNOWDZ(JJ)) +                                 &
                    ((1.0-ZDELTA(JJ-1))*PSNOWHEAT(JJ-1)                   &
                          /PSNOWDZ(JJ-1)) )                               &
                      + ZSNOWDDZ(JJ)*                                     &
                    ((    ZDELTA(JJ) *PSNOWHEAT(JJ+1)                     &
                          /PSNOWDZ(JJ+1)) +                               &
                    ((1.0-ZDELTA(JJ))*PSNOWHEAT(JJ)                       &
                          /PSNOWDZ(JJ)) )  
ENDDO
!write(*,*) 'AP_AGREG8', PSNOWGRAN1(8)
!
!
! 5. Update mass (density and thickness) and heat:
! ------------------------------------------------
!
PSNOWRHO(:)   = ZSNOWRHON(:)
!PSNOWDZ(:)    = PSNOWDZN(:)
PSNOWHEAT(:)  = ZSNOWHEATN(:)
PSNOWGRAN1(:) = ZSNOWGRAN1(:)
PSNOWGRAN2(:) = ZSNOWGRAN2(:)
PSNOWHIST(:) =  ZSNOWHIST(:)
        IF (LHOOK) CALL DR_HOOK('SNOWCROTRANSF_1D',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
        END SUBROUTINE SNOWCROTRANSF_1D
!################################################################################
!################################################################################
!################################################################################
!
!
SUBROUTINE SNOWNLTRANSFGRID_1D(PSNOW,PSNOWDZ,PSNOWDZN,                      &
                                  PSNOWRHO,PSNOWHEAT,PSNOWGRAN1,          &
                                  PSNOWGRAN2, PSNOWHIST)  
!
!!    PURPOSE
!!    -------
!     Snow mass and heat redistibution due to grid thickness
!     configuration resetting. Total mass and heat content
!     of the overall snowpack unchanged/conserved within this routine.
!
!
USE MODD_SNOW_PAR, ONLY : XSNOWCRITD
USE MODD_SNOW_METAMO
USE MODE_SNOW3L
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                  :: PSNOW
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHEAT, PSNOWRHO, PSNOWDZ, &
                                         PSNOWDZN, PSNOWGRAN1, PSNOWGRAN2, &
                                         PSNOWHIST  
!
!*      0.2    declarations of local variables
!
INTEGER JJ, KILAYER1, KILAYER2
!
INTEGER                             :: INLVLS,INLVLS_OLD, INLVLS_NEW
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWRHON,ZSNOWGRAN1N,ZSNOWGRAN2N,     &
                                         ZSNOWHEATN,ZSNOWHISTN &
            , ZSNOWZTOP_OLD, ZSNOWZTOP_NEW, ZSNOWZBOT_OLD, ZSNOWZBOT_NEW                                        
!
                                       
REAL, PARAMETER :: D1=1., D2=3., D3=4., X=99., COEFALB2=15.4                                      
INTEGER :: JJ_OLD,JJ_NEW
REAL :: ZDENTMOYN ,ZSPHERMOYN, ZALBMOYN, ZMASTOTN, ZSNOWHEAN, &
          ZPROPOR,ZMASDZ_OLD, ZDIAM,ZHISTMOYN  
REAL :: ZPSNOW_OLD, ZPSNOW_NEW
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOWNLTRANSFGRID_1D',0,ZHOOK_HANDLE)
INLVLS = SIZE(PSNOWDZ(:),1)
! a ce stade, les couches restent identiques
INLVLS_OLD=INLVLS
INLVLS_NEW=INLVLS
ZPSNOW_OLD=PSNOW
ZPSNOW_NEW=PSNOW

!
! 1. Calculate vertical grid limits (m):
! --------------------------------------
!
ZSNOWZTOP_OLD(1) = ZPSNOW_OLD
ZSNOWZTOP_NEW(1) = ZPSNOW_NEW
ZSNOWZBOT_OLD(1) = ZSNOWZTOP_OLD(1)-PSNOWDZ(1)
ZSNOWZBOT_NEW(1) = ZSNOWZTOP_NEW(1)-PSNOWDZN(1)

!
DO JJ_OLD=2,INLVLS_OLD
   ZSNOWZTOP_OLD(JJ_OLD) = ZSNOWZBOT_OLD(JJ_OLD-1)
   ZSNOWZBOT_OLD(JJ_OLD) = ZSNOWZTOP_OLD(JJ_OLD)-PSNOWDZ(JJ_OLD)
ENDDO
DO JJ_NEW=2,INLVLS_NEW
   ZSNOWZTOP_NEW(JJ_NEW) = ZSNOWZBOT_NEW(JJ_NEW-1)
   ZSNOWZBOT_NEW(JJ_NEW) = ZSNOWZTOP_NEW(JJ_NEW)-PSNOWDZN(JJ_NEW)
ENDDO
!
!
! 3. Calculate mass and heat transfers due to grid adjustment/changes:
! --------------------------------------------------------------------
!

! on boucle sur les couches de la nouvelle grille et pour chaque couche
! on somme ou on moyenne les quantités des couches totales ou partielles
! des couches anciennes qui la constituent

DO JJ_NEW=1,INLVLS_NEW
    ZSNOWHEAN=0.
    ZMASTOTN=0.

    ZDENTMOYN=0.
    ZSPHERMOYN=0.
    ZALBMOYN=0.
    ZDIAM=0.
    ZHISTMOYN=0.

    ! on balaye les couches anciennes pour identifier celles qui constituent les
! nouvelles et on somme leurs contribs à la nouvelle couche JJ_NEW
    DO JJ_OLD=1, INLVLS_OLD
        IF( ZSNOWZTOP_OLD(JJ_OLD) <= ZSNOWZBOT_NEW(JJ_NEW)) THEN
                ! haut de JJ_O plus bas que bas de JJ_N ==> pas contrib
        ELSEIF ( ZSNOWZBOT_OLD(JJ_OLD) >= ZSNOWZTOP_NEW(JJ_NEW)) THEN
                ! bas de JJ_O plus haut que bas de JJ_N ==> pas contrib
        ELSE
                ! ancienne couche à cheval ou englobant nouvelle
          ZPROPOR= (MIN(ZSNOWZTOP_OLD(JJ_OLD), ZSNOWZTOP_NEW(JJ_NEW))&
                  - MAX(ZSNOWZBOT_OLD(JJ_OLD), ZSNOWZBOT_NEW(JJ_NEW))) &
                  / PSNOWDZ(JJ_OLD)  
          ZMASDZ_OLD= ZPROPOR*PSNOWRHO(JJ_OLD)*PSNOWDZ(JJ_OLD)   
          ZMASTOTN=ZMASTOTN + ZMASDZ_OLD
          ZSNOWHEAN=ZSNOWHEAN+ZPROPOR*PSNOWHEAT(JJ_OLD)

          IF(PSNOWGRAN1(JJ_OLD)<0.) THEN
              ! calcul dimametre optique en 1/10 mmm        
             ZDIAM=-PSNOWGRAN1(JJ_OLD)*D1/X+(1.+PSNOWGRAN1(JJ_OLD)/X)* &
               (PSNOWGRAN2(JJ_OLD)*D2/X+(1.-PSNOWGRAN2(JJ_OLD)/X)*D3)  
             ZDIAM=ZDIAM/10000.      
             ZDENTMOYN= ZDENTMOYN-ZMASDZ_OLD*PSNOWGRAN1(JJ_OLD)/X
             ZSPHERMOYN=ZSPHERMOYN+ZMASDZ_OLD*PSNOWGRAN2(JJ_OLD)/X
           ELSE
             ZDIAM=PSNOWGRAN2(JJ_OLD)
             ZDENTMOYN= ZDENTMOYN+ZMASDZ_OLD*0.
             ZSPHERMOYN=ZSPHERMOYN+ZMASDZ_OLD*PSNOWGRAN1(JJ_OLD)/X
           ENDIF
           ZALBMOYN=ZALBMOYN+MAX(0.,ZMASDZ_OLD*(1.-COEFALB2*SQRT(ZDIAM)))
           ZHISTMOYN=ZHISTMOYN+ZMASDZ_OLD*PSNOWHIST(JJ_OLD)
        ENDIF
    ENDDO 
! on affecte à la nouvelle couche ses propriétés moyennes

     ZSNOWHEATN(JJ_NEW)= ZSNOWHEAN
     ZSNOWRHON(JJ_NEW)= ZMASTOTN/PSNOWDZN(JJ_NEW)
       ZALBMOYN=ZALBMOYN/ZMASTOTN
       ZSPHERMOYN=MAX(0.,ZSPHERMOYN/ZMASTOTN)
       ZDENTMOYN=MAX(0.,ZDENTMOYN/ZMASTOTN)
       ZDIAM=((1.-ZALBMOYN)/COEFALB2)**2
       write(*,*) 'zdiam=', zdiam, zalbmoyn, zdentmoyn, zsphermoyn
!      ZDIAM=MAX(1.E-4, ZDIAM)
       IF (ZDIAM <3.E-4-0.0000001) THEN
!      on préserve dendricite  puis on calcule sphericite               
         ZSNOWGRAN1N(JJ_NEW)=-X*ZDENTMOYN
         IF(ABS(ZSNOWGRAN1N(JJ_NEW)+X)< 0.01) THEN
           ZSNOWGRAN2N(JJ_NEW)=X*ZSPHERMOYN                  
         ELSE
           ZSNOWGRAN2N(JJ_NEW)=X*((ZDIAM*10000.+ZSNOWGRAN1N(JJ_NEW)*D1/X) &
              / (1.+ZSNOWGRAN1N(JJ_NEW)/X)-D3)/(D2-D3)  
           IF (ZSNOWGRAN2N(JJ_NEW)<0.) THEN
             IF(ZSNOWGRAN2N(JJ_NEW)<-0.1) write(*,*) 'pb av1a',ZDIAM,ZDENTMOYN,ZSPHERMOYN, &
                              ZSNOWGRAN1N(JJ_NEW),ZSNOWGRAN2N(JJ_NEW)  
             ZSNOWGRAN2N(JJ_NEW)=0.
           ENDIF  
           IF (ZSNOWGRAN2N(JJ_NEW)>  X + 0.0000001) THEN
             IF(ZSNOWGRAN2N(JJ_NEW)>99.1) write(*,*) 'pb av1b',ZDIAM,ZDENTMOYN,ZSPHERMOYN, &
                        ZSNOWGRAN1N(JJ_NEW),ZSNOWGRAN2N(JJ_NEW)  
             ZSNOWGRAN2N(JJ_NEW)=X
           ENDIF  
         ENDIF
        ELSEIF  (ZDIAM >4.E-4) THEN
          ZSNOWGRAN1N(JJ_NEW)=X*ZSPHERMOYN
          ZSNOWGRAN2N(JJ_NEW)=ZDIAM 
        ELSEIF(ZDENTMOYN<= 0.+0.0000001) THEN
! taille entre 3.E-4 et 4E-4 et dendricite nulle        
          ZSNOWGRAN1N(JJ_NEW)=X*ZSPHERMOYN
          ZSNOWGRAN2N(JJ_NEW)=ZDIAM
        ELSE
! taille entre 3.E-4 et 4E-4 et dendricite  <0   
! on preserve d'abord sphericite. Si impossible on met dendit a 0 et on 
! preserve sphericite
          ZSNOWGRAN1N(JJ_NEW)=-X*ZDENTMOYN
          ZSNOWGRAN2N(JJ_NEW)=X*((ZDIAM*10000.+ZSNOWGRAN1N(JJ_NEW)*D1/X) &
                 / (1.+ZSNOWGRAN1N(JJ_NEW)/X)-D3)/(D2-D3)  
          IF ( ZSNOWGRAN2N(JJ_NEW)<0..OR. ZSNOWGRAN2N(JJ_NEW)> X) THEN
! incompatible avec ZDIAM on met dendicite a 0 et on preserve sphericite 
            IF ( ZSNOWGRAN2N(JJ_NEW)<-0.1.OR. ZSNOWGRAN2N(JJ_NEW)>99.1)  &
                 write(*,*) 'pb av2', ZDIAM, ZDENTMOYN, ZSPHERMOYN, &
              ZSNOWGRAN1N(JJ_NEW),ZSNOWGRAN2N(JJ_NEW)  
  
            ZSNOWGRAN1N(JJ_NEW)=X*ZSPHERMOYN
            ZSNOWGRAN2N(JJ_NEW)=ZDIAM
          ENDIF
        ENDIF
        ZSNOWHISTN(JJ_NEW)=NINT(ZHISTMOYN/ZMASTOTN)
!              ZSNOWGRAN2N(JJ_NEW)=X*ZSPHERMOYN
!              ZSNOWGRAN1N(JJ_NEW)=X*(ZDIAM*10000.-(ZSNOWGRAN2N(JJ_NEW)*D2/X+&
!              (1.-ZSNOWGRAN2N(JJ_NEW)/X)*D3))/&
!              (ZSNOWGRAN2N(JJ_NEW)*D2/X+(1.-ZSNOWGRAN2N(JJ_NEW)/X)*D3)
ENDDO        
!
!verifs
write (*,*) 'verifs chgt grille INLVLS=',INLVLS
    ZSNOWHEAN=0.
    ZMASTOTN=0.

DO JJ=1,INLVLS
ZSNOWHEAN=ZSNOWHEAN+PSNOWHEAT(JJ)- ZSNOWHEATN(JJ)
ZMASTOTN=ZMASTOTN+PSNOWRHO(JJ)*PSNOWDZ(JJ)- ZSNOWRHON(JJ)*PSNOWDZN(JJ)
write(*,*) JJ,'DZ', PSNOWDZ(JJ),PSNOWDZN(JJ)
write (*,*) 'RHO', PSNOWRHO(JJ)   , ZSNOWRHON(JJ)
write (*,*) 'HEAT',PSNOWHEAT(JJ)  , ZSNOWHEATN(JJ)
write (*,*) 'GR1', PSNOWGRAN1(JJ) , ZSNOWGRAN1N(JJ)
write (*,*) 'GR2',PSNOWGRAN2(JJ) , ZSNOWGRAN2N(JJ)
write (*,*) 'HIST',PSNOWHIST(JJ) ,  ZSNOWHISTN(JJ)
ENDDO
write(*,*) 'diff', ZSNOWHEAN,ZMASTOTN



!
! 5. Update mass (density and thickness) and heat:
! ------------------------------------------------
!
PSNOWRHO(:)   = ZSNOWRHON(:)
PSNOWHEAT(:)  = ZSNOWHEATN(:)
PSNOWGRAN1(:) = ZSNOWGRAN1N(:)
PSNOWGRAN2(:) = ZSNOWGRAN2N(:)
PSNOWHIST(:) =  ZSNOWHISTN(:)
        IF (LHOOK) CALL DR_HOOK('SNOWNLTRANSFGRID_1D',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
        END SUBROUTINE SNOWNLTRANSFGRID_1D

!####################################################################
!############################################################################
END SUBROUTINE SNOWCROUPGRID
