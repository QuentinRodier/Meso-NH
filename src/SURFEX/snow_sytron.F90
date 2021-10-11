!     ##########################################################################

        SUBROUTINE SNOW_SYTRON(PTSTEP,PPS,PTA,PQA,PVMOD,PVDIR,PSLOPEDIR,PDIRCOSZW,   &
                        PSNOWHEAT,PSNOWSWE,PSNOWRHO,                                 &
                        PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE,KTAB_SYT,PBLOWSNW,  &
                        PSYTMASS)


!     ##########################################################################
!
!!****  *SNOWSYTRON*
!!
!!    PURPOSE
!!    -------
!
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
!!
!!
!!    REFERENCE
!!    ---------

!!
!!    AUTHOR
!!    ------
!!    
!!    V .Vionnet      * Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/14
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,    ONLY : XPI
USE MODD_SYTRON_PAR

USE MODE_THERMOS
USE MODE_SNOW3L
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS, PTA, PQA, PVMOD, PVDIR
!                                      PTA     = atmospheric temperature at
!                                      level ZREF (K)
!                                      PVMOD   = modulus of the wind parallel to
!                                      the orography (m/s)
!                                      PVDIR   = wind direction (rad)
!                                      PPS     = surface pressure
!                                      PQA     = atmospheric specific humidity
!                                                at level ZREF

REAL, DIMENSION(:), INTENT(IN)      :: PSLOPEDIR  ! slope direction (deg)
REAL, DIMENSION(:), INTENT(IN)      :: PDIRCOSZW  ! Cosinus of the angle between the 
!                                                  normal to the surface and the vertical
INTEGER , DIMENSION(:), INTENT(IN)   ::  KTAB_SYT    ! Array of index defining
!                                                  opposite points for Sytron
REAL, DIMENSION(:,:), INTENT(INOUT)    :: PSNOWHEAT,PSNOWSWE,PSNOWRHO
!                                      PSNOWHEAT = Snow layer(s) heat content     (J/m3)
!                                      PSNOWRHO  = Snow layer(s) averaged density (kg/m3)
!                                      PSNOWSWE  = Snow layer(s) Snow Water Equivalent (SWE:kg m-2)

REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1,PSNOWGRAN2,       &
                                         PSNOWHIST,PSNOWAGE
!                                      PSNOWGRAN1 = Snow layer(s) grain parameter 1
!                                      PSNOWGRAN2 = Snow layer(s) grain parameter 2
!                                      PSNOWHIST  = Snow layer(s) grain historical parameter
!                                      PSNOWAGE   = Snow layer(s) age

REAL, DIMENSION(:,:), INTENT(OUT)    :: PBLOWSNW   ! Properties of deposited blowing snow
                                      !    1 : Deposition flux (kg/m2/s)
                                      !    2 : Density of deposited snow (kg/m3)
                                      !    3 : SGRA1 of deposited snow
                                      !    4 : SGRA2 of deposited snow

REAL, DIMENSION(:), INTENT(OUT)    :: PSYTMASS   ! Erosion/deposition flux (kg/m2/s)
                                                 ! (for diagnostic only)

!*      0.2    declarations of local variables
!
INTEGER JJ,JWRK     ! loop coutners
INTEGER, DIMENSION(SIZE(PTA))       :: INLVLS_USE ! varying number of effective layers 
INTEGER, DIMENSION(SIZE(PTA))       :: INLVLS_USE_TMP ! varying number of effective layers during erosion 

REAL, DIMENSION(SIZE(PVMOD))          :: ZVMOD_SYT ! wind speed used for Sytron
                                                   ! taken as the normal component to the slope direction (m/s)
REAL, DIMENSION(SIZE(PVMOD))          :: ZVDIR_DEG ! wind direction  (deg)
REAL, DIMENSION(SIZE(PVMOD))          :: ZSLOPE    ! Slope angle  (deg)

REAL, DIMENSION(SIZE(PVMOD))          :: ZSNOW  ! Snow depth  (m)

REAL, DIMENSION(SIZE(PVMOD))          :: ZREHU  ! Relative humidity  (%)
REAL, DIMENSION(SIZE(PVMOD))          :: ZQSAT  ! Specific humidity at saturation  (kg/kg)

REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2))       :: ZSNOWHEAT ! Heat content in J/m2
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2))       :: ZSNOWDZ   ! Snow layer thickness (m)

REAL ZCOSDIFF

REAL ZSNOWMOB,ZSNOWDRIFT       
REAL ZRSALT, ZRSUSP, ZRSUBL
REAL ZGRAN1,ZGRAN2,ZRHO,ZHIST
REAL ZDD,ZN,ZSS,ZST
REAL ZSUBL,ZSUSP,ZSALT,ZTT,ZZ
REAL Z31,Z32,ZV33,ZV44
REAL ZSNOW_ERO

REAL, DIMENSION(SIZE(PVMOD)) :: ZTR,ZHT,ZHD,ZRHOD,ZRHOT,ZHTE,ZHDE,ZGRAN1F,ZGRAN2F
REAL, DIMENSION(SIZE(PVMOD)) :: ZHTE_FIN,ZHDE_FIN
LOGICAL,DIMENSION(SIZE(PVMOD)) :: OSTOP_ERO
! - - ---------------------------------------------------
!
!       0.3     Initialization

ZTR(:) = 0.
ZHT(:) = 0.
ZHD(:) = 0.
ZRHOD(:) = 0.
ZRHOT(:) = 0.
ZHTE(:) = 0.
ZHDE(:) = 0.
ZHTE_FIN(:) = 0.
ZHDE_FIN(:) = 0.
ZGRAN1F(:) = 0.
ZGRAN2F(:) = 0.

OSTOP_ERO=.FALSE.

! convert wind direction in deg
ZVDIR_DEG(:) = PVDIR(:)*180./XPI
WHERE(ZVDIR_DEG<0.) ZVDIR_DEG(:)=ZVDIR_DEG(:)+360. 

! convert heat content from J/m3 into J/m2
WHERE(PSNOWSWE(:,:)>0.) &      
  ZSNOWHEAT(:,:) = PSNOWHEAT(:,:) / PSNOWRHO (:,:) * PSNOWSWE (:,:)

! Compute snow depth
ZSNOW = 0.
INLVLS_USE(:) = 0
DO JWRK=1,SIZE(PSNOWSWE,2)
    DO JJ=1,SIZE(PSNOWSWE,1)
      ZSNOWDZ(JJ,JWRK) = PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK) 
      ZSNOW(JJ) = ZSNOW(JJ) + ZSNOWDZ(JJ,JWRK)
      IF(PSNOWSWE(JJ,JWRK)>0.) INLVLS_USE(JJ) = JWRK   
   ENDDO
ENDDO
INLVLS_USE_TMP(:) = INLVLS_USE(:)

! Compute slope angle
ZSLOPE = ACOS(PDIRCOSZW)

! Compute relative humidity 
ZQSAT(:) = QSAT(PTA(:),PPS(:))
ZREHU(:) = MIN(100.,100*PQA(:)/ZQSAT(:)*(0.622+0.378*ZQSAT(:))/& 
                        (0.622+0.378*PQA(:)))

!!WRITE(*,*) 'On entre ds sytron'
!!WRITE(*,*) 'SYT',KTAB_SYT(:)
!!WRITE(*,*) 'WINDDIR',ZVDIR_DEG(:)
!!WRITE(*,*) 'Slope orient',PSLOPEDIR(:)
!!WRITE(*,*) 'HTN',ZSNOW(:)


!!
!*       1.     Wind speed used for Sytron  : 
!       Compute normal component of the wind speed according to the windward 
!       exposure.
!               ----------------
!
ZVMOD_SYT(:) = 0.

DO JJ = 1,SIZE(PVMOD)
      IF(ABS(ZVDIR_DEG(JJ)-PSLOPEDIR(JJ))<=XANGLE_LIM) THEN
           ZVMOD_SYT(JJ)=PVMOD(JJ)
      ELSE
           ZCOSDIFF = COS((ABS(ZVDIR_DEG(JJ)-PSLOPEDIR(JJ))-XANGLE_LIM)*XPI/180.)
           IF(ZCOSDIFF>0.) ZVMOD_SYT(JJ) = PVMOD(JJ)*ZCOSDIFF   
      ENDIF
ENDDO

!!WRITE(*,*) 'WIND IN',PVMOD(:)
!!WRITE(*,*) 'WIND SYT',ZVMOD_SYT(:)

!
!*       2.     Compute mass of eroded and deposited snow
!               -------------------
!
DO JWRK = 1,SIZE(PSNOWRHO,2)   ! Loop over snow layers
    DO JJ = 1, SIZE(PVMOD)     ! Loop over grid points
       
     IF(JWRK<=INLVLS_USE(JJ) .AND. ZSNOW(JJ)> 0. .AND. (.NOT. OSTOP_ERO(JJ)) .AND. (JJ.NE.KTAB_SYT(JJ))) THEN
       
       ! Determine occurence of snow transport using PROTEON
       CALL SYVPROT(ZVMOD_SYT(JJ),PSNOWGRAN1(JJ,JWRK),PSNOWGRAN2(JJ,JWRK),PSNOWHIST(JJ,JWRK),   &
                 ZSNOWMOB,ZSNOWDRIFT)
       !!WRITE(*,*) 'Point',JJ,'Layer',JWRK,'Mob',ZSNOWMOB,'Drift', ZSNOWDRIFT
       IF(ZSNOWDRIFT<=0.) THEN
          OSTOP_ERO(JJ)=.TRUE.
       ELSE 
          ! Compute rate of erosion due to saltation and turbulent suspension
          ! and loss due to sublimation 
          CALL SYVTAUX(ZSNOWMOB,ZVMOD_SYT(JJ),ZREHU(JJ),PTA(JJ),ZRSALT,    &
                                       ZRSUSP,ZRSUBL) 
          ZSS = (ZRSALT + ZRSUSP) * PTSTEP ! Potiential height of snow removed
                                           !from this layer that can be deposited
          ZST = ZSS + ZRSUBL*PTSTEP     ! Total potential height of snow removed from
                                       ! this layer (including sublimation)
          !!WRITE(*,*) 'ZSS',ZSS,'ZST',ZST
          !
          !  Compute characteristics of transported snow grains from eroded snow
          !  including fragmentation
          CALL SYVGRAI(PTSTEP,XV1*ZVMOD_SYT(JJ),PSNOWGRAN1(JJ,JWRK),PSNOWGRAN2(JJ,JWRK),ZGRAN1,ZGRAN2)
! 
          ! Compute density of transported snow including compaction effet due
          ! to the transport
          CALL SYVTASS(PTSTEP,ZSNOWDRIFT,XV2*ZVMOD_SYT(JJ),PSNOWRHO(JJ,JWRK),ZRHO)
          !
          ! Maximal height of snow removed from this layer
          ZTR(JJ) = MAX(ZTR(JJ), ZST)
          ! Height of snow which is eroded from this layer and transported
          ZZ = MIN(ZSS, MAX(0.,ZSNOWDZ(JJ,JWRK) - ZRSUBL*PTSTEP))
          ! Total eroded height (including sublimation)
          ZN = ZZ + ZRSUBL*PTSTEP + ZHT(JJ)
          !!WRITE(*,*) 'ZSNOWDZ',ZSNOWDZ(JJ,JWRK)
          !!WRITE(*,*) 'ZTR',ZTR(JJ),'ZZ',ZZ,'ZN',ZN
          !
          IF(JWRK==1) THEN
             ZGRAN1F(JJ) = ZGRAN1
             ZGRAN2F(JJ) = ZGRAN2
             ZRHOT(JJ) = PSNOWRHO(JJ,JWRK)
             ZRHOD(JJ) = ZRHO
         ELSE
            ZRHOT(JJ) = MAX(ZRHOT(JJ),XRHOMIN)
            ZRHOD(JJ) = MAX(ZRHOD(JJ),XRHOMIN)
            !ZRHOT(JJ) = (PSNOWRHO(JJ,JWRK)*ZZ +ZRHOT(JJ)*ZHTE(JJ))/MAX(XRHMI,ZZ+ZHTE(JJ)) 
            ZRHOT(JJ) = (PSNOWRHO(JJ,JWRK)*ZZ +ZRHOT(JJ)*ZHTE(JJ))/(ZZ+ZHTE(JJ)) 
            !ZRHOD(JJ) = (ZRHO*ZZ + ZRHOD(JJ)*ZHTE(JJ))/MAX(XRHMI,ZZ+ZHTE(JJ))  
            ZRHOD(JJ) = (ZRHO*ZZ + ZRHOD(JJ)*ZHTE(JJ))/(ZZ+ZHTE(JJ))  
          ! Aggregate snow grains
            CALL SYVAGRE(ZGRAN1F(JJ),ZGRAN2F(JJ),ZGRAN1,ZGRAN2,ZGRAN1F(JJ),ZGRAN2F(JJ),ZHTE(JJ),ZZ)
          ! 
          ENDIF
          !
          ZRHOT(JJ) = MAX(ZRHOT(JJ),XRHOMIN)
          ZRHOD(JJ) = MAX(ZRHOD(JJ),XRHOMIN)
          !
          ! Amount of deposited snow
          ZDD = MIN(ZTR(JJ)-MAX(0.,ZHT(JJ)-ZHD(JJ)), ZHD(JJ)+ZZ*MIN(1., PSNOWRHO(JJ,JWRK)/ZRHO))
          ZDD = MAX(0., ZDD-ZHD(JJ))
          ZHD(JJ) = ZHD(JJ)+ZDD
          !!WRITE(*,*) 'ZDD',ZDD,'ZHD',ZHD(JJ)
          !
          ! Amount of eroded snow
          ZTT = MAX(0., MIN(ZN,ZTR(JJ))-ZHT(JJ))
          ZHT(JJ) = ZN
          !!WRITE(*,*) 'ZTT',ZTT,'ZHT',ZHT(JJ)

          ! Assumption: snow flux enters the eroded side
          ! Factor to compute reduced wind speed
          ZV33 = MAX(0.35, MIN(0.99, 1./(1.+XV3*XV5*SIN(ZSLOPE(JJ)))))
          ! Determine occurence of snow transport using PROTEON
          CALL SYVPROT(ZV33*ZVMOD_SYT(JJ),PSNOWGRAN1(JJ,JWRK),PSNOWGRAN2(JJ,JWRK), &
                      PSNOWHIST(JJ,JWRK),ZSNOWMOB,ZSNOWDRIFT)
          !!WRITE(*,*) 'ZV33',ZV33,'Drift',ZSNOWDRIFT
          IF(ZSNOWDRIFT>0.) THEN
               CALL SYVTAUX(ZSNOWMOB,ZV33*ZVMOD_SYT(JJ),ZREHU(JJ),PTA(JJ),ZRSALT,&
                                       ZRSUSP,ZRSUBL) 
               Z31 = MIN((ZSALT+ZSUSP)*PTSTEP,MAX(0.,ZSNOWDZ(JJ,JWRK)-ZSUBL*PTSTEP))
          ELSE
               Z31 = 0.
          ENDIF
          ! Modif.VV 20151125 to remove snow fluxes entering the eroded area 
          Z31 = 0.
          ! Reduce total amount of eroded snow accounting for snow fluxes
          ! entering the eroded area
          ZHTE(JJ) = ZHTE(JJ) + MAX(0., ZTT-Z31)
          !
          ! Assumption: snow flux leaves the deposited side
          ! Factor to compute reduced wind speed
          ZV44 = MAX(0.35, MIN(0.99, 1./(1.+XV4*XV5*SIN(ZSLOPE(KTAB_SYT(JJ))))))
          ZHIST=0.
          CALL SYVPROT(ZV44*ZVMOD_SYT(JJ),ZGRAN1F(JJ),ZGRAN2F(JJ),ZHIST,ZSNOWMOB,  &
                        ZSNOWDRIFT)
          IF(ZSNOWDRIFT>0.) THEN
               CALL SYVTAUX(1.1*ZSNOWMOB,ZV44*ZVMOD_SYT(JJ),ZREHU(JJ),PTA(JJ),ZRSALT,&
                                       ZRSUSP,ZRSUBL)
               Z32 = MIN(ZSNOWDZ(JJ,JWRK),(ZRSALT+ZRSUSP+ZRSALT)*PTSTEP)
          ELSE
               Z32 = 0.
          ENDIF
          ! Modif VV 20151125 to remove snow fluxes leaving the accumulated area
          Z32=0.
          ! Reduce total amount of accumulated snow accounting for snow fluxes
          ! leaving the accumulated area
          ZHDE(JJ) = ZHDE(JJ) + MAX(0., ZDD-Z32)

          ! Computation of snow erosion/deposition is over when 
          !
          IF( (JWRK /=INLVLS_USE(JJ) .AND. ZHT(JJ)>ZTR(JJ)) .OR. (ZST<=ZSNOWDZ(JJ,JWRK))) THEN
              ZHT(JJ)  = MIN(ZHT(JJ),ZTR(JJ))
              ZHTE(JJ) = MIN(ZHTE(JJ),ZTR(JJ))
              OSTOP_ERO(JJ)=.TRUE.
          ENDIF

        ENDIF 

     ENDIF  

   ENDDO! JJ 

ENDDO !JWRK

ZHTE_FIN(:)=ZHTE(:)
ZHDE_FIN(:)=ZHDE(:)
!
!*       3.     Compute snow erosion where it occurs.
!               -------------------
!

DO JWRK = 1,SIZE(PSNOWRHO,2)
    DO JJ = 1, SIZE(PVMOD)
        IF(JWRK<=INLVLS_USE(JJ) .AND. ZHTE(JJ)> 0.) THEN
           ! Amount of snow removed from the top layer
           ZSNOW_ERO = MIN(ZSNOWDZ(JJ,1),ZHTE(JJ))
     

           CALL SYVERO(ZSNOW_ERO, ZSNOWHEAT(JJ,:), ZSNOWDZ(JJ,:), PSNOWSWE(JJ,:),  &
                    PSNOWRHO(JJ,:), PSNOWGRAN1(JJ,:), PSNOWGRAN2(JJ,:),            &
                    PSNOWHIST(JJ,:),PSNOWAGE(JJ,:), INLVLS_USE_TMP(JJ))

           ZHTE(JJ) = ZHTE(JJ)-ZSNOW_ERO ! Amount of snow that still need to be
                                         ! eroded from the snowpack
      ENDIF
    ENDDO

ENDDO
! 
! conversion of snow heat from J/m2 into J/m3
WHERE(PSNOWSWE(:,:)>0.) &
    PSNOWHEAT(:,:) = ZSNOWHEAT(:,:)* PSNOWRHO (:,:)  / PSNOWSWE (:,:)
!
!
!*       4.    Get properties of deposited snow sent to Crocus
!               -------------------
!
DO  JJ = 1, SIZE(PVMOD)

   PBLOWSNW(KTAB_SYT(JJ),1) = ZHDE(JJ)/PTSTEP*ZRHOD(JJ)              ! deposition flux (kg/m2/s)
   PBLOWSNW(KTAB_SYT(JJ),2) = ZRHOD(JJ)              ! density of deposited snow (kg/m3)
   PBLOWSNW(KTAB_SYT(JJ),3) = ZGRAN1F(JJ)
   PBLOWSNW(KTAB_SYT(JJ),4) = ZGRAN2F(JJ)

ENDDO

PSYTMASS(:)=0.

DO  JJ = 1, SIZE(PVMOD)
  IF(ZHTE_FIN(JJ)>0.) THEN 
    PSYTMASS(JJ) = -ZHTE_FIN(JJ)/PTSTEP*ZRHOT(JJ)      ! Erosion flux (negative)
    PSYTMASS(KTAB_SYT(JJ)) = ZHDE_FIN(JJ)/PTSTEP*ZRHOD(JJ) ! Deposition flux (positive)
  ENDIF
ENDDO

CONTAINS

SUBROUTINE SYVPROT(PVMOD,PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWMOB,PSNOWDRIFT)
!
!!    PURPOSE
!!    -------
!     Get snow mobility index and snow dritability index from crystal
!     characteristics and wind speed
!
!!    AUTHOR
!!    ------
!!    Y. Durand   * Meteo-France *   Original version  
!!    V. Vionnet  * Meteo-France *   Implementation in SURFEX
!!
!!
USE MODD_SNOW_METAMO
!   
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)      :: PVMOD
REAL, INTENT(IN)      :: PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST

REAL, INTENT(OUT)     :: PSNOWMOB,PSNOWDRIFT
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SYVPROT',0,ZHOOK_HANDLE)
!
!      1. Initialization:
! ------------------
!
PSNOWMOB=0.
!
!      2. Compute mobility index:
! ------------------
!
IF(PSNOWGRAN1<0) THEN   ! Dendritic snow
      PSNOWMOB = 0.5 + 0.75*(-PSNOWGRAN1/XVGRAN1) - 0.5*PSNOWGRAN2/XVGRAN1
ELSE                    ! Non dendritic snow
      PSNOWMOB = 0.833 - 0.833*PSNOWGRAN1/XVGRAN1 - 0.583*PSNOWGRAN2*1000.
ENDIF

! Decrease mobility index if snow layer has been wet (liquid water inside)
!  but without faceted crystal
IF(PSNOWHIST>=2) THEN
    PSNOWMOB = MIN(PSNOWMOB,-0.0583)
ENDIF
!
!      3. Compute driftability index
! ------------------
!
PSNOWDRIFT= PSNOWMOB-(2.868*EXP(-0.085*PVMOD)-1.)
!
END SUBROUTINE SYVPROT

SUBROUTINE SYVTAUX(PSNOWMOB,PVMOD,PTA,PREHU,PRSALT,PRSUSP,PRSUBL)
!
!!    PURPOSE
!!    -------
!!    Get erosion rate (in m/s) of a snow layer with a given mobility index
!!    and a given wind speed
!!
!!    METHOD
!!    -------
!!    Compute contibution of saltation and turbulent suspension. 
!!    Computation for fresh snow (d=1, s=0.5) and weighted by the mobility index
!!    for other type of snow
!!    
!!    AUTHOR
!!    ------
!!    Y. Durand   * Meteo-France *   Original version  
!!    V. Vionnet  * Meteo-France *   Implementation in SURFEX
!!
!!
USE MODD_SNOW_METAMO
!   
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)      :: PVMOD,PTA,PREHU
REAL, INTENT(IN)      :: PSNOWMOB

REAL, INTENT(OUT)     :: PRSALT,PRSUSP,PRSUBL
!
!*      0.2    declarations of local variables
!

REAL                  :: ZVMOD,ZMOGR,ZZ
REAL                  :: ZCT,ZCH,ZSUBL
!
!      1. Computation of contributions of erosion rates
! ------------------
!
IF(PVMOD<XVMIN) THEN
  PRSALT = 0.
  PRSUBL = 0.
  PRSUSP = 0. 
ELSE
  
  ZVMOD = MIN(PVMOD,XVMAX)
! 
! Weigthing factor since the computation is made for fresh snow 
!
  ZMOGR = MAX(0.1,MIN(1.,(PSNOWMOB+1.)/2.))
!
! Weigthed erosion rate due to saltation
!
  PRSALT = ZMOGR * XPOSALT * XCOSALT * ZVMOD ! in cm/h
  PRSALT = PRSALT * 1E-5/3.6   ! Conversion from cm/h to  m/s
!
! Weigthed erosion rate due to turbulent suspension 
!
  ZZ = XCOSUSP * ZVMOD**XPUSUSP
  PRSUSP = ZMOGR * (1.-XPOSALT) * ZZ  ! in cm/h
  PRSUSP = PRSUSP * 1E-5/3.6   ! Conversion from cm/h to m/s
!
! Weigthed rate loss by sublimation 
! 
  IF(PVMOD< XVMINS) THEN
     ZSUBL = 0.
  ELSE
    ! Sublimation rate for a reference temperature and humidity
    ZSUBL = XCOSUBL * MIN(PVMOD,XVMAXS) 
    ! 
    ! Factor accounting for effects of humidity
    ZCH = (100. - MIN(100.,MAX(25.,PREHU))) / (100. - XHREF)
    ! 
    ! Factor accounting for effects of temperature
    ZCT = MIN(300.,MAX(190.,PTA)) / XTREF
    ! 
    ! Sublimation factor combining both effects
    ZSUBL = MAX(XCMIN,MIN(XCMAX,ZCT*ZCH)) * ZSUBL
  ENDIF
  PRSUBL = ZMOGR * (1.-XPOSALT) * MIN(ZZ,ZSUBL)
  PRSUBL = PRSUBL * 1E-5/3.6   ! Conversion from cm/h to m/s
ENDIF

END SUBROUTINE SYVTAUX

SUBROUTINE SYVGRAI(PTSTEP,PVMOD,PSNOWGRAN1,PSNOWGRAN2,PSNOWGRAN1_O,PSNOWGRAN2_O)
!
!!    PURPOSE
!!    -------
!!    Modification of snow grain characteristics for transported snow 
!!
!!    METHOD
!!    -------
!!    We generalize the methode used in CROCUS for fresh snow falling with wind.
!!    For dendritic snow, snow grain are transformed towards the lower right
!!    corner of the triangle. For non-dendritic snow snow grain are transformed
!!    towards the upper right corner of the square.
!!    
!!    AUTHOR
!!    ------
!!    Y. Durand   * Meteo-France *   Original version  
!!    V. Vionnet  * Meteo-France *   Implementation in SURFEX
!!
USE MODD_SNOW_METAMO
USE MODD_SYTRON_PAR
!   
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)      :: PTSTEP
REAL, INTENT(IN)      :: PVMOD
REAL, INTENT(IN)      :: PSNOWGRAN1,PSNOWGRAN2

REAL, INTENT(OUT)     :: PSNOWGRAN1_O,PSNOWGRAN2_O
!
!*      0.2    declarations of local variables
!
REAL ZSDEN,ZSPHE,ZSPHMA
REAL ZA2,ZB,ZN

!
!*      1.   Assume we have fresh snow in this layer
!
ZSDEN = MAX( MIN( XNDEN1*PVMOD-XNDEN2, XNDEN3 ),-XGRAN )
ZSPHE = MIN( MAX( XNSPH1*PVMOD+XNSPH2, XNSPH3 ),XNSPH4 )

ZSPHMA = (XNSPH3+XNSPH4)/2.
ZSPHE = MIN(ZSPHE,ZSPHMA)
!
!*      2.   Dendritic snow in this layer
!
IF(PSNOWGRAN1<0.) THEN
!
! Distance in the triangle between fresh snow (with this given wind)
! and fresh snow without wind taken as DEND=1, SPHE=0.7 in Sytron
   ZA2 = (ZSDEN+XGRAN)**2.+(ZSPHE-ZSPHMA)**2.
!
! Slope of the straight line between the snow of the current snow layer (SNOWGRAN1,
! SNOWGRAN2) and the lower right corner of the triangle (DEND=0, SPHE=1)
  ZB = (0.-PSNOWGRAN1)/MAX(10.,100. - PSNOWGRAN2)
!    
! A value equal to 0.35 * ZA is added to the sphericity of grains in the current
! snow layer
  PSNOWGRAN2_O = MAX(XNSPH3/2., MIN(ZSPHMA,PSNOWGRAN2 + &
              PTSTEP/3600.*0.35 *SQRT(ZA2/(ZB**2.+1.))))
! 
! Compute dendricity from the new value of SNOWGRAN2
  PSNOWGRAN1_O = MAX(-XGRAN, MIN(XNDEN3,PSNOWGRAN1+ZB*(PSNOWGRAN2_O-PSNOWGRAN2)))  
  
!
!*      3.   Non-dendritic snow in this layer
!
!
ELSE
!
! Distance in the triangle between fresh snow (with this given wind)
! and fresh snow withou wind (taken as DEND=1, SPHE=0.7 in Sytron)
! with a conversion in size
  ZN = (12. - 4.)/100.
  ZA2 = ZN**2.*(ZSDEN+XGRAN)**2.+(ZSPHE-ZSPHMA)**2.
!
! Slope of the straight line between the snow of the current snow layer (SNOWGRAN1,
! SNOWGRAN2) and the upper right corner of the square (SPHE=1, SIZE = 4)
  ZB = (100. - PSNOWGRAN1)/(4. - MAX(4.1, PSNOWGRAN2*10000.))
! A value equal to 0.35 * ZA is added to the size of grains in the current
! snow layer
  PSNOWGRAN2_O = MAX(1., MIN(7.,PSNOWGRAN2*10000. -            & 
              0.35*PTSTEP/3600.*SQRT(ZA2/(ZB**2.+1.))))/10000.
! 
! Compute sphericity from the new value of SNOWGRAN2
   PSNOWGRAN1_O = MAX(XNSPH3/2.,MIN(ZSPHMA,PSNOWGRAN1+ZB*(PSNOWGRAN2_O-PSNOWGRAN2)*10000.))

ENDIF

END SUBROUTINE SYVGRAI

SUBROUTINE SYVTASS(PTSTEP,PSNOWDRIFT,PVMOD,PSNOWRHO,PSNOWRHO_O)
!
!!    PURPOSE
!!    -------
!!    Compaction of transported snow 
!!
!!    METHOD
!!    -------
!!    
!!    AUTHOR
!!    ------
!!    Y. Durand   * Meteo-France *   Original version  
!!    V. Vionnet  * Meteo-France *   Implementation in SURFEX
!!
USE MODD_SNOW_PAR
USE MODD_SYTRON_PAR
!   
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)      :: PTSTEP
REAL, INTENT(IN)      :: PSNOWDRIFT
REAL, INTENT(IN)      :: PVMOD
REAL, INTENT(IN)      :: PSNOWRHO

REAL, INTENT(OUT)     :: PSNOWRHO_O
!
!*      0.2    declarations of local variables
!
REAL :: ZDRHO
!
!*      1.0  Compute new density
!

! Increase of density due to erosion and aggregation
IF(PSNOWRHO < XRHOMAX .AND. PSNOWDRIFT > 0. ) THEN
         ZDRHO = (XRHOMAX - PSNOWRHO) *PTSTEP/XRHOTO * PSNOWDRIFT
ELSE
         ZDRHO = 0.
ENDIF

PSNOWRHO_O = MIN(XRHOMAX, PSNOWRHO+ZDRHO)

!Increase of density due to transport
PSNOWRHO_O = PSNOWRHO_O + XSNOWFALL_C_SN *SQRT(PVMOD)*PTSTEP/3600.
PSNOWRHO_O = MAX(PSNOWRHO_O, XRHOMIN) 


END SUBROUTINE SYVTASS
!
SUBROUTINE SYVERO(PSD_REM,PSNOWHEAT,PSNOWDZ,PSNOWSWE,                    &
                                PSNOWRHO,PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,&
                                PSNOWAGE,INLVLS_USE)
!
USE MODE_SNOW3L
USE MODD_SNOW_PAR
USE MODD_CSTS,     ONLY : XLMTT, XTT, XCI
USE MODD_SNOW_METAMO, ONLY : XUEPSI
!
IMPLICIT NONE
!
!      Erosion of the snowpack top layer.
!      2 options : 
!         - top layer is partially eroded: reduces heat content, SWE and thickness
!         - top layer is totally eroded: snowpack is layering is updated 
!                        
!       0.1 declarations of arguments        
!             
!       
REAL, INTENT(IN) :: PSD_REM   ! Snow depth to be removed
!        from the snow top layer (never exceed top layer thickness)
!
!       Snowpack properties that must be updated
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHEAT,PSNOWDZ,PSNOWSWE,PSNOWRHO, &
                      PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE

INTEGER,INTENT(INOUT)                :: INLVLS_USE
!
!       0.2 declaration of local variables
!
REAL ::ZSNOWHMASS,ZSNOWTEMP
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSCAP
INTEGER :: JLAYER,KNLVLS
!
!       0.3 initialization
!         
KNLVLS = SIZE(PSNOWSWE,1)
!
!       1. Compute erosion
!

IF(ABS(PSD_REM-PSNOWDZ(1))<XUEPSI) THEN ! Surface layer is totally removed.
!   
     DO JLAYER=1,INLVLS_USE-1
          PSNOWSWE(JLAYER)=PSNOWSWE(JLAYER+1)
          PSNOWHEAT(JLAYER)=PSNOWHEAT(JLAYER+1)
          PSNOWDZ(JLAYER)=PSNOWDZ(JLAYER+1)
          PSNOWRHO(JLAYER)=PSNOWRHO(JLAYER+1)
          PSNOWGRAN1(JLAYER)=PSNOWGRAN1(JLAYER+1)
          PSNOWGRAN2(JLAYER)=PSNOWGRAN2(JLAYER+1)
          PSNOWHIST(JLAYER)=PSNOWHIST(JLAYER+1)
          PSNOWAGE(JLAYER)=PSNOWAGE(JLAYER+1)
      ENDDO
!
      PSNOWSWE(INLVLS_USE)=0.0
      PSNOWRHO(INLVLS_USE)=999.
      PSNOWDZ(INLVLS_USE)=0.
      PSNOWGRAN1(INLVLS_USE)=0.
      PSNOWGRAN2(INLVLS_USE)=0.
      PSNOWHIST(INLVLS_USE)=0.
      PSNOWAGE(INLVLS_USE)=0.
      PSNOWHEAT(INLVLS_USE)=0.
      INLVLS_USE=INLVLS_USE-1
!
ELSE  ! Surface layer is partially removed.
!
!       1. Compute snow layer temperature and energy to be removed
!                
      ZSCAP      = SNOW3LSCAP(PSNOWRHO)
      ZSNOWTEMP  = XTT + (PSNOWHEAT(1) +                &
            XLMTT*PSNOWRHO(1)*PSNOWDZ(1))/                    &
            (ZSCAP(1)*MAX(XSNOWDMIN/KNLVLS,PSNOWDZ(1)))
      ZSNOWTEMP  = MIN(XTT, ZSNOWTEMP)
      ZSNOWHMASS = PSD_REM*PSNOWRHO(1)*(XCI*(ZSNOWTEMP-XTT)-XLMTT)
!
!       2.Reduce layer thickness
!                
      PSNOWDZ(1)=PSNOWDZ(1)-PSD_REM
!
!       3.Reduce heat content and SWE
!
      PSNOWHEAT(1) = PSNOWHEAT(1)-ZSNOWHMASS
      PSNOWSWE(1)  = PSNOWDZ(1)*PSNOWRHO(1)
END IF

END SUBROUTINE SYVERO

END SUBROUTINE SNOW_SYTRON
