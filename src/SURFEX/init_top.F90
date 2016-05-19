!     ######spl
      SUBROUTINE INIT_TOP (HISBA, HTOPREG, KLUOUT, PPATCH, PRUNOFFD, &
                           PDZG, PWWILT, PWSAT, PTI_MIN,             &
                           PTI_MAX, PTI_MEAN, PTI_STD, PTI_SKEW,     &
                           PSOILWGHT, PTAB_FSAT, PTAB_WTOP, PM       )
!
!     #####################################################################
!
!!****  *INIT_TOP*  
!!
!!    PURPOSE
!!    =======
!
!     Calculates the new array of the Datin-Saulnier TOPMODEL framework fonction for xsat and compute each 
!     satured fraction for each xsat value of the grids cells but also the active TOPMODEL-layer array, the 
!     driest fraction array and the normalized mean deficit array.
!     For calculate new array, we use the incomplete gamma function. (see gamma_inc.f for more detail)
! 
!     Note that over land point where topographic index do not exist, a VIC
!     distribution is used with Bcoef at least equal to 0.1. This value can be
!     change in namelist
!     
!-------------------------------------------------------------------------------
!
USE MODD_SURF_PAR,ONLY : XUNDEF
USE MODD_ISBA_n,  ONLY : NPATCH, NGROUND_LAYER, NSIZE_NATURE_P
!
USE MODD_SGH_PAR, ONLY : X2, X4, XREGP, XREGA
!
USE MODI_DGAM
USE MODI_GAMMAS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
!*       0.     DECLARATIONS
!        ===================
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN)         :: HISBA   ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
!
 CHARACTER(LEN=*), INTENT(IN)         :: HTOPREG ! Wolock and McCabe (2000) linear regression for Topmodel
                                                ! 'DEF' = Reg
                                                ! 'NON'
!
INTEGER, INTENT(IN)                  :: KLUOUT
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PPATCH
!
REAL,    DIMENSION(:,:,:),INTENT(IN) :: PDZG     ! soil layer thickness
!
REAL, DIMENSION(:,:,:),INTENT(IN)    :: PSOILWGHT  ! ISBA-DIF: weights for vertical
!                                                  ! integration of soil water and properties
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PRUNOFFD ! depth over which sub-grid runoff is computed
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PWWILT, PWSAT
!                                       PWWILT = the wilting point volumetric 
!                                                water content (m3 m-3)
!                                       PWSAT  = saturation volumetric water content
!                                                of the soil (m3 m-3)
!
REAL,    DIMENSION(:), INTENT(IN)    :: PTI_MIN, PTI_MAX, PTI_STD, &
                                        PTI_SKEW, PTI_MEAN
!                                       PTI_MEAN  = ti mean
!                                       PTI_MIN  = ti min
!                                       PTI_MAX  = ti max
!                                       PTI_STD  = ti standard deviation
!                                       PTI_SKEW = ti skewness
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PTAB_FSAT, PTAB_WTOP
!                                       PTAB_FSAT = Satured fraction array
!                                       PTAB_WTOP = Active TOPMODEL-layer array
!
REAL,    DIMENSION(:), INTENT(INOUT) :: PM
!                                       PM = exponential decay factor of the local deficit
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PM)) :: ZD_TOP, ZWSAT_AVG, ZWWILT_AVG
!                            ZD_TOP  = Topmodel active layer
!
REAL                  :: ZXI, ZPHI, ZNU, ZTI_MEAN, ZTI_MIN, ZTI_MAX, ZTI_STD, ZTI_SKEW
!                        ZTI_MEAN= ti mean after regression
!                        ZXI     = ti pdf parameter
!                        ZPHI    = ti pdf parameter
!                        ZNU     = ti pdf parameter
!
REAL                  :: ZFTOT, ZYMAX, ZYMIN
REAL                  :: ZGYMAX, ZGYMIN
!                        ZFTOT   = total fraction of a grid cell
!                        ZYMAX   = yi maximum variable
!                        ZYMIN   = yi minimum variable
!                        ZGYMAX  = incomplete gamma function for ymax (GAMSTAR result)
!                        ZGYMIN  = incomplete gamma function for ymin (GAMSTAR result)
!
REAL                  :: ZXSAT_IND, ZYSAT, ZY0, ZDMOY, ZXMOY, ZFMED, ZF0
!                        ZXSAT_IND = Satured index for all index 
!                        ZYSAT     = changing variable of satured index
!                        ZY0       = changing variable of dry index
!                        ZDMOY     = grid cell average deficit (= Dbar/M)
!                        ZXMOY     = ti mean value on wet (1-fsat-f0) fraction
!                        ZF0       = dry fraction
!                        ZFMED     = wet (1-fsat-f0) fraction
!
REAL                  :: ZG, ZGYSAT, ZGY0
!                        ZG     = GAM result 
!                        ZGYSAT = the incomplete gamma function for ysat (GAMSTAR result)
!                        ZGY0   = the incomplete gamma function for y0 (GAMSTAR result)
!
REAL                  :: ZD0, ZPAS, ZCOR
!                        ZD0  = Normalized TOPMODEL maximum deficit D0/M coefficient
!                        ZPAS = pas for calculate the new xsat values array
!
INTEGER               :: IFLG, IFLGST
!                        IFLG   = incomplete gamma function error flag (GAM result)
!                        IFLGST = incomplete gamma function error flag (GAMSTAR result)
!                                 (see gamma_inc.f for more detail)
!
REAL                  :: ZNO, ZAR, ZTOT
!
REAL                  :: ZFUP, ZFDOWN, ZWUP, ZWDOWN, ZSLOPEW
!
INTEGER, DIMENSION (1):: ID
!
INTEGER               :: INI, I, IND, JSI_MIN, JSI_MAX, IPAS, &
                         JL, INL, JPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1 TOPMODEL SURFACE RUNOFF SCHEME
!       ================================
!
!  1.1     initialisation of local variable
!  ----------------------------------------
!
! Grid cells number
!
IF (LHOOK) CALL DR_HOOK('INIT_TOP',0,ZHOOK_HANDLE)
INI = SIZE(PM(:))
INL = SIZE(PWSAT,2)
IPAS = SIZE(PTAB_FSAT,2)
!
! GAM result (not use here !)
!
ZG  = 0.0
!
ZD_TOP    (:) = 0.0
ZWSAT_AVG (:) = 0.0
ZWWILT_AVG(:) = 0.0
!
! soil properties for runoff (m)
!
IF (HISBA == 'DIF') THEN                                   
!
  DO JPATCH=1,NPATCH
    IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
    DO JL=1,INL
       DO I=1,INI
          ZD_TOP    (I) = ZD_TOP    (I) + PPATCH(I,JPATCH)*PSOILWGHT(I,JL,JPATCH)
          ZWSAT_AVG (I) = ZWSAT_AVG (I) + PPATCH(I,JPATCH)*PSOILWGHT(I,JL,JPATCH)*PWSAT (I,JL)
          ZWWILT_AVG(I) = ZWWILT_AVG(I) + PPATCH(I,JPATCH)*PSOILWGHT(I,JL,JPATCH)*PWWILT(I,JL)
       ENDDO
    ENDDO
  ENDDO
!
  WHERE(ZD_TOP(:)>0.0)
        ZWSAT_AVG (:)=ZWSAT_AVG (:)/ZD_TOP(:)
        ZWWILT_AVG(:)=ZWWILT_AVG(:)/ZD_TOP(:)
  ENDWHERE
!
ELSE
!     
  DO JPATCH=1,NPATCH
     IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
     DO I=1,INI
        ZD_TOP(I)=ZD_TOP(I)+PRUNOFFD(I,JPATCH)*PPATCH(I,JPATCH)
     ENDDO
  ENDDO
!     
  ZWSAT_AVG (:) = PWSAT (:,1)
  ZWWILT_AVG(:) = PWWILT(:,1)
!      
ENDIF
!
!
!  1.2     Algorithme
!  ------------------
!
!grid cells loops 
!
ZAR = 0.0
ZTOT= 0.0
ZNO = 0.0
!
DO I=1,INI
!   
   IF(PTI_MEAN(I)==XUNDEF)THEN
!
!    *Case where the Topographics index are not defined.
!    --------------------------------------------------------         
     ZNO=ZNO+1.0
     PTAB_FSAT(I,:)=0.0     
     PTAB_WTOP(I,:)=XUNDEF
!     
     PM(I) =XUNDEF
!
   ELSE 
!
     ZTOT = ZTOT + 1.0
!
!    1.2.0 first initialisation
!    --------------------------
!
     ZXI       = 0.0        
     ZPHI      = 0.0
     ZNU       = 0.0
!
!    Wolock and McCabe (2000) linear regression equation between the mean
!    topographic index computed with a 1000 meter DEM and a 100 meter DEM.
!
     IF(HTOPREG=="DEF")THEN
       ZTI_MEAN=XREGP*PTI_MEAN(I)-XREGA
       IF (HISBA=='DIF'.OR.(PTI_MAX(I)-PTI_MIN(I))<0.2) THEN                                   
          ZTI_MIN =XREGP*PTI_MIN (I)-XREGA
          ZTI_MAX =XREGP*PTI_MAX (I)-XREGA
          ZTI_STD =SQRT((XREGP**2)*(PTI_STD(I)**2))
       ELSE
          ZTI_MIN =PTI_MIN (I)
          ZTI_MAX =PTI_MAX (I)
          ZTI_STD =PTI_STD (I)
       ENDIF
     ELSE
       ZTI_MEAN=PTI_MEAN(I)
       ZTI_MIN =PTI_MIN (I)
       ZTI_MAX =PTI_MAX (I)
       ZTI_STD =PTI_STD (I)
     ENDIF
!
!    Calculate topographic index pdf parameters 
!
!    Numerical problem especialy over Greenland
     IF(PTI_SKEW(I)<=0.2)THEN
!     
       ZTI_SKEW=0.2
!       
       WRITE(KLUOUT,*)'TI_SKEW is too low or negatif (=',PTI_SKEW(I),'),' 
       WRITE(KLUOUT,*)'then PHI is too big for the grid-cell',I,'So,GAMMA(PHI) -> +inf.'
       WRITE(KLUOUT,*)'The applied solution is to put TI_SKEW = 0.2'
       IF(ZTI_STD<1.0)THEN
         WRITE(KLUOUT,*)'In addition TI_STD is too low (=',ZTI_STD,'),' 
         WRITE(KLUOUT,*)'The applied solution is to put TI_STD = 1.0'
         ZTI_STD=1.0
       ENDIF               
!
       ZAR  = ZAR +1.0
!     
       ZXI  = ZTI_SKEW*ZTI_STD/X2 
       ZPHI = (ZTI_STD/ZXI)**X2
!
     ELSE
!
       ZXI  = PTI_SKEW(I)*ZTI_STD/X2 
       ZPHI = (ZTI_STD/ZXI)**X2
!
     ENDIF
!
     ZNU  = ZTI_MEAN-ZPHI*ZXI
!
!    Exponential decay factor of the local deficit
!
     PM(I) =(ZWSAT_AVG(I)-ZWWILT_AVG(I))*ZD_TOP(I)/X4 
!
!    1.2.1 Calculate grid cell pdf total density FTOT = F(ymin --> ymax)
!    -------------------------------------------------------------------
!
!    Normalized TOPMODEL maximum deficit D0/M coefficient
!
     ZD0 = (ZWSAT_AVG(I)-ZWWILT_AVG(I))*ZD_TOP(I)/PM(I)
!
!    Initialise
!
     ZGYMAX = 0.0
     ZGYMIN = 0.0
     ZFTOT  = 0.0
     ZYMIN  = 0.0
     ZYMAX  = 0.0
!
!    variable changing yi ---> (ti-nu)/xi
!
     ZYMIN = (ZTI_MIN-ZNU)/ZXI
     ZYMAX = (ZTI_MAX-ZNU)/ZXI
!  
!    Supress numerical artifact
!
     ZCOR  = ABS(MIN(0.0,ZYMIN))
!
     ZYMIN = MAX(0.0,ZYMIN+ZCOR)
     ZYMAX = ZYMAX+ZCOR
!
!    Errors flags indicating a number of error condition in G and GSTAR
!    (see gamma_inc.f for more detail)
!
     IFLG        =0
     IFLGST      =0
!
!    Computation of F(0 --> ymin)
!
     CALL DGAM(ZPHI,ZYMIN,10.,ZG,ZGYMIN,IFLG,IFLGST)
!      
!    if the incomplete gamma function don't work, print why
!
     IF (IFLGST/=0)THEN
        WRITE(KLUOUT,*)'GRID-CELL =',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMIN= ',ZYMIN 
        CALL ABOR1_SFX('INIT_TOP: (1) PROBLEM WITH DGAM FUNCTION')
     ENDIF      
!
!    Computation of F(0 --> ymax)
!
     CALL DGAM(ZPHI,ZYMAX,10.,ZG,ZGYMAX,IFLG,IFLGST)
!      
!    if the incomplete gamma function don't work, print why
!
     IF (IFLGST/=0)THEN
        WRITE(KLUOUT,*)'GRID-CELL =',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMAX= ',ZYMAX
        CALL ABOR1_SFX('INIT_TOP: (2) PROBLEM WITH DGAM FUNCTION')
     ENDIF      
!
!    FTOT = F(0 --> ymax) - F(0 --> ymin)
!
     ZFTOT=ZGYMAX-ZGYMIN
!
!    initialization water content and fraction
!
     PTAB_WTOP(I,1) = ZWSAT_AVG(I)
     PTAB_FSAT(I,1) = 1.0
!     
     PTAB_WTOP(I,IPAS) = ZWWILT_AVG(I)
     PTAB_FSAT(I,IPAS) = 0.0
!
!    Define the new limits for the satured index loop
!
     JSI_MIN = 2
     JSI_MAX = IPAS-1
     ZPAS    = (ZTI_MAX-ZTI_MIN)/REAL(IPAS-1)
!
!    1.2.2 Calculate all topmodel arrays
!    -----------------------------------
!
!    Satured index loop
!
     DO IND=JSI_MIN,JSI_MAX
!
!       initialize of loops variables
!
        ZXSAT_IND = 0.0 
        ZYSAT     = 0.0
        ZY0       = 0.0
        ZDMOY     = 0.0
        ZXMOY     = 0.0
        ZFMED     = 0.0
!
!       Initialize of incomplete gamma function flags and variables
!
        IFLG   = 0
        IFLGST = 0
        ZGYSAT = 0.0
        ZGY0   = 0.0
!
!       calculate xsat for all new index
!
        ZXSAT_IND=ZTI_MIN+REAL(IND-1)*ZPAS
!
!       Changing variable to compute incomplete gamma function 
!
        ZYSAT=(ZXSAT_IND-ZNU)/ZXI 
        ZY0  =((ZXSAT_IND-ZD0)-ZNU)/ZXI
!      
!       Calculate Y0 and ysat and assume ymin < y0 < ymax !

        ZYSAT=MAX(ZYMIN,MIN(ZYSAT+ZCOR,ZYMAX))
        ZY0  =MAX(ZYMIN,MIN(ZY0  +ZCOR,ZYMAX))
!
!       call incomplete gamma function for xsat
!
        CALL DGAM(ZPHI,ZYSAT,10.,ZG,ZGYSAT,IFLG,IFLGST)
!
!       if the incomplete gamma function don't works, print why
!
        IF (IFLGST/=0)THEN
           WRITE(KLUOUT,*)'GRID-CELL= ',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YSAT= ',ZYSAT
           CALL ABOR1_SFX('INIT_TOP: (3) PROBLEM WITH DGAM FUNCTION')
        ENDIF 
!
!       call incomplete gamma function for xsat
!
        CALL DGAM(ZPHI,ZY0,10.,ZG,ZGY0,IFLG,IFLGST)
!
!       if the incomplete gamma function don't works, print why
!
        IF (IFLGST/=0)THEN
           WRITE(KLUOUT,*)'GRID-CELL= ',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'Y0= ',ZY0
           CALL ABOR1_SFX('INIT_TOP: (4) PROBLEM WITH DGAM FUNCTION')
        ENDIF 
!
!       compute satured fraction as FSAT = F(0 --> ymax) - F(0 --> ysat)
!       
        PTAB_FSAT(I,IND)=MAX(0.0,(ZGYMAX-ZGYSAT)/ZFTOT)
!
!       Compute driest fraction
!
        ZF0=MAX(0.0,(ZGY0-ZGYMIN)/ZFTOT)
!
!       Calculate FMED
!        
        ZFMED=(1.0-PTAB_FSAT(I,IND)-ZF0)
!
        IF (ZFMED/=0.0) THEN
!
!          Compute the new x mean, xmoy', over the wet fraction Fwet
!
           ZXMOY = ZNU+ZXI*(ZPHI-ZCOR+(EXP(-ZY0)*(ZY0**(ZPHI/X2))*(ZY0**(ZPHI/X2))     &
                 - EXP(-ZYSAT)*(ZYSAT**(ZPHI/X2))*(ZYSAT**(ZPHI/X2)))/(ZFMED*GAMMAS(ZPHI)))
!
!          supress numerical artifacs
!
           ZXMOY =MAX((ZXSAT_IND-ZD0),MIN(ZXSAT_IND,ZXMOY))
!
!          Calculate the mean normalysed deficit as Dbar/M = (1-fsat-f0)*(xsat-xmoy')+f0*D0/M
!
           ZDMOY = ZFMED*(ZXSAT_IND-ZXMOY)+ZF0*ZD0
!
        ENDIF
!
!       supress numerical artifacs
!
        ZDMOY = MAX(0.0,MIN(ZDMOY,ZD0))
!
!       Solves Dbar = (Wsat-WT)*d_top with Dbar/M (=ZDMOY) = (Wsat-WT)*d_top/M
!
        PTAB_WTOP(I,IND) = ZWSAT_AVG(I)-(PM(I)*ZDMOY/ZD_TOP(I))
!        
      ENDDO
!
    ENDIF
!
ENDDO
!
! supress numerical artifacs for boundaries conditions
!
DO I=1,INI
!
!  Upper boundary
!
   IF(PTAB_WTOP(I,2)==ZWSAT_AVG(I))THEN
!
     ZFUP=PTAB_FSAT(I,1)
     ZWUP=PTAB_WTOP(I,1)
!   
     ID(:)=MAXLOC(PTAB_WTOP(I,:),PTAB_WTOP(I,:)<ZWSAT_AVG(I))
!   
     ZFDOWN=PTAB_FSAT(I,ID(1))
     ZWDOWN=PTAB_WTOP(I,ID(1))
!     
     ZSLOPEW=(ZWUP-ZWDOWN)/(ZFUP-ZFDOWN)   
!
     DO IND=2,ID(1)-1
        PTAB_WTOP(I,IND)=ZWDOWN+(PTAB_FSAT(I,IND)-ZFDOWN)*ZSLOPEW
     ENDDO
!   
   ENDIF
!
!  Lower boundary
!
   WHERE(PTAB_FSAT(I,:)<=0.0      )
         PTAB_WTOP(I,:)=ZWWILT_AVG(I)
   ENDWHERE
   WHERE(PTAB_WTOP(I,:)<=ZWWILT_AVG(I))
         PTAB_FSAT(I,:)=0.0
   ENDWHERE
!   
ENDDO
!
WRITE(KLUOUT,*)'-------------------TOPMODEL SUM-UP-------------------------'
WRITE(KLUOUT,*)'Number of grid-cells ',INI,'Number of Topmodel points',INT(ZTOT)
IF(INI/=0) THEN
  WRITE(KLUOUT,*)'Percentage of non TOPMODEL grid-cells',(100.*ZNO/FLOAT(INI))
ENDIF
IF(ZTOT>0.0)THEN
  WRITE(KLUOUT,*)'Percentage of arranged (TI-SKE=0.2) grid-cells',(100.*ZAR/ZTOT)
ENDIF
WRITE(KLUOUT,*)'-----------------------------------------------------------'
!
IF (LHOOK) CALL DR_HOOK('INIT_TOP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_TOP
