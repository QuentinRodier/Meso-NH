!     ###############################################################
      SUBROUTINE ISBA_SGH_UPDATE(HISBA,HRUNOFF,HRAIN,PRAIN,PMUF,PFSAT)
!     ###############################################################
!
!!****  *SGH_UPDATE*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the fraction, mu, of the grid cell
!     reached by the rain, the Topmodel saturated fraction and the diagnostic
!     wetland fraction.
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!	B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      07/2011 (B. Decharme) : Add fsat diag for dt92
!!
!-------------------------------------------------------------------------------
!
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n,      ONLY : NGROUND_LAYER, NPATCH, XPATCH, XWG, XWWILT,  &
                             XWSAT, XTAB_FSAT, XTAB_WTOP,                 &
                             XTI_MEAN, XSOILWGHT, XRUNOFFD,               &
                             NSIZE_NATURE_P, NLAYER_DUN, XWGI
!
USE MODD_ISBA_GRID_n, ONLY : XMESH_SIZE
!
USE MODD_SGH_PAR,     ONLY : NDIMTAB, XMTOKM, XSTOHR, X001,      &
                             XMUREGP, XMUREGA
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!------------------waiting for MEB-------------------------!
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT
USE MODD_SNOW_PAR , ONLY : XWSNV
!------------------waiting for MEB-------------------------!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN)     :: HISBA  ! type of ISBA version:
!                                          ! '2-L' (default)
!                                          ! '3-L'
!                                          ! 'DIF'
!
 CHARACTER(LEN=*), INTENT(IN)     :: HRUNOFF! surface runoff formulation
!                                          ! 'WSAT'
!                                          ! 'DT92'
!                                          ! 'SGH ' Topmodel
!                                                     
!
 CHARACTER(LEN=*), INTENT(IN)     :: HRAIN  ! Rainfall spatial distribution
                                           ! 'DEF' = No rainfall spatial distribution
                                           ! 'SGH' = Rainfall exponential spatial distribution
!
REAL, DIMENSION(:), INTENT(IN)   :: PRAIN
!                                   PRAIN   = rain rate (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PMUF
!                                   PMUF = fraction of the grid cell reached by the precipitation
!
REAL, DIMENSION(:), INTENT(OUT)  :: PFSAT
!                                   PFSAT   = Topmodel satured fraction
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZDIST, ZBETA    ! HRAIN = SGH
!                                        ZDIST  = the cell scale (in km)
!                                        ZBETA  = cell scale dependency parameter
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZD_TOP, ZW_TOP  ! HRUNOFF = SGH
!                                        ZW_TOP = ative TOPMODEL-soil moisture at 't' (m3 m-3)
!                                        ZD_TOP  = Topmodel active layer
!
INTEGER, DIMENSION(SIZE(PRAIN))       :: IUP,IDOWN  ! HRUNOFF = SGH
!                                        change in xsat (or fsat) index
!
INTEGER, DIMENSION(SIZE(PRAIN))       :: NMASK      ! indices correspondance between arrays
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZWSAT_AVG, ZWWILT_AVG
!                                           Average soil properties content
!
REAL                                  :: ZF_UP, ZF_DOWN, ZW_UP, ZW_DOWN, ZSLOPEF
!
INTEGER                               :: INI, JJ, JI, JPATCH, JTAB, ICOUNT, &
                                         JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SGH_UPDATE',0,ZHOOK_HANDLE)
!
INI=SIZE(PRAIN,1)
!
PFSAT   (:) = 0.0
!
!*   1.0 Spatial distribution of precipitation
!    ---------------------------------------------
!
IF(HRAIN=='SGH')THEN
!
  WHERE(PRAIN(:)>0.0)
        PMUF (:) =1.0
  ELSEWHERE
        PMUF (:) =0.0
  ENDWHERE

!        
! calculate the cell scale (in km)
!
  ZDIST(:) = SQRT(XMESH_SIZE(:))/XMTOKM
!
  WHERE(ZDIST(:)>=15.0)
!
!       calculate beta for the mu calculation
!         
        ZBETA (:) = XMUREGA + XMUREGP * EXP(-X001*ZDIST(:))
!
!       calculate mu, precip is in mm/hr
!         
        PMUF (:) = 1.0 - EXP(-ZBETA(:)*(PRAIN(:)*XSTOHR))
!
  ENDWHERE
!
ENDIF
!
!*   2.0 Computation of the saturated fraction given by TOPMODEL 
!    -----------------------------------------------------------
!
IF(HRUNOFF=='SGH')THEN
!
! Calculation of the ative TOPMODEL-soil moisture at 't' (m)
! ---------------------------------------------------------------
!  
  ZW_TOP    (:) = 0.0
  ZD_TOP    (:) = 0.0
  ZWSAT_AVG (:) = 0.0
  ZWWILT_AVG(:) = 0.0
!
  IF(HISBA=='DIF')THEN        
!
    DO JPATCH=1,NPATCH
      IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
      DO JL=1,NLAYER_DUN
        DO JJ=1,INI
          ZD_TOP    (JJ) = ZD_TOP    (JJ) + XPATCH(JJ,JPATCH)*XSOILWGHT(JJ,JL,JPATCH)
          ZWSAT_AVG (JJ) = ZWSAT_AVG (JJ) + XPATCH(JJ,JPATCH)*XSOILWGHT(JJ,JL,JPATCH)*XWSAT (JJ,JL)
          ZWWILT_AVG(JJ) = ZWWILT_AVG(JJ) + XPATCH(JJ,JPATCH)*XSOILWGHT(JJ,JL,JPATCH)*XWWILT(JJ,JL)
          !------------------waiting for MEB-------------------------!
          IF(LSNOW_FRAC_TOT.OR.XWSNV<0.1)THEN
            ZW_TOP(JJ) = ZW_TOP(JJ) + XPATCH(JJ,JPATCH)*XSOILWGHT(JJ,JL,JPATCH)*XWG(JJ,JL,JPATCH)
          ELSE
            ZW_TOP(JJ) = ZW_TOP(JJ) + XPATCH(JJ,JPATCH)*XSOILWGHT(JJ,JL,JPATCH)*(XWG(JJ,JL,JPATCH)+XWGI(JJ,JL,JPATCH))
          ENDIF
          !------------------waiting for MEB-------------------------!
        ENDDO
      ENDDO
    ENDDO
!
    WHERE(ZD_TOP(:)>0.0)
         ZWSAT_AVG (:) = ZWSAT_AVG (:)/ZD_TOP(:)
         ZWWILT_AVG(:) = ZWWILT_AVG(:)/ZD_TOP(:)
         ZW_TOP    (:) = ZW_TOP    (:)/ZD_TOP(:)
    ENDWHERE
!
  ELSE
!    
    DO JPATCH=1,NPATCH
      IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
      DO JJ=1,INI
        ZD_TOP(JJ) = ZD_TOP(JJ)+XRUNOFFD(JJ,JPATCH)*XPATCH(JJ,JPATCH)
        ZW_TOP(JJ) = ZW_TOP(JJ)+XRUNOFFD(JJ,JPATCH)*XPATCH(JJ,JPATCH)*XWG(JJ,2,JPATCH)
      ENDDO
    ENDDO
!  
    WHERE(ZD_TOP(:)>0.0)
          ZW_TOP(:) = ZW_TOP(:) / ZD_TOP(:)
    ENDWHERE
!      
    ZWSAT_AVG (:) = XWSAT (:,1)
    ZWWILT_AVG(:) = XWWILT(:,1)
!
  ENDIF
!
! Find the boundary
! -----------------
!
  NMASK(:)=0
  ICOUNT=0
  DO JJ=1,INI  
     IF((XTI_MEAN(JJ)/=XUNDEF.AND.ZW_TOP(JJ)<ZWSAT_AVG(JJ).AND.ZW_TOP(JJ)>ZWWILT_AVG(JJ)))THEN     
       ICOUNT=ICOUNT+1
       NMASK(ICOUNT)=JJ       
     ENDIF
     IF(ZW_TOP(JJ)>=ZWSAT_AVG(JJ))THEN
        PFSAT (JJ) = 1.0
     ENDIF
  ENDDO
!     
! compare wt_array and WT
! -----------------------
!
  DO JTAB=1,NDIMTAB
     DO JJ=1,ICOUNT
        JI = NMASK(JJ)    
        IF(XTAB_WTOP(JI,JTAB)>ZW_TOP(JI))THEN
          IUP(JJ)=JTAB
          IDOWN(JJ)=JTAB+1
        ELSEIF(XTAB_WTOP(JI,JTAB)==ZW_TOP(JI))THEN
          IUP(JJ)=JTAB
          IDOWN(JJ)=JTAB
        ENDIF
     ENDDO    
  ENDDO 
!    
! calculate fsat
! --------------
!     
  DO JJ=1,ICOUNT
!  
     JI = NMASK(JJ)
!     
!    new range
     ZF_UP   = XTAB_FSAT(JI,IUP  (JJ))
     ZF_DOWN = XTAB_FSAT(JI,IDOWN(JJ))
     ZW_UP   = XTAB_WTOP(JI,IUP  (JJ))
     ZW_DOWN = XTAB_WTOP(JI,IDOWN(JJ))
!     
!    Calculate new FSAT
     ZSLOPEF = 0.0
     IF(IUP(JJ)/=IDOWN(JJ))THEN
       ZSLOPEF = (ZF_UP-ZF_DOWN)/(ZW_UP-ZW_DOWN)
     ENDIF
!     
     PFSAT(JI) = ZF_DOWN+(ZW_TOP(JI)-ZW_DOWN)*ZSLOPEF
!     
  ENDDO 
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SGH_UPDATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------
!
END SUBROUTINE ISBA_SGH_UPDATE
