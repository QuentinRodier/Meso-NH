!######################################################
SUBROUTINE INIT_TRIP_n (HPROGRAM,KYEAR,KMONTH,ORESTART)
!######################################################
!
!!****  *INIT_TRIP_n*  
!!
!!    PURPOSE
!!    -------
!
!     Initialize TRIP variables and parameters.
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
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/05 
!!      For surfex  21/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR
USE MODD_TRIP_GRID_n, ONLY : XGRID_TRIP, XAREA, GMASK
USE MODD_TRIP_n
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
!
USE MODE_GRID_TRIP
USE MODE_TRIP_INIT
USE MODE_RW_TRIP
!
USE MODI_INIT_TRIP_PAR
USE MODI_GET_LUOUT
!
USE MODI_DEFAULT_TRIP
USE MODI_READ_NAM_GRID_TRIP
USE MODI_READ_TRIP_CONF_n
USE MODI_INIT_DIAG_TRIP_n
USE MODI_INIT_RESTART_TRIP_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
INTEGER,          INTENT(IN) :: KYEAR
INTEGER,          INTENT(IN) :: KMONTH 
LOGICAL,          INTENT(IN) :: ORESTART
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=13), PARAMETER         :: YFILE_PARAM  ='TRIP_PARAM.nc'
 CHARACTER(LEN=12), PARAMETER         :: YFILE_INIT   ='TRIP_PREP.nc'
 CHARACTER(LEN=15), PARAMETER         :: YFILE_RESTART='TRIP_RESTART.nc'
 CHARACTER(LEN=10), PARAMETER         :: YDIAG        ='TRIP_DIAG_'
!
 CHARACTER(LEN=6)                     :: YTIME
!
 CHARACTER(LEN=50)                    :: YFILE
 CHARACTER(LEN=20)                    :: YVAR 
!
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZREAD, ZHSTREAM
!
INTEGER           :: ILUOUT         ! output listing logical unit
!
REAL    :: ZLONMIN   ! minimum longitude (degrees)
REAL    :: ZLONMAX   ! maximum longitude (degrees)
REAL    :: ZLATMIN   ! minimum latitude  (degrees)
REAL    :: ZLATMAX   ! maximum latitude  (degrees)
REAL    :: ZGRID_RES ! 1° or 0.5° resolution
INTEGER :: ILON      ! number of points in longitude
INTEGER :: ILAT      ! number of points in latitude
!
INTEGER :: IWORK, IFLOOD, I, J, INI
!
!-------------------------------------------------------------------------------
!Output attribut for netcdf diag file
!-------------------------------------------------------------------------------
!
 CHARACTER(LEN=50) :: YTITLE,YTIMEUNIT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                    YTITLE    = Title of each output file
!                    YTIMEUNIT = Time unit in each output file if present
!
!-------------------------------------------------------------------------------
!Initilyse TRIP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_N',0,ZHOOK_HANDLE)
 CALL INIT_TRIP_PAR
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'!'
WRITE(ILUOUT,*)'! TRIP RUN !!!!!!!!!!!!!'
WRITE(ILUOUT,*)'!'
!
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'        INITIALYSE TRIP            '
WRITE(ILUOUT,*)''
!
!-------------------------------------------------------------------------------
!        
IF (LNAM_READ) THEN
!        
! * default values for the TRIP configuration
!
  CALL DEFAULT_TRIP(CGROUNDW,CVIT,LFLOODT,LDIAG_CPL,LTRIP_DIAG_MISC, &
                    LNCPRINT,LPRINT_TRIP,XTSTEP_COUPLING,XTRIP_TSTEP,&
                    XDATA_TAUG,XCVEL,XRATMED                         )  
!
ENDIF
!
! * TRIP general options
!
  CALL READ_TRIP_CONF_n(HPROGRAM)
!
! * Read TRIP grid configuration
!
 CALL READ_NAM_GRID_TRIP(HPROGRAM)
!
! * Store TRIP grid configuration
!
 CALL GET_GRID_TRIP(XGRID_TRIP,ZLONMIN,ZLONMAX,ZLATMIN,ZLATMAX,ZGRID_RES,ILON,ILAT)
!
!-------------------------------------------------------------------------------
! * Check options
!-------------------------------------------------------------------------------
!
IF(CVIT == 'VAR')THEN
   IF(XCVEL/=0.5)THEN
      XCVEL=0.5
      WRITE(ILUOUT,*)'! ATTENTION : You use the velocity scheme and XCVEL is not 0.5 m/s !!!'
      WRITE(ILUOUT,*)'! ATTENTION : XCVEL put at 0.5 m/s !!!'
  ENDIF
ELSE
   IF(XCVEL<0.1)THEN
      WRITE(ILUOUT,*)'!!! ATTENTION : XCVEL < 0.1 m/s !!! Not good !!!'
      STOP
  ENDIF     
ENDIF      
!
IF(LFLOODT)THEN
  IF(CGROUNDW=='DEF')THEN
    WRITE(ILUOUT,*)'! ATTENTION : You use the flooding scheme without the groundwater scheme !!!'
  ENDIF
  IF(CVIT /= 'VAR')THEN
    WRITE(ILUOUT,*)'! You cannot use the flooding scheme without the variable velocity scheme !!!'
    STOP
  ENDIF
  IF(XTRIP_TSTEP>3600.)THEN
    WRITE(ILUOUT,*)'!'
    WRITE(ILUOUT,*)'! For flooding, the TRIP time step is too big      !!!'
    WRITE(ILUOUT,*)'! XTRIP_TSTEP must be equal or inferior to 3600s   !!!'
    WRITE(ILUOUT,*)'!'
    STOP
  ENDIF
ENDIF
!
IF(MOD(XTSTEP_COUPLING,XTRIP_TSTEP)/=0.)THEN
  WRITE(ILUOUT,*)'! XTSTEP_COUPLING and XTRIP_TSTEP are not good !!!'     
  WRITE(ILUOUT,*)'! MOD(XTSTEP_COUPLING,XTRIP_TSTEP) should be 0 !!!'     
  STOP
ENDIF
!
WRITE(ILUOUT,*)'! ',ZGRID_RES,'° TRIP run !!!'  
!
IF(ZGRID_RES/=1.0)THEN
!
  IF(XRATMED==1.4)THEN
     WRITE(ILUOUT,*)'! meandering ratio is 1.4 at 0.5° or 1° resolution !!!'   
     WRITE(ILUOUT,*)'! for other resolution change XRATMED in namelist  !!!' 
     IF(ZGRID_RES<0.5)STOP
  ENDIF 
!
ENDIF
!
!-------------------------------------------------------------------------------
!Allocate arguments
!-------------------------------------------------------------------------------
!
ALLOCATE(IGRCN      (ILON,ILAT))       
ALLOCATE(ISEQ       (ILON,ILAT))   
ALLOCATE(INEXTX     (ILON,ILAT))    
ALLOCATE(INEXTY     (ILON,ILAT))     
ALLOCATE(XLEN       (ILON,ILAT))           
ALLOCATE(XAREA      (ILON,ILAT))          
ALLOCATE(XSURF_STO  (ILON,ILAT))
!
IF(CGROUNDW/='DEF')THEN
  ALLOCATE(XGROUND_STO(ILON,ILAT))      
  ALLOCATE(XTAUG      (ILON,ILAT))
ELSE
  ALLOCATE(XGROUND_STO(0,0))      
  ALLOCATE(XTAUG      (0,0))
ENDIF
!
IF(LFLOODT)THEN
  ALLOCATE(XN_FLOOD    (ILON,ILAT))      
  ALLOCATE(XHC_BED     (ILON,ILAT))
  ALLOCATE(XFLOOD_STO  (ILON,ILAT))
  ALLOCATE(XWFLOOD     (ILON,ILAT))
  ALLOCATE(XFLOOD_LEN  (ILON,ILAT))
  ALLOCATE(XHFLOOD     (ILON,ILAT))
  ALLOCATE(XFFLOOD     (ILON,ILAT))
  ALLOCATE(XPIFLOOD    (ILON,ILAT))  
ELSE
  ALLOCATE(XN_FLOOD  (0,0))      
  ALLOCATE(XHC_BED   (0,0))
  ALLOCATE(XFLOOD_STO(0,0))
  ALLOCATE(XWFLOOD   (0,0))
  ALLOCATE(XFLOOD_LEN(0,0))
  ALLOCATE(XHFLOOD   (0,0))
  ALLOCATE(XFFLOOD   (0,0))
  ALLOCATE(XPIFLOOD  (0,0))  
ENDIF
!
IF(CVIT=='VAR')THEN
  ALLOCATE(XSLOPEBED(ILON,ILAT))      
  ALLOCATE(XWIDTH   (ILON,ILAT))      
  ALLOCATE(XN       (ILON,ILAT))
ELSE
  ALLOCATE(XSLOPEBED(0,0))      
  ALLOCATE(XWIDTH   (0,0))      
  ALLOCATE(XN       (0,0))
ENDIF
!
!-------------------------------------------------------------------------------
! * Compute and Read TRIP parameter
!-------------------------------------------------------------------------------
!
ALLOCATE(ZREAD(ILON,ILAT))
!
! * Flow direction 
!
YVAR ='FLOWDIR'
 CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZREAD)
WHERE(ZREAD==XTRIP_UNDEF)ZREAD=0.0
IGRCN(:,:)=INT(ZREAD(:,:))
!
IF(.NOT.ASSOCIATED(GMASK))THEN
  ALLOCATE(GMASK(ILON,ILAT))
  GMASK(:,:)=.TRUE.
  WHERE(IGRCN(:,:)==0)GMASK(:,:)=.FALSE.
ENDIF
!
! * Rriver sequence
!
YVAR ='RIVSEQ'
 CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZREAD)
WHERE(ZREAD==XTRIP_UNDEF)ZREAD=0.0
ISEQ(:,:)=INT(ZREAD(:,:))
!
! * Maximum river sequence value
!
ISEQMAX = MAXVAL(ISEQ)
!
DEALLOCATE(ZREAD)
!
! * Set down stream
!
 CALL SETNEXT(ILON,ILAT,IGRCN,INEXTX,INEXTY)
!
! * Set area size
!
 CALL SETAREA(ILAT,ZLATMIN,ZGRID_RES,XAREA)
!
! * Distance between grids with the meandering ratio
!
YVAR ='RIVLEN'
 CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XLEN)
WHERE(.NOT.GMASK(:,:))XLEN(:,:)=0.0
!
! * Variable velocity schemes variables
!
IF(CVIT == 'VAR')THEN
!
  YVAR ='N_RIV'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XN)
!
  YVAR ='WIDTHRIV'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XWIDTH)
!
  YVAR ='SLOPERIV'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XSLOPEBED)
!  
  WHERE(ISEQ(:,:)==0.OR.XWIDTH>=XTRIP_UNDEF-1.0)
        XSLOPEBED= 0.0      
        XWIDTH   = 0.0      
        XN       = 0.0
  ENDWHERE
!
ENDIF 
!
! * Calculate the groundwater transfert time
!
IF(CGROUNDW/='DEF')THEN
!
  YVAR ='TAUG'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XTAUG)
!
  WHERE(ISEQ(:,:)==0)
        XTAUG=0.0
  ELSEWHERE
        XTAUG=XTAUG*XDAY_T
  ENDWHERE
!
ENDIF
!
! * Calculate floodplains parameters
!
IF(LFLOODT)THEN
!
  YVAR ='RIVDEPTH'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XHC_BED)
!
  YVAR ='NFLOOD'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XN_FLOOD)
!  
  WHERE(XHC_BED==XTRIP_UNDEF)
    XHC_BED  = 0.0
    XN_FLOOD = 0.0
  ENDWHERE
!
  ALLOCATE(ITABMAX(ILON,ILAT)) 
!
  ALLOCATE(XTAB_F (ILON,ILAT,NTRIPTAB))      
  ALLOCATE(XTAB_H (ILON,ILAT,NTRIPTAB))      
  ALLOCATE(XTAB_VF(ILON,ILAT,NTRIPTAB))      
!
  YVAR ='TABF'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XTAB_F)
!
  YVAR ='TABH'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XTAB_H)
!
  YVAR ='TABVF'
  CALL READ_TRIP(ILUOUT,YFILE_PARAM,YVAR,XTAB_VF)
!
  ITABMAX(:,:)=NTRIPTAB-COUNT(XTAB_H(:,:,:)>=XTRIP_UNDEF-1.0,3)  
!
ELSE
!
  ALLOCATE(ITABMAX(0,0  ))  
  ALLOCATE(XTAB_F (0,0,0))      
  ALLOCATE(XTAB_H (0,0,0))      
  ALLOCATE(XTAB_VF(0,0,0))      
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Read initial and historical variables
!-------------------------------------------------------------------------------
!
YVAR ='SURF_STO'
 CALL READ_TRIP(ILUOUT,YFILE_INIT,YVAR,XSURF_STO)
!
IF(CGROUNDW/='DEF')THEN
  YVAR ='GROUND_STO'
  CALL READ_TRIP(ILUOUT,YFILE_INIT,YVAR,XGROUND_STO)
ENDIF
!
IF(LFLOODT)THEN
!
  YVAR ='FLOOD_STO'
  CALL READ_TRIP(ILUOUT,YFILE_INIT,YVAR,XFLOOD_STO)
!
  YVAR ='FFLOOD_T'
  CALL READ_TRIP(ILUOUT,YFILE_INIT,YVAR,XFFLOOD)
!
  YVAR ='HFLOOD_T'
  CALL READ_TRIP(ILUOUT,YFILE_INIT,YVAR,XHFLOOD)
!  
ENDIF
!
!-------------------------------------------------------------------------------
! * Initial Conditions
!-------------------------------------------------------------------------------
!
IF(CVIT == 'VAR')THEN
!        
  ALLOCATE(ZHSTREAM(ILON,ILAT))
!
  WHERE(XWIDTH(:,:)>0.0)
        ZHSTREAM(:,:)=MAX(0.0,XSURF_STO(:,:)/(XRHOLW_T*XLEN(:,:)*XWIDTH(:,:)))
  ELSEWHERE
        ZHSTREAM(:,:)=0.0
  ENDWHERE
!
ENDIF
!
! * Fraction, width, water depth of floodplains
! 
IWORK=0
!
IF(LFLOODT)THEN 
!        
  WHERE(XLEN(:,:)/=0.0)
        XFLOOD_LEN(:,:) = XRATMED*SQRT(XFFLOOD(:,:)*XAREA(:,:))
        XPIFLOOD  (:,:) = XFLOOD_STO(:,:)        
  ELSEWHERE
        XFLOOD_LEN(:,:) = 0.0
        XWFLOOD   (:,:) = 0.0
        XFFLOOD   (:,:) = 0.0
        XHFLOOD   (:,:) = 0.0
        XFLOOD_STO(:,:) = 0.0
        XPIFLOOD  (:,:) = 0.0        
  ENDWHERE
!
!
  WHERE(XFFLOOD(:,:)>0.0)
        XWFLOOD(:,:) = XAREA(:,:) * XFFLOOD(:,:) / XFLOOD_LEN(:,:)
  ELSEWHERE
        XWFLOOD(:,:) = 0.0
  ENDWHERE
!  
  INI = COUNT(XLEN>0.0)
  IWORK =COUNT(XHC_BED>0.0)
  IFLOOD=COUNT(XFFLOOD>0.0)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT,*)'Coupling_time_step :           ',XTSTEP_COUPLING
WRITE(ILUOUT,*)'TRIP_time_step :               ',XTRIP_TSTEP
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'Sequence max :                 ',ISEQMAX
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'MEANDERING RATIO FIXED TO      ',XRATMED
WRITE(ILUOUT,*)'CELL LENGTH MIN, MAX (km):     ',MINVAL(XLEN/1.E3, XLEN>0.0),  &
                                                   MAXVAL(XLEN/1.E3, XLEN>0.0)                                 
WRITE(ILUOUT,*)'CELL AREA MIN, MAX (km²):      ',MINVAL(XAREA/1.E6,XLEN>0.0),  &
                                                   MAXVAL(XAREA/1.E6,XLEN>0.0)  
WRITE(ILUOUT,*)''
IF(CGROUNDW/='DEF')THEN
  WRITE(ILUOUT,*)''
  WRITE(ILUOUT,*)'Ground transf. time MIN, MAX : ',MINVAL(XTAUG/XDAY_T,XLEN>0.0),  &
                                                     MAXVAL(XTAUG/XDAY_T,XLEN>0.0)  
ENDIF
IF(CVIT == 'VAR')THEN
  WRITE(ILUOUT,*)'WIDTH_RIVER MIN, MAX :         ',MINVAL(XWIDTH,    XWIDTH>0.0),  &
                                                   MAXVAL(XWIDTH,    XWIDTH>0.0)
  WRITE(ILUOUT,*)'N MANNING COEF MIN, MAX :      ',MINVAL(XN,        XWIDTH>0.0),  &
                                                   MAXVAL(XN,        XWIDTH>0.0)
  WRITE(ILUOUT,*)'RIVER SLOPE MIN, MAX :         ',MINVAL(XSLOPEBED, XN  >0.0),  &
                                                     MAXVAL(XSLOPEBED, XN  >0.0)  
  WRITE(ILUOUT,*)''
  WRITE(ILUOUT,*)'Initial river depth          : ',MINVAL(ZHSTREAM,  XWIDTH>0.0),  &
                                                   MAXVAL(ZHSTREAM,  XWIDTH>0.0)  
  DEALLOCATE(ZHSTREAM)
ENDIF
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'Initial river storage        : ',MINVAL(XSURF_STO, XLEN>0.0),  &
                                                   MAXVAL(XSURF_STO, XLEN>0.0)  
IF(CGROUNDW/='DEF')THEN                                                 
  WRITE(ILUOUT,*)''
  WRITE(ILUOUT,*)'Initial ground storage       : ',MINVAL(XGROUND_STO,XLEN>0.0),  &
                                                     MAXVAL(XGROUND_STO,XLEN>0.0)  
ENDIF
WRITE(ILUOUT,*)''

IF(LFLOODT)THEN 
  WRITE(ILUOUT,*)'N FLOOD FIXED TO               ',MINVAL(XN_FLOOD,  XHC_BED>0.0)
  WRITE(ILUOUT,*)'RIVER DEPTH MIN, MAX :         ',MINVAL(XHC_BED,   XHC_BED>0.0),  &
                                                     MAXVAL(XHC_BED,   XHC_BED>0.0)  
  WRITE(ILUOUT,*)''
  WRITE(ILUOUT,*)'Number of potential flood cell : ',IWORK,'on',INI
  WRITE(ILUOUT,*)'          %                      ',100.0*(FLOAT(IWORK)/FLOAT(INI))
  WRITE(ILUOUT,*)'Number of actual flood cell :    ',IFLOOD,'on',IWORK
  WRITE(ILUOUT,*)'          %                      ',100.0*(FLOAT(IFLOOD)/FLOAT(IWORK))
  WRITE(ILUOUT,*)'% of flooded area in the domain :',SUM(XFFLOOD*XAREA)/SUM(XAREA)
  WRITE(ILUOUT,*)''
  WRITE(ILUOUT,*)'Initial flood depth (m) :      ',MINVAL(XHFLOOD,    XHC_BED>0.0), &
                                                     MAXVAL(XHFLOOD,    XHC_BED>0.0)  
  WRITE(ILUOUT,*)'Initial flood fraction :       ',MINVAL(XFFLOOD,    XHC_BED>0.0), &
                                                     MAXVAL(XFFLOOD,    XHC_BED>0.0)  
  WRITE(ILUOUT,*)'Initial flood volume m3/1E9 :  ',MINVAL(XFLOOD_STO/(XRHOLW_T*1.E9),XHC_BED>0.0), &
                                                     MAXVAL(XFLOOD_STO/(XRHOLW_T*1.E9),XHC_BED>0.0)   
  WRITE(ILUOUT,*)'Initial flood length (km):     ',MINVAL(XFLOOD_LEN/1.E3, XHC_BED>0.0),  &
                                                     MAXVAL(XFLOOD_LEN/1.E3, XHC_BED>0.0)                           
  WRITE(ILUOUT,*)'Initial flood WIDTH (km) :     ',MINVAL(XWFLOOD/1.E3,    XHC_BED>0.0), &
                                                     MAXVAL(XWFLOOD/1.E3,    XHC_BED>0.0)      
  WRITE(ILUOUT,*)''
ENDIF
!
!-------------------------------------------------------------------------------
! * Create diag file
!-------------------------------------------------------------------------------
!
IF(KMONTH/=0)THEN
  WRITE(YTIME,'(i4.4,i2.2)') KYEAR, KMONTH
ELSE
  WRITE(YTIME,'(i4.4)') KYEAR
ENDIF
!
IF(LDIAG_CPL)THEN
  YFILE  = YDIAG//'CPL_'//YTIME(1:LEN_TRIM(YTIME))//'.nc'
  YTITLE = 'TRIP coupling time step outputs'
  YTIMEUNIT='-'
  CALL INIT_DIAG_TRIP_n(ILUOUT,YFILE,ILON,ILAT,YTITLE,YTIMEUNIT,.TRUE.)
ENDIF
!
NRUN=0
!
YFILE     = YDIAG//'RUN_'//YTIME(1:LEN_TRIM(YTIME))//'.nc'
YTITLE    = 'TRIP run outputs'
YTIMEUNIT = '-'
 CALL INIT_DIAG_TRIP_n(ILUOUT,YFILE,ILON,ILAT,YTITLE,YTIMEUNIT,.FALSE.)
!
!-------------------------------------------------------------------------------
! * Create restart file
!-------------------------------------------------------------------------------
!
IF(ORESTART)THEN        
  YTITLE   ='TRIP restart variables'
  YTIMEUNIT='-'
  CALL INIT_RESTART_TRIP_n(ILUOUT,YFILE_RESTART,ILON,ILAT,YTITLE,YTIMEUNIT,.FALSE.)
ENDIF
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * END
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP_n





