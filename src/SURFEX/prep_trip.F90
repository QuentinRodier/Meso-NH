!     #########
      SUBROUTINE PREP_TRIP (HPROGRAM)
!     ###############################
!
!!****  *PREP_TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     Prepare TRIP variables and parameters.
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
!!	B. Decharme     (Météo-France)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR
USE MODD_TRIP_GRID_n
USE MODD_TRIP_n,      ONLY : XSURF_STO, XGROUND_STO, XTAUG, XHC_BED, &
                               XFLOOD_STO, XHFLOOD, XFFLOOD, XN,       &
                               XSLOPEBED, XWIDTH, XTAB_F, XTAB_H,      &
                               XTAB_VF, CGROUNDW, CVIT, LFLOODT,       &
                               LDIAG_CPL, LNCPRINT,LPRINT_TRIP,        &
                               XTSTEP_COUPLING, XTRIP_TSTEP,XLEN,      &
                               LTRIP_DIAG_MISC, XDATA_TAUG,            &
                               XN_FLOOD, XRATMED, XCVEL  
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
!
USE MODE_TRIP_INIT
USE MODE_RW_TRIP
!
USE MODI_INIT_TRIP_PAR
USE MODI_GET_LUOUT
USE MODI_GET_GRID_CONF_TRIP_n
!
USE MODI_DEFAULT_TRIP
USE MODI_READ_NAM_GRID_TRIP
USE MODI_READ_TRIP_CONF_n
USE MODI_INIT_PARAM_TRIP_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_INIT_RESTART_TRIP_n
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),      INTENT(IN) :: HPROGRAM
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=14), PARAMETER         :: YFILE_PARAM_1D  ='TRIP_PGD_1D.nc'
 CHARACTER(LEN=14), PARAMETER         :: YFILE_PARAM_HD  ='TRIP_PGD_HD.nc'
 CHARACTER(LEN=15), PARAMETER         :: YFILE_INIT_1D   ='TRIP_INIT_1D.nc'
 CHARACTER(LEN=15), PARAMETER         :: YFILE_INIT_HD   ='TRIP_INIT_HD.nc'
!
 CHARACTER(LEN=13), PARAMETER         :: YFILE_PARAM     ='TRIP_PARAM.nc'
 CHARACTER(LEN=12), PARAMETER         :: YFILE_PREP      ='TRIP_PREP.nc'
!
 CHARACTER(LEN=14)                    :: YFILE_READ
 CHARACTER(LEN=15)                    :: YFILE_READ_INIT
!
 CHARACTER(LEN=18), PARAMETER         :: YFILE_FLOOD_1D  ='TRIP_SGFLOOD_1D.nc'
 CHARACTER(LEN=18), PARAMETER         :: YFILE_FLOOD_HD  ='TRIP_SGFLOOD_HD.nc'
 CHARACTER(LEN=18)                    :: YFILE_FLOOD_READ
!
 CHARACTER(LEN=20)                    :: YVAR 
!
LOGICAL,DIMENSION(:,:),ALLOCATABLE   :: LMASK_NATURE
!
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZSAND, ZCLAY, ZSILT, ZPERM
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZGRCN, ZSEQ, ZMASK, ZNUM_BAS, &
                                          ZDR_AREA, ZELEV  
!
INTEGER,DIMENSION(:,:),ALLOCATABLE   :: IGRCN,INEXTX,INEXTY
!
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZREAD
REAL,DIMENSION(:,:,:),ALLOCATABLE    :: ZREAD3D
REAL,DIMENSION(:),ALLOCATABLE        :: ZTAUG
!
INTEGER           :: ILUOUT         ! output listing logical unit
!
!
REAL    :: ZLONMIN   ! minimum longitude (degrees)
REAL    :: ZLONMAX   ! maximum longitude (degrees)
REAL    :: ZLATMIN   ! minimum latitude  (degrees)
REAL    :: ZLATMAX   ! maximum latitude  (degrees)
REAL    :: ZGRID_RES ! 1° or 0.5° resolution
INTEGER :: ILON      ! number of points in longitude
INTEGER :: ILAT      ! number of points in latitude
!
INTEGER :: ILON_G, ILON_DEB, ILON_END
INTEGER :: ILAT_G, ILAT_DEB, ILAT_END
!
INTEGER :: IWORK, IFLOOD, I, J, INI, ILATF
!
LOGICAL :: LMASKLON, LMASKLAT
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
IF (LHOOK) CALL DR_HOOK('PREP_TRIP',0,ZHOOK_HANDLE)
 CALL INIT_TRIP_PAR
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'        PREP TRIP            '
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
IF(LFLOODT)THEN
  IF(CGROUNDW=='DEF') &
      WRITE(ILUOUT,*)'! Attention, you use the flooding scheme without the groundwater scheme !!!'       
  IF(CVIT /= 'VAR')THEN
    WRITE(ILUOUT,*)'! You cannot use the flooding scheme without the variable velocity scheme !!!'
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
!-------------------------------------------------------------------------------
! * Read TRIP grid configuration
!-------------------------------------------------------------------------------
!
 CALL READ_NAM_GRID_TRIP(HPROGRAM)
!
 CALL GET_GRID_CONF_TRIP_n(ZLONMIN,ZLONMAX,ZLATMIN,ZLATMAX,ZGRID_RES,ILON,ILAT)
!
IF(ZGRID_RES==1.0)THEN
!
  WRITE(ILUOUT,*)'! 1° by 1° TRIP run !!!'   
!
  ILON_G = 360
  ILAT_G = 180
  ILATF  = 31
  YFILE_READ = YFILE_PARAM_1D
  YFILE_READ_INIT = YFILE_INIT_1D
  YFILE_FLOOD_READ = YFILE_FLOOD_1D
!
ELSEIF(ZGRID_RES==0.5)THEN
!
  WRITE(ILUOUT,*)'! 0.5° by 0.5° TRIP run !!!'   
!
  ILON_G = 720
  ILAT_G = 360
  ILATF  = 61
  YFILE_READ = YFILE_PARAM_HD
  YFILE_READ_INIT = YFILE_INIT_HD
  YFILE_FLOOD_READ = YFILE_FLOOD_HD
!
  IF(XRATMED==1.4)THEN
     WRITE(ILUOUT,*)'! meandering ratio is the same than at 1° resolution   !!!'   
     WRITE(ILUOUT,*)'! if you want another value change XRATMED in namelist !!!' 
  ENDIF 
!
ELSE
!
  WRITE(ILUOUT,*)'! The resolution of the TRIP run is not good !!!'   
  WRITE(ILUOUT,*)'           ! Should be 1° or 0.5° !!!           '  
  STOP
!  
ENDIF
!
LMASKLON=.FALSE.
IF(ZLONMIN/=-180..OR.ZLONMAX/=180.)LMASKLON=.TRUE.
!
LMASKLAT=.FALSE.
IF(ZLATMIN/=-180..OR.ZLATMAX/=180.)LMASKLAT=.TRUE.
!
!-------------------------------------------------------------------------------
!Allocate arguments
!-------------------------------------------------------------------------------
!
ALLOCATE(ZREAD   (ILON_G,ILAT_G))
!
ALLOCATE(ZGRCN    (ILON,ILAT))       
ALLOCATE(ZMASK    (ILON,ILAT))
ALLOCATE(XLEN     (ILON,ILAT))
ALLOCATE(XAREA    (ILON,ILAT))
ALLOCATE(ZSEQ     (ILON,ILAT))   
ALLOCATE(XSURF_STO(ILON,ILAT))
ALLOCATE(ZNUM_BAS (ILON,ILAT))
ALLOCATE(ZDR_AREA (ILON,ILAT))
ALLOCATE(ZELEV    (ILON,ILAT))
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
  ALLOCATE(XHC_BED     (ILON,ILAT))
  ALLOCATE(XN_FLOOD    (ILON,ILAT))
  ALLOCATE(XFLOOD_STO  (ILON,ILAT))
  ALLOCATE(XHFLOOD     (ILON,ILAT))
  ALLOCATE(XFFLOOD     (ILON,ILAT))
ELSE
  ALLOCATE(XHC_BED   (0,0))
  ALLOCATE(XN_FLOOD  (0,0))
  ALLOCATE(XFLOOD_STO(0,0))
  ALLOCATE(XHFLOOD   (0,0))
  ALLOCATE(XFFLOOD   (0,0))
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
! * Create param and init file
!-------------------------------------------------------------------------------
!
YTITLE    = 'TRIP parameters for a run'
YTIMEUNIT = '-'
 CALL INIT_PARAM_TRIP_n(ILUOUT,YFILE_PARAM,ILON,ILAT,YTITLE,YTIMEUNIT)
!
YTITLE   ='TRIP prep historical variable'
YTIMEUNIT='-'
 CALL INIT_RESTART_TRIP_n(ILUOUT,YFILE_PREP,ILON,ILAT,YTITLE,YTIMEUNIT,.FALSE.)
!
!-------------------------------------------------------------------------------
! * Compute the mask of the run
!-------------------------------------------------------------------------------        
!
ILON_DEB = INT(ZLONMIN/ZGRID_RES) + ILON_G/2 + 1
ILON_END = INT(ZLONMAX/ZGRID_RES) + ILON_G/2  
ILAT_DEB = INT(ZLATMIN/ZGRID_RES) + ILAT_G/2 + 1
ILAT_END = INT(ZLATMAX/ZGRID_RES) + ILAT_G/2
!
!-------------------------------------------------------------------------------
! * Compute and Read TRIP parameter
!-------------------------------------------------------------------------------        
!
ALLOCATE(IGRCN (ILON_G,ILAT_G))
ALLOCATE(INEXTX(ILON_G,ILAT_G))
ALLOCATE(INEXTY(ILON_G,ILAT_G))
!
! * Basin ID
!
YVAR ='NUM_BAS'
 CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
ZNUM_BAS = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
! * Drainage Area
!
YVAR ='DR_AREA'
 CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
ZDR_AREA = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
! * Elevation
!
YVAR ='ELEV'
 CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
ZELEV = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
! * Flow direction
!
YVAR ='FLOWDIR'
 CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
ZGRCN=ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
WHERE(ZREAD==XTRIP_UNDEF)ZREAD=0.0
IGRCN = INT(ZREAD)
!
ZMASK=0.0
WHERE(ZGRCN/=XTRIP_UNDEF)ZMASK=1.0
!        
! * Set the distance between grids with the meandering ratio
!
 CALL SETNEXT(ILON_G,ILAT_G,IGRCN,INEXTX,INEXTY)
!
YVAR ='RIVLEN'
 CALL SETLEN(ILON_G,ILAT_G,IGRCN,INEXTX,INEXTY,XRATMED,ZREAD)
XLEN = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
DEALLOCATE(IGRCN )
DEALLOCATE(INEXTX)
DEALLOCATE(INEXTY)
!
! * Domain test
!
ALLOCATE(IGRCN (ILON,ILAT))
ALLOCATE(INEXTX(ILON,ILAT))
ALLOCATE(INEXTY(ILON,ILAT))
!
WHERE(ZGRCN(:,:)<XTRIP_UNDEF)
      IGRCN(:,:)=INT(ZGRCN(:,:))
ELSEWHERE
      IGRCN(:,:)=0
ENDWHERE
!
 CALL SETNEXT(ILON,ILAT,IGRCN(:,:),INEXTX(:,:),INEXTY(:,:),LMASKLON,LMASKLAT)
!
! * Store some param
!
YVAR ='FLOWDIR'
ZGRCN(:,:)=REAL(IGRCN(:,:))
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,ZGRCN)
YVAR ='RIVLEN'
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XLEN)
YVAR ='NUM_BAS'
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,ZNUM_BAS)
YVAR ='DR_AREA'
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,ZDR_AREA)
YVAR ='ELEV'
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,ZELEV)
!
DEALLOCATE(IGRCN )
DEALLOCATE(INEXTX)
DEALLOCATE(INEXTY)
DEALLOCATE(ZDR_AREA)
DEALLOCATE(ZELEV)
DEALLOCATE(ZNUM_BAS)
!
! * River sequence values read
!
YVAR ='RIVSEQ'
 CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
ZSEQ = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
 CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,ZSEQ)
!
! * Set area size
!
 CALL SETAREA(ILAT,ZLATMIN,ZGRID_RES,XAREA)
!
! * Initial river storage
!
YVAR ='SURF_STO'
 CALL READ_TRIP(ILUOUT,YFILE_READ_INIT,YVAR,ZREAD)
XSURF_STO = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!        
 CALL WRITE_TRIP(ILUOUT,YFILE_PREP,YVAR,ZMASK,XSURF_STO)
!
! * Calculate the groundwater transfert time
!
IF(CGROUNDW=='VAR')THEN
!
  ALLOCATE(ZSAND (ILON,ILAT))
  ALLOCATE(ZCLAY (ILON,ILAT))
  ALLOCATE(ZSILT (ILON,ILAT))
  ALLOCATE(ZPERM (ILON,ILAT))
  ALLOCATE(ZTAUG (3))
!
  YVAR ='CLAY'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  ZCLAY = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
  YVAR ='SAND'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  ZSAND = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
  ZTAUG(1)=XDATA_TAUG
  ZTAUG(2)=1.5*XDATA_TAUG
  ZTAUG(3)=2.0*XDATA_TAUG
!
  WHERE(ZCLAY(:,:)>=0.0.AND.ZSAND(:,:)>=0.0.AND.ZCLAY(:,:)/=XTRIP_UNDEF)
        ZSILT(:,:)=1.0-(ZCLAY(:,:)+ZSAND(:,:))
        XTAUG(:,:)=ZTAUG(2)
        WHERE(ZCLAY(:,:)>ZSILT(:,:).AND.ZCLAY(:,:)>ZSAND(:,:))XTAUG(:,:)=ZTAUG(3)
        WHERE(ZSAND(:,:)>ZCLAY(:,:).AND.ZSAND(:,:)>ZSILT(:,:))XTAUG(:,:)=ZTAUG(1)
  ELSEWHERE
        XTAUG(:,:)=XTRIP_UNDEF
  ENDWHERE
!
  YVAR ='PERM'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  ZPERM = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!         
  WHERE(ZPERM(:,:)==1)XTAUG(:,:)=0.0
!          
  YVAR ='TAUG'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XTAUG)
!
  DEALLOCATE(ZSAND)
  DEALLOCATE(ZCLAY)
  DEALLOCATE(ZSILT)
  DEALLOCATE(ZTAUG)
  DEALLOCATE(ZPERM)
!
ELSEIF(CGROUNDW=='CST')THEN
!          
  ALLOCATE(ZPERM (ILON,ILAT))
!
  YVAR ='PERM'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  ZPERM = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!  
  XTAUG(:,:)=XDATA_TAUG
  WHERE(ZPERM(:,:)==1)XTAUG(:,:)=0.0
!          
  YVAR ='TAUG'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XTAUG)
!  
  DEALLOCATE(ZPERM)
!
ENDIF
!
! * Groundwater water storage
!
IF(CGROUNDW/='DEF')THEN
!
  YVAR ='GROUND_STO'
  CALL READ_TRIP(ILUOUT,YFILE_READ_INIT,YVAR,ZREAD)
  XGROUND_STO = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
  CALL WRITE_TRIP(ILUOUT,YFILE_PREP,YVAR,ZMASK,XGROUND_STO)          
!
ENDIF
!
! * Variable velocity scheme parameters
!
IF(CVIT == 'VAR')THEN
!
  YVAR ='N_RIV'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  XN = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XN)
!
  YVAR ='WIDTHRIV'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  XWIDTH = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XWIDTH)
!
  YVAR ='SLOPERIV'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  XSLOPEBED = MAX(ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END),1.E-5)
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XSLOPEBED)  
!  
ENDIF 
!
! * Calculate floodplains parameters
!
IF(LFLOODT)THEN
!
  YVAR ='RIVDEPTH'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  XHC_BED = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  WHERE(XHC_BED(:,:)==0.0)XHC_BED(:,:)=XTRIP_UNDEF
!
  YVAR ='NFLOOD'
  CALL READ_TRIP(ILUOUT,YFILE_READ,YVAR,ZREAD)
  XN_FLOOD = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
!
  ALLOCATE(ZREAD3D(ILON_G,ILAT_G,NTRIPTAB))
!  
  ALLOCATE(XTAB_F (ILON,ILAT,NTRIPTAB))      
  ALLOCATE(XTAB_H (ILON,ILAT,NTRIPTAB))      
  ALLOCATE(XTAB_VF(ILON,ILAT,NTRIPTAB))  
!  
  XTAB_F (:,:,:)= XTRIP_UNDEF
  XTAB_H (:,:,:)= XTRIP_UNDEF
  XTAB_VF(:,:,:)= XTRIP_UNDEF
!
  YVAR ='TABF'
  CALL READ_TRIP(ILUOUT,YFILE_FLOOD_READ,YVAR,ZREAD3D(:,ILATF:ILAT_G,:))
  XTAB_F = ZREAD3D(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END,:)
  YVAR ='TABH'
  CALL READ_TRIP(ILUOUT,YFILE_FLOOD_READ,YVAR,ZREAD3D(:,ILATF:ILAT_G,:))
  XTAB_H = ZREAD3D(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END,:)
!
  CALL SET_SUBGRID_FLOOD(ILON,ILAT,XAREA,XTAB_H,XTAB_F,XTAB_VF)
!
  DEALLOCATE(ZREAD3D)
!
  IF(.NOT.ASSOCIATED(GMASK))THEN
    ALLOCATE(GMASK(ILON,ILAT))
    GMASK(:,:)=.TRUE.
    WHERE(ZMASK(:,:)==0.0)GMASK(:,:)=.FALSE.
  ENDIF
  DO I=1,ILON
     DO J=1,ILAT
        IF(GMASK(I,J))THEN
          IF(COUNT(XTAB_H(I,J,:)<XTRIP_UNDEF-1.0)<2)THEN
            XTAB_F (I,J,:)= XTRIP_UNDEF
            XTAB_H (I,J,:)= XTRIP_UNDEF
            XTAB_VF(I,J,:)= XTRIP_UNDEF
            XHC_BED(I,J)  = XTRIP_UNDEF
          ENDIF
          IF(XTAB_F (I,J,2)>=0.98)THEN                  
            XTAB_F (I,J,:)= XTRIP_UNDEF
            XTAB_H (I,J,:)= XTRIP_UNDEF
            XTAB_VF(I,J,:)= XTRIP_UNDEF
            XHC_BED(I,J)  = XTRIP_UNDEF
          ENDIF
        ENDIF
        IF(XHC_BED(I,J)==XTRIP_UNDEF)THEN
          XTAB_F  (I,J,:) = XTRIP_UNDEF
          XTAB_H  (I,J,:) = XTRIP_UNDEF
          XTAB_VF (I,J,:) = XTRIP_UNDEF
          XN_FLOOD(I,J)  = XTRIP_UNDEF
        ENDIF
     ENDDO
  ENDDO
!
  WHERE(XTAB_H(:,:,1)==XTRIP_UNDEF)
        XHC_BED (:,:)=XTRIP_UNDEF
        XN_FLOOD(:,:)=XTRIP_UNDEF
  ENDWHERE
!
  YVAR ='RIVDEPTH'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XHC_BED)
!  
  YVAR ='NFLOOD'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XN_FLOOD)
!
  YVAR ='TABF'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XTAB_F)
  YVAR ='TABH'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XTAB_H)  
  YVAR ='TABVF'
  CALL WRITE_TRIP(ILUOUT,YFILE_PARAM,YVAR,ZMASK,XTAB_VF)
!
! Floodplains initial variables
!
  YVAR ='FLOOD_STO'
  CALL READ_TRIP(ILUOUT,YFILE_READ_INIT,YVAR,ZREAD)
  XFLOOD_STO = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  CALL WRITE_TRIP(ILUOUT,YFILE_PREP,YVAR,ZMASK,XFLOOD_STO)          
!
  YVAR ='FFLOOD_T'
  CALL READ_TRIP(ILUOUT,YFILE_READ_INIT,YVAR,ZREAD)
  XFFLOOD = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  CALL WRITE_TRIP(ILUOUT,YFILE_PREP,YVAR,ZMASK,XFFLOOD)          
!
  YVAR ='HFLOOD_T'
  CALL READ_TRIP(ILUOUT,YFILE_READ_INIT,YVAR,ZREAD)
  XHFLOOD = ZREAD(ILON_DEB:ILON_END,ILAT_DEB:ILAT_END)
  CALL WRITE_TRIP(ILUOUT,YFILE_PREP,YVAR,ZMASK,XHFLOOD)          
!  
ENDIF
!
DEALLOCATE(ZREAD)
!
!-------------------------------------------------------------------------------
! * END
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT,*)''
WRITE(ILUOUT,*)'        END PREP TRIP            '
WRITE(ILUOUT,*)''
IF (LHOOK) CALL DR_HOOK('PREP_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE PREP_TRIP
