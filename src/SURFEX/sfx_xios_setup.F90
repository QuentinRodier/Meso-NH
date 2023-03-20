!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_SETUP(IM, UG, KCOMM, KLUOUT, KYEAR, KMONTH, KDAY, PTIME, PTSTEP, &
                          KDIM1, KDIM2, KEXT1, PSW_BANDS, PCLAT, PCLON, KXINDEX,  &
                          ODXMASK, KMASKNAT, KMASKSEA, KMASKWAT, KMASKTOWN        )
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Initialize all Surfex context for XIOS (calendar, grids, masks, model time step, ...)
!!
!!
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     LXIOS, YXIOS_CONTEXT
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> ; cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2015
!!
!!     D. St-Martin 02/2018 : add LXIOS_INVERT_LEVELS
!!     A. Alias     06/2018 - add specification of the month for the reference date
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n,        ONLY : ISBA_MODEL_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_DATA_COVER_PAR,  ONLY : NVEGTYPE
!
USE MODD_XIOS
!
#ifdef WXIOS 
!
USE XIOS, ONLY : XIOS_CONTEXT_INITIALIZE, XIOS_GET_HANDLE,   &
     XIOS_SET_CURRENT_CONTEXT, XIOS_SET_TIMESTEP, XIOS_DATE, &
     XIOS_DURATION, XIOS_DEFINE_CALENDAR, XIOS_GETVAR,       &
     XIOS_SOLVE_INHERITANCE, XIOS_GETVAR
!
USE MODI_SFX_XIOS_SET_DOMAIN
USE MODI_SET_AXIS
!
#endif
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!
!   Arguments
!
TYPE(ISBA_MODEL_t),    INTENT(INOUT) :: IM
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER,               INTENT(IN) :: KCOMM  ! Communicator
! 
INTEGER,               INTENT(IN) :: KLUOUT    ! Listing logical unit number
INTEGER,               INTENT(IN) :: KYEAR     ! current year (UTC)
INTEGER,               INTENT(IN) :: KMONTH    ! current month (UTC)
INTEGER,               INTENT(IN) :: KDAY      ! current day (UTC)
REAL,                  INTENT(IN) :: PTIME     ! current time since midnight (UTC,s)
REAL,                  INTENT(IN) :: PTSTEP    ! model time step 
INTEGER,               INTENT(IN) :: KDIM1     ! Geometry param. (see  sfx_set_domain)
INTEGER,               INTENT(IN) :: KDIM2     ! Geometry param. (see  sfx_set_domain)
INTEGER,               INTENT(IN) :: KEXT1     ! Geometry param. (see  sfx_set_domain)
!
REAL,DIMENSION(:),     INTENT(IN) :: PSW_BANDS
REAL,DIMENSION(:,:),   INTENT(IN) :: PCLAT      ! Lat corners "
REAL,DIMENSION(:,:),   INTENT(IN) :: PCLON      ! Lon corners "
INTEGER, DIMENSION(:), INTENT(IN) :: KXINDEX    ! index of proc cells in global grid
LOGICAL, DIMENSION(:), INTENT(IN) :: ODXMASK    ! Cells mask
INTEGER, DIMENSION(:), INTENT(IN) :: KMASKNAT   ! Masks for the whole MPI task
INTEGER, DIMENSION(:), INTENT(IN) :: KMASKSEA
INTEGER, DIMENSION(:), INTENT(IN) :: KMASKWAT
INTEGER, DIMENSION(:), INTENT(IN) :: KMASKTOWN
!
!  Local variables
!
REAL, DIMENSION(  IM%O%NGROUND_LAYER) :: ZMID_DG
REAL, DIMENSION(2,IM%O%NGROUND_LAYER) :: ZBOUND
!
#ifdef WXIOS
INTEGER               :: IX, JX
TYPE(XIOS_DURATION)   :: TDTIME ! Time-step 'a la XIOS'
INTEGER               :: INHOURS,INMINUTES,INSECONDS
INTEGER               :: IREFYEAR,IREFMON
#endif
!
INTEGER            :: JL
INTEGER            :: ITMP
INTEGER            :: IRESP
LOGICAL            :: LL_INVERT_LEVELS
!
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP',0,ZHOOK_HANDLE)
!
IF (.NOT. LXIOS ) THEN 
   CALL DR_HOOK('SFX_XIOS_SETUP',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
#ifndef WXIOS
!
IF (LXIOS) THEN 
   CALL ABOR1_SFX('SFX_XIOS_INIT_CONTEXT : cannot setup : Surfex was compiled without XIOS support')
ELSE
   LXIOS_DEF_CLOSED=.TRUE.
ENDIF
!
#else 
!
LXIOS_DEF_CLOSED=.FALSE.
!
! -----------------------------------------------------------------------------
!
!       Set Xios context to Surfex's one
!
! -----------------------------------------------------------------------------
!
!$OMP SINGLE
CALL XIOS_CONTEXT_INITIALIZE(YXIOS_CONTEXT,KCOMM)
CALL XIOS_GET_HANDLE(YXIOS_CONTEXT,TXIOS_CONTEXT)
CALL XIOS_SET_CURRENT_CONTEXT(YXIOS_CONTEXT)
!
! -----------------------------------------------------------------------------
!
!      Set date for XIOS
!
! -----------------------------------------------------------------------------
!
INHOURS   = INT(PTIME/3600)
INMINUTES = INT((PTIME - INHOURS*3600)/60)
INSECONDS = INT(PTIME - INHOURS*3600 - INMINUTES*60)
!
! Calendar init 
!
IF (.NOT.(XIOS_GETVAR('ref_year',IREFYEAR))) IREFYEAR=1850
IF (.NOT.(XIOS_GETVAR('ref_mon',IREFMON)))   IREFMON=1
CALL XIOS_DEFINE_CALENDAR("Gregorian", &
     start_date=xios_date(KYEAR,KMONTH,KDAY,INHOURS,INMINUTES,INSECONDS), &
     time_origin=xios_date(IREFYEAR,IREFMON,1,0,0,0))
!
! -----------------------------------------------------------------------------
!
!   Set duration between 2 calls to write_diag_surf
!
! -----------------------------------------------------------------------------
!
IF (XIOS_GETVAR("timesteps_between_samples",ITMP)) NBASE_XIOS_FREQ=ITMP
TDTIME%SECOND = INT(PTSTEP*NBASE_XIOS_FREQ)
CALL XIOS_SET_TIMESTEP(TDTIME)
!$OMP END SINGLE
!
!
!
! -----------------------------------------------------------------------------
!
!   Set option for inverting levels
!
! -----------------------------------------------------------------------------
!
IF (XIOS_GETVAR("invert_levels", LL_INVERT_LEVELS)) THEN
  LXIOS_INVERT_LEVELS = LL_INVERT_LEVELS
ENDIF
! ---------------------------------------------------------------------------------
!
!   Declare a 'full' domain and one domain per tile 
!
! ---------------------------------------------------------------------------------
!
 CALL SFX_XIOS_SET_DOMAIN(UG%G%CGRID, "FULL"  , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
                          UG%G%XLON, UG%G%XLAT, PCLON, PCLAT )
 CALL SFX_XIOS_SET_DOMAIN(UG%G%CGRID, "SEA"   , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
                          UG%G%XLON, UG%G%XLAT, PCLON, PCLAT, KMASK=KMASKSEA )
 CALL SFX_XIOS_SET_DOMAIN(UG%G%CGRID, "NATURE", KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
                          UG%G%XLON, UG%G%XLAT, PCLON, PCLAT, KMASK=KMASKNAT)
 CALL SFX_XIOS_SET_DOMAIN(UG%G%CGRID, "WATER" , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
                          UG%G%XLON, UG%G%XLAT , PCLON, PCLAT, KMASK=KMASKWAT )
 CALL SFX_XIOS_SET_DOMAIN(UG%G%CGRID, "TOWN"  , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
                          UG%G%XLON, UG%G%XLAT , PCLON, PCLAT, KMASK=KMASKTOWN )
!
! ---------------------------------------------------------------------------------
!
! Declare axes from atmosphere
!
! ---------------------------------------------------------------------------------
!
!YSWBAND_DIM_NAME="swband"
CALL SET_AXIS(YSWBAND_DIM_NAME,PVALUE=PSW_BANDS,CDUNITS='m')
!
! ---------------------------------------------------------------------------------
!
! Declare axes from surfex
!
! ---------------------------------------------------------------------------------
!
!YPATCH_DIM_NAME="patch"
CALL SET_AXIS(YPATCH_DIM_NAME,KSIZE=IM%O%NPATCH)
!
!YPATCHES_DIM_NAME="patches"
CALL SET_AXIS(YPATCHES_DIM_NAME,KSIZE=IM%O%NPATCH)
!
!YSNOW_LAYER_DIM_NAME="snow_layer"
CALL SET_AXIS(YSNOW_LAYER_DIM_NAME,KSIZE=IM%NPE%AL(1)%TSNOW%NLAYER)
!
IF(IM%O%CISBA/='DIF'.OR.ANY(IM%DTV%LDATA_DG))THEN
!
! YGROUND_LAYER_DIM_NAME="ground_layer"
  CALL SET_AXIS(YGROUND_LAYER_DIM_NAME,KSIZE=IM%O%NGROUND_LAYER)
!
! YTG_LAYER_DIM_NAME="ground_layer_for_temperature"
  CALL SET_AXIS(YTG_LAYER_DIM_NAME,KSIZE=SIZE(IM%NPE%AL(1)%XTG,2))
!
ELSE
!
  ZBOUND(1,1)=0.0  
  ZBOUND(2,:)=IM%O%XSOILGRID(:)
  DO JL=2,IM%O%NGROUND_LAYER
     ZBOUND(1,JL)=ZBOUND(2,JL-1)
  ENDDO
!
  ZMID_DG(1)=0.5*IM%O%XSOILGRID(1)
  DO JL=2,IM%O%NGROUND_LAYER
     ZMID_DG(JL)=0.5*(IM%O%XSOILGRID(JL-1)+IM%O%XSOILGRID(JL))
  ENDDO
!  
  !YGROUND_LAYER_DIM_NAME="ground_layer"
  CALL SET_AXIS(YGROUND_LAYER_DIM_NAME,PVALUE=ZMID_DG,PBOUNDS=ZBOUND,CDUNITS='m',CDPOSITIVE='down')
!
  !YTG_LAYER_DIM_NAME="ground_layer_for_temperature"
  CALL SET_AXIS(YTG_LAYER_DIM_NAME,PVALUE=ZMID_DG,PBOUNDS=ZBOUND,CDUNITS='m',CDPOSITIVE='down')
!
ENDIF
!
!YVEGTYPE_DIM_NAME="vegtype"
CALL SET_AXIS(YVEGTYPE_DIM_NAME,KSIZE=NVEGTYPE)
!
!YSOIL_CARBON_POOL_DIM_NAME="soil_carbon_pools"
CALL SET_AXIS(YSOIL_CARBON_POOL_DIM_NAME,KSIZE=IM%O%NNSOILCARB)
!
!YLANDUSE_TILES_DIM_NAME="landusetype4"
CALL SET_AXIS(YLANDUSE_TILES_DIM_NAME,KSIZE=NLUT)
!
CALL XIOS_SOLVE_INHERITANCE()
! ---------------------------------------------------------------------------------
!
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFX_XIOS_SETUP
