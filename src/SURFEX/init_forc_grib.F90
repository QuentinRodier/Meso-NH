!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_FORC_GRIB(YSC,HPROGRAM)
!     #############################################################
!
!!****  *INIT_FORC_GRIB* - routine to initialize useful variables 
!!        to interpolate forcing variables
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!   From routine INIT_PGD_SURF_ATM
!!
!!    AUTHOR
!!    ------
!!	B. Vincendon       *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n,       ONLY : SURFEX_t
!
USE MODD_SURF_ATM,       ONLY : XCO2UNCPL
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_DATA_COVER,     ONLY : LCLIM_LAI, NECO2_START_YEAR, NECO2_END_YEAR  
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODN_PREP_SURF_ATM,  ONLY : LWRITE_EXTERN
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_SSO
USE MODI_DEFAULT_CH_SURF_ATM
USE MODI_DEFAULT_DIAG_SURF_ATM
USE MODI_READ_DEFAULT_SURF_ATM_n
USE MODI_READ_SURF_ATM_CONF_n
USE MODI_READ_NAM_PREP_SURF_n
USE MODI_READ_SURF
USE MODI_SUNPOS
USE MODI_GET_SIZE_FULL_n
USE MODI_READ_COVER_n
USE MODI_READ_SSO_n
USE MODI_SUBSCALE_Z0EFF
USE MODI_READ_SSO_CANOPY_n
USE MODI_READ_DUMMY_n
USE MODI_READ_GRID
USE MODI_READ_GRIDTYPE
USE MODI_END_IO_SURF_n
USE MODI_PREP_CTRL_SURF_ATM
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_TSURF
USE MODI_INIT_CHEMICAL_n
USE MODI_CH_INIT_DEPCONST
USE MODI_CH_INIT_EMISSION_n
USE MODI_CH_INIT_SNAP_n
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
USE MODI_ALLOC_DIAG_SURF_ATM_n
USE MODI_GET_1D_MASK
USE MODI_INI_DATA_COVER
USE MODI_INIT_INLAND_WATER_n
USE MODI_INIT_NATURE_n
USE MODI_INIT_SEA_n
USE MODI_INIT_TOWN_n
USE MODI_READ_ARRANGE_COVER
USE MODI_READ_COVER_GARDEN
USE MODI_READ_ECO2_IRRIG
USE MODI_READ_LCLIM_LAI
USE MODI_READ_LECOCLIMAP
USE MODI_SURF_VERSION
USE MODI_GET_LUOUT
USE MODI_SET_SURFEX_FILEIN
!
USE MODI_INIT_CPL_GCM_n
!
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=3)  :: YREAD
!
INTEGER           :: IRESP    ! error return code
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IVERSION, IBUGFIX       ! surface version
!
REAL, DIMENSION(:), ALLOCATABLE :: ZZ0VEG
!
INTEGER :: ISIZE_FULL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_FORC_GRIB',0,ZHOOK_HANDLE)
!
!
CPROGNAME=HPROGRAM
!
CALL SURF_VERSION
!
!-------------------------------------------------------------------------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
CALL DEFAULT_SSO(YSC%USS%CROUGH, YSC%USS%XFRACZ0, YSC%USS%XCOEFBE, &
                 YSC%USS%LDSV,YSC%USS%LDSH,YSC%USS%LDSL)
CALL DEFAULT_CH_SURF_ATM(YSC%CHU%CCHEM_SURF_FILE, YSC%CHU%LCH_SURF_EMIS)
CALL DEFAULT_DIAG_SURF_ATM(YSC%DUO%N2M, YSC%DUO%LT2MMW, YSC%DUO%LSURF_BUDGET,&
                           YSC%DUO%L2M_MIN_ZS, YSC%DUO%LRAD_BUDGET, YSC%DUO%LCOEF,&
                           YSC%DUO%LSURF_VARS, YSC%DUO%LSURF_BUDGETC, &
                           YSC%DUO%LRESET_BUDGETC, YSC%DUO%LSELECT, &
                           YSC%DUO%LPROVAR_TO_DIAG, YSC%DUO%LDIAG_GRID, &
                           YSC%DUO%LFRAC, YSC%DUO%XDIAG_TSTEP, &
                           YSC%DUO%LSNOWDIMNC, YSC%DUO%LRESETCUMUL, YSC%DUO%LDIAG_MIP)                       
ENDIF
!
!        0.2. Defaults from file header
!    
CALL READ_DEFAULT_SURF_ATM_n(YSC%CHU, YSC%DUO, YSC%USS, HPROGRAM)
!
!*       1.     Reading of configuration
!               ------------------------
!
!        1.1. general options (diagnostics, etc...)
!
CALL READ_SURF_ATM_CONF_n(YSC%CHU, YSC%DUO, YSC%USS, HPROGRAM)
!
IF(XCO2UNCPL/=XUNDEF)THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!          WARNING    WARNING               !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!! Decoupling between CO2 for photosynthesis !!!' 
  WRITE(ILUOUT,*)'!!! and atmospheric CO2 activated             !!!'
  WRITE(ILUOUT,*)'!!! In NAM_SURF_ATM XCO2UNCPL =',XCO2UNCPL,'  !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'        
ENDIF
!
!        1.2. Date
!
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
CALL READ_SURF(HPROGRAM,'DTCUR',YSC%U%TTIME,IRESP)
CALL END_IO_SURF_n(HPROGRAM)
LWRITE_EXTERN = .FALSE.
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!        1.3. Schemes used
!
!         Initialisation for IO
!
CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
CALL READ_SURF(HPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,  IRESP)
CALL END_IO_SURF_n(HPROGRAM)
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')                
!
CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  CALL READ_SURF(HPROGRAM,'STORAGETYPE',YREAD,IRESP)
ENDIF
!         reading
!
CALL READ_SURF(HPROGRAM,'SEA   ',YSC%U%CSEA   ,IRESP)
CALL READ_SURF(HPROGRAM,'WATER ',YSC%U%CWATER ,IRESP)
CALL READ_SURF(HPROGRAM,'NATURE',YSC%U%CNATURE,IRESP)
CALL READ_SURF(HPROGRAM,'TOWN  ',YSC%U%CTOWN  ,IRESP)
!
! 
CALL READ_SURF(HPROGRAM,'DIM_SEA   ',YSC%U%NDIM_SEA,   IRESP)
CALL READ_SURF(HPROGRAM,'DIM_NATURE',YSC%U%NDIM_NATURE,IRESP)
CALL READ_SURF(HPROGRAM,'DIM_WATER ',YSC%U%NDIM_WATER, IRESP)
CALL READ_SURF(HPROGRAM,'DIM_TOWN  ',YSC%U%NDIM_TOWN,  IRESP)
!
CALL READ_LECOCLIMAP(HPROGRAM,YSC%U%LECOCLIMAP,YSC%U%LECOSG)
CALL READ_ARRANGE_COVER(HPROGRAM,YSC%U%LWATER_TO_NATURE,YSC%U%LTOWN_TO_ROCK, &
     YSC%U%LTOWN_TO_COVER, YSC%U%NREPLACE_COVER)
CALL READ_COVER_GARDEN(HPROGRAM,YSC%U%LGARDEN)
!
!* reads if climatological LAI is used or not for ecoclimap2. If not, looks for year to be used.
CALL READ_LCLIM_LAI(HPROGRAM,LCLIM_LAI)
IF (.NOT. LCLIM_LAI .AND. YSC%U%TTIME%TDATE%YEAR >= NECO2_START_YEAR &
                    .AND. YSC%U%TTIME%TDATE%YEAR <= NECO2_END_YEAR   ) YSC%DTCO%NYEAR=YSC%U%TTIME%TDATE%YEAR
CALL INI_DATA_COVER(YSC%DTCO, YSC%U)
CALL READ_ECO2_IRRIG(YSC%DTCO, HPROGRAM)
!
!*       2.     Cover fields and grid:
!               ---------------------
!
!        2.0. Get number of points on this proc
!
CALL GET_SIZE_FULL_n(HPROGRAM,YSC%U%NDIM_FULL,YSC%U%NSIZE_FULL,ISIZE_FULL)
YSC%U%NSIZE_FULL = ISIZE_FULL
!
!        2.1. Read cover
!
CALL READ_COVER_n(YSC%DTCO, YSC%U, HPROGRAM)
!
!        2.2. Read grid
!
ALLOCATE(YSC%UG%G%XLAT      (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%G%XLON      (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%G%XMESH_SIZE(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJPDIR(YSC%U%NSIZE_FULL))
CALL READ_GRID(HPROGRAM,YSC%UG%G,IRESP,YSC%USS%XZ0EFFJPDIR)
!
!
!
!*       2.5 Subgrid orography
!
CALL READ_SSO_n(YSC%U%NSIZE_FULL, YSC%U%XSEA, YSC%USS, HPROGRAM)
!
!*       2.6 Orographic roughness length
!
ALLOCATE(YSC%USS%XZ0EFFIP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFIM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0REL  (YSC%U%NSIZE_FULL))
!
ALLOCATE(ZZ0VEG(YSC%U%NSIZE_FULL))
ZZ0VEG(:) = 0.
!
CALL SUBSCALE_Z0EFF(YSC%USS,ZZ0VEG,.TRUE.)
!
DEALLOCATE(ZZ0VEG)
!
!*       2.7 Dummy fields
!
CALL READ_DUMMY_n(YSC%DUU,YSC%U%NSIZE_FULL, HPROGRAM)
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!
!
!-----------------------------------------------------------------------------------------------------
!
!*       4.     Initialization of masks for each surface
!               ----------------------------------------
!
!* number of geographical points
YSC%U%NSIZE_NATURE    = COUNT(YSC%U%XNATURE(:) > 0.0)
YSC%U%NSIZE_TOWN      = COUNT(YSC%U%XTOWN(:)   > 0.0)
YSC%U%NSIZE_WATER     = COUNT(YSC%U%XWATER(:)  > 0.0)
YSC%U%NSIZE_SEA       = COUNT(YSC%U%XSEA(:)    > 0.0)
!
ALLOCATE(YSC%U%NR_NATURE (YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%U%NR_TOWN   (YSC%U%NSIZE_TOWN  ))
ALLOCATE(YSC%U%NR_WATER  (YSC%U%NSIZE_WATER ))
ALLOCATE(YSC%U%NR_SEA    (YSC%U%NSIZE_SEA   ))
!
IF (YSC%U%NSIZE_SEA   >0)CALL GET_1D_MASK( YSC%U%NSIZE_SEA,    YSC%U%NSIZE_FULL, YSC%U%XSEA   , YSC%U%NR_SEA   )
IF (YSC%U%NSIZE_WATER >0)CALL GET_1D_MASK( YSC%U%NSIZE_WATER,  YSC%U%NSIZE_FULL, YSC%U%XWATER , YSC%U%NR_WATER )
IF (YSC%U%NSIZE_TOWN  >0)CALL GET_1D_MASK( YSC%U%NSIZE_TOWN,   YSC%U%NSIZE_FULL, YSC%U%XTOWN  , YSC%U%NR_TOWN  )
IF (YSC%U%NSIZE_NATURE>0)CALL GET_1D_MASK( YSC%U%NSIZE_NATURE, YSC%U%NSIZE_FULL, YSC%U%XNATURE, YSC%U%NR_NATURE)
!
IF (LHOOK) CALL DR_HOOK('INIT_FORC_GRIB',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FORC_GRIB


