!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 convert 2006/05/18 13:07:25
!-----------------------------------------------------------------
!###########################
MODULE MODI_INIT_FOR_CONVLFI
!###########################
!
!
INTERFACE
      SUBROUTINE INIT_FOR_CONVLFI(HINIFILE,HLUOUT)
!
CHARACTER(LEN=28),      INTENT(IN)    :: HINIFILE    ! file being read
CHARACTER(LEN=*),       INTENT(IN)    :: HLUOUT      ! output listing
!
END SUBROUTINE INIT_FOR_CONVLFI
END INTERFACE
END MODULE MODI_INIT_FOR_CONVLFI
!
!     ############################################
      SUBROUTINE INIT_FOR_CONVLFI(HINIFILE,HLUOUT)
!     ############################################
!
!!****  *INIT_FOR_CONVLFI * - light monitor to initialize the variables 
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize some variables   
!     necessary in the conversion program.
!
!!**  METHOD
!!    ------
!!      This initialization takes some parts of the whole initialization modules
!!    of monitor INIT: 
!!        geometry and dimensions from ini_sizen
!!        grids, metric coefficients, dates and times from set_grid
!!        reading of the pressure field
!!             
!!
!!    EXTERNAL
!!    --------
!!      INI_CST    : to initialize physical constants
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	I. Mallet       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                 20/02/01 
!!      J.-P. Pinty and D. Gazen 31/03/04 Add the 2D capability for V5D plots
!!    10/10/2011  J.Escobar call INI_PARAZ_ll
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CST
USE MODD_DIM_n
USE MODD_FIELD_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_LUNIT
USE MODD_TIME
USE MODD_TIME_n
USE MODD_VAR_ll, ONLY : NPROC
!
USE MODE_TIME
USE MODE_GRIDPROJ
USE MODE_GRIDCART
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_IO_ll
USE MODE_ll
!
USE MODI_GATHER_ll
USE MODI_INI_CST
!JUANZ
USE MODE_SPLITTINGZ_ll
!JUANZ
!
IMPLICIT NONE
!
!*       0.1   Arguments variables
!
CHARACTER(LEN=28),      INTENT(IN)    :: HINIFILE    ! file being read
CHARACTER(LEN=*),       INTENT(IN)    :: HLUOUT      ! output listing
!
!*       0.2   Local variables
!
INTEGER  :: IGRID,ILENCH,IRESP,ILUOUT          ! return code of file management
CHARACTER (LEN=16)     :: YRECFM               ! management
CHARACTER (LEN=100)    :: YCOMMENT             ! variables
CHARACTER (LEN=2)      :: YDIR                 !
INTEGER, DIMENSION(3)  :: ITDATE               ! date array
CHARACTER (LEN=40)     :: YTITLE               ! Title for date print
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZCOEF
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZJ      ! Jacobian
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZXHAT_ll    !  Position x in the conformal
                                                 ! plane (array on the complete domain)
REAL, DIMENSION(:), ALLOCATABLE   :: ZYHAT_ll    !   Position y in the conformal
                                                 ! plane (array on the complete domain)
REAL                         :: ZXHATM,ZYHATM    ! coordinates of mass point 
REAL                         :: ZLATORI, ZLONORI ! lat and lon of left-bottom point
!
INTEGER             :: IIU,IJU       ! Upper dimension in x,y direction (local)
INTEGER             :: IKU           ! Upper dimension in z direction
INTEGER             :: IINFO_ll      ! return code of // routines
INTEGER             :: IMASDEV       ! masdev of the file
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZE EACH MODEL SIZES AND DEPENDENCY (ini_sizen)
!              ------------------------------------------
!
!*       1.1   Read the geometry kind in the LFIFM file (Cartesian or spherical)
!
YRECFM = 'CARTESIAN'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LCARTESIAN,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*       1.2  Read configuration and dimensions in initial file and initialize
!             subdomain dimensions and parallel variables
!
YRECFM='IMAX'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,NIMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JMAX'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,NJMAX_ll,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM = 'L1D'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,L1D,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) THEN
  L1D=.FALSE.
  IF( (NIMAX_ll == 1).AND.(NJMAX_ll == 1) ) L1D=.TRUE.
ENDIF  
!
YRECFM = 'L2D'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,L2D,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) THEN
  L2D=.FALSE.
  IF( (NIMAX_ll /= 1).AND.(NJMAX_ll == 1) ) L2D=.TRUE.
ENDIF  
!
YRECFM = 'PACK'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LPACK,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) LPACK=.TRUE.
!
CALL SET_FMPACK_ll(L1D,L2D,LPACK)
!
YRECFM='KMAX'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,NKMAX,IGRID,ILENCH,YCOMMENT,IRESP)
!
CSPLIT ='BSPLITTING' ; NHALO = 1
CALL SET_SPLITTING_ll(CSPLIT)
CALL SET_JP_ll(1,JPHEXT,JPVEXT, NHALO)
CALL SET_DAD0_ll()
CALL SET_DIM_ll(NIMAX_ll, NJMAX_ll, NKMAX)
CALL SET_FMPACK_ll(L1D,L2D,LPACK)
CALL SET_LBX_ll('OPEN', 1)
CALL SET_LBY_ll('OPEN', 1)
CALL SET_XRATIO_ll(1, 1)
CALL SET_YRATIO_ll(1, 1)
CALL SET_XOR_ll(1, 1)
CALL SET_XEND_ll(NIMAX_ll+2*JPHEXT, 1)
CALL SET_YOR_ll(1, 1)
CALL SET_YEND_ll(NJMAX_ll+2*JPHEXT, 1)
CALL SET_DAD_ll(0, 1)
!JUANZ CALL INI_PARA_ll(IINFO_ll)
CALL INI_PARAZ_ll(IINFO_ll)
!
!*       1.4  Compute sizes of arrays of the extended sub-domain (ini_modeln)
!
IKU=NKMAX + 2*JPVEXT
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_DIM_PHYS_ll('B',NIMAX,NJMAX)
!
!-------------------------------------------------------------------------------
!
!*       2.    INITIALIZE GRIDS AND METRIC COEFFICIENTS (set_grid)
!              ---------------------
!
!        2.1  reading
!
YRECFM='LON0'   
YDIR='--'   
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLON0,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
YDIR='--'     
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLAT0,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'  
YDIR='--'       
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XBETA,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='XHAT'
ALLOCATE(XXHAT(IIU))
YDIR='XX'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='YHAT'
ALLOCATE(XYHAT(IJU))
YDIR='YY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='MASDEV' 
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='RPK'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XRPK,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LONORI'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LATORI'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (IMASDEV<=45) THEN
    CALL FMREAD(HINIFILE,'LONOR',HLUOUT,'--',XLONORI,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HINIFILE,'LATOR',HLUOUT,'--',XLATORI,IGRID,ILENCH,YCOMMENT,IRESP)
    ALLOCATE(ZXHAT_ll(NIMAX_ll+ 2 * JPHEXT),ZYHAT_ll(NJMAX_ll+2 * JPHEXT))
    CALL GATHERALL_FIELD_ll('XX',XXHAT,ZXHAT_ll,IRESP) !//
    CALL GATHERALL_FIELD_ll('YY',XYHAT,ZYHAT_ll,IRESP) !//
    ZXHATM = - 0.5 * (ZXHAT_ll(1)+ZXHAT_ll(2))
    ZYHATM = - 0.5 * (ZYHAT_ll(1)+ZYHAT_ll(2))
    CALL SM_LATLON(XLATORI,XLONORI,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    DEALLOCATE(ZXHAT_ll,ZYHAT_ll)
    XLATORI = ZLATORI
    XLONORI = ZLONORI
  END IF
END IF
!
YRECFM='ZS'
ALLOCATE(XZS(IIU,IJU))
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XZS,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) XZS(:,:)=0.
!
YRECFM='ZSMT'
ALLOCATE(XZSMT(IIU,IJU))
YDIR='XY'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XZSMT,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) XZSMT(:,:)=XZS(:,:)
!
YRECFM='ZHAT'
ALLOCATE(XZHAT(IKU))
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XZHAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SLEVE'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LSLEVE,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IRESP/=0) LSLEVE = .FALSE.
!
IF (LSLEVE) THEN
  YRECFM='LEN1'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLEN1,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM='LEN2'
  YDIR='--'
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XLEN2,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
YRECFM='DTEXP%TDATE' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TDTEXP%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3))  
!
YRECFM='DTEXP%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTEXP%TIME,IGRID,ILENCH,YCOMMENT,IRESP)
!   
YRECFM='DTMOD%TDATE' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TDTMOD%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3)) 
!
YRECFM='DTMOD%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTMOD%TIME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTSEG%TDATE' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TDTSEG%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3)) 
!
YRECFM='DTSEG%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTSEG%TIME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='DTCUR%TDATE' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ITDATE,IGRID,ILENCH,YCOMMENT,IRESP)
TDTCUR%TDATE=DATE(ITDATE(1),ITDATE(2),ITDATE(3)) 
!
YRECFM='DTCUR%TIME'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,TDTCUR%TIME,IGRID,ILENCH,YCOMMENT,IRESP)
!
YTITLE='CURRENT DATE AND TIME'
CALL SM_PRINT_TIME(TDTCUR,HLUOUT,YTITLE)
!
!*       2.2    Spatial grid
! 
ALLOCATE(XDXHAT(IIU))
ALLOCATE(XDYHAT(IJU))
ALLOCATE(XZZ(IIU,IJU,IKU))
ALLOCATE(ZJ(IIU,IJU,IKU))
ALLOCATE(ZCOEF(IIU,IJU))
!
CALL INI_CST
!
IF (LCARTESIAN) THEN
  CALL SM_GRIDCART(HLUOUT,XXHAT,XYHAT,XZHAT,XZS,LSLEVE,XLEN1,XLEN2,XZSMT,XDXHAT,XDYHAT,XZZ,ZJ) 
ELSE
  ALLOCATE(XLON(IIU,IJU))
  ALLOCATE(XLAT(IIU,IJU))
  ALLOCATE(XMAP(IIU,IJU))
  CALL SM_GRIDPROJ(HLUOUT,XXHAT,XYHAT,XZHAT,XZS,LSLEVE,XLEN1,XLEN2,XZSMT,XLATORI,XLONORI, &
                   XMAP,XLAT,XLON,XDXHAT,XDYHAT,XZZ,ZJ)  
END IF    
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZE THE PROGNOSTIC AND SURFACE FIELDS (read_field)
!              --------------------------------------------
ALLOCATE(XPABST(IIU,IJU,IKU))
!
YDIR='XY'
YRECFM = 'PABST'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,XPABST,IGRID,ILENCH,YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_FOR_CONVLFI
