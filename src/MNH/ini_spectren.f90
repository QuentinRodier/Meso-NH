!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_INI_SPECTRE_n
!     #######################
!
INTERFACE
!
       SUBROUTINE INI_SPECTRE_n(KMI,TPINIFILE)
!
       USE MODD_IO, ONLY: TFILEDATA
!
       INTEGER,           INTENT(IN) :: KMI       ! Model index 
       TYPE(TFILEDATA),   INTENT(IN) :: TPINIFILE ! Initial file
!
END SUBROUTINE INI_SPECTRE_n
!
END INTERFACE
!
END MODULE MODI_INI_SPECTRE_n
!     #######################################
      SUBROUTINE INI_SPECTRE_n(KMI,TPINIFILE)
!     #######################################
!
!!****  *INI_SPECTRE_n* - routine to initialize SPECTRE (based on ini_modeln.f90)
!!
!!
!!    AUTHOR
!!    ------
!!      J.P Chaboureau       * L.A*
!!      10/2016 (C.Lac) Cleaning of the modules
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!
!---------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ADV_n
USE MODD_BIKHARDT_n
USE MODD_BUDGET
USE MODD_CH_AERO_n,     ONLY: XSOLORG,XMI
USE MODD_CH_AEROSOL,    ONLY: LORILAM
USE MODD_CH_MNHC_n,     ONLY: LUSECHEM, LUSECHAQ, LUSECHIC, LCH_INIT_FIELD, &
                              CCHEM_INPUT_FILE, LCH_CONV_LINOX,             &
                              XCH_TUV_DOBNEW, LCH_PH
USE MODD_CH_PH_n
USE MODD_CLOUD_MF_n
USE MODD_CST
USE MODD_CONF
USE MODD_CONF_n
USE MODD_CTURB
USE MODD_CURVCOR_n
USE MODD_DEEP_CONVECTION_n
USE MODD_DIAG_FLAG,     ONLY: LCHEMDIAG
USE MODD_DIM_n
USE MODD_DRAGTREE
USE MODD_DUST
USE MODD_DYN
USE MODD_DYN_n
USE MODD_DYNZD
USE MODD_DYNZD_n
USE MODD_ELEC_n,        ONLY: XCION_POS_FW, XCION_NEG_FW
USE MODD_FIELD_n
USE MODD_FRC
USE MODD_FRC_n
USE MODD_GET_n
USE MODD_GRID,          ONLY: XLONORI,XLATORI
USE MODD_GRID_n
USE MODD_IO,            ONLY: TFILEDATA
USE MODD_LBC_n
USE MODD_LSFIELD_n
USE MODD_LUNIT_n,       ONLY: COUTFILE, TLUOUT
USE MODD_MEAN_FIELD
USE MODD_MEAN_FIELD_n
USE MODD_METRICS_n
USE MODD_NESTING
USE MODD_NSV
USE MODD_NUDGING_n,     ONLY: LNUDGING
USE MODD_OUT_n
USE MODD_PARAMETERS
USE MODD_PARAM_KAFR_n
USE MODD_PARAM_MFSHALL_n
USE MODD_PARAM_n
USE MODD_PARAM_RAD_n,   ONLY: CLW, CAER
USE MODD_PASPOL
USE MODD_PASPOL_n
USE MODD_BLOWSNOW
USE MODD_BLOWSNOW_n
USE MODD_PAST_FIELD_n
USE MODD_RADIATIONS_n
USE MODD_REF
USE MODD_REF_n
USE MODD_SERIES,        ONLY: LSERIES
USE MODD_SHADOWS_n
USE MODD_SPECTRE
USE MODD_STAND_ATM,     ONLY: XSTROATM, XSMLSATM, XSMLWATM, XSPOSATM, XSPOWATM
USE MODD_TIME
USE MODD_TIME_n
USE MODD_TURB_CLOUD,    ONLY: NMODEL_CLOUD, CTURBLEN_CLOUD,XCEI
USE MODD_TURB_n
USE MODD_VAR_ll,        ONLY: IP
!
USE MODD_ARGSLIST_ll,   ONLY: LIST_ll
USE MODE_GATHER_ll
USE MODE_IO_FIELD_READ, only: IO_Field_read
USE MODE_ll
USE MODE_MODELN_HANDLER
USE MODE_MSG
USE MODE_SPLITTINGZ_ll, ONLY: GET_DIM_EXTZ_ll
USE MODE_TYPE_ZDIFFU
!
USE MODI_INI_BIKHARDT_n
USE MODI_INI_CPL
USE MODI_INI_DYNAMICS
USE MODI_INI_ONE_WAY_n
USE MODI_INI_SPAWN_LS_n
USE MODI_GET_SIZEX_LB
USE MODI_GET_SIZEY_LB
USE MODI_SET_GRID
USE MODI_METRICS
USE MODI_SET_REF
USE MODI_UPDATE_METRICS
USE MODI_UPDATE_NSV
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
INTEGER,           INTENT(IN) :: KMI       ! Model index 
TYPE(TFILEDATA),   INTENT(IN) :: TPINIFILE ! Initial file
!
!*       0.2   declarations of local variables
!
INTEGER             :: JSV     ! Loop index
INTEGER             :: ILUOUT  ! Logical unit number of output-listing
INTEGER             :: IIU     ! Upper dimension in x direction (local)
INTEGER             :: IJU     ! Upper dimension in y direction (local)
INTEGER             :: IIU_ll  ! Upper dimension in x direction (global)
INTEGER             :: IJU_ll  ! Upper dimension in y direction (global)
INTEGER             :: IKU     ! Upper dimension in z direction
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZJ ! Jacobian
LOGICAL             :: GINIDCONV ! logical switch for the deep convection
                               ! initialization
LOGICAL             :: GINIRAD ! logical switch for the radiation
                               ! initialization
!
!
TYPE(LIST_ll), POINTER :: TZINITHALO2D_ll ! pointer for the list of 2D fields
                                      !  which must be communicated in INIT
TYPE(LIST_ll), POINTER :: TZINITHALO3D_ll ! pointer for the list of 3D fields
                                      !  which must be communicated in INIT
!
INTEGER :: IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU     ! dimensions of the
INTEGER :: IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2       ! West-east LB arrays
INTEGER :: IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV     ! dimensions of the
INTEGER :: IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2       ! North-south LB arrays
INTEGER :: IINFO_ll  ! Return code of //routines
INTEGER :: IIY,IJY
INTEGER :: IIU_B,IJU_B
INTEGER :: IIU_SXP2_YP1_Z_ll,IJU_SXP2_YP1_Z_ll,IKU_SXP2_YP1_Z_ll
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZCO2   ! CO2 concentration near the surface
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZCOVER ! surface cover types
INTEGER                             :: ICOVER ! number of cover types
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDIR_ALB ! direct albedo
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZSCA_ALB ! diffuse albedo
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZEMIS    ! emissivity
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZTSRAD   ! surface temperature
!------------------------------------------
! Dummy pointers needed to correct an ifort Bug
REAL, DIMENSION(:), POINTER :: DPTR_XZHAT
REAL, DIMENSION(:), POINTER :: DPTR_XBMX1,DPTR_XBMX2,DPTR_XBMX3,DPTR_XBMX4
REAL, DIMENSION(:), POINTER :: DPTR_XBMY1,DPTR_XBMY2,DPTR_XBMY3,DPTR_XBMY4
REAL, DIMENSION(:), POINTER :: DPTR_XBFX1,DPTR_XBFX2,DPTR_XBFX3,DPTR_XBFX4
REAL, DIMENSION(:), POINTER :: DPTR_XBFY1,DPTR_XBFY2,DPTR_XBFY3,DPTR_XBFY4
CHARACTER(LEN=4), DIMENSION(:), POINTER :: DPTR_CLBCX,DPTR_CLBCY
INTEGER, DIMENSION(:,:,:), POINTER :: DPTR_NKLIN_LBXU,DPTR_NKLIN_LBYU,DPTR_NKLIN_LBXV,DPTR_NKLIN_LBYV
INTEGER, DIMENSION(:,:,:), POINTER :: DPTR_NKLIN_LBXW,DPTR_NKLIN_LBYW,DPTR_NKLIN_LBXM,DPTR_NKLIN_LBYM
REAL, DIMENSION(:,:,:), POINTER :: DPTR_XCOEFLIN_LBXU,DPTR_XCOEFLIN_LBYU
REAL, DIMENSION(:,:,:), POINTER :: DPTR_XCOEFLIN_LBXV,DPTR_XCOEFLIN_LBYV
REAL, DIMENSION(:,:,:), POINTER :: DPTR_XCOEFLIN_LBXW,DPTR_XCOEFLIN_LBYW
REAL, DIMENSION(:,:,:), POINTER :: DPTR_XCOEFLIN_LBXM,DPTR_XCOEFLIN_LBYM
REAL, DIMENSION(:,:,:),   POINTER :: DPTR_XLBXUM,DPTR_XLBYUM,DPTR_XLBXVM,DPTR_XLBYVM
REAL, DIMENSION(:,:,:),   POINTER :: DPTR_XLBXWM,DPTR_XLBYWM,DPTR_XLBXTHM,DPTR_XLBYTHM
REAL, DIMENSION(:,:,:),   POINTER :: DPTR_XLBXTKEM,DPTR_XLBYTKEM
REAL, DIMENSION(:,:,:,:),   POINTER :: DPTR_XLBXSVM,DPTR_XLBYSVM                
REAL, DIMENSION(:,:,:,:), POINTER :: DPTR_XLBXRM,DPTR_XLBYRM
REAL, DIMENSION(:,:,:),   POINTER ::  DPTR_XZZ
REAL, DIMENSION(:,:,:), POINTER ::   DPTR_XLSUM,DPTR_XLSVM,DPTR_XLSWM,DPTR_XLSTHM,DPTR_XLSRVM
REAL, DIMENSION(:,:,:), POINTER ::   DPTR_XLSUS,DPTR_XLSVS,DPTR_XLSWS,DPTR_XLSTHS,DPTR_XLSRVS
!
!-------------------------------------------------------------------------------
!
!*       0.    PROLOGUE
!              --------
!
NULLIFY(TZINITHALO2D_ll)
NULLIFY(TZINITHALO3D_ll)
!
!*       1.    RETRIEVE LOGICAL UNIT NUMBER
!              ----------------------------
!
ILUOUT = TLUOUT%NLU
!
!-------------------------------------------------------------------------------
!
!*       2.   END OF READING
!             --------------
!*       2.1  Read number of forcing fields
!
!*       2.2  Checks the position of vertical absorbing layer
!
IKU=NKMAX+2*JPVEXT
!
ALLOCATE(XZHAT(IKU))
CALL IO_Field_read(TPINIFILE,'ZHAT',XZHAT)
CALL IO_Field_read(TPINIFILE,'ZTOP',XZTOP)
IF (XALZBOT>=XZHAT(IKU) .AND. LVE_RELAX) THEN
  WRITE(ILUOUT,FMT=*) "INI_SPECTRE_n ERROR: you want to use vertical relaxation"
  WRITE(ILUOUT,FMT=*) "                  but bottom of layer XALZBOT(",XALZBOT,")"
  WRITE(ILUOUT,FMT=*) "                  is upper than model top    (",XZHAT(IKU),")"
!callabortstop
  CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_SPECTRE_n','')
END IF
IF (LVE_RELAX) THEN
 IF (XALZBOT>=XZHAT(IKU-4) ) THEN
  WRITE(ILUOUT,FMT=*) "INI_SPECTRE_n WARNING: you want to use vertical relaxation"
  WRITE(ILUOUT,FMT=*) "                    but the layer defined by XALZBOT(",XALZBOT,")"
  WRITE(ILUOUT,FMT=*) "                    contains less than 5 model levels"
 END IF
END IF
DEALLOCATE(XZHAT)
!
!*       2.3  Compute sizes of arrays of the extended sub-domain
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IIU_ll=NIMAX_ll + 2 * JPHEXT
IJU_ll=NJMAX_ll + 2 * JPHEXT
! initialize NIMAX and NJMAX for not updated versions regarding the parallelism
! spawning,...
CALL GET_DIM_PHYS_ll('B',NIMAX,NJMAX)
!
NRR=1
NRRL=0
NRRI=0
IF (NVERB >= 5) THEN
  WRITE (UNIT=ILUOUT,FMT='("THERE ARE ",I2," WATER VARIABLES")') NRR
  WRITE (UNIT=ILUOUT,FMT='("THERE ARE ",I2," LIQUID VARIABLES")') NRRL
  WRITE (UNIT=ILUOUT,FMT='("THERE ARE ",I2," SOLID VARIABLES")') NRRI
END IF
!
!*       2.3  Update NSV and floating indices for the current model
!
! 
CALL UPDATE_NSV(KMI) 
!
!-------------------------------------------------------------------------------
!
!*       3.    ALLOCATE  MEMORY
!              -----------------
!
!*       3.2   Module MODD_GRID_n and MODD_METRICS_n
!
IF (LCARTESIAN) THEN
  ALLOCATE(XLON(0,0))
  ALLOCATE(XLAT(0,0))
  ALLOCATE(XMAP(0,0))
ELSE
  ALLOCATE(XLON(IIU,IJU))
  ALLOCATE(XLAT(IIU,IJU))
  ALLOCATE(XMAP(IIU,IJU))
END IF
ALLOCATE(XXHAT(IIU))
ALLOCATE(XDXHAT(IIU))
ALLOCATE(XYHAT(IJU))
ALLOCATE(XDYHAT(IJU))
ALLOCATE(XZS(IIU,IJU))
ALLOCATE(XZSMT(IIU,IJU))
ALLOCATE(XZZ(IIU,IJU,IKU))
ALLOCATE(XZHAT(IKU))
!
ALLOCATE(XDXX(IIU,IJU,IKU))
ALLOCATE(XDYY(IIU,IJU,IKU))
ALLOCATE(XDZX(IIU,IJU,IKU))
ALLOCATE(XDZY(IIU,IJU,IKU))
ALLOCATE(XDZZ(IIU,IJU,IKU))
!
!*       3.3   Modules MODD_REF and  MODD_REF_n
!
IF (KMI == 1) THEN
  ALLOCATE(XRHODREFZ(IKU),XTHVREFZ(IKU))
END IF
ALLOCATE(XRHODREF(IIU,IJU,IKU))
ALLOCATE(XTHVREF(IIU,IJU,IKU))
ALLOCATE(XEXNREF(IIU,IJU,IKU))
ALLOCATE(XRHODJ(IIU,IJU,IKU))
IF (CEQNSYS=='DUR' .AND. LUSERV) THEN
  ALLOCATE(XRVREF(IIU,IJU,IKU))
ELSE
  ALLOCATE(XRVREF(0,0,0))
END IF
!
!*       3.4   Module MODD_CURVCOR_n
!
IF (LTHINSHELL) THEN
  ALLOCATE(XCORIOX(0,0))
  ALLOCATE(XCORIOY(0,0))
ELSE
  ALLOCATE(XCORIOX(IIU,IJU))
  ALLOCATE(XCORIOY(IIU,IJU))
END IF
  ALLOCATE(XCORIOZ(IIU,IJU))
IF (LCARTESIAN) THEN
  ALLOCATE(XCURVX(0,0))
  ALLOCATE(XCURVY(0,0))
ELSE
  ALLOCATE(XCURVX(IIU,IJU))
  ALLOCATE(XCURVY(IIU,IJU))
END IF
!
!*       3.5   Module MODD_DYN_n
!
CALL GET_DIM_EXT_ll('Y',IIY,IJY)
IF (L2D) THEN
  ALLOCATE(XBFY(IIY,IJY,IKU))
ELSE
  ALLOCATE(XBFY(IJY,IIY,IKU)) ! transposition needed by the optimisition of the
                              ! FFT solver
END IF
CALL GET_DIM_EXT_ll('B',IIU_B,IJU_B)
ALLOCATE(XBFB(IIU_B,IJU_B,IKU))
CALL GET_DIM_EXTZ_ll('SXP2_YP1_Z',IIU_SXP2_YP1_Z_ll,IJU_SXP2_YP1_Z_ll,IKU_SXP2_YP1_Z_ll)
ALLOCATE(XBF_SXP2_YP1_Z(IIU_SXP2_YP1_Z_ll,IJU_SXP2_YP1_Z_ll,IKU_SXP2_YP1_Z_ll))
ALLOCATE(XAF(IKU),XCF(IKU))
ALLOCATE(XTRIGSX(3*IIU_ll))
ALLOCATE(XTRIGSY(3*IJU_ll))
ALLOCATE(XRHOM(IKU))
ALLOCATE(XALK(IKU))
ALLOCATE(XALKW(IKU))
!
IF ( LHORELAX_UVWTH .OR. LHORELAX_RV .OR.                                  &
     LHORELAX_RC .OR. LHORELAX_RR .OR. LHORELAX_RI  .OR. LHORELAX_RS  .OR. &
     LHORELAX_RG .OR. LHORELAX_RH .OR. LHORELAX_TKE .OR.                   &
     ANY(LHORELAX_SV) ) THEN
  ALLOCATE(XKURELAX(IIU,IJU))
  ALLOCATE(XKVRELAX(IIU,IJU))
  ALLOCATE(XKWRELAX(IIU,IJU))
  ALLOCATE(LMASK_RELAX(IIU,IJU))
ELSE
  ALLOCATE(XKURELAX(0,0))
  ALLOCATE(XKVRELAX(0,0))
  ALLOCATE(XKWRELAX(0,0))
  ALLOCATE(LMASK_RELAX(0,0))
END IF
!
! Additional fields for truly horizontal diffusion (Module MODD_DYNZD$n)
IF (LZDIFFU) THEN
  CALL INIT_TYPE_ZDIFFU_HALO2(XZDIFFU_HALO2)
ELSE
  CALL INIT_TYPE_ZDIFFU_HALO2(XZDIFFU_HALO2,0)
ENDIF
!
!*       3.6   Larger Scale variables (Module MODD_LSFIELD$n)
!
!
! upper relaxation part
!
ALLOCATE(XLSUM(IIU,IJU,IKU))    ; XLSUM  = 0.0
ALLOCATE(XLSVM(IIU,IJU,IKU))    ; XLSVM  = 0.0
ALLOCATE(XLSWM(IIU,IJU,IKU))    ; XLSWM  = 0.0
ALLOCATE(XLSTHM(IIU,IJU,IKU))   ; XLSTHM = 0.0
IF ( NRR > 0 ) THEN
  ALLOCATE(XLSRVM(IIU,IJU,IKU)) ; XLSRVM = 0.0
ELSE
  ALLOCATE(XLSRVM(0,0,0))
END IF
!
!  lbc part
!
IF ( L1D) THEN                         ! 1D case
!
  NSIZELBX_ll=0
  NSIZELBXU_ll=0
  NSIZELBY_ll=0
  NSIZELBYV_ll=0
  NSIZELBXTKE_ll=0
  NSIZELBXR_ll=0
  NSIZELBXSV_ll=0
  NSIZELBYTKE_ll=0
  NSIZELBYR_ll=0
  NSIZELBYSV_ll=0
  ALLOCATE(XLBXUM(0,0,0))
  ALLOCATE(XLBYUM(0,0,0))
  ALLOCATE(XLBXVM(0,0,0))
  ALLOCATE(XLBYVM(0,0,0))
  ALLOCATE(XLBXWM(0,0,0))
  ALLOCATE(XLBYWM(0,0,0))
  ALLOCATE(XLBXTHM(0,0,0))
  ALLOCATE(XLBYTHM(0,0,0))
  ALLOCATE(XLBXTKEM(0,0,0))
  ALLOCATE(XLBYTKEM(0,0,0))
  ALLOCATE(XLBXRM(0,0,0,0))
  ALLOCATE(XLBYRM(0,0,0,0))
  ALLOCATE(XLBXSVM(0,0,0,0))
  ALLOCATE(XLBYSVM(0,0,0,0))
!
ELSEIF( L2D ) THEN                         ! 2D case
!
  NSIZELBY_ll=0
  NSIZELBYV_ll=0
  NSIZELBYTKE_ll=0
  NSIZELBYR_ll=0
  NSIZELBYSV_ll=0
  ALLOCATE(XLBYUM(0,0,0))
  ALLOCATE(XLBYVM(0,0,0))
  ALLOCATE(XLBYWM(0,0,0))
  ALLOCATE(XLBYTHM(0,0,0))
  ALLOCATE(XLBYTKEM(0,0,0))
  ALLOCATE(XLBYRM(0,0,0,0))
  ALLOCATE(XLBYSVM(0,0,0,0))
!
  CALL GET_SIZEX_LB(NIMAX_ll,NJMAX_ll,NRIMX,               &
                    IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU, &
                    IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2)
!
  IF ( LHORELAX_UVWTH ) THEN
    NSIZELBX_ll=2*NRIMX+2
    NSIZELBXU_ll=2*NRIMX+2
    ALLOCATE(XLBXUM(IISIZEXFU,IJSIZEXFU,IKU))
    ALLOCATE(XLBXVM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBXWM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBXTHM(IISIZEXF,IJSIZEXF,IKU))
  ELSE
    NSIZELBX_ll=2
    NSIZELBXU_ll=4
    ALLOCATE(XLBXUM(IISIZEX4,IJSIZEX4,IKU))
    ALLOCATE(XLBXVM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBXWM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBXTHM(IISIZEX2,IJSIZEX2,IKU))
  END IF
!
  IF (CTURB /= 'NONE') THEN
    IF ( LHORELAX_TKE) THEN
      NSIZELBXTKE_ll=2* NRIMX+2
      ALLOCATE(XLBXTKEM(IISIZEXF,IJSIZEXF,IKU))
    ELSE
      NSIZELBXTKE_ll=2
      ALLOCATE(XLBXTKEM(IISIZEX2,IJSIZEX2,IKU))
    END IF
  ELSE
    NSIZELBXTKE_ll=0
    ALLOCATE(XLBXTKEM(0,0,0))
  END IF
  !
  IF ( NRR > 0 ) THEN
    IF (LHORELAX_RV .OR. LHORELAX_RC .OR. LHORELAX_RR .OR. LHORELAX_RI    &
         .OR. LHORELAX_RS .OR. LHORELAX_RG .OR. LHORELAX_RH               &
       ) THEN
      NSIZELBXR_ll=2* NRIMX+2
      ALLOCATE(XLBXRM(IISIZEXF,IJSIZEXF,IKU,NRR))
    ELSE
      NSIZELBXR_ll=2
      ALLOCATE(XLBXRM(IISIZEX2,IJSIZEX2,IKU,NRR))
    ENDIF
  ELSE
    NSIZELBXR_ll=0
    ALLOCATE(XLBXRM(0,0,0,0))
  END IF
  !
  IF ( NSV > 0 ) THEN
    IF ( ANY( LHORELAX_SV(:)) ) THEN
      NSIZELBXSV_ll=2* NRIMX+2
      ALLOCATE(XLBXSVM(IISIZEXF,IJSIZEXF,IKU,NSV))
    ELSE
      NSIZELBXSV_ll=2
      ALLOCATE(XLBXSVM(IISIZEX2,IJSIZEX2,IKU,NSV))
    END IF
  ELSE
    NSIZELBXSV_ll=0
    ALLOCATE(XLBXSVM(0,0,0,0))
  END IF
!
ELSE                                   ! 3D case
!
!
  CALL GET_SIZEX_LB(NIMAX_ll,NJMAX_ll,NRIMX,               &
                    IISIZEXF,IJSIZEXF,IISIZEXFU,IJSIZEXFU, &
                    IISIZEX4,IJSIZEX4,IISIZEX2,IJSIZEX2)
  CALL GET_SIZEY_LB(NIMAX_ll,NJMAX_ll,NRIMY,               &
                    IISIZEYF,IJSIZEYF,IISIZEYFV,IJSIZEYFV, &
                    IISIZEY4,IJSIZEY4,IISIZEY2,IJSIZEY2)
!
! check if local domain not to small for NRIMX NRIMY
!
  IF ( CLBCX(1) /= 'CYCL' )  THEN
     IF ( NRIMX+2*JPHEXT .GE. IIU )   THEN
        WRITE(*,'(A,I8,A/A,2I8,/A)') "Processor=", IP-1, &
             " :: INI_SPECTRE_n ERROR:  ( NRIMX+2*JPHEXT >= IIU )  ", &
             " Local domain to small for relaxation NRIMX+2*JPHEXT,IIU ", &
             NRIMX+2*JPHEXT,IIU ,&
             " change relaxation parameters or number of processors "
        !callabortstop
        CALL ABORT
        STOP    
     END IF
  END IF
  IF ( CLBCY(1) /= 'CYCL' ) THEN
     IF ( NRIMY+2*JPHEXT .GE. IJU )  THEN
        WRITE(*,'(A,I8,A/A,2I8,/A)') "Processor=", IP-1, &
             " :: INI_SPECTRE_n ERROR:  ( NRIMY+2*JPHEXT >= IJU )  ", &
             " Local domain to small for relaxation NRIMY+2*JPHEXT,IJU ", &
             NRIMY+2*JPHEXT,IJU ,&
             " change relaxation parameters or number of processors "
        !callabortstop
        CALL ABORT
        STOP    
     END IF
  END IF
IF ( LHORELAX_UVWTH ) THEN
    NSIZELBX_ll=2*NRIMX+2
    NSIZELBXU_ll=2*NRIMX+2
    NSIZELBY_ll=2*NRIMY+2
    NSIZELBYV_ll=2*NRIMY+2
    ALLOCATE(XLBXUM(IISIZEXFU,IJSIZEXFU,IKU))
    ALLOCATE(XLBYUM(IISIZEYF,IJSIZEYF,IKU))
    ALLOCATE(XLBXVM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBYVM(IISIZEYFV,IJSIZEYFV,IKU))
    ALLOCATE(XLBXWM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBYWM(IISIZEYF,IJSIZEYF,IKU))
    ALLOCATE(XLBXTHM(IISIZEXF,IJSIZEXF,IKU))
    ALLOCATE(XLBYTHM(IISIZEYF,IJSIZEYF,IKU))
  ELSE
    NSIZELBX_ll=2
    NSIZELBXU_ll=4
    NSIZELBY_ll=2
    NSIZELBYV_ll=4
    ALLOCATE(XLBXUM(IISIZEX4,IJSIZEX4,IKU))
    ALLOCATE(XLBYUM(IISIZEY2,IJSIZEY2,IKU))
    ALLOCATE(XLBXVM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBYVM(IISIZEY4,IJSIZEY4,IKU))
    ALLOCATE(XLBXWM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBYWM(IISIZEY2,IJSIZEY2,IKU))
    ALLOCATE(XLBXTHM(IISIZEX2,IJSIZEX2,IKU))
    ALLOCATE(XLBYTHM(IISIZEY2,IJSIZEY2,IKU))
  END IF
  !
  IF (CTURB /= 'NONE') THEN
    IF ( LHORELAX_TKE) THEN
      NSIZELBXTKE_ll=2*NRIMX+2
      NSIZELBYTKE_ll=2*NRIMY+2
      ALLOCATE(XLBXTKEM(IISIZEXF,IJSIZEXF,IKU))
      ALLOCATE(XLBYTKEM(IISIZEYF,IJSIZEYF,IKU))
    ELSE
      NSIZELBXTKE_ll=2
      NSIZELBYTKE_ll=2
      ALLOCATE(XLBXTKEM(IISIZEX2,IJSIZEX2,IKU))
      ALLOCATE(XLBYTKEM(IISIZEY2,IJSIZEY2,IKU))
    END IF
  ELSE
    NSIZELBXTKE_ll=0
    NSIZELBYTKE_ll=0
    ALLOCATE(XLBXTKEM(0,0,0))
    ALLOCATE(XLBYTKEM(0,0,0))
  END IF
  !
  IF ( NRR > 0 ) THEN
    IF (LHORELAX_RV .OR. LHORELAX_RC .OR. LHORELAX_RR .OR. LHORELAX_RI    &
          .OR. LHORELAX_RS .OR. LHORELAX_RG .OR. LHORELAX_RH              &
       ) THEN
      NSIZELBXR_ll=2*NRIMX+2
      NSIZELBYR_ll=2*NRIMY+2
      ALLOCATE(XLBXRM(IISIZEXF,IJSIZEXF,IKU,NRR))
      ALLOCATE(XLBYRM(IISIZEYF,IJSIZEYF,IKU,NRR))
    ELSE
      NSIZELBXR_ll=2
      NSIZELBYR_ll=2
      ALLOCATE(XLBXRM(IISIZEX2,IJSIZEX2,IKU,NRR))
      ALLOCATE(XLBYRM(IISIZEY2,IJSIZEY2,IKU,NRR))
    ENDIF
  ELSE
    NSIZELBXR_ll=0
    NSIZELBYR_ll=0
    ALLOCATE(XLBXRM(0,0,0,0))
    ALLOCATE(XLBYRM(0,0,0,0))
  END IF
  !
  IF ( NSV > 0 ) THEN
    IF ( ANY( LHORELAX_SV(:)) ) THEN
      NSIZELBXSV_ll=2*NRIMX+2
      NSIZELBYSV_ll=2*NRIMY+2
      ALLOCATE(XLBXSVM(IISIZEXF,IJSIZEXF,IKU,NSV))
      ALLOCATE(XLBYSVM(IISIZEYF,IJSIZEYF,IKU,NSV))
    ELSE
      NSIZELBXSV_ll=2
      NSIZELBYSV_ll=2
      ALLOCATE(XLBXSVM(IISIZEX2,IJSIZEX2,IKU,NSV))
      ALLOCATE(XLBYSVM(IISIZEY2,IJSIZEY2,IKU,NSV))
    END IF
  ELSE
    NSIZELBXSV_ll=0
    NSIZELBYSV_ll=0
    ALLOCATE(XLBXSVM(0,0,0,0))
    ALLOCATE(XLBYSVM(0,0,0,0))
  END IF
END IF      ! END OF THE IF STRUCTURE ON THE MODEL DIMENSION
!
!
IF ( KMI > 1 ) THEN 
  ! it has been assumed that the THeta field used the largest rim area compared
  ! to the others prognostic variables, if it is not the case, you must change
  ! these lines
  ALLOCATE(XCOEFLIN_LBXM(SIZE(XLBXTHM,1),SIZE(XLBXTHM,2),SIZE(XLBXTHM,3)))
  ALLOCATE(   NKLIN_LBXM(SIZE(XLBXTHM,1),SIZE(XLBXTHM,2),SIZE(XLBXTHM,3)))
  ALLOCATE(XCOEFLIN_LBYM(SIZE(XLBYTHM,1),SIZE(XLBYTHM,2),SIZE(XLBYTHM,3)))
  ALLOCATE(   NKLIN_LBYM(SIZE(XLBYTHM,1),SIZE(XLBYTHM,2),SIZE(XLBYTHM,3)))
  ALLOCATE(XCOEFLIN_LBXU(SIZE(XLBXUM,1),SIZE(XLBXUM,2),SIZE(XLBXUM,3)))
  ALLOCATE(   NKLIN_LBXU(SIZE(XLBXUM,1),SIZE(XLBXUM,2),SIZE(XLBXUM,3)))
  ALLOCATE(XCOEFLIN_LBYU(SIZE(XLBYUM,1),SIZE(XLBYUM,2),SIZE(XLBYUM,3)))
  ALLOCATE(   NKLIN_LBYU(SIZE(XLBYUM,1),SIZE(XLBYUM,2),SIZE(XLBYUM,3)))
  ALLOCATE(XCOEFLIN_LBXV(SIZE(XLBXVM,1),SIZE(XLBXVM,2),SIZE(XLBXVM,3)))
  ALLOCATE(   NKLIN_LBXV(SIZE(XLBXVM,1),SIZE(XLBXVM,2),SIZE(XLBXVM,3)))
  ALLOCATE(XCOEFLIN_LBYV(SIZE(XLBYVM,1),SIZE(XLBYVM,2),SIZE(XLBYVM,3)))
  ALLOCATE(   NKLIN_LBYV(SIZE(XLBYVM,1),SIZE(XLBYVM,2),SIZE(XLBYVM,3)))
  ALLOCATE(XCOEFLIN_LBXW(SIZE(XLBXWM,1),SIZE(XLBXWM,2),SIZE(XLBXWM,3)))
  ALLOCATE(   NKLIN_LBXW(SIZE(XLBXWM,1),SIZE(XLBXWM,2),SIZE(XLBXWM,3)))
  ALLOCATE(XCOEFLIN_LBYW(SIZE(XLBYWM,1),SIZE(XLBYWM,2),SIZE(XLBYWM,3)))
  ALLOCATE(   NKLIN_LBYW(SIZE(XLBYWM,1),SIZE(XLBYWM,2),SIZE(XLBYWM,3)))
END IF
!
!  allocation of the LS fields for vertical relaxation and numerical diffusion
IF( .NOT. LSTEADYLS )  THEN
!
  ALLOCATE(XLSUS(SIZE(XLSUM,1),SIZE(XLSUM,2),SIZE(XLSUM,3)))
  ALLOCATE(XLSVS(SIZE(XLSVM,1),SIZE(XLSVM,2),SIZE(XLSVM,3)))
  ALLOCATE(XLSWS(SIZE(XLSWM,1),SIZE(XLSWM,2),SIZE(XLSWM,3)))
  ALLOCATE(XLSTHS(SIZE(XLSTHM,1),SIZE(XLSTHM,2),SIZE(XLSTHM,3)))
  ALLOCATE(XLSRVS(SIZE(XLSRVM,1),SIZE(XLSRVM,2),SIZE(XLSRVM,3)))
!
ELSE
!
  ALLOCATE(XLSUS(0,0,0))
  ALLOCATE(XLSVS(0,0,0))
  ALLOCATE(XLSWS(0,0,0))
  ALLOCATE(XLSTHS(0,0,0))
  ALLOCATE(XLSRVS(0,0,0))
!
END IF
!  allocation of the LB fields for horizontal relaxation and Lateral Boundaries
IF( .NOT. ( LSTEADYLS .AND. KMI==1 ) )  THEN
!
  ALLOCATE(XLBXTKES(SIZE(XLBXTKEM,1),SIZE(XLBXTKEM,2),SIZE(XLBXTKEM,3)))
  ALLOCATE(XLBYTKES(SIZE(XLBYTKEM,1),SIZE(XLBYTKEM,2),SIZE(XLBYTKEM,3)))
  ALLOCATE(XLBXUS(SIZE(XLBXUM,1),SIZE(XLBXUM,2),SIZE(XLBXUM,3)))
  ALLOCATE(XLBYUS(SIZE(XLBYUM,1),SIZE(XLBYUM,2),SIZE(XLBYUM,3)))
  ALLOCATE(XLBXVS(SIZE(XLBXVM,1),SIZE(XLBXVM,2),SIZE(XLBXVM,3)))
  ALLOCATE(XLBYVS(SIZE(XLBYVM,1),SIZE(XLBYVM,2),SIZE(XLBYVM,3)))
  ALLOCATE(XLBXWS(SIZE(XLBXWM,1),SIZE(XLBXWM,2),SIZE(XLBXWM,3)))
  ALLOCATE(XLBYWS(SIZE(XLBYWM,1),SIZE(XLBYWM,2),SIZE(XLBYWM,3)))
  ALLOCATE(XLBXTHS(SIZE(XLBXTHM,1),SIZE(XLBXTHM,2),SIZE(XLBXTHM,3)))
  ALLOCATE(XLBYTHS(SIZE(XLBYTHM,1),SIZE(XLBYTHM,2),SIZE(XLBYTHM,3)))
  ALLOCATE(XLBXRS(SIZE(XLBXRM,1),SIZE(XLBXRM,2),SIZE(XLBXRM,3),SIZE(XLBXRM,4)))
  ALLOCATE(XLBYRS(SIZE(XLBYRM,1),SIZE(XLBYRM,2),SIZE(XLBYRM,3),SIZE(XLBYRM,4)))
  ALLOCATE(XLBXSVS(SIZE(XLBXSVM,1),SIZE(XLBXSVM,2),SIZE(XLBXSVM,3),SIZE(XLBXSVM,4)))
  ALLOCATE(XLBYSVS(SIZE(XLBYSVM,1),SIZE(XLBYSVM,2),SIZE(XLBYSVM,3),SIZE(XLBYSVM,4)))
!
ELSE
!
  ALLOCATE(XLBXTKES(0,0,0))
  ALLOCATE(XLBYTKES(0,0,0))
  ALLOCATE(XLBXUS(0,0,0))
  ALLOCATE(XLBYUS(0,0,0))
  ALLOCATE(XLBXVS(0,0,0))
  ALLOCATE(XLBYVS(0,0,0))
  ALLOCATE(XLBXWS(0,0,0))
  ALLOCATE(XLBYWS(0,0,0))
  ALLOCATE(XLBXTHS(0,0,0))
  ALLOCATE(XLBYTHS(0,0,0))
  ALLOCATE(XLBXRS(0,0,0,0))
  ALLOCATE(XLBYRS(0,0,0,0))
  ALLOCATE(XLBXSVS(0,0,0,0))
  ALLOCATE(XLBYSVS(0,0,0,0))
!
END IF
!
!*       3.9   Local variables
!
ALLOCATE(ZJ(IIU,IJU,IKU))
!
!-------------------------------------------------------------------------------
!
!
!*       5.    INITIALIZE INTERPOLATION COEFFICIENTS
!
CALL INI_BIKHARDT_n (NDXRATIO_ALL(KMI),NDYRATIO_ALL(KMI),KMI)
!
!-------------------------------------------------------------------------------
!
!*       6.    INITIALIZE GRIDS AND METRIC COEFFICIENTS
!              ----------------------------------------
!
CALL SET_GRID(KMI,TPINIFILE,IIU,IJU,IKU,NIMAX_ll,NJMAX_ll,               &
              XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4,           &
              XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4,           &
              NXOR_ALL(KMI),NYOR_ALL(KMI),NXEND_ALL(KMI),NYEND_ALL(KMI), &
              NDXRATIO_ALL(KMI),NDYRATIO_ALL(KMI),                       &
              CLBCX,CLBCY,                                               &
              XTSTEP,XSEGLEN,                                            &
              XLONORI,XLATORI,XLON,XLAT,                                 &
              XXHAT,XYHAT,XDXHAT,XDYHAT, XMAP,                           &
              XZS,XZZ,XZHAT,XZTOP,LSLEVE,XLEN1,XLEN2,XZSMT,              &
              ZJ,                                                        &
              TDTMOD,TDTCUR,NSTOP,NBAK_NUMB,NOUT_NUMB,TBACKUPN,TOUTPUTN)
!
CALL METRICS(XMAP,XDXHAT,XDYHAT,XZZ,XDXX,XDYY,XDZX,XDZY,XDZZ)
!
!* update halos of metric coefficients
!
!
CALL UPDATE_METRICS(CLBCX,CLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ)
!
!
! grid nesting initializations
IF ( KMI == 1 ) THEN
  XTSTEP_MODEL1=XTSTEP
END IF
!
NDT_2_WAY(KMI)=4
!
!-------------------------------------------------------------------------------
!
!*       8.    INITIALIZE THE PROGNOSTIC FIELDS
!              --------------------------------
!

IF (LSPECTRE_U) THEN
  ALLOCATE(XUT(IIU,IJU,IKU))      ; XUT  = 0.0
  CALL IO_Field_read(TPINIFILE,'UT',XUT)
END IF
!
IF (LSPECTRE_V) THEN
  ALLOCATE(XVT(IIU,IJU,IKU))      ; XVT  = 0.0
  CALL IO_Field_read(TPINIFILE,'VT',XVT)
END IF
!
IF (LSPECTRE_W) THEN  
  ALLOCATE(XWT(IIU,IJU,IKU))      ; XWT  = 0.0
  CALL IO_Field_read(TPINIFILE,'WT',XWT)
END IF
!
IF (LSPECTRE_TH) THEN
  ALLOCATE(XTHT(IIU,IJU,IKU))     ; XTHT = 0.0
  CALL IO_Field_read(TPINIFILE,'THT',XTHT)
END IF
!
IF (LSPECTRE_RV) THEN
  ALLOCATE(XRT(IIU,IJU,IKU,NRR))
  CALL IO_Field_read(TPINIFILE,'RVT',XRT(:,:,:,1))
END IF
!
!-------------------------------------------------------------------------------
!
!
!*        9.   INITIALIZE REFERENCE STATE
!              ---------------------------
!
!
CALL SET_REF(KMI,TPINIFILE,                        &
             XZZ,XZHAT,ZJ,XDXX,XDYY,CLBCX,CLBCY,   &
             XREFMASS,XMASS_O_PHI0,XLINMASS,       &
             XRHODREF,XTHVREF,XRVREF,XEXNREF,XRHODJ)
!-------------------------------------------------------------------------------
!
!*       11.    INITIALIZE THE SOURCE OF TOTAL DRY MASS Md
!               ------------------------------------------
!
IF((KMI==1).AND.LSTEADYLS) THEN
  XDRYMASSS = 0.
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       14.   INITIALIZE THE LARGE SCALE SOURCES
!              ----------------------------------
!
IF ((KMI==1).AND.(.NOT. LSTEADYLS)) THEN
  IF (LSPECTRE_LSU.OR.LSPECTRE_LSV.OR.LSPECTRE_LSW.OR. &
      LSPECTRE_LSRV.OR.LSPECTRE_LSTH) THEN 
    CALL INI_CPL(NSTOP,XTSTEP,LSTEADYLS,CCONF,                                &
               CGETTKET,                                                      &
               CGETRVT,CGETRCT,CGETRRT,CGETRIT,                               &
               CGETRST,CGETRGT,CGETRHT,CGETSVT,LCH_INIT_FIELD,                &
               NSV,NIMAX_ll,NJMAX_ll,                                         &
               NSIZELBX_ll,NSIZELBXU_ll,NSIZELBY_ll,NSIZELBYV_ll,             &
               NSIZELBXTKE_ll,NSIZELBYTKE_ll,                                 &
               NSIZELBXR_ll,NSIZELBYR_ll,NSIZELBXSV_ll,NSIZELBYSV_ll,         &
               XLSUM,XLSVM,XLSWM,XLSTHM,XLSRVM,XDRYMASST,                     &
               XLBXUM,XLBXVM,XLBXWM,XLBXTHM,XLBXTKEM,XLBXRM,XLBXSVM,          &
               XLBYUM,XLBYVM,XLBYWM,XLBYTHM,XLBYTKEM,XLBYRM,XLBYSVM,          &
               XLSUS,XLSVS,XLSWS,XLSTHS,XLSRVS,XDRYMASSS,                     &
               XLBXUS,XLBXVS,XLBXWS,XLBXTHS,XLBXTKES,XLBXRS,XLBXSVS,          &
               XLBYUS,XLBYVS,XLBYWS,XLBYTHS,XLBYTKES,XLBYRS,XLBYSVS           )
  END IF
END IF
!
IF ( KMI > 1) THEN
  ! Use dummy pointers to correct an ifort BUG
  DPTR_XBMX1=>XBMX1
  DPTR_XBMX2=>XBMX2
  DPTR_XBMX3=>XBMX3
  DPTR_XBMX4=>XBMX4
  DPTR_XBMY1=>XBMY1
  DPTR_XBMY2=>XBMY2
  DPTR_XBMY3=>XBMY3
  DPTR_XBMY4=>XBMY4
  DPTR_XBFX1=>XBFX1
  DPTR_XBFX2=>XBFX2
  DPTR_XBFX3=>XBFX3
  DPTR_XBFX4=>XBFX4
  DPTR_XBFY1=>XBFY1
  DPTR_XBFY2=>XBFY2
  DPTR_XBFY3=>XBFY3
  DPTR_XBFY4=>XBFY4
  DPTR_CLBCX=>CLBCX
  DPTR_CLBCY=>CLBCY
  !
  DPTR_XZZ=>XZZ
  DPTR_XZHAT=>XZHAT
  DPTR_XLSUM=>XLSUM
  DPTR_XLSVM=>XLSVM
  DPTR_XLSWM=>XLSWM
  DPTR_XLSTHM=>XLSTHM
  DPTR_XLSRVM=>XLSRVM
  DPTR_XLSUS=>XLSUS
  DPTR_XLSVS=>XLSVS
  DPTR_XLSWS=>XLSWS
  DPTR_XLSTHS=>XLSTHS
  DPTR_XLSRVS=>XLSRVS
  !
  DPTR_NKLIN_LBXU=>NKLIN_LBXU
  DPTR_XCOEFLIN_LBXU=>XCOEFLIN_LBXU
  DPTR_NKLIN_LBYU=>NKLIN_LBYU
  DPTR_XCOEFLIN_LBYU=>XCOEFLIN_LBYU
  DPTR_NKLIN_LBXV=>NKLIN_LBXV
  DPTR_XCOEFLIN_LBXV=>XCOEFLIN_LBXV
  DPTR_NKLIN_LBYV=>NKLIN_LBYV
  DPTR_XCOEFLIN_LBYV=>XCOEFLIN_LBYV
  DPTR_NKLIN_LBXW=>NKLIN_LBXW
  DPTR_XCOEFLIN_LBXW=>XCOEFLIN_LBXW
  DPTR_NKLIN_LBYW=>NKLIN_LBYW
  DPTR_XCOEFLIN_LBYW=>XCOEFLIN_LBYW
  DPTR_NKLIN_LBXM=>NKLIN_LBXM
  DPTR_XCOEFLIN_LBXM=>XCOEFLIN_LBXM
  DPTR_NKLIN_LBYM=>NKLIN_LBYM
  DPTR_XCOEFLIN_LBYM=>XCOEFLIN_LBYM
  !
  CALL INI_SPAWN_LS_n(NDAD(KMI),XTSTEP,KMI,                                 &
       DPTR_XBMX1,DPTR_XBMX2,DPTR_XBMX3,DPTR_XBMX4,DPTR_XBMY1,DPTR_XBMY2,DPTR_XBMY3,DPTR_XBMY4,      &
       DPTR_XBFX1,DPTR_XBFX2,DPTR_XBFX3,DPTR_XBFX4,DPTR_XBFY1,DPTR_XBFY2,DPTR_XBFY3,DPTR_XBFY4,      &
       NDXRATIO_ALL(KMI),NDYRATIO_ALL(KMI),                  &
       DPTR_CLBCX,DPTR_CLBCY,DPTR_XZZ,DPTR_XZHAT,                                &
       LSLEVE,XLEN1,XLEN2,                                   &
       DPTR_XLSUM,DPTR_XLSVM,DPTR_XLSWM,DPTR_XLSTHM,DPTR_XLSRVM,                      &
       DPTR_XLSUS,DPTR_XLSVS,DPTR_XLSWS,DPTR_XLSTHS,DPTR_XLSRVS,                      &
       DPTR_NKLIN_LBXU,DPTR_XCOEFLIN_LBXU,DPTR_NKLIN_LBYU,DPTR_XCOEFLIN_LBYU,    &
       DPTR_NKLIN_LBXV,DPTR_XCOEFLIN_LBXV,DPTR_NKLIN_LBYV,DPTR_XCOEFLIN_LBYV,    &
       DPTR_NKLIN_LBXW,DPTR_XCOEFLIN_LBXW,DPTR_NKLIN_LBYW,DPTR_XCOEFLIN_LBYW,    &
       DPTR_NKLIN_LBXM,DPTR_XCOEFLIN_LBXM,DPTR_NKLIN_LBYM,DPTR_XCOEFLIN_LBYM     )
  !
  DPTR_XLBXUM=>XLBXUM
  DPTR_XLBYUM=>XLBYUM
  DPTR_XLBXVM=>XLBXVM
  DPTR_XLBYVM=>XLBYVM
  DPTR_XLBXWM=>XLBXWM
  DPTR_XLBYWM=>XLBYWM
  DPTR_XLBXTHM=>XLBXTHM
  DPTR_XLBYTHM=>XLBYTHM
  DPTR_XLBXTKEM=>XLBXTKEM
  DPTR_XLBYTKEM=>XLBYTKEM
  DPTR_XLBXRM=>XLBXRM
  DPTR_XLBYRM=>XLBYRM
  DPTR_XLBXSVM=>XLBXSVM
  DPTR_XLBYSVM=>XLBYSVM
  CALL INI_ONE_WAY_n(NDAD(KMI),XTSTEP,KMI,1,                         &
       DPTR_XBMX1,DPTR_XBMX2,DPTR_XBMX3,DPTR_XBMX4,DPTR_XBMY1,DPTR_XBMY2,DPTR_XBMY3,DPTR_XBMY4,        &
       DPTR_XBFX1,DPTR_XBFX2,DPTR_XBFX3,DPTR_XBFX4,DPTR_XBFY1,DPTR_XBFY2,DPTR_XBFY3,DPTR_XBFY4,        &
       NDXRATIO_ALL(KMI),NDYRATIO_ALL(KMI),NDTRATIO(KMI),      &
       DPTR_CLBCX,DPTR_CLBCY,NRIMX,NRIMY,                                &
       DPTR_NKLIN_LBXU,DPTR_XCOEFLIN_LBXU,DPTR_NKLIN_LBYU,DPTR_XCOEFLIN_LBYU,      &
       DPTR_NKLIN_LBXV,DPTR_XCOEFLIN_LBXV,DPTR_NKLIN_LBYV,DPTR_XCOEFLIN_LBYV,      &
       DPTR_NKLIN_LBXW,DPTR_XCOEFLIN_LBXW,DPTR_NKLIN_LBYW,DPTR_XCOEFLIN_LBYW,      &
       DPTR_NKLIN_LBXM,DPTR_XCOEFLIN_LBXM,DPTR_NKLIN_LBYM,DPTR_XCOEFLIN_LBYM,      &
       CCLOUD, LUSECHAQ, LUSECHIC,                                                 &
       DPTR_XLBXUM,DPTR_XLBYUM,DPTR_XLBXVM,DPTR_XLBYVM,DPTR_XLBXWM,DPTR_XLBYWM,    &
       DPTR_XLBXTHM,DPTR_XLBYTHM,                                                  &
       DPTR_XLBXTKEM,DPTR_XLBYTKEM,                                                &
       DPTR_XLBXRM,DPTR_XLBYRM,DPTR_XLBXSVM,DPTR_XLBYSVM                           )
END IF
!
!
!*       16.    BUILT THE GENERIC OUTPUT NAME
!               ----------------------------
!
WRITE(COUTFILE,'(A,".",I1,".",A)') CEXP,KMI,TRIM(ADJUSTL(CSEG))

!-------------------------------------------------------------------------------
!
!*       17.    INITIALIZE THE PARAMETERS FOR THE DYNAMICS
!               ------------------------------------------
!
CALL INI_DYNAMICS(XLON,XLAT,XRHODJ,XTHVREF,XMAP,XZZ,XDXHAT,XDYHAT,            &
             XZHAT,CLBCX,CLBCY,XTSTEP,                                        &
             LVE_RELAX,LVE_RELAX_GRD,LHORELAX_UVWTH,LHORELAX_RV,              &
             LHORELAX_RC,LHORELAX_RR,LHORELAX_RI,LHORELAX_RS,LHORELAX_RG,     &
             LHORELAX_RH,LHORELAX_TKE,LHORELAX_SV,                            &
             LHORELAX_SVC2R2,LHORELAX_SVC1R3,LHORELAX_SVELEC,LHORELAX_SVLG,   &
             LHORELAX_SVCHEM,LHORELAX_SVAER,LHORELAX_SVDST,LHORELAX_SVSLT,    &
             LHORELAX_SVPP,LHORELAX_SVCS,LHORELAX_SVCHIC,LHORELAX_SVSNW,      &
#ifdef MNH_FOREFIRE
             LHORELAX_SVFF,                                                   &
#endif
             XRIMKMAX,NRIMX,NRIMY,                                            &
             XALKTOP,XALKGRD,XALZBOT,XALZBAS,                                 &
             XT4DIFU,XT4DIFTH,XT4DIFSV,                                       &
             XCORIOX,XCORIOY,XCORIOZ,XCURVX,XCURVY,                           &
             XDXHATM,XDYHATM,XRHOM,XAF,XBFY,XCF,XTRIGSX,XTRIGSY,NIFAXX,NIFAXY,&
             XALK,XALKW,NALBOT,XALKBAS,XALKWBAS,NALBAS,                       &
             LMASK_RELAX,XKURELAX,XKVRELAX,XKWRELAX,                          &
             XDK2U,XDK4U,XDK2TH,XDK4TH,XDK2SV,XDK4SV,                         &
             LZDIFFU,XZDIFFU_HALO2,                                           &
             XBFB,XBF_SXP2_YP1_Z                                              ) 
!
!-------------------------------------------------------------------------------
!
!*      22.    UPDATE HALO
!              -----------
!
!
CALL UPDATE_HALO_ll(TZINITHALO3D_ll,IINFO_ll)
CALL UPDATE_HALO_ll(TZINITHALO2D_ll,IINFO_ll)
CALL CLEANLIST_ll(TZINITHALO3D_ll)
CALL CLEANLIST_ll(TZINITHALO2D_ll)
!
!
!-------------------------------------------------------------------------------
!
!*      23.    DEALLOCATION
!              -------------
!
DEALLOCATE(ZJ)
!

END SUBROUTINE INI_SPECTRE_n

