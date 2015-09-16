!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_READ_PRC_FMFILE
!     ###########################
!
INTERFACE 
!
      SUBROUTINE READ_PRC_FMFILE(KIINF,KISUP,KJINF,KJSUP                     )

!
INTEGER,           INTENT(IN)  :: KIINF      !
INTEGER,           INTENT(IN)  :: KISUP      !
INTEGER,           INTENT(IN)  :: KJINF      ! zoom
INTEGER,           INTENT(IN)  :: KJSUP      ! 
!
END SUBROUTINE READ_PRC_FMFILE
!
END INTERFACE
!
END MODULE MODI_READ_PRC_FMFILE
!     ########################################################################
      SUBROUTINE READ_PRC_FMFILE(KIINF,KISUP,KJINF,KJSUP                     )
!     ########################################################################
!
!!****  *READ_PRC_FMFILE* - routine to read prognostic and surface fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!      FMREAD   : to read data in LFIFM file
!!      FMLOOK   : to retrieve the logical unit of a file
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_CONF   : contains configuration variables
!!
!!        NVERB      : Level of informations on output-listing
!!                          0 for minimum  prints
!!                          5 for intermediate level of prints
!!                         10 for maximum  prints 
!!
!!      Module MODD_CONF1
!!
!!        LUSERV
!!        LUSERC
!!        LUSERR
!!        LUSERI
!!        LUSERS
!!        LUSERG
!!        LUSERH
!!        NRR        : number of moist variables
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (routine READ_PRC_FMFILE)
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        01/02/96
!!                      24/09/96 (V. Masson) add reading of DAD_NAME
!!                      25/10/96 (V. Masson) add deallocations
!!                      04/12/96 (V. Masson) add clay and sand fractions
!!                      12/12/96 (V. Masson) add vertical velocity
!!                      13/01/97 (V. Masson) erases lateral boundaries
!!                      07/05/97 (V. Masson) add tke
!!                      09/07/97 (V. Masson) add pressure
!!                      10/07/97 (V. Masson) add epsilon
!!                      11/07/97 (V. Masson) test on configuration variables
!!                      11/07/97 (V. Masson) add scalar mixing ratio
!!                      15/09/97 (V. Masson) interpolation of boundaries with Hu
!!                      16/09/97 (V. Masson) bug in horizontal interpolations
!!                      16/01/98 (V. Masson) add veg and lai reading
!!                      04/06/98 (V. Masson) add cover types and Ts readings
!!                      15/03/99 (V. Masson) use of cover types
!!                      29/11/02 (JP Pinty)  add C3R5, ICE2, ICE4
!!                      01/2004  (V. Masson) removes surface (externalization)
!!                      05/2006              Remove EPS
!!                      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
USE MODD_CST
USE MODD_CONF
USE MODD_CONF_n
USE MODD_PARAM_n
USE MODD_LUNIT
USE MODD_REF
USE MODD_REF_n
USE MODD_PARAMETERS
USE MODD_PRECIP_n
USE MODD_FIELD_n
USE MODD_GR_FIELD_n
USE MODD_LSFIELD_n
USE MODD_GRID_n
USE MODD_TIME_n
USE MODD_PREP_REAL
!
USE MODE_FMREAD
USE MODE_FM
USE MODE_IO_ll
!
USE MODI_TRUNC_FIELD
USE MODI_DEALLOCATE_MODEL1
!
USE MODE_THERMO
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,           INTENT(IN)  :: KIINF      !
INTEGER,           INTENT(IN)  :: KISUP      !
INTEGER,           INTENT(IN)  :: KJINF      ! zoom
INTEGER,           INTENT(IN)  :: KJSUP      ! 
!
!*       0.2   declarations of local variables
!
INTEGER             :: IIU, IJU                   ! sizes of zoomed arrays
INTEGER             :: ILU                        ! vertical size of arrays

INTEGER             :: IRESP                      ! return value
INTEGER             :: ILUOUT0                    ! output file logical unit 
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZHU_LS     ! relative humidity
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZINPRC_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZACPRC_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZINPRR_LS
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: ZINPRR3D_LS
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: ZEVAP3D_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZACPRR_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZINPRS_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZACPRS_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZINPRG_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZACPRG_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZINPRH_LS
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZACPRH_LS
REAL, DIMENSION(:),     ALLOCATABLE :: ZXHAT
REAL, DIMENSION(:),     ALLOCATABLE :: ZYHAT
REAL, DIMENSION(:),     ALLOCATABLE :: ZZHAT
INTEGER  :: IMI
INTEGER         :: IIB, IIE, IJB, IJE
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATIONS
!              ---------------
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(1)

!
IIU=KISUP-KIINF+1
IJU=KJSUP-KJINF+1
!
ILU=SIZE(XTHT,3)
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       2.    WATER VAPOR MUST EXIST IN PREP_REAL_CASE
!              ----------------------------------------
!
IF (.NOT. LUSERV) THEN
  NRR=1
  LUSERV=.TRUE.
  DEALLOCATE(XRT)
  ALLOCATE(XRT(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3),NRR))
  XRT(:,:,:,:) = 0.
END IF
!-------------------------------------------------------------------------------
!
!*       3.    TRUNCATIONS OF FIELDS
!              ---------------------
!
ALLOCATE(XTH_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XTHT,XTH_LS)
IF (SIZE(XTKET)>0) THEN
  ALLOCATE(XTKE_LS(IIU,IJU,ILU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XTKET,XTKE_LS)
ELSE
  ALLOCATE(XTKE_LS(0,0,0))
END IF
ALLOCATE(XU_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XUT,XU_LS)
ALLOCATE(XV_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XVT,XV_LS)
ALLOCATE(XW_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XWT,XW_LS)
ALLOCATE(XPMASS_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XPABST,XPMASS_LS)
ALLOCATE(XR_LS(IIU,IJU,ILU,SIZE(XRT,4)))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XRT,XR_LS)
ALLOCATE(XSV_LS(IIU,IJU,ILU,SIZE(XSVT,4)))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XSVT,XSV_LS)
ALLOCATE(XLSTH_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XLSTHM,XLSTH_LS)
ALLOCATE(XLSU_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XLSUM,XLSU_LS)
ALLOCATE(XLSV_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XLSVM,XLSV_LS)
ALLOCATE(XLSW_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XLSWM,XLSW_LS)
ALLOCATE(XLSRV_LS(IIU,IJU,ILU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XLSRVM,XLSRV_LS)

ALLOCATE(XZS_LS(IIU,IJU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XZS,XZS_LS)
ALLOCATE(XZSMT_LS(IIU,IJU))
CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XZSMT,XZSMT_LS)

IF (SIZE(XINPRC) /= 0 ) THEN
  ALLOCATE(ZINPRC_LS(IIU,IJU))
  ALLOCATE(ZACPRC_LS(IIU,IJU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRC,ZINPRC_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XACPRC,ZACPRC_LS)
END IF
!
IF (SIZE(XINPRR) /= 0 ) THEN
  ALLOCATE(ZINPRR_LS(IIU,IJU))
  ALLOCATE(ZINPRR3D_LS(IIU,IJU,ILU))
  ALLOCATE(ZEVAP3D_LS(IIU,IJU,ILU))
  ALLOCATE(ZACPRR_LS(IIU,IJU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRR,ZINPRR_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRR3D,ZINPRR3D_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XEVAP3D,ZEVAP3D_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XACPRR,ZACPRR_LS)
END IF
!
IF (SIZE(XINPRS) /= 0 ) THEN
  ALLOCATE(ZINPRS_LS(IIU,IJU))
  ALLOCATE(ZACPRS_LS(IIU,IJU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRS,ZINPRS_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XACPRS,ZACPRS_LS)
END IF
!
IF (SIZE(XINPRG) /= 0 ) THEN
  ALLOCATE(ZINPRG_LS(IIU,IJU))
  ALLOCATE(ZACPRG_LS(IIU,IJU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRG,ZINPRG_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XACPRG,ZACPRG_LS)
END IF
!
IF (SIZE(XINPRH) /= 0 ) THEN
  ALLOCATE(ZINPRH_LS(IIU,IJU))
  ALLOCATE(ZACPRH_LS(IIU,IJU))
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XINPRH,ZINPRH_LS)
  CALL TRUNC_FIELD(IIU,IJU,KIINF,KISUP,KJINF,KJSUP,XACPRH,ZACPRH_LS)
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.    SAVE FIELDS STORED IN MODEL1 MODULE already INITIALIZED in PREP_REAL_CASE
!              -------------------------------------------------------------------------
!
ALLOCATE(ZXHAT(SIZE(XXHAT)))
ALLOCATE(ZYHAT(SIZE(XYHAT)))
ALLOCATE(ZZHAT(SIZE(XZHAT)))
ZXHAT(:) = XXHAT(:)
ZYHAT(:) = XYHAT(:)
ZZHAT(:) = XZHAT(:)
!
!-------------------------------------------------------------------------------
!
!*       5.    DEALLOCATIONS OF FIELDS OF MODEL1
!              ---------------------------------
!
CALL DEALLOCATE_MODEL1(1)
CALL DEALLOCATE_MODEL1(2)
CALL DEALLOCATE_MODEL1(3)
!
DEALLOCATE(XRHODREFZ)
DEALLOCATE(XTHVREFZ)
!
!-------------------------------------------------------------------------------
!
!*       6.    RECOVERS FIELDS STORED IN MODEL1 MODULE already INITIALIZED in PREP_REAL_CASE
!              -----------------------------------------------------------------------------
!
ALLOCATE(XXHAT(SIZE(ZXHAT)))
ALLOCATE(XYHAT(SIZE(ZYHAT)))
ALLOCATE(XZHAT(SIZE(ZZHAT)))
XXHAT(:) = ZXHAT(:)
XYHAT(:) = ZYHAT(:)
XZHAT(:) = ZZHAT(:)
DEALLOCATE(ZXHAT)
DEALLOCATE(ZYHAT)
DEALLOCATE(ZZHAT)
!
IF (ALLOCATED(ZINPRC_LS)) THEN
  ALLOCATE(XINPRC(IIU,IJU))
  ALLOCATE(XACPRC(IIU,IJU))
  XINPRC(:,:)=ZINPRC_LS(:,:)
  XACPRC(:,:)=ZACPRC_LS(:,:)
  DEALLOCATE(ZINPRC_LS)
  DEALLOCATE(ZACPRC_LS)
END IF
!
IF (ALLOCATED(ZINPRR_LS)) THEN
  ALLOCATE(XINPRR(IIU,IJU))
  ALLOCATE(XINPRR3D(IIU,IJU,ILU))
  ALLOCATE(XEVAP3D(IIU,IJU,ILU))
  ALLOCATE(XACPRR(IIU,IJU))
  XINPRR(:,:)=ZINPRR_LS(:,:)
  XINPRR3D(:,:,:)=ZINPRR3D_LS(:,:,:)
  XEVAP3D(:,:,:)=ZEVAP3D_LS(:,:,:)
  XACPRR(:,:)=ZACPRR_LS(:,:)
  DEALLOCATE(ZINPRR_LS)
  DEALLOCATE(ZINPRR3D_LS)
  DEALLOCATE(ZEVAP3D_LS)
  DEALLOCATE(ZACPRR_LS)
END IF
!
IF (ALLOCATED(ZINPRS_LS)) THEN
  ALLOCATE(XINPRS(IIU,IJU))
  ALLOCATE(XACPRS(IIU,IJU))
  XINPRS(:,:)=ZINPRS_LS(:,:)
  XACPRS(:,:)=ZACPRS_LS(:,:)
  DEALLOCATE(ZINPRS_LS)
  DEALLOCATE(ZACPRS_LS)
END IF
!
IF (ALLOCATED(ZINPRG_LS)) THEN
  ALLOCATE(XINPRG(IIU,IJU))
  ALLOCATE(XACPRG(IIU,IJU))
  XINPRG(:,:)=ZINPRG_LS(:,:)
  XACPRG(:,:)=ZACPRG_LS(:,:)
  DEALLOCATE(ZINPRG_LS)
  DEALLOCATE(ZACPRG_LS)
END IF
!
IF (ALLOCATED(ZINPRH_LS)) THEN
  ALLOCATE(XINPRH(IIU,IJU))
  ALLOCATE(XACPRH(IIU,IJU))
  XINPRH(:,:)=ZINPRH_LS(:,:)
  XACPRH(:,:)=ZACPRH_LS(:,:)
  DEALLOCATE(ZINPRH_LS)
  DEALLOCATE(ZACPRH_LS)
END IF
!-------------------------------------------------------------------------------
!
!*       7.    ERASES LATERAL BOUNDARIES
!              -------------------------
!
!!$IF (JPHEXT>1) THEN
!!$  WRITE (ILUOUT0,*) 'READ_PRC_FMFILE: abort (JPHEXT= ',JPHEXT,' )'
!!$ !callabortstop
!!$  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
!!$  CALL ABORT
!!$  STOP
!!$END IF
!
!*       7.1   left boundary I=1+JPHEXT for U
!              ------------------------------
!
IF (IIU>3) XU_LS(IIB  ,:,:)=2.*XU_LS(  IIB+1  ,:,:)-XU_LS(  IIB+2  ,:,:)
!
!*       7.2   bottom boundary J=1+JPHEXT for V
!              --------------------------------
!
IF (IJU>3) XV_LS(:,  IJB,:)=2.*XV_LS(:,  IJB+1  ,:)-XV_LS(:,  IJB+2  ,:)
!
!*       7.3   all boundaries for all fields except vapor
!              ------------------------------------------
!
XU_LS(IIB-1  ,:,:)=2.*XU_LS(  IIB  ,:,:)-XU_LS(  IIB+1  ,:,:)
XU_LS(IIE+1,:,:)=2.*XU_LS(IIE,:,:)-XU_LS(IIE-1,:,:)
XV_LS(IIB-1  ,:,:)=2.*XV_LS(  IIB  ,:,:)-XV_LS(  IIB+1  ,:,:)
XV_LS(IIE+1,:,:)=2.*XV_LS(IIE,:,:)-XV_LS(IIE-1,:,:)
XW_LS(IIB-1  ,:,:)=2.*XW_LS(  IIB  ,:,:)-XW_LS(  IIB+1  ,:,:)
XW_LS(IIE+1,:,:)=2.*XW_LS(IIE,:,:)-XW_LS(IIE-1,:,:)
XTH_LS(IIB-1  ,:,:)=2.*XTH_LS(  IIB  ,:,:)-XTH_LS(  IIB+1  ,:,:)
XTH_LS(IIE+1,:,:)=2.*XTH_LS(IIE,:,:)-XTH_LS(IIE-1,:,:)
XR_LS(IIB-1  ,:,:,:)=MAX(2.*XR_LS(  IIB  ,:,:,:)-XR_LS(  IIB+1  ,:,:,:),0.)
XR_LS(IIE+1,:,:,:)=MAX(2.*XR_LS(IIE,:,:,:)-XR_LS(IIE-1,:,:,:),0.)
!
XU_LS(:,  IJB-1,:)=2.*XU_LS(:,  IJB  ,:)-XU_LS(:,  IJB+1  ,:)
XU_LS(:,IJE+1,:)=2.*XU_LS(:,IJE,:)-XU_LS(:,IJE-1,:)
XV_LS(:,  IJB-1,:)=2.*XV_LS(:,  IJB  ,:)-XV_LS(:,  IJB+1  ,:)
XV_LS(:,IJE+1,:)=2.*XV_LS(:,IJE,:)-XV_LS(:,IJE-1,:)
XW_LS(:,  IJB-1,:)=2.*XW_LS(:,  IJB  ,:)-XW_LS(:,  IJB+1  ,:)
XW_LS(:,IJE+1,:)=2.*XW_LS(:,IJE,:)-XW_LS(:,IJE-1,:)
XTH_LS(:,  IJB-1,:)=2.*XTH_LS(:,  IJB  ,:)-XTH_LS(:,  IJB+1  ,:)
XTH_LS(:,IJE+1,:)=2.*XTH_LS(:,IJE,:)-XTH_LS(:,IJE-1,:)
XR_LS(:,  IJB-1,:,:)=MAX(2.*XR_LS(:,  IJB  ,:,:)-XR_LS(:,  IJB+1  ,:,:),0.)
XR_LS(:,IJE+1,:,:)=MAX(2.*XR_LS(:,IJE,:,:)-XR_LS(:,IJE-1,:,:),0.)
!
!*       7.4   all boundaries for vapor (using relative humidity)
!              ------------------------
!
!
ALLOCATE(ZHU_LS(IIU,IJU,ILU))
WHERE (XR_LS(:,:,:,1)>0.)
  ZHU_LS(:,:,:)=100.*XPMASS_LS(:,:,:)/(XRD/XRV/XR_LS(:,:,:,1)+1.)          &
              /SM_FOES( XTH_LS(:,:,:)*(XPMASS_LS(:,:,:)/XP00)**(XRD/XCPD)  )
ELSEWHERE
  ZHU_LS(:,:,:)=0.
END WHERE
!
ZHU_LS(IIB-1  ,:,:)=ZHU_LS(  IIB  ,:,:)
ZHU_LS(IIE+1,:,:)=ZHU_LS(IIE,:,:)
ZHU_LS(:,  IJB-1,:)=ZHU_LS(:,  IJB  ,:)
ZHU_LS(:,IJE+1,:)=ZHU_LS(:,IJE,:)
!
IF (NRR>1) THEN
  WHERE (XR_LS(IIB-1  ,:,:,2)>0.)
    ZHU_LS(IIB-1  ,:,:)=100.
  END WHERE
  WHERE (XR_LS(IIE+1,:,:,2)>0.)
    ZHU_LS(IIE+1,:,:)=100.
  END WHERE
  WHERE (XR_LS(:,  IJB-1,:,2)>0.)
    ZHU_LS(:,  IJB-1,:)=100.
  END WHERE
  WHERE (XR_LS(:,IJE+1,:,2)>0.)
    ZHU_LS(:,IJE+1,:)=100.
  END WHERE
END IF
!
WHERE (ZHU_LS(:,:,:)>0.)
  XR_LS(:,:,:,1)=XRD/XRV/(100./ZHU_LS(:,:,:)*XPMASS_LS(:,:,:)                &
                              /SM_FOES(XTH_LS(:,:,:)*(XPMASS_LS(:,:,:)/XP00) &
                                                     **(XRD/XCPD)           )&
                          -1.)
ELSEWHERE
  XR_LS(:,:,:,1)=0.
END WHERE
DEALLOCATE(ZHU_LS)
!-------------------------------------------------------------------------------
!
!*       8.    ERASES VERTICAL BOUNDARIES
!              --------------------------
!
XU_LS(:,:,1:JPVEXT)=-SPREAD(XU_LS(:,:,JPVEXT+1),3,JPVEXT)
XV_LS(:,:,1:JPVEXT)=-SPREAD(XV_LS(:,:,JPVEXT+1),3,JPVEXT)
!
XU_LS(:,:,ILU-JPVEXT+1:ILU)=SPREAD(XU_LS(:,:,ILU-JPVEXT),3,JPVEXT)
XV_LS(:,:,ILU-JPVEXT+1:ILU)=SPREAD(XV_LS(:,:,ILU-JPVEXT),3,JPVEXT)
!
!-------------------------------------------------------------------------------
!
WRITE (ILUOUT0,*) 'Routine READ_PRC_FMFILE completed'
!
!-------------------------------------------------------------------------------
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE READ_PRC_FMFILE
