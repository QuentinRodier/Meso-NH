!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_real 2006/09/25 14:23:42
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_INI_PROG_VAR
!     ########################
INTERFACE
      SUBROUTINE INI_PROG_VAR(HLUOUT,PTKE_MX,PSV_MX,HCHEMFILE)
!
CHARACTER(LEN=*), INTENT(IN)       :: HLUOUT      ! Name of the output-listing
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTKE_MX
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSV_MX
CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: HCHEMFILE  ! Name of the chem file
END SUBROUTINE INI_PROG_VAR
END INTERFACE
END MODULE MODI_INI_PROG_VAR
!
!     ########################################################
      SUBROUTINE INI_PROG_VAR(HLUOUT,PTKE_MX,PSV_MX,HCHEMFILE)
!     ########################################################
!
!!****  *INI_PROG_VAR* - initialization the prognostic variables not yet 
!!                       initialized
!!
!!    PURPOSE
!!    -------
!!
!!    This routine initializes the scalar variables to zero.
!!    This routine duplicates the values of a variable at t in MODD_FIELD1
!!    or MODD_LSFIELD1 in the variables at t.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!      Routine PGDFILTER      : to filter a 2D field.
!!      Module  MODI_PGDFILTER
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB   : verbosity level for output-listing
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_FIELD1    : contains the prognostic fields of model1
!!         XUM
!!         XVM
!!         XWM
!!         XTHM
!!         XRM
!!      Module MODD_LSFIELD1
!!         XLSUM
!!         XLSVM
!!         XLSWM
!!         XLSTHM
!!         XLSRVM
!!      Module MODD_DYN1
!!         NRIMX,NRIMY
!!   
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/12/94
!!                  14/05/96 (V. Masson) filtering of LS fields
!!                  08/01/97 (V. Masson) no filtering for boundaries on XLSWM
!!                  10/07/97 (V. Masson) add tke and epsilon
!!                  11/07/97 (V. Masson) add scalar variables
!!                  20/01/98 (J. Stein ) add the lB fields + remove the 2Dx filter
!!                  20/08/90 (J. Stein and P. Jabouille) add the SIZE of the LB
!!                           fields
!!                  01/02/01 (D. Gazen) add module MODD_NSV for NSV variable
!!                  May 2006    Remove KEPS
!!                  02/11/09 (M. Leriche) add aqueous phase chemistry
!!                  Oct 2010 (P. Tulet) input of chemical gas, dusts 
!!                                      and sea salts concentration from 
!!                                      another MesoNH simulation
!!                  Aug 2012 (J.-P. Chaboureau) read the chem-file descriptor
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CONF  ! declaration modules
USE MODD_CONF_n
USE MODD_DYN_n
USE MODD_TURB_n
USE MODD_PARAM_n
USE MODD_LUNIT
USE MODD_FIELD_n
USE MODD_LSFIELD_n
USE MODD_PARAMETERS
USE MODD_NSV
USE MODD_CH_M9_n, ONLY : NEQ, CNAMES
USE MODD_CH_MNHC_n, ONLY : LUSECHEM, LUSECHAQ, LUSECHIC, LCH_PH 
USE MODD_CH_AEROSOL
USE MODD_DUST
USE MODD_SALT
!
USE MODN_DUST
USE MODN_SALT
!
USE MODI_PGDFILTER
USE MODI_CH_INIT_SCHEME_n
USE MODI_CH_AER_INIT_SOA
!
USE MODE_POS
USE MODE_FM
USE MODE_IO_ll
USE MODE_FMREAD
USE MODD_DIM_n
!
IMPLICIT NONE
!
!*       0.1   declaration of arguments
!
CHARACTER(LEN=*), INTENT(IN)       :: HLUOUT      ! Name of the output-listing
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTKE_MX
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSV_MX
CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: HCHEMFILE  ! Name of the chem file
!
!*       0.2   declaration of local variables
!
INTEGER                :: ILUOUT                     !  Logical unit number
                                                     ! associated with HLUOUT 
INTEGER                :: IGRID,ILENCH,IRESP,ININAR      !  File 
CHARACTER (LEN=16)     :: YRECFM                               ! management
CHARACTER (LEN=100)    :: YCOMMENT                             ! variables 
CHARACTER(LEN=2)       :: YDIR 
CHARACTER(LEN=32)      :: YDESFM
!
INTEGER                :: IIMAX,IJMAX,IKMAX       !  Dimensions of the chem file
INTEGER :: IMI ! model number
INTEGER :: IIB,IIE,IIU
INTEGER :: IJB,IJE,IJU
INTEGER :: IIU_ll, IJU_ll
INTEGER :: IKU
INTEGER :: ILBX,ILBY
INTEGER :: JSV   ! Loop index
INTEGER :: JMOM, IMOMENTS, JMODE, ISV_NAME_IDX  ! dust modes
INTEGER :: ILUDES                         !  logical unit numbers of DESFM file
LOGICAL :: GFOUND                         ! Return code when searching namelist
!-------------------------------------------------------------------------------
!
CALL GET_MODEL_NUMBER_ll(IMI)
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
IIB=JPHEXT+1
IIE=SIZE(XWT,1)-JPHEXT
IIU=SIZE(XWT,1)
IJB=JPHEXT+1
IJE=SIZE(XWT,2)-JPHEXT
IJU=SIZE(XWT,2)
IKU=SIZE(XWT,3)
IIU_ll=NIMAX_ll + 2 * JPHEXT
IJU_ll=NJMAX_ll + 2 * JPHEXT
!-------------------------------------------------------------------------------
!
!*       1.    TURBULENCE FIELDS
!              -----------------
!
ALLOCATE(XTKET(0,0,0))
ALLOCATE(XSRCT(0,0,0))
IF (CTURB=='TKEL' ) THEN
  ALLOCATE(XTKET(IIU,IJU,IKU))
  XTKET(:,:,:)=PTKE_MX(:,:,:)
  IF (NRR>1) THEN
    ALLOCATE(XSRCT(IIU,IJU,IKU))
    ALLOCATE(XSIGS(IIU,IJU,IKU))
    WHERE (XRT(:,:,:,2)>1.E-10)
      XSRCT(:,:,:)=1.
    ELSEWHERE
      XSRCT(:,:,:)=0.
    END WHERE
    XSIGS(:,:,:)=0.
  ELSE
    ALLOCATE(XSRCT(0,0,0))
    ALLOCATE(XSIGS(0,0,0))
  END IF
ELSE
  ALLOCATE(XTKET(0,0,0))
  ALLOCATE(XSRCT(0,0,0))
  ALLOCATE(XSIGS(0,0,0))
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       3.    PASSIVE SCALAR
!              --------------
!
ALLOCATE(XSVT(0,0,0,0))
IF(PRESENT(HCHEMFILE)) THEN
  WRITE(ILUOUT,*) 'Routine INI_PROG_VAR: CHEMical species read in ',TRIM(HCHEMFILE)

  IF (.NOT.LDUST) THEN
  ! Always initialize chemical scheme variables before INI_NSV call !
    CALL CH_INIT_SCHEME_n(IMI,LUSECHAQ,LUSECHIC,LCH_PH,ILUOUT,NVERB)
! Question CL : Maud a supprime l appel a CH_INIT_CCS ?
    LUSECHEM = .TRUE.
  END IF
  IF (LORILAM) THEN
    CORGANIC = "MPMPO"
    LVARSIGI = .TRUE.
    LVARSIGJ = .TRUE.
    CALL CH_AER_INIT_SOA(ILUOUT,NVERB)
  END IF ! lorilam
  ! initialise NSV_* variables
  CALL INI_NSV(1)
  ALLOCATE(XSVT(IIU,IJU,IKU,NSV))
  ! Read dimensions in chem file and checks with output file
  CALL FMOPEN_ll(HCHEMFILE,'READ',HLUOUT,0,2,NVERB,ININAR,IRESP)
  YRECFM='IMAX'
  YDIR='--'
  CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,IIMAX,IGRID,ILENCH,         &
              YCOMMENT,IRESP)
  IF (IRESP/=0) THEN
    WRITE(ILUOUT,FMT=9000) 
    WRITE(ILUOUT,*) TRIM(YRECFM),' NOT FOUND IN THE CHEM FILE ',HCHEMFILE
   !callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF !IRESP
  YRECFM='JMAX'
  YDIR='--'
  CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,IJMAX,IGRID,ILENCH,         &
              YCOMMENT,IRESP)
  IF (IRESP/=0) THEN
    WRITE(ILUOUT,FMT=9000) 
    WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
!callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF !IRESP
  YRECFM='KMAX'
  YDIR='--'
  CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,IKMAX,IGRID,ILENCH,         &
              YCOMMENT,IRESP)
  IF (IRESP/=0) THEN
    WRITE(ILUOUT,FMT=9000) 
    WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
!callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF !IRESP
  IF ( (IIMAX/=(IIU_ll-2*JPHEXT)) .OR. (IJMAX/=(IJU_ll-2*JPHEXT))      &
                                  .OR. (IKMAX/=(IKU-2*JPVEXT)) ) THEN
    WRITE(ILUOUT,FMT=9000) 
    WRITE(ILUOUT,*) 'THE GRIDS ARE DIFFERENT IN THE OUTPUT FILE :'
    WRITE(ILUOUT,*) IIU_ll-2*JPHEXT,'*',IJU_ll-2*JPHEXT,'*',IKU-2*JPVEXT
    WRITE(ILUOUT,*) 'AND IN THE CHEM FILE :',HCHEMFILE
    WRITE(ILUOUT,*) IIMAX,'*',IJMAX,'*',IKMAX
    !callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF ! IIMAX
  IF (.NOT.LDUST) THEN
  ! Read scalars in chem file 
    DO JSV = NSV_CHEMBEG,NSV_CHEMEND
      YRECFM=TRIM(CNAMES(JSV-NSV_CHEMBEG+1))//'T'
      YDIR='XY'
      CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                  YCOMMENT,IRESP)
      IF (IRESP/=0) THEN
        WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
        XSVT(:,:,:,JSV) = 0.
      END IF !IRESP
    END DO ! JSV
    IF (ALL(XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND) == 0.)) THEN 
      LUSECHEM=.FALSE.
      NEQ = 0
    END IF
  END IF

  IF (LDUST) THEN
    LDSTINIT=.TRUE.
    LDSTPRES=.FALSE.
    YDESFM=TRIM(ADJUSTL(HCHEMFILE))//'.des'
    CALL FMLOOK_ll(YDESFM,HLUOUT,ILUDES,IRESP)
    CALL POSNAM(ILUDES,'NAM_DUST',GFOUND,ILUOUT)
    IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DUST)
    CALL INI_NSV(1)
    IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG+1)/NMODE_DST
    IF (IMOMENTS == 1) THEN
      DO JMODE=1, NMODE_DST
        !Index from which names are picked
        ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + 2
        JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
             +  1                 & !Number of moments in this mode
             + (NSV_DSTBEG -1)      !Previous list of tracers  
        YRECFM = TRIM(YPDUST_INI(ISV_NAME_IDX))//'M'
        YDIR='XY'
        CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (IRESP/=0) THEN
          WRITE(ILUOUT,FMT=9000) 
          WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
          CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
          CALL ABORT
          STOP
        END IF !IRESP
      END DO !JMOD
    ELSE  ! IMOMENTS diff 1
      DO JMODE=1,NMODE_DST
        DO JMOM=1,IMOMENTS
          ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + JMOM
          JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
               + JMOM               & !Number of moments in this mode
               + (NSV_DSTBEG -1)      !Previous list of tracers
          YRECFM = TRIM(YPDUST_INI(ISV_NAME_IDX))//'T'
          YDIR='XY'
          WRITE(ILUOUT,*) 'JPC titi ',YRECFM
          CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                      YCOMMENT,IRESP)
          IF (IRESP/=0) THEN
            WRITE(ILUOUT,FMT=9000) 
            WRITE(ILUOUT,*) TRIM(YRECFM),' NOT FOUND IN THE CHEM FILE ',HCHEMFILE
            STOP
          END IF !IRESP
        END DO ! JMOM
      END DO !JMOD
    END IF !if IMOMENTS    
  END IF  ! LDUST

  IF (LSALT) THEN
    LSLTINIT=.TRUE.
    LSLTPRES=.FALSE.
    CALL POSNAM(ILUDES,'NAM_SALT',GFOUND,ILUOUT)
    IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_SALT)
    CALL INI_NSV(1)
    IMOMENTS = INT(NSV_SLTEND - NSV_SLTBEG+1)/NMODE_SLT
    IF (IMOMENTS == 1) THEN
      DO JMODE=1, NMODE_SLT
        !Index from which names are picked
        ISV_NAME_IDX = (JPSALTORDER(JMODE) - 1)*3 + 2
        JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
             +  1                 & !Number of moments in this mode
             + (NSV_SLTBEG -1)      !Previous list of tracers  
        YRECFM = TRIM(YPSALT_INI(ISV_NAME_IDX))//'T'
        YDIR='XY'
        CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                    YCOMMENT,IRESP)
        IF (IRESP/=0) THEN
          WRITE(ILUOUT,FMT=9000) 
          WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
          CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
          CALL ABORT
          STOP
        END IF !IRESP
      END DO !JMOD
    ELSE  ! IMOMENTS
      DO JMODE=1,NMODE_SLT
        DO JMOM=1,IMOMENTS
          ISV_NAME_IDX = (JPSALTORDER(JMODE) - 1)*3 + JMOM
          JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
               + JMOM               & !Number of moments in this mode
               + (NSV_SLTBEG -1)      !Previous list of tracers
          YRECFM = TRIM(YPSALT_INI(ISV_NAME_IDX))//'T'
          YDIR='XY'
          CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                      YCOMMENT,IRESP)
          IF (IRESP/=0) THEN
            WRITE(ILUOUT,FMT=9000) 
            WRITE(ILUOUT,*) TRIM(YRECFM),' NOT FOUND IN THE CHEM FILE ',HCHEMFILE
            STOP
          END IF !IRESP
        END DO ! JMOM
      END DO !JMOD
    END IF !if IMOMENTS    
  END IF  ! LSALT

  DO JSV = NSV_AERBEG,NSV_AEREND
    YRECFM=TRIM(CAERONAMES(JSV-NSV_AERBEG+1))//'T'
    YDIR='XY'
    CALL FMREAD(HCHEMFILE,YRECFM,HLUOUT,YDIR,XSVT(:,:,:,JSV),IGRID,ILENCH,  &
                YCOMMENT,IRESP)
    IF (IRESP/=0) THEN
      WRITE(ILUOUT,FMT=9000) 
      WRITE(ILUOUT,*) TRIM(YRECFM),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
!callabortstop
!CALL ABORT
!      STOP
      LORILAM=.FALSE.
    END IF !IRESP
  END DO ! JSV
  CALL FMCLOS_ll(HCHEMFILE,'KEEP',HLUOUT,IRESP)

ELSE ! HCHEMFILE
  IF (NSV >=1) THEN
    ALLOCATE(XSVT(IIU,IJU,IKU,NSV))
    XSVT(:,:,:,:)=PSV_MX(:,:,:,:)
  ELSE !NSV
    ALLOCATE(XSVT(0,0,0,0))
  END IF ! NSV
ENDIF ! HCHEMFILE
!-------------------------------------------------------------------------------
!
!*       4.    2D LARGE SCALE FIELDS FOR LBC
!              -----------------------------
!
!
IF (CTURB /= 'NONE') THEN
  IF ( LHORELAX_TKE) THEN
    ALLOCATE(XLBXTKEM(2*NRIMX+2,IJU,IKU))
    ALLOCATE(XLBYTKEM(IIU,2*NRIMY+2,IKU))
  ELSE
    ALLOCATE(XLBXTKEM(2,IJU,IKU))
    ALLOCATE(XLBYTKEM(IIU,2,IKU))
  END IF 
  !       
  ILBX=SIZE(XLBXTKEM,1)/2-1     
  XLBXTKEM(1:ILBX+1,:,:)         = XTKET(IIB-1:IIB-1+ILBX,:,:)
  XLBXTKEM(ILBX+2:2*ILBX+2,:,:)  = XTKET(IIE+1-ILBX:IIE+1,:,:)
  ILBY=SIZE(XLBYTKEM,2)/2-1
  XLBYTKEM(:,1:ILBY+1,:)        = XTKET(:,IJB-1:IJB-1+ILBY,:)
  XLBYTKEM(:,ILBY+2:2*ILBY+2,:) = XTKET(:,IJE+1-ILBY:IJE+1,:)
ELSE
  ALLOCATE(XLBXTKEM(0,0,0))
  ALLOCATE(XLBYTKEM(0,0,0))
END IF  
!
IF ( NSV > 0 ) THEN 
  IF ( ANY( LHORELAX_SV(:)) ) THEN
    ALLOCATE(XLBXSVM(2*NRIMX+2,IJU,IKU,NSV))
    ALLOCATE(XLBYSVM(IIU,2*NRIMY+2,IKU,NSV))
  ELSE
    ALLOCATE(XLBXSVM(2,IJU,IKU,NSV))
    ALLOCATE(XLBYSVM(IIU,2,IKU,NSV))
  END IF
  !       
  ILBX=SIZE(XLBXSVM,1)/2-1     
  XLBXSVM(1:ILBX+1,:,:,:)         = XSVT(IIB-1:IIB-1+ILBX,:,:,:)
  XLBXSVM(ILBX+2:2*ILBX+2,:,:,:)  = XSVT(IIE+1-ILBX:IIE+1,:,:,:)
  ILBY=SIZE(XLBYSVM,2)/2-1
  XLBYSVM(:,1:ILBY+1,:,:)        = XSVT(:,IJB-1:IJB-1+ILBY,:,:)
  XLBYSVM(:,ILBY+2:2*ILBY+2,:,:) = XSVT(:,IJE+1-ILBY:IJE+1,:,:)
ELSE
  ALLOCATE(XLBXSVM(0,0,0,0))
  ALLOCATE(XLBYSVM(0,0,0,0))
END IF
!
!
NSIZELBXTKE_ll=SIZE(XLBXTKEM,1)
NSIZELBYTKE_ll=SIZE(XLBYTKEM,2)
NSIZELBXSV_ll =SIZE(XLBXSVM,1)
NSIZELBYSV_ll =SIZE(XLBYSVM,2)
!
!-------------------------------------------------------------------------------
9000  FORMAT(/,'FATAL ERROR IN INI_PROG_VAR : ',/, &
             '--------------------------------------' )
!
END SUBROUTINE INI_PROG_VAR
