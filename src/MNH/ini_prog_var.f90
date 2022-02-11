!MNH_LIC Copyright 1994-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_INI_PROG_VAR
!     ########################
INTERFACE
      SUBROUTINE INI_PROG_VAR(PTKE_MX, PSV_MX, HCHEMFILE)
!
REAL,DIMENSION(:,:,:),  INTENT(IN)          :: PTKE_MX
REAL,DIMENSION(:,:,:,:),INTENT(IN)          :: PSV_MX
CHARACTER(LEN=*),       INTENT(IN),OPTIONAL :: HCHEMFILE  ! Name of the chem file
END SUBROUTINE INI_PROG_VAR
END INTERFACE
END MODULE MODI_INI_PROG_VAR
!
!     ###################################################
      SUBROUTINE INI_PROG_VAR(PTKE_MX, PSV_MX, HCHEMFILE)
!     ###################################################
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB   : verbosity level for output-listing
!!      Module MODD_LUNIT_n     :  contains logical unit names for all models
!!         TLUOUT : name of output-listing
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
!!                  Fev 2015  (J.-P. Chaboureau) read instant T insteed of M
!!                  J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!                  Mai 2017 (M. Leriche) read aerosol namelists before call ini_nsv
!!                  Mai 2017 (M. Leriche) Get wet dep. sv in Meso-NH init file
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!  P. Wautelet 09/03/2021: simplify allocation of scalar variable names
!  P. Wautelet 09/03/2021: move some chemistry initializations to ini_nsv
!  P. Wautelet 10/03/2021: move scalar variable name initializations to ini_nsv
!  P. Wautelet 10/03/2021: use scalar variable names for dust and salt
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CH_M9_n,          ONLY: NEQ
USE MODD_CH_MNHC_n,        ONLY: LUSECHEM
USE MODD_CONF,             ONLY: NVERB
USE MODD_CONF_n,           ONLY: NRR
USE MODD_DIM_n,            ONLY: NIMAX_ll, NJMAX_ll
USE MODD_DYN_n,            ONLY: LHORELAX_SV, LHORELAX_TKE, NRIMX, NRIMY, &
                                 NSIZELBXSV_ll, NSIZELBYSV_ll, NSIZELBXTKE_ll, NSIZELBYTKE_ll
use modd_field,            only: TFIELDMETADATA
USE MODD_FIELD_n,          ONLY: XRT, XSIGS, XSRCT, XSVT, XTKET, XWT
USE MODD_IO,               ONLY: TFILEDATA
USE MODD_LSFIELD_n,        ONLY: XLBXSVM, XLBYSVM, XLBXTKEM, XLBYTKEM
USE MODD_LUNIT_n,          ONLY: TLUOUT
USE MODD_NSV,              ONLY: NSV, NSV_AERBEG, NSV_AEREND, NSV_AERDEPBEG, NSV_AERDEPEND, NSV_CHEMBEG, NSV_CHEMEND, &
                                 NSV_DSTBEG, NSV_DSTEND, NSV_DSTDEPBEG, NSV_DSTDEPEND, NSV_SLTBEG, NSV_SLTEND,        &
                                 NSV_SLTDEPBEG, NSV_SLTDEPEND, TSVLIST
USE MODD_PARAMETERS,       ONLY: JPHEXT, JPVEXT
USE MODD_PARAM_n,          ONLY: CTURB
!
USE MODE_IO_FIELD_READ,    only: IO_Field_read
USE MODE_IO_FILE,          ONLY: IO_File_close, IO_File_open
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_add2list
USE MODE_MODELN_HANDLER,   ONLY: GET_CURRENT_MODEL_INDEX
USE MODE_MSG
USE MODE_POS,              ONLY: POSNAM
use mode_tools_ll,         only: GET_INDICE_ll
!
USE MODN_CH_ORILAM
USE MODN_DUST
USE MODN_SALT
!
IMPLICIT NONE
!
!*       0.1   declaration of arguments
!
REAL,DIMENSION(:,:,:),  INTENT(IN)          :: PTKE_MX
REAL,DIMENSION(:,:,:,:),INTENT(IN)          :: PSV_MX
CHARACTER(LEN=*),       INTENT(IN),OPTIONAL :: HCHEMFILE  ! Name of the chem file
!
!*       0.2   declaration of local variables
!
INTEGER                :: ILUOUT
INTEGER                :: IRESP
!
INTEGER                :: IIMAX,IJMAX,IKMAX       !  Dimensions of the chem file
INTEGER :: IMI ! model number
INTEGER :: IIB,IIE,IIU
INTEGER :: IJB,IJE,IJU
INTEGER :: IIU_ll, IJU_ll
INTEGER :: IKU
INTEGER :: ILBX,ILBY
INTEGER :: JSV   ! Loop index
INTEGER :: JMOM, IMOMENTS, JMODE, ISV_NAME_IDX, IMODEIDX  ! dust and salt modes
INTEGER :: ILUDES                         !  logical unit numbers of DESFM file
LOGICAL :: GFOUND                         ! Return code when searching namelist
LOGICAL :: GOLDFILEFORMAT
TYPE(TFIELDMETADATA)    :: TZFIELD
TYPE(TFILEDATA),POINTER :: TZCHEMFILE => NULL()
!-------------------------------------------------------------------------------
!
! get model index
IMI = GET_CURRENT_MODEL_INDEX()
!
ILUOUT = TLUOUT%NLU
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IIU=SIZE(XWT,1)
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
IF(PRESENT(HCHEMFILE)) THEN
  WRITE(ILUOUT,*) 'Routine INI_PROG_VAR: CHEMical species read in ',TRIM(HCHEMFILE)
  ! Read dimensions in chem file and checks with output file
  CALL IO_File_add2list(TZCHEMFILE,TRIM(HCHEMFILE),'MNH','READ',KLFITYPE=2,KLFIVERB=NVERB)
  CALL IO_File_open(TZCHEMFILE)

  !If TZCHEMFILE file was written with a MesoNH version < 5.5.1, some variables had different names (or were not available)
  GOLDFILEFORMAT = ( TZCHEMFILE%NMNHVERSION(1) < 5                                                                              &
              .OR. ( TZCHEMFILE%NMNHVERSION(1) == 5 .AND. TZCHEMFILE%NMNHVERSION(2) < 5 )                                       &
              .OR. ( TZCHEMFILE%NMNHVERSION(1) == 5 .AND. TZCHEMFILE%NMNHVERSION(2) == 5  .AND. TZCHEMFILE%NMNHVERSION(3) < 1 ) )

  ILUDES = TZCHEMFILE%TDESFILE%NLU
  !
  CALL IO_Field_read(TZCHEMFILE,'IMAX',IIMAX,IRESP)
  IF (IRESP/=0) THEN
   !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR','IMAX not found in the CHEM file '//TRIM(HCHEMFILE))
  END IF !IRESP
  !
  CALL IO_Field_read(TZCHEMFILE,'JMAX',IJMAX,IRESP)
  IF (IRESP/=0) THEN
!callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR','JMAX not found in the CHEM file '//TRIM(HCHEMFILE))
  END IF !IRESP
  !
  CALL IO_Field_read(TZCHEMFILE,'KMAX',IKMAX,IRESP)
  IF (IRESP/=0) THEN
!callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR','KMAX not found in the CHEM file '//TRIM(HCHEMFILE))
  END IF !IRESP
  IF ( (IIMAX/=(IIU_ll-2*JPHEXT)) .OR. (IJMAX/=(IJU_ll-2*JPHEXT))      &
                                  .OR. (IKMAX/=(IKU-2*JPVEXT)) ) THEN
    WRITE(ILUOUT,*) 'THE GRIDS ARE DIFFERENT IN THE OUTPUT FILE :'
    WRITE(ILUOUT,*) IIU_ll-2*JPHEXT,'*',IJU_ll-2*JPHEXT,'*',IKU-2*JPVEXT
    WRITE(ILUOUT,*) 'AND IN THE CHEM FILE :',HCHEMFILE
    WRITE(ILUOUT,*) IIMAX,'*',IJMAX,'*',IKMAX
    !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR','')
  END IF ! IIMAX
  IF (.NOT.LDUST) THEN
    LUSECHEM = .TRUE.
  END IF
  IF (LORILAM) THEN
    CALL POSNAM(ILUDES,'NAM_CH_ORILAM',GFOUND,ILUOUT)
    IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_ORILAM)
  ENDIF
  IF (LDUST) THEN
    LDSTINIT=.TRUE.
    LDSTPRES=.FALSE.
    CALL POSNAM(ILUDES,'NAM_DUST',GFOUND,ILUOUT)
    IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DUST)
  ENDIF
  IF (LSALT) THEN
    LSLTINIT=.TRUE.
    LSLTPRES=.FALSE.
    CALL POSNAM(ILUDES,'NAM_SALT',GFOUND,ILUOUT)
    IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_SALT)
  ENDIF

  ! initialise NSV_* variables
  CALL INI_NSV(IMI)
  ALLOCATE(XSVT(IIU,IJU,IKU,NSV))

  ! Read scalars in chem file
  IF (.NOT.LDUST) THEN
    DO JSV = NSV_CHEMBEG, NSV_CHEMEND
      TZFIELD = TSVLIST(JSV)
      IF ( GOLDFILEFORMAT ) THEN
        TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
        TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
      END IF
      CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
      IF (IRESP/=0) THEN
        WRITE(ILUOUT,*) TRIM(TZFIELD%CMNHNAME),' NOT FOUND IN THE CHEM FILE ',HCHEMFILE
        XSVT(:,:,:,JSV) = 0.
      END IF !IRESP
    END DO ! JSV
    IF (ALL(XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND) == 0.)) THEN 
      LUSECHEM=.FALSE.
      NEQ = 0
    END IF
  END IF

  IF (LORILAM) THEN
    DO JSV = NSV_AERBEG,NSV_AEREND
      TZFIELD = TSVLIST(JSV)
      IF ( GOLDFILEFORMAT ) THEN
        TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
        TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
      END IF
      CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
      IF (IRESP/=0) THEN
        WRITE(ILUOUT,*) TRIM(TZFIELD%CMNHNAME),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
        LORILAM=.FALSE.
      END IF !IRESP
    END DO ! JSV
    !
    IF (LDEPOS_AER(IMI)) THEN   
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        TZFIELD = TSVLIST(JSV)
        IF ( GOLDFILEFORMAT ) THEN
          TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
          TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
        END IF
        CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
        IF (IRESP/=0) THEN
          WRITE(ILUOUT,*) TRIM(TZFIELD%CMNHNAME),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
          LDEPOS_AER(IMI)=.FALSE.
        END IF !IRESP
      END DO ! JSV
    END IF ! ldepos_aer
  END IF ! lorilam
  
  IF (LDUST) THEN
    DO JSV = NSV_DSTBEG, NSV_DSTEND
      TZFIELD = TSVLIST(JSV)
      IF ( GOLDFILEFORMAT ) THEN
        TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
        TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
      END IF
      CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
      IF (IRESP/=0) THEN
        CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR',TRIM(TZFIELD%CMNHNAME)//' not found in the CHEM file '//TRIM(HCHEMFILE))
      END IF !IRESP
    END DO ! JSV

    IF (LDEPOS_DST(IMI)) THEN   
      DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
        TZFIELD = TSVLIST(JSV)
        IF ( GOLDFILEFORMAT ) THEN
          TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
          TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
        END IF
        CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
        IF (IRESP/=0) THEN
          WRITE(ILUOUT,*) TRIM(TZFIELD%CMNHNAME),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
          LDEPOS_DST(IMI)=.FALSE.
        END IF !IRESP
      END DO ! JSV
    END IF ! ldepos_dst
  END IF  ! LDUST

  IF (LSALT) THEN
    DO JSV = NSV_SLTBEG, NSV_SLTEND
      TZFIELD = TSVLIST(JSV)
      IF ( GOLDFILEFORMAT ) THEN
        TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
        TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
      END IF
      CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
      IF (IRESP/=0) THEN
        CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_PROG_VAR',TRIM(TZFIELD%CMNHNAME)//' not found in the CHEM file '//TRIM(HCHEMFILE))
      END IF !IRESP
    END DO ! JSV
    !
    IF (LDEPOS_SLT(IMI)) THEN
      DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
        TZFIELD = TSVLIST(JSV)
        IF ( GOLDFILEFORMAT ) THEN
          TZFIELD%CMNHNAME  = TRIM( TZFIELD%CMNHNAME )  // 'T'
          TZFIELD%CLONGNAME = TRIM( TZFIELD%CLONGNAME ) // 'T'
        END IF
        CALL IO_Field_read(TZCHEMFILE,TZFIELD,XSVT(:,:,:,JSV),IRESP)
        IF (IRESP/=0) THEN
          WRITE(ILUOUT,*) TRIM(TZFIELD%CMNHNAME),'NOT FOUND IN THE CHEM FILE ',HCHEMFILE
          LDEPOS_SLT(IMI)=.FALSE.
        END IF !IRESP
      END DO ! JSV      
    ENDIF ! ldepos_slt 
  END IF  ! LSALT
  !
  CALL IO_File_close(TZCHEMFILE)
  !
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
    ALLOCATE(XLBXTKEM(2*NRIMX+2*JPHEXT,IJU,IKU))
    ALLOCATE(XLBYTKEM(IIU,2*NRIMY+2*JPHEXT,IKU))
  ELSE
    ALLOCATE(XLBXTKEM(2*JPHEXT,IJU,IKU))
    ALLOCATE(XLBYTKEM(IIU,2*JPHEXT,IKU))
  END IF 
  !       
  ILBX=SIZE(XLBXTKEM,1)/2-JPHEXT     
  XLBXTKEM(1:ILBX+JPHEXT,:,:)                  = XTKET(1:ILBX+JPHEXT,:,:)
  XLBXTKEM(ILBX+JPHEXT+1:2*ILBX+2*JPHEXT,:,:)  = XTKET(IIE+1-ILBX:IIE+JPHEXT,:,:)
  ILBY=SIZE(XLBYTKEM,2)/2-JPHEXT
  XLBYTKEM(:,1:ILBY+JPHEXT,:)                  = XTKET(:,1:ILBY+JPHEXT,:)
  XLBYTKEM(:,ILBY+JPHEXT+1:2*ILBY+2*JPHEXT,:)  = XTKET(:,IJE+1-ILBY:IJE+JPHEXT,:)
ELSE
  ALLOCATE(XLBXTKEM(0,0,0))
  ALLOCATE(XLBYTKEM(0,0,0))
END IF  
!
IF ( NSV > 0 ) THEN 
  IF ( ANY( LHORELAX_SV(:)) ) THEN
    ALLOCATE(XLBXSVM(2*NRIMX+2*JPHEXT,IJU,IKU,NSV))
    ALLOCATE(XLBYSVM(IIU,2*NRIMY+2*JPHEXT,IKU,NSV))
  ELSE
    ALLOCATE(XLBXSVM(2*JPHEXT,IJU,IKU,NSV))
    ALLOCATE(XLBYSVM(IIU,2*JPHEXT,IKU,NSV))
  END IF
  !       
  ILBX=SIZE(XLBXSVM,1)/2-JPHEXT     
  XLBXSVM(1:ILBX+JPHEXT,:,:,:)                  = XSVT(1:ILBX+JPHEXT,:,:,:)
  XLBXSVM(ILBX+JPHEXT+1:2*ILBX+2*JPHEXT,:,:,:)  = XSVT(IIE+1-ILBX:IIE+JPHEXT,:,:,:)
  ILBY=SIZE(XLBYSVM,2)/2-JPHEXT
  XLBYSVM(:,1:ILBY+JPHEXT,:,:)                  = XSVT(:,1:ILBY+JPHEXT,:,:)
  XLBYSVM(:,ILBY+JPHEXT+1:2*ILBY+2*JPHEXT,:,:)  = XSVT(:,IJE+1-ILBY:IJE+JPHEXT,:,:)
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
!
END SUBROUTINE INI_PROG_VAR
