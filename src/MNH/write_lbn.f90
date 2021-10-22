!MNH_LIC Copyright 1998-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_WRITE_LB_n
!     ######################
!
INTERFACE
!
SUBROUTINE WRITE_LB_n(TPFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
END SUBROUTINE WRITE_LB_n
!
END INTERFACE
!
END MODULE MODI_WRITE_LB_n
!
!
!
!     ##############################
      SUBROUTINE WRITE_LB_n(TPFILE)
!     ##############################
!
!!****  *WRITE_LFIFM_n* - routine to write LB fields in the LFIFM file
!!
!!    PURPOSE
!!    -------
!        The purpose of this routine is to write LB fields in the
!     YFMFILE//'.lfi' with the FM routines.  
!
!!**  METHOD
!!    ------
!!       The LB fields (distributed on the processors) are gathered. Then
!!       they are writen on the file.
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT : FM-routine to write a record
!!      GET_DISTRIBX_LB :  to get the indices of the LB arrays
!!      GET_DISTRIBY_LB    for each sub-domain
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_DIM_n     : contains dimensions
!!      Module MODD_LUNIT_n   : contains logical unit variables.
!!      Module MODD_LSFIELD_n : contains Lateral boundaries variables
!!      Module MODD_CONF_n  : contains configuration variables
!!      Module MODD_PARAM_n   : contains parameterization options
!!      Module MODD_TURB_n    : contains turbulence options
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!  	P Jabouille  *Meteo France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   15/10/98 //
!!      D. Gazen   22/01/01 treat NSV_* with floating indices
!!     J.-P. Pinty 06/05/04 treat NSV_* for C1R3 and ELEC
!!     P. Tulet    06/03/05 treat NSV_* for DUST, SALT and ORILAM
!!                 05/06    Remove KEPS
!!     G. Tanguy   10/09    add ILENCH=LEN(YCOMMENT) after
!!                              change of YCOMMENT
!!     M. Leriche  07/10    add NSV_* for ice phase chemistry
!!     P. Tulet    09/14    modif SALT
!!     J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!    J.-P. Pinty  09/02/16 Add LIMA that is LBC for CCN and IFN
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  S. Bielli      02/2019: Sea salt: significant sea wave height influences salt emission; 5 salt modes
!  P. Wautelet 10/03/2021: use scalar variable names for dust and salt
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIM_n
USE MODD_DYN_n
USE MODD_CONF_n
USE MODD_LSFIELD_n
USE MODD_LUNIT_n
USE MODD_PARAM_n
USE MODD_TURB_n
USE MODD_NSV
USE MODD_PARAM_LIMA
USE MODD_PARAM_n
!
USE MODE_IO_FIELD_WRITE, only: IO_Field_write, IO_Field_write_lb
USE MODE_ll
USE MODE_MSG
USE MODE_MODELN_HANDLER
USE MODE_TOOLS, ONLY: UPCASE
!
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_CH_M9_n,         ONLY: CNAMES, CICNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_PARAM_LIMA_WARM, ONLY: CLIMA_WARM_NAMES
USE MODD_PARAM_LIMA_COLD, ONLY: CLIMA_COLD_NAMES
USE MODD_CH_AEROSOL
USE MODD_CH_AERO_n
USE MODI_CH_AER_REALLFI_n
USE MODD_CONF
USE MODD_REF,   ONLY : XRHODREFZ
USE MODD_CONF,  ONLY : CPROGRAM
USE MODD_GRID_n, ONLY : XZZ
USE MODD_PARAMETERS, ONLY : JPHEXT, JPVEXT
USE MODD_DUST
USE MODD_SALT
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n
USE MODD_PARAMETERS,      ONLY: JPHEXT
USE MODD_IO, ONLY: TFILEDATA
use modd_field, only: tfielddata, TYPELOG, TYPEREAL
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
!*       0.2   Declarations of local variables
!
INTEGER            :: ILUOUT        ! logical unit
!
INTEGER            :: IRR           ! Index for moist variables
INTEGER            :: JRR,JSV       ! loop index for moist and scalar variables
! 
LOGICAL            :: GHORELAX_R, GHORELAX_SV   ! global hor. relax. informations
INTEGER            :: IRIMX,IRIMY   ! size of the RIM zone
CHARACTER (LEN=1), DIMENSION (7) :: YC    ! array with the prefix of the moist variables
LOGICAL, DIMENSION (7)           :: GUSER ! array with the use indicator of the moist variables
REAL,    DIMENSION(SIZE(XLBXSVM, 1), SIZE(XLBXSVM,2), SIZE(XLBXSVM,3)) :: ZRHODREFX
REAL,    DIMENSION(SIZE(XLBYSVM, 1), SIZE(XLBYSVM,2), SIZE(XLBYSVM,3)) :: ZRHODREFY
INTEGER            :: JK
INTEGER            :: IMI    ! Current model index
CHARACTER(LEN=2)   :: INDICE ! to index CCN and IFN fields of LIMA scheme
INTEGER           :: I
INTEGER           :: ILBX,ILBY
INTEGER           :: IIB, IIE, IJB, IJE, IKB, IKE
INTEGER           :: IIU, IJU, IKU
REAL, DIMENSION(SIZE(XLBXSVM,1), SIZE(XLBXSVM,2), SIZE(XLBXSVM,3)) :: ZLBXZZ
REAL, DIMENSION(SIZE(XLBYSVM,1), SIZE(XLBYSVM,2), SIZE(XLBYSVM,3)) :: ZLBYZZ
CHARACTER(LEN=100) :: YMSG
TYPE(TFIELDDATA)   :: TZFIELD
!-------------------------------------------------------------------------------
!
!*       1.    SOME INITIALIZATIONS
!              --------------------
!
ILUOUT = TLUOUT%NLU
!
IMI = GET_CURRENT_MODEL_INDEX()

IIB=JPHEXT+1
IIE=SIZE(XZZ,1)-JPHEXT
IIU=SIZE(XZZ,1)
IJB=JPHEXT+1
IJE=SIZE(XZZ,2)-JPHEXT
IJU=SIZE(XZZ,2)
IKB=JPVEXT+1
IKE=SIZE(XZZ,3)-JPVEXT
IKU=SIZE(XZZ,3)
!
!        2.  WRITE THE DIMENSION OF LB FIELDS
!            --------------------------------
!
CALL IO_Field_write(TPFILE,'RIMX',NRIMX)
CALL IO_Field_write(TPFILE,'RIMY',NRIMY)
!
!*       3.  BASIC VARIABLES
!            --------------
!
CALL IO_Field_write(TPFILE,'HORELAX_UVWTH',LHORELAX_UVWTH)
!
!gathering and writing of the LB fields
IF(NSIZELBXU_ll /= 0) CALL IO_Field_write_lb(TPFILE,'LBXUM', NSIZELBXU_ll,XLBXUM)
IF(NSIZELBX_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBXVM', NSIZELBX_ll,XLBXVM)
IF(NSIZELBX_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBXWM', NSIZELBX_ll,XLBXWM)
IF(NSIZELBY_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBYUM', NSIZELBY_ll,XLBYUM)
IF(NSIZELBYV_ll /= 0) CALL IO_Field_write_lb(TPFILE,'LBYVM', NSIZELBYV_ll,XLBYVM)
IF(NSIZELBY_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBYWM', NSIZELBY_ll,XLBYWM)
IF(NSIZELBX_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBXTHM',NSIZELBX_ll,XLBXTHM)
IF(NSIZELBY_ll  /= 0) CALL IO_Field_write_lb(TPFILE,'LBYTHM',NSIZELBY_ll,XLBYTHM)
!
!*        4  LB-TKE
!            ------
!
IF(CTURB/='NONE') THEN
  CALL IO_Field_write(TPFILE,'HORELAX_TKE',LHORELAX_TKE)
!
  IF(NSIZELBXTKE_ll /= 0) CALL IO_Field_write_lb(TPFILE,'LBXTKEM',NSIZELBXTKE_ll,XLBXTKEM)
  IF(NSIZELBYTKE_ll /= 0) CALL IO_Field_write_lb(TPFILE,'LBYTKEM',NSIZELBYTKE_ll,XLBYTKEM)
END IF
!
!
!*        6  LB-Rx
!            -----
!
IF (NRR >=1) THEN
  GHORELAX_R = LHORELAX_RV .OR. LHORELAX_RC .OR. LHORELAX_RR .OR. &
               LHORELAX_RI .OR. LHORELAX_RS .OR. LHORELAX_RG .OR. &
               LHORELAX_RH
  !
  TZFIELD = TFIELDDATA(                                          &
    CMNHNAME   = 'HORELAX_R',                                    &
    CSTDNAME   = '',                                             &
    CLONGNAME  = 'HORELAX_R',                                    &
    CUNITS     = '',                                             &
    CDIR       = '--',                                           &
    CCOMMENT   = 'Switch to activate the HOrizontal RELAXation', &
    CLBTYPE    = 'NONE',                                         &
    NGRID      = 1,                                              &
    NTYPE      = TYPELOG,                                        &
    NDIMS      = 0,                                              &
    LTIMEDEP   = .FALSE.                                         )
  !
  CALL IO_Field_write(TPFILE,TZFIELD,GHORELAX_R)
  !
  GUSER(:)=(/LUSERV,LUSERC,LUSERR,LUSERI,LUSERS,LUSERG,LUSERH/)
  YC(:)=(/"V","C","R","I","S","G","H"/)
  IRR=0
  !
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = ''
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  ! Loop on moist variables
  DO JRR=1,7
    IF (GUSER(JRR)) THEN
      IRR=IRR+1 
      IF(NSIZELBXR_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBXR'//YC(JRR)//'M'
        TZFIELD%CLONGNAME  = 'LBXR'//YC(JRR)//'M'
        TZFIELD%CLBTYPE    = 'LBX'
        TZFIELD%CCOMMENT   = '2_Y_Z_LBXR'//YC(JRR)//'M'
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXR_ll,XLBXRM(:,:,:,IRR))
      END IF
      !
      IF(NSIZELBYR_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBYR'//YC(JRR)//'M'
        TZFIELD%CLONGNAME  = 'LBYR'//YC(JRR)//'M'
        TZFIELD%CLBTYPE    = 'LBY'
        TZFIELD%CCOMMENT   = '2_Y_Z_LBYR'//YC(JRR)//'M'
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYR_ll,XLBYRM(:,:,:,IRR))
      END IF
    END IF
  END DO
END IF
!
!
!*        7  LB-SV
!            -----
!
IF (NSV >=1) THEN
  GHORELAX_SV=ANY ( LHORELAX_SV )
!
  TZFIELD = TFIELDDATA(        &
    CMNHNAME   = 'HORELAX_SV', &
    CSTDNAME   = '',           &
    CLONGNAME  = 'HORELAX_SV', &
    CUNITS     = '',           &
    CDIR       = '--',         &
    CCOMMENT   = '',           &
    CLBTYPE    = 'NONE',       &
    NGRID      = 0,            &
    NTYPE      = TYPELOG,      &
    NDIMS      = 0,            &
    LTIMEDEP   = .FALSE.       )
  CALL IO_Field_write(TPFILE,TZFIELD,GHORELAX_SV)
!
  IRIMX =(NSIZELBXSV_ll-2*JPHEXT)/2
  IRIMY =(NSIZELBYSV_ll-2*JPHEXT)/2
  IF (NSV_USER>0) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = 1,NSV_USER
      IF(NSIZELBXSV_ll /= 0) THEN
        WRITE(TZFIELD%CMNHNAME,'(A6,I3.3)')'LBXSVM',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        WRITE(TZFIELD%CMNHNAME,'(A6,I3.3)')'LBYSVM',JSV
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  !
  IF (NSV_C2R2END>=NSV_C2R2BEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm-3'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_C2R2BEG,NSV_C2R2END
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  !
  IF (NSV_C1R3END>=NSV_C1R3BEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm-3'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_C1R3BEG,NSV_C1R3END
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
!
! LIMA: CCN and IFN scalar variables
!
  IF (CCLOUD=='LIMA' ) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_LIMA_CCN_FREE,NSV_LIMA_CCN_FREE+NMOD_CCN-1
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_FREE + 1)
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CLIMA_WARM_NAMES(3))//INDICE)
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CLIMA_WARM_NAMES(3))//INDICE)
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    !
    DO JSV = NSV_LIMA_IFN_FREE,NSV_LIMA_IFN_FREE+NMOD_IFN-1
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_FREE + 1)
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CLIMA_COLD_NAMES(2))//INDICE)
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CLIMA_COLD_NAMES(2))//INDICE)
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
!
! ELEC
!
  IF (NSV_ELECEND>=NSV_ELECBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_ELECBEG,NSV_ELECEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  !
  !
  IF (LORILAM) THEN
    DO JK=1,SIZE(XLBXSVM,3)
       ZRHODREFX(:,:,JK) =  XRHODREFZ(JK)
       ZRHODREFY(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
    !
    IF (NSIZELBXSV_ll /= 0) &
      XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND) = MAX(XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND), 0.)
    IF (NSIZELBYSV_ll /= 0) &
      XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND) = MAX(XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND), 0.)
    IF (LDEPOS_AER(IMI).AND.(NSIZELBXSV_ll /= 0)) &
        XLBXSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND) = MAX(XLBXSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND), 0.)      
    IF (LDEPOS_AER(IMI).AND.(NSIZELBYSV_ll /= 0)) &
        XLBYSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND) = MAX(XLBYSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND), 0.)      
    IF (LAERINIT) THEN ! GRIBEX CASE (aerosols initialization)
    IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'REAL  ').AND.(NSP > 1)) &
      CALL CH_AER_REALLFI_n(XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND),XLBXSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),ZRHODREFX)
    IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'REAL  ').AND.(NSP > 1)) &
      CALL CH_AER_REALLFI_n(XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND),XLBYSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),ZRHODREFY)
    IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSP > 1)) &
      CALL CH_AER_REALLFI_n(XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND),XLBXSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),ZRHODREFX)
    IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSP > 1)) & 
      CALL CH_AER_REALLFI_n(XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND),XLBYSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO),ZRHODREFY)
    END IF
    !
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'ppp'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_AERBEG,NSV_AEREND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    !
    IF (LDEPOS_AER(IMI)) THEN
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF             
      END DO      
    END IF  
  END IF
  !
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = ''
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  !
  DO JSV = NSV_CHEMBEG,NSV_CHEMEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
    END IF
  END DO
  !
  DO JSV = NSV_CHICBEG,NSV_CHICEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
    END IF
  END DO
  !
  DO JSV = NSV_LNOXBEG,NSV_LNOXEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_LINOX'
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_LINOX'
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
    END IF
  END DO
  !
  IF (LDUST) THEN
    DO JK=1,size(XLBXSVM,3)
       ZRHODREFX(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
    DO JK=1,size(XLBYSVM,3)
      ZRHODREFY(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
    IF (NSIZELBXSV_ll /= 0) &
      XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND) = MAX(XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), 0.)
    IF (NSIZELBYSV_ll /= 0) &
      XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND) = MAX(XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), 0.)
    IF (LDEPOS_DST(IMI).AND.(NSIZELBXSV_ll /= 0)) &
        XLBXSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND) = MAX(XLBXSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND), 0.)      
    IF (LDEPOS_DST(IMI).AND.(NSIZELBYSV_ll /= 0)) &
        XLBYSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND) = MAX(XLBYSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND), 0.)      
    IF ((LDSTINIT).OR.(LDSTPRES)) THEN ! GRIBEX case (dust initialization)
      IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_DST > 1)) THEN
        CALL DUSTLFI_n(XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFX)
      END IF
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_DST > 1)) THEN
        CALL DUSTLFI_n(XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFY)
      END IF
      IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_DST > 1)) &
        CALL DUSTLFI_n(XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFX)
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_DST > 1)) &
        CALL DUSTLFI_n(XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFY)
    END IF
    !
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'ppp'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_DSTBEG,NSV_DSTEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    IF (LDEPOS_DST(IMI)) THEN
      DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF
      END DO
    END IF
  ENDIF
  !
  IF (LSALT) THEN
    DO JK=1,size(XLBXSVM,3)
      ZRHODREFX(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
    DO JK=1,size(XLBYSVM,3)
      ZRHODREFY(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
    IIU  = SIZE(XZZ,1)
    IJU  = SIZE(XZZ,2)
    IKU  = SIZE(XZZ,3)
    IF (SIZE(ZLBXZZ) .NE. 0 ) THEN
      ILBX=SIZE(ZLBXZZ,1)/2-1
      ZLBXZZ(1:ILBX+1,:,:)         = XZZ(IIB-1:IIB-1+ILBX,:,:)
      ZLBXZZ(ILBX+2:2*ILBX+2,:,:)  = XZZ(IIE+1-ILBX:IIE+1,:,:)
    ENDIF
    IF (SIZE(ZLBYZZ) .NE. 0 ) THEN
      ILBY=SIZE(ZLBYZZ,2)/2-1
      ZLBYZZ(:,1:ILBY+1,:)        = XZZ(:,IJB-1:IJB-1+ILBY,:)
      ZLBYZZ(:,ILBY+2:2*ILBY+2,:) = XZZ(:,IJE+1-ILBY:IJE+1,:)
    ENDIF
    IF (NSIZELBXSV_ll /= 0) &
      XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND) = MAX(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), 0.)
    IF (NSIZELBYSV_ll /= 0) &
      XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND) = MAX(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), 0.)
    IF (LDEPOS_SLT(IMI).AND.(NSIZELBXSV_ll /= 0)) &
        XLBXSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND) = MAX(XLBXSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND), 0.)      
    IF (LDEPOS_SLT(IMI).AND.(NSIZELBYSV_ll /= 0)) &
        XLBYSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND) = MAX(XLBYSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND), 0.)      
    IF ((LSLTINIT).OR.(LSLTPRES)) THEN ! GRIBEX case (dust initialization)
      IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX, ZLBXZZ)
      END IF
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY, ZLBYZZ)
      END IF
      IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX, ZLBXZZ)
      END IF
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY, ZLBYZZ)
      END IF
    END IF
    !
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'ppp'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_SLTBEG,NSV_SLTEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    IF (LDEPOS_SLT(IMI)) THEN
      DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
          TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF
      END DO
    END IF
  ENDIF   
  !
  ! lagrangian variables
  IF (NSV_LGEND>=NSV_LGBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_LGBEG,NSV_LGEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
!
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  ! passive pollutants
  IF (NSV_PPEND>=NSV_PPBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_PPBEG,NSV_PPEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_PP'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_PP'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  ! conditional sampling
  IF (NSV_CSEND>=NSV_CSBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_CSBEG,NSV_CSEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_CS'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_CS'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
#ifdef MNH_FOREFIRE
  ! ForeFire scalar variables
  IF (NSV_FFEND>=NSV_FFBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = ''
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    !
    DO JSV = NSV_FFBEG,NSV_FFEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_FF'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_FF'
        TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_Field_write_lb(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
#endif
END IF
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_LB_n  
