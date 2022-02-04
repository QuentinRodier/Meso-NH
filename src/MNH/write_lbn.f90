!MNH_LIC Copyright 1998-2022 CNRS, Meteo-France and Universite Paul Sabatier
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
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_AEROSOL,     ONLY: JP_CH_CO, LAERINIT, LDEPOS_AER, LORILAM, NSP
USE MODD_CONF,           ONLY: CPROGRAM
USE MODD_CONF_n,         ONLY: LUSERV, LUSERC, LUSERR, LUSERI, LUSERS, LUSERG, LUSERH, NRR
USE MODD_DUST,           ONLY: LDEPOS_DST, LDSTINIT, LDSTPRES, LDUST
USE MODD_DYN_n,          ONLY: LHORELAX_RV, LHORELAX_RC, LHORELAX_RR, LHORELAX_RI, LHORELAX_RS,        &
                               LHORELAX_RG, LHORELAX_RH, LHORELAX_SV, LHORELAX_TKE, LHORELAX_UVWTH,    &
                               NRIMX, NRIMY,                                                           &
                               NSIZELBX_ll, NSIZELBXR_ll, NSIZELBXSV_ll, NSIZELBXTKE_ll, NSIZELBXU_ll, &
                               NSIZELBY_ll, NSIZELBYR_ll, NSIZELBYSV_ll, NSIZELBYTKE_ll, NSIZELBYV_ll
use modd_field,          only: tfieldmetadata, NMNHDIM_UNKNOWN, TYPELOG, TYPEREAL
USE MODD_GRID_n,         ONLY: XZZ
USE MODD_IO,             ONLY: TFILEDATA
USE MODD_LSFIELD_n
USE MODD_NSV
USE MODD_PARAMETERS,     ONLY: JPHEXT, JPVEXT, NLONGNAMELGTMAX, NMNHNAMELGTMAX
USE MODD_PARAM_n,        ONLY: CTURB
USE MODD_REF,            ONLY: XRHODREFZ
USE MODD_SALT,           ONLY: LDEPOS_SLT, LSALT, LSLTINIT, LSLTPRES

USE MODE_IO_FIELD_WRITE, only: IO_Field_write, IO_Field_write_lb
USE MODE_MODELN_HANDLER, ONLY: GET_CURRENT_MODEL_INDEX

USE MODI_CH_AER_REALLFI_n
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n

IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
!*       0.2   Declarations of local variables
!
INTEGER            :: IRR           ! Index for moist variables
INTEGER            :: JRR,JSV       ! loop index for moist and scalar variables
! 
LOGICAL            :: GHORELAX_R, GHORELAX_SV   ! global hor. relax. informations
CHARACTER (LEN=1), DIMENSION (7) :: YC    ! array with the prefix of the moist variables
LOGICAL, DIMENSION (7)           :: GUSER ! array with the use indicator of the moist variables
REAL,    DIMENSION(SIZE(XLBXSVM, 1), SIZE(XLBXSVM,2), SIZE(XLBXSVM,3)) :: ZRHODREFX
REAL,    DIMENSION(SIZE(XLBYSVM, 1), SIZE(XLBYSVM,2), SIZE(XLBYSVM,3)) :: ZRHODREFY
INTEGER            :: JK
INTEGER            :: IMI    ! Current model index
INTEGER           :: ILBX,ILBY
INTEGER           :: IIB, IIE, IJB, IJE, IKB, IKE
REAL, DIMENSION(SIZE(XLBXSVM,1), SIZE(XLBXSVM,2), SIZE(XLBXSVM,3)) :: ZLBXZZ
REAL, DIMENSION(SIZE(XLBYSVM,1), SIZE(XLBYSVM,2), SIZE(XLBYSVM,3)) :: ZLBYZZ
CHARACTER(LEN=NMNHNAMELGTMAX)  :: YMNHNAME_BASE
CHARACTER(LEN=NLONGNAMELGTMAX) :: YLONGNAME_BASE
TYPE(TFIELDMETADATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
!*       1.    SOME INITIALIZATIONS
!              --------------------
!
IMI = GET_CURRENT_MODEL_INDEX()

IIB=JPHEXT+1
IIE=SIZE(XZZ,1)-JPHEXT
IJB=JPHEXT+1
IJE=SIZE(XZZ,2)-JPHEXT
IKB=JPVEXT+1
IKE=SIZE(XZZ,3)-JPVEXT
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
  TZFIELD = TFIELDMETADATA(                                      &
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
  TZFIELD = TFIELDMETADATA( &
    CUNITS     = 'kg kg-1', &
    CDIR       = '',        &
    NGRID      = 1,         &
    NTYPE      = TYPEREAL,  &
    NDIMS      = 3,         &
    LTIMEDEP   = .TRUE.     )

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
  TZFIELD = TFIELDMETADATA(    &
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
  CALL IO_Field_write( TPFILE, TZFIELD, GHORELAX_SV )

  IF ( LORILAM .OR. LDUST .OR. LSALT ) THEN
    DO JK = 1, SIZE( XLBXSVM, 3 )
      ZRHODREFX(:,:,JK) =  XRHODREFZ(JK)
    END DO
    DO JK = 1, SIZE( XLBYSVM, 3 )
      ZRHODREFY(:,:,JK) =  XRHODREFZ(JK)
    ENDDO
  END IF

  IF ( LORILAM ) THEN
    IF ( NSIZELBXSV_ll /= 0 ) XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND) = MAX( XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND), 0. )
    IF ( NSIZELBYSV_ll /= 0 ) XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND) = MAX( XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND), 0. )
    IF ( LDEPOS_AER(IMI) .AND. ( NSIZELBXSV_ll /= 0 ) ) &
        XLBXSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND) = MAX( XLBXSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND), 0. )
    IF ( LDEPOS_AER(IMI) .AND. ( NSIZELBYSV_ll /= 0 ) ) &
        XLBYSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND) = MAX( XLBYSVM(:,:,:,NSV_AERDEPBEG:NSV_AERDEPEND), 0. )
    IF ( LAERINIT ) THEN ! GRIBEX CASE (aerosols initialization)
      IF ( ( NSIZELBXSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSP > 1 ) ) &
        CALL CH_AER_REALLFI_n( XLBXSVM(:,:,:,NSV_AERBEG:NSV_AEREND), XLBXSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO), ZRHODREFX )
      IF ( ( NSIZELBYSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSP > 1 ) ) &
        CALL CH_AER_REALLFI_n( XLBYSVM(:,:,:,NSV_AERBEG:NSV_AEREND), XLBYSVM(:,:,:,NSV_CHEMBEG-1+JP_CH_CO), ZRHODREFY )
    END IF
  END IF

  IF ( LDUST ) THEN
    IF ( NSIZELBXSV_ll /= 0 ) XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND) = MAX( XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), 0. )
    IF ( NSIZELBYSV_ll /= 0 ) XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND) = MAX( XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), 0. )
    IF ( LDEPOS_DST(IMI) .AND. ( NSIZELBXSV_ll /= 0 ) ) &
        XLBXSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND) = MAX( XLBXSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND), 0. )
    IF ( LDEPOS_DST(IMI) .AND. ( NSIZELBYSV_ll /= 0 ) ) &
        XLBYSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND) = MAX( XLBYSVM(:,:,:,NSV_DSTDEPBEG:NSV_DSTDEPEND), 0. )
    IF ( LDSTINIT .OR. LDSTPRES ) THEN ! GRIBEX case (dust initialization)
      IF ( ( NSIZELBXSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSV_DST > 1 ) ) &
        CALL DUSTLFI_n( XLBXSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFX )
      IF ( ( NSIZELBYSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSV_DST > 1 ) ) &
        CALL DUSTLFI_n( XLBYSVM(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZRHODREFY )
    END IF
  END IF

  IF ( LSALT ) THEN
    IF ( SIZE( ZLBXZZ ) > 0 ) THEN
      ILBX = SIZE( ZLBXZZ, 1 ) / 2 - 1
      ZLBXZZ(1:ILBX+1,:,:)         = XZZ(IIB-1:IIB-1+ILBX,:,:)
      ZLBXZZ(ILBX+2:2*ILBX+2,:,:)  = XZZ(IIE+1-ILBX:IIE+1,:,:)
    ENDIF
    IF ( SIZE( ZLBYZZ ) > 0 ) THEN
      ILBY = SIZE( ZLBYZZ, 2 ) / 2 - 1
      ZLBYZZ(:,1:ILBY+1,:)        = XZZ(:,IJB-1:IJB-1+ILBY,:)
      ZLBYZZ(:,ILBY+2:2*ILBY+2,:) = XZZ(:,IJE+1-ILBY:IJE+1,:)
    ENDIF
    IF ( NSIZELBXSV_ll /= 0 ) XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND) = MAX( XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), 0. )
    IF ( NSIZELBYSV_ll /= 0 ) XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND) = MAX( XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), 0. )
    IF ( LDEPOS_SLT(IMI) .AND. ( NSIZELBXSV_ll /= 0 ) ) &
        XLBXSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND) = MAX( XLBXSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND), 0. )
    IF ( LDEPOS_SLT(IMI) .AND. ( NSIZELBYSV_ll /= 0 ) ) &
        XLBYSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND) = MAX( XLBYSVM(:,:,:,NSV_SLTDEPBEG:NSV_SLTDEPEND), 0. )
    IF ( LSLTINIT .OR. LSLTPRES ) THEN ! GRIBEX case (dust initialization)
      IF ( ( NSIZELBXSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSV_SLT > 1 ) ) &
        CALL SALTLFI_n( XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX, ZLBXZZ )
      IF ( ( NSIZELBYSV_ll /= 0 ) .AND. ( TRIM( CPROGRAM ) == 'REAL' .OR. TRIM( CPROGRAM ) == 'IDEAL' ) .AND. ( NSV_SLT > 1 ) ) &
        CALL SALTLFI_n( XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY, ZLBYZZ )
    END IF
  END IF

  DO JSV = 1, NSV
    TZFIELD = TSVLIST(JSV)
    TZFIELD%CDIR = ''
    TZFIELD%NDIMLIST(:) = NMNHDIM_UNKNOWN
    YMNHNAME_BASE  = TRIM( TZFIELD%CMNHNAME )
    YLONGNAME_BASE = TRIM( TZFIELD%CLONGNAME )

    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME  = 'LBX_' // TRIM( YMNHNAME_BASE )
      TZFIELD%CLONGNAME = 'LBX_' // TRIM( YLONGNAME_BASE )
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      TZFIELD%CLBTYPE    = 'LBX'
      CALL IO_Field_write_lb( TPFILE, TZFIELD, NSIZELBXSV_ll, XLBXSVM(:,:,:,JSV) )
    END IF

    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME  = 'LBY_' // TRIM( YMNHNAME_BASE )
      TZFIELD%CLONGNAME = 'LBY_' // TRIM( YLONGNAME_BASE )
!PW: TODO: comment a adapter (a la lecture aussi)
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      TZFIELD%CLBTYPE    = 'LBY'
      CALL IO_Field_write_lb( TPFILE, TZFIELD, NSIZELBYSV_ll, XLBYSVM(:,:,:,JSV) )
    END IF
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_LB_n
