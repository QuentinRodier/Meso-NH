!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/write_lbn.f90,v $ $Revision: 1.2.2.4.2.2.2.2.10.1.2.2 $
! masdev4_8 init 2008/06/30 12:13:35
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_WRITE_LB_n
!     ######################
!
INTERFACE
!
SUBROUTINE WRITE_LB_n(TPFILE)
!
USE MODD_IO_ll, ONLY: TFILEDATA
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
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll, ONLY: UPCASE, CLOSE_ll
USE MODE_MSG
USE MODE_MODELN_HANDLER
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
USE MODD_DUST
USE MODD_SALT
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n
USE MODD_PARAMETERS,      ONLY: JPHEXT
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODE_FIELD, ONLY: TFIELDDATA,TYPELOG,TYPEREAL
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
INTEGER            :: IRESP         ! IRESP  : return-code for fmwrit
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
!         Integers, counters for dust modes
INTEGER            :: JMOM, IMOMENTS, JMODE, ISV_NAME_IDX
INTEGER            :: IMI    ! Current model index
CHARACTER(LEN=2)   :: INDICE ! to index CCN and IFN fields of LIMA scheme
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
!
!        2.  WRITE THE DIMENSION OF LB FIELDS
!            --------------------------------
!
CALL IO_WRITE_FIELD(TPFILE,'RIMX',NRIMX)
CALL IO_WRITE_FIELD(TPFILE,'RIMY',NRIMY)
!
!*       3.  BASIC VARIABLES
!            --------------
!
CALL IO_WRITE_FIELD(TPFILE,'HORELAX_UVWTH',LHORELAX_UVWTH)
!
!gathering and writing of the LB fields
IF(NSIZELBXU_ll /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBXUM', NSIZELBXU_ll,XLBXUM)
IF(NSIZELBX_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBXVM', NSIZELBX_ll,XLBXVM)
IF(NSIZELBX_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBXWM', NSIZELBX_ll,XLBXWM)
IF(NSIZELBY_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBYUM', NSIZELBY_ll,XLBYUM)
IF(NSIZELBYV_ll /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBYVM', NSIZELBYV_ll,XLBYVM)
IF(NSIZELBY_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBYWM', NSIZELBY_ll,XLBYWM)
IF(NSIZELBX_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBXTHM',NSIZELBX_ll,XLBXTHM)
IF(NSIZELBY_ll  /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBYTHM',NSIZELBY_ll,XLBYTHM)
!
!*        4  LB-TKE
!            ------
!
IF(CTURB/='NONE') THEN
  CALL IO_WRITE_FIELD(TPFILE,'HORELAX_TKE',LHORELAX_TKE)
!
  IF(NSIZELBXTKE_ll /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBXTKEM',NSIZELBXTKE_ll,XLBXTKEM)
  IF(NSIZELBYTKE_ll /= 0) CALL IO_WRITE_FIELD_LB(TPFILE,'LBYTKEM',NSIZELBYTKE_ll,XLBYTKEM)
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
  TZFIELD%CMNHNAME   = 'HORELAX_R'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: HORELAX_R'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = 'Switch to activate the HOrizontal RELAXation'
  TZFIELD%CLBTYPE    = 'NONE'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPELOG
  TZFIELD%NDIMS      = 0
  !
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,GHORELAX_R)
  !
  GUSER(:)=(/LUSERV,LUSERC,LUSERR,LUSERI,LUSERS,LUSERG,LUSERH/)
  YC(:)=(/"V","C","R","I","S","G","H"/)
  IRR=0
  !
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  ! Loop on moist variables
  DO JRR=1,7
    IF (GUSER(JRR)) THEN
      IRR=IRR+1 
      IF(NSIZELBXR_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBXR'//YC(JRR)//'M'
        TZFIELD%CLONGNAME  = 'MesoNH: LBXR'//YC(JRR)//'M'
        TZFIELD%CLBTYPE    = 'LBX'
        TZFIELD%CCOMMENT   = '2_Y_Z_LBXR'//YC(JRR)//'M'
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXR_ll,XLBXRM(:,:,:,IRR))
      END IF
      !
      IF(NSIZELBYR_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBYR'//YC(JRR)//'M'
        TZFIELD%CLONGNAME  = 'MesoNH: LBYR'//YC(JRR)//'M'
        TZFIELD%CLBTYPE    = 'LBY'
        TZFIELD%CCOMMENT   = '2_Y_Z_LBYR'//YC(JRR)//'M'
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXR_ll,XLBYRM(:,:,:,IRR))
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
  TZFIELD%CMNHNAME   = 'HORELAX_SV'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: HORELAX_SV'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%CLBTYPE    = 'NONE'
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPELOG
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,GHORELAX_SV)
!
  IRIMX =(NSIZELBXSV_ll-2*JPHEXT)/2
  IRIMY =(NSIZELBYSV_ll-2*JPHEXT)/2
  IF (NSV_USER>0) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = 1,NSV_USER
      IF(NSIZELBXSV_ll /= 0) THEN
        WRITE(TZFIELD%CMNHNAME,'(A6,I3.3)')'LBXSVM',JSV
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        WRITE(TZFIELD%CMNHNAME,'(A6,I3.3)')'LBYSVM',JSV
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  !
  IF (NSV_C2R2END>=NSV_C2R2BEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm-3'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_C2R2BEG,NSV_C2R2END
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  !
  IF (NSV_C1R3END>=NSV_C1R3BEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm-3'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_C1R3BEG,NSV_C1R3END
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
!
! LIMA: CCN and IFN scalar variables
!
  IF (CCLOUD=='LIMA' ) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_LIMA_CCN_FREE,NSV_LIMA_CCN_FREE+NMOD_CCN-1
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_CCN_FREE + 1)
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CLIMA_WARM_NAMES(3))//INDICE)
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CLIMA_WARM_NAMES(3))//INDICE)
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    !
    DO JSV = NSV_LIMA_IFN_FREE,NSV_LIMA_IFN_FREE+NMOD_IFN-1
      WRITE(INDICE,'(I2.2)')(JSV - NSV_LIMA_IFN_FREE + 1)
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CLIMA_COLD_NAMES(2))//INDICE)
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CLIMA_COLD_NAMES(2))//INDICE)
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
!
! ELEC
!
  IF (NSV_ELECEND>=NSV_ELECBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_ELECBEG,NSV_ELECEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
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
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_AERBEG,NSV_AEREND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
    !
    IF (LDEPOS_AER(IMI)) THEN
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF             
      END DO      
    END IF  
  END IF
  !
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = 'kg kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  !
  DO JSV = NSV_CHEMBEG,NSV_CHEMEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
    END IF
  END DO
  !
  DO JSV = NSV_CHICBEG,NSV_CHICEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
    END IF
  END DO
  !
  DO JSV = NSV_LNOXBEG,NSV_LNOXEND
    IF(NSIZELBXSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBX_LINOX'
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBX'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
    END IF
    !
    IF(NSIZELBYSV_ll /= 0) THEN
      TZFIELD%CMNHNAME   = 'LBY_LINOX'
      TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CLBTYPE    = 'LBY'
      WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
      CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
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
    IF ((CPROGRAM == 'REAL  ').OR. (CPROGRAM == 'IDEAL ')) THEN
      ! In this case CDUSTNAMES is not allocated. We will use YPDUST_INI,
      !but remember that this variable does not follow JPDUSTORDER
      IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG + 1)/NMODE_DST
      !Should equal 3 at this point
      IF (IMOMENTS >  3) THEN
        WRITE(YMSG,*) 'number of DST moments must be 3',NSV_DSTBEG, NSV_DSTEND,NMODE_DST,IMOMENTS
        CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_LB_n',YMSG)
      END IF ! Test IMOMENTS
      !
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CUNITS     = 'ppp'
      TZFIELD%CDIR       = 'XY'
      TZFIELD%NGRID      = 1
      TZFIELD%NTYPE      = TYPEREAL
      TZFIELD%NDIMS      = 3
      !
      IF (IMOMENTS == 1) THEN
        DO JMODE=1, NMODE_DST
          !Index from which names are picked
          ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + 2
          JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                +  1              & !Number of moments in this mode
                + (NSV_DSTBEG -1)      !Previous list of tracers

          IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction             
            TZFIELD%CMNHNAME   = 'LBX_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBX'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
          ENDIF !Check on border points in X direction
          IF(NSIZELBYSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBY_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBY'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
          ENDIF  !Check on points in Y direction
        ENDDO ! Loop on mode
      ELSE  ! valeur IMOMENTS =/ 1
        DO JMODE=1,NMODE_DST
          DO JMOM=1,IMOMENTS
            ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + JMOM
            JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                + JMOM               & !Number of moments in this mode
                + (NSV_DSTBEG -1)
            !
            IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction
              TZFIELD%CMNHNAME   = 'LBX_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
              TZFIELD%CLBTYPE    = 'LBX'
              WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV
              CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
            ENDIF !Check on border points in X direction
            IF(NSIZELBYSV_ll /= 0) THEN
              TZFIELD%CMNHNAME   = 'LBY_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
              TZFIELD%CLBTYPE    = 'LBY'
              WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV
              CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
            ENDIF  !Check on points in Y direction
          ENDDO ! Loop on moments
        ENDDO    ! Loop on modes
      END IF ! valeur IMOMENTS

    ELSE  ! Test CPROGRAM
      ! We are in the subprogram MESONH, CDUSTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPDUSTORDER)
      !
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CUNITS     = 'kg kg-1'
      TZFIELD%CDIR       = 'XY'
      TZFIELD%NGRID      = 1
      TZFIELD%NTYPE      = TYPEREAL
      TZFIELD%NDIMS      = 3
      !
      DO JSV = NSV_DSTBEG,NSV_DSTEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF
      END DO
      IF (LDEPOS_DST(IMI)) THEN
        DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
          IF(NSIZELBXSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBX'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
          END IF             
          !
          IF(NSIZELBYSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBY'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
          END IF             
        END DO      
      END IF  
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
        CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX)
      END IF
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY)
      END IF
      IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX)
      END IF
      IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) THEN
        CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY)
      END IF
    END IF
    !
    IF ((CPROGRAM == 'REAL  ').OR. (CPROGRAM == 'IDEAL ')) THEN
      ! In this case CSALTNAMES is not allocated. We will use YPSALT_INI,
      !but remember that this variable does not follow JPSALTORDER
      IMOMENTS = INT(NSV_SLTEND - NSV_SLTBEG + 1)/NMODE_SLT
      !Should equal 3 at this point
      IF (IMOMENTS >  3) THEN
        WRITE(YMSG,*) 'number of SLT moments must be 3',NSV_SLTBEG, NSV_SLTEND,NMODE_SLT,IMOMENTS
        CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_LB_n',YMSG)
      END IF ! Test IMOMENTS
      !
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CUNITS     = 'ppp'
      TZFIELD%CDIR       = 'XY'
      TZFIELD%NGRID      = 1
      TZFIELD%NTYPE      = TYPEREAL
      TZFIELD%NDIMS      = 3
      !
      IF (IMOMENTS == 1) THEN
        DO JMODE=1, NMODE_SLT
          !Index from which names are picked
          ISV_NAME_IDX = (JPSALTORDER(JMODE) - 1)*3 + 2
          JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                +  1              & !Number of moments in this mode
                + (NSV_SLTBEG -1)      !Previous list of tracers

          IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction   
            TZFIELD%CMNHNAME   = 'LBX_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBX'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
          ENDIF !Check on border points in X direction
          IF(NSIZELBYSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBY_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBY'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
          ENDIF  !Check on points in Y direction
        ENDDO ! Loop on mode
      ELSE  ! valeur IMOMENTS =/ 1
        DO JMODE=1,NMODE_SLT
          DO JMOM=1,IMOMENTS
            ISV_NAME_IDX = (JPSALTORDER(JMODE) - 1)*3 + JMOM
            JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                + JMOM               & !Number of moments in this mode
                + (NSV_SLTBEG -1)

            IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction
              TZFIELD%CMNHNAME   = 'LBX_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
              TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
              TZFIELD%CLBTYPE    = 'LBX'
              WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
              CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
            ENDIF !Check on border points in X direction
            IF(NSIZELBYSV_ll /= 0) THEN
              TZFIELD%CMNHNAME   = 'LBY_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
              TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
              TZFIELD%CLBTYPE    = 'LBY'
              WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
              CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
            ENDIF  !Check on points in Y direction
          ENDDO ! Loop on moments
        ENDDO    ! Loop on modes
      END IF ! valeur IMOMENTS
    ELSE  ! Test CPROGRAM
      ! We are in the subprogram MESONH, CSALTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPSALTORDER)
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CUNITS     = 'kg kg-1'
      TZFIELD%CDIR       = 'XY'
      TZFIELD%NGRID      = 1
      TZFIELD%NTYPE      = TYPEREAL
      TZFIELD%NDIMS      = 3
      !
      DO JSV = NSV_SLTBEG,NSV_SLTEND
        IF(NSIZELBXSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBX'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
          TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
          TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
          TZFIELD%CLBTYPE    = 'LBY'
          WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
          CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
        END IF            
      END DO
      IF (LDEPOS_SLT(IMI)) THEN
        DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
          IF(NSIZELBXSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBX'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
          END IF             
          !
          IF(NSIZELBYSV_ll /= 0) THEN
            TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
            TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
            TZFIELD%CLBTYPE    = 'LBY'
            WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
            CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
          END IF             
        END DO      
      END IF  
    END IF  
  ENDIF   
  !
  ! lagrangian variables
  IF (NSV_LGEND>=NSV_LGBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'm'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_LGBEG,NSV_LGEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
!
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  ! passive pollutants
  IF (NSV_PPEND>=NSV_PPBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_PPBEG,NSV_PPEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_PP'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_PP'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
  ! conditional sampling
  IF (NSV_CSEND>=NSV_CSBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_CSBEG,NSV_CSEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_CS'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_CS'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
      END IF
    END DO
  END IF
#ifdef MNH_FOREFIRE
  ! ForeFire scalar variables
  IF (NSV_FFEND>=NSV_FFBEG) THEN
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    !
    DO JSV = NSV_FFBEG,NSV_FFEND
      IF(NSIZELBXSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBX_FF'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBX'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'2_Y_Z_','LBXSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBXSV_ll,XLBXSVM(:,:,:,JSV))
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        TZFIELD%CMNHNAME   = 'LBY_FF'
        TZFIELD%CLONGNAME  = 'MesoNH: '//TRIM(TZFIELD%CMNHNAME)
        TZFIELD%CLBTYPE    = 'LBY'
        WRITE(TZFIELD%CCOMMENT,'(A6,A6,I3.3)')'X_2_Z_','LBYSVM',JSV
        CALL IO_WRITE_FIELD_LB(TPFILE,TZFIELD,NSIZELBYSV_ll,XLBYSVM(:,:,:,JSV))
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
