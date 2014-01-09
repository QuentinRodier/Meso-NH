!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_8 init 2008/06/30 12:13:35
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_WRITE_LB_n
!     ######################
!
INTERFACE
!
SUBROUTINE WRITE_LB_n(HFMFILE)
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
END SUBROUTINE WRITE_LB_n
!
END INTERFACE
!
END MODULE MODI_WRITE_LB_n
!
!
!
!     ##############################
      SUBROUTINE WRITE_LB_n(HFMFILE)
!     ##############################
!
!!****  *WRITE_LFIFM_n* - routine to write LB fields in the LFIFM file
!!
!!    PURPOSE
!!    -------
!        The purpose of this routine is to write LB fields in the
!     HFMFILE//'.lfi' with the FM routines.  
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
!
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll, ONLY: UPCASE, CLOSE_ll
USE MODE_MODELN_HANDLER
!
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_CH_M9_n,         ONLY: CNAMES, CICNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
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


!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
!*       0.2   Declarations of local variables
!
INTEGER           :: ILUOUT         ! logical unit
INTEGER            :: IRESP         ! IRESP  : return-code for fmwrit
INTEGER            :: IGRID         ! IGRID : grid indicator
CHARACTER (LEN=2)  :: YDIR          ! Type of the data field
INTEGER            :: ILENCH        ! ILENCH : length of comment string 
CHARACTER(LEN=16)  :: YRECFM        ! Name of the article to be written
CHARACTER(LEN=100) :: YCOMMENT      ! Comment string
!
INTEGER            :: IRR           ! Index for moist variables
INTEGER            :: JRR,JSV       ! loop index for moist and scalar variables
! 
LOGICAL            :: GHORELAX_R, GHORELAX_SV   ! global hor. relax. informations
INTEGER            :: IRIMX,IRIMY,IRIMXU,IRIMYV ! size of the RIM zone
CHARACTER (LEN=1), DIMENSION (7) :: YC    ! array with the prefix of the moist variables
LOGICAL, DIMENSION (7)           :: GUSER ! array with the use indicator of the moist variables
REAL,    DIMENSION(SIZE(XLBXSVM, 1), SIZE(XLBXSVM,2), SIZE(XLBXSVM,3)) :: ZRHODREFX
REAL,    DIMENSION(SIZE(XLBYSVM, 1), SIZE(XLBYSVM,2), SIZE(XLBYSVM,3)) :: ZRHODREFY
INTEGER  :: JK
!         Integers, counters for dust modes
INTEGER                          :: JMOM, IMOMENTS, JMODE, ISV_NAME_IDX
INTEGER :: IMI ! Current model index
!-------------------------------------------------------------------------------
!
!*       1.    SOME INITIALIZATIONS
!              --------------------
!
IMI = GET_CURRENT_MODEL_INDEX()

CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!
!        2.  WRITE THE DIMENSION OF LB FIELDS
!            --------------------------------
IGRID=1
YDIR='--'
YCOMMENT=''
ILENCH=LEN(YCOMMENT)
!
YRECFM = 'RIMX'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NRIMX,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM = 'RIMY'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,NRIMY,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
!*       3.  BASIC VARIABLES
!            --------------
!
YRECFM = 'HORELAX_UVWTH'
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LHORELAX_UVWTH,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
! compute the size of riming zone
IRIMX =(NSIZELBX_ll-1)/2
IRIMXU=(NSIZELBXU_ll-1)/2
IRIMY =(NSIZELBY_ll-1)/2
IRIMYV=(NSIZELBYV_ll-1)/2
!
!gathering and writing of the LB fields
IF(NSIZELBXU_ll /= 0) THEN 
  YRECFM='LBXUM'
  YCOMMENT='2_Y_Z_LBXUM (M/S)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBXU",XLBXUM,IRIMXU,NSIZELBXU_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBY_ll /= 0) THEN 
  YRECFM='LBYUM'
  YCOMMENT='X_2_Z_LBYUM (M/S)'
  IGRID=2
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYUM,IRIMY,NSIZELBY_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBX_ll /= 0) THEN 
  YRECFM='LBXVM'
  YCOMMENT='2_Y_Z_LBXVM (M/S)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXVM,IRIMX,NSIZELBX_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBYV_ll /= 0) THEN 
  YRECFM='LBYVM'
  YCOMMENT='X_2_Z_LBYVM (M/S)'
  IGRID=3
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBYV",XLBYVM,IRIMYV,NSIZELBYV_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBX_ll /= 0) THEN 
  YRECFM='LBXWM'
  YCOMMENT='2_Y_Z_LBXWM (M/S)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXWM,IRIMX,NSIZELBX_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBY_ll /= 0) THEN 
  YRECFM='LBYWM'
  YCOMMENT='X_2_Z_LBYWM (M/S)'
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYWM,IRIMY,NSIZELBY_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBX_ll /= 0) THEN 
  YRECFM='LBXTHM'
  YCOMMENT='2_Y_Z_LBXTHM (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXTHM,IRIMX,NSIZELBX_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF(NSIZELBY_ll /= 0) THEN 
  YRECFM='LBYTHM'
  YCOMMENT='X_2_Z_LBYTHM (K)'
  IGRID=1
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYTHM,IRIMY,NSIZELBY_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
!
!*        4  LB-TKE
!            ------
!
IF(CTURB/='NONE') THEN
  YRECFM = 'HORELAX_TKE'
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,LHORELAX_TKE,IGRID,ILENCH,YCOMMENT,IRESP)
!
  IRIMX =(NSIZELBXTKE_ll-1)/2
  IRIMY =(NSIZELBYTKE_ll-1)/2

  IF(NSIZELBXTKE_ll /= 0) THEN
    YRECFM='LBXTKEM'
    YCOMMENT='2_Y_Z_LBXTKEM (M**2/S**2)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXTKEM,IRIMX,NSIZELBXTKE_ll,&
       & IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
!
  IF(NSIZELBYTKE_ll /= 0) THEN
    YRECFM='LBYTKEM'
    YCOMMENT='X_2_Z_LBYTKEM (M**2/S**2)'
    IGRID=1
    ILENCH=LEN(YCOMMENT)
    CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYTKEM,IRIMY,NSIZELBYTKE_ll,&
         & IGRID,ILENCH,YCOMMENT,IRESP)
  END IF
END IF
!
!
!*        6  LB-Rx
!            -----
!
IF (NRR >=1) THEN
  IGRID=1
  GHORELAX_R = LHORELAX_RV .OR. LHORELAX_RC .OR. LHORELAX_RR .OR. &
               LHORELAX_RI .OR. LHORELAX_RS .OR. LHORELAX_RG .OR. &
               LHORELAX_RH
  YRECFM = 'HORELAX_R'
  YCOMMENT=' '
  ILENCH=LEN(YCOMMENT)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,GHORELAX_R,IGRID,ILENCH,YCOMMENT,IRESP)
!
  GUSER(:)=(/LUSERV,LUSERC,LUSERR,LUSERI,LUSERS,LUSERG,LUSERH/)
  YC(:)=(/"V","C","R","I","S","G","H"/)
  IRIMX =(NSIZELBXR_ll-1)/2
  IRIMY =(NSIZELBYR_ll-1)/2
  IRR=0
! Loop on moist variables
  DO JRR=1,7
    IF (GUSER(JRR)) THEN
      IRR=IRR+1 
      IF(NSIZELBXR_ll /= 0) THEN
        YRECFM= 'LBXR'//YC(JRR)//'M'
        YCOMMENT= '2_Y_Z_LBXR'//YC(JRR)//'M (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXRM(:,:,:,IRR),IRIMX,NSIZELBXR_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
!
      IF(NSIZELBYR_ll /= 0) THEN
        YRECFM= 'LBYR'//YC(JRR)//'M'
        YCOMMENT= '2_Y_Z_LBYR'//YC(JRR)//'M (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYRM(:,:,:,IRR),IRIMY,NSIZELBYR_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
    END IF
  END DO
END IF
!
!*        7  LB-SV
!            -----
!

IF (NSV >=1) THEN
  YRECFM = 'HORELAX_SV'
  YCOMMENT=' '
  ILENCH=LEN(YCOMMENT)
  GHORELAX_SV=ANY ( LHORELAX_SV ) 
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,GHORELAX_SV,IGRID,ILENCH,YCOMMENT,IRESP)  
  IGRID=1
  IRIMX =(NSIZELBXSV_ll-1)/2
  IRIMY =(NSIZELBYSV_ll-1)/2
  DO JSV = 1,NSV_USER
    IF(NSIZELBXSV_ll /= 0) THEN
      WRITE(YRECFM,'(A6,I3.3)')'LBXSVM',JSV
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      WRITE(YRECFM,'(A6,I3.3)')'LBYSVM',JSV
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  DO JSV = NSV_C2R2BEG,NSV_C2R2END
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM='LBX_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (/M3)'  
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM='LBY_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (/M3)'  
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  DO JSV = NSV_C1R3BEG,NSV_C1R3END
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM='LBX_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (/M3)'  
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM='LBY_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (/M3)'  
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  DO JSV = NSV_ELECBEG,NSV_ELECEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM='LBX_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM='LBY_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
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
    DO JSV = NSV_AERBEG,NSV_AEREND
      IF(NSIZELBXSV_ll /= 0) THEN
        YRECFM = 'LBX_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
      !
      IF(NSIZELBYSV_ll /= 0) THEN
        YRECFM = 'LBY_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
    END DO
      IF (LDEPOS_AER(IMI)) THEN
      DO JSV = NSV_AERDEPBEG,NSV_AERDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
           YRECFM = 'LBX_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
           YRECFM = 'LBY_'//TRIM(CDEAERNAMES(JSV-NSV_AERDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
      END DO      
      END IF  

  END IF
  !
  DO JSV = NSV_CHEMBEG,NSV_CHEMEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  !
  DO JSV = NSV_CHICBEG,NSV_CHICEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  !
  DO JSV = NSV_LNOXBEG,NSV_LNOXEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_LINOX'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_LINOX'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
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
    IF ((CPROGRAM == 'REAL  ').OR.&
        (CPROGRAM == 'IDEAL ')) THEN
      ! In this case CDUSTNAMES is not allocated. We will use YPDUST_INI,
      !but remember that this variable does not follow JPDUSTORDER
      IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG + 1)/NMODE_DST
      !Should equal 3 at this point
      IF (IMOMENTS >  3) THEN
        WRITE(ILUOUT,*) 'Error in write_lbn: number of moments DST must be 3'
        WRITE(ILUOUT,*) NSV_DSTBEG, NSV_DSTEND,NMODE_DST,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF ! Test IMOMENTS
      
 IF (IMOMENTS == 1) THEN
      DO JMODE=1, NMODE_DST
      !Index from which names are picked
      ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + 2
           JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                +  1              & !Number of moments in this mode
                + (NSV_DSTBEG -1)      !Previous list of tracers

            IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction
             
              YRECFM = 'LBX_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),&
                             IRIMX,NSIZELBXSV_ll, IGRID,ILENCH,YCOMMENT,IRESP)
           ENDIF !Check on border points in X direction
           IF(NSIZELBYSV_ll /= 0) THEN
              YRECFM = 'LBY_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),&
                            IRIMY,NSIZELBYSV_ll, IGRID,ILENCH,YCOMMENT,IRESP)

           ENDIF  !Check on points in Y direction
      ENDDO ! Loop on mode

 ELSE  ! valeur IMOMENTS =/ 1
   DO JMODE=1,NMODE_DST
      DO JMOM=1,IMOMENTS
    ISV_NAME_IDX = (JPDUSTORDER(JMODE) - 1)*3 + JMOM
           JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                + JMOM               & !Number of moments in this mode
                + (NSV_DSTBEG -1)

            IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction
              YRECFM = 'LBX_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),&
                             IRIMX,NSIZELBXSV_ll, IGRID,ILENCH,YCOMMENT,IRESP)
           ENDIF !Check on border points in X direction
           IF(NSIZELBYSV_ll /= 0) THEN
              YRECFM = 'LBY_'//TRIM(YPDUST_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),&
                             IRIMY,NSIZELBYSV_ll, IGRID,ILENCH,YCOMMENT,IRESP)

           ENDIF  !Check on points in Y direction
         ENDDO ! Loop on moments
      ENDDO    ! Loop on modes
END IF ! valeur IMOMENTS

    ELSE  ! Test CPROGRAM
      ! We are in the subprogram MESONH, CDUSTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPDUSTORDER)
      DO JSV = NSV_DSTBEG,NSV_DSTEND
        IF(NSIZELBXSV_ll /= 0) THEN
           YRECFM = 'LBX_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
           YRECFM = 'LBY_'//TRIM(CDUSTNAMES(JSV-NSV_DSTBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
         END IF            
      END DO
      IF (LDEPOS_DST(IMI)) THEN
      DO JSV = NSV_DSTDEPBEG,NSV_DSTDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
           YRECFM = 'LBX_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
           YRECFM = 'LBY_'//TRIM(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
      END DO      
    END IF  


    END IF  
  ENDIF   
  !
  IF (LSALT) THEN
    DO JK=1,size(XLBXSVM,3)
      ZRHODREFX(:,:,JK) =  XRHODREFZ(JK)
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
    IF (LSLTINIT) THEN ! GRIBEX CASE (aerosols initialization)
    IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_SLT > 1)) &
      CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX)
    IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'REAL ').AND.(NSV_SLT > 1)) &
      CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY)
    IF ((NSIZELBXSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) &
      CALL SALTLFI_n(XLBXSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFX)
    IF ((NSIZELBYSV_ll /= 0).AND.(CPROGRAM == 'IDEAL ').AND.(NSV_SLT > 1)) &
      CALL SALTLFI_n(XLBYSVM(:,:,:,NSV_SLTBEG:NSV_SLTEND), ZRHODREFY)
    END IF
    !
    IF ((CPROGRAM == 'REAL  ').OR.&
        (CPROGRAM == 'IDEAL ')) THEN
      ! In this case CSALTNAMES is not allocated. We will use YPSALT_INI,
      !but remember that this variable does not follow JPSALTORDER
      IMOMENTS = INT(NSV_SLTEND - NSV_SLTBEG + 1)/NMODE_SLT
      !Should equal 3 at this point
      IF (IMOMENTS .NE. 3) THEN
        WRITE(ILUOUT,*) 'Error in write_lbn: number of moments SLT must be 3'
        WRITE(ILUOUT,*) NSV_SLTBEG, NSV_SLTEND,NMODE_SLT,IMOMENTS
 !callabortstop
        CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
        CALL ABORT
        STOP
      END IF
      DO JMODE=1, NMODE_SLT
        DO JMOM = 1, IMOMENTS
           !Index from which names are picked
           ISV_NAME_IDX = (JPSALTORDER(JMODE)-1)*IMOMENTS + JMOM  
           !Index which counts in the XLBXSV
           JSV = (JMODE-1)*IMOMENTS  & !Number of moments previously counted
                + JMOM               & !Number of moments in this mode
                + (NSV_SLTBEG -1)      !Previous list of tracers  
           IF(NSIZELBXSV_ll /= 0) THEN !Check on border points in X direction
              YRECFM = 'LBX_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
                   & IGRID,ILENCH,YCOMMENT,IRESP)
           ENDIF !Check on border points in X direction
           IF(NSIZELBYSV_ll /= 0) THEN
              YRECFM = 'LBY_'//TRIM(YPSALT_INI(ISV_NAME_IDX))
              WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (ppp)'
              ILENCH=LEN(YCOMMENT)
              CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
                   & IGRID,ILENCH,YCOMMENT,IRESP)
        
           ENDIF  !Check on points in Y direction
         ENDDO ! Loop on moments
      ENDDO    ! Loop on modes
      !    
    ELSE  
      ! We are in the subprogram MESONH, CSALTNAMES are allocated and are 
      !in the same order as the variables in XSVM (i.e. following JPSALTORDER)
      DO JSV = NSV_SLTBEG,NSV_SLTEND
        IF(NSIZELBXSV_ll /= 0) THEN
           YRECFM = 'LBX_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF
             !
        IF(NSIZELBYSV_ll /= 0) THEN
           YRECFM = 'LBY_'//TRIM(CSALTNAMES(JSV-NSV_SLTBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
      END DO
      IF (LDEPOS_SLT(IMI)) THEN
      DO JSV = NSV_SLTDEPBEG,NSV_SLTDEPEND
        IF(NSIZELBXSV_ll /= 0) THEN
           YRECFM = 'LBX_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
        !
        IF(NSIZELBYSV_ll /= 0) THEN
           YRECFM = 'LBY_'//TRIM(CDESLTNAMES(JSV-NSV_SLTDEPBEG+1))
        WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
        ILENCH=LEN(YCOMMENT)
        CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        END IF             
      END DO      
      END IF  

    END IF  
  ENDIF   

  ! lagrangian variables
  DO JSV = NSV_LGBEG,NSV_LGEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (M)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (M)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
  ! passive pollutants  
  DO JSV = NSV_PPBEG,NSV_PPEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_PP'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_PP'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO

  ! conditional sampling
  DO JSV = NSV_CSBEG,NSV_CSEND
    IF(NSIZELBXSV_ll /= 0) THEN
      YRECFM = 'LBX_CS'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'2_Y_Z_','LBXSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBX",XLBXSVM(:,:,:,JSV),IRIMX,NSIZELBXSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
!
    IF(NSIZELBYSV_ll /= 0) THEN
      YRECFM = 'LBY_CS'
      WRITE(YCOMMENT,'(A6,A6,I3.3,A8)')'X_2_Z_','LBYSVM',JSV,' (KG/KG)'
      ILENCH=LEN(YCOMMENT)
      CALL FMWRIT_LB(HFMFILE,YRECFM,CLUOUT,"LBY",XLBYSVM(:,:,:,JSV),IRIMY,NSIZELBYSV_ll,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
    END IF
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_LB_n  

