!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_GR_SNOW(HPROGRAM,HSURFTYPE,HPREFIX,TPSNOW  )
!     ##########################################################
!
!!****  *WRITESURF_GR_SNOW* - routine to write snow surface fields
!!
!!    PURPOSE
!!    -------
!       Writes snow surface fields
!
!!**  METHOD
!!    ------
!!    
!!    
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
!!
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      02/2003
!!     A. Bogatchev 09/2005 EBA snow option
!!     M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_SNOW
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT
!
USE MODI_DETECT_FIELD
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
USE MODI_WRITE_SURF_FIELD3D
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
 CHARACTER (LEN=6),  INTENT(IN) :: HPROGRAM   ! program
 CHARACTER (LEN=*),  INTENT(IN) :: HSURFTYPE  ! generic name used for
                                             ! snow characteristics
                                             ! storage in file
 CHARACTER (LEN=3),  INTENT(IN) :: HPREFIX    ! generic name of prefix for
                                             ! patch identification
TYPE(SURF_SNOW),    INTENT(IN) :: TPSNOW     ! snow characteristics
!
!*       0.2   declarations of local variables
!
INTEGER             :: ISURFTYPE_LEN
!
 CHARACTER (LEN=100) :: YFMT           ! format for writing
 CHARACTER(LEN=12)   :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT         ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT     ! Comment string : unit of the datas in the field to write
INTEGER             :: IRESP          ! IRESP  : return-code if a problem appears
!
LOGICAL             :: GSNOW          ! T --> snow exists somewhere
!
INTEGER             :: JLAYER         ! loop counter
CHARACTER(LEN=4)    :: YPATCH              ! number of the patch
 CHARACTER(LEN=4)    :: YNLAYER        ! String depending on the number of layer : less
                                      !than 10 or more                              
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',0,ZHOOK_HANDLE)
!
!*       1.    Initialisation
!              --------------

ISURFTYPE_LEN = LEN_TRIM(HSURFTYPE)
!
!
!*       2.    Type of snow scheme
!              -------------------
!
WRITE(YFMT,'(A5,I1,A4)') '(A3,A',ISURFTYPE_LEN,',A4)'
WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE,'_TYP'
YRECFM=ADJUSTL(HPREFIX//YRECFM)
YCOMMENT=' '
 CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%SCHEME,IRESP,HCOMMENT=YCOMMENT)
!
!
!*       3.    Number of layers
!              ----------------
!
WRITE(YFMT,'(A5,I1,A4)') '(A3,A',ISURFTYPE_LEN,',A2)'
WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE,'_N'
YRECFM=ADJUSTL(HPREFIX//YRECFM)
YCOMMENT    = '(INTEGER)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%NLAYER,IRESP,HCOMMENT=YCOMMENT)
!
!
!*       4.    Tests to find if there is snow
!              ------------------------------
!
IF (TPSNOW%NLAYER>0) THEN
  CALL DETECT_FIELD(HPROGRAM,TPSNOW%WSNOW(:,1,:),GSNOW)
ELSE
  GSNOW = .FALSE.
END IF
!
WRITE(YFMT,'(A5,I1,A1)') '(A3,A',ISURFTYPE_LEN,')'
WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE
YRECFM=ADJUSTL(HPREFIX//YRECFM)
YCOMMENT    = '(LOGICAL)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,GSNOW,IRESP,HCOMMENT=YCOMMENT)
!
!
IF (.NOT. GSNOW) THEN
  IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!
!*       5.    Additional key
!              ---------------
!
YCOMMENT    = '(LOGICAL)'
 CALL WRITE_SURF(HPROGRAM,'LSNOW_FRAC_T',LSNOW_FRAC_TOT,IRESP,HCOMMENT=YCOMMENT)
!
!
IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. &
    TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  !
  !
  !*       6.    Snow reservoir
  !              --------------
  !
  YRECFM=ADJUSTL(HPREFIX//'WSN_'//HSURFTYPE)
  YCOMMENT='X_Y_WSNOW_'//HSURFTYPE
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%WSNOW,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  !*       7.    Snow density
  !              ------------
  !
  YRECFM=ADJUSTL(HPREFIX//'RSN_'//HSURFTYPE)
  YCOMMENT='X_Y_RSNOW_'//HSURFTYPE
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%RHO,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
!*       8.    Snow temperature
!              ----------------
!
IF (TPSNOW%SCHEME=='1-L') THEN
  !
  YRECFM=ADJUSTL(HPREFIX//'TSN_'//HSURFTYPE)
  YCOMMENT='X_Y_TSNOW_'//HSURFTYPE
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%T,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
!*       9.    Heat content
!              ------------
!
IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  !
  YRECFM=ADJUSTL(HPREFIX//'HSN_'//HSURFTYPE)
  YCOMMENT='X_Y_HSNOW_'//HSURFTYPE
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%HEAT,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
IF (TPSNOW%SCHEME=='CRO') THEN
  !
  !
  !*       10.    Snow Gran1
  !              ----------
  !
  YRECFM=ADJUSTL(HPREFIX//'SG1_'//HSURFTYPE)
  YCOMMENT='X_Y_SGRAN1_'//HSURFTYPE
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%GRAN1,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  !*       11.    Snow Gran2
  !              ------------
  !
  YRECFM=ADJUSTL(HPREFIX//'SG2_'//HSURFTYPE)
  YCOMMENT='X_Y_SGRAN2_'//HSURFTYPE
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%GRAN2,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  !*       12.   Historical parameter
  !              -------------------
  !
  YRECFM=ADJUSTL(HPREFIX//'SHI_'//HSURFTYPE)
  YCOMMENT='X_Y_SHIST_'//HSURFTYPE
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%HIST,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  !*       13.    Age parameter
  !              ---------------
  !
  YRECFM=ADJUSTL(HPREFIX//'SAG_'//HSURFTYPE)
  YCOMMENT='X_Y_SAGE_'//HSURFTYPE
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,TPSNOW%AGE,1,TPSNOW%NLAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
!
!*       14.    Albedo
!              ------
!
IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. &
    TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  !
  YRECFM=ADJUSTL(HPREFIX//'ASN_'//HSURFTYPE)
  YCOMMENT='X_Y_ASNOW_'//HSURFTYPE
  YCOMMENTUNIT='no unit'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,TPSNOW%ALB,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITESURF_GR_SNOW
