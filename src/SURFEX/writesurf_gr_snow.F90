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
 CHARACTER(LEN=100)  :: YCOMMENT       ! Comment string
INTEGER             :: IRESP          ! IRESP  : return-code if a problem appears
!
LOGICAL             :: GSNOW          ! T --> snow exists somewhere
!
INTEGER             :: JLAYER         ! loop counter
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
DO JLAYER = 1,TPSNOW%NLAYER
  !
  YNLAYER='I1.1'
  IF (JLAYER>9) YNLAYER='I2.2'
  !
  IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. &
      TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    !*       6.    Snow reservoir
    !              --------------
    !
    WRITE(YFMT,'(A5,I1,A6)') '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'WSN_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)') '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_WSNOW_',HSURFTYPE,JLAYER,' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%WSNOW(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
    !*       7.    Snow density
    !              ------------
    !
    WRITE(YFMT,'(A5,I1,A6)') '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'RSN_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)') '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_RSNOW_',HSURFTYPE,JLAYER,' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%RHO(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  !*       8.    Snow temperature
  !              ----------------
  !
  IF (TPSNOW%SCHEME=='1-L') THEN
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'TSN_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_TSNOW_',HSURFTYPE,JLAYER,' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%T(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  !*       9.    Heat content
  !              ------------
  !
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'HSN_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_HSNOW_',HSURFTYPE,JLAYER,' (kg/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%HEAT(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  IF (TPSNOW%SCHEME=='CRO') THEN
    !
    !*       10.    Snow Gran1
    !              ----------
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'SG1_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A11,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_SGRAN1_',HSURFTYPE,JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%GRAN1(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
    !*       11.    Snow Gran2
    !              ------------
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'SG2_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A11,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_SGRAN2_',HSURFTYPE,JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%GRAN2(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
    !*       12.   Historical parameter
    !              -------------------
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'SHI_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_SHIST_',HSURFTYPE,JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%HIST(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
    !*       13.    Age parameter
    !              ---------------
    !
    WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
    WRITE(YRECFM,YFMT) 'SAG_',HSURFTYPE,JLAYER
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A9)')     '(A9,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
    WRITE(YCOMMENT,YFMT) 'X_Y_SAGE_',HSURFTYPE,JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%AGE(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
END DO
!
!
!*       14.    Albedo
!              ------
!
IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. &
    TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  !
  WRITE(YFMT,'(A5,I1,A1)')     '(A4,A',ISURFTYPE_LEN,')'
  WRITE(YRECFM,YFMT) 'ASN_',HSURFTYPE
  YRECFM=ADJUSTL(HPREFIX//YRECFM)
  WRITE(YFMT,'(A6,I1,A5)')     '(A10,A',ISURFTYPE_LEN,',A10)'
  WRITE(YCOMMENT,YFMT) 'X_Y_ASNOW_',HSURFTYPE,' (no unit)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,TPSNOW%ALB(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITESURF_GR_SNOW
