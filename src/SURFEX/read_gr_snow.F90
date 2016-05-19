!     #########
      SUBROUTINE READ_GR_SNOW(HPROGRAM,HSURFTYPE,HPREFIX,     &
                              KLU,KPATCH,TPSNOW,HDIR)  
!     ##########################################################
!
!!****  *READ_GR_SNOW* - routine to read snow surface fields
!!
!!    PURPOSE
!!    -------
!       Initialize snow surface fields.
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
!!      Original       20/01/99
!       F.solmon       06/00 adaptation for patch
!       V.Masson       01/03 new version of ISBA
!       B. Decharme    2008  If no WSNOW, WSNOW = XUNDEF
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_TYPE_SNOW
!
USE MODI_READ_SURF
!
USE MODI_ALLOCATE_GR_SNOW
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)           :: HPROGRAM  ! calling program
 CHARACTER (LEN=*),  INTENT(IN)           :: HSURFTYPE ! generic name used for
                                                      ! snow characteristics
                                                      ! storage in file
 CHARACTER (LEN=3),  INTENT(IN)           :: HPREFIX   ! generic name for patch
!                                                     ! identification                      
INTEGER,            INTENT(IN)           :: KLU       ! horizontal size of snow var.
INTEGER,            INTENT(IN)           :: KPATCH    ! number of tiles
TYPE(SURF_SNOW)                          :: TPSNOW    ! snow characteristics
 CHARACTER (LEN=1),  INTENT(IN), OPTIONAL :: HDIR      ! type of reading
!                                                     ! HDIR = 'A' : entire field on All processors
!                                                     ! HDIR = 'H' : distribution on each processor
!
!*       0.2   declarations of local variables
!
INTEGER             :: IRESP               ! Error code after redding
 CHARACTER(LEN=12)   :: YRECFM              ! Name of the article to be read
 CHARACTER(LEN=16)   :: YRECFM2 
!
 CHARACTER (LEN=100) :: YFMT                ! format for writing
INTEGER             :: ISURFTYPE_LEN       ! 
LOGICAL             :: GSNOW               ! snow written in the file
INTEGER             :: JLAYER              ! loop counter
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZWORK ! 2D array to write data in file
 CHARACTER(LEN=1)    :: YDIR                ! type of reading
 CHARACTER(LEN=4)    :: YNLAYER     !Format depending on the number of layers
INTEGER             :: IVERSION, IBUGFIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW',0,ZHOOK_HANDLE)
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
!-------------------------------------------------------------------------------
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!-------------------------------------------------------------------------------
!
!*       1.    Type of snow scheme
!              -------------------
!
ISURFTYPE_LEN=LEN_TRIM(HSURFTYPE)
IF (IVERSION <=2 .OR. (IVERSION==3 .AND. IBUGFIX<=4)) THEN
  WRITE(YFMT,'(A5,I1,A4)')     '(A5,A',ISURFTYPE_LEN,',A5)'
  WRITE(YRECFM2,YFMT) 'SNOW_',HSURFTYPE,'_TYPE'
ELSE
  IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
    WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A5)'
    WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_TYPE'
  ELSE
    WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A4)'
    WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_TYP'
    YRECFM2=ADJUSTL(HPREFIX//YRECFM2)
  ENDIF
END IF
!
 CALL READ_SURF(HPROGRAM,YRECFM2,TPSNOW%SCHEME,IRESP)
!
!*       2.    Snow levels
!              -----------
!
!
IF (IVERSION <=2 .OR. (IVERSION==3 .AND. IBUGFIX<=4)) THEN
  WRITE(YFMT,'(A5,I1,A4)')     '(A5,A',ISURFTYPE_LEN,',A6)'
  WRITE(YRECFM2,YFMT) 'SNOW_',HSURFTYPE,'_LAYER'
ELSE
  WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A2)'
  WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_N'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2=ADJUSTL(HPREFIX//YRECFM2)
END IF
!
 CALL READ_SURF(HPROGRAM,YRECFM2,TPSNOW%NLAYER,IRESP)
!
!*       2.    Presence of snow fields in the file
!              -----------------------------------
!
IF (IVERSION >6 .OR. (IVERSION==6 .AND. IBUGFIX>=1)) THEN
  WRITE(YFMT,'(A5,I1,A1)')     '(A3,A',ISURFTYPE_LEN,')'
  WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM=ADJUSTL(HPREFIX//YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,GSNOW,IRESP)
ELSE
  IF (TPSNOW%NLAYER==0) THEN
    GSNOW = .FALSE.
    IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='EBA') TPSNOW%NLAYER=1
    IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO'                          ) TPSNOW%NLAYER=3
  ELSE
    GSNOW = .TRUE.
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.    Allocations
!              -----------
!
 CALL ALLOCATE_GR_SNOW(TPSNOW,KLU,KPATCH)
!
IF (.NOT. GSNOW) THEN
  IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
!*       4.    Additional key
!              ---------------
!
IF (IVERSION >= 7 .AND. HSURFTYPE=='VEG') CALL READ_SURF(HPROGRAM,'LSNOW_FRAC_T',LSNOW_FRAC_TOT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       5.    Snow reservoir
!              --------------
!
ALLOCATE(ZWORK(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,3)))
!
DO JLAYER = 1,TPSNOW%NLAYER
!
  YNLAYER='I1.1'
  IF (JLAYER>9) YNLAYER='I2.2'
!   
  IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='3-L' &
     .OR. TPSNOW%SCHEME=='CRO') THEN  
!
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)') '(A6,A',ISURFTYPE_LEN,','//YNLAYER//')'            
      WRITE(YRECFM,YFMT) 'WSNOW_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)') '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'WSN_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%WSNOW(:,JLAYER,:)=ZWORK
  END IF
!
!*       6.    Snow density
!              ------------
!
  IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='3-L' &
     .OR. TPSNOW%SCHEME=='CRO') THEN  
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A6,A',ISURFTYPE_LEN,','//YNLAYER//')'            
      WRITE(YRECFM,YFMT) 'RSNOW_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'RSN_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF    
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%RHO(:,JLAYER,:)=ZWORK
    WHERE(TPSNOW%WSNOW(:,JLAYER,:)==0.0)TPSNOW%RHO(:,JLAYER,:)=XUNDEF
  END IF
!
!*       7.    Snow temperature
!              ----------------
!
  IF (TPSNOW%SCHEME=='1-L') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A6,A',ISURFTYPE_LEN,','//YNLAYER//')'      
      WRITE(YRECFM,YFMT) 'TSNOW_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'TSN_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF      
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%T(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%T(:,JLAYER,:) = XUNDEF
  END IF
!
!*       8.    Heat content
!              ------------
!
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A6,A',ISURFTYPE_LEN,','//YNLAYER//')'      
      WRITE(YRECFM,YFMT) 'HSNOW_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'HSN_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF       
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%HEAT(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%HEAT(:,JLAYER,:) = XUNDEF
  END IF
!
!*       9.    Snow Gran1
!              ------------
!
  IF (TPSNOW%SCHEME=='CRO') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A7,A',ISURFTYPE_LEN,','//YNLAYER//')'      
      WRITE(YRECFM,YFMT) 'SGRAN1_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SG1_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF      
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%GRAN1(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%GRAN1(:,JLAYER,:) = XUNDEF
  END IF
!
!*       10.    Snow Gran2
!              ------------
!
  IF (TPSNOW%SCHEME=='CRO') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A7,A',ISURFTYPE_LEN,','//YNLAYER//')'       
      WRITE(YRECFM,YFMT) 'SGRAN2_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SG2_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF     
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%GRAN2(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%GRAN2(:,JLAYER,:) = XUNDEF
  END IF
!
!*       11.    Historical parameter
!              -------------------
!
  IF (TPSNOW%SCHEME=='CRO') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A6,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SHIST_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SHI_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF    
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%HIST(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%HIST(:,JLAYER,:) = XUNDEF
  END IF
!
!*       12.    Age parameter
!              -------------------
!
  IF (TPSNOW%SCHEME=='CRO') THEN
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A6)')     '(A5,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SAGE_',HSURFTYPE,JLAYER
    ELSE
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SAG_',HSURFTYPE,JLAYER
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
    ENDIF     
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    TPSNOW%AGE(:,JLAYER,:)=ZWORK
    WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%AGE(:,JLAYER,:) = XUNDEF
  END IF
!-------------------------------------------------------------------------------
!
END DO
!
DEALLOCATE(ZWORK)
!-------------------------------------------------------------------------------
!
!*       13.    Albedo
!              ------
!
IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' &
    .OR. TPSNOW%SCHEME=='CRO') THEN  
  IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
    WRITE(YFMT,'(A5,I1,A1)')     '(A6,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'ASNOW_',HSURFTYPE
  ELSE
    WRITE(YFMT,'(A5,I1,A1)')     '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'ASN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
  ENDIF  
  CALL READ_SURF(HPROGRAM,YRECFM,TPSNOW%ALB(:,:),IRESP,HDIR=YDIR)
  WHERE (TPSNOW%WSNOW(:,1,:) == 0.0) TPSNOW%ALB(:,:) = XUNDEF
END IF
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_GR_SNOW
