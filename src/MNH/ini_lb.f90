!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######################
      MODULE MODI_INI_LB
!     ######################
!
INTERFACE 
!
SUBROUTINE INI_LB(HINIFILE,HLUOUT,OLSOURCE,KSV,                             &
     KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,                     &
     KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                         &
     KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,                 &
     HGETTKEM,HGETRVM,HGETRCM,HGETRRM,HGETRIM,HGETRSM,                      &
     HGETRGM,HGETRHM,HGETSVM,                                               &
     PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,                  &
     PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,                  &
     PLBXUMM,PLBXVMM,PLBXWMM,PLBXTHMM,PLBXTKEMM,PLBXRMM,PLBXSVMM,           &
     PLBYUMM,PLBYVMM,PLBYWMM,PLBYTHMM,PLBYTKEMM,PLBYRMM,PLBYSVMM,           &
     PLENG )
!
CHARACTER (LEN=*),       INTENT(IN) :: HINIFILE! name of the initial file       
CHARACTER (LEN=*),       INTENT(IN) :: HLUOUT  ! name for output-listing of nested models      
LOGICAL,                 INTENT(IN) :: OLSOURCE ! switch for the source term 
! Larger Scale fields (source if OLSOURCE=T,  fields at time t-dt if OLSOURCE=F) :
INTEGER,               INTENT(IN)   :: KSV       ! number of passive variables
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN) :: KSIZELBYTKE_ll                ! for TKE
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV
! Get indicators
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKEM,                          &
                                          HGETRVM,HGETRCM,HGETRRM,           &
                                          HGETRIM,HGETRSM,HGETRGM,HGETRHM
CHARACTER (LEN=*), DIMENSION(:),INTENT(IN)  :: HGETSVM
! LB  fields (source if OLSOURCE=T,  fields at time t-dt if OLSOURCE=F) :
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! LB arrays at time t-dt (if OLSOURCE=T) : 
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL  :: PLBXUMM,PLBXVMM,PLBXWMM ! Wind
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBXTHMM              ! Mass
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYUMM,PLBYVMM,PLBYWMM ! Wind
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYTHMM              ! Mass
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBXTKEMM           ! TKE
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYTKEMM
REAL, DIMENSION(:,:,:,:),INTENT(IN), OPTIONAL  :: PLBXRMM  ,PLBXSVMM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),INTENT(IN), OPTIONAL  :: PLBYRMM  ,PLBYSVMM  ! in x and y-dir.
REAL,                  INTENT(IN),   OPTIONAL :: PLENG    ! Interpolation length
!
END SUBROUTINE INI_LB
!
END INTERFACE
!
END MODULE MODI_INI_LB
!     ############################################################
SUBROUTINE INI_LB(HINIFILE,HLUOUT,OLSOURCE,KSV,                    &
     KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,            &
     KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                &
     KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,        &
     HGETTKEM,HGETRVM,HGETRCM,HGETRRM,HGETRIM,HGETRSM,             &
     HGETRGM,HGETRHM,HGETSVM,                                      &
     PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,         &
     PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,         &
     PLBXUMM,PLBXVMM,PLBXWMM,PLBXTHMM,PLBXTKEMM,PLBXRMM,PLBXSVMM,  &
     PLBYUMM,PLBYVMM,PLBYWMM,PLBYTHMM,PLBYTKEMM,PLBYRMM,PLBYSVMM,  &
     PLENG )
!     ############################################################
!
!!****  *INI_LB* - routine to initialize  LB fields
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to read the LB fields and to distribute
! on subdomain which have a non-nul intersection with the LB areas.
!       In case of OLSOURCE=T, it initializes the LB sources instead of the
!   LB fields at time t-dt
!
!!**  METHOD
!!    ------
!!    The LB fields are read in file and distributed by FMREAD_LB
!!
!!    In case of OLSOURCE=T (INI_LB called by INI_CPL or LS_COUPLING), the LB sources
!!   are computed
!!     
!!
!!    EXTERNAL
!!    --------
!!      FMLOOK    : to retrieve logical number 
!!      FMREAD    : to read data in LFIFM file
!!      FMREAD_LB : to read LB data in LFIFM file
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_CONF   : NVERB
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (routine INI_LB)
!!      
!!
!!    AUTHOR
!!    ------
!!  	V. Ducrocq       * Meteo France *
!!      D. Gazen         L.A. 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        22/09/98    FMREAD_LB handle LBs fields
!!      J. Stein        18/09/99    problem with the dry case
!!      D. Gazen        22/01/01    treat NSV_* with floating indices
!!      F Gheusi        29/10/03    bug in LB sources for NSV
!!      J.-P. Pinty     06/05/04    treat NSV_* for C1R3 and ELEC
!!                      20/05/06    Remove KEPS
!!      C.Lac           20/03/08    Add passive pollutants
!!      M.Leriche       16/07/10    Add ice phase chemical species
!!      Pialat/tulet    15/02/12    Add ForeFire scalars 
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!

USE MODD_NSV
USE MODD_CONF
USE MODD_CTURB
USE MODD_DUST
USE MODD_SALT
USE MODD_CH_AEROSOL
!

USE MODE_FM
USE MODE_IO_ll,           ONLY: CLOSE_ll
USE MODE_FMREAD
!
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_CH_M9_n,         ONLY: CNAMES, CICNAMES
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
USE MODD_PARAMETERS,      ONLY: JPHEXT
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!
CHARACTER (LEN=*),       INTENT(IN) :: HINIFILE! name of the initial file       
CHARACTER (LEN=*),       INTENT(IN) :: HLUOUT  ! name for output-listing of nested models      
LOGICAL,                 INTENT(IN) :: OLSOURCE ! switch for the source term 
! Larger Scale fields (source if OLSOURCE=T,  fields at time t-dt if OLSOURCE=F) :
INTEGER,               INTENT(IN)   :: KSV       ! number of passive variables
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                !  for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN) :: KSIZELBYTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV
! Get indicators
CHARACTER (LEN=*),         INTENT(IN)  :: HGETTKEM,                          &
                                          HGETRVM,HGETRCM,HGETRRM,           &
                                          HGETRIM,HGETRSM,HGETRGM,HGETRHM
CHARACTER (LEN=*), DIMENSION(:),INTENT(IN)  :: HGETSVM
! LB  fields (source if OLSOURCE=T,  fields at time t-dt if OLSOURCE=F) :
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKEM          ! 
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! LB arrays at time t-dt (if OLSOURCE=T) : 
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL  :: PLBXUMM,PLBXVMM,PLBXWMM ! Wind
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBXTHMM              ! Mass
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYUMM,PLBYVMM,PLBYWMM ! Wind
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYTHMM              ! Mass
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBXTKEMM           ! TKE
REAL, DIMENSION(:,:,:),INTENT(IN), OPTIONAL  :: PLBYTKEMM
REAL, DIMENSION(:,:,:,:),INTENT(IN), OPTIONAL  :: PLBXRMM  ,PLBXSVMM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),INTENT(IN), OPTIONAL  :: PLBYRMM  ,PLBYSVMM  ! in x and y-dir.
REAL,                  INTENT(IN),   OPTIONAL :: PLENG    ! Interpolation length
!
!
!*       0.2   declarations of local variables
!
INTEGER             :: ILBSIZEX,ILBSIZEY   ! depth  of the LB area in the RIM direction 
                                           !  written in FM file
INTEGER       :: IL3DX,IL3DY ! Size of the LB arrays in FM file
                             ! in  the RIM direction
INTEGER       :: IL3DXU,IL3DYV  ! Size of the LB arrays in FM file
                                ! in  the RIM direction for the normal wind
INTEGER             :: IRIMX,IRIMY ! Total size of the LB area (for the RIM direction)
INTEGER             :: IRIMXU,IRIMYV ! Total size of the LB area (for the RIM direction)
                                     ! for the normal wind (spatial gradient needed)

INTEGER             :: JSV,JRR                    ! Loop index for MOIST AND 
                                                  !  additional scalar variables 
INTEGER             :: IRR                        !  counter for moist variables
INTEGER             :: IGRID,ILENCH,IRESP  !   File 
CHARACTER (LEN=16)  :: YRECFM              ! management
CHARACTER (LEN=100) :: YCOMMENT            ! variables  
CHARACTER (LEN=2)   :: YDIR
CHARACTER (LEN=4)   :: YDIRLB
INTEGER                :: ILUOUT   !  Logical unit number associated with HLUOUT
LOGICAL :: GHORELAX_UVWTH  ! switch for the horizontal relaxation for U,V,W,TH in the FM file 
LOGICAL :: GHORELAX_TKE    ! switch for the horizontal relaxation for tke in the FM file
LOGICAL :: GHORELAX_R, GHORELAX_SV ! switch for the horizontal relaxation 
                                   ! for moist and scalar variables
CHARACTER (LEN= LEN(HGETRVM)), DIMENSION (7) :: YGETRXM ! Arrays with  the get indicators 
                                                        !  for the moist variables
CHARACTER (LEN= 16), DIMENSION (7) :: YRECFMX,YRECFMY ! arrays with the name  of the LB fields
                                                      ! in FM files for the moist variables
INTEGER :: IMASDEV                                                      
!-------------------------------------------------------------------------------
!
!
!*       0.    READ CPL_AROME to know which LB_fileds there are to read
!              --------------------
YRECFM='MASDEV' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IMASDEV > 48) THEN
YRECFM='CPL_AROME' 
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,LCPL_AROME,IGRID,ILENCH,YCOMMENT,IRESP)
ELSE
  LCPL_AROME=.FALSE.
ENDIF
!
!
!*       1.    SOME INITIALIZATIONS
!              --------------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
!
!-------------------------------------------------------------------------------
!
!*       2.    READ 2D "surfacic" LB fields 
!              ----------------------------
!
!*       2.1   read the number of available points for the horizontal relaxation
! for basic variables 
YRECFM = 'RIMX'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ILBSIZEX,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM = 'RIMY'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,ILBSIZEY,IGRID,ILENCH,YCOMMENT,IRESP)
!
!*        2.2 Basic variables
! 
YRECFM = 'HORELAX_UVWTH'
YDIR='--'
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,GHORELAX_UVWTH,IGRID,ILENCH,YCOMMENT,IRESP)
                                !
IF (GHORELAX_UVWTH) THEN 
  IRIMX =(KSIZELBX_ll-2*JPHEXT)/2   
  IRIMXU=(KSIZELBXU_ll-2*JPHEXT)/2  
  IRIMY =(KSIZELBY_ll-2*JPHEXT)/2
  IRIMYV=(KSIZELBYV_ll-2*JPHEXT)/2
  IL3DX=2*ILBSIZEX+2*JPHEXT
  IL3DXU=IL3DX
  IL3DY=2*ILBSIZEY+2*JPHEXT
  IL3DYV=IL3DY
ELSE
  IRIMX=0
  IRIMXU=1
  IRIMY=0
  IRIMYV=1
  IL3DX=2*JPHEXT ! 2
  IL3DY=2*JPHEXT ! 2
  IL3DXU=2 + 2*JPHEXT ! 4 
  IL3DYV=2 + 2*JPHEXT ! 4 
ENDIF
!
IF (KSIZELBXU_ll/= 0) THEN
  YRECFM = 'LBXUM'
  YDIRLB='LBXU'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXUM,IRIMXU,IL3DXU,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF ( KSIZELBY_ll /= 0) THEN
  YRECFM = 'LBYUM'
  YDIRLB='LBY'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYUM,IRIMY,IL3DY,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF ( KSIZELBX_ll /= 0) THEN
  YRECFM = 'LBXVM'
  YDIRLB='LBX'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXVM,IRIMX,IL3DX,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
ENDIF

IF ( KSIZELBYV_ll  /= 0) THEN
  YRECFM = 'LBYVM'
  YDIRLB='LBYV'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYVM,IRIMYV,IL3DYV,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF ( KSIZELBX_ll /= 0) THEN
  YRECFM = 'LBXWM'
  YDIRLB='LBX'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXWM,IRIMX,IL3DX,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF (KSIZELBY_ll /= 0) THEN
  YRECFM = 'LBYWM'
  YDIRLB='LBY'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYWM,IRIMY,IL3DY,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF (KSIZELBX_ll /= 0) THEN
  YRECFM = 'LBXTHM'
  YDIRLB='LBX'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXTHM,IRIMX,IL3DX,IGRID,&
       & ILENCH,YCOMMENT,IRESP)
END IF

IF ( KSIZELBY_ll /= 0) THEN
  YRECFM = 'LBYTHM'
  YDIRLB='LBY'
  CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYTHM,IRIMY,IL3DY,IGRID,&
       & ILENCH,YCOMMENT,IRESP)    
END IF
!
!*        2.3  LB-TKE
!
SELECT CASE(HGETTKEM)                 
CASE('READ') 
  IF (.NOT. LCPL_AROME .AND. OLSOURCE) THEN
    IF (PRESENT(PLBXTKEMM).AND.PRESENT(PLBYTKEMM)) THEN
      WRITE ( ILUOUT,*) 'LBXTKES AND LBYTKES WILL BE INITIALIZED TO 0'
      PLBXTKEM(:,:,:) = PLBXTKEMM(:,:,:)    
      PLBYTKEM(:,:,:) = PLBYTKEMM(:,:,:)
    ELSE
      WRITE ( ILUOUT,*) 'PB TO INITIALIZE LBXTKES AND LBYTKES  '
!callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    ENDIF
  ELSE
    YRECFM = 'HORELAX_TKE'
    YDIR='--'      
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,GHORELAX_TKE,IGRID,ILENCH,YCOMMENT,IRESP)        
    IF (GHORELAX_TKE) THEN 
      IRIMX=(KSIZELBXTKE_ll-2*JPHEXT)/2   
      IRIMY=(KSIZELBYTKE_ll-2*JPHEXT)/2
      IL3DX=2*ILBSIZEX+2*JPHEXT
      IL3DY=2*ILBSIZEY+2*JPHEXT
    ELSE
      IRIMX=0
      IRIMY=0
      IL3DX=2*JPHEXT ! 2
      IL3DY=2*JPHEXT ! 2
    ENDIF
!
    YRECFM='LBXTKEM'
    IF (KSIZELBXTKE_ll /= 0) THEN  
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXTKEM,IRIMX,IL3DX,IGRID,&
           & ILENCH,YCOMMENT,IRESP)
    END IF
!
    YRECFM='LBYTKEM'
    IF (KSIZELBYTKE_ll /= 0) THEN  
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYTKEM,IRIMY,IL3DY,IGRID,&
           & ILENCH,YCOMMENT,IRESP)
    END IF
  ENDIF
CASE('INIT')
  IF (SIZE(PLBXTKEM,1) /= 0) PLBXTKEM(:,:,:) = XTKEMIN
  IF (SIZE(PLBYTKEM,1) /= 0) PLBYTKEM(:,:,:) = XTKEMIN
END SELECT
!
! 
!*        2.5 LB-Rx
!
IF(KSIZELBXR_ll  > 0 ) THEN
  YRECFM = 'HORELAX_R'
  YDIR='--' 
  CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,GHORELAX_R,IGRID,ILENCH,YCOMMENT,IRESP)
  YGETRXM(:)=(/HGETRVM,HGETRCM,HGETRRM,HGETRIM,HGETRSM,HGETRGM,HGETRHM/)
  YRECFMX(:)=(/"LBXRVM","LBXRCM","LBXRRM","LBXRIM","LBXRSM","LBXRGM","LBXRHM"/)
  YRECFMY(:)=(/"LBYRVM","LBYRCM","LBYRRM","LBYRIM","LBYRSM","LBYRGM","LBYRHM"/)
  IF (GHORELAX_R) THEN 
    IRIMX=(KSIZELBXR_ll-2*JPHEXT)/2  
    IRIMY= (KSIZELBYR_ll-2*JPHEXT)/2  
    IL3DX=2*ILBSIZEX+2*JPHEXT
    IL3DY=2*ILBSIZEY+2*JPHEXT
  ELSE
    IRIMX=0
    IRIMY=0
    IL3DX=2*JPHEXT ! 2
    IL3DY=2*JPHEXT ! 2
  END IF
  !                               
  IRR=0
  SELECT CASE(YGETRXM(1))                 
    CASE('READ') 
      IRR=IRR+1 
      IF ( KSIZELBXR_ll  /= 0 ) THEN
        YDIRLB='LBX'
        CALL FMREAD_LB(HINIFILE,YRECFMX(1),HLUOUT,YDIRLB,PLBXRM(:,:,:,IRR),IRIMX,IL3DX,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
      !
      IF ( KSIZELBYR_ll /= 0 ) THEN
        YDIRLB='LBY'
        CALL FMREAD_LB(HINIFILE,YRECFMY(1),HLUOUT,YDIRLB,PLBYRM(:,:,:,IRR),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      END IF
    CASE('INIT')
      IRR=IRR+1 
      IF ( SIZE(PLBXRM,1) /= 0 )  PLBXRM(:,:,:,IRR) = 0.
      IF ( SIZE(PLBYRM,1) /= 0 )  PLBYRM(:,:,:,IRR) = 0.
  END SELECT
    !
    !
  DO JRR=2,7
    SELECT CASE(YGETRXM(JRR))                 
    CASE('READ') 
      IRR=IRR+1 
      IF ( KSIZELBXR_ll  /= 0 ) THEN
        IF (.NOT. LCPL_AROME .AND. OLSOURCE) THEN
            IF (PRESENT(PLBXRMM)) THEN
              PLBXRM(:,:,:,IRR)=PLBXRMM(:,:,:,IRR)
              WRITE(ILUOUT,*) 'PLBXRS  will be initialized to 0 FOR ', YRECFMX(JRR)
            ELSE
              WRITE(ILUOUT,*) 'Pb to initialze PLBXRM  For', YRECFMX(JRR)
              !callabortstop
              CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
              CALL ABORT
              STOP
            ENDIF
        ELSE
          YDIRLB='LBX'
          CALL FMREAD_LB(HINIFILE,YRECFMX(JRR),HLUOUT,YDIRLB,PLBXRM(:,:,:,IRR),IRIMX,IL3DX,&
             & IGRID,ILENCH,YCOMMENT,IRESP)
        ENDIF
      END IF
      !
      IF ( KSIZELBYR_ll /= 0 ) THEN
        IF (.NOT. LCPL_AROME .AND. OLSOURCE) THEN
            IF (PRESENT(PLBYRMM)) THEN
              PLBYRM(:,:,:,IRR)=PLBYRMM(:,:,:,IRR)
              WRITE(ILUOUT,*) 'PLBYRS  will be initialized to 0 For ', YRECFMY(JRR)
            ELSE
              WRITE(ILUOUT,*) 'Pb to initialze PLBYRM For ', YRECFMY(JRR)
              !callabortstop
              CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
              CALL ABORT
              STOP
            ENDIF
         ELSE
           YDIRLB='LBY'
           CALL FMREAD_LB(HINIFILE,YRECFMY(JRR),HLUOUT,YDIRLB,PLBYRM(:,:,:,IRR),IRIMY,IL3DY,&
              & IGRID,ILENCH,YCOMMENT,IRESP)
         ENDIF
       END IF
    CASE('INIT')
      IRR=IRR+1 
      IF ( SIZE(PLBXRM,1) /= 0 )  PLBXRM(:,:,:,IRR) = 0.
      IF ( SIZE(PLBYRM,1) /= 0 )  PLBYRM(:,:,:,IRR) = 0.
    END SELECT
  END DO
END IF
!
!*        2.6    LB-Scalar Variables
!
IF (KSV > 0) THEN 
  IF (ANY(HGETSVM(:)=='READ')) THEN
    YRECFM = 'HORELAX_SV'
    YDIR='--'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,GHORELAX_SV,IGRID,ILENCH,YCOMMENT,IRESP)  
    IF ( GHORELAX_SV ) THEN
      IRIMX=(KSIZELBXSV_ll-2*JPHEXT)/2   
      IRIMY=(KSIZELBYSV_ll-2*JPHEXT)/2
      IL3DX=2*ILBSIZEX+2*JPHEXT
      IL3DY=2*ILBSIZEY+2*JPHEXT
    ELSE
      IRIMX=0
      IRIMY=0
      IL3DX=2*JPHEXT !2
      IL3DY=2*JPHEXT !2
    END IF
  END IF
END IF
! User scalar variables
DO JSV = 1, NSV_USER     
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      WRITE(YRECFM,'(A6,I3.3)')'LBXSVM',JSV
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'PLXYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialze PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      WRITE(YRECFM,'(A6,I3.3)')'LBYSVM',JSV
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialze PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! C2R2 scalar variables 
DO JSV = NSV_C2R2BEG, NSV_C2R2END
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM='LBX_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'C2R2 PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize C2R2 PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM='LBY_'//TRIM(C2R2NAMES(JSV-NSV_C2R2BEG+1))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'C2R2 PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize C2R2 PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! C1R3 scalar variables 
DO JSV = NSV_C1R3BEG, NSV_C1R3END
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM='LBX_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'C1R3 PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize C1R3 PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM='LBY_'//TRIM(C1R3NAMES(JSV-NSV_C1R3BEG+1))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'C1R3 PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize C1R3 PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! ELEC scalar variables 
DO JSV = NSV_ELECBEG, NSV_ELECEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM='LBX_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'ELEC PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize ELEC PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM='LBY_'//TRIM(CELECNAMES(JSV-NSV_ELECBEG+1))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'ELEC PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize ELEC PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Chemical scalar variables
DO JSV = NSV_CHEMBEG, NSV_CHEMEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Chemical PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Chemical PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CNAMES(JSV-NSV_CHEMBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Chemical PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Chemical PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Chemical ice phase scalar variables
DO JSV = NSV_CHICBEG, NSV_CHICEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Ice phase chemical PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize ice phase chemical PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CICNAMES(JSV-NSV_CHICBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Ice phase chemical PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Ice phase chemical PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Orilam aerosol scalar variables
DO JSV = NSV_AERBEG, NSV_AEREND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Aerosol PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Aerosol PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CAERONAMES(JSV-NSV_AERBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Aerosol PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Aerosol PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Orilam aerosols moist scalar variables
DO JSV = NSV_AERDEPBEG, NSV_AERDEPEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CDEAERNAMES(JSV-NSV_AERDEPBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Aerosol PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Aerosol PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CDEAERNAMES(JSV-NSV_AERDEPBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Aerosol PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Aerosol PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Dust scalar variables
DO JSV = NSV_DSTBEG, NSV_DSTEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CDUSTNAMES(JSV-NSV_DSTBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Dust PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize dust PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CDUSTNAMES(JSV-NSV_DSTBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Dust PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize dust PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
DO JSV = NSV_DSTDEPBEG, NSV_DSTDEPEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Dust Desposition PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize dust PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CDEDSTNAMES(JSV-NSV_DSTDEPBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Dust Depoistion  PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize dust PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Sea salt scalar variables
DO JSV = NSV_SLTBEG, NSV_SLTEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(UPCASE(CSALTNAMES(JSV-NSV_SLTBEG+1)))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Sea Salt PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize sea salt PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(UPCASE(CSALTNAMES(JSV-NSV_SLTBEG+1)))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Sea Salt PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize sea salt PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Passive pollutant variables
DO JSV = NSV_PPBEG, NSV_PPEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_PP'
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Passive pollutant PLBXSVM   will be initialized to 0'
          ELSE
            PLBXSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'Passive pollutant PLBXSVM   will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_PP'
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Passive pollutant PLBYSVM   will be initialized to 0'
          ELSE
            PLBYSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'Passive pollutant PLBYSVM   will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
#ifdef MNH_FOREFIRE
! ForeFire scalar variables
DO JSV = NSV_FFBEG, NSV_FFEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_FF'
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
       WRITE(ILUOUT,*) 'ForeFire LBX_FF ', IRESP
       IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'ForeFire pollutant PLBXSVM   will be initialized to 0'
          ELSE
            PLBXSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'ForeFire pollutant PLBXSVM   will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_FF'
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'ForeFire scalar variable PLBYSVM will be initialized to 0'
          ELSE
            PLBYSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'ForeFire scalar variable PLBYSVM will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
#endif
! Conditional sampling variables
DO JSV = NSV_CSBEG, NSV_CSEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_CS'
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Conditional sampling LBXSVM   will be initialized to 0'
          ELSE
            PLBXSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'Conditional sampling PLBXSVM   will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_CS'
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Conditional sampling PLBYSVM   will be initialized to 0'
          ELSE
            PLBYSVM(:,:,:,JSV)=0.
            WRITE(ILUOUT,*) 'Conditional sampling PLBYSVM   will be initialized to 0'
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Linox scalar variables
DO JSV = NSV_LNOXBEG, NSV_LNOXEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_LINOX'
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Linox PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Linox PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_LINOX'
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'Linox PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize Linox PLBYSVM '
!calla bortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
! Lagrangian variables
DO JSV = NSV_LGBEG, NSV_LGEND
  SELECT CASE(HGETSVM(JSV))
  CASE ('READ')
    IF ( KSIZELBXSV_ll /= 0 ) THEN
      YRECFM = 'LBX_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      YDIRLB='LBX'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBXSVM(:,:,:,JSV),IRIMX,IL3DX,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBXSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBXSVMM)) THEN
            PLBXSVM(:,:,:,JSV)=PLBXSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'lagrangian PLBXSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize lagrangian PLBXSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
    IF (KSIZELBYSV_ll  /= 0 ) THEN
      YRECFM = 'LBY_'//TRIM(CLGNAMES(JSV-NSV_LGBEG+1))
      YDIRLB='LBY'
      CALL FMREAD_LB(HINIFILE,YRECFM,HLUOUT,YDIRLB,PLBYSVM(:,:,:,JSV),IRIMY,IL3DY,&
           & IGRID,ILENCH,YCOMMENT,IRESP)
      IF ( SIZE(PLBYSVM,1) /= 0 ) THEN
        IF (IRESP/=0) THEN
          IF (PRESENT(PLBYSVMM)) THEN
            PLBYSVM(:,:,:,JSV)=PLBYSVMM(:,:,:,JSV)
            WRITE(ILUOUT,*) 'lagrangian PLBYSVM   will be initialized to 0'
          ELSE
            WRITE(ILUOUT,*) 'Pb to initialize lagrangian PLBYSVM '
!callabortstop
            CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
            CALL ABORT
            STOP
          ENDIF
        END IF
      END IF
    END IF
!
  CASE('INIT')
    IF ( SIZE(PLBXSVM,1) /= 0 ) PLBXSVM(:,:,:,JSV) = 0.
    IF ( SIZE(PLBYSVM,1) /= 0 ) PLBYSVM(:,:,:,JSV) = 0.
  END SELECT
END DO
!-------------------------------------------------------------------------------
!
!*       3.    COMPUTE THE LB SOURCES
!              -----------------------
!
! IN case of initialization of LB source terms (OLSOURCE=T) :
! xxxM are LB source terms 
! xxxMM are LB fields at time t -dt 
IF (OLSOURCE) THEN 
  IF (PRESENT(PLBXUMM).AND.PRESENT(PLBYUMM)) THEN
    PLBXUM(:,:,:) = (PLBXUM(:,:,:) - PLBXUMM(:,:,:))   / PLENG
    PLBYUM(:,:,:) = (PLBYUM(:,:,:) - PLBYUMM(:,:,:))   / PLENG
  ENDIF
  IF (PRESENT(PLBXVMM).AND.PRESENT(PLBYVMM)) THEN
    PLBXVM(:,:,:) = (PLBXVM(:,:,:) - PLBXVMM(:,:,:))   / PLENG
    PLBYVM(:,:,:) = (PLBYVM(:,:,:) - PLBYVMM(:,:,:))   / PLENG
  ENDIF
  IF (PRESENT(PLBXWMM).AND.PRESENT(PLBYWMM)) THEN 
    PLBXWM(:,:,:) = (PLBXWM(:,:,:) - PLBXWMM(:,:,:))   / PLENG
    PLBYWM(:,:,:) = (PLBYWM(:,:,:) - PLBYWMM(:,:,:))   / PLENG
  ENDIF
   IF (PRESENT(PLBXTHMM).AND.PRESENT(PLBYTHMM)) THEN 
    PLBXTHM(:,:,:) = (PLBXTHM(:,:,:) - PLBXTHMM(:,:,:))   / PLENG
    PLBYTHM(:,:,:) = (PLBYTHM(:,:,:) - PLBYTHMM(:,:,:))   / PLENG
  ENDIF
  IF (HGETTKEM =='READ') THEN
    IF (PRESENT(PLBXTKEMM).AND.PRESENT(PLBYTKEMM)) THEN 
      PLBXTKEM(:,:,:) = (PLBXTKEM(:,:,:) - PLBXTKEMM(:,:,:))   / PLENG
      PLBYTKEM(:,:,:) = (PLBYTKEM(:,:,:) - PLBYTKEMM(:,:,:))   / PLENG
    ENDIF
  ENDIF
  IF (HGETTKEM =='INIT') THEN
      PLBXTKEM(:,:,:) = 0.
      PLBYTKEM(:,:,:) = 0.
  ENDIF
! LB moist variables 
  IRR=0
  IF (PRESENT(PLBXRMM).AND.PRESENT(PLBYRMM))   THEN      
    DO JRR=1,7
      IF (YGETRXM(JRR) == 'READ') THEN      
        IRR=IRR+1  
        PLBXRM(:,:,:,IRR) = (PLBXRM(:,:,:,IRR) - PLBXRMM(:,:,:,IRR))   / PLENG
        PLBYRM(:,:,:,IRR) = (PLBYRM(:,:,:,IRR) - PLBYRMM(:,:,:,IRR))   / PLENG  
      ENDIF
    END DO
  ENDIF
! LB-scalar variables
  DO JSV=1,KSV
    IF (HGETSVM(JSV) == 'READ') THEN   
      PLBXSVM(:,:,:,JSV) = (PLBXSVM(:,:,:,JSV) - PLBXSVMM(:,:,:,JSV))   / PLENG
      PLBYSVM(:,:,:,JSV) = (PLBYSVM(:,:,:,JSV) - PLBYSVMM(:,:,:,JSV))   / PLENG 
    ENDIF
  END DO
! 
ENDIF

CONTAINS
FUNCTION UPCASE(HSTRING)

CHARACTER(LEN=*)            :: HSTRING
CHARACTER(LEN=LEN(HSTRING)) :: UPCASE

INTEGER :: JC
INTEGER, PARAMETER :: IAMIN = IACHAR("a")
INTEGER, PARAMETER :: IAMAJ = IACHAR("A")

DO JC=1,LEN(HSTRING)
  IF (HSTRING(JC:JC) >= "a" .AND. HSTRING(JC:JC) <= "z") THEN
      UPCASE(JC:JC) = ACHAR(IACHAR(HSTRING(JC:JC)) - IAMIN + IAMAJ)
  ELSE
      UPCASE(JC:JC) = HSTRING(JC:JC)
  END IF
END DO

END FUNCTION UPCASE
!
END SUBROUTINE INI_LB
