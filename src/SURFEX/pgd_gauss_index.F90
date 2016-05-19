!     #########
      SUBROUTINE PGD_GAUSS_INDEX(HPROGRAM,OZS)
!     #########################################
!!
!!    PURPOSE
!!    -------
!!     
!!    Initialize the gaussien grid mesh index where point (lat,lon) at heigh resolution is located
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     02/2010
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_GAUSS_INDEX
!
USE MODI_GAUSS_INDEX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM
LOGICAL,           INTENT(IN)  :: OZS      ! .true. if orography is imposed by atm. model
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=5)            :: YRES_COMP    ! Resolution in file 
 CHARACTER(LEN=6)            :: YFLAG
INTEGER                     :: ILUOUT       ! output listing logical unit
LOGICAL                     :: LNOPERFORM
!
!*    0.3    Declaration of namelist
!            -----------------------
!
LOGICAL                  :: LINDEX_STORE      ! Store index in a binary file
 CHARACTER(LEN=28)        :: YINDEX_1KM        ! file name for gauss index at 1km
 CHARACTER(LEN=28)        :: YINDEX_10KM       ! file name for gauss index at 10km
 CHARACTER(LEN=28)        :: YINDEX_100KM      ! file name for gauss index at 100km
 CHARACTER(LEN=28)        :: YCOVER            ! file name for cover types
 CHARACTER(LEN=28)        :: YZS               ! file name for orography
 CHARACTER(LEN=28)        :: YSAND             ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY             ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI              ! file name for topographic index
 CHARACTER(LEN=28)        :: YPERM             ! file name for permafrost map
 CHARACTER(LEN=28)        :: YSOC_TOP          ! file name for organic carbon
 CHARACTER(LEN=28)        :: YSOC_SUB          ! file name for organic carbon
!
LOGICAL                  :: LIMP_COVER        ! Imposed values for Cover from another PGD file
LOGICAL                  :: LIMP_ZS           ! Imposed orography from another PGD file
LOGICAL                  :: LIMP_SAND         ! Imposed maps of Sand from another PGD file
LOGICAL                  :: LIMP_CLAY         ! Imposed maps of Clay from another PGD file
LOGICAL                  :: LIMP_CTI          ! Imposed values for topographic index statistics from another PGD file
LOGICAL                  :: LIMP_PERM         ! Imposed values for topographic index statistics from another PGD file
LOGICAL                  :: LIMP_SOC          ! Imposed maps of organic carbon
!
LOGICAL                  :: LUNIF_COVER
LOGICAL                  :: LUNIF_ZS
LOGICAL                  :: LUNIF_SAND
LOGICAL                  :: LUNIF_CLAY
LOGICAL                  :: LUNIF_CTI
LOGICAL                  :: LUNIF_PERM
LOGICAL                  :: LUNIF_SOC
LOGICAL                  :: LSTOP_PGD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------
!*    1.      Initializations
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_GAUSS_INDEX',0,ZHOOK_HANDLE)
YRES_COMP='     '
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!*    2.      Read all namelists
!-------------------------------------------------------------------------------
!
 CALL READ_NAM_PGD_GAUSS_INDEX(HPROGRAM,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,  &
                              YINDEX_100KM,YCOVER,YZS,YCLAY,YSAND,YCTI,      &
                              YPERM,YSOC_TOP,YSOC_SUB,                       &
                              LIMP_COVER,LIMP_ZS,LIMP_CLAY,LIMP_SAND,        &
                              LIMP_CTI,LIMP_PERM,LIMP_SOC,                   &
                              LUNIF_COVER,LUNIF_ZS,LUNIF_SAND,LUNIF_CLAY,    &
                              LUNIF_CTI,LUNIF_PERM,LUNIF_SOC,                &
                              LSTOP_PGD                                      )  
!
!-------------------------------------------------------------------------------
!*    3.      Check consitensy
!-------------------------------------------------------------------------------
!
IF(LIMP_COVER.AND.LIMP_ZS.AND.LIMP_CLAY.AND.LIMP_SAND.AND.LIMP_CTI.AND.LIMP_PERM.AND.LIMP_SOC)THEN
  WRITE(ILUOUT,*) '*****************************************************'
  WRITE(ILUOUT,*)'All pgd fields are imposed from another PGD file      '
  WRITE(ILUOUT,*)'Consequently, Gauss indexes are not calculated or read'
  WRITE(ILUOUT,*) '*****************************************************'
  IF (LHOOK) CALL DR_HOOK('PGD_GAUSS_INDEX',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF(LUNIF_COVER.AND.LUNIF_ZS.AND.LUNIF_CLAY.AND.LUNIF_SAND.AND.LUNIF_CTI.AND.LUNIF_PERM.AND.LUNIF_SOC)THEN
  WRITE(ILUOUT,*) '*****************************************************'
  WRITE(ILUOUT,*)'All pgd fields are prescribed                         '
  WRITE(ILUOUT,*)'Consequently, Gauss indexes are not calculated or read'
  WRITE(ILUOUT,*) '*****************************************************'
  IF (LHOOK) CALL DR_HOOK('PGD_GAUSS_INDEX',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (LEN_TRIM(YCOVER)==0.AND..NOT.LUNIF_COVER) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in COVER fractions preparation                    *'
  WRITE(ILUOUT,*) '* There is no prescribed cover fraction and no input file *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('GAUSS_INDEX: NO PRESCRIBED COVER NOR INPUT FILE')
ELSEIF (LEN_TRIM(YZS)==0.AND..NOT.LUNIF_ZS.AND..NOT.OZS) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in orography preparation                          *'
  WRITE(ILUOUT,*) '* There is no prescribed orography and no input file      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('GAUSS_INDEX: NO PRESCRIBED OROGRAPHY NOR INPUT FILE')
ENDIF
!
!-------------------------------------------------------------------------------
!*    4.      Calculate gauss indexes
!-------------------------------------------------------------------------------
!
!* 4.1 Orography treatment
!  -----------------------
!
LNOPERFORM=(OZS.OR.LUNIF_ZS.OR.LIMP_ZS)
!
IF(LEN_TRIM(YZS)/=0.AND..NOT.LNOPERFORM)THEN
!
  YFLAG='A_OROG'
  CALL GAUSS_INDEX(HPROGRAM,YZS,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
YFLAG='NONE'
!
!* 4.2 Cover treatment
!  -------------------
!
LNOPERFORM=(LUNIF_COVER.OR.LIMP_COVER)
!
IF(LEN_TRIM(YCOVER)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YCOVER,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
!* 4.3 Nature treatment
!  --------------------
!
LNOPERFORM=(LUNIF_CLAY.OR.LIMP_CLAY)
!
IF(LEN_TRIM(YCLAY)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YCLAY,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
LNOPERFORM=(LUNIF_SAND.OR.LIMP_SAND)
!
IF(LEN_TRIM(YSAND)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YSAND,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
LNOPERFORM=(LUNIF_CTI.OR.LIMP_CTI)
!
IF(LEN_TRIM(YCTI)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YCTI,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
LNOPERFORM=(LUNIF_PERM.OR.LIMP_PERM)
!
IF(LEN_TRIM(YPERM)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YPERM,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
LNOPERFORM=(LUNIF_SOC.OR.LIMP_SOC)
!
IF((LEN_TRIM(YSOC_TOP)==0.AND.LEN_TRIM(YSOC_SUB)/=0).OR.(LEN_TRIM(YSOC_TOP)/=0.AND.LEN_TRIM(YSOC_SUB)==0))THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in soil organic carbon preparation                *'
  WRITE(ILUOUT,*) '* If used, sub and top soil input file must be given      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('GAUSS_INDEX: TOP AND SUB SOC INPUT FILE REQUIRED')        
ENDIF
!
IF(LEN_TRIM(YSOC_TOP)/=0.AND.LEN_TRIM(YSOC_SUB)/=0.AND..NOT.LNOPERFORM)THEN
!
  CALL GAUSS_INDEX(HPROGRAM,YSOC_TOP,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
  CALL GAUSS_INDEX(HPROGRAM,YSOC_SUB,YFLAG,LINDEX_STORE,YINDEX_1KM,YINDEX_10KM,YINDEX_100KM,YRES_COMP)
!  
ENDIF
!
!-------------------------------------------------------------------------------
!*    4.      Stop PGD after storage of gauss index if required
!-------------------------------------------------------------------------------
!
IF(LSTOP_PGD)THEN
  WRITE(ILUOUT,*) '**************************************************************'
  WRITE(ILUOUT,*) 'GAUSS_INDEX: Stop PGD after storage of gauss index as required'
  WRITE(ILUOUT,*) '**************************************************************'
  STOP 'GAUSS_INDEX: Stop PGD after storage of gauss index as required'
ENDIF
IF (LHOOK) CALL DR_HOOK('PGD_GAUSS_INDEX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GAUSS_INDEX
