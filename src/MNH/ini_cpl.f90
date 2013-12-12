!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 lbc 2006/05/23 09:12:02
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_INI_CPL
!     ###################
!
INTERFACE
!
      SUBROUTINE INI_CPL(HLUOUT,KSTOP,PTSTEP,OSTEADY_DMASS,HCONF,            &
            HGETTKEM,                                                        &
            HGETRVM,HGETRCM,HGETRRM,HGETRIM,                                 &
            HGETRSM,HGETRGM,HGETRHM,HGETSVM,OCH_INIT_FIELD,                  &
            KSV,KIMAX_ll,KJMAX_ll,                                           &
            KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,               &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,PDRYMASST,                       &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            PLSUS,PLSVS,PLSWS,PLSTHS,PLSRVS,PDRYMASSS,                       &
            PLBXUS,PLBXVS,PLBXWS,PLBXTHS,PLBXTKES,PLBXRS,PLBXSVS,            &
            PLBYUS,PLBYVS,PLBYWS,PLBYTHS,PLBYTKES,PLBYRS,PLBYSVS             )
!
CHARACTER(LEN=*), INTENT(IN)       :: HLUOUT      ! Name of the output-listing
INTEGER,          INTENT(IN)       :: KSTOP       ! Number of time steps for
                                                  ! current segment
REAL,             INTENT(IN)       :: PTSTEP      ! Time step
LOGICAL,          INTENT(IN)       :: OSTEADY_DMASS ! Md evolution logical switch
CHARACTER(LEN=*), INTENT(IN) :: HCONF  ! configuration var. linked to FMfile
!
                                                  ! Get indicators at t-dt for
CHARACTER(LEN=*), INTENT(IN) :: HGETTKEM          !  tke, eps
CHARACTER(LEN=*), INTENT(IN) :: HGETRVM,HGETRCM,HGETRRM   !  vapor, cloud, rain
CHARACTER(LEN=*), INTENT(IN) :: HGETRIM,HGETRSM   !  ice, snow
CHARACTER(LEN=*), INTENT(IN) :: HGETRGM,HGETRHM   !  graupel, hail
CHARACTER(LEN=*),                                 &
    DIMENSION(:), INTENT(IN) :: HGETSVM           !  scalar variables
LOGICAL,          INTENT(IN) :: OCH_INIT_FIELD                                
INTEGER,                INTENT(IN)        :: KSV  ! number of scalar var.
INTEGER,               INTENT(IN)   :: KIMAX_ll  !  Dimensions  in x direction 
                                                 ! of the physical domain,
INTEGER,               INTENT(IN)   :: KJMAX_ll  !  Dimensions  in y direction 
                                                 ! of the physical domain,
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN):: KSIZELBYTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV 
!
REAL, DIMENSION(:,:,:),  INTENT(IN)  :: PLSUM,PLSVM,PLSWM ! Large Scale 
REAL, DIMENSION(:,:,:),  INTENT(IN)  :: PLSTHM, PLSRVM    ! fields at t-dt
REAL,                    INTENT(IN)  :: PDRYMASST         ! Mass of dry air Md
! larger scale fields for Lateral Boundary condition
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
!
REAL, DIMENSION(:,:,:),  INTENT(OUT) :: PLSUS,PLSVS,PLSWS  ! Large Scale 
REAL, DIMENSION(:,:,:),  INTENT(OUT) :: PLSTHS,PLSRVS      ! source terms
REAL,                    INTENT(OUT) :: PDRYMASSS          !  Md source 
! larger scale fields sources for Lateral Boundary condition
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUS,PLBXVS,PLBXWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUS,PLBYVS,PLBYWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKES          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKES
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRS  ,PLBXSVS  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRS  ,PLBYSVS  ! in x and y-dir.
!
END SUBROUTINE INI_CPL
!
END INTERFACE
!
END MODULE MODI_INI_CPL
!
!
!     #####################################################################
      SUBROUTINE INI_CPL(HLUOUT,KSTOP,PTSTEP,OSTEADY_DMASS,HCONF,            &
            HGETTKEM,                                                        &
            HGETRVM,HGETRCM,HGETRRM,HGETRIM,                                 &
            HGETRSM,HGETRGM,HGETRHM,HGETSVM,OCH_INIT_FIELD,                  &
            KSV,KIMAX_ll,KJMAX_ll,                                           &
             KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,              &
            KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                   &
            KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,           &   
            PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,PDRYMASST,                       &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,            &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,            &
            PLSUS,PLSVS,PLSWS,PLSTHS,PLSRVS,PDRYMASSS,                       &
            PLBXUS,PLBXVS,PLBXWS,PLBXTHS,PLBXTKES,PLBXRS,PLBXSVS,            &
            PLBYUS,PLBYVS,PLBYWS,PLBYTHS,PLBYTKES,PLBYRS,PLBYSVS             )
!     #####################################################################
!
!!****  *INI_CPL * - routine to initialize variables for the coupling
!!                   of the model 1 
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize specific variables for the
!     coupling of the model 1, from current time and large scale fields read in
!     the coupling files.
!      
!!
!!**  METHOD
!!    ------
!!      First, all the coupling files are opened and current times are read to
!!    initialize the coupling time array. 
!!      Then, several tests are performed :
!!      - since the calendar is not taken into account yet, dates of the coupling
!!    times and the current date (which corresponds of this segment beginning 
!!    date) must be the same, otherwise the initialization is stopped;
!!      - if the coupling files are not specified in a chronological order, the
!!    initialization is stopped;
!!      - the coupling files, for which current time occurs before the current
!!    time (which corresponds of this segment beginning time), are released;
!!      - the number of the current coupling file is initialized as the first
!!    one following in time, the initial file;
!!      - the actual number of coupling files are reinitialized and the coupling
!!    times are converted in number of time step;
!!      - if the segment end is greater than the last coupling time, the 
!!    initialization is stopped;
!!      - finally, names of the useful coupling files are written in the 
!!    output-listing.
!!      Secondly, after checking grid dimensions, large scale fields are read in
!!    the first coupling file and source terms are computed. Note that this 
!!    computation occurs also in subroutine SET_COUPLING.
!!      The same treatment is also performed to couple the DRYMASS.
!!         
!!
!!    EXTERNAL
!!    --------
!!      FMLOOK      : to retrieve a logical unit number 
!!      FMREAD      : to read data in LFI_FM file
!!      FMCLOS      : to close a FM-file
!!      INI_LS      : to initialize larger scale fields
!!      INI_LB      : to initialize "2D" surfacic LB fields 
!!      TEMPORAL_DIST : compute the temporal distance in secunds between 2 dates
!!
!!      Module MODE_TIME : contains SM_PRINT_TIME routine
!!                         and uses module MODD_TIME (for definition
!!                         of types DATE_TIME and DATE)
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS 
!!         JPHEXT : Horizontal external points number
!!         JPVEXT : Vertical external points number
!!         JPCPLFILEMAX : Maximum allowed number of coupling files
!!
!!      Module MODD_CONF   
!!         NVERB      : Level of informations on output-listing
!!
!!      Module MODD_CTURB :
!!         XTKEMIN : mimimum value for the TKE
!!
!!      Module MODD_DYN   
!!         XSEGLEN     : Duration of segment (in seconds)
!!
!!         NCPL_NBR    : NumBeR of CouPLing files
!!         NCPL_CUR    : Number of the CURrent CouPLing file              
!!         NCPL_TIMES  : List of the values of the time index in the temporal
!!                      model loop where large scale sources are computed
!!
!!      Module MODD_NESTING : NDTRATIO  
!!
!!      Module MODD_TIME  
!!         TDTCPL       : Time and Date of the CouPLing files
!!
!!      Module MODD_TIME1 
!!         TDTCUR      : CURrent Time and Date (in the initial file)
!!
!!      Module MODD_LUNIT1 
!!         CCPLFILE    :  Names of the CouPLing FILEs 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine INI_CPL)
!!      
!!
!!    AUTHOR
!!    ------
!!	I.Mallet       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                10/03/95 
!!      Modifications  (Mallet)  14/09/95  to take into account different days  
!!      Modifications  (Lafore)  20/09/95  Introduction of the DRYMASS coupling
!!                     (Stein)   02/01/96  add the calendar 
!!                     (Lafore)  11/03/97  "surfacic" LS fieds  introduction
!!      Modification   (Lafore)  10/04/97   proper treatment of minima for LS-fields
!!      Modification   (Stein)   22/12/97  new LS fields  
!!      Modification   (Josse)   20/04/98  temporal evolution of SST
!!      Modification   (Ducrocq)  22/09/98  //,  and introduce INI_LS and INI_LB
!!      Modification   (Masson)  01/2004 surface externalization, removes 
!!                                       SST forcing
!!      Modification             05/2006  Remove KEPS
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_PARAMETERS 
USE MODD_CONF
USE MODD_CTURB
USE MODD_DYN
USE MODD_TIME_n                
USE MODD_LUNIT_n
USE MODD_NSV
USE MODD_CH_MNHC_n
!
USE MODE_TIME
USE MODD_NESTING
USE MODE_FM
USE MODE_IO_ll
!
USE MODI_TEMPORAL_DIST
USE MODE_FMREAD
USE MODI_INI_LS
USE MODI_INI_LB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments 
!
!
!
CHARACTER(LEN=*), INTENT(IN)       :: HLUOUT      ! Name of the output-listing
INTEGER,          INTENT(IN)       :: KSTOP       ! Number of time steps for
                                                  ! current segment
REAL,             INTENT(IN)       :: PTSTEP      ! Time step
LOGICAL,          INTENT(IN)       :: OSTEADY_DMASS ! Md evolution logical switch
CHARACTER(LEN=*), INTENT(IN) :: HCONF  ! configuration var. linked to FMfile
!
                                                  ! Get indicators at t-dt for
CHARACTER(LEN=*), INTENT(IN) :: HGETTKEM          !  tke, eps
CHARACTER(LEN=*), INTENT(IN) :: HGETRVM,HGETRCM,HGETRRM   !  vapor, cloud, rain
CHARACTER(LEN=*), INTENT(IN) :: HGETRIM,HGETRSM   !  ice, snow
CHARACTER(LEN=*), INTENT(IN) :: HGETRGM,HGETRHM   !  graupel, hail
CHARACTER(LEN=*),                                 &
    DIMENSION(:), INTENT(IN) :: HGETSVM           !  scalar variables
LOGICAL,          INTENT(IN) :: OCH_INIT_FIELD                                
INTEGER,                INTENT(IN)        :: KSV  ! number of scalar var.
INTEGER,               INTENT(IN)   :: KIMAX_ll  !  Dimensions  in x direction 
                                                 ! of the physical domain,
INTEGER,               INTENT(IN)   :: KJMAX_ll  !  Dimensions  in y direction 
                                                 ! of the physical domain,
! sizes of the West-east total LB area
INTEGER, INTENT(IN) :: KSIZELBX_ll,KSIZELBXU_ll      ! for T,V,W and u 
INTEGER, INTENT(IN) :: KSIZELBXTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBXR_ll,KSIZELBXSV_ll    ! for Rx and SV    
! sizes of the North-south total LB area
INTEGER, INTENT(IN) :: KSIZELBY_ll,KSIZELBYV_ll      ! for T,U,W  and v
INTEGER, INTENT(IN):: KSIZELBYTKE_ll                ! for TKE 
INTEGER, INTENT(IN) :: KSIZELBYR_ll,KSIZELBYSV_ll    ! for Rx and SV 
!
REAL, DIMENSION(:,:,:),  INTENT(IN)  :: PLSUM,PLSVM,PLSWM ! Large Scale 
REAL, DIMENSION(:,:,:),  INTENT(IN)  :: PLSTHM, PLSRVM    ! fields at t-dt
REAL,                    INTENT(IN)  :: PDRYMASST         ! Mass of dry air Md
! larger scale fields for Lateral Boundary condition
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
!
REAL, DIMENSION(:,:,:),  INTENT(OUT) :: PLSUS,PLSVS,PLSWS  ! Large Scale 
REAL, DIMENSION(:,:,:),  INTENT(OUT) :: PLSTHS,PLSRVS      ! source terms
REAL,                    INTENT(OUT) :: PDRYMASSS          !  Md source 
! larger scale fields sources for Lateral Boundary condition
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXUS,PLBXVS,PLBXWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYUS,PLBYVS,PLBYWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBXTKES          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(OUT) :: PLBYTKES
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBXRS  ,PLBXSVS  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(OUT) :: PLBYRS  ,PLBYSVS  ! in x and y-dir.
!
!*       0.2   declarations of local variables
!
INTEGER                :: ILUOUT                     !  Logical unit number
                                                     ! associated with HLUOUT 
INTEGER                :: IGRID,ILENCH,IRESP,ININAR      !  File 
CHARACTER (LEN=16)     :: YRECFM                               ! management
CHARACTER (LEN=100)    :: YCOMMENT                             ! variables 
CHARACTER(LEN=2)       :: YDIR 
CHARACTER (LEN=40)     :: YTITLE                     !  Title for date print 
INTEGER                :: JCI                        !  Loop index on number of
                                                     ! coupling files
CHARACTER (LEN=2)      :: YCI                        !  String for coupling files
                                                     ! index
REAL                   :: ZLENG                      ! Interpolation length
LOGICAL                :: GEND                    !  Logical to see if coupling
                                                  ! times respect the chronolo.
                                                  ! order or the segment length
INTEGER                :: IIMAX,IJMAX,IKMAX       !  Dimensions  of the physical 
                                                  ! part of the arrays stored in
                                                  ! coupling file
INTEGER                :: IKU             !  Dimensions of arrays in 
                                                  ! initial file

INTEGER                :: ICPLEND                 ! number of the last cpl file
LOGICAL, DIMENSION(JPCPLFILEMAX)    :: GSKIP      ! array to skip or not after
                                                  ! a cpl file
REAL                   :: ZDIST                   ! temporal distance in secunds
                                                  ! between 2 dates
LOGICAL :: GLSOURCE ! switch for the source term (for ini_ls and ini_lb)
!CHARACTER(LEN=4), DIMENSION(KSV)    :: YGETSVM
!
!-------------------------------------------------------------------------------
!
!*       1.    SOME INITIALIZATIONS
!              --------------------
!
GSKIP(:)=.FALSE.
GEND=.FALSE.
NCPL_TIMES(:,:) = NUNDEF
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       2.    CHECK COUPLING FILES DATES
!              --------------------------
!
DO JCI=1,NCPL_NBR
  WRITE(YCI,'(I2.0)') JCI
  CALL FMOPEN_ll(CCPLFILE(JCI),'READ',HLUOUT,0,2,NVERB,ININAR,IRESP)
  IF (IRESP /= 0) THEN
      WRITE(ILUOUT,*) 'ERROR when opening coupling file',JCI,', IRESP=',IRESP
!callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
  END IF
!
!*       2.1   Read current time in coupling files
!
  YRECFM='DTCUR' 
  YDIR='--'
  CALL FMREAD(CCPLFILE(JCI),YRECFM,HLUOUT,YDIR,TDTCPL(JCI),IGRID,ILENCH,   &
              YCOMMENT,IRESP)
!
!*       2.2   Check chronological order
!
  CALL TEMPORAL_DIST(TDTCPL(JCI)%TDATE%YEAR,TDTCPL(JCI)%TDATE%MONTH,    &
                     TDTCPL(JCI)%TDATE%DAY ,TDTCPL(JCI)%TIME,           &
                     TDTCUR%TDATE%YEAR,TDTCUR%TDATE%MONTH,              &
                     TDTCUR%TDATE%DAY ,TDTCUR%TIME,                     &
                     ZDIST                                              )
  !
  IF ( ZDIST == XUNDEF .OR. ZDIST ==0. ) THEN
    WRITE(ILUOUT,FMT=9002) 1
    WRITE(ILUOUT,*) 'YOUR COUPLING FILE ',JCI,' IS PREVIOUS TO THE DATE &
               & CORRESPONDING TO THE BEGINNING OF THE SEGMENT. IT WILL &
               & NOT BE TAKEN INTO ACCOUNT.'
    YTITLE='CURRENT DATE AND TIME IN THE INITIAL FILE'
    CALL SM_PRINT_TIME(TDTCUR,HLUOUT,YTITLE)         
    YTITLE='CURRENT DATE AND TIME OF THE FILE'//YCI
    CALL SM_PRINT_TIME(TDTCPL(JCI),HLUOUT,YTITLE)     
    GSKIP(JCI)=.TRUE.      ! flag to skip after this coupling file
  ELSE
    NCPL_TIMES(JCI,1) = NINT( ZDIST / PTSTEP ) + 2
  END IF
  !
  IF (JCI > 1) THEN
    CALL TEMPORAL_DIST(TDTCPL(JCI)%TDATE%YEAR,TDTCPL(JCI)%TDATE%MONTH,      &
                       TDTCPL(JCI)%TDATE%DAY ,TDTCPL(JCI)%TIME,             &
                       TDTCPL(JCI-1)%TDATE%YEAR,TDTCPL(JCI-1)%TDATE%MONTH,  &
                       TDTCPL(JCI-1)%TDATE%DAY ,TDTCPL(JCI-1)%TIME,         &
                       ZDIST                                                )
    !
    IF ( ZDIST == XUNDEF ) THEN
      WRITE(ILUOUT,FMT=9003) 1
      WRITE(ILUOUT,*) 'YOU MUST SPECIFY THE COUPLING FILES IN A CHRONOLOGICAL &
                       & ORDER'
      YTITLE='CURRENT DATE AND TIME OF THE FILE'//YCI
      CALL SM_PRINT_TIME(TDTCPL(JCI),HLUOUT,YTITLE)
      WRITE(YCI,'(I2.0)') JCI-1        
      YTITLE='CURRENT DATE AND TIME OF THE FILE'//YCI
      CALL SM_PRINT_TIME(TDTCPL(JCI-1),HLUOUT,YTITLE)
      GEND=.TRUE.           ! error flag set to true   
    END IF
  !   
  END IF
END DO
!  exit when a fatal error has been encountered
IF ( GEND ) THEN
  RETURN
END IF
!
!*       2.3   Find the current coupling file and the last one
!
NCPL_CUR=1
DO JCI=1,NCPL_NBR
  IF( GSKIP(JCI) ) THEN
    NCPL_CUR = NCPL_CUR +1
  END IF 
  ! 
END DO
!
ICPLEND=NCPL_NBR
DO JCI=NCPL_CUR,NCPL_NBR
  IF( NCPL_TIMES(JCI,1) > KSTOP ) THEN
    ICPLEND = JCI 
    EXIT
  END IF
END DO
!
IF (ICPLEND==NCPL_NBR .AND. NCPL_TIMES(NCPL_NBR,1) < KSTOP) THEN
  WRITE(ILUOUT,FMT=9003) 1
  WRITE(ILUOUT,*) 'THE SEGMENT END IS GREATER THAN THE LAST COUPLING TIME'
  WRITE(ILUOUT,*) 'SPECIFY ANOTHER COUPLING FILE OR DECREASE THE SEGMENT LENGTH.'
  WRITE(ILUOUT,*) 'PLEASE, REFER TO THE USER GUIDE TO OBTAIN MORE INFORMATIONS'
  WRITE(ILUOUT,*) 'ON THE TEMPORAL GRID.'
  YTITLE='CURRENT DATE AND TIME OF THE LAST FILE'
  CALL SM_PRINT_TIME(TDTCPL(NCPL_NBR),HLUOUT,YTITLE)
  YTITLE='DATE AND TIME OF THE BEGINNING OF THE SEGMENT YOU WANT TO BE PERFORMED'
  CALL SM_PRINT_TIME(TDTCUR,HLUOUT,YTITLE)
  WRITE(ILUOUT,*) 'XSEGLEN = ', XSEGLEN 
!callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
! save the right number of useful coupling files
NCPL_NBR = ICPLEND - NCPL_CUR + 1
!         
!
!*      2.4   Write in output-listing the useful coupling files
!
!
WRITE(ILUOUT,*) 'FOR THE EVOLUTION OF LARGE SCALE FIELDS THE SEGMENT REQUIRES ',&
                & NCPL_NBR,' COUPLING FILES :'
DO JCI=NCPL_CUR,NCPL_CUR+NCPL_NBR-1
  WRITE(ILUOUT,*) CCPLFILE(JCI),' UNTIL TIME STEP : ',NCPL_TIMES(JCI,1)
END DO
!-------------------------------------------------------------------------------
!
!*      3.    FIRST COMPUTATION OF LARGE SCALE SOURCES
!             ----------------------------------------
!
!*      3.1   Read dimensions in coupling file and checks with initial file
!
YRECFM='IMAX'
YDIR='--'
CALL FMREAD(CCPLFILE(NCPL_CUR),YRECFM,HLUOUT,YDIR,IIMAX,IGRID,ILENCH,         &
            YCOMMENT,IRESP)
YRECFM='JMAX'
YDIR='--'
CALL FMREAD(CCPLFILE(NCPL_CUR),YRECFM,HLUOUT,YDIR,IJMAX,IGRID,ILENCH,         &
            YCOMMENT,IRESP)
YRECFM='KMAX'
YDIR='--'
CALL FMREAD(CCPLFILE(NCPL_CUR),YRECFM,HLUOUT,YDIR,IKMAX,IGRID,ILENCH,         &
            YCOMMENT,IRESP)
!
IKU=SIZE(PLSUM,3)
!
IF ( (IIMAX/=KIMAX_ll) .OR. (IJMAX/=KJMAX_ll)                      &
                             .OR. (IKMAX/=(IKU-2*JPVEXT)) ) THEN
  WRITE(ILUOUT,FMT=9003) 1
  WRITE(ILUOUT,*) 'THE GRIDS ARE DIFFERENT IN THE INITIAL FILE :'
  WRITE(ILUOUT,*) KIMAX_ll,'*',KJMAX_ll,'*',IKU-2*JPVEXT
  WRITE(ILUOUT,*) 'AND IN THE COUPLING FILE :',CCPLFILE(NCPL_CUR)
  WRITE(ILUOUT,*) IIMAX,'*',IJMAX,'*',IKMAX
!callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
!*      3.2   Initialize  the large scale sources
!
GLSOURCE=.TRUE.
ZLENG = (NCPL_TIMES(NCPL_CUR,1)-2) * PTSTEP
!
CALL INI_LS(CCPLFILE(NCPL_CUR),HLUOUT,HGETRVM,GLSOURCE,PLSUS,PLSVS,PLSWS,PLSTHS,PLSRVS, &
             PDRYMASSS,PLSUM,PLSVM,PLSWM,PLSTHM,PLSRVM,PDRYMASST,          &
             ZLENG,OSTEADY_DMASS)
!
!
!*      3.2   Initialize  the LB sources
!
!YGETSVM(1:KSV) = HGETSVM(1:KSV)
!IF ( LUSECHEM .AND. (.NOT. OCH_INIT_FIELD) )  &
!   YGETSVM(NSV_CHEMBEG: NSV_CHEMEND) = 'INIT'
!IF (HCONF == 'RESTA')  THEN
!   IF (NSV_USER /= 0) YGETSVM(1/NSV_USER) = 'INIT'
!   IF (NSV_C2R2 /= 0) YGETSVM(NSV_C2R2BEG: NSV_C2R2END) = 'INIT'
!   IF (NSV_C1R3 /= 0) YGETSVM(NSV_C1R3BEG: NSV_C1R3END) = 'INIT'
!   IF (NSV_ELEC /= 0) YGETSVM(NSV_ELECBEG: NSV_ELECEND) = 'INIT'
!   IF (NSV_LG   /= 0) YGETSVM(NSV_LGBEG: NSV_LGEND) = 'INIT'
!   IF (NSV_LNOX /= 0) YGETSVM(NSV_LNOXBEG: NSV_LNOXEND) = 'INIT'
!   IF (NSV_DST  /= 0) YGETSVM(NSV_DSTBEG: NSV_DSTEND) = 'INIT'
!   IF (NSV_SLT  /= 0) YGETSVM(NSV_SLTBEG: NSV_SLTEND) = 'INIT'
!   IF (NSV_DSTDEP /= 0) YGETSVM(NSV_DSTDEPBEG: NSV_DSTDEPEND) = 'INIT'
!   IF (NSV_SLTDEP /= 0) YGETSVM(NSV_SLTDEPBEG: NSV_SLTDEPEND) = 'INIT'
!   IF (NSV_PP   /= 0) YGETSVM(NSV_PPBEG: NSV_PPEND) = 'INIT'
!   IF (NSV_CS   /= 0) YGETSVM(NSV_CSBEG: NSV_CSEND) = 'INIT'
!END IF
 
GLSOURCE=.TRUE.
CALL INI_LB(CCPLFILE(NCPL_CUR),HLUOUT,GLSOURCE,KSV,                  &
     KSIZELBX_ll,KSIZELBXU_ll,KSIZELBY_ll,KSIZELBYV_ll,              &
     KSIZELBXTKE_ll,KSIZELBYTKE_ll,                                  &
     KSIZELBXR_ll,KSIZELBYR_ll,KSIZELBXSV_ll,KSIZELBYSV_ll,          &
     HGETTKEM,HGETRVM,HGETRCM,HGETRRM,HGETRIM,HGETRSM,               &
     HGETRGM,HGETRHM,HGETSVM,                                        &
     PLBXUS,PLBXVS,PLBXWS,PLBXTHS,PLBXTKES,PLBXRS,PLBXSVS,           &
     PLBYUS,PLBYVS,PLBYWS,PLBYTHS,PLBYTKES,PLBYRS,PLBYSVS,           &
     PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,           &
     PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,           &
     ZLENG)
!
!*      3.5   Close the coupling file
!
CALL FMCLOS_ll(CCPLFILE(NCPL_CUR),'KEEP',HLUOUT,IRESP)
!!-------------------------------------------------------------------------------
!
!*      6.    FORMATS
!             -------
!
9000  FORMAT(/,'NOTE  IN INI_CPL FOR MODEL ', I2, ' : ',/, &
             '--------------------------------')
9001  FORMAT(/,'CAUTION ERROR IN INI_CPL FOR MODEL ', I2,' : ',/, &
             '----------------------------------------' )
9002  FORMAT(/,'WARNING IN INI_CPL FOR MODEL ', I2,' : ',/, &
             '----------------------------------' )
9003  FORMAT(/,'FATAL ERROR IN INI_CPL FOR MODEL ', I2,' : ',/, &
             '--------------------------------------' )
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_CPL
