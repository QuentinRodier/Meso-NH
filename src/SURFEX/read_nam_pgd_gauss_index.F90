!     #########
      SUBROUTINE READ_NAM_PGD_GAUSS_INDEX(HPROGRAM,OINDEX_STORE,HINDEX_1KM,HINDEX_10KM,    &
                                            HINDEX_100KM,HCOVER,HZS,HCLAY,HSAND,HCTI,      &
                                            HPERM,HSOC_TOP,HSOC_SUB,                       & 
                                            OIMP_COVER,OIMP_ZS,OIMP_CLAY,OIMP_SAND,        &
                                            OIMP_CTI,OIMP_PERM,OIMP_SOC,                   &
                                            OUNIF_COVER,OUNIF_ZS,OUNIF_SAND,               &
                                            OUNIF_CLAY,OUNIF_CTI,OUNIF_PERM,OUNIF_SOC,     &
                                            OSTOP_PGD                 )  
!     ####################################################################################
!!
!!    PURPOSE
!!    -------
!!     
!!    *READ_NAM_PGD_GAUSS_INDEX* reads namelist for Gauss grid index
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_SURF_ATM_n,     ONLY : CNATURE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
USE MODI_READ_NAM_PGD_COVER
USE MODI_READ_NAM_PGD_OROGRAPHY
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_READ_NAM_PGD_ISBA
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     
LOGICAL,             INTENT(OUT)   :: OINDEX_STORE
 CHARACTER(LEN=28),   INTENT(OUT)   :: HINDEX_1KM
 CHARACTER(LEN=28),   INTENT(OUT)   :: HINDEX_10KM
 CHARACTER(LEN=28),   INTENT(OUT)   :: HINDEX_100KM
 CHARACTER(LEN=28),   INTENT(OUT)   :: HCOVER
 CHARACTER(LEN=28),   INTENT(OUT)   :: HZS
 CHARACTER(LEN=28),   INTENT(OUT)   :: HCLAY
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSAND
 CHARACTER(LEN=28),   INTENT(OUT)   :: HCTI
 CHARACTER(LEN=28),   INTENT(OUT)   :: HPERM
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSOC_TOP
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSOC_SUB
LOGICAL,             INTENT(OUT)   :: OIMP_COVER
LOGICAL,             INTENT(OUT)   :: OIMP_ZS
LOGICAL,             INTENT(OUT)   :: OIMP_CLAY
LOGICAL,             INTENT(OUT)   :: OIMP_SAND
LOGICAL,             INTENT(OUT)   :: OIMP_CTI
LOGICAL,             INTENT(OUT)   :: OIMP_PERM
LOGICAL,             INTENT(OUT)   :: OIMP_SOC
LOGICAL,             INTENT(OUT)   :: OUNIF_COVER
LOGICAL,             INTENT(OUT)   :: OUNIF_ZS 
LOGICAL,             INTENT(OUT)   :: OUNIF_SAND
LOGICAL,             INTENT(OUT)   :: OUNIF_CLAY
LOGICAL,             INTENT(OUT)   :: OUNIF_CTI
LOGICAL,             INTENT(OUT)   :: OUNIF_PERM
LOGICAL,             INTENT(OUT)   :: OUNIF_SOC
LOGICAL,             INTENT(OUT)   :: OSTOP_PGD
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                         :: ILUOUT    ! output listing logical unit
INTEGER                         :: ILUNAM    ! namelist file logical unit
LOGICAL                         :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of cover namelist
!            -----------------------------
!
REAL, DIMENSION(JPCOVER) :: XUNIF_COVER    ! value of each cover (cover will be
!                                                   uniform on the horizontal)
 CHARACTER(LEN=28)        :: YCOVER         ! file name for cover types
 CHARACTER(LEN=6)         :: YCOVERFILETYPE ! data file type
REAL                     :: XRM_COVER      ! limit of coverage under which the
                                           ! cover is removed. Default is 1.E-6
REAL                     :: XRM_COAST      ! limit of coast coverage under which
                                           ! the coast is replaced by sea or
                                           ! inland water. Default is 1.
REAL                     :: XRM_LAKE       ! limit of inland lake coverage under which
                                           ! the water is removed. Default is 0.0                                        
REAL                     :: XRM_SEA        ! limit of sea coverage under which
                                           ! the sea is removed. Default is 0.0
LOGICAL                  :: LORCA_GRID     ! flag to compatibility between Surfex and Orca grid 
                                           ! (Earth Model over Antarctic)
REAL                     :: XLAT_ANT       ! Lattitude limit from Orca grid (Antartic)
LOGICAL                  :: LIMP_COVER     ! Imposed values for Cover from another PGD file
!
!*    0.4    Declaration of orography namelist
!            ---------------------------------
!
 CHARACTER(LEN=28)        :: YZS         ! file name for orography
 CHARACTER(LEN=6)         :: YZSFILETYPE ! data file type
REAL                     :: XUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3)         :: COROGTYPE   ! orogpraphy type 
REAL                     :: XENV        ! parameter for enveloppe orography:
LOGICAL                  :: LIMP_ZS     ! Imposed orography from another PGD file
!
!*    0.5    Declaration of Nature namelist
!            ------------------------------
!
INTEGER                  :: NPATCH           ! number of patches
INTEGER                  :: NGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: CISBA            ! ISBA option
 CHARACTER(LEN=4)         :: CPEDOTF          !
 CHARACTER(LEN=3)         :: CPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YPERM            ! file name for permafrost distribution
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=28)        :: YSOC_TOP         ! file name for organic carbon
 CHARACTER(LEN=28)        :: YSOC_SUB         ! file name for organic carbon
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE    ! permafrost distribution data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
 CHARACTER(LEN=6)         :: YSOCFILETYPE     ! organic carbon data file type
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand from another PGD file
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay from another PGD file
LOGICAL                  :: LIMP_CTI         ! Imposed values for topographic index statistics from another PGD file
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
LOGICAL                  :: LIMP_SOC         ! Imposed maps of organic carbon
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform value of subgrid drainage coefficient
REAL                     :: XUNIF_PERM       ! uniform value of permafrost distribution
REAL                     :: XUNIF_SOC_TOP    ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil (kg/m2)
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil layer thickness for DIF
!
! NO flux
!
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
!*    0.6    Declaration of gauss namelist
!            -----------------------------
!
LOGICAL                  :: LINDEX_STORE
LOGICAL                  :: LSTOP_PGD
!
 CHARACTER(LEN=28)        :: YINDEX_1KM        ! file name for gauss index at 1km
 CHARACTER(LEN=28)        :: YINDEX_10KM       ! file name for gauss index at 10km
 CHARACTER(LEN=28)        :: YINDEX_100KM      ! file name for gauss index at 100km
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_GAUSS_INDEX/ LINDEX_STORE, LSTOP_PGD, YINDEX_1KM, YINDEX_10KM, YINDEX_100KM
!
!---------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_GAUSS_INDEX',0,ZHOOK_HANDLE)
LINDEX_STORE    = .TRUE.
LSTOP_PGD       = .FALSE.
!
YINDEX_1KM      = '                          '
YINDEX_10KM     = '                          '
YINDEX_100KM    = '                          '
!
XUNIF_COVER(:) = XUNDEF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Read gauss index namelists
!             --------------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_GAUSS_INDEX',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_GAUSS_INDEX)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Read all namelists
!             ------------------
!
! Cover
!
 CALL READ_NAM_PGD_COVER(HPROGRAM, YCOVER, YCOVERFILETYPE, XUNIF_COVER,  &
                          XRM_COVER, XRM_COAST, XRM_LAKE, XRM_SEA,        &
                          LORCA_GRID, XLAT_ANT, LIMP_COVER                )  
!
! Orography
!
 CALL READ_NAM_PGD_OROGRAPHY(HPROGRAM, YZS, YZSFILETYPE, XUNIF_ZS, &
                              COROGTYPE, XENV, LIMP_ZS   )  
!
! Nature field
!
IF(CNATURE=='ISBA')THEN
!
   CALL READ_NAM_PGD_ISBA(HPROGRAM, NPATCH, NGROUND_LAYER, CISBA, CPEDOTF,             &
                            CPHOTO, GTR_ML, ZRM_PATCH,                                 &
                            YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,               &
                            YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,               &
                            YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,           &
                            XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,     &
                            YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM,               & 
                            YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,                 &
                            YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,       &
                            YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,          &
                            XUNIF_FERT                                )  
!
ENDIF
!
!-------------------------------------------------------------------------------
!
OINDEX_STORE = LINDEX_STORE
HINDEX_1KM   = YINDEX_1KM
HINDEX_10KM  = YINDEX_10KM
HINDEX_100KM = YINDEX_100KM
OSTOP_PGD    = LSTOP_PGD
!
IF(LSTOP_PGD.AND..NOT.OINDEX_STORE)THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in READ_NAM_PGD_GAUSS_INDEX                       *'
  WRITE(ILUOUT,*) '* IF LSTOP_PGD, LINDEX_STORE must be True (default)       *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('READ_NAM_PGD_GAUSS_INDEX: IF LSTOP_PGD, LINDEX_STORE must be True')        
ENDIF
!
GFOUND=(LEN_TRIM(YINDEX_1KM)/=0.OR.LEN_TRIM(YINDEX_10KM)/=0.OR.LEN_TRIM(YINDEX_100KM)/=0)
!
IF(LSTOP_PGD.AND.GFOUND)THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***************************************************************'
  WRITE(ILUOUT,*) '* Error in READ_NAM_PGD_GAUSS_INDEX                           *'
  WRITE(ILUOUT,*) '* IF file index is defined, LSTOP_PGD must be false (default) *'
  WRITE(ILUOUT,*) '***************************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('READ_NAM_PGD_GAUSS_INDEX: IF YINDEX, LSTOP_PGD must be false')        
ENDIF
!
HCOVER = YCOVER
HZS    = YZS
HCLAY  = YCLAY
HSAND  = YSAND
HCTI   = YCTI
HPERM  = YPERM
HSOC_TOP  = YSOC_TOP
HSOC_SUB  = YSOC_SUB
!
OIMP_COVER = LIMP_COVER
OIMP_ZS    = LIMP_ZS
OIMP_CLAY  = LIMP_CLAY
OIMP_SAND  = LIMP_SAND
OIMP_CTI   = LIMP_CTI
OIMP_PERM  = LIMP_PERM
OIMP_SOC   = LIMP_SOC
!
OUNIF_COVER = (ANY(XUNIF_COVER/=0.))
OUNIF_ZS    = (XUNIF_ZS/=XUNDEF)
OUNIF_SAND  = (LEN_TRIM(YSAND)==0)
OUNIF_CLAY  = (LEN_TRIM(YCLAY)==0)
OUNIF_CTI   = (LEN_TRIM(YCTI)==0)
OUNIF_PERM  = (XUNIF_PERM/=XUNDEF)
OUNIF_SOC   = (XUNIF_SOC_TOP/=XUNDEF.AND.XUNIF_SOC_TOP/=XUNDEF)
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_GAUSS_INDEX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_GAUSS_INDEX
