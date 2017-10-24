!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA (DTCO, DTI, DGU, IG, I, UG, U, USS, &
                           HPROGRAM,OECOCLIMAP)
!     ##############################################################
!
!!**** *PGD_ISBA* monitor for averaging and interpolations of ISBA physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    P. Le Moigne  12/2004 : add type of photosynthesis and correct computation
!!                            of ground layers number in diffusion case
!!    P. Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!    B. Decharme      2008 :  XWDRAIN
!!    E. Martin     12/2008 : files of data for runoffb and wdrain
!!    B. Decharme   06/2009 : files of data for topographic index
!!    A.L. Gibelin  04/2009 : dimension NBIOMASS for ISBA-A-gs
!!    R. Alkama     05/2012 : npatch must be 12 or 19 if CPHOTO/='NON'
!!    B. Decharme   11/2013 : groundwater distribution for water table/surface coupling
!!    P. Samuelsson 02/2012 : MEB
!!    M. Moge       02/2015 : MPPDB_CHECK
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER
!
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
USE MODI_READ_NAM_PGD_ISBA_MEB
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_GET_AOS_n
USE MODI_GET_SSO_n
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD_ISBA
USE MODI_PACK_PGD
USE MODI_WRITE_COVER_TEX_ISBA
USE MODI_WRITE_COVER_TEX_ISBA_PAR
USE MODI_PGD_TOPO_INDEX
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_ISBA_PAR
USE MODI_PGD_TOPD
USE MODI_CONVERT_COVER_ISBA
USE MODE_POS_SURF
!
USE MODI_READ_SURF
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
#ifdef MNH_PARALLEL
USE MODE_MPPDB
!
#endif
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
LOGICAL,          INTENT(IN)  :: OECOCLIMAP ! T if parameters are computed with ecoclimap
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: JLAYER    ! loop counter
INTEGER                           :: ILU       ! number of points
INTEGER                           :: ILUNAM    ! namelist file logical unit
REAL, DIMENSION(NL)               :: ZAOSIP    ! A/S i+ on all surface points
REAL, DIMENSION(NL)               :: ZAOSIM    ! A/S i- on all surface points
REAL, DIMENSION(NL)               :: ZAOSJP    ! A/S j+ on all surface points
REAL, DIMENSION(NL)               :: ZAOSJM    ! A/S j- on all surface points
REAL, DIMENSION(NL)               :: ZHO2IP    ! h/2 i+ on all surface points
REAL, DIMENSION(NL)               :: ZHO2IM    ! h/2 i- on all surface points
REAL, DIMENSION(NL)               :: ZHO2JP    ! h/2 j+ on all surface points
REAL, DIMENSION(NL)               :: ZHO2JM    ! h/2 j- on all surface points
REAL, DIMENSION(NL)               :: ZSSO_SLOPE! subgrid slope on all surface points
INTEGER                           :: IRESP     ! error code
LOGICAL                           :: GMEB      ! Multi-energy balance (MEB)
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: YISBA            ! ISBA option
 CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo transfert function for DIF
 CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YSOC_TOP         ! file name for organic carbon top soil
 CHARACTER(LEN=28)        :: YSOC_SUB         ! file name for organic carbon sub soil
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=28)        :: YPERM            ! file name for permafrost distribution
 CHARACTER(LEN=28)        :: YGW               ! file name for groundwater map
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YSOCFILETYPE     ! organic carbon data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE    ! permafrost distribution data file type
 CHARACTER(LEN=6)         :: YGWFILETYPE      ! groundwater distribution data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction  (-)
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction  (-)
REAL                     :: XUNIF_SOC_TOP    ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
REAL                     :: XUNIF_PERM       ! uniform permafrost distribution
REAL                     :: XUNIF_GW         ! uniform groundwater distribution
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_SOC         ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
LOGICAL                  :: LIMP_GW          ! Imposed maps of groundwater distribution
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil grid reference for DIF
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
LOGICAL, DIMENSION(19)   :: GMEB_PATCH
LOGICAL, DIMENSION(19)   :: GMEB_PATCH_REC ! Recommended MEB patch settings
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.1      Reading of ISBA namelist
!             -------------------------
!
 CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                          &
                       YISBA,  YPEDOTF, YPHOTO, GTR_ML, ZRM_PATCH,               &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,              &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,              &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,          &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,    &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, GMEB,        &                       
                       YGW, YGWFILETYPE, XUNIF_GW, LIMP_GW,                      &                       
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,                &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,      &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,         &
                       XUNIF_FERT                          )  
!
I%NPATCH        = IPATCH
I%NGROUND_LAYER = IGROUND_LAYER
I%CISBA         = YISBA
I%CPEDOTF       = YPEDOTF
I%CPHOTO        = YPHOTO
I%LTR_ML        = GTR_ML
I%XRM_PATCH     = MAX(MIN(ZRM_PATCH,1.),0.)
!
!
!-------------------------------------------------------------------------------
!
!*    2.2      Reading of ISBA MEB namelist
!             -----------------------------
!
IF (I%NPATCH<1 .OR. I%NPATCH>NVEGTYPE) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* Number of patch must be between 1 and ', NVEGTYPE
  WRITE(ILUOUT,*) '* You have chosen NPATCH = ', I%NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: NPATCH MUST BE BETWEEN 1 AND NVEGTYPE')
END IF
!
ALLOCATE(I%LMEB_PATCH(I%NPATCH))
!
I%LMEB_PATCH(:) = .FALSE.
I%LFORC_MEASURE = .FALSE.
I%LMEB_LITTER   = .FALSE.
I%LMEB_GNDRES   = .FALSE.

IF(GMEB)THEN

  I%LTR_ML      = .TRUE. ! Always use this SW radiative transfer option with MEB

  CALL READ_NAM_PGD_ISBA_MEB(HPROGRAM,ILUOUT,GMEB_PATCH,I%LFORC_MEASURE,I%LMEB_LITTER,I%LMEB_GNDRES)

! Current recommendation is to use MEB for tree patches only.
! Here follows a test in which non-tree patches in LMEB_PATCH are set to FALSE.
! Thus, if you wish to test MEB for non-tree patches you can set 
! GMEB_PATCH_REC(:)=.TRUE.
! in the following line:

  GMEB_PATCH_REC(:)=.FALSE.

  IF(I%NPATCH==1 .AND. GMEB_PATCH(1))THEN
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* WARNING!'
    WRITE(ILUOUT,*) '* Using MEB for one patch only is not recommended.'
    WRITE(ILUOUT,*) '* LMEB_PATCH(1) has been set to .FALSE.'
    WRITE(ILUOUT,*) '*****************************************'
  ELSEIF(I%NPATCH>=2 .AND. I%NPATCH<=6)THEN
    GMEB_PATCH_REC(2)=.TRUE.  ! Only the tree patch (number 2) is allowed to be TRUE
  ELSEIF(I%NPATCH>=7 .AND. I%NPATCH<=8)THEN
    GMEB_PATCH_REC(3)=.TRUE.  ! Only the tree patch (number 3) is allowed to be TRUE
  ELSEIF(I%NPATCH==9)THEN
    GMEB_PATCH_REC(3:4)=(/.TRUE.,.TRUE./)  ! Only the tree patches (numbers 3-4) are allowed to be TRUE
  ELSEIF(I%NPATCH==10)THEN
    GMEB_PATCH_REC(3:5)=(/.TRUE.,.TRUE.,.TRUE./)  ! Only the tree patches (numbers 3-5) are allowed to be TRUE
  ELSEIF(I%NPATCH>=11 .AND. I%NPATCH<=12)THEN
    GMEB_PATCH_REC(4:6)=(/.TRUE.,.TRUE.,.TRUE./)  ! Only the tree patches (numbers 4-6) are allowed to be TRUE
  ELSEIF(I%NPATCH==19)THEN
    GMEB_PATCH_REC(4:6)=(/.TRUE.,.TRUE.,.TRUE./)  ! The "old" tree patches (numbers 4-6) are allowed to be TRUE
    GMEB_PATCH_REC(13:17)=(/.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./)  ! The "new" tree patches (numbers 13-17) are allowed to be TRUE
  ENDIF

  IF(COUNT(.NOT.GMEB_PATCH_REC(:) .AND. GMEB_PATCH(:))>0)THEN
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* WARNING!'
    WRITE(ILUOUT,*) '* Using MEB for non-tree patches is not yet recommended.'
    WRITE(ILUOUT,*) '* Therefor, LMEB_PATCH for non-tree patches has been set to .FALSE.'
    WRITE(ILUOUT,*) '* The final LMEB_PATCH vector becomes:'
    WRITE(ILUOUT,*) GMEB_PATCH(1:I%NPATCH).AND.GMEB_PATCH_REC(1:I%NPATCH)
    WRITE(ILUOUT,*) '*****************************************'
  ENDIF
  GMEB_PATCH(:)=GMEB_PATCH(:).AND.GMEB_PATCH_REC(:)

  I%LMEB_PATCH(1:I%NPATCH) = GMEB_PATCH(1:I%NPATCH)
!
  IF (I%LMEB_LITTER) THEN
   I%LMEB_GNDRES = .FALSE.
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',I%CISBA,'2-L','3-L','DIF')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',I%CPEDOTF,'CH78','CO84')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',I%CPHOTO,'NON','AGS','LAI','AST','LST','NIT','NCB')
!
SELECT CASE (I%CISBA)
!
  CASE ('2-L')
!          
    I%NGROUND_LAYER = 2
    I%CPEDOTF       ='CH78'   
    ALLOCATE(I%XSOILGRID(0))
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',I%CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'    
    WRITE(ILUOUT,*) '*****************************************'
!    
  CASE ('3-L')
!          
    I%NGROUND_LAYER = 3
    I%CPEDOTF       ='CH78'   
    ALLOCATE(I%XSOILGRID(0))    
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',I%CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'    
    WRITE(ILUOUT,*) '*****************************************'
!    
  CASE ('DIF')
!          
    IF(I%NGROUND_LAYER==NUNDEF)THEN
      IF(OECOCLIMAP)THEN
        I%NGROUND_LAYER=NOPTIMLAYER
      ELSE
        WRITE(ILUOUT,*) '****************************************'
        WRITE(ILUOUT,*) '* Number of ground layer not specified *'
        WRITE(ILUOUT,*) '****************************************'
        CALL ABOR1_SFX('PGD_ISBA: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
      ENDIF
    ENDIF
! 
    ALLOCATE(I%XSOILGRID(I%NGROUND_LAYER))
    I%XSOILGRID(:)=XUNDEF
    I%XSOILGRID(:)=ZSOILGRID(1:I%NGROUND_LAYER) 
    IF (ALL(ZSOILGRID(:)==XUNDEF)) THEN
      IF(OECOCLIMAP) I%XSOILGRID(1:I%NGROUND_LAYER)=XOPTIMGRID(1:I%NGROUND_LAYER)
    ELSEIF (COUNT(I%XSOILGRID/=XUNDEF)/=I%NGROUND_LAYER) THEN
      WRITE(ILUOUT,*) '********************************************************'
      WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
      WRITE(ILUOUT,*) '********************************************************'
      CALL ABOR1_SFX('PGD_ISBA: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA') 
    ELSEIF (I%XSOILGRID(1).GT.0.01) THEN
      CALL ABOR1_SFX('PGD_ISBA: First layer of XSOILGRID must be lower than 1cm')
    ENDIF
!
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* Option CISBA            = ',I%CISBA
    WRITE(ILUOUT,*) '* Pedo transfert function = ',I%CPEDOTF    
    WRITE(ILUOUT,*) '* Number of soil layers   = ',I%NGROUND_LAYER
    IF(OECOCLIMAP)THEN
      WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',I%XSOILGRID(1:I%NGROUND_LAYER)
    ENDIF
    WRITE(ILUOUT,*) '*****************************************'
!    
END SELECT
!
SELECT CASE (I%CPHOTO)
  CASE ('AGS','LAI','AST','LST')
    I%NNBIOMASS = 1
  CASE ('NIT')
    I%NNBIOMASS = 3
  CASE ('NCB')
    I%NNBIOMASS = 6
END SELECT
WRITE(ILUOUT,*) '*****************************************'
WRITE(ILUOUT,*) '* With option CPHOTO = ',I%CPHOTO,'               *'
WRITE(ILUOUT,*) '* the number of biomass pools is set to ', I%NNBIOMASS
WRITE(ILUOUT,*) '*****************************************'
!
IF ( I%CPHOTO/='NON' .AND. I%NPATCH/=12 .AND. I%NPATCH/=19 ) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ', I%CPHOTO
  WRITE(ILUOUT,*) '* Number of patch must be equal to 12 or 19'
  WRITE(ILUOUT,*) '* But you have chosen NPATCH = ', I%NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: CPHOTO='//I%CPHOTO//' REQUIRES NPATCH=12 or 19')
END IF
!
IF ( I%CPHOTO=='NON' .AND. I%LTR_ML .AND. .NOT. GMEB) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO == NON      '
  WRITE(ILUOUT,*) '* And With MEB = F               '
  WRITE(ILUOUT,*) '* New radiative transfert TR_ML  '
  WRITE(ILUOUT,*) '* cant be used '
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: WITH CPHOTO= NON LTR_ML MUST BE FALSE')
END IF
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing of general fields
!             ----------------------------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'NATURE',ILU)
!
ALLOCATE(I%LCOVER     (JPCOVER))
ALLOCATE(I%XZS        (ILU))
ALLOCATE(IG%XLAT       (ILU))
ALLOCATE(IG%XLON       (ILU))
ALLOCATE(IG%XMESH_SIZE (ILU))
ALLOCATE(I%XZ0EFFJPDIR(ILU))
!
 CALL PACK_PGD(DTCO, U, &
               HPROGRAM, 'NATURE',                    &
                IG%CGRID,  IG%XGRID_PAR,                     &
                I%LCOVER, I%XCOVER, I%XZS,                   &
                IG%XLAT, IG%XLON, IG%XMESH_SIZE, I%XZ0EFFJPDIR    )  
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX2D(IG%XLAT,"PGD_ISBA after PACK_PGD:XLAT",PRECISION,ILUOUT, 'NATURE')
 CALL MPPDB_CHECK_SURFEX2D(IG%XLON,"PGD_ISBA after PACK_PGD:XLON",PRECISION,ILUOUT, 'NATURE')
 CALL MPPDB_CHECK_SURFEX2D(IG%XMESH_SIZE,"PGD_ISBA after PACK_PGD:XMESH_SIZE",PRECISION,ILUOUT, 'NATURE')
#endif
!
!-------------------------------------------------------------------------------
!
!*    5.      Packing of ISBA specific fields
!             -------------------------------
!
 CALL GET_AOS_n(USS, &
                HPROGRAM,NL,ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM)
 CALL GET_SSO_n(USS, &
                HPROGRAM,NL,ZSSO_SLOPE)
!
 CALL PACK_PGD_ISBA(DTCO, IG, I, U, &
                    HPROGRAM,                                    &
                     ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,              &
                     ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM,              &
                     ZSSO_SLOPE                                   )  
!
!-------------------------------------------------------------------------------
!
!*    6.      Topographic index for TOPMODEL
!             ------------------------------
!
 CALL PGD_TOPO_INDEX(DGU, DTCO, UG, U, USS, I, &
                     HPROGRAM,ILU,YCTI,YCTIFILETYPE,LIMP_CTI)
!
!-------------------------------------------------------------------------------
!
!*    7.      Sand fraction
!             -------------
!
 CATYPE='ARI'
!
ALLOCATE(I%XSAND(ILU,I%NGROUND_LAYER))
!
IF(LIMP_SAND)THEN
!
  IF(YSANDFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for sand input file with LIMP_SAND')
  ELSE
#ifdef SFX_ASC
     CFILEIN     = ADJUSTL(ADJUSTR(YSAND)//'.txt')
#endif
#ifdef SFX_FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(YSAND)//'.fa')
#endif
#ifdef SFX_LFI
     CFILEIN_LFI = ADJUSTL(YSAND)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YSANDFILETYPE,'NATURE','ISBA  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(&
                 YSANDFILETYPE,'SAND',I%XSAND(:,1),IRESP) 
!
  CALL END_IO_SURF_n(YSANDFILETYPE)
!
ELSE
   CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'sand fraction','NAT',YSAND,YSANDFILETYPE,XUNIF_SAND,I%XSAND(:,1))
ENDIF
!
DO JLAYER=1,I%NGROUND_LAYER
  I%XSAND(:,JLAYER) = I%XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    8.      Clay fraction
!             -------------
!
ALLOCATE(I%XCLAY(ILU,I%NGROUND_LAYER))
!
IF(LIMP_CLAY)THEN
!
  IF(YCLAYFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for clay input file with LIMP_CLAY')
  ELSE
#ifdef SFX_ASC
     CFILEIN     = ADJUSTL(ADJUSTR(YCLAY)//'.txt')
#endif
#ifdef SFX_FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(YCLAY)//'.fa')
#endif
#ifdef SFX_LFI
     CFILEIN_LFI = ADJUSTL(YCLAY)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YCLAYFILETYPE,'NATURE','ISBA  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(&
                 YCLAYFILETYPE,'CLAY',I%XCLAY(:,1),IRESP) 
!
  CALL END_IO_SURF_n(YCLAYFILETYPE)
!
ELSE
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'clay fraction','NAT',YCLAY,YCLAYFILETYPE,XUNIF_CLAY,I%XCLAY(:,1))
ENDIF
!
DO JLAYER=1,I%NGROUND_LAYER
  I%XCLAY(:,JLAYER) = I%XCLAY(:,1)
END DO
!
!-------------------------------------------------------------------------------
!
!*    9.      organic carbon profile
!             ----------------------
!
IF(LEN_TRIM(YSOCFILETYPE)/=0.OR.(XUNIF_SOC_TOP/=XUNDEF.AND.XUNIF_SOC_SUB/=XUNDEF))THEN
!
  ALLOCATE(I%XSOC(ILU,I%NGROUND_LAYER))
!
  I%LSOCP=.TRUE.
!
  IF((LEN_TRIM(YSOC_TOP)==0.AND.LEN_TRIM(YSOC_SUB)/=0).OR.(LEN_TRIM(YSOC_TOP)/=0.AND.LEN_TRIM(YSOC_SUB)==0))THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in soil organic carbon preparation                *'
    WRITE(ILUOUT,*) '* If used, sub and top soil input file must be given      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA: TOP AND SUB SOC INPUT FILE REQUIRED')        
  ENDIF
!
  IF(LIMP_SOC)THEN
!
!   Topsoil
!
    IF(YSOCFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for organic carbon input file with LIMP_SOC')
    ELSE
#ifdef SFX_ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YSOC_TOP)//'.txt')
#endif
#ifdef SFX_FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YSOC_TOP)//'.fa')
#endif
#ifdef SFX_LFI
       CFILEIN_LFI = ADJUSTL(YSOC_TOP)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YSOCFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(&
                 YSOCFILETYPE,'SOC_TOP',I%XSOC(:,1),IRESP) 
!
    CALL END_IO_SURF_n(YSOCFILETYPE)
!
!   Subsoil
!
    IF(YSOCFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for organic carbon input file with LIMP_SOC')
    ELSE
#ifdef SFX_ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YSOC_SUB)//'.txt')
#endif
#ifdef SFX_FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YSOC_SUB)//'.fa')
#endif
#ifdef SFX_LFI
       CFILEIN_LFI = ADJUSTL(YSOC_SUB)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YSOCFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(&
                 YSOCFILETYPE,'SOC_SUB',I%XSOC(:,2),IRESP) 
!
    CALL END_IO_SURF_n(YSOCFILETYPE)
!
  ELSE
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'organic carbon','NAT',YSOC_TOP,YSOCFILETYPE,XUNIF_SOC_TOP,I%XSOC(:,1))
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'organic carbon','NAT',YSOC_SUB,YSOCFILETYPE,XUNIF_SOC_SUB,I%XSOC(:,2))
  ENDIF
!
  DO JLAYER=2,I%NGROUND_LAYER
    I%XSOC(:,JLAYER) = I%XSOC(:,2)
  END DO
!
ELSE
!
  I%LSOCP=.FALSE.
  ALLOCATE(I%XSOC(0,0))
!
ENDIF
!
!*    10.     Permafrost distribution
!             -----------------------
!
IF(LEN_TRIM(YPERM)/=0.OR.XUNIF_PERM/=XUNDEF)THEN
!
  ALLOCATE(I%XPERM(ILU))
!
  I%LPERM=.TRUE.
!
  IF(LIMP_PERM)THEN
!
    IF(YPERMFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for permafrost input file with LIMP_PERM')
    ELSE
#ifdef SFX_ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YPERM)//'.txt')
#endif
#ifdef SFX_FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YPERM)//'.fa')
#endif
#ifdef SFX_LFI
       CFILEIN_LFI = ADJUSTL(YPERM)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YPERMFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(&
                 YPERMFILETYPE,'PERM',I%XPERM(:),IRESP) 
!
    CALL END_IO_SURF_n(YPERMFILETYPE)
  ELSE
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'permafrost mask','NAT',YPERM,YPERMFILETYPE,XUNIF_PERM,I%XPERM(:))
  ENDIF
!
ELSE
!
  I%LPERM=.FALSE.  
  ALLOCATE(I%XPERM(0))
!
ENDIF
!
!*    11.     Groundwater bassin distribution
!             -----------------------
!
IF(LEN_TRIM(YGW)/=0.OR.XUNIF_GW/=XUNDEF)THEN
!
  ALLOCATE(I%XGW(ILU))
!
  I%LGW=.TRUE.
!
  IF(LIMP_GW)THEN
!
    IF(YGWFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for groundwater input file with LIMP_GW')
    ELSE
#ifdef SFX_ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YGW)//'.txt')
#endif
#ifdef SFX_FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YGW)//'.fa')
#endif
#ifdef SFX_LFI
       CFILEIN_LFI = ADJUSTL(YGW)
#endif
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                         YGWFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(&
                 YGWFILETYPE,'GW',I%XGW(:),IRESP) 
!
    CALL END_IO_SURF_n(YGWFILETYPE)
  ELSE
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'Groundwater bassin','NAT',YGW,YGWFILETYPE,XUNIF_GW,I%XGW(:))
  ENDIF
!
ELSE
!
  I%LGW=.FALSE.  
  ALLOCATE(I%XGW(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    12.  pH and fertlisation data
!             --------------------------
!
IF((LEN_TRIM(YPHFILETYPE)/=0.OR.XUNIF_PH/=XUNDEF) .AND. (LEN_TRIM(YFERTFILETYPE)/=0.OR.XUNIF_FERT/=XUNDEF)) THEN
  !
  ALLOCATE(I%XPH(ILU))
  ALLOCATE(I%XFERT(ILU))
  !
  I%LNOF = .TRUE.
  !
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'pH value','NAT',YPH,YPHFILETYPE,XUNIF_PH,I%XPH(:))
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'fertilisation','NAT',YFERT,YFERTFILETYPE,XUNIF_FERT,I%XFERT(:))
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    13.      Subgrid runoff 
!             --------------
!
ALLOCATE(I%XRUNOFFB(ILU))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid runoff','NAT',YRUNOFFB,YRUNOFFBFILETYPE,XUNIF_RUNOFFB,I%XRUNOFFB(:))  
!
!-------------------------------------------------------------------------------
!
!*    14.     Drainage coefficient
!             --------------------
!
ALLOCATE(I%XWDRAIN(ILU))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid drainage','NAT',YWDRAIN,YWDRAINFILETYPE,XUNIF_WDRAIN,I%XWDRAIN(:))
!
!-------------------------------------------------------------------------------
!
!*   15.      ISBA specific fields
!             --------------------
!
I%LECOCLIMAP = OECOCLIMAP
!
 CALL PGD_ISBA_PAR(DTCO, DGU, UG, U, USS, DTI, I, IG, &
                   HPROGRAM)
!
!-------------------------------------------------------------------------------
!
 CALL PGD_TOPD(I, UG, U, USS, &
               HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*   16.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (OECOCLIMAP) THEN
  ALLOCATE(I%XDG(ILU,I%NGROUND_LAYER,I%NPATCH))
  IF (I%CISBA=='DIF') THEN
    ALLOCATE(I%NWG_LAYER(ILU,I%NPATCH))
  ELSE
    ALLOCATE(I%NWG_LAYER(0,0))
  END IF
  CALL CONVERT_COVER_ISBA(DTCO,I,I%CISBA,1,I%XCOVER,I%LCOVER,'   ','NAT',&
          PSOILGRID=I%XSOILGRID,PDG=I%XDG,KWG_LAYER=I%NWG_LAYER)
END IF

!*   17.     Prints of cover parameters in a tex file
!            ----------------------------------------
!
IF (OECOCLIMAP) THEN
  CALL WRITE_COVER_TEX_ISBA    (I%NPATCH,I%NGROUND_LAYER,I%CISBA)
  CALL WRITE_COVER_TEX_ISBA_PAR(DTCO, I, &
                                I%NPATCH,I%NGROUND_LAYER,I%CISBA,I%CPHOTO,I%XSOILGRID)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_ISBA
