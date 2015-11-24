!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA(HPROGRAM,OECOCLIMAP)
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
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER
USE MODD_ISBA_n,         ONLY : NPATCH, NGROUND_LAYER, NNBIOMASS, CISBA, &
                                CPEDOTF, XCOVER, LCOVER, XZS,            &
                                XZ0EFFJPDIR, CPHOTO, LTR_ML, XRM_PATCH,  &
                                XCLAY, XSAND, XSOC, LSOCP, LNOF,         &
                                XRUNOFFB, XWDRAIN, LECOCLIMAP,           &
                                XSOILGRID, LPERM, XPERM, XPH, XFERT,     &
                                XDG, NWG_LAYER 
USE MODD_ISBA_GRID_n,    ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE
!
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
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
USE MODI_PGD_ISBA_PAR
USE MODI_PGD_TOPD
USE MODI_CONVERT_COVER_ISBA
!
USE MODI_READ_SURF
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
#ifdef ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
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
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YSOCFILETYPE     ! organic carbon data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE    ! permafrost distribution data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction  (-)
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction  (-)
REAL                     :: XUNIF_SOC_TOP    ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
REAL                     :: XUNIF_PERM       ! uniform permafrost distribution
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_SOC         ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil grid reference for DIF
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                          &
                       YISBA,  YPEDOTF, YPHOTO, GTR_ML, ZRM_PATCH,               &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,              &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,              &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,          &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,    &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM,              &                       
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,                &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,      &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,         &
                       XUNIF_FERT                          )  
!
NPATCH        = IPATCH
NGROUND_LAYER = IGROUND_LAYER
CISBA         = YISBA
CPEDOTF       = YPEDOTF
CPHOTO        = YPHOTO
LTR_ML        = GTR_ML
XRM_PATCH     = MAX(MIN(ZRM_PATCH,1.),0.)
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',CISBA,'2-L','3-L','DIF')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',CPEDOTF,'CH78','CO84')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',CPHOTO,'NON','AGS','LAI','AST','LST','NIT','NCB')
!
SELECT CASE (CISBA)
!
  CASE ('2-L')
!          
    NGROUND_LAYER = 2
    CPEDOTF       ='CH78'   
    ALLOCATE(XSOILGRID(0))
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'    
    WRITE(ILUOUT,*) '*****************************************'
!    
  CASE ('3-L')
!          
    NGROUND_LAYER = 3
    CPEDOTF       ='CH78'    
    ALLOCATE(XSOILGRID(0))
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'    
    WRITE(ILUOUT,*) '*****************************************'
!    
  CASE ('DIF')
!          
    IF(NGROUND_LAYER==NUNDEF)THEN
      IF(OECOCLIMAP)THEN
        NGROUND_LAYER=NOPTIMLAYER
      ELSE
        WRITE(ILUOUT,*) '****************************************'
        WRITE(ILUOUT,*) '* Number of ground layer not specified *'
        WRITE(ILUOUT,*) '****************************************'
        CALL ABOR1_SFX('PGD_ISBA: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
      ENDIF
    ENDIF
! 
    ALLOCATE(XSOILGRID(NGROUND_LAYER))
    XSOILGRID(:)=XUNDEF
    XSOILGRID(:)=ZSOILGRID(1:NGROUND_LAYER) 
    IF (ALL(ZSOILGRID(:)==XUNDEF)) THEN
      IF(OECOCLIMAP) XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
    ELSEIF (COUNT(XSOILGRID/=XUNDEF)/=NGROUND_LAYER) THEN
      WRITE(ILUOUT,*) '********************************************************'
      WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
      WRITE(ILUOUT,*) '********************************************************'
      CALL ABOR1_SFX('PGD_ISBA: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA') 
    ELSEIF (XSOILGRID(1).GT.0.01) THEN
      CALL ABOR1_SFX('PGD_ISBA: First layer of XSOILGRID must be lower than 1cm')
    ENDIF
!
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* Option CISBA            = ',CISBA
    WRITE(ILUOUT,*) '* Pedo transfert function = ',CPEDOTF    
    WRITE(ILUOUT,*) '* Number of soil layers   = ',NGROUND_LAYER
    IF(OECOCLIMAP)THEN
      WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',XSOILGRID(1:NGROUND_LAYER)
    ENDIF
    WRITE(ILUOUT,*) '*****************************************'
!    
END SELECT
!
SELECT CASE (CPHOTO)
  CASE ('AGS','LAI','AST','LST')
    NNBIOMASS = 1
  CASE ('NIT')
    NNBIOMASS = 3
  CASE ('NCB')
    NNBIOMASS = 6
END SELECT
WRITE(ILUOUT,*) '*****************************************'
WRITE(ILUOUT,*) '* With option CPHOTO = ',CPHOTO,'               *'
WRITE(ILUOUT,*) '* the number of biomass pools is set to ', NNBIOMASS
WRITE(ILUOUT,*) '*****************************************'
!
IF (NPATCH<1 .OR. NPATCH>NVEGTYPE) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* Number of patch must be between 1 and ', NVEGTYPE
  WRITE(ILUOUT,*) '* You have chosen NPATCH = ', NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: NPATCH MUST BE BETWEEN 1 AND NVEGTYPE')
END IF
!
IF ( CPHOTO/='NON' .AND. NPATCH/=12 ) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ', CPHOTO
  WRITE(ILUOUT,*) '* Number of patch must be equal to 12 '
  WRITE(ILUOUT,*) '* But you have chosen NPATCH = ', NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: CPHOTO='//CPHOTO//' REQUIRES NPATCH=12')
END IF
!
IF ( CPHOTO=='NON' .AND. LTR_ML ) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO == NON '
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
 CALL GET_SURF_SIZE_n('NATURE',ILU)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (ILU,JPCOVER))
ALLOCATE(XZS        (ILU))
ALLOCATE(XLAT       (ILU))
ALLOCATE(XLON       (ILU))
ALLOCATE(XMESH_SIZE (ILU))
ALLOCATE(XZ0EFFJPDIR(ILU))
!
 CALL PACK_PGD(HPROGRAM, 'NATURE',                    &
                CGRID,  XGRID_PAR,                     &
                LCOVER, XCOVER, XZS,                   &
                XLAT, XLON, XMESH_SIZE, XZ0EFFJPDIR    )  
!
!-------------------------------------------------------------------------------
!
!*    5.      Packing of ISBA specific fields
!             -------------------------------
!
 CALL GET_AOS_n(HPROGRAM,NL,ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM)
 CALL GET_SSO_n(HPROGRAM,NL,ZSSO_SLOPE)
!
 CALL PACK_PGD_ISBA(HPROGRAM,                                    &
                     ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,              &
                     ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM,              &
                     ZSSO_SLOPE                                   )  
!
!-------------------------------------------------------------------------------
!
!*    6.      Topographic index for TOPMODEL
!             ------------------------------
!
 CALL PGD_TOPO_INDEX(HPROGRAM,ILU,YCTI,YCTIFILETYPE,LIMP_CTI)
!
!-------------------------------------------------------------------------------
!
!*    7.      Sand fraction
!             -------------
!
CATYPE='ARI'
!
ALLOCATE(XSAND(ILU,NGROUND_LAYER))
!
IF(LIMP_SAND)THEN
!
  IF(YSANDFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for sand input file with LIMP_SAND')
  ELSE
#ifdef ASC
     CFILEIN     = ADJUSTL(ADJUSTR(YSAND)//'.txt')
#endif
#ifdef FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(YSAND)//'.fa')
#endif
#ifdef LFI
     CFILEIN_LFI = ADJUSTL(YSAND)
#endif
     CALL INIT_IO_SURF_n(YSANDFILETYPE,'NATURE','ISBA  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(YSANDFILETYPE,'SAND',XSAND(:,1),IRESP) 
!
  CALL END_IO_SURF_n(YSANDFILETYPE)
!
ELSE
   CALL PGD_FIELD(HPROGRAM,'sand fraction','NAT',YSAND,YSANDFILETYPE,XUNIF_SAND,XSAND(:,1))
ENDIF
!
DO JLAYER=1,NGROUND_LAYER
  XSAND(:,JLAYER) = XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    8.      Clay fraction
!             -------------
!
ALLOCATE(XCLAY(ILU,NGROUND_LAYER))
!
IF(LIMP_CLAY)THEN
!
  IF(YCLAYFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for clay input file with LIMP_CLAY')
  ELSE
#ifdef ASC
     CFILEIN     = ADJUSTL(ADJUSTR(YSAND)//'.txt')
#endif
#ifdef FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(YSAND)//'.fa')
#endif
#ifdef LFI
     CFILEIN_LFI = ADJUSTL(YSAND)
#endif
     CALL INIT_IO_SURF_n(YCLAYFILETYPE,'NATURE','ISBA  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(YCLAYFILETYPE,'CLAY',XCLAY(:,1),IRESP) 
!
  CALL END_IO_SURF_n(YCLAYFILETYPE)
!
ELSE
  CALL PGD_FIELD(HPROGRAM,'clay fraction','NAT',YCLAY,YCLAYFILETYPE,XUNIF_CLAY,XCLAY(:,1))
ENDIF
!
DO JLAYER=1,NGROUND_LAYER
  XCLAY(:,JLAYER) = XCLAY(:,1)
END DO
!
!-------------------------------------------------------------------------------
!
!*    9.      organic carbon profile
!             ----------------------
!
IF(LEN_TRIM(YSOCFILETYPE)/=0.OR.(XUNIF_SOC_TOP/=XUNDEF.AND.XUNIF_SOC_SUB/=XUNDEF))THEN
!
  ALLOCATE(XSOC(ILU,NGROUND_LAYER))
!
  LSOCP=.TRUE.
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
#ifdef ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YSOC_TOP)//'.txt')
#endif
#ifdef FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YSOC_TOP)//'.fa')
#endif
#ifdef LFI
       CFILEIN_LFI = ADJUSTL(YSOC_TOP)
#endif
       CALL INIT_IO_SURF_n(YSOCFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(YSOCFILETYPE,'SOC_TOP',XSOC(:,1),IRESP) 
!
    CALL END_IO_SURF_n(YSOCFILETYPE)
!
!   Subsoil
!
    IF(YSOCFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for organic carbon input file with LIMP_SOC')
    ELSE
#ifdef ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YSOC_SUB)//'.txt')
#endif
#ifdef FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YSOC_SUB)//'.fa')
#endif
#ifdef LFI
       CFILEIN_LFI = ADJUSTL(YSOC_SUB)
#endif
       CALL INIT_IO_SURF_n(YSOCFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(YSOCFILETYPE,'SOC_SUB',XSOC(:,2),IRESP) 
!
    CALL END_IO_SURF_n(YSOCFILETYPE)
!
  ELSE
    CALL PGD_FIELD(HPROGRAM,'organic carbon','NAT',YSOC_TOP,YSOCFILETYPE,XUNIF_SOC_TOP,XSOC(:,1))
    CALL PGD_FIELD(HPROGRAM,'organic carbon','NAT',YSOC_SUB,YSOCFILETYPE,XUNIF_SOC_SUB,XSOC(:,2))
  ENDIF
!
  DO JLAYER=2,NGROUND_LAYER
    XSOC(:,JLAYER) = XSOC(:,2)
  END DO
!
ELSE
!
  LSOCP=.FALSE.
  ALLOCATE(XSOC(0,0))
!
ENDIF
!
!*    10.     Permafrost distribution
!             -----------------------
!
IF(LEN_TRIM(YPERM)/=0.OR.XUNIF_PERM/=XUNDEF)THEN
!
  ALLOCATE(XPERM(ILU))
!
  LPERM=.TRUE.
!
  IF(LIMP_PERM)THEN
!
    IF(YPERMFILETYPE=='NETCDF')THEN
       CALL ABOR1_SFX('Use another format than netcdf for permafrost input file with LIMP_PERM')
    ELSE
#ifdef ASC
       CFILEIN     = ADJUSTL(ADJUSTR(YPERM)//'.txt')
#endif
#ifdef FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(YPERM)//'.fa')
#endif
#ifdef LFI
       CFILEIN_LFI = ADJUSTL(YPERM)
#endif
       CALL INIT_IO_SURF_n(YPERMFILETYPE,'NATURE','ISBA  ','READ ')
    ENDIF     
!   
    CALL READ_SURF(YPERMFILETYPE,'PERM',XPERM(:),IRESP) 
!
    CALL END_IO_SURF_n(YPERMFILETYPE)
  ELSE
    CALL PGD_FIELD(HPROGRAM,'permafrost','NAT',YPERM,YPERMFILETYPE,XUNIF_PERM,XPERM(:))
  ENDIF
!
ELSE
!
  LPERM=.FALSE.  
  ALLOCATE(XPERM(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    11.  pH and fertlisation data
!             --------------------------
!
IF((LEN_TRIM(YPHFILETYPE)/=0.OR.XUNIF_PH/=XUNDEF) .AND. (LEN_TRIM(YFERTFILETYPE)/=0.OR.XUNIF_FERT/=XUNDEF)) THEN
  !
  ALLOCATE(XPH(ILU))
  ALLOCATE(XFERT(ILU))
  !
  LNOF = .TRUE.
  !
  CALL PGD_FIELD(HPROGRAM,'pH value','NAT',YPH,YPHFILETYPE,XUNIF_PH,XPH(:))
  CALL PGD_FIELD(HPROGRAM,'fertilisation','NAT',YFERT,YFERTFILETYPE,XUNIF_FERT,XFERT(:))
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    12.      Subgrid runoff 
!             --------------
!
ALLOCATE(XRUNOFFB(ILU))
 CALL PGD_FIELD                                                                              &
       (HPROGRAM,'subgrid runoff','NAT',YRUNOFFB,YRUNOFFBFILETYPE,XUNIF_RUNOFFB,XRUNOFFB(:))  
!
!-------------------------------------------------------------------------------
!
!*    13.     Drainage coefficient
!             --------------------
!
ALLOCATE(XWDRAIN(ILU))
 CALL PGD_FIELD                                                                              &
       (HPROGRAM,'subgrid drainage','NAT',YWDRAIN,YWDRAINFILETYPE,XUNIF_WDRAIN,XWDRAIN(:))  
!
!-------------------------------------------------------------------------------
!
!*   14.      ISBA specific fields
!             --------------------
!
LECOCLIMAP = OECOCLIMAP
!
 CALL PGD_ISBA_PAR(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*   15.      TOPODYN fields
!             --------------
!
 CALL PGD_TOPD(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*   16.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (LECOCLIMAP) THEN
  ALLOCATE(XDG(ILU,NGROUND_LAYER,NPATCH))
  IF (CISBA=='DIF') THEN
    ALLOCATE(NWG_LAYER(ILU,NPATCH))
  ELSE
    ALLOCATE(NWG_LAYER(0,0))
  END IF
  CALL CONVERT_COVER_ISBA(CISBA,NUNDEF,XCOVER,'   ','NAT',PSOILGRID=XSOILGRID,PDG=XDG,KWG_LAYER=NWG_LAYER)
END IF
!
!-------------------------------------------------------------------------------
!
!*   17.     Prints of cover parameters in a tex file
!            ----------------------------------------
!
IF (OECOCLIMAP) THEN
  CALL WRITE_COVER_TEX_ISBA    (NPATCH,NGROUND_LAYER,CISBA)
  CALL WRITE_COVER_TEX_ISBA_PAR(NPATCH,NGROUND_LAYER,CISBA,CPHOTO,XSOILGRID)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_ISBA
