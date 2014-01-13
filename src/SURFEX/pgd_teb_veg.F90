!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_VEG(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_VEG* monitor for averaging and interpolations of physiographic fields
!!                   for natural covers of TEB
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
!!    Original    03/2010
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,          ONLY : NL
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
USE MODD_TEB_n,             ONLY : XCOVER, LCOVER, XZS,                     &
                                   LECOCLIMAP, LGREENROOF, LHYDRO
USE MODD_TEB_VEG_n,         ONLY : NNBIOMASS,                               &
                                   CISBA, CPHOTO, CPEDOTF, LTR_ML
USE MODD_TEB_GARDEN_n,      ONLY : NGROUND_LAYER, XSOILGRID,                &
                                   XCLAY, XSAND, XRUNOFFB, XWDRAIN
USE MODD_TEB_GRID_n,        ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE, NDIM
USE MODD_DATA_TEB_GARDEN_n, ONLY : NTIME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_PGD_TEB_GARDEN_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_PGD_TEB_GREENROOF
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                  :: ILUOUT    ! output listing logical unit
INTEGER                  :: JLAYER    ! loop counter
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: YISBA            ! ISBA option
 CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo-transfert function for DIF
 CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil layer thickness for DIF
!
! Not used in TEB garden
!
 CHARACTER(LEN=28)        :: YSOC_TOP      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YSOC_SUB      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YPERM         ! file name for permafrost distribution
 CHARACTER(LEN=6)         :: YSOCFILETYPE  ! organic carbon data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE ! permafrost distribution data file type
REAL                     :: XUNIF_SOC_TOP ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_PERM    ! uniform permafrost distribution
LOGICAL                  :: LIMP_SOC      ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_PERM     ! Imposed maps of permafrost distribution
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_VEG',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    1.      Reading of namelist NAM_ISBA for general options of vegetation
!             --------------------------------------------------------------
!
NGROUND_LAYER = 0
CISBA         = '   '
CPEDOTF       = '   '
CPHOTO        = '   '
!
 CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                         &
                       YISBA, YPEDOTF, YPHOTO,  GTR_ML, ZRM_PATCH,              &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,             &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,             &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,         &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,   &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM,             &                     
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,               &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,     &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,        &
                       XUNIF_FERT                                               )  
!
NGROUND_LAYER = IGROUND_LAYER
CISBA         = YISBA
CPEDOTF       = YPEDOTF
CPHOTO        = YPHOTO
LTR_ML        = GTR_ML
!
!-------------------------------------------------------------------------------
!
!*    2.      Coherence of options
!             --------------------
!
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',CISBA,'2-L','3-L','DIF')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',CPEDOTF,'CH78','CO84')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',CPHOTO,'NON','AGS','LAI','AST','LST','NIT','NCB')
  !
  IF (CPHOTO=='NCB') THEN
    CPHOTO = 'NIT'
    WRITE(ILUOUT,*) '****************************************************************'
    WRITE(ILUOUT,*) '* FOR GARDENS, AGS OPTION HAS BEEN CHANGED FROM "NCB" TO "NIT" *'
    WRITE(ILUOUT,*) '****************************************************************'
  END IF
!
  SELECT CASE (CISBA)
    CASE ('2-L')
      NGROUND_LAYER = 2
      CPEDOTF       ='CH78'       
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'          
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('3-L')
      NGROUND_LAYER = 3
      CPEDOTF       ='CH78'         
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'        
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('DIF')
      IF(NGROUND_LAYER==NUNDEF)THEN
        IF(LECOCLIMAP)THEN
          NGROUND_LAYER=NOPTIMLAYER
        ELSE
          WRITE(ILUOUT,*) '****************************************'
          WRITE(ILUOUT,*) '* Number of ground layer not specified *'
          WRITE(ILUOUT,*) '****************************************'
          CALL ABOR1_SFX('PGD_TEB_GARDEN: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
        ENDIF
      ENDIF
! 
      ALLOCATE(XSOILGRID(NGROUND_LAYER))
      XSOILGRID(:)=XUNDEF
      XSOILGRID(:)=ZSOILGRID(1:NGROUND_LAYER) 
      IF(ALL(ZSOILGRID(:)==XUNDEF))THEN
        IF(LECOCLIMAP) XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
      ELSEIF(COUNT(XSOILGRID/=XUNDEF)/=NGROUND_LAYER)THEN
        WRITE(ILUOUT,*) '********************************************************'
        WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
        WRITE(ILUOUT,*) '********************************************************'
        CALL ABOR1_SFX('PGD_TEB_GARDEN: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA')            
      ENDIF
!
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* Option CISBA            = ',CISBA
      WRITE(ILUOUT,*) '* Pedo transfert function = ',CPEDOTF    
      WRITE(ILUOUT,*) '* Number of soil layers   = ',NGROUND_LAYER
      IF(LECOCLIMAP)THEN
        WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',XSOILGRID(1:NGROUND_LAYER)
      ENDIF
      WRITE(ILUOUT,*) '*****************************************' 

  END SELECT
!
  SELECT CASE (CPHOTO)
    CASE ('AGS','LAI','AST','LST')
      NNBIOMASS = 1
    CASE ('NIT')
      NNBIOMASS = 3
  END SELECT
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ',CPHOTO,'               *'
  WRITE(ILUOUT,*) '* the number of biomass pools is set to ', NNBIOMASS
  WRITE(ILUOUT,*) '*****************************************'
!
!-------------------------------------------------------------------------------
!
!*    3.      Sand fraction
!             -------------
!
ALLOCATE(XSAND(NDIM,NGROUND_LAYER))
!
IF(LIMP_SAND)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
!
 CALL PGD_FIELD(HPROGRAM,'sand fraction','TWN',YSAND,YSANDFILETYPE,XUNIF_SAND,XSAND(:,1))
ENDIF
!
DO JLAYER=1,NGROUND_LAYER
  XSAND(:,JLAYER) = XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    4.      Clay fraction
!             -------------
!
ALLOCATE(XCLAY(NDIM,NGROUND_LAYER))
!
IF(LIMP_CLAY)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
 CALL PGD_FIELD(HPROGRAM,'clay fraction','TWN',YCLAY,YCLAYFILETYPE,XUNIF_CLAY,XCLAY(:,1))
ENDIF
!
DO JLAYER=1,NGROUND_LAYER
  XCLAY(:,JLAYER) = XCLAY(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    5.      Subgrid runoff 
!             --------------
!
ALLOCATE(XRUNOFFB(NDIM))
 CALL PGD_FIELD                                                                              &
       (HPROGRAM,'subgrid runoff','TWN',YRUNOFFB,YRUNOFFBFILETYPE,XUNIF_RUNOFFB,XRUNOFFB(:))  
!
!-------------------------------------------------------------------------------
!
!*    6.      Drainage coefficient
!             --------------------
!
ALLOCATE(XWDRAIN(NDIM))
 CALL PGD_FIELD                                                                              &
       (HPROGRAM,'subgrid drainage','TWN',YWDRAIN,YWDRAINFILETYPE,XUNIF_WDRAIN,XWDRAIN(:))  
!
!-------------------------------------------------------------------------------
!
!*    7.      Interpolation of GARDEN physiographic fields
!             --------------------------------------------
!
NTIME = 12
 CALL PGD_TEB_GARDEN_PAR(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of greenroofs
!             ------------------
!
IF (LGREENROOF) CALL PGD_TEB_GREENROOF(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Case of urban hydrology
!             -----------------------
!
IF (LHYDRO) print*," CALL PGD_TEB_URBHYDRO(HPROGRAM,LECOCLIMAP)"
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_VEG
