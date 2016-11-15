!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_COVERS_AND_AV_PGD_1D_ON_LAYERS(HFILEPGDTYPE,HRECFM,DTCO,KLU,KDATA_LAYER,PFIELD2D,&
                                                     PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
!     ################################################################
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
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
!!    M.Moge        CNRS - LA
!!     inspired from AV_PGD_1D
!!
!!    MODIFICATION
!!    ------------
!
!
!!    Original    06/05/2015
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_BLD_HEIGHT 
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_DATA_COVER_PAR, ONLY : XCDREF, JPCOVER,NVT_TRBD,NVT_TEBE,NVT_TEBD ,NVT_BOBD,&
                                NVT_TENE,NVT_BONE,NVT_BOND,NVT_TRBE,NVT_SHRB
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_READ_SURF
#ifdef SFX_MNH
USE MODI_READ_SURFX2COV_1COV_MNH
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
CHARACTER(LEN=LEN_HREC),   INTENT(IN) :: HRECFM         ! Name of the article to be read
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
INTEGER,   INTENT(IN) :: KLU                      ! number of points
INTEGER,   INTENT(IN)  :: KDATA_LAYER  ! number of layers
REAL, DIMENSION(KLU,KDATA_LAYER),     INTENT(OUT) :: PFIELD2D  ! secondary field to construct
REAL, DIMENSION(JPCOVER,KDATA_LAYER),     INTENT(IN)  :: PDATA   ! secondary field value for each class
CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field is defined
CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(KLU),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
INTEGER :: JLAYER  ! loop on layers
!
REAL, DIMENSION(KLU) :: ZWORK, ZDZ
REAL                            :: ZWEIGHT
REAL, DIMENSION(KLU) :: ZCOVER_WEIGHT
REAL                            :: ZDATA_COVER
REAL, DIMENSION(KLU) :: ZSUM_COVER_WEIGHT
REAL, DIMENSION(KLU) :: ZWEIGHT_MAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
LOGICAL, DIMENSION(JPCOVER)          :: GCOVER ! flag to read the covers
REAL,    DIMENSION(KLU)		 :: ZCOVER ! cover fractions
CHARACTER(LEN=100) :: YCOMMENT
INTEGER           :: IRESP          ! reading return code
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_COVERS_AND_AV_PGD_1D_ON_LAYERS',0,ZHOOK_HANDLE)
!
!*    0.3 Initializations
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD2D(:,:)=XUNDEF
!
!
!* depths are deduced from the cover types
!* reading of the cover to obtain the thickness of layers
CALL READ_SURF(HFILEPGDTYPE,HRECFM,GCOVER(:),IRESP,HDIR='-')
YRECFM='COVER'
#ifdef SFX_MNH
!
! Loop on layers
DO JLAYER=1,KDATA_LAYER
  ZWORK(:)=0.
  ZWEIGHT_MAX(:)=0.
  ZSUM_COVER_WEIGHT(:)=0.
  ! loop on covers
  DO JCOVER=1,JPCOVER
    !
    !*    1. depths are deduced from the cover types
    !        reading of the cover to obtain the thickness of layers
    !
    IF ( GCOVER( JCOVER ) ) THEN
      CALL READ_SURFX2COV_1COV_MNH(YRECFM,KLU,JCOVER,ZCOVER(:),IRESP,YCOMMENT,'A')
    ELSE
      ZCOVER(:) = 0.
    ENDIF
    !
    !*    2. averaging
    !
    ! 2.1. Selection of the weighting function
    SELECT CASE (HSFTYPE)
      CASE('ALL')
        ZWEIGHT=1.
      CASE('NAT')
        ZWEIGHT=DTCO%XDATA_NATURE(JCOVER)
      CASE('GRD')
        ZWEIGHT=DTCO%XDATA_TOWN (JCOVER) * DTCO%XDATA_GARDEN(JCOVER)
      CASE('TWN')
        ZWEIGHT=DTCO%XDATA_TOWN  (JCOVER)
      CASE('WAT')
        ZWEIGHT=DTCO%XDATA_WATER (JCOVER)
      CASE('SEA')
        ZWEIGHT=DTCO%XDATA_SEA   (JCOVER)
      CASE('BLD')
        ZWEIGHT=DTCO%XDATA_TOWN  (JCOVER) *        DTCO%XDATA_BLD(JCOVER)
      CASE('BLV')  !* building Volume
        ZWEIGHT=DTCO%XDATA_TOWN  (JCOVER) *        DTCO%XDATA_BLD(JCOVER) * XDATA_BLD_HEIGHT(JCOVER)
      CASE('STR')
        ZWEIGHT=DTCO%XDATA_TOWN  (JCOVER) * ( 1. - DTCO%XDATA_BLD(JCOVER) )
      CASE('TRE')
        ZWEIGHT=DTCO%XDATA_NATURE(JCOVER) * (  DTCO%XDATA_VEGTYPE(JCOVER,NVT_TRBD) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TEBE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TEBD) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BOBD) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_SHRB) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TRBE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TENE) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BONE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BOND) )  
      CASE('GRT')
        ZWEIGHT=DTCO%XDATA_TOWN(JCOVER) * DTCO%XDATA_GARDEN(JCOVER) * ( DTCO%XDATA_VEGTYPE(JCOVER,NVT_TRBD) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TEBE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TEBD) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BOBD) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_SHRB) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TRBE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_TENE) &
                + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BONE) + DTCO%XDATA_VEGTYPE(JCOVER,NVT_BOND) )  
      CASE DEFAULT
        CALL ABOR1_SFX('AV_PGD_1D: WEIGHTING FUNCTION NOT ALLOWED '//HSFTYPE)
      END SELECT
    ! 2.2. Averaging
    ZCOVER_WEIGHT(:) = ZCOVER(:) * ZWEIGHT
    ZSUM_COVER_WEIGHT(:) = ZSUM_COVER_WEIGHT(:) + ZCOVER_WEIGHT(:)
    ZDATA_COVER = PDATA(JCOVER,JLAYER)
    SELECT CASE (HATYPE)
    CASE ('ARI')
      ZWORK(:) = ZWORK(:) + ZDATA_COVER * ZCOVER_WEIGHT(:) 
    CASE('INV' )
      ZWORK (:)= ZWORK(:) + 1./ZDATA_COVER * ZCOVER_WEIGHT(:)
    CASE('CDN')
      ZWORK (:)= ZWORK(:) + 1./(LOG(ZDZ(:)/ZDATA_COVER))**2 * ZCOVER_WEIGHT(:)
    CASE('MAJ' )
      WHERE(ZCOVER_WEIGHT(:)>ZWEIGHT_MAX(:))
        ZWEIGHT_MAX(:) = ZCOVER_WEIGHT(:)
        ZWORK      (:) = ZDATA_COVER
      END WHERE
    CASE DEFAULT
      CALL ABOR1_SFX('AV_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
    END SELECT
  ! 
  END DO ! DO JCOVER=1,JPCOVER
  !     
  ! 2.3. End of Averaging
  SELECT CASE (HATYPE)
  CASE ('ARI')
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD2D(:,JLAYER) = ZWORK(:) / ZSUM_COVER_WEIGHT(:)
    END WHERE
  CASE('INV' )
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD2D(:,JLAYER) = ZSUM_COVER_WEIGHT(:) / ZWORK(:)
    END WHERE
  CASE('CDN')
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD2D(:,JLAYER) = ZDZ(:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:)/ZWORK(:)) )
    END WHERE
  CASE('MAJ' )
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD2D(:,JLAYER) = ZWORK(:)
    END WHERE
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
  END SELECT
!
END DO !DO JLAYER=1,KDATA_LAYER
#endif
!
IF (LHOOK) CALL DR_HOOK('READ_COVERS_AND_AV_PGD_1D_ON_LAYERS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_COVERS_AND_AV_PGD_1D_ON_LAYERS
