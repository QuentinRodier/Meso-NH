!     ##################
      MODULE MODI_AV_PGD
!     ##################
INTERFACE AV_PGD
!
      SUBROUTINE AV_PGD(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
      
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD
!
      SUBROUTINE AV_PATCH_PGD(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
      
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD
!
      SUBROUTINE AV_PGD_1D(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
      
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_1D
!
      SUBROUTINE AV_PATCH_PGD_1D(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
      
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD_1D
!
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,KDECADE)
      
!
USE MODD_TYPE_DATE_SURF
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN) :: TDATA  ! secondary field to construct for each patch
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE MAJOR_PATCH_PGD_1D
!

!
END INTERFACE
END MODULE MODI_AV_PGD
!
!
!     ################################################################
      SUBROUTINE AV_PGD_1D(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PGD* average a secondary physiographic variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!
!     F.Solmon patch modif: remove the case 'veg' as veg is defined for patches 
!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_BLD_HEIGHT 
USE MODD_DATA_COVER_n,   ONLY : XDATA_NATURE, XDATA_TOWN, XDATA_BLD, XDATA_GARDEN, &
                                XDATA_SEA, XDATA_WATER, XDATA_VEGTYPE
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, XCDREF
!
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
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZWORK, ZDZ
REAL                            :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZCOVER_WEIGHT
REAL                            :: ZDATA
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZSUM_COVER_WEIGHT
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZWEIGHT_MAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
!
ZWORK(:)=0.
ZWEIGHT_MAX(:)=0.
ZSUM_COVER_WEIGHT(:)=0.
!-------------------------------------------------------------------------------
DO JCOVER=1,ICOVER
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)
       CASE('ALL')
         ZWEIGHT=1.

       CASE('NAT')
         ZWEIGHT=XDATA_NATURE(JCOVER)

       CASE('GRD')
         ZWEIGHT=XDATA_TOWN (JCOVER) * XDATA_GARDEN(JCOVER)

       CASE('TWN')
         ZWEIGHT=XDATA_TOWN  (JCOVER)

       CASE('WAT')
         ZWEIGHT=XDATA_WATER (JCOVER)

       CASE('SEA')
         ZWEIGHT=XDATA_SEA   (JCOVER)

       CASE('BLD')
         ZWEIGHT=XDATA_TOWN  (JCOVER) *        XDATA_BLD(JCOVER)

       CASE('BLV')  !* building Volume
         ZWEIGHT=XDATA_TOWN  (JCOVER) *        XDATA_BLD(JCOVER) &
                                      * XDATA_BLD_HEIGHT(JCOVER)

       CASE('STR')
         ZWEIGHT=XDATA_TOWN  (JCOVER) * ( 1. - XDATA_BLD(JCOVER) )

       CASE('TRE')
         PFIELD(:)=0.
         ZWEIGHT=XDATA_NATURE(JCOVER) * (  XDATA_VEGTYPE(JCOVER,NVT_TREE) &
                                           + XDATA_VEGTYPE(JCOVER,NVT_EVER) &
                                           + XDATA_VEGTYPE(JCOVER,NVT_CONI) )  

       CASE('GRT')
         PFIELD(:)=0.
         ZWEIGHT=XDATA_TOWN(JCOVER) * XDATA_GARDEN(JCOVER) &
                         * (  XDATA_VEGTYPE(JCOVER,NVT_TREE) &
                            + XDATA_VEGTYPE(JCOVER,NVT_EVER) &
                            + XDATA_VEGTYPE(JCOVER,NVT_CONI) )  

       CASE DEFAULT
         CALL ABOR1_SFX('AV_PGD_1D: WEIGHTING FUNCTION NOT ALLOWED '//HSFTYPE)
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
  ZCOVER_WEIGHT(:) = PCOVER(:,JCOVER) * ZWEIGHT
!
  ZSUM_COVER_WEIGHT(:) = ZSUM_COVER_WEIGHT(:) + ZCOVER_WEIGHT(:)
!
  ZDATA = PDATA(JCOVER)
!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.4    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    ZWORK(:) = ZWORK(:) + ZDATA * ZCOVER_WEIGHT(:) 
!
!-------------------------------------------------------------------------------
!
!*    3.5    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    ZWORK (:)= ZWORK(:) + 1./ZDATA * ZCOVER_WEIGHT(:)
!
!-------------------------------------------------------------------------------!
!
!*    3.6    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    ZWORK (:)= ZWORK(:) + 1./(LOG(ZDZ(:)/ZDATA))**2 * ZCOVER_WEIGHT(:)
!
!-------------------------------------------------------------------------------
!
!*    3.7    Majoritary averaging
!            --------------------
!
  CASE('MAJ' )
!
    WHERE(ZCOVER_WEIGHT(:)>ZWEIGHT_MAX(:))
      ZWEIGHT_MAX(:) = ZCOVER_WEIGHT(:)
      ZWORK      (:) = ZDATA
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
!
  END SELECT
!
END DO
!
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:) / ZSUM_COVER_WEIGHT(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZSUM_COVER_WEIGHT(:) / ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZDZ(:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:)/ZWORK(:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.4    Majoritary averaging
!            --------------------
!
  CASE('MAJ' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_1D
!
!
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PATCH_PGD* average for each surface patch a secondary physiographic 
!!                    variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
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
!!    F.Solmon /V. Masson       
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_VEG, XDATA_LAI  
USE MODD_DATA_COVER_n,   ONLY : XDATA_NATURE, XDATA_TOWN, XDATA_BLD, XDATA_GARDEN, &
                                XDATA_SEA, XDATA_WATER, XDATA_VEGTYPE
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, NVEGTYPE, XCDREF
!
USE MODI_VEGTYPE_TO_PATCH 
!
!
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
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEGTYPE! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JPATCH  ! PATCH index
INTEGER :: JJ, JI
!

!
REAL, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)         :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),NVEGTYPE)         :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZSUM_COVER_WEIGHT_PATCH
REAL, DIMENSION(NVEGTYPE)                        :: ZDATA
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZDZ
!
INTEGER, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))  :: NMASK
INTEGER, DIMENSION(SIZE(PFIELD,2)) :: JCOUNT
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
IPATCH=SIZE(PFIELD,2)
!
!
!
IF (PRESENT(PDZ)) THEN
  DO JPATCH=1,IPATCH
      ZDZ(:,JPATCH)=PDZ(:)
  END DO
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:) = 0.
ZWEIGHT(:,:) = 0.0
ZSUM_COVER_WEIGHT_PATCH(:,:) = 0.
!
DO JVEGTYPE=1,NVEGTYPE
  PATCH_LIST(JVEGTYPE) = VEGTYPE_TO_PATCH (JVEGTYPE, IPATCH)
ENDDO

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
SELECT CASE (HSFTYPE)

   CASE('NAT')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=XDATA_NATURE(:)*XDATA_VEGTYPE(:,JVEGTYPE)
     END DO

   CASE('GRD')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=XDATA_TOWN(:)*XDATA_GARDEN(:)*XDATA_VEGTYPE(:,JVEGTYPE)
     END DO

   CASE('VEG')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=XDATA_NATURE(:)*XDATA_VEGTYPE(:,JVEGTYPE)*&
                           XDATA_VEG(:,KDECADE,JVEGTYPE)  
     END DO

   CASE('BAR')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=XDATA_NATURE(:)*XDATA_VEGTYPE(:,JVEGTYPE)*&
                           (1.-XDATA_VEG(:,KDECADE,JVEGTYPE))  
     END DO
     
   CASE('GRV')
     DO JVEGTYPE=1,NVEGTYPE  
       ZWEIGHT(:,JVEGTYPE)=XDATA_TOWN(:)*XDATA_GARDEN(:)*XDATA_VEGTYPE(:,JVEGTYPE)* &
                             XDATA_VEG(:,KDECADE,JVEGTYPE)  
     END DO

   CASE('GRB')
     DO JVEGTYPE=1,NVEGTYPE  
       ZWEIGHT(:,JVEGTYPE)=XDATA_TOWN(:)*XDATA_GARDEN(:)*XDATA_VEGTYPE(:,JVEGTYPE)* &
                             (1.-XDATA_VEG(:,KDECADE,JVEGTYPE))  
     END DO
     
   CASE('DVG') ! for diffusion scheme only
     DO JVEGTYPE=1,NVEGTYPE
       WHERE ( SUM(XDATA_LAI(:,:,JVEGTYPE),2) .GT. 0.0) &
         ZWEIGHT(:,JVEGTYPE)=XDATA_NATURE(:)*XDATA_VEGTYPE(:,JVEGTYPE)
     END DO     

   CASE('GDV') ! for diffusion scheme only
     DO JVEGTYPE=1,NVEGTYPE
       WHERE ( SUM(XDATA_LAI(:,:,JVEGTYPE),2) .GT. 0.0) &
         ZWEIGHT(:,JVEGTYPE)=XDATA_TOWN(:)*XDATA_GARDEN(:)*XDATA_VEGTYPE(:,JVEGTYPE)
     END DO     

   CASE('LAI')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=XDATA_NATURE(:)*XDATA_VEGTYPE(:,JVEGTYPE)*&
                           XDATA_LAI(:,KDECADE,JVEGTYPE)  
     END DO
     
   CASE('GRL')
     DO JVEGTYPE=1,NVEGTYPE  
       ZWEIGHT(:,JVEGTYPE)=XDATA_TOWN(:)*XDATA_GARDEN(:)*XDATA_VEGTYPE(:,JVEGTYPE)* &
                             XDATA_LAI(:,KDECADE,JVEGTYPE)  
     END DO

    CASE('TRE')
      ZWEIGHT(:,:)=0.
      WHERE (XDATA_VEGTYPE(:,NVT_TREE)>0.)
        ZWEIGHT(:,NVT_TREE)=XDATA_NATURE(:) * XDATA_VEGTYPE(:,NVT_TREE)
      ENDWHERE
      WHERE (XDATA_VEGTYPE(:,NVT_CONI)>0.)
        ZWEIGHT(:,NVT_CONI)=XDATA_NATURE(:) * XDATA_VEGTYPE(:,NVT_CONI)
      ENDWHERE
      WHERE (XDATA_VEGTYPE(:,NVT_EVER)>0.)
        ZWEIGHT(:,NVT_EVER)=XDATA_NATURE(:) * XDATA_VEGTYPE(:,NVT_EVER)
      ENDWHERE

    CASE('GRT')
      ZWEIGHT(:,:)=0.
      WHERE (XDATA_VEGTYPE(:,NVT_TREE)>0.)
        ZWEIGHT(:,NVT_TREE)=XDATA_TOWN(:)*XDATA_GARDEN(:) * XDATA_VEGTYPE(:,NVT_TREE)
      ENDWHERE
      WHERE (XDATA_VEGTYPE(:,NVT_CONI)>0.)
        ZWEIGHT(:,NVT_CONI)=XDATA_TOWN(:)*XDATA_GARDEN(:) * XDATA_VEGTYPE(:,NVT_CONI)
      ENDWHERE
      WHERE (XDATA_VEGTYPE(:,NVT_EVER)>0.)
        ZWEIGHT(:,NVT_EVER)=XDATA_TOWN(:)*XDATA_GARDEN(:) * XDATA_VEGTYPE(:,NVT_EVER)
      ENDWHERE

    CASE DEFAULT
       CALL ABOR1_SFX('AV_PATCH_PGD_1D: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
END SELECT
!
!-------------------------------------------------------------------------------
DO JCOVER=1,ICOVER
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
! 

  ZDATA(:) = PDATA(JCOVER,:)

!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.3    Arithmetic averaging
!            --------------------
!
    CASE ('ARI')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH= PATCH_LIST(JVEGTYPE)
        DO JJ=1,SIZE(PCOVER,1) 
          ZCOVER_WEIGHT(JJ,JVEGTYPE) =  PCOVER(JJ,JCOVER) * ZWEIGHT(JCOVER,JVEGTYPE)      
          ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH) = ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH) + ZCOVER_WEIGHT(JJ,JVEGTYPE)
          ZWORK(JJ,JPATCH) =  ZWORK(JJ,JPATCH) + ZDATA(JVEGTYPE)  * ZCOVER_WEIGHT(JJ,JVEGTYPE)
        ENDDO
      END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
    CASE('INV' )
!
     DO JVEGTYPE=1,NVEGTYPE 
       JPATCH=PATCH_LIST(JVEGTYPE)
       DO JJ=1,SIZE(PCOVER,1) 
         ZCOVER_WEIGHT(JJ,JVEGTYPE) =  PCOVER(JJ,JCOVER) * ZWEIGHT(JCOVER,JVEGTYPE)      
         ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH) =  ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH)+ ZCOVER_WEIGHT(JJ,JVEGTYPE)
         ZWORK(JJ,JPATCH)= ZWORK(JJ,JPATCH) + 1./ ZDATA(JVEGTYPE) * ZCOVER_WEIGHT(JJ,JVEGTYPE)
       ENDDO
     END DO    
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
    CASE('CDN')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH=PATCH_LIST(JVEGTYPE) 
        DO JJ=1,SIZE(PCOVER,1) 
          ZCOVER_WEIGHT(JJ,JVEGTYPE) =  PCOVER(JJ,JCOVER) * ZWEIGHT(JCOVER,JVEGTYPE)      
          ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH) =  ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH)+ ZCOVER_WEIGHT(JJ,JVEGTYPE)
          ZWORK(JJ,JPATCH)= ZWORK(JJ,JPATCH) + 1./(LOG(ZDZ(JJ,JPATCH)/ ZDATA(JVEGTYPE)))**2    &
                              * ZCOVER_WEIGHT(JJ,JVEGTYPE)  
        ENDDO
      END DO   
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
NMASK(:,:)=0
JCOUNT(:)=0
DO JPATCH=1,IPATCH
  DO JJ=1,SIZE(PCOVER,1)
    IF ( ZSUM_COVER_WEIGHT_PATCH(JJ,JPATCH) >0.) THEN
      JCOUNT(JPATCH)=JCOUNT(JPATCH)+1
      NMASK(JCOUNT(JPATCH),JPATCH)=JJ
    ENDIF
  ENDDO
ENDDO
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!   
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
          JI = NMASK(JJ,JPATCH)
          PFIELD(JI,JPATCH) =  ZWORK(JI,JPATCH) / ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH)
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
        JI = NMASK(JJ,JPATCH)
        PFIELD(JI,JPATCH) = ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH) / ZWORK(JI,JPATCH)
      ENDDO
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
        JI=NMASK(JJ,JPATCH)
        PFIELD(JI,JPATCH) = ZDZ(JI,JPATCH) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH)/ZWORK(JI,JPATCH)) )
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PATCH_PGD_1D
!
!     ################################################################
      SUBROUTINE AV_PGD(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PGD* average a secondary physiographic variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!
!     F.Solmon patch modif: remove the case 'veg' as veg is defined for patches 
!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_n,   ONLY : XDATA_NATURE, XDATA_TOWN, XDATA_BLD, XDATA_GARDEN, &
                                XDATA_SEA, XDATA_WATER, XDATA_VEGTYPE
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, XCDREF
!
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
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZWORK, ZDZ
REAL                                           :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZCOVER_WEIGHT
REAL                                           :: ZDATA
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,3)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:,:)=PDZ(:,:)
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:)=0.
ZSUM_COVER_WEIGHT(:,:)=0.
!-------------------------------------------------------------------------------
DO JCOVER=1,ICOVER
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)
       CASE('ALL')
         ZWEIGHT=1.

       CASE('NAT')
         ZWEIGHT=XDATA_NATURE(JCOVER)

       CASE('GRD')
         ZWEIGHT=XDATA_TOWN (JCOVER) * XDATA_GARDEN(JCOVER)

       CASE('TWN')
         ZWEIGHT=XDATA_TOWN  (JCOVER)

       CASE('WAT')
         ZWEIGHT=XDATA_WATER (JCOVER)

       CASE('SEA')
         ZWEIGHT=XDATA_SEA   (JCOVER)

       CASE('BLD')
         ZWEIGHT=XDATA_TOWN  (JCOVER) *        XDATA_BLD(JCOVER)

       CASE('STR')
         ZWEIGHT=XDATA_TOWN  (JCOVER) * ( 1. - XDATA_BLD(JCOVER) )

       CASE('TRE')
         PFIELD(:,:)=0.
         ZWEIGHT=XDATA_NATURE(JCOVER) * (  XDATA_VEGTYPE(JCOVER,NVT_TREE) &
                                           + XDATA_VEGTYPE(JCOVER,NVT_EVER) &
                                           + XDATA_VEGTYPE(JCOVER,NVT_CONI) )  

       CASE('GRT')
         PFIELD(:,:)=0.
         ZWEIGHT=XDATA_TOWN (JCOVER) * XDATA_GARDEN(JCOVER)  &
                          * (  XDATA_VEGTYPE(JCOVER,NVT_TREE)  &
                             + XDATA_VEGTYPE(JCOVER,NVT_EVER)  &
                             + XDATA_VEGTYPE(JCOVER,NVT_CONI) )  

       CASE DEFAULT
         CALL ABOR1_SFX('AV_PGD: WEIGHTING FUNCTION NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
  ZCOVER_WEIGHT(:,:) = PCOVER(:,:,JCOVER) * ZWEIGHT
!
  ZSUM_COVER_WEIGHT(:,:) = ZSUM_COVER_WEIGHT(:,:) + ZCOVER_WEIGHT(:,:)
!
  ZDATA = PDATA(JCOVER)
!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.4    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    ZWORK(:,:) = ZWORK(:,:) + ZDATA * ZCOVER_WEIGHT(:,:) 
!
!-------------------------------------------------------------------------------
!
!*    3.5    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    ZWORK (:,:)= ZWORK(:,:) + 1./ZDATA * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------!
!
!*    3.6    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    ZWORK (:,:)= ZWORK(:,:) + 1./(LOG(ZDZ(:,:)/ZDATA))**2 * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZWORK(:,:) / ZSUM_COVER_WEIGHT(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZSUM_COVER_WEIGHT(:,:) / ZWORK(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZDZ(:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:,:)/ZWORK(:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD
!
!
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD(PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PATCH_PGD* average for each surface patch a secondary physiographic 
!!                    variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
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
!!    F.Solmon /V. Masson       
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY :  XDATA_VEG, XDATA_LAI  
USE MODD_DATA_COVER_n,   ONLY : XDATA_NATURE, XDATA_TOWN, XDATA_BLD, XDATA_GARDEN, &
                                XDATA_SEA, XDATA_WATER, XDATA_VEGTYPE
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, NVEGTYPE, XCDREF
!
USE MODI_VEGTYPE_TO_PATCH 
!
!
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
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEGTYPE! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JPATCH  ! PATCH index
!
REAL, DIMENSION(NVEGTYPE)                                    :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),NVEGTYPE)      :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZCOVER_WEIGHT_PATCH
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZSUM_COVER_WEIGHT_PATCH
REAL, DIMENSION(NVEGTYPE)                                    :: ZDATA
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZDZ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,3)
IPATCH=SIZE(PFIELD,3)
!
!
!
IF (PRESENT(PDZ)) THEN
  DO JPATCH=1,IPATCH
    ZDZ(:,:,JPATCH)=PDZ(:,:)
  END DO
ELSE
  ZDZ(:,:,:)=XCDREF
END IF
!
PFIELD(:,:,:)=XUNDEF
!
ZWORK(:,:,:)=0.
ZSUM_COVER_WEIGHT_PATCH(:,:,:)=0.
!
!-------------------------------------------------------------------------------
DO JCOVER=1,ICOVER
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)

     CASE('NAT')
       DO JVEGTYPE=1,NVEGTYPE
         ZWEIGHT(JVEGTYPE)=XDATA_NATURE(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)
       END DO

     CASE('GRD')
       DO JVEGTYPE=1,NVEGTYPE
         ZWEIGHT(JVEGTYPE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)
       END DO

     CASE('VEG')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_NATURE(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             XDATA_VEG(JCOVER,KDECADE,JVEGTYPE)  
       END DO

     CASE('BAR')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_NATURE(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             (1.-XDATA_VEG(JCOVER,KDECADE,JVEGTYPE)) 
       END DO

     CASE('GRV')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             XDATA_VEG(JCOVER,KDECADE,JVEGTYPE)  
       END DO

     CASE('GRB')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             (1.-XDATA_VEG(JCOVER,KDECADE,JVEGTYPE))
       ENDDO 
       
     CASE('DVG') ! average only on vegetated area
       ZWEIGHT(:) = 0.0
       DO JVEGTYPE=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JCOVER,:,JVEGTYPE)).GT.0.) &
           ZWEIGHT(JVEGTYPE)=XDATA_NATURE(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)
       END DO     

     CASE('GDV') ! average only on vegetated area
       ZWEIGHT(:) = 0.0             
       DO JVEGTYPE=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JCOVER,:,JVEGTYPE)).GT.0.) &
           ZWEIGHT(JVEGTYPE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)       
       END DO     

     CASE('LAI')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_NATURE(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             XDATA_LAI(JCOVER,KDECADE,JVEGTYPE)  
       END DO

     CASE('GRL')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER)*XDATA_VEGTYPE(JCOVER,JVEGTYPE)*&
                             XDATA_LAI(JCOVER,KDECADE,JVEGTYPE)  
       END DO

      CASE('TRE')
        ZWEIGHT(:)=0.
        IF (XDATA_VEGTYPE(JCOVER,NVT_TREE)>0.) THEN
          ZWEIGHT(NVT_TREE)=XDATA_NATURE(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_TREE)
        END IF
        IF (XDATA_VEGTYPE(JCOVER,NVT_CONI)>0.) THEN
          ZWEIGHT(NVT_CONI)=XDATA_NATURE(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_CONI)
        END IF
        IF (XDATA_VEGTYPE(JCOVER,NVT_EVER)>0.) THEN
          ZWEIGHT(NVT_EVER)=XDATA_NATURE(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_EVER)
        END IF

      CASE('GRT')
        ZWEIGHT(:)=0.
        IF (XDATA_VEGTYPE(JCOVER,NVT_TREE)>0.) THEN
          ZWEIGHT(NVT_TREE)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_TREE)
        END IF
        IF (XDATA_VEGTYPE(JCOVER,NVT_CONI)>0.) THEN
          ZWEIGHT(NVT_CONI)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_CONI)
        END IF
        IF (XDATA_VEGTYPE(JCOVER,NVT_EVER)>0.) THEN
          ZWEIGHT(NVT_EVER)=XDATA_TOWN(JCOVER)*XDATA_GARDEN(JCOVER) * XDATA_VEGTYPE(JCOVER,NVT_EVER)
        END IF

      CASE DEFAULT
         CALL ABOR1_SFX('AV_PATCH_PGD: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
! 
  ZCOVER_WEIGHT(:,:,:)=0. 
  ZCOVER_WEIGHT_PATCH(:,:,:)=0.
 
  DO JVEGTYPE=1,NVEGTYPE
     ZCOVER_WEIGHT(:,:,JVEGTYPE) =  ZCOVER_WEIGHT(:,:,JVEGTYPE) +&
                                      PCOVER(:,:,JCOVER) * ZWEIGHT(JVEGTYPE)    

     JPATCH= VEGTYPE_TO_PATCH (JVEGTYPE, IPATCH)
    
     ZCOVER_WEIGHT_PATCH(:,:,JPATCH) =  ZCOVER_WEIGHT_PATCH(:,:,JPATCH)+   &
                                          PCOVER(:,:,JCOVER) * ZWEIGHT(JVEGTYPE)  
  END DO 

!
  ZSUM_COVER_WEIGHT_PATCH(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) + ZCOVER_WEIGHT_PATCH(:,:,:)


  ZDATA(:) = PDATA(JCOVER,:)

!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.3    Arithmetic averaging
!            --------------------
!
    CASE ('ARI')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH= VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
        ZWORK(:,:,JPATCH) =  ZWORK(:,:,JPATCH) + ZDATA(JVEGTYPE)          &
                                   * ZCOVER_WEIGHT(:,:,JVEGTYPE)  
      END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
    CASE('INV' )
!
     DO JVEGTYPE=1,NVEGTYPE 
       JPATCH=VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
       ZWORK(:,:,JPATCH)= ZWORK(:,:,JPATCH) + 1./ ZDATA(JVEGTYPE)     &
                                 * ZCOVER_WEIGHT(:,:,JVEGTYPE)  
     END DO    
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
    CASE('CDN')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH=VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
        ZWORK(:,:,JPATCH)= ZWORK(:,:,JPATCH) + 1./(LOG(ZDZ(:,:,JPATCH)/ ZDATA(JVEGTYPE)))**2    &
                                * ZCOVER_WEIGHT(:,:,JVEGTYPE)  
      END DO   
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) =  ZWORK(:,:,:) / ZSUM_COVER_WEIGHT_PATCH(:,:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) / ZWORK(:,:,:)
    END WHERE
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZDZ(:,:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(:,:,:)/ZWORK(:,:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PATCH_PGD
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,KDECADE)
!     ################################################################
!
!!**** *MAJOR_PATCH_PGD* find the dominant date for each vegetation type
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
!!    P. LE MOIGNE
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2006
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_DATE_SURF
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, NVEGTYPE, XCDREF
!
USE MODI_VEGTYPE_TO_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: JVEGTYPE! loop on vegtype
!
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)      :: IDATA_DOY
INTEGER, DIMENSION(SIZE(PCOVER,1))               :: IDOY
REAL,    DIMENSION(365)                          :: ZCOUNT
INTEGER                                          :: JP, IMONTH, IDAY
INTEGER                                          :: IPATCH, JPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',0,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
IPATCH=SIZE(TFIELD,2)
!
TFIELD(:,:)%TDATE%YEAR  = NUNDEF
TFIELD(:,:)%TDATE%MONTH = NUNDEF
TFIELD(:,:)%TDATE%DAY   = NUNDEF
TFIELD(:,:)%TIME        = 0.
!
 CALL DATE2DOY(TDATA,IDATA_DOY)
!-------------------------------------------------------------------------------
DO JP = 1, SIZE(PCOVER,1)

  DO JPATCH=1,IPATCH
    !
    ZCOUNT(:) = 0.
    !
    DO JVEGTYPE=1,NVEGTYPE
      ! 
      IF(JPATCH==VEGTYPE_TO_PATCH(JVEGTYPE,IPATCH)) THEN
        !
        DO JCOVER=1,ICOVER
          !
          IF (IDATA_DOY(JCOVER,JVEGTYPE) /= NUNDEF) THEN
            !
            ZCOUNT(IDATA_DOY(JCOVER,JVEGTYPE)) = ZCOUNT(IDATA_DOY(JCOVER,JVEGTYPE)) + PCOVER(JP,JCOVER)
            !
          END IF
          !
        END DO
        !
      ENDIF
      !
      IDOY(JP) = MAXLOC(ZCOUNT,1)
      CALL DOY2DATE(IDOY(JP),IMONTH,IDAY)
      !
      TFIELD(JP,JPATCH)%TDATE%MONTH = IMONTH
      TFIELD(JP,JPATCH)%TDATE%DAY   = IDAY
      !
    END DO
    !
  END DO
  !
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
CONTAINS

SUBROUTINE DATE2DOY(TPDATA, KDOY)
TYPE (DATE_TIME), DIMENSION(SIZE(PCOVER,2),NVEGTYPE) :: TPDATA
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE) :: KDOY
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE) :: IMONTH, IDAY
INTEGER, PARAMETER, DIMENSION(12)     :: TAB=(/1,32,60,91,121,152,182,213,244,274,305,335/)
INTEGER :: JCOVER
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:DATE2DOY',0,ZHOOK_HANDLE)
IMONTH(:,:) = TPDATA(:,:)%TDATE%MONTH
IDAY(:,:)   = TPDATA(:,:)%TDATE%DAY
KDOY(:,:)   = NUNDEF

DO JCOVER = 1, SIZE(PCOVER,2)
   DO JVEGTYPE = 1, NVEGTYPE
      IF (IMONTH(JCOVER,JVEGTYPE)/=NUNDEF .AND. IDAY(JCOVER,JVEGTYPE) /= NUNDEF) THEN
         KDOY(JCOVER,JVEGTYPE) = TAB(IMONTH(JCOVER,JVEGTYPE)) + IDAY(JCOVER,JVEGTYPE) - 1
      ENDIF
   END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:DATE2DOY',1,ZHOOK_HANDLE)

END SUBROUTINE DATE2DOY

SUBROUTINE DOY2DATE(KDOY,KMONTH,KDAY)
INTEGER :: KDOY, KMONTH, KDAY
REAL    :: ZWORK(12)
INTEGER, PARAMETER, DIMENSION(12)     :: ZTAB=(/31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334.,365./)
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:DOY2DATE',0,ZHOOK_HANDLE)
KMONTH = NUNDEF
KDAY   = NUNDEF

ZWORK(1) = REAL(KDOY) / ZTAB(1)
DO J = 2, 12
   ZWORK(J) = REAL(KDOY) / ZTAB(J)
   IF ( INT(ZWORK(J))==0 .AND. INT(ZWORK(J-1))==1 ) THEN
      KMONTH = J
      KDAY   = KDOY - INT(ZTAB(J-1))
   ENDIF
END DO 
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:DOY2DATE',1,ZHOOK_HANDLE)

END SUBROUTINE DOY2DATE
!-------------------------------------------------------------------------------
!
END SUBROUTINE MAJOR_PATCH_PGD_1D
