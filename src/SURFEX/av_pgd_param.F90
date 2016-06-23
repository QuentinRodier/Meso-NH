!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE AV_PGD_PARAM (DTI, &
                               PFIELD,PVEGTYPE,PDATA,HSFTYPE,HATYPE,PDZ,KDECADE)
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
!!    R. Alkama   04/2012  add 6 new tree vegtype (9 instead 3)
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE,  &
                                NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, NVEGTYPE,  &
                                XCDREF

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
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE  ! fraction of each cover class
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
REAL, DIMENSION(SIZE(PFIELD,1),NVEGTYPE)  :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PFIELD,1),SIZE(PFIELD,2))   :: ZSUM_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PFIELD,1),SIZE(PFIELD,2))   :: ZWORK
REAL, DIMENSION(SIZE(PFIELD,1),SIZE(PFIELD,2))   :: ZDZ
!
INTEGER, DIMENSION(SIZE(PFIELD,1),SIZE(PFIELD,2))  :: NMASK
INTEGER, DIMENSION(SIZE(PFIELD,2)) :: JCOUNT
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
IPATCH=SIZE(PFIELD,2)
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
ZWORK(:,:)=0.
ZWEIGHT(:,:)=0.
ZSUM_WEIGHT_PATCH(:,:)=0.
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

   CASE('NAT','GRD')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=PVEGTYPE(:,JVEGTYPE)
     END DO

   CASE('VEG','GRV')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=PVEGTYPE(:,JVEGTYPE)*DTI%XPAR_VEG(:,KDECADE,JVEGTYPE)
     END DO

   CASE('BAR','GRB')
     DO JVEGTYPE=1,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=PVEGTYPE(:,JVEGTYPE)*(1.-DTI%XPAR_VEG(:,KDECADE,JVEGTYPE))
     END DO
     
    CASE('DVG','GDV') ! for diffusion scheme only, average only on vegetated area
     DO JVEGTYPE=1,NVEGTYPE
       WHERE ( SUM(DTI%XPAR_LAI(:,:,JVEGTYPE),2) .GT. 0.0) &
        ZWEIGHT(:,JVEGTYPE)=PVEGTYPE(:,JVEGTYPE)
     END DO

   CASE('LAI','GRL')
     DO JVEGTYPE=4,NVEGTYPE
       ZWEIGHT(:,JVEGTYPE)=PVEGTYPE(:,JVEGTYPE)*DTI%XPAR_LAI(:,KDECADE,JVEGTYPE)
     END DO

    CASE('TRE','GRT')
      ZWEIGHT(:,:)=0.
      WHERE (PVEGTYPE(:,NVT_TEBD)>0.)
        ZWEIGHT(:,NVT_TEBD)=PVEGTYPE(:,NVT_TEBD)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_BONE)>0.)
        ZWEIGHT(:,NVT_BONE)=PVEGTYPE(:,NVT_BONE)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_TRBE)>0.)
        ZWEIGHT(:,NVT_TRBE)=PVEGTYPE(:,NVT_TRBE)
      ENDWHERE

      WHERE (PVEGTYPE(:,NVT_TRBD)>0.)
        ZWEIGHT(:,NVT_TRBD)=PVEGTYPE(:,NVT_TRBD)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_TEBE)>0.)
        ZWEIGHT(:,NVT_TEBE)=PVEGTYPE(:,NVT_TEBE)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_TENE)>0.)
        ZWEIGHT(:,NVT_TENE)=PVEGTYPE(:,NVT_TENE)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_BOBD)>0.)
        ZWEIGHT(:,NVT_BOBD)=PVEGTYPE(:,NVT_BOBD)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_BOND)>0.)
        ZWEIGHT(:,NVT_BOND)=PVEGTYPE(:,NVT_BOND)
      ENDWHERE
      WHERE (PVEGTYPE(:,NVT_SHRB)>0.)
        ZWEIGHT(:,NVT_SHRB)=PVEGTYPE(:,NVT_SHRB)
      ENDWHERE

    CASE DEFAULT
       CALL ABOR1_SFX('AV_PGD_PARAM: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
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
      DO JJ=1,SIZE(PDATA,1)
        ZSUM_WEIGHT_PATCH(JJ,JPATCH) = ZSUM_WEIGHT_PATCH(JJ,JPATCH) + ZWEIGHT(JJ,JVEGTYPE)
        ZWORK(JJ,JPATCH) =  ZWORK(JJ,JPATCH) + PDATA(JJ,JVEGTYPE)  * ZWEIGHT(JJ,JVEGTYPE)
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
     DO JJ=1,SIZE(PDATA,1)
       ZSUM_WEIGHT_PATCH(JJ,JPATCH) = ZSUM_WEIGHT_PATCH(JJ,JPATCH)+ZWEIGHT(JJ,JVEGTYPE)
       IF (PDATA(JJ,JVEGTYPE).NE.0.) THEN
         ZWORK(JJ,JPATCH)= ZWORK(JJ,JPATCH) + 1./ PDATA(JJ,JVEGTYPE) * ZWEIGHT(JJ,JVEGTYPE)
       ENDIF
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
      DO JJ=1,SIZE(PDATA,1)
        ZSUM_WEIGHT_PATCH(JJ,JPATCH) =  ZSUM_WEIGHT_PATCH(JJ,JPATCH)+ ZWEIGHT(JJ,JVEGTYPE)
        IF (PDATA(JJ,JVEGTYPE).NE.0.) THEN
          ZWORK(JJ,JPATCH)= ZWORK(JJ,JPATCH) + 1./(LOG(ZDZ(JJ,JPATCH)/ PDATA(JJ,JVEGTYPE)))**2    &
                            * ZWEIGHT(JJ,JVEGTYPE)
        ENDIF
      ENDDO
    END DO   
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM: (1) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
NMASK(:,:)=0
JCOUNT(:)=0
DO JPATCH=1,IPATCH
  DO JJ=1,SIZE(ZWORK,1)
    IF ( ZSUM_WEIGHT_PATCH(JJ,JPATCH) >0.) THEN
      JCOUNT(JPATCH)=JCOUNT(JPATCH)+1
      NMASK(JCOUNT(JPATCH),JPATCH)=JJ
    ENDIF
  ENDDO
ENDDO

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
          PFIELD(JI,JPATCH) =  ZWORK(JI,JPATCH) / ZSUM_WEIGHT_PATCH(JI,JPATCH)
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
        PFIELD(JI,JPATCH) = ZSUM_WEIGHT_PATCH(JI,JPATCH) / ZWORK(JI,JPATCH)
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
        PFIELD(JI,JPATCH) = ZDZ(JI,JPATCH) * EXP( - SQRT(ZSUM_WEIGHT_PATCH(JI,JPATCH)/ZWORK(JI,JPATCH)) )
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PGD_PARAM
