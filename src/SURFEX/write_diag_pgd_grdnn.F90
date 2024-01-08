!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_PGD_GRDN_n (DTCO, HSELECT, U, OSURF_DIAG_ALBEDO, &
                                        HSURF, S, P, K, PEK, IO, HPROGRAM)
!     #########################################
!
!!****  *WRITE_DIAG_PGD_TEB_GARDEN_n* - writes the ISBA physiographic diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      Modified    10/2004 by P. Le Moigne: add XZ0REL, XVEGTYPE_PATCH
!!      Modified    11/2005 by P. Le Moigne: limit length of VEGTYPE_PATCH field names
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, LEN_HREC
!
!
USE MODD_IO_SURF_FA, ONLY : LFANOCOMPACT, LPREP
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
CHARACTER(LEN=2), INTENT(IN)  :: HSURF
LOGICAL, INTENT(IN) :: OSURF_DIAG_ALBEDO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YLVLV, YPAS
!
INTEGER :: JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_GRDN_N',0,ZHOOK_HANDLE)
CALL INIT_IO_SURF_n(DTCO, U,   HPROGRAM,'TOWN  ','TEB   ','WRITE','TEB_PGD.OUT.nc')
!
!* Leaf Area Index
!
IF (IO%CPHOTO=='NON' .OR. IO%CPHOTO=='AST') THEN
  !
  YRECFM=HSURF//'_LAI'
  YCOMMENT='leaf area index (-)'
  !
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Vegetation fraction
!
YRECFM=HSURF//'_VEG'
YCOMMENT='vegetation fraction (-)'
!
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XVEG(:),IRESP,HCOMMENT=YCOMMENT)
!
!* Surface roughness length (without snow)
!
YRECFM=HSURF//'_Z0VEG'
YCOMMENT='surface roughness length (without snow) (M)'
!
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
IF (HSURF=='GD') THEN
!
!* Soil depth for each patch
!
  DO JL=1,SIZE(P%XDG,2)
    !
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A5,I1)') HSURF//'_DG',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A5,I2)') HSURF//'_DG',JL
    ELSE
      CALL ABOR1_SFX ("No format defined")
    ENDIF
    !
    YCOMMENT='soil depth'//' (M)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,P%XDG(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
!-------------------------------------------------------------------------------
!
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A7,I1)') HSURF//'_WSAT',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A7,I2)') HSURF//'_WSAT',JL          
    ENDIF  
    YCOMMENT='garden soil porosity by layer (m3/m3)'  
    CALL WRITE_SURF(HSELECT, &
                    HPROGRAM,YRECFM,K%XWSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A6,I1)') HSURF//'_WFC',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A6,I2)') HSURF//'_WFC',JL          
    ENDIF  
    YCOMMENT='garden field capacity by layer (m3/m3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XWFC(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A8,I1)') HSURF//'_WWILT',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A8,I2)') HSURF//'_WWILT',JL          
    ENDIF  
    YCOMMENT='garden wilting point by layer (m3/m3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XWWILT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A10,I1)') HSURF//'_MPOTSAT',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A10,I2)') HSURF//'_MPOTSAT',JL          
    ENDIF  
    YCOMMENT='Matric potential at saturation for garden (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XMPOTSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A8,I1)') HSURF//'_BCOEF',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A8,I2)') HSURF//'_BCOEF',JL          
    ENDIF  
    YCOMMENT='soil water CH78 b-parameter for garden (-)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XBCOEF(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
  DO JL=1,SIZE(P%XDG,2)
    IF     (JL.LT.10) THEN
      WRITE(YRECFM,FMT='(A10,I1)') HSURF//'_CONDSAT',JL
    ELSEIF (JL.LT.100) THEN
      WRITE(YRECFM,FMT='(A10,I2)') HSURF//'_CONDSAT',JL          
    ENDIF  
    YCOMMENT='Hydraulic conductivity at saturation for garden (m/s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,P%XCONDSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
END IF
!-------------------------------------------------------------------------------
! For Earth System Model
IF(LFANOCOMPACT.AND..NOT.LPREP)THEN
   CALL END_IO_SURF_n(HPROGRAM)
   IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_GRDN_N',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
!-------------------------------------------------------------------------------
IF (HSURF=='GD') THEN
!
!* Runoff soil ice depth for each patch
!
  IF(IO%CHORT=='SGH')THEN
    YRECFM=HSURF//'_DICE'
    YCOMMENT='soil ice depth for runoff (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,P%XD_ICE(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
!
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type for each patch
!
  DO JL=1,SIZE(S%XVEGTYPE,2)
    WRITE(YPAS,'(I2)') JL 
    YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    WRITE(YRECFM,FMT='(A12)') HSURF//'_VEGTY_P'//YLVLV
    YCOMMENT='fraction of each vegetation type '//' (-)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XVEGTYPE(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
END IF
!-------------------------------------------------------------------------------
!
!* other surface parameters
!
YRECFM=HSURF//'_RSMIN'
YCOMMENT='minimum stomatal resistance (SM-1)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRSMIN(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM=HSURF//'_GAMMA'
YCOMMENT='coefficient for RSMIN calculation (-)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XGAMMA(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM=HSURF//'_CV'
YCOMMENT='vegetation thermal inertia coefficient (-)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XCV(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM=HSURF//'_RGL'
YCOMMENT='maximum solar radiation usable in photosynthesis (-)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRGL(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM=HSURF//'_EMIS_ISBA'
YCOMMENT='surface emissivity (-)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XEMIS(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (HSURF=='GD') THEN
  YRECFM=HSURF//'_WRMAX_CF'
  YCOMMENT='coefficient for maximum water interception (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XWRMAX_CF(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
!-------------------------------------------------------------------------------
!
IF (OSURF_DIAG_ALBEDO) THEN
!
!* Soil albedos
!
!
  IF (HSURF=='GD') THEN
   YRECFM=HSURF//'_ALBNIR_S'
   YCOMMENT='soil near-infra-red albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBNIR_SOIL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM=HSURF//'_ALBVIS_S'
   YCOMMENT='soil visible albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBVIS_SOIL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM=HSURF//'_ALBUV_S'
   YCOMMENT='soil UV albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBUV_SOIL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* albedos
!

   YRECFM=HSURF//'_ALBNIR_T'
   YCOMMENT='total near-infra-red albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBNIR(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM=HSURF//'_ALBVIS_T'
   YCOMMENT='total visible albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBVIS(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM=HSURF//'_ALBUV_T'
   YCOMMENT='total UV albedo (-)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XALBUV(:),IRESP,HCOMMENT=YCOMMENT)
!
   END IF
END IF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_GRDN_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_PGD_GRDN_n
