!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SSO_n (KSIZE_FULL, PSEA, USS, HPROGRAM)
!     ################################
!
!!****  *READ_SSO_n* - routine to read a file for
!!                         physiographic data file of model _n 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise the 
!!       physiographic data file.
!!
!!
!!**  METHOD
!!    ------
!!      The data are read in the initial surface file :
!!        - 2D physiographic data fields
!!          
!!      It does not read the grid definition. This should have been
!!      read already.
!!
!!    EXTERNAL
!!    --------
!!      
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
!!      Original    01/2003
!!      A. Mary     04/2016  Add orographic radiation parameters
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_ABOR1_SFX
USE MODI_READ_SURF
!
USE MODD_SURF_PAR, ONLY : XUNDEF, LEN_HREC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
INTEGER, INTENT(IN) :: KSIZE_FULL
REAL, DIMENSION(:), INTENT(IN) :: PSEA
!
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

!
INTEGER           :: IRESP          ! Error code after redding
INTEGER           :: JK             ! Loop index
INTEGER           :: IVERSION       ! Surfex Version
LOGICAL           :: GORORAD
! 
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB)   :: ZEPS             ! Imposed minimum difference between Hmin and Hmax for the system to have a solution
REAL(KIND=JPRB)   :: ZHMAXFRAC        ! Fraction of direct solar radiation seen if zenith == hmax
REAL(KIND=JPRB), DIMENSION(:,:), ALLOCATABLE :: ZDIFF            ! Difference between Hmin and Hmax

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       2.     Orography :
!               ---------
!
!
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',0,ZHOOK_HANDLE)
IF(.NOT.ASSOCIATED(USS%XAVG_ZS)) ALLOCATE(USS%XAVG_ZS(KSIZE_FULL))
YRECFM='AVG_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAVG_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XSIL_ZS)) ALLOCATE(USS%XSIL_ZS(KSIZE_FULL))
YRECFM='SIL_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSIL_ZS(:),IRESP)
!
!
!*       3.     Subgrid Orography :
!               -----------------
!
!
IF(.NOT.ASSOCIATED(USS%XSSO_STDEV)) ALLOCATE(USS%XSSO_STDEV(KSIZE_FULL))
YRECFM='SSO_STDEV'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_STDEV(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_STDEV(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XMIN_ZS)) ALLOCATE(USS%XMIN_ZS(KSIZE_FULL))
YRECFM='MIN_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XMIN_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XMAX_ZS)) ALLOCATE(USS%XMAX_ZS(KSIZE_FULL))
YRECFM='MAX_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XMAX_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XSSO_ANIS)) ALLOCATE(USS%XSSO_ANIS(KSIZE_FULL))
YRECFM='SSO_ANIS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_ANIS(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_ANIS(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XSSO_DIR)) ALLOCATE(USS%XSSO_DIR(KSIZE_FULL))
YRECFM='SSO_DIR'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_DIR(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_DIR(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XSSO_SLOPE)) ALLOCATE(USS%XSSO_SLOPE(KSIZE_FULL))
YRECFM='SSO_SLOPE'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_SLOPE(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_SLOPE(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       3.     Subgrid Orography roughness:
!               ---------------------------
!
!
IF(.NOT.ASSOCIATED(USS%XHO2IP)) ALLOCATE(USS%XHO2IP(KSIZE_FULL))
YRECFM='HO2IP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2IP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2IP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2JP)) ALLOCATE(USS%XHO2JP(KSIZE_FULL))
YRECFM='HO2JP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2JP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2JP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2IM)) ALLOCATE(USS%XHO2IM(KSIZE_FULL))
YRECFM='HO2IM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2IM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2IM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2JM)) ALLOCATE(USS%XHO2JM(KSIZE_FULL))
YRECFM='HO2JM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2JM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2JM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSIP)) ALLOCATE(USS%XAOSIP(KSIZE_FULL))
YRECFM='AOSIP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSIP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSIP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSJP)) ALLOCATE(USS%XAOSJP(KSIZE_FULL))
YRECFM='AOSJP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSJP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSJP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSIM)) ALLOCATE(USS%XAOSIM(KSIZE_FULL))
YRECFM='AOSIM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSIM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSIM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSJM)) ALLOCATE(USS%XAOSJM(KSIZE_FULL))
YRECFM='AOSJM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSJM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSJM(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       4.     Orographic radiation parameters:
!               -------------------------------
!
IF (USS%LDSV .OR. USS%LDSH .OR. USS%LDSL) THEN
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)      
  IF (IVERSION < 8) THEN
    CALL ABOR1_SFX("READ_SSON : To use LDSV LDSH or LDSL, you need a PGD computed with version >= 9")
  ELSE
    CALL READ_SURF(HPROGRAM,'LORORAD',GORORAD,IRESP)
  END IF
  !
  IF (GORORAD) THEN
    CALL READ_SURF(HPROGRAM,'ASPSECTORS',USS%NSECTORS,IRESP)
    !
    IF(.NOT.ASSOCIATED(USS%XAVG_SLO)) ALLOCATE(USS%XAVG_SLO(KSIZE_FULL))
    YRECFM='AVG_SLO'
    CALL READ_SURF(HPROGRAM,YRECFM,USS%XAVG_SLO(:),IRESP)
    WHERE (PSEA(:) == 1.) USS%XAVG_SLO(:) = XUNDEF
    !
    IF(.NOT.ASSOCIATED(USS%XSLOPE)) ALLOCATE(USS%XSLOPE(KSIZE_FULL))
    YRECFM='SLOPE'
    CALL READ_SURF(HPROGRAM,YRECFM,USS%XSLOPE(:),IRESP)
    WHERE (PSEA(:) == 1.) USS%XSLOPE(:) = XUNDEF
    !
    IF(.NOT.ASSOCIATED(USS%XASPECT)) ALLOCATE(USS%XASPECT(KSIZE_FULL))
    YRECFM='ASPECT'
    CALL READ_SURF(HPROGRAM,YRECFM,USS%XASPECT(:),IRESP)
    WHERE (PSEA(:) == 1.) USS%XASPECT(:) = XUNDEF
    !
    IF(.NOT.ASSOCIATED(USS%XSLOPE_DIR)) ALLOCATE(USS%XSLOPE_DIR(KSIZE_FULL,USS%NSECTORS))
    DO JK = 1,USS%NSECTORS
      WRITE(YRECFM,"(a9,i0.2)") 'SLOPE_DIR',JK
      CALL READ_SURF(HPROGRAM,YRECFM,USS%XSLOPE_DIR(:,JK),IRESP)
      WHERE (PSEA(:) == 1.) USS%XSLOPE_DIR(:,JK) = XUNDEF
    END DO
    ! 
    IF(.NOT.ASSOCIATED(USS%XFRAC_DIR)) ALLOCATE(USS%XFRAC_DIR(KSIZE_FULL,USS%NSECTORS))
    DO JK = 1,USS%NSECTORS
      WRITE(YRECFM,"(a8,i0.2)") 'FRAC_DIR',JK
      CALL READ_SURF(HPROGRAM,YRECFM,USS%XFRAC_DIR(:,JK),IRESP)
      WHERE (PSEA(:) == 1.) USS%XFRAC_DIR(:,JK) = XUNDEF
    END DO
    !
    IF(.NOT.ASSOCIATED(USS%XSVF)) ALLOCATE(USS%XSVF(KSIZE_FULL))
    YRECFM='SVF'
    CALL READ_SURF(HPROGRAM,YRECFM,USS%XSVF(:),IRESP)
    !
    IF(.NOT.ASSOCIATED(USS%XHMINS_DIR)) ALLOCATE(USS%XHMINS_DIR(KSIZE_FULL,USS%NSECTORS))
    DO JK = 1,USS%NSECTORS
      WRITE(YRECFM,"(a9,i0.2)") 'HMINS_DIR',JK
      CALL READ_SURF(HPROGRAM,YRECFM,USS%XHMINS_DIR(:,JK),IRESP)
    END DO
    !
    IF(.NOT.ASSOCIATED(USS%XHMAXS_DIR)) ALLOCATE(USS%XHMAXS_DIR(KSIZE_FULL,USS%NSECTORS))
    DO JK = 1,USS%NSECTORS
      WRITE(YRECFM,"(a9,i0.2)") 'HMAXS_DIR',JK
      CALL READ_SURF(HPROGRAM,YRECFM,USS%XHMAXS_DIR(:,JK),IRESP)
    END DO
    ! compute and A/B factors for shadowing
    IF(.NOT.ASSOCIATED(USS%XSHA_DIR)) ALLOCATE(USS%XSHA_DIR(KSIZE_FULL,USS%NSECTORS))
    IF(.NOT.ASSOCIATED(USS%XSHB_DIR)) ALLOCATE(USS%XSHB_DIR(KSIZE_FULL,USS%NSECTORS))
    ALLOCATE(ZDIFF(KSIZE_FULL,USS%NSECTORS))
    ZEPS = 0.001
    ZHMAXFRAC = 1.
    ZDIFF(:,:) = USS%XHMAXS_DIR(:,:) - USS%XHMINS_DIR(:,:)
    WHERE (ZDIFF(:,:) < ZEPS)
      USS%XHMAXS_DIR(:,:) = USS%XHMINS_DIR(:,:) + ZEPS
      ZDIFF(:,:) = ZEPS
    END WHERE
    USS%XSHA_DIR(:,:) = ZHMAXFRAC / ZDIFF(:,:)
    USS%XSHB_DIR(:,:) = -USS%XSHA_DIR(:,:) * USS%XHMINS_DIR(:,:)
  ELSE
    CALL ABOR1_SFX("READ_SSON : To use LDSV LDSH or LDSL you need a PGD computed with LORORAD = T ")
  END IF
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SSO_n
