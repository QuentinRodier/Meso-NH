!     #########
      SUBROUTINE READ_SSO_n(HPROGRAM)
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_SSO_n,     ONLY : XAVG_ZS, XSIL_ZS, XSSO_STDEV,    &
                                      XSSO_ANIS, XSSO_DIR, XSSO_SLOPE, &
                                      XMIN_ZS, XMAX_ZS,                &
                                      XAOSIP, XAOSIM, XAOSJP, XAOSJM,  &
                                      XHO2IP, XHO2IM, XHO2JP, XHO2JM  
USE MODD_SURF_ATM_n    ,     ONLY : NSIZE_FULL, XSEA
!
!
USE MODI_READ_SURF
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

!
INTEGER           :: IRESP          ! Error code after redding
! 
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       2.     Orography :
!               ---------
!
!
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',0,ZHOOK_HANDLE)
IF(.NOT.ASSOCIATED(XAVG_ZS)) ALLOCATE(XAVG_ZS(NSIZE_FULL))
YRECFM='AVG_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XAVG_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(XSIL_ZS)) ALLOCATE(XSIL_ZS(NSIZE_FULL))
YRECFM='SIL_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XSIL_ZS(:),IRESP)
!
!
!*       3.     Subgrid Orography :
!               -----------------
!
!
IF(.NOT.ASSOCIATED(XSSO_STDEV)) ALLOCATE(XSSO_STDEV(NSIZE_FULL))
YRECFM='SSO_STDEV'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_STDEV(:),IRESP)
WHERE (XSEA(:) == 1.) XSSO_STDEV(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XMIN_ZS)) ALLOCATE(XMIN_ZS(NSIZE_FULL))
YRECFM='MIN_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XMIN_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(XMAX_ZS)) ALLOCATE(XMAX_ZS(NSIZE_FULL))
YRECFM='MAX_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XMAX_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(XSSO_ANIS)) ALLOCATE(XSSO_ANIS(NSIZE_FULL))
YRECFM='SSO_ANIS'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_ANIS(:),IRESP)
WHERE (XSEA(:) == 1.) XSSO_ANIS(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XSSO_DIR)) ALLOCATE(XSSO_DIR(NSIZE_FULL))
YRECFM='SSO_DIR'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_DIR(:),IRESP)
WHERE (XSEA(:) == 1.) XSSO_DIR(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XSSO_SLOPE)) ALLOCATE(XSSO_SLOPE(NSIZE_FULL))
YRECFM='SSO_SLOPE'
 CALL READ_SURF(HPROGRAM,YRECFM,XSSO_SLOPE(:),IRESP)
WHERE (XSEA(:) == 1.) XSSO_SLOPE(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       3.     Subgrid Orography roughness:
!               ---------------------------
!
!
IF(.NOT.ASSOCIATED(XHO2IP)) ALLOCATE(XHO2IP(NSIZE_FULL))
YRECFM='HO2IP'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2IP(:),IRESP)
WHERE (XSEA(:) == 1.) XHO2IP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XHO2JP)) ALLOCATE(XHO2JP(NSIZE_FULL))
YRECFM='HO2JP'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2JP(:),IRESP)
WHERE (XSEA(:) == 1.) XHO2JP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XHO2IM)) ALLOCATE(XHO2IM(NSIZE_FULL))
YRECFM='HO2IM'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2IM(:),IRESP)
WHERE (XSEA(:) == 1.) XHO2IM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XHO2JM)) ALLOCATE(XHO2JM(NSIZE_FULL))
YRECFM='HO2JM'
 CALL READ_SURF(HPROGRAM,YRECFM,XHO2JM(:),IRESP)
WHERE (XSEA(:) == 1.) XHO2JM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XAOSIP)) ALLOCATE(XAOSIP(NSIZE_FULL))
YRECFM='AOSIP'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSIP(:),IRESP)
WHERE (XSEA(:) == 1.) XAOSIP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XAOSJP)) ALLOCATE(XAOSJP(NSIZE_FULL))
YRECFM='AOSJP'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSJP(:),IRESP)
WHERE (XSEA(:) == 1.) XAOSJP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XAOSIM)) ALLOCATE(XAOSIM(NSIZE_FULL))
YRECFM='AOSIM'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSIM(:),IRESP)
WHERE (XSEA(:) == 1.) XAOSIM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(XAOSJM)) ALLOCATE(XAOSJM(NSIZE_FULL))
YRECFM='AOSJM'
 CALL READ_SURF(HPROGRAM,YRECFM,XAOSJM(:),IRESP)
WHERE (XSEA(:) == 1.) XAOSJM(:) = XUNDEF
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SSO_n
