!#############################################################
SUBROUTINE INIT_CHEMICAL_n(KLUOUT, KSV, HSV, KBEQ, HSVO, KBAER,            &
                           KSV_CHSBEG, KSV_CHSEND, KSV_AERBEG, KSV_AEREND, &
                           HCH_NAMES, HAER_NAMES, KDSTEQ, KSV_DSTBEG,      &
                           KSV_DSTEND, KSLTEQ, KSV_SLTBEG, KSV_SLTEND,     &
                           HDSTNAMES, HSLTNAMES                           )  
!#############################################################
!
!!****  *INIT_CHEMICAL_n* - routine to initialize CHEMICAL SPECIES
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2011
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
! 
USE MODD_CHS_AEROSOL,    ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,       ONLY: LVARSIG_DST, NDSTMDE,  NDST_MDEBEG, LRGFIX_DST, JPMODE_DST
USE MODD_SLT_SURF,       ONLY: LVARSIG_SLT, NSLTMDE,  NSLT_MDEBEG, LRGFIX_SLT, JPMODE_SLT

USE MODD_DST_n
USE MODD_SLT_n
!
USE MODI_CH_INIT_NAMES
USE MODI_DSLT_INIT_NAMES
USE MODI_DSLT_INIT_MODES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                          INTENT(IN) :: KLUOUT
INTEGER,                          INTENT(IN) :: KSV      ! number of scalars
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN) :: HSV      ! name of all scalar variables
INTEGER,                         INTENT(OUT) :: KBEQ     ! number of chemical variables
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HSVO          ! name of scalar species without # and @
INTEGER,                         INTENT(OUT) :: KBAER    ! number of aerosol variables
INTEGER,                         INTENT(OUT) :: KSV_CHSBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_CHSEND  ! last  chemical var.
INTEGER,                         INTENT(OUT) :: KSV_AERBEG  ! first aerosol var.
INTEGER,                         INTENT(OUT) :: KSV_AEREND  ! last  aerosol var.
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HCH_NAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HAER_NAMES     
INTEGER,                         INTENT(OUT) :: KDSTEQ     ! number of chemical variables
INTEGER,                         INTENT(OUT) :: KSV_DSTBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_DSTEND  ! last  chemical var.
INTEGER,                         INTENT(OUT) :: KSLTEQ     ! number of chemical variables
INTEGER,                         INTENT(OUT) :: KSV_SLTBEG  ! first chemical var.
INTEGER,                         INTENT(OUT) :: KSV_SLTEND  ! last  chemical var.
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HDSTNAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HSLTNAMES
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
!
IF (LHOOK) CALL DR_HOOK('INIT_CHEMICAL_n',0,ZHOOK_HANDLE)
!
IF (KSV /= 0) THEN
  !
  ALLOCATE(HSVO(KSV))
  CALL CH_INIT_NAMES(KLUOUT, HSV, KBEQ, KBAER, HSVO, &
                     KSV_CHSBEG, KSV_CHSEND,         &
                     KSV_AERBEG, KSV_AEREND,         &
                     LVARSIGI, LVARSIGJ              )  

  IF (KBEQ > 0 ) THEN
    ALLOCATE(HCH_NAMES(KBEQ))
    HCH_NAMES(:) = HSVO(KSV_CHSBEG:KSV_CHSEND)
  ELSE
    ALLOCATE(HCH_NAMES(0))
  END IF

  IF (KBAER > 0 ) THEN
    ALLOCATE(HAER_NAMES(KBAER))
    HAER_NAMES(:) = HSVO(KSV_AERBEG:KSV_AEREND)
  ELSE
    ALLOCATE(HAER_NAMES(0))
  END IF
  !
  CALL DSLT_INIT_NAMES(         &
         KLUOUT,                &!I [idx] index of writing unit
         'DSTM',                &
         HSV,                   &!I [char] list of scalar variables
         JPMODE_DST,            &
         KDSTEQ,                &!O [nbr] number of dust related tracers
         KSV_DSTBEG,            &!O [idx] first dust related scalar variable
         KSV_DSTEND,            &!O [idx] last dust related scalar variable
         LVARSIG_DST,           &!O type of standard deviation (fixed or variable)
         LRGFIX_DST             &!O type of mean radius (fixed or variable)        
         )  

  IF (PRESENT(HDSTNAMES)) THEN
    IF (KDSTEQ >=1) THEN
      CALL DSLT_INIT_MODES(       &
            KDSTEQ,               &!I [nbr] number of dust related variables in scalar list
            KSV_DSTBEG,           &!I [idx] index of first dust related variable in scalar list
            KSV_DSTEND,           &!I [idx] index of last dust related variable in scalar list
            LVARSIG_DST,          &!I type of standard deviation (fixed or variable)
            LRGFIX_DST,           &!O type of mean radius (fixed or variable)        
            NDST_MDEBEG,          &!O [idx] index of mass for first mode in scalar list
            NDSTMDE               &!O [nbr] number of modes to be transported
            )

      IF(.NOT. ASSOCIATED(HDSTNAMES)) ALLOCATE (HDSTNAMES(KDSTEQ))
      HDSTNAMES(:) = HSVO(KSV_DSTBEG:KSV_DSTEND)
    ENDIF
  ENDIF


  CALL DSLT_INIT_NAMES(         &
          KLUOUT,               &!I [idx] index of writing unit
         'SLTM',                &          
          HSV,                  &!I [char] list of scalar variables
          JPMODE_SLT,           &          
          KSLTEQ,               &!O [nbr] number of sea salt related tracers
          KSV_SLTBEG,           &!O [idx] first sea salt related scalar variable
          KSV_SLTEND,           &!O [idx] last sea salt related scalar variable
          LVARSIG_SLT,          &!O type of standard deviation (fixed or variable)
          LRGFIX_SLT            &!O type of mean radius (fixed or variable)        
          )  

  IF (PRESENT(HSLTNAMES)) THEN
    IF (KSLTEQ >=1) THEN
      CALL DSLT_INIT_MODES(       &
            KSLTEQ,               &!I [nbr] number of sea salt related variables in scalar list
            KSV_SLTBEG,           &!I [idx] index of first sea salt related variable in scalar list
            KSV_SLTEND,           &!I [idx] index of last sea salt related variable in scalar list
            LVARSIG_SLT,          &!I type of standard deviation (fixed or variable)
            LRGFIX_SLT,           &!O type of mean radius (fixed or variable)
            NSLT_MDEBEG,          &!O [idx] index of mass for first mode in scalar list
            NSLTMDE               &!O [nbr] number of modes to be transported
            )  
      IF(.NOT. ASSOCIATED(HSLTNAMES)) ALLOCATE (HSLTNAMES(KSLTEQ))
      HSLTNAMES(:) = HSVO(KSV_SLTBEG:KSV_SLTEND)
    ENDIF
  END IF

ELSE
  ALLOCATE(HSVO     (0))
  IF (PRESENT(HDSTNAMES)) ALLOCATE(HDSTNAMES(0))
  IF (PRESENT(HSLTNAMES)) ALLOCATE(HSLTNAMES(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_CHEMICAL_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_CHEMICAL_n
