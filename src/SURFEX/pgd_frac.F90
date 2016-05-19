!     #########
      SUBROUTINE PGD_FRAC(HPROGRAM,OECOCLIMAP)
!     ##############################################################
!
!!**** *PGD_FRAC* monitor for averaging and interpolations of cover fractions
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
!!
!!       Modified 08/12/05, P. Le Moigne: user defined fields
!!       Modified 04/2013   V. Masson   : set a cover containing garden for TOWN default
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL, CGRID
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_SURF_ATM_n,     ONLY : XNATURE, XSEA, XTOWN, XWATER,             &
                                  XCOVER, LCOVER,                         &
                                  NSIZE_NATURE, NSIZE_SEA,                &
                                  NSIZE_TOWN, NSIZE_WATER,NSIZE_FULL,     &
                                  NDIM_NATURE, NDIM_SEA,                  &
                                  NDIM_TOWN,NDIM_WATER  
!
USE MODD_PGDWORK,        ONLY : CATYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_SUM_ON_ALL_PROCS
!
USE MODE_POS_SURF
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
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(OUT)   :: OECOCLIMAP   ! F if fractions prescribed by user
!                                                  ! T if fractions will be computed from ecoclimap
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JCOVER    ! loop counter on covers
!
REAL, DIMENSION(NL)   :: ZSUM      ! sum of 4 tiles fractions
!
!*    0.3    Declaration of namelists
!            ------------------------
!
LOGICAL  :: LECOCLIMAP  ! F if ecoclimap is not used
REAL     :: XUNIF_SEA   ! value of sea    fraction
REAL     :: XUNIF_WATER ! value of water  fraction
REAL     :: XUNIF_NATURE! value of nature fraction
REAL     :: XUNIF_TOWN  ! value of town   fraction
!
! name of files containing data
!
 CHARACTER(LEN=28)     :: CFNAM_SEA    ! name of sea    file
 CHARACTER(LEN=28)     :: CFNAM_WATER  ! name of water  file
 CHARACTER(LEN=28)     :: CFNAM_NATURE ! name of nature file
 CHARACTER(LEN=28)     :: CFNAM_TOWN   ! name of town   file
!
! type of files containing data
!
 CHARACTER(LEN=6)      :: CFTYP_SEA    ! type of sea    file
 CHARACTER(LEN=6)      :: CFTYP_WATER  ! type of water  file
 CHARACTER(LEN=6)      :: CFTYP_NATURE ! type of nature file
 CHARACTER(LEN=6)      :: CFTYP_TOWN   ! type of town   file
!
INTEGER               :: ICOVER       ! 0 if cover is not present, >1 if present somewhere
!                                     ! (even on another processor)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
NAMELIST/NAM_FRAC/ LECOCLIMAP,                                         &
                     XUNIF_SEA, XUNIF_WATER, XUNIF_NATURE, XUNIF_TOWN, &
                     CFNAM_SEA, CFNAM_WATER, CFNAM_NATURE, CFNAM_TOWN, &
                     CFTYP_SEA, CFTYP_WATER, CFTYP_NATURE, CFTYP_TOWN  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_FRAC',0,ZHOOK_HANDLE)
XUNIF_SEA      = XUNDEF
XUNIF_WATER    = XUNDEF
XUNIF_NATURE   = XUNDEF
XUNIF_TOWN     = XUNDEF
LECOCLIMAP     = .TRUE.
CFNAM_SEA   (:)= '                            '
CFNAM_WATER (:)= '                            '
CFNAM_NATURE(:)= '                            '
CFNAM_TOWN  (:)= '                            '
CFTYP_SEA   (:)= '      '
CFTYP_WATER (:)= '      '
CFTYP_NATURE(:)= '      '
CFTYP_TOWN  (:)= '      '
!
OECOCLIMAP = .TRUE.
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_FRAC',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_FRAC)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
IF ((LEN_TRIM(CFNAM_SEA)/=0 .OR. XUNIF_SEA/=XUNDEF) .AND. (LEN_TRIM(CFNAM_WATER)/=0 .OR. XUNIF_WATER/=XUNDEF) .AND. &
    (LEN_TRIM(CFNAM_NATURE)/=0 .OR. XUNIF_NATURE/=XUNDEF) .AND. (LEN_TRIM(CFNAM_TOWN)/=0 .OR. XUNIF_TOWN/=XUNDEF)) THEN
!
  ALLOCATE(XSEA   (NL))
  ALLOCATE(XWATER (NL))
  ALLOCATE(XNATURE(NL))
  ALLOCATE(XTOWN  (NL))
!
!*    3.      Uniform fractions are prescribed
!             --------------------------------
!
  IF (XUNIF_SEA/=XUNDEF .AND. XUNIF_WATER/=XUNDEF .AND. XUNIF_NATURE/=XUNDEF .AND.  XUNIF_TOWN/=XUNDEF) THEN
!
!*    3.1     Verification of the total input cover fractions
!             -----------------------------------------------
!
    IF (ABS(XUNIF_SEA+XUNIF_WATER+XUNIF_NATURE+XUNIF_TOWN-1.)>1.E-6) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*********************************************************'
      WRITE(ILUOUT,*) '* Error in fractions preparation                        *'
      WRITE(ILUOUT,*) '* The prescribed fractions do not fit                   *'
      WRITE(ILUOUT,*) '* The sum of all 4 fractions must be equal to 1 exactly *'
      WRITE(ILUOUT,*) '*********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_FRAC: SUM OF ALL FRACTIONS MUST BE 1.')
!
!*    3.2     Use of the presribed cover fractions
!             ------------------------------------
!
    ELSE
!
      XSEA    = XUNIF_SEA 
      XWATER  = XUNIF_WATER
      XNATURE = XUNIF_NATURE
      XTOWN   = XUNIF_TOWN

    END IF
!
!*    3.3     No data
!             -------
!
  ELSE

    CATYPE = 'ARI'
    IF (XUNIF_SEA==XUNDEF) THEN
      CALL PGD_FIELD(HPROGRAM,'XSEA: sea fraction      ','ALL', CFNAM_SEA   , &
                    CFTYP_SEA   , XUNIF_SEA   , XSEA(:)   )  
    ELSE                 
      XSEA(:) = XUNIF_SEA
    ENDIF
    IF (XUNIF_WATER==XUNDEF) THEN
      CALL PGD_FIELD(HPROGRAM,'XWATER: water fraction  ','ALL', CFNAM_WATER , &
                    CFTYP_WATER , XUNIF_WATER , XWATER(:) )  
    ELSE                    
      XWATER(:) = XUNIF_WATER
    ENDIF
    IF (XUNIF_NATURE==XUNDEF) THEN
      CALL PGD_FIELD(HPROGRAM,'XNATURE: nature fraction','ALL', CFNAM_NATURE, &
                    CFTYP_NATURE, XUNIF_NATURE, XNATURE(:))  
    ELSE                    
      XNATURE(:) = XUNIF_NATURE
    ENDIF
    IF (XUNIF_TOWN==XUNDEF) THEN
      CALL PGD_FIELD(HPROGRAM,'XTOWN: town fraction    ','ALL', CFNAM_TOWN  , &
                    CFTYP_TOWN  , XUNIF_TOWN  , XTOWN(:)  )  
    ELSE                    
      XTOWN(:) = XUNIF_TOWN
    ENDIF
  ENDIF

ELSE
!
!*    4.      No prescription of fractions
!             ----------------------------
!
  IF (LHOOK) CALL DR_HOOK('PGD_FRAC',1,ZHOOK_HANDLE)
  RETURN
!
ENDIF
!-------------------------------------------------------------------------------
!         consistency check
!         ------------------
!
ZSUM(:) = XSEA(:) + XNATURE(:) + XWATER(:) + XTOWN(:)

XSEA(:)    = XSEA(:)    / ZSUM(:)
XNATURE(:) = XNATURE(:) / ZSUM(:)
XWATER(:)  = XWATER(:)  / ZSUM(:)
XTOWN(:)   = XTOWN(:)   / ZSUM(:)
!
!-------------------------------------------------------------------------------

WRITE(ILUOUT,*) ' '
!-------------------------------------------------------------------------------
!
OECOCLIMAP = LECOCLIMAP
!
!*    5.      List of cover present
!             ---------------------
!
IF (.NOT.LECOCLIMAP) THEN

  ALLOCATE(XCOVER (NL,JPCOVER))

  XCOVER(:,:) =0.
  XCOVER(:,1) = XSEA(:)
  XCOVER(:,2) = XWATER(:)
  XCOVER(:,4) = XNATURE(:)
  XCOVER(:,151) = XTOWN(:)
  ! comment V. Masson: to use this cover type for town by default avoids crashes
  ! when garden fraction is specified but no garden vegetation parameters.
  ! In this cas, the properties for garden come from the cover 151
!
 ALLOCATE(LCOVER(JPCOVER))
  LCOVER = .FALSE.
 DO JCOVER=1,JPCOVER
    ICOVER = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XCOVER(:,JCOVER)/=0. ,'COV')
    IF (ICOVER>0) LCOVER(JCOVER)=.TRUE. 
 END DO  
!
!
!-------------------------------------------------------------------------------
!
!*    6.      Land - sea fractions
!             --------------------
!
  NSIZE_NATURE    = COUNT(XNATURE(:) > 0.0)
  NSIZE_WATER     = COUNT(XWATER (:) > 0.0)
  NSIZE_SEA       = COUNT(XSEA   (:) > 0.0)
  NSIZE_TOWN      = COUNT(XTOWN  (:) > 0.0)
  NSIZE_FULL      = NL
!
  NDIM_NATURE    = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XNATURE(:) > 0.0, 'DIM')
  NDIM_WATER     = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XWATER (:) > 0.0, 'DIM')
  NDIM_SEA       = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XSEA   (:) > 0.0, 'DIM')
  NDIM_TOWN      = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XTOWN  (:) > 0.0, 'DIM')
!  
ENDIF
IF (LHOOK) CALL DR_HOOK('PGD_FRAC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FRAC
