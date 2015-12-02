!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_CHEMISTRY_SNAP(HPROGRAM,OCH_EMIS)
!     ##############################################################
!
!!**** *PGD_CHEMISTRY_SNAP* monitor for averaging and interpolations of physiographic fields
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
!!    S; Queguiner        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2011
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,            ONLY : CATYPE
USE MODD_SURF_PAR,           ONLY : XUNDEF
USE MODD_PGD_GRID,           ONLY : NL
USE MODD_CH_SURF,            ONLY : JPEMISMAX_S, JPSNAPMAX
USE MODD_CH_SNAP_n,          ONLY : NEMIS_SNAP_n=>NEMIS_SNAP,                &
                                    NEMIS_NBR_n=>NEMIS_NBR,                  & 
                                    CEMIS_NAME_n=>CEMIS_NAME,                &
                                    CEMIS_COMMENT_n=>CEMIS_COMMENT,          &
                                    XSNAP_MONTHLY,XSNAP_DAILY, XSNAP_HOURLY, &
                                    NSNAP_M, NSNAP_D, NSNAP_H,               &
                                    XEMIS_FIELDS_SNAP, LEMIS_FIELDS,         &
                                    CSNAP_TIME_REF, XDELTA_LEGAL_TIME
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
USE MODI_PGD_FIELD
USE MODI_PGD_SNAP_TEMP_PROFILE
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(OUT)   :: OCH_EMIS     ! emission flag
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
INTEGER                           :: JSPEC     ! loop counter on emission species
INTEGER                           :: JSNAP     ! loop counter on SNAP categories
 CHARACTER(LEN=5)                  :: YSNAP_TIME_REF ! to check if all snaps use
!                                                   ! the same time  reference
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
 CHARACTER(LEN=12), DIMENSION(JPEMISMAX_S):: CEMIS_NAME
 CHARACTER(LEN=40), DIMENSION(JPEMISMAX_S):: CEMIS_COMMENT
 CHARACTER(LEN=28), DIMENSION(JPEMISMAX_S):: CSNAP_MONTHLY_FILE
 CHARACTER(LEN=28), DIMENSION(JPEMISMAX_S):: CSNAP_DAILY_FILE
 CHARACTER(LEN=28), DIMENSION(JPEMISMAX_S):: CSNAP_HOURLY_FILE
 CHARACTER(LEN=50), DIMENSION(JPEMISMAX_S,JPSNAPMAX):: CSNAP_POTENTIAL_FILE
 CHARACTER(LEN=6),  DIMENSION(JPEMISMAX_S)          :: CSNAP_POTENTIAL_FILETYPE
REAL,              DIMENSION(JPEMISMAX_S,JPSNAPMAX):: XUNIF_SNAP
 CHARACTER(LEN=50)                                :: CDELTA_LEGAL_TIME_FILE
 CHARACTER(LEN=6)                                 :: CDELTA_LEGAL_TIME_FILETYPE
REAL                                             :: XUNIF_DELTA_LEGAL_TIME
INTEGER :: NEMIS_NBR
INTEGER :: NEMIS_SNAP
!
!
NAMELIST/NAM_CH_SNAP_EMIS_PGD/ NEMIS_NBR, NEMIS_SNAP, CEMIS_NAME,&
                               CEMIS_COMMENT,                    &
                               CSNAP_MONTHLY_FILE,               &
                               CSNAP_DAILY_FILE,                 &
                               CSNAP_HOURLY_FILE,                &
                               CSNAP_POTENTIAL_FILE,             &
                               CSNAP_POTENTIAL_FILETYPE,         &
                               XUNIF_SNAP,                       &
                               XUNIF_DELTA_LEGAL_TIME,           &
                               CDELTA_LEGAL_TIME_FILE,           &
                               CDELTA_LEGAL_TIME_FILETYPE
 !-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!
IF (LHOOK) CALL DR_HOOK('PGD_CHEMISTRY_SNAP',0,ZHOOK_HANDLE)
NEMIS_NBR  = 0
CEMIS_NAME(:)              = '                           '
CEMIS_COMMENT(:)           = ''
!
NEMIS_SNAP = 0
NSNAP_M   = 12
NSNAP_D   = 7
NSNAP_H   = 24
XUNIF_SNAP             = XUNDEF
XUNIF_DELTA_LEGAL_TIME = XUNDEF
CSNAP_MONTHLY_FILE(:)      = '                           '
CSNAP_DAILY_FILE(:)        = '                           '
CSNAP_HOURLY_FILE(:)       = '                           '
CSNAP_POTENTIAL_FILETYPE(:)= '      '
CSNAP_POTENTIAL_FILE(:,:)  = '                           '
CDELTA_LEGAL_TIME_FILETYPE = '      '
CDELTA_LEGAL_TIME_FILE     = '                           '
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_CH_SNAP_EMIS_PGD',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CH_SNAP_EMIS_PGD)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocation
!             ----------
!
NEMIS_NBR_n  = NEMIS_NBR
NEMIS_SNAP_n = NEMIS_SNAP
!
ALLOCATE(CEMIS_NAME_n(NEMIS_NBR))
ALLOCATE(CEMIS_COMMENT_n(NEMIS_NBR))
!
ALLOCATE(XSNAP_MONTHLY (NSNAP_M,NEMIS_SNAP,NEMIS_NBR))
ALLOCATE(XSNAP_DAILY   (NSNAP_D,NEMIS_SNAP,NEMIS_NBR))
ALLOCATE(XSNAP_HOURLY  (NSNAP_H,NEMIS_SNAP,NEMIS_NBR))
!
CEMIS_NAME_n         (:) = CEMIS_NAME   (1:NEMIS_NBR)
CEMIS_COMMENT_n      (:) = CEMIS_COMMENT(1:NEMIS_NBR)
!
ALLOCATE(XEMIS_FIELDS_SNAP(NL,NEMIS_SNAP,NEMIS_NBR))
!
LEMIS_FIELDS = .FALSE.
!
!-------------------------------------------------------------------------------
OCH_EMIS = NEMIS_NBR > 0
!-------------------------------------------------------------------------------
!
!*    4.      Computes Potential maps for each snap and reads temporal profiles
!             -----------------------------------------------------------------
!
YSNAP_TIME_REF = '     '
!
DO JSPEC=1,NEMIS_NBR

  CALL PGD_SNAP_TEMP_PROFILE('ASCII  ',CSNAP_MONTHLY_FILE(JSPEC),XSNAP_MONTHLY(:,:,JSPEC),NEMIS_SNAP,NSNAP_M)
  CALL PGD_SNAP_TEMP_PROFILE('ASCII  ',CSNAP_DAILY_FILE(JSPEC),  XSNAP_DAILY(:,:,JSPEC),NEMIS_SNAP,NSNAP_D)
  CALL PGD_SNAP_TEMP_PROFILE('ASCII  ',CSNAP_HOURLY_FILE(JSPEC), XSNAP_HOURLY(:,:,JSPEC),NEMIS_SNAP,NSNAP_H,CSNAP_TIME_REF)

  IF (JSPEC==1) YSNAP_TIME_REF = CSNAP_TIME_REF
  IF (YSNAP_TIME_REF/=CSNAP_TIME_REF) THEN
    CALL ABOR1_SFX('ALL SNAP HOURLY PROFILES MUST HAVE THE SAME TIME REFERENCE')
  END IF

  DO JSNAP=1,NEMIS_SNAP
    CATYPE = 'ARI'
    CALL PGD_FIELD(HPROGRAM,'SNAP','ALL',CSNAP_POTENTIAL_FILE(JSPEC,JSNAP), &
                   CSNAP_POTENTIAL_FILETYPE(JSPEC),XUNIF_SNAP(JSPEC,JSNAP), &
                   XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC)                         )
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    5.      Computes legal time map if legal time option is used
!             ----------------------------------------------------
!
IF (CSNAP_TIME_REF=='LEGAL') THEN
  ALLOCATE(XDELTA_LEGAL_TIME(NL))
  CALL PGD_FIELD(HPROGRAM,'LEGAL_TIME','ALL', CDELTA_LEGAL_TIME_FILE, &
                 CDELTA_LEGAL_TIME_FILETYPE,XUNIF_DELTA_LEGAL_TIME,   &
                 XDELTA_LEGAL_TIME(:)                                 )
  !* conversion from seconds to hours
  !  Beware: 
  !  one uses the fact here that no legal hour increment is less more than 24h. 
  !  Legal hour is either zero (in which case division has no effect) 
  !  or specified unit is second
  WHERE(ABS(XDELTA_LEGAL_TIME(:))>=24.) &
  XDELTA_LEGAL_TIME(:) = XDELTA_LEGAL_TIME(:) / 3600.
END IF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_CHEMISTRY_SNAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_CHEMISTRY_SNAP
