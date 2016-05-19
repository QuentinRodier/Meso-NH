!     #########
      SUBROUTINE READ_NAM_PGD_OROGRAPHY(HPROGRAM, HZS, HFILETYPE, PUNIF_ZS, &
                                          HOROGTYPE, PENV, OIMP_ZS )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_OROGRAPHY* reads namelist for Orography
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
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2010
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!                                   
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM    ! Type of program
 CHARACTER(LEN=28),   INTENT(OUT)   :: HZS         ! file name for orography
 CHARACTER(LEN=6),    INTENT(OUT)   :: HFILETYPE   ! data file type
REAL,                INTENT(OUT)   :: PUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3),    INTENT(OUT)   :: HOROGTYPE   ! orogpraphy type 
REAL,                INTENT(OUT)   :: PENV        ! parameter for enveloppe orography:
LOGICAL,             INTENT(OUT)   :: OIMP_ZS     ! Imposed orography from another PGD file
!                                  
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YZS         ! file name for orography
 CHARACTER(LEN=6)         :: YFILETYPE   ! data file type
REAL                     :: XUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3)         :: COROGTYPE   ! orogpraphy type 
!                                       ! 'AVG' : average orography
!                                       ! 'SIL' : silhouette orography
!                                       ! 'ENV' : enveloppe orography
REAL                     :: XENV        ! parameter for enveloppe orography:
!                                       ! zs = avg_zs + XENV * SSO_STEDV
LOGICAL                  :: LIMP_ZS     ! Imposed orography from another PGD file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_ZS/YZS, YFILETYPE, XUNIF_ZS, COROGTYPE, XENV, LIMP_ZS  
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_OROGRAPHY',0,ZHOOK_HANDLE)
XUNIF_ZS       = XUNDEF
YZS            = '                          '
YFILETYPE      = '      '
!
COROGTYPE      = 'ENV'
XENV           = 0.
LIMP_ZS        = .FALSE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_ZS',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_ZS)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
HZS       = YZS       ! file name for orography
HFILETYPE = YFILETYPE ! data file type
PUNIF_ZS  = XUNIF_ZS  ! uniform orography
HOROGTYPE = COROGTYPE ! orogpraphy type 
PENV      = XENV      ! parameter for enveloppe orography:
OIMP_ZS   = LIMP_ZS   ! Imposed orography from another PGD file
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_OROGRAPHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_OROGRAPHY
