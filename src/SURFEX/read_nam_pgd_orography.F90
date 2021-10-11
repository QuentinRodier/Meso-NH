!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_OROGRAPHY(HPROGRAM, HZS, HFILETYPE, PUNIF_ZS, &
                                        HOROGTYPE, PENV, OIMP_ZS,&
                                        OORORAD, KSECTORS, PRFSSO, PHALORADIUS, KFSSOMAX, &
                                        HSVF, OFSSOSVF, &
                                        HSLOPE, HSLOPEFILETYPE, OEXPLICIT_SLOPE )  
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
!!    M Lafaysse  07/2013 : explicit slope
!!    A Mary      04/2016 : ororad
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
! ororad
LOGICAL,             INTENT(OUT)   :: OORORAD     ! activate orographic radiation parameters
INTEGER,             INTENT(OUT)   :: KSECTORS    ! number of aspect sectors
REAL,                INTENT(OUT)   :: PRFSSO      ! reduction factor for computing NFSSO
INTEGER,             INTENT(OUT)   :: KFSSOMAX    ! max for NFSSO (limit for memory reasons)
REAL,                INTENT(OUT)   :: PHALORADIUS ! radius of the halo in which the horizon is computed (m)
 CHARACTER(LEN=16),  INTENT(OUT)   :: HSVF        ! formula for SVF computation:
!                                                   'SENKOVA' = Senkova et al. 2007
!                                                   'MANNERS' = Manners et al. 2012
LOGICAL,             INTENT(OUT)   :: OFSSOSVF    ! compute SVF on fractional slopes if possible
!
CHARACTER(LEN=28),   INTENT(OUT),OPTIONAL   :: HSLOPE         ! file name for slope
CHARACTER(LEN=6),    INTENT(OUT),OPTIONAL   :: HSLOPEFILETYPE   ! data file type
LOGICAL,             INTENT(OUT),OPTIONAL   :: OEXPLICIT_SLOPE ! Slope is computed from explicit ZS field and not subgrid orography
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
 CHARACTER(LEN=6)         :: YZSFILETYPE   ! data file type
CHARACTER(LEN=28)        :: YSLOPE         ! file name for slope
CHARACTER(LEN=6)         :: YSLOPEFILETYPE   ! data file type
REAL                     :: XUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3)         :: COROGTYPE   ! orogpraphy type 
!                                       ! 'AVG' : average orography
!                                       ! 'SIL' : silhouette orography
!                                       ! 'ENV' : enveloppe orography
REAL                     :: XENV        ! parameter for enveloppe orography:
!                                       ! zs = avg_zs + XENV * SSO_STEDV
LOGICAL                  :: LIMP_ZS     ! Imposed orography from another PGD file
LOGICAL                  :: LEXPLICIT_SLOPE ! Slope is computed from explicit ZS field and not subgrid orography
!
LOGICAL                  :: LORORAD     ! activate orographic radiation parameters
INTEGER                  :: NSECTORS    ! number of aspect sectors
REAL                     :: XRFSSO      ! reduction factor for computing NFSSO
INTEGER                  :: NFSSOMAX    ! max for NFSSO (limit for memory reasons)
REAL                     :: XHALORADIUS ! radius of the halo in which the horizon is computed (m)
 CHARACTER(LEN=16)       :: CSVF        ! formula for SVF computation:
!                                         'SENKOVA' = Senkova et al. 2007
!                                         'MANNERS' = Manners et al. 2012
LOGICAL                  :: LFSSOSVF    ! compute SVF on fractional slopes if possible
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_ZS/YZS, YZSFILETYPE, XUNIF_ZS, COROGTYPE, XENV, LIMP_ZS , & 
                YSLOPE, YSLOPEFILETYPE, LEXPLICIT_SLOPE, &
                LORORAD, NSECTORS, XRFSSO, XHALORADIUS, NFSSOMAX, CSVF, LFSSOSVF
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_OROGRAPHY',0,ZHOOK_HANDLE)
XUNIF_ZS       = XUNDEF
YZS            = '                          '
YZSFILETYPE    = '      '
YSLOPE            = '                          '
YSLOPEFILETYPE      = '      '
!
COROGTYPE      = 'ENV'
XENV           = 0.
LIMP_ZS        = .FALSE.
LEXPLICIT_SLOPE=.FALSE.
!
LORORAD        = .FALSE.
NSECTORS       = 8
XRFSSO         = 1.
NFSSOMAX       = 30
XHALORADIUS    = 20000.
CSVF           = 'MANNERS'
LFSSOSVF       = .FALSE.
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
HFILETYPE = YZSFILETYPE ! data file type
IF (PRESENT(HSLOPE)) THEN
  HSLOPE       = YSLOPE       ! file name for slope
  HSLOPEFILETYPE = YSLOPEFILETYPE ! data file type
END IF
PUNIF_ZS  = XUNIF_ZS  ! uniform orography
HOROGTYPE = COROGTYPE ! orogpraphy type 
PENV      = XENV      ! parameter for enveloppe orography:
OIMP_ZS   = LIMP_ZS   ! Imposed orography from another PGD file
OORORAD   = LORORAD
IF (OORORAD) THEN
  KSECTORS  = NSECTORS
ELSE
  KSECTORS = 0 ! Ororad not activated, sectorial fields not computed
ENDIF
PRFSSO    = XRFSSO
KFSSOMAX  = NFSSOMAX
PHALORADIUS = XHALORADIUS
HSVF      = CSVF
OFSSOSVF  = LFSSOSVF
IF (PRESENT(OEXPLICIT_SLOPE)) THEN
    OEXPLICIT_SLOPE=LEXPLICIT_SLOPE ! Slope is computed from explicit ZS field and not subgrid orography
END IF
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_OROGRAPHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_OROGRAPHY
