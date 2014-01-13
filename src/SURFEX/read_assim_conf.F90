!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ASSIM_CONF(HPROGRAM)
!     #######################################################
!
!!****  *READ_ASSIM_CONF* - routine to read the configuration for assimilation
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
!!	T. Aspelien met.no
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_POS_SURF, ONLY : POSNAM
USE MODN_ASSIM,    ONLY : NAM_NACVEG,NAM_ASSIM,LASSIM,CASSIM,&
                          NAM_IO_VARASSIM,NAM_OBS,NAM_VAR
!
USE YOMHOOK,       ONLY : LHOOK,DR_HOOK
USE PARKIND1,      ONLY : JPRB
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_TEST_NAM_VAR_SURF  
USE MODI_CLOSE_NAMELIST
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_ASSIM_CONF',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)

!* open namelist file
 CALL OPEN_NAMELIST(HPROGRAM,INAM)

!* reading of namelist
 CALL POSNAM(INAM,'NAM_ASSIM',      GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_ASSIM)
 CALL POSNAM(INAM,'NAM_NACVEG',     GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_NACVEG)
 CALL POSNAM(INAM,'NAM_IO_VARASSIM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_IO_VARASSIM)
 CALL POSNAM(INAM,'NAM_OBS',        GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_OBS)
 CALL POSNAM(INAM,'NAM_VAR',        GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_VAR)

!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CASSIM',CASSIM,'PLUS ','2DVAR','AVERA')

!* close namelist file
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)

IF (LHOOK) CALL DR_HOOK('READ_ASSIM_CONF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_ASSIM_CONF
