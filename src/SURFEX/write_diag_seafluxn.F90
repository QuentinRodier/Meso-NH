!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_SEAFLUX_n (DTCO, DUO, U, SM, HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_SEAFLUX_n * - diagnostics for SEAFLUX
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    09/2013 : S. Senesi : call WRITE_DIAG_SEB_SEAICE_n
!!------------------------------------------------------------------
!
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_n, ONLY : SEAFLUX_MODEL_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_WRITE_DIAG_SEB_SEAFLUX_n
USE MODI_WRITE_DIAG_SEB_OCEAN_n
USE MODI_WRITE_DIAG_SEB_SEAICE_n
USE MODI_WRITE_DIAG_MISC_SEAICE_n
! 
USE MODI_WRITE_DIAG_MIP_SEAFLUX_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t),  INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
!                                            ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
INTEGER         :: ITIME_RATIO
LOGICAL         :: GWRITE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEAFLUX_N',0,ZHOOK_HANDLE)
!
ITIME_RATIO=NINT(SM%S%TTIME%TIME/SM%SD%O%XDIAG_TSTEP)
GWRITE     =(SM%SD%O%XDIAG_TSTEP==XUNDEF.OR.ABS(ITIME_RATIO*SM%SD%O%XDIAG_TSTEP-SM%S%TTIME%TIME)<1.E-3)
!
! * Write Diag other than pgd
!
IF (GWRITE.AND.HWRITE/='PGD'.AND.SM%SD%O%LDIAG_MIP) THEN
!
! * Write cmip Diag
!
    CALL WRITE_DIAG_MIP_SEAFLUX_n(DTCO, DUO, U, SM%S, SM%SD%D, SM%SD%DMI, HPROGRAM) 
!
ENDIF
!
IF (GWRITE.AND.HWRITE/='PGD') THEN
!
! * Write original surfex diag
!
    CALL WRITE_DIAG_SEB_SEAFLUX_n(DTCO, DUO, U, SM%CHS, SM%SD%O, SM%SD%D, SM%SD%DC,  &
                                     SM%S%LHANDLE_SIC, HPROGRAM)
!
    IF (SM%SD%GO%LDIAG_OCEAN) THEN
       CALL WRITE_DIAG_SEB_OCEAN_n(DTCO, DUO%CSELECT, U, SM%SD%GO, HPROGRAM)
    ENDIF
!
    IF (SM%S%LHANDLE_SIC.OR.LCPL_SEAICE) THEN
       CALL WRITE_DIAG_SEB_SEAICE_n(DTCO, DUO, U, SM%SD%O, SM%SD%DI, SM%SD%DIC, HPROGRAM)                                                
    ENDIF
!
    IF (SM%SD%DMI%LDIAG_MISC_SEAICE) THEN
       CALL WRITE_DIAG_MISC_SEAICE_n(DTCO, DUO%CSELECT, U, SM%SD%DMI, SM%S, HPROGRAM)
    ENDIF
!        
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEAFLUX_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_SEAFLUX_n
