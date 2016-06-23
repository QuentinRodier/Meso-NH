!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_0D (DTCO, DGU, UG, U, USS, &
                                       HPROGRAM, HATYPE,  HNAME, HTYPE, HFNAM, &
                                        HFTYP, PUNIF, PFIELD, OPRESENT)
!     ##############################################################
!
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    16/11/10
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_PGDWORK,       ONLY : CATYPE
!
USE MODI_PGD_FIELD
USE MODI_READ_FROM_SURFEX_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), INTENT(IN) :: HFTYP
REAL, INTENT(IN) :: PUNIF
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_DATA_0D',0,ZHOOK_HANDLE)
!
PFIELD(:)=XUNDEF
OPRESENT=.FALSE.
!
IF (HFTYP.EQ.'FA    ' .OR. HFTYP.EQ.'ASCII ' .OR. HFTYP.EQ.'LFI   ') THEN

  OPRESENT=.TRUE.
  SELECT CASE (HTYPE)
    CASE ('LAN')
      CALL READ_FROM_SURFEX_FILE(DTCO, DGU, U, &
                                 HFTYP,HFNAM,'SURF  ','      ',PFIELD)
    CASE ('TWN')
      CALL READ_FROM_SURFEX_FILE(DTCO, DGU, U, &
                                 HFTYP,HFNAM,'TOWN  ','      ',PFIELD)              
    CASE ('NAT')
      CALL READ_FROM_SURFEX_FILE(DTCO, DGU, U, &
                                 HFTYP,HFNAM,'NATURE','      ',PFIELD)              
    CASE ('SEA')
      CALL READ_FROM_SURFEX_FILE(DTCO, DGU, U, &
                                 HFTYP,HFNAM,'SEA   ','      ',PFIELD)              
    CASE ('WAT')
      CALL READ_FROM_SURFEX_FILE(DTCO, DGU, U, &
                                 HFTYP,HFNAM,'WATER ','      ',PFIELD)              
   END SELECT

ELSE

  CATYPE = HATYPE
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,HNAME,HTYPE,HFNAM,HFTYP,PUNIF,PFIELD(:),OPRESENT=OPRESENT)

ENDIF
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_DATA_0D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_0D

