!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SEAFLUX_SBL_n (DGU, U, &
                                           S, SSB, &
                                          HPROGRAM,HWRITE)
!     ####################################
!
!!****  *WRITE_SEAFLUX_n* - writes SEAFLUX fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      E. Martin   01/2012 avoid write of XUNDEF fields
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SEAFLUX_SBL_n, ONLY : SEAFLUX_SBL_t
!
USE MODI_WRITE_SURF
!
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
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SEAFLUX_SBL_t), INTENT(INOUT) :: SSB
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HWRITE   ! 'PREP' : does not write SBL XUNDEF fields
!                                          ! 'ALL' : all fields are written
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER :: JLAYER  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     Prognostic fields:
!               -----------------
!
!* flag to define if SBL is computed
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_SBL_N',0,ZHOOK_HANDLE)
YRECFM='SEA_SBL'
YCOMMENT='flag to use SBL levels'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,S%LSBL,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. S%LSBL .AND. LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
IF (.NOT. S%LSBL) RETURN
!
!* number of levels
!
YRECFM='SEA_SBL_LVL'
YCOMMENT='number of SBL levels'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JLAYER=1,SSB%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_Z',JLAYER,' '
  YCOMMENT='altitudes of SBL levels (m)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XZ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in SBL
  !
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_U',JLAYER,' '
    YCOMMENT='wind at SBL levels (m/s)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XU(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* temperature in SBL
  !
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_T',JLAYER,' '
    YCOMMENT='temperature at SBL levels (K)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XT(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* humidity in SBL
  !
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_Q',JLAYER,' '
    YCOMMENT='humidity at SBL levels (kg/m3)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XQ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Tke in SBL
  !
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_E',JLAYER,' '
    YCOMMENT='Tke at SBL levels (m2/s2)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XTKE(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Monin-Obhukov length
  !
  YRECFM='SEA_SBL_LMO '
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XLMO(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !* Air pressure in SBL
  !
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_P',JLAYER,' '
    YCOMMENT='Pressure at SBL levels (Pa)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,SSB%XP(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SEAFLUX_SBL_n
