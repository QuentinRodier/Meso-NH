!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SEAFLUX_SBL_n (DTCO, S, SSB, U, &
                                     HPROGRAM)
!     #########################################
!
!!****  *READ_SEAFLUX_SBL_n* - reads SEAFLUX fields
!!                        
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
!!      E. Martin   01/2012 Add LSBL_COLD_START
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SEAFLUX_SBL_n, ONLY : SEAFLUX_SBL_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_READ_SURF
USE MODI_CANOPY_GRID
USE MODI_GET_TYPE_DIM_n
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SEAFLUX_SBL_t), INTENT(INOUT) :: SSB
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=3)  :: YREAD
INTEGER :: ILU     ! 1D physical dimension
INTEGER :: IRESP   ! Error code after redding
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: IVERSION, IBUGFIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_SBL_N',0,ZHOOK_HANDLE)
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'SEA   ',ILU)
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!* flag to use or not SBL levels
!
YRECFM='SEA_SBL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,S%LSBL,IRESP)
!
IF (.NOT.S%LSBL) THEN
  ALLOCATE(SSB%XZ  (0,0))
  ALLOCATE(SSB%XU  (0,0))
  ALLOCATE(SSB%XT  (0,0))
  ALLOCATE(SSB%XQ  (0,0))
  ALLOCATE(SSB%XTKE(0,0))
  ALLOCATE(SSB%XLMO(0)  )
  ALLOCATE(SSB%XP  (0,0))
  ALLOCATE(SSB%XDZ (0,0))
  ALLOCATE(SSB%XZF (0,0))
  ALLOCATE(SSB%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='SEA_SBL_LVL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(SSB%XZ(ILU,SSB%NLVL))
!
DO JLAYER=1,SSB%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_Z',JLAYER,' '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(SSB%XU  (ILU,SSB%NLVL))
ALLOCATE(SSB%XT  (ILU,SSB%NLVL))
ALLOCATE(SSB%XQ  (ILU,SSB%NLVL))
ALLOCATE(SSB%XTKE(ILU,SSB%NLVL))
ALLOCATE(SSB%XLMO(ILU)     )
ALLOCATE(SSB%XP  (ILU,SSB%NLVL))
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  YRECFM='STORAGETYPE'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,YREAD,IRESP)
ELSE
  YREAD = 'ALL'
ENDIF
!
IF(YREAD=='ALL') THEN
  !
  !* wind in SBL
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_U',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_T',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_Q',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_E',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  YRECFM='SEA_SBL_LMO '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XLMO(:),IRESP)
  !
  !* Pressure
  DO JLAYER=1,SSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SEA_SBL_P',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,SSB%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  SSB%XU  (:,:) = XUNDEF
  SSB%XT  (:,:) = XUNDEF
  SSB%XQ  (:,:) = XUNDEF
  SSB%XTKE(:,:) = XUNDEF
  SSB%XLMO(:)   = XUNDEF
  SSB%XP  (:,:) = XUNDEF
ENDIF
!
!
!* Grid characteristics
!
!
!  --------------------------------- XZ(k+1)                     XDZ(k+1)
!                                                                           ^
!                                                                           |
!                                                                           |
!  - - - - - - - - - - - - - - - - - XZf(k+1)                               | XDZf(k+1)
!                                                              ^            |
!                                                              |            |
!  --------------------------------- XZ(k), XU, XT, XQ, XTKE   | XDZ(k)     V
!                                                              |            ^
!  - - - - - - - - - - - - - - - - - XZf(k)                    V            | XDZf(k)
!  --------------------------------- XZ(k-1)                     XDZ(k-1)   V
!  - - - - - - - - - - - - - - - - - XZf(k-1)
!
ALLOCATE(SSB%XDZ (ILU,SSB%NLVL))
ALLOCATE(SSB%XZF (ILU,SSB%NLVL))
ALLOCATE(SSB%XDZF(ILU,SSB%NLVL))
 CALL CANOPY_GRID(ILU,SSB%NLVL,SSB%XZ,SSB%XZF,SSB%XDZ,SSB%XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SEAFLUX_SBL_n
