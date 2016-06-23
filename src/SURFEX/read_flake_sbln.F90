!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_FLAKE_SBL_n (DTCO, U, F, FSB, &
                                   HPROGRAM)
!     #########################################
!
!!****  *READ_FLAKE_SBL_n* - reads FLAKE fields
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
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_FLAKE_SBL_n, ONLY : FLAKE_SBL_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,      ONLY : XUNDEF
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
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(FLAKE_SBL_t), INTENT(INOUT) :: FSB
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
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
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_SBL_N',0,ZHOOK_HANDLE)
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'WATER ',ILU)
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
YRECFM='WAT_SBL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%LSBL,IRESP)
!
IF (.NOT.F%LSBL) THEN
  ALLOCATE(FSB%XZ  (0,0))
  ALLOCATE(FSB%XU  (0,0))
  ALLOCATE(FSB%XT  (0,0))
  ALLOCATE(FSB%XQ  (0,0))
  ALLOCATE(FSB%XTKE(0,0))
  ALLOCATE(FSB%XLMO(0)  )
  ALLOCATE(FSB%XP  (0,0))
  ALLOCATE(FSB%XDZ (0,0))
  ALLOCATE(FSB%XZF (0,0))
  ALLOCATE(FSB%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='WAT_SBL_LVL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(FSB%XZ(ILU,FSB%NLVL))
!
DO JLAYER=1,FSB%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Z',JLAYER,' '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(FSB%XU  (ILU,FSB%NLVL))
ALLOCATE(FSB%XT  (ILU,FSB%NLVL))
ALLOCATE(FSB%XQ  (ILU,FSB%NLVL))
ALLOCATE(FSB%XTKE(ILU,FSB%NLVL))
ALLOCATE(FSB%XLMO(ILU)     )
ALLOCATE(FSB%XP  (ILU,FSB%NLVL))
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
  DO JLAYER=1,FSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_U',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,FSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_T',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,FSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Q',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,FSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_E',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  YRECFM='WAT_SBL_LMO     '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XLMO(:),IRESP)
  !
  !* Pressure
  DO JLAYER=1,FSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_P',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,FSB%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  FSB%XU  (:,:) = XUNDEF
  FSB%XT  (:,:) = XUNDEF
  FSB%XQ  (:,:) = XUNDEF
  FSB%XTKE(:,:) = XUNDEF
  FSB%XLMO(:)   = XUNDEF
  FSB%XP  (:,:) = XUNDEF
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
ALLOCATE(FSB%XDZ (ILU,FSB%NLVL))
ALLOCATE(FSB%XZF (ILU,FSB%NLVL))
ALLOCATE(FSB%XDZF(ILU,FSB%NLVL))
 CALL CANOPY_GRID(ILU,FSB%NLVL,FSB%XZ,FSB%XZF,FSB%XDZ,FSB%XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_SBL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_FLAKE_SBL_n
