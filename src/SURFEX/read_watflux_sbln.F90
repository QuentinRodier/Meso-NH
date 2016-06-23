!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_WATFLUX_SBL_n (DTCO, U, W, WSB, &
                                     HPROGRAM)
!     #########################################
!
!!****  *READ_WATFLUX_SBL_n* - reads WATFLUX fields
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
USE MODD_WATFLUX_SBL_n, ONLY : WATFLUX_SBL_t
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
TYPE(WATFLUX_SBL_t), INTENT(INOUT) :: WSB
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=3)  :: YREAD
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: ILU     ! 1D physical dimension
INTEGER :: IRESP   ! Error code after redding
INTEGER :: IVERSION, IBUGFIX   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_SBL_N',0,ZHOOK_HANDLE)
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'WATER ',ILU)
!
!* flag to use or not SBL levels
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
YRECFM='WAT_SBL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,W%LSBL,IRESP)
!
IF (.NOT.W%LSBL) THEN
  ALLOCATE(WSB%XZ  (0,0))
  ALLOCATE(WSB%XU  (0,0))
  ALLOCATE(WSB%XT  (0,0))
  ALLOCATE(WSB%XQ  (0,0))
  ALLOCATE(WSB%XTKE(0,0))
  ALLOCATE(WSB%XLMO(0)  )
  ALLOCATE(WSB%XP  (0,0))
  ALLOCATE(WSB%XDZ (0,0))
  ALLOCATE(WSB%XZF (0,0))
  ALLOCATE(WSB%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_SBL_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='WAT_SBL_LVL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(WSB%XZ(ILU,WSB%NLVL))
!
DO JLAYER=1,WSB%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Z',JLAYER,' '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(WSB%XU  (ILU,WSB%NLVL))
ALLOCATE(WSB%XT  (ILU,WSB%NLVL))
ALLOCATE(WSB%XQ  (ILU,WSB%NLVL))
ALLOCATE(WSB%XTKE(ILU,WSB%NLVL))
ALLOCATE(WSB%XLMO(ILU)     )
ALLOCATE(WSB%XP  (ILU,WSB%NLVL))
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
  DO JLAYER=1,WSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_U',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,WSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_T',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,WSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Q',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,WSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_E',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  YRECFM='WAT_SBL_LMO '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XLMO(:),IRESP)
  !
  !* Pressure
  DO JLAYER=1,WSB%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_P',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,WSB%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  WSB%XU  (:,:) = XUNDEF
  WSB%XT  (:,:) = XUNDEF
  WSB%XQ  (:,:) = XUNDEF
  WSB%XTKE(:,:) = XUNDEF
  WSB%XLMO(:)   = XUNDEF
  WSB%XP  (:,:) = XUNDEF
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
ALLOCATE(WSB%XDZ (ILU,WSB%NLVL))
ALLOCATE(WSB%XZF (ILU,WSB%NLVL))
ALLOCATE(WSB%XDZF(ILU,WSB%NLVL))
 CALL CANOPY_GRID(ILU,WSB%NLVL,WSB%XZ,WSB%XZF,WSB%XDZ,WSB%XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_SBL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_WATFLUX_SBL_n
