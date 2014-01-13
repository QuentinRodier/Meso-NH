!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_FLAKE_SBL_n(HPROGRAM)
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
!!	V. Masson   *Meteo France*	
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
USE MODD_SURF_PAR,      ONLY : XUNDEF
USE MODD_FLAKE_n,       ONLY : LSBL
USE MODD_FLAKE_SBL_n,   ONLY : NLVL, XZ, XU, XT, XQ, XTKE, XLMO, XDZ, XZF, XDZF, XP
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
 CALL GET_TYPE_DIM_n('WATER ',ILU)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!* flag to use or not SBL levels
!
YRECFM='WAT_SBL'
 CALL READ_SURF(HPROGRAM,YRECFM,LSBL,IRESP)
!
IF (.NOT.LSBL) THEN
  ALLOCATE(XZ  (0,0))
  ALLOCATE(XU  (0,0))
  ALLOCATE(XT  (0,0))
  ALLOCATE(XQ  (0,0))
  ALLOCATE(XTKE(0,0))
  ALLOCATE(XLMO(0)  )
  ALLOCATE(XP  (0,0))
  ALLOCATE(XDZ (0,0))
  ALLOCATE(XZF (0,0))
  ALLOCATE(XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_SBL_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='WAT_SBL_LVL'
 CALL READ_SURF(HPROGRAM,YRECFM,NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(XZ(ILU,NLVL))
!
DO JLAYER=1,NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Z',JLAYER,' '
  CALL READ_SURF(HPROGRAM,YRECFM,XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(XU  (ILU,NLVL))
ALLOCATE(XT  (ILU,NLVL))
ALLOCATE(XQ  (ILU,NLVL))
ALLOCATE(XTKE(ILU,NLVL))
ALLOCATE(XLMO(ILU)     )
ALLOCATE(XP  (ILU,NLVL))
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  YRECFM='STORAGETYPE'
  CALL READ_SURF(HPROGRAM,YRECFM,YREAD,IRESP)
ELSE
  YREAD = 'ALL'
ENDIF
!
IF(YREAD=='ALL') THEN
  !
  !* wind in SBL
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_U',JLAYER,' '
    CALL READ_SURF(HPROGRAM,YRECFM,XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_T',JLAYER,' '
    CALL READ_SURF(HPROGRAM,YRECFM,XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_Q',JLAYER,' '
    CALL READ_SURF(HPROGRAM,YRECFM,XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_E',JLAYER,' '
    CALL READ_SURF(HPROGRAM,YRECFM,XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  YRECFM='WAT_SBL_LMO     '
  CALL READ_SURF(HPROGRAM,YRECFM,XLMO(:),IRESP)
  !
  !* Pressure
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'WAT_SBL_P',JLAYER,' '
    CALL READ_SURF(HPROGRAM,YRECFM,XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  XU  (:,:) = XUNDEF
  XT  (:,:) = XUNDEF
  XQ  (:,:) = XUNDEF
  XTKE(:,:) = XUNDEF
  XLMO(:)   = XUNDEF
  XP  (:,:) = XUNDEF
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
ALLOCATE(XDZ (ILU,NLVL))
ALLOCATE(XZF (ILU,NLVL))
ALLOCATE(XDZF(ILU,NLVL))
 CALL CANOPY_GRID(ILU,NLVL,XZ,XZF,XDZ,XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_SBL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_FLAKE_SBL_n
