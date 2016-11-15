!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_CANOPY_n (DTCO, ICP, I, U, &
                                     HPROGRAM)
!     #########################################
!
!!****  *READ_ISBA_CANOPY_n* - reads ISBA fields
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
USE MODD_ISBA_CANOPY_n, ONLY : ISBA_CANOPY_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
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
TYPE(ISBA_CANOPY_t), INTENT(INOUT) :: ICP
TYPE(ISBA_t), INTENT(INOUT) :: I
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
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: ILU     ! 1D physical dimension
INTEGER :: IRESP   ! Error code after redding
INTEGER :: IVERSION, IBUGFIX  ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CANOPY_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_NATURE'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'NATURE',ILU)
!
!
!* flag to use or not canopy levels
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<3) THEN
  I%LCANOPY = .FALSE.
ELSE
  YRECFM='ISBA_CANOPY'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,I%LCANOPY,IRESP)
END IF
!
IF (.NOT.I%LCANOPY) THEN
  ALLOCATE(ICP%XZ  (0,0))
  ALLOCATE(ICP%XU  (0,0))
  ALLOCATE(ICP%XT  (0,0))
  ALLOCATE(ICP%XQ  (0,0))
  ALLOCATE(ICP%XTKE(0,0))
  ALLOCATE(ICP%XLMO(0)  )
  ALLOCATE(ICP%XP  (0,0))
  ALLOCATE(ICP%XDZ (0,0))
  ALLOCATE(ICP%XZF (0,0))
  ALLOCATE(ICP%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_ISBA_CANOPY_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='ISBA_CAN_LVL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(ICP%XZ(ILU,ICP%NLVL))
!
DO JLAYER=1,ICP%NLVL
  WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_Z',JLAYER
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(ICP%XU  (ILU,ICP%NLVL))
ALLOCATE(ICP%XT  (ILU,ICP%NLVL))
ALLOCATE(ICP%XQ  (ILU,ICP%NLVL))
ALLOCATE(ICP%XTKE(ILU,ICP%NLVL))
ALLOCATE(ICP%XLMO(ILU)     )
ALLOCATE(ICP%XP  (ILU,ICP%NLVL))
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
  DO JLAYER=1,ICP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_U',JLAYER
    CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,ICP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_T',JLAYER
    CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,ICP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_Q',JLAYER
    CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,ICP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_E',JLAYER
    CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  YRECFM='ISBA_CAN_LMO     '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XLMO(:),IRESP)
  !
  !* Pressure
  DO JLAYER=1,ICP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'ISBA_CAN_P',JLAYER
    CALL READ_SURF(&
                HPROGRAM,YRECFM,ICP%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  ICP%XU  (:,:) = XUNDEF
  ICP%XT  (:,:) = XUNDEF
  ICP%XQ  (:,:) = XUNDEF
  ICP%XTKE(:,:) = XUNDEF
  ICP%XLMO(:)   = XUNDEF
  ICP%XP  (:,:) = XUNDEF
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
ALLOCATE(ICP%XDZ (ILU,ICP%NLVL))
ALLOCATE(ICP%XZF (ILU,ICP%NLVL))
ALLOCATE(ICP%XDZF(ILU,ICP%NLVL))
 CALL CANOPY_GRID(ILU,ICP%NLVL,ICP%XZ,ICP%XZF,ICP%XDZ,ICP%XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CANOPY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_CANOPY_n
