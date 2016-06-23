!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_CANOPY_n (DTCO, U, TCP, TOP, &
                                    HPROGRAM)
!     #########################################
!
!!****  *READ_TEB_CANOPY_n* - reads TEB fields
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
USE MODD_TEB_CANOPY_n, ONLY : TEB_CANOPY_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
TYPE(TEB_CANOPY_t), INTENT(INOUT) :: TCP
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=3)  :: YREAD
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: ILU     ! 1D physical dimension
INTEGER :: IRESP   ! Error code after redding
INTEGER           :: IVERSION, IBUGFIX   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_CANOPY_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',ILU)
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
  TOP%LCANOPY = .FALSE.
ELSE
  YRECFM='TEB_CANOPY'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TOP%LCANOPY,IRESP)
END IF
!
IF (.NOT.TOP%LCANOPY) THEN
  ALLOCATE(TCP%XZ  (0,0))
  ALLOCATE(TCP%XU  (0,0))
  ALLOCATE(TCP%XT  (0,0))
  ALLOCATE(TCP%XQ  (0,0))
  ALLOCATE(TCP%XTKE(0,0))
  ALLOCATE(TCP%XLMO(0,0))
  ALLOCATE(TCP%XP  (0,0))
  ALLOCATE(TCP%XLM (0,0))
  ALLOCATE(TCP%XLEPS(0,0))  
  ALLOCATE(TCP%XDZ (0,0))
  ALLOCATE(TCP%XZF (0,0))
  ALLOCATE(TCP%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_TEB_CANOPY_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
!
YRECFM='TEB_CAN_LVL'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(TCP%XZ(ILU,TCP%NLVL))
!
DO JLAYER=1,TCP%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Z',JLAYER,' '
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(TCP%XU  (ILU,TCP%NLVL))
ALLOCATE(TCP%XT  (ILU,TCP%NLVL))
ALLOCATE(TCP%XQ  (ILU,TCP%NLVL))
ALLOCATE(TCP%XTKE(ILU,TCP%NLVL))
ALLOCATE(TCP%XLMO(ILU,TCP%NLVL))
ALLOCATE(TCP%XP  (ILU,TCP%NLVL))
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
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_U',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_T',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Q',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_E',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  IF (IVERSION<7) THEN
    YRECFM='TEB_CAN_LMO ' 
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XLMO(:,1),IRESP) 
    DO JLAYER = 2,TCP%NLVL
      TCP%XLMO(:,JLAYER) = TCP%XLMO(:,1)
    ENDDO    
  ELSE
    DO JLAYER=1,TCP%NLVL
      WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_MO',JLAYER
      CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XLMO(:,JLAYER),IRESP)
    ENDDO
  ENDIF    
  !
  !* Pressure
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_P',JLAYER,' '
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TCP%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  TCP%XU  (:,:) = XUNDEF
  TCP%XT  (:,:) = XUNDEF
  TCP%XQ  (:,:) = XUNDEF
  TCP%XTKE(:,:) = XUNDEF
  TCP%XLMO(:,:) = XUNDEF
  TCP%XP  (:,:) = XUNDEF
ENDIF
!
!* mixing length
!
ALLOCATE(TCP%XLM(ILU,TCP%NLVL))
!
!* dissipative length
!
ALLOCATE(TCP%XLEPS(ILU,TCP%NLVL))
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
ALLOCATE(TCP%XDZ (ILU,TCP%NLVL))
ALLOCATE(TCP%XZF (ILU,TCP%NLVL))
ALLOCATE(TCP%XDZF(ILU,TCP%NLVL))
 CALL CANOPY_GRID(ILU,TCP%NLVL,TCP%XZ,TCP%XZF,TCP%XDZ,TCP%XDZF)
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_CANOPY_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_CANOPY_n
