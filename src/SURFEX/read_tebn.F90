!     #########
      SUBROUTINE READ_TEB_n(HPROGRAM,KPATCH)
!     #########################################
!
!!****  *READ_TEB_n* - reads TEB fields
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODD_TEB_n,          ONLY : NROOF_LAYER, XT_ROOF, XWS_ROOF, &
                                  NROAD_LAYER, XT_ROAD, XWS_ROAD, &
                                  NWALL_LAYER,XT_WALL_A,XT_WALL_B,&
                                  XTI_ROAD, CBEM,                 &
                                  TSNOW_ROOF, TSNOW_ROAD,         &
                                  XT_CANYON, XQ_CANYON,           &
                                  NTEB_PATCH, CROAD_DIR, CWALL_OPT
USE MODD_BEM_n, ONLY : NFLOOR_LAYER, XT_FLOOR, XT_MASS,           &
                       XT_WIN1, XT_WIN2, XQI_BLD, XTI_BLD                                   
!
USE MODI_READ_SURF
!
USE MODI_INIT_IO_SURF_n
USE MODI_SET_SURFEX_FILEIN
USE MODI_END_IO_SURF_n
USE MODI_TOWN_PRESENCE
USE MODI_ALLOCATE_GR_SNOW
USE MODI_READ_GR_SNOW
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KPATCH   ! current patch number
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL           :: GTOWN          ! town variables written in the file
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=3)  :: YPATCH         ! suffix if more than 1 patch
!
INTEGER           :: IVERSION, IBUGFIX
LOGICAL           :: GOLD_NAME      ! name of temperatures in old versions of SURFEX
!
INTEGER :: JLAYER  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n('TOWN  ',ILU)
!
YPATCH='   '
IF (NTEB_PATCH>1) WRITE(YPATCH,FMT='(A1,I1,A1)') 'T',KPATCH,'_'
!  
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
GOLD_NAME = (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<=2))
!
!*       2.     Prognostic fields:
!               -----------------
!
!* roof temperatures
!
ALLOCATE(XT_ROOF(ILU,NROOF_LAYER))
!
DO JLAYER=1,NROOF_LAYER
  WRITE(YRECFM,'(A3,A5,I1.1)') YPATCH,'TROOF',JLAYER
  YRECFM=ADJUSTL(YRECFM)
  IF (GOLD_NAME) WRITE(YRECFM,'(A6,I1.1)') 'T_ROOF',JLAYER

 CALL READ_SURF(HPROGRAM,YRECFM,XT_ROOF(:,JLAYER),IRESP)
END DO
!
!* roof water content
!
ALLOCATE(XWS_ROOF(ILU))
!
YRECFM=YPATCH//'WS_ROOF'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XWS_ROOF(:),IRESP)
!
!* road temperatures
!
ALLOCATE(XT_ROAD(ILU,NROAD_LAYER))
!
DO JLAYER=1,NROAD_LAYER
  WRITE(YRECFM,'(A3,A5,I1.1)') YPATCH,'TROAD',JLAYER
  YRECFM=ADJUSTL(YRECFM)
  IF (GOLD_NAME) WRITE(YRECFM,'(A6,I1.1)') 'T_ROAD',JLAYER
 CALL READ_SURF(HPROGRAM,YRECFM,XT_ROAD(:,JLAYER),IRESP)
END DO
!
!* road water content
!
ALLOCATE(XWS_ROAD(ILU))
!
YRECFM=YPATCH//'WS_ROAD'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XWS_ROAD(:),IRESP)
!
!* wall temperatures
!
ALLOCATE(XT_WALL_A(ILU,NWALL_LAYER))
ALLOCATE(XT_WALL_B(ILU,NWALL_LAYER))
!
DO JLAYER=1,NWALL_LAYER
  IF (CWALL_OPT=='UNIF' .OR. GOLD_NAME) THEN
    WRITE(YRECFM,'(A3,A5,I1.1)') YPATCH,'TWALL',JLAYER
    YRECFM=ADJUSTL(YRECFM)
    IF (GOLD_NAME) WRITE(YRECFM,'(A6,I1.1)') 'T_WALL',JLAYER
    CALL READ_SURF(HPROGRAM,YRECFM,XT_WALL_A(:,JLAYER),IRESP)
    !
    XT_WALL_B = XT_WALL_A
  ELSE
    WRITE(YRECFM,'(A3,A6,I1.1)') YPATCH,'TWALLA',JLAYER
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,XT_WALL_A(:,JLAYER),IRESP)
    !
    WRITE(YRECFM,'(A3,A6,I1.1)') YPATCH,'TWALLB',JLAYER
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,XT_WALL_B(:,JLAYER),IRESP)
  END IF
END DO
!
!* internal building temperature
!
ALLOCATE(XTI_BLD(ILU))
!
YRECFM=YPATCH//'TI_BLD'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XTI_BLD(:),IRESP)

!
!* outdoor window temperature
!
ALLOCATE(XT_WIN1(ILU))
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
   YRECFM=YPATCH//'T_WIN1'
   YRECFM=ADJUSTL(YRECFM)
   CALL READ_SURF(HPROGRAM,YRECFM,XT_WIN1(:),IRESP)
ELSE
   XT_WIN1(:)=XUNDEF
ENDIF
!
!
!* internal building specific humidity
!
ALLOCATE(XQI_BLD(ILU))
!
IF (CBEM=='BEM' .AND. (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM=YPATCH//'QI_BLD'
   YRECFM=ADJUSTL(YRECFM)
   CALL READ_SURF(HPROGRAM,YRECFM,XQI_BLD(:),IRESP)
ELSE
   XQI_BLD(:) = XUNDEF
ENDIF
!
IF (CBEM=='BEM') THEN
  !
  !* indoor window temperature
  !
  ALLOCATE(XT_WIN2(ILU))
  !
  YRECFM=YPATCH//'T_WIN2'
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,XT_WIN2(:),IRESP)        
  !
  !* floor temperatures
  !
  ALLOCATE(XT_FLOOR(ILU,NFLOOR_LAYER))
  !
  DO JLAYER=1,NFLOOR_LAYER
    WRITE(YRECFM,'(A3,A5,I1.1)') YPATCH,'TFLOO',JLAYER
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,XT_FLOOR(:,JLAYER),IRESP)
  END DO
  !
  !* mass temperatures
  !
  ALLOCATE(XT_MASS(ILU,NFLOOR_LAYER))
  !
  DO JLAYER=1,NFLOOR_LAYER
    WRITE(YRECFM,'(A3,A5,I1.1)') YPATCH,'TMASS',JLAYER
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,XT_MASS(:,JLAYER),IRESP)
  END DO
  !
ELSE 
  ALLOCATE(XT_WIN2(0))
  ALLOCATE(XT_FLOOR(0,0))
  ALLOCATE(XT_MASS(0,0))
ENDIF
!
!* deep road temperature
!
ALLOCATE(XTI_ROAD(ILU))
!
YRECFM=YPATCH//'TI_ROAD'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XTI_ROAD(:),IRESP)
!
!
!* snow mantel
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL TOWN_PRESENCE(HPROGRAM,GTOWN)
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP')
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
IF (.NOT. GTOWN) THEN
  TSNOW_ROAD%SCHEME='1-L'
  CALL ALLOCATE_GR_SNOW(TSNOW_ROAD,ILU,1)
  TSNOW_ROOF%SCHEME='1-L'
  CALL ALLOCATE_GR_SNOW(TSNOW_ROOF,ILU,1)  
ELSE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_GR_SNOW(HPROGRAM,'RD',YPATCH,ILU,1,TSNOW_ROAD  )
    CALL READ_GR_SNOW(HPROGRAM,'RF',YPATCH,ILU,1,TSNOW_ROOF  )
  ELSE
    CALL READ_GR_SNOW(HPROGRAM,'ROAD',YPATCH,ILU,1,TSNOW_ROAD  )
    CALL READ_GR_SNOW(HPROGRAM,'ROOF',YPATCH,ILU,1,TSNOW_ROOF  )
  ENDIF    
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.     Semi-prognostic fields:
!               ----------------------
!
!* temperature in canyon air
!
ALLOCATE(XT_CANYON(ILU))
XT_CANYON(:) = XT_ROAD(:,1)
!
YRECFM=YPATCH//'TCANYON'
YRECFM=ADJUSTL(YRECFM)
IF (GOLD_NAME) YRECFM='T_CANYON'
 CALL READ_SURF(HPROGRAM,YRECFM,XT_CANYON(:),IRESP)
!
!* water vapor in canyon air
!
ALLOCATE(XQ_CANYON(ILU))
XQ_CANYON(:) = 0.
!
YRECFM=YPATCH//'QCANYON'
YRECFM=ADJUSTL(YRECFM)
IF (GOLD_NAME) YRECFM='Q_CANYON'
 CALL READ_SURF(HPROGRAM,YRECFM,XQ_CANYON(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_n
