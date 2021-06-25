!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_READER
!     #######################
!
INTERFACE
!
! ADNR
! 
SUBROUTINE READ_CSVDATA_FARM_ADNR(KLUNAM,HFILE,TPFARM)
        USE MODD_EOL_ADNR, ONLY: FARM
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(FARM),         INTENT(OUT) :: TPFARM       ! stored farm data
END SUBROUTINE READ_CSVDATA_FARM_ADNR
!
SUBROUTINE READ_CSVDATA_TURBINE_ADNR(KLUNAM,HFILE,TPTURBINE)
        USE MODD_EOL_ADNR, ONLY : TURBINE
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(TURBINE),      INTENT(OUT) :: TPTURBINE    ! stored turbine data
END SUBROUTINE READ_CSVDATA_TURBINE_ADNR
!
! ALM
!
SUBROUTINE READ_CSVDATA_FARM_ALM(KLUNAM,HFILE,TPFARM)
        USE MODD_EOL_ALM, ONLY: FARM
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(FARM),         INTENT(OUT) :: TPFARM       ! stored farm data
END SUBROUTINE READ_CSVDATA_FARM_ALM
!
SUBROUTINE READ_CSVDATA_TURBINE_ALM(KLUNAM,HFILE,TPTURBINE)
        USE MODD_EOL_ALM, ONLY : TURBINE
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(TURBINE),      INTENT(OUT) :: TPTURBINE    ! stored turbine data
END SUBROUTINE READ_CSVDATA_TURBINE_ALM

SUBROUTINE READ_CSVDATA_BLADE_ALM(KLUNAM,HFILE,TPTURBINE,TPBLADE)
        USE MODD_EOL_ALM, ONLY : TURBINE, BLADE
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(TURBINE),      INTENT(IN)  :: TPTURBINE    ! stored turbine data
        TYPE(BLADE),        INTENT(OUT) :: TPBLADE      ! stored blade data
END SUBROUTINE READ_CSVDATA_BLADE_ALM
!
SUBROUTINE READ_CSVDATA_AIRFOIL_ALM(KLUNAM,HFILE,TPBLADE,TPAIRFOIL)
        USE MODD_EOL_ALM, ONLY : BLADE, AIRFOIL
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(BLADE),        INTENT(IN)  :: TPBLADE      ! stored blade data (to select airfoils)
        TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: TPAIRFOIL  ! stored airfoil data
END SUBROUTINE READ_CSVDATA_AIRFOIL_ALM
!
SUBROUTINE HOW_MANY_LINES_OF(KLUNAM,HFILE,HNAME,KLINE) 
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        CHARACTER(LEN=*),   INTENT(IN)  :: HNAME        ! turbine's name  
        INTEGER,            INTENT(OUT) :: KLINE
END SUBROUTINE HOW_MANY_LINES_OF
!
FUNCTION GET_AIRFOIL_ID(TPTURBINE,TPBLADE,TPAIRFOIL,PRADIUS) 
        USE MODD_EOL_ALM, ONLY : TURBINE, BLADE, AIRFOIL
        IMPLICIT NONE
        INTEGER                                   :: GET_AIRFOIL_ID
        TYPE(TURBINE),                INTENT(IN)  :: TPTURBINE    ! stored turbine data
        TYPE(BLADE),                  INTENT(IN)  :: TPBLADE      ! stored blade data
        TYPE(AIRFOIL), DIMENSION(:),  INTENT(IN)  :: TPAIRFOIL    ! stored airfoil data 
        REAL,                         INTENT(IN)  :: PRADIUS      ! Radius position studied
END FUNCTION GET_AIRFOIL_ID
!
END INTERFACE
!
END MODULE MODI_EOL_READER
!-------------------------------------------------------------------
!
!!****  *EOL_READER* -
!!
!!    PURPOSE
!!    -------
!!    Some usefull subs to read wind turbine's datas
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original     05/2018  
!!    Modification 21/10/20 (PA. Joulin) Updated for a main version
!!
!!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE READ_CSVDATA_FARM_ADNR(KLUNAM,HFILE,TPFARM)
!        
USE MODD_EOL_ADNR,  ONLY: FARM
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN)    :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)    :: HFILE      ! file to read    
TYPE(FARM),         INTENT(OUT)   :: TPFARM     ! dummy stored data blade
!
LOGICAL                           :: GEXIST     ! Existence of file
!
INTEGER                           :: INBLINE    ! Nb of line in csv file
!
CHARACTER(LEN=400)                :: YSTRING   
!
! Read data
REAL                              :: ZPOS_X
REAL                              :: ZPOS_Y
REAL                              :: ZCT_INF
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Opening the file 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
! Counting number of line  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
IF (INBLINE < 2) THEN
 CALL EOL_CSVEMPTY_ERROR(HFILE,INBLINE)
 STOP
ELSE
 ! Saving number of wind turbine 
 TPFARM%NNB_TURBINES = INBLINE - 1 
 ! Allocations 
 ALLOCATE(TPFARM%XPOS_X(TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XPOS_Y(TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XCT_INF(TPFARM%NNB_TURBINES))
 !
 ! New read 
 REWIND(KLUNAM)
 READ(KLUNAM,FMT='(A400)') YSTRING ! Header reading 
 !
 ! Saving data 
 DO INBLINE=1, TPFARM%NNB_TURBINES
  READ(KLUNAM,FMT='(A400)') YSTRING
  READ(YSTRING,*) ZPOS_X, ZPOS_Y, ZCT_INF
  TPFARM%XPOS_X(INBLINE)  = ZPOS_X
  TPFARM%XPOS_Y(INBLINE)  = ZPOS_Y
  TPFARM%XCT_INF(INBLINE) = ZCT_INF
 END DO
 CLOSE(KLUNAM)
 RETURN
END IF
!
END SUBROUTINE READ_CSVDATA_FARM_ADNR
!#########################################################
!
!#########################################################
SUBROUTINE READ_CSVDATA_TURBINE_ADNR(KLUNAM,HFILE,TPTURBINE)
!        
USE MODD_EOL_ADNR,  ONLY: TURBINE
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN)  :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)  :: HFILE      ! file to read    
TYPE(TURBINE),      INTENT(OUT) :: TPTURBINE  ! dummy stored data turbine
!
LOGICAL                         :: GEXIST     ! Existence of file
!
INTEGER                         :: INBLINE    ! Nb of line in csv file
!
CHARACTER(LEN=400)              :: YSTRING   
!
CHARACTER(LEN=80)               :: YWT_NAME
REAL                            :: ZH_HEIGHT
REAL                            :: ZR_MAX
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Opening 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
!
! Counting number of line  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
IF (INBLINE /= 2) THEN
 CALL EOL_CSVEMPTY_ERROR(HFILE,INBLINE)
 STOP
ELSE 
 REWIND(KLUNAM)
 READ(KLUNAM,FMT='(A400)') YSTRING                    ! Header reading 
 READ(KLUNAM,FMT='(A400)') YSTRING                    ! Reading next line
 ! Read data 
 READ(YSTRING,*) YWT_NAME, ZH_HEIGHT, ZR_MAX          ! reading data
 TPTURBINE%CNAME      = YWT_NAME                      ! Saving them
 TPTURBINE%XH_HEIGHT  = ZH_HEIGHT
 TPTURBINE%XR_MAX     = ZR_MAX
 REWIND(KLUNAM)                                        ! Rembobinage, plutôt 2 fois qu'1 !
 RETURN
 CLOSE(KLUNAM)
END IF
!
END SUBROUTINE READ_CSVDATA_TURBINE_ADNR
!#########################################################
!
!#########################################################
SUBROUTINE READ_CSVDATA_FARM_ALM(KLUNAM,HFILE,TPFARM)
!        
USE MODD_EOL_ALM,   ONLY: FARM
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN)    :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)    :: HFILE      ! file to read    
TYPE(FARM),         INTENT(OUT)   :: TPFARM     ! dummy stored data blade
!
LOGICAL                           :: GEXIST     ! Existence of file
!
INTEGER                           :: INBLINE    ! Nb of line in csv file
!
CHARACTER(LEN=400)                :: YSTRING   
!
! Read data
REAL                              :: ZPOS_X
REAL                              :: ZPOS_Y
REAL                              :: ZOMEGA
REAL                              :: ZYAW
REAL                              :: ZPITCH
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Opening the file 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
! Counting number of line  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
IF (INBLINE < 2) THEN
 CALL EOL_CSVEMPTY_ERROR(HFILE,INBLINE)
 STOP
ELSE
 ! Saving number of wind turbine 
 TPFARM%NNB_TURBINES = INBLINE - 1 
 ! Allocations 
 ALLOCATE(TPFARM%XPOS_X    (TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XPOS_Y    (TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XOMEGA    (TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XNAC_YAW  (TPFARM%NNB_TURBINES))
 ALLOCATE(TPFARM%XBLA_PITCH(TPFARM%NNB_TURBINES))
 !
 ! New read 
 REWIND(KLUNAM)
 READ(KLUNAM,FMT='(A400)') YSTRING ! Header reading 
 !
 ! Saving data 
 DO INBLINE=1, TPFARM%NNB_TURBINES
  READ(KLUNAM,FMT='(A400)') YSTRING
  READ(YSTRING,*) ZPOS_X, ZPOS_Y, ZOMEGA, ZYAW, ZPITCH
  TPFARM%XPOS_X(INBLINE)     = ZPOS_X
  TPFARM%XPOS_Y(INBLINE)     = ZPOS_Y
  TPFARM%XOMEGA(INBLINE)     = ZOMEGA
  TPFARM%XNAC_YAW(INBLINE)   = ZYAW
  TPFARM%XBLA_PITCH(INBLINE) = ZPITCH
 END DO
 CLOSE(KLUNAM)
 RETURN
END IF
!
END SUBROUTINE READ_CSVDATA_FARM_ALM
!#########################################################
!
!#########################################################
SUBROUTINE READ_CSVDATA_TURBINE_ALM(KLUNAM,HFILE,TPTURBINE)
!        
USE MODD_EOL_ALM,   ONLY: TURBINE
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN)  :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)  :: HFILE      ! file to read    
TYPE(TURBINE),      INTENT(OUT) :: TPTURBINE  ! dummy stored data turbine
!
LOGICAL                         :: GEXIST     ! Existence of file
!
INTEGER                         :: INBLINE    ! Nb of line in csv file
!
CHARACTER(LEN=400)              :: YSTRING   
!
CHARACTER(LEN=80)               :: YWT_NAME
INTEGER                         :: INB_BLADE
REAL                            :: ZH_HEIGHT
REAL                            :: ZR_MIN
REAL                            :: ZR_MAX
REAL                            :: ZNAC_TILT
REAL                            :: ZHUB_DEPORT
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Opening 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
!
! Counting number of line  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
IF (INBLINE /= 2) THEN
 CALL EOL_CSVEMPTY_ERROR(HFILE,INBLINE)
 STOP
ELSE 
 REWIND(KLUNAM)
 READ(KLUNAM,FMT='(A400)') YSTRING                    ! Header reading 
 READ(KLUNAM,FMT='(A400)') YSTRING                    ! Reading next line
 ! Read data 
 READ(YSTRING,*) YWT_NAME, INB_BLADE, ZH_HEIGHT,&     ! reading data
                 ZR_MIN, ZR_MAX, ZNAC_TILT,     &
                 ZHUB_DEPORT 
 TPTURBINE%CNAME       = YWT_NAME                     ! Saving them
 TPTURBINE%NNB_BLADES  = INB_BLADE
 TPTURBINE%XH_HEIGHT   = ZH_HEIGHT
 TPTURBINE%XR_MIN      = ZR_MIN
 TPTURBINE%XR_MAX      = ZR_MAX
 TPTURBINE%XNAC_TILT   = ZNAC_TILT
 TPTURBINE%XH_DEPORT   = ZHUB_DEPORT
 REWIND(KLUNAM)                                       
 RETURN
 CLOSE(KLUNAM)
END IF
!
END SUBROUTINE READ_CSVDATA_TURBINE_ALM
!#########################################################
!
!#########################################################
SUBROUTINE READ_CSVDATA_BLADE_ALM(KLUNAM,HFILE,TPTURBINE,TPBLADE)
!       
USE MODD_EOL_ALM,   ONLY: TURBINE, BLADE, NNB_BLAELT
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
USE MODI_EOL_ERROR, ONLY: EOL_BLADEDATA_ERROR
!
INTEGER,            INTENT(IN)  :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)  :: HFILE      ! file to read    
TYPE(TURBINE),      INTENT(IN)  :: TPTURBINE  ! stored turbine data
TYPE(BLADE),        INTENT(OUT) :: TPBLADE    ! dummy stored data blade
!
LOGICAL                         :: GEXIST     ! Existence of file
!
INTEGER                         :: INBLINE    ! Nb of line in csv file
INTEGER                         :: INBDATA    ! Nb of data (line/section) of blade
!
CHARACTER(LEN=400)              :: YSTRING
!
REAL              :: ZCENTER     ! Center pos. of elmt [m]
REAL              :: ZCHORD      ! Blade chord of elmt [m]
REAL              :: ZTWIST      ! Twist of elmt [rad]
CHARACTER(LEN=20) :: YAIRFOIL    ! Airfoil name [-]
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Ouverture 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
! Counting number of line  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
IF (INBLINE < 2) THEN
 CALL EOL_CSVEMPTY_ERROR(HFILE,INBLINE)
 STOP
ELSE
 TPBLADE%NNB_BLAELT = NNB_BLAELT 
 ! Saving number of data 
 TPBLADE%NNB_BLADAT = INBLINE - 1 
 ALLOCATE(TPBLADE%XRAD(TPBLADE%NNB_BLADAT))
 ALLOCATE(TPBLADE%XCHORD(TPBLADE%NNB_BLADAT))
 ALLOCATE(TPBLADE%XTWIST(TPBLADE%NNB_BLADAT))
 ALLOCATE(TPBLADE%CAIRFOIL(TPBLADE%NNB_BLADAT))
 !
 ! New read
 REWIND(KLUNAM)
 READ(KLUNAM,FMT='(A400)') YSTRING                    ! Header reading 
 !
 ! Saving data
 DO INBLINE=1, TPBLADE%NNB_BLADAT
  READ(KLUNAM,FMT='(A400)') YSTRING
  READ(YSTRING,*) ZCENTER, ZCHORD, ZTWIST, YAIRFOIL  ! Reading data
  IF ((ZCENTER<=0.0) .OR. (ZCENTER>= 1.0)) THEN
   ! Checking data
   CALL EOL_BLADEDATA_ERROR(ZCENTER)
  ELSE
   ! Storing them
   TPBLADE%XRAD(INBLINE)     = ZCENTER*(TPTURBINE%XR_MAX-TPTURBINE%XR_MIN) & ! Data in %
                               + TPTURBINE%XR_MIN
   TPBLADE%XCHORD(INBLINE)   = ZCHORD
   TPBLADE%XTWIST(INBLINE)   = ZTWIST
   TPBLADE%CAIRFOIL(INBLINE) = YAIRFOIL
  END IF
 END DO
 CLOSE(KLUNAM)
 RETURN
END IF
!
END SUBROUTINE READ_CSVDATA_BLADE_ALM
!#########################################################
!
!#########################################################
SUBROUTINE READ_CSVDATA_AIRFOIL_ALM(KLUNAM,HFILE,TPBLADE,TPAIRFOIL)
!        
USE MODD_EOL_ALM,   ONLY: BLADE, AIRFOIL
USE MODI_EOL_ERROR, ONLY: EOL_CSVNOTFOUND_ERROR, EOL_CSVEMPTY_ERROR
USE MODI_EOL_ERROR, ONLY: EOL_AIRFOILNOTFOUND_ERROR 
!
INTEGER,            INTENT(IN)  :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)  :: HFILE      ! file to read    
TYPE(BLADE),        INTENT(IN)  :: TPBLADE    ! stored blade data (to select airfoils)
TYPE(AIRFOIL), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: TPAIRFOIL  ! dummy stored data blade
!
LOGICAL                         :: GEXIST     ! Existence of file
!
INTEGER                         :: INBDATA    ! Nb of data (line/section) per airfoil
INTEGER                         :: INBLINE    ! Nb of line in csv file
LOGICAL                         :: GAIRFLAG   ! Flag for airfoil counting
!
INTEGER                         :: JI, JJ, IA ! loop control
INTEGER                         :: INBAIRFOIL ! Nb of differents airfoils on one blade
!
CHARACTER(LEN=400)              :: YSTRING
!
CHARACTER(LEN=15), DIMENSION(:), ALLOCATABLE    :: YAIRFOIL
!
CHARACTER(LEN=15) :: YREAD_NAME
REAL              :: ZAA        ! Attack Angle [rad]
REAL              :: ZRE        ! Reynolds Number [-]
REAL              :: ZCL        ! Lift Coef [-]
REAL              :: ZCD        ! Drag Coef [-]
REAL              :: ZCM        ! Moment Coef [-]
!
! Checking file existence
INQUIRE(FILE=HFILE, EXIST=GEXIST)
IF (.NOT.GEXIST) THEN
 CALL EOL_CSVNOTFOUND_ERROR(HFILE)
END IF
!
! Ouverture 
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted', STATUS='OLD')
!
! 1. Counting number of differents airfoils along the blade and selection :
!
! Allcation of local airfoil array
ALLOCATE(YAIRFOIL(SIZE(TPBLADE%CAIRFOIL)))
!
YAIRFOIL(:) = ''
INBAIRFOIL = 1
YAIRFOIL(1) = TRIM(TPBLADE%CAIRFOIL(1))
!
DO JI=1, SIZE(TPBLADE%CAIRFOIL)
 GAIRFLAG = .FALSE.
 DO JJ=1, INBAIRFOIL
  IF (TRIM(TPBLADE%CAIRFOIL(JI)) == TRIM(YAIRFOIL(JJ))) THEN
   GAIRFLAG = .TRUE.
  END IF
 END DO
 IF (GAIRFLAG .EQV. .FALSE.) THEN
  INBAIRFOIL = INBAIRFOIL + 1
  YAIRFOIL(INBAIRFOIL) = TRIM(TPBLADE%CAIRFOIL(JI))
 END IF
END DO
ALLOCATE(TPAIRFOIL(INBAIRFOIL))
!
! 2. Reading and storing data :
!
DO IA = 1, INBAIRFOIL
 ! Array allocation
 CALL HOW_MANY_LINES_OF(KLUNAM,HFILE,YAIRFOIL(IA),INBDATA)
 ALLOCATE(TPAIRFOIL(IA)%XAA(INBDATA))
 ALLOCATE(TPAIRFOIL(IA)%XRE(INBDATA))
 ALLOCATE(TPAIRFOIL(IA)%XCL(INBDATA))
 ALLOCATE(TPAIRFOIL(IA)%XCD(INBDATA))
 ALLOCATE(TPAIRFOIL(IA)%XCM(INBDATA))
 !
 REWIND(KLUNAM)
 INBLINE = 0
 DO
  READ(KLUNAM,END=101,FMT='(A400)') YSTRING        ! Header
  !* reads the string
  IF (LEN_TRIM(YSTRING)>0) THEN
   READ(YSTRING,FMT=*) YREAD_NAME
   IF (TRIM(YREAD_NAME)==TRIM(YAIRFOIL(IA))) THEN  ! Read data
    INBLINE = INBLINE + 1
    READ(YSTRING,*) YREAD_NAME, ZAA, ZRE, &
                    ZCL, ZCD, ZCM
    ! Storing data
    TPAIRFOIL(IA)%CNAME            = YREAD_NAME
    TPAIRFOIL(IA)%XAA(INBLINE)     = ZAA
    TPAIRFOIL(IA)%XRE(INBLINE)     = ZRE
    TPAIRFOIL(IA)%XCL(INBLINE)     = ZCL
    TPAIRFOIL(IA)%XCD(INBLINE)     = ZCD
    TPAIRFOIL(IA)%XCM(INBLINE)     = ZCM
   ELSE                     ! The name doesnt appear during a new read..
    IF (INBLINE > 0) THEN   ! .. but it has already been found, ..
     REWIND(KLUNAM)         ! .. so it is the end of the data ..
     EXIT                   ! .. and we can exit :)
    END IF
   END IF
  END IF
 END DO
END DO
!
CLOSE(KLUNAM)
101 CONTINUE
 IF (INBLINE == 0) THEN
  CALL EOL_AIRFOILNOTFOUND_ERROR(HFILE,YAIRFOIL(IA))
  STOP
 END IF
END SUBROUTINE READ_CSVDATA_AIRFOIL_ALM
!#########################################################
!
!#########################################################
SUBROUTINE HOW_MANY_LINES_OF(KLUNAM,HFILE,HNAME,KLINE)
!
USE MODI_EOL_ERROR, ONLY: EOL_AIRFOILNOTFOUND_ERROR 
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
CHARACTER(LEN=*),   INTENT(IN)  :: HNAME        ! turbine's name  
INTEGER,            INTENT(OUT) :: KLINE
!
!
CHARACTER(LEN=400)              :: YSTRING
CHARACTER(LEN=80)               :: HREAD_NAME
!
REWIND(KLUNAM)
KLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
!* reads the string
 IF (LEN_TRIM(YSTRING) > 0) THEN
 READ(YSTRING,FMT=*) HREAD_NAME
  IF (TRIM(HREAD_NAME)==TRIM(HNAME)) THEN
   KLINE = KLINE + 1
  ELSE                   ! The name doesnt appear during a new read..
   IF (KLINE > 0) THEN   ! .. but it has already been found, ..
    REWIND(KLUNAM)       ! .. so it is the end of the data ..
    RETURN               ! .. and we can return :)
   END IF
  END IF
 END IF
END DO
101 CONTINUE
 IF (KLINE == 0) THEN
  CALL EOL_AIRFOILNOTFOUND_ERROR(HFILE,HNAME)
  STOP
 END IF
END SUBROUTINE HOW_MANY_LINES_OF
!#########################################################
!
!#########################################################
FUNCTION GET_AIRFOIL_ID(TPTURBINE,TPBLADE,TPAIRFOIL,PRADIUS)
! Allows to link an airfoil from a TPBLADE, at a specific radius (PRADIUS)
! to the airfoils characteristics (TPAIRFOIL).
! The result is an integer (IAID) that should be used like that :
! IAID = GET_GET_AIRFOIL_ID(TPTURBINE,TPBLADE,TPAIRFOIL,PRADIUS)
! TPAIRFOIL(IAID)%MEMBER_OF_TPAIRFOIL
!
USE MODD_EOL_ALM, ONLY : TURBINE, BLADE, AIRFOIL
!
IMPLICIT NONE
!
TYPE(TURBINE),                INTENT(IN)  :: TPTURBINE    ! stored turbine data
TYPE(BLADE),                  INTENT(IN)  :: TPBLADE      ! stored blade data
TYPE(AIRFOIL), DIMENSION(:),  INTENT(IN)  :: TPAIRFOIL    ! stored arifoil data
REAL,                         INTENT(IN)  :: PRADIUS      ! Radius position studied
INTEGER                                   :: GET_AIRFOIL_ID
!
INTEGER                                   :: INB_BDATA    ! Total number of blade data
INTEGER                                   :: JBDATA       ! Index over blade's data
INTEGER                                   :: JA           ! Index over diffetents airfoils
REAL, DIMENSION(SIZE(TPBLADE%XRAD))       :: ZDELTARAD    ! 2*ZDELTARAD = section lenght
!
! Checking data
IF ((PRADIUS < TPTURBINE%XR_MIN) .OR. (PRADIUS > TPTURBINE%XR_MAX)) THEN
 PRINT*, 'The studied radius R = ', PRADIUS, ' is out of blade range : [', &
          TPTURBINE%XR_MIN, ';', TPTURBINE%XR_MAX, ']'
 RETURN
END IF
!
! Preliminaires
INB_BDATA = SIZE(TPBLADE%XRAD)
!
! Computes half length of sections
ZDELTARAD(1) = TPBLADE%XRAD(1) - TPTURBINE%XR_MIN
DO JBDATA=2,INB_BDATA-1
 ZDELTARAD(JBDATA) = TPBLADE%XRAD(JBDATA) &
                   - TPBLADE%XRAD(JBDATA-1) &
                   - ZDELTARAD(JBDATA-1)
END DO
ZDELTARAD(INB_BDATA) = TPTURBINE%XR_MAX - TPBLADE%XRAD(INB_BDATA)
!
! Looking for the section at r=PRADIUS 
DO JBDATA=1,INB_BDATA
 IF ((PRADIUS >= TPBLADE%XRAD(JBDATA)-ZDELTARAD(JBDATA)) .AND. &
     (PRADIUS <  TPBLADE%XRAD(JBDATA)+ZDELTARAD(JBDATA))) THEN
! Looking for the ID of the airfoil of this section
  DO JA=1,SIZE(TPAIRFOIL)
   IF (TRIM(TPBLADE%CAIRFOIL(JBDATA)) == TRIM(TPAIRFOIL(JA)%CNAME)) THEN
    GET_AIRFOIL_ID = JA
    EXIT
   END IF
  END DO
!
  EXIT
 END IF
END DO
!
END FUNCTION GET_AIRFOIL_ID
!#########################################################
