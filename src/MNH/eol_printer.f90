!MNH_LIC Copyright 2020-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #########################
       MODULE MODI_EOL_PRINTER
!     #########################
!
INTERFACE
!
! ****
! ADNR
! ****
!
SUBROUTINE PRINT_DATA_FARM_ADNR(KFILE,TPFARM)
        USE MODD_EOL_ADNR, ONLY: FARM
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(FARM),                  INTENT(IN) :: TPFARM        ! stored farm data
END SUBROUTINE PRINT_DATA_FARM_ADNR
!
SUBROUTINE PRINT_DATA_TURBINE_ADNR(KFILE,TPTURBINE)
        USE MODD_EOL_ADNR, ONLY : TURBINE
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(TURBINE),               INTENT(IN) :: TPTURBINE     ! stored turbine data
END SUBROUTINE PRINT_DATA_TURBINE_ADNR
!
! ***
! ALM
! ***
!
SUBROUTINE PRINT_DATA_FARM_ALM(KFILE,TPFARM)
        USE MODD_EOL_ALM, ONLY: FARM
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(FARM),                  INTENT(IN) :: TPFARM        ! stored farm data
END SUBROUTINE PRINT_DATA_FARM_ALM
!
SUBROUTINE PRINT_DATA_TURBINE_ALM(KFILE,TPTURBINE)
        USE MODD_EOL_ALM, ONLY : TURBINE
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(TURBINE),               INTENT(IN) :: TPTURBINE     ! stored turbine data
END SUBROUTINE PRINT_DATA_TURBINE_ALM
!
SUBROUTINE PRINT_DATA_BLADE_ALM(KFILE,TPBLADE)
        USE MODD_EOL_ALM, ONLY : BLADE
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(BLADE),                 INTENT(IN) :: TPBLADE       ! stored blade data
END SUBROUTINE PRINT_DATA_BLADE_ALM
!
SUBROUTINE PRINT_DATA_AIRFOIL_ALM(KFILE,TPAIRFOIL)
        USE MODD_EOL_ALM, ONLY : AIRFOIL
        INTEGER,                     INTENT(IN) :: KFILE         ! output file
        TYPE(AIRFOIL), DIMENSION(:), INTENT(IN) :: TPAIRFOIL     ! stored airfoil data
END SUBROUTINE PRINT_DATA_AIRFOIL_ALM
!
SUBROUTINE OPEN_TECOUT(KFILE, KTCOUNT, KTSUBCOUNT)
        INTEGER,                     INTENT(IN)   :: KFILE       ! File index
        INTEGER,                     INTENT(IN)   :: KTCOUNT     ! Time step index
        INTEGER,                     INTENT(IN)   :: KTSUBCOUNT  ! Subtime step index
END SUBROUTINE OPEN_TECOUT
!
SUBROUTINE PRINT_TECOUT(KFILE,PVAR)
        INTEGER,                     INTENT(IN)   :: KFILE       ! File index
        REAL, DIMENSION(3),          INTENT(IN)   :: PVAR        ! Vector to plot
END SUBROUTINE PRINT_TECOUT
!
SUBROUTINE PRINT_TSPLIT(KNBSUBCOUNT,PTSUBSTEP)
        INTEGER,                     INTENT(IN)   :: KNBSUBCOUNT ! splitting value
        REAL,                        INTENT(IN)   :: PTSUBSTEP   ! sub timestep
END SUBROUTINE PRINT_TSPLIT
!
!
END INTERFACE
!
END MODULE MODI_EOL_PRINTER
!-------------------------------------------------------------------
!
!!****  *EOL_PRINT* -
!!
!!    PURPOSE
!!    -------
!!    Some usefull subs to print wind turbine's datas
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original     26/10/2020  
!!
!!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE PRINT_DATA_FARM_ADNR(KFILE,TPFARM)
!        
USE MODD_EOL_ADNR, ONLY : FARM
USE MODD_EOL_SHARED_IO, ONLY : CFARM_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,    INTENT(IN)  :: KFILE  ! File index
TYPE(FARM), INTENT(IN)  :: TPFARM ! dummy stored farm data
!
INTEGER  :: JROT    ! Loop index
!
IF (IP==1) THEN
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '======================== WIND TURBINE DATA ========================'
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '---- Farm ----'
 WRITE(KFILE,*) 'Data from file                    : ', TRIM(CFARM_CSVDATA)
 WRITE(KFILE,*) 'Number of turbines                : ', TPFARM%NNB_TURBINES
 WRITE(KFILE,*) 'Positions [m] and thrust coef [-] : '
 DO JROT=1, TPFARM%NNB_TURBINES
  WRITE(KFILE,'(1X,A,I3,A,F10.1,A,F10.1,A,F10.3)') 'n.', JROT,&
        ' : X = ',       TPFARM%XPOS_X(JROT),&
        ' ; Y = ',       TPFARM%XPOS_Y(JROT),&
        ' ; CT_inf = ',  TPFARM%XCT_INF(JROT)
 END DO
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_FARM_ADNR
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_DATA_TURBINE_ADNR(KFILE,TPTURBINE)
!        
USE MODD_EOL_ADNR, ONLY : TURBINE
USE MODD_EOL_SHARED_IO, ONLY : CTURBINE_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,       INTENT(IN)  :: KFILE     ! File index
TYPE(TURBINE), INTENT(IN)  :: TPTURBINE ! dummy stored turbine data
!
IF (IP==1) THEN
 WRITE(KFILE,*) '---- Turbine ----'
 WRITE(KFILE,*             ) 'Data from file   : ', TRIM(CTURBINE_CSVDATA)
 WRITE(KFILE,'(1X,A,A10)'  ) 'Wind turbine     : ', TPTURBINE%CNAME
 WRITE(KFILE,'(1X,A,F10.1)') 'Hub height [m]   : ', TPTURBINE%XH_HEIGHT
 WRITE(KFILE,'(1X,A,F10.3)') 'Blade radius [m] : ', TPTURBINE%XR_MAX
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '==================================================================='
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_TURBINE_ADNR
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_DATA_FARM_ALM(KFILE,TPFARM)
!        
USE MODD_EOL_ALM,  ONLY : FARM
USE MODD_EOL_SHARED_IO, ONLY : CFARM_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,    INTENT(IN) :: KFILE    ! File index
TYPE(FARM), INTENT(IN) :: TPFARM   ! dummy stored farm data
!
INTEGER  :: JROT    ! Loop index
!
IF (IP==1) THEN
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '======================== WIND TURBINE DATA ========================'
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '---- Farm ----'
 WRITE(KFILE,*) 'Data from file                  : ', TRIM(CFARM_CSVDATA)
 WRITE(KFILE,*) 'Number of turbines              : ', TPFARM%NNB_TURBINES
 WRITE(KFILE,*) 'Tower base positions (X,Y) [m]  : '
 DO JROT=1, TPFARM%NNB_TURBINES
  WRITE(KFILE, '(1X,A,I3,A,F10.1,A,F10.1,A)') 'n.', JROT,&
         ': (', TPFARM%XPOS_X(JROT),',',TPFARM%XPOS_Y(JROT),')'
 END DO
 WRITE(KFILE,*) 'Working state (rad/s,rad,rad)   : '
 DO JROT=1, TPFARM%NNB_TURBINES
  WRITE(KFILE, '(1X,A,I3,A,F10.5,A,F10.5,A,F10.5)') 'n.', JROT,&
         ': Omega = ',   TPFARM%XOMEGA(JROT),  &
         ' ; Yaw = ',     TPFARM%XNAC_YAW(JROT),&
         ' ; Pitch = ',   TPFARM%XBLA_PITCH(JROT)
 END DO
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_FARM_ALM
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_DATA_TURBINE_ALM(KFILE,TPTURBINE)
!        
USE MODD_EOL_ALM,  ONLY : TURBINE
USE MODD_EOL_SHARED_IO, ONLY : CTURBINE_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,       INTENT(IN)  :: KFILE      ! File index
TYPE(TURBINE), INTENT(IN)  :: TPTURBINE  ! dummy stored turbine data
!
IF (IP==1) THEN
 WRITE(KFILE,*) '---- Turbine ----'
 WRITE(KFILE,*             ) 'Data from file                  : ', TRIM(CTURBINE_CSVDATA)
 WRITE(KFILE,'(1X,A,A10)'  ) 'Wind turbine                    : ', TPTURBINE%CNAME
 WRITE(KFILE,'(1X,A,I10)'  ) 'Number of blades                : ', TPTURBINE%NNB_BLADES
 WRITE(KFILE,'(1X,A,F10.1)') 'Hub height [m]                  : ', TPTURBINE%XH_HEIGHT
 WRITE(KFILE,'(1X,A,F10.3)') 'Blade min radius [m]            : ', TPTURBINE%XR_MIN
 WRITE(KFILE,'(1X,A,F10.3)') 'Blade max radius [m]            : ', TPTURBINE%XR_MAX
 WRITE(KFILE,'(1X,A,F10.3)') 'Nacelle tilt [rad]              : ', TPTURBINE%XNAC_TILT
 WRITE(KFILE,'(1X,A,F10.3)') 'Hub deport [m]                  : ', TPTURBINE%XH_DEPORT
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_TURBINE_ALM
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_DATA_BLADE_ALM(KFILE,TPBLADE)
!        
USE MODD_EOL_ALM,  ONLY : BLADE
USE MODD_EOL_SHARED_IO, ONLY : CBLADE_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,     INTENT(IN)  :: KFILE    ! File index
TYPE(BLADE), INTENT(IN)  :: TPBLADE  ! dummy stored blade data
!
IF (IP==1) THEN
 WRITE(KFILE,*) '---- Blade ----'
 WRITE(KFILE,*             ) 'Data from file                  : ', TRIM(CBLADE_CSVDATA)
 WRITE(KFILE,'(1X,A,I10)'  ) 'Nb of data (from data file)     : ', TPBLADE%NNB_BLADAT
 WRITE(KFILE,'(1X,A,F10.1)') 'First node radius [m]           : ', TPBLADE%XRAD(1)
 WRITE(KFILE,'(1X,A,F10.1)') 'Last node radius [m]            : ', TPBLADE%XRAD(TPBLADE%NNB_BLADAT)
 WRITE(KFILE,'(1X,A,F10.1)') 'Chord max. [m]                  : ', MAXVAL(TPBLADE%XCHORD(:))
 WRITE(KFILE,'(1X,A,I10)'  ) 'Nb of blade element (from nam)  : ', TPBLADE%NNB_BLAELT
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_BLADE_ALM
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_DATA_AIRFOIL_ALM(KFILE,TPAIRFOIL)
!        
USE MODD_EOL_ALM,  ONLY : AIRFOIL
USE MODD_EOL_SHARED_IO, ONLY : CAIRFOIL_CSVDATA
USE MODD_VAR_ll,   ONLY : IP                    ! only master cpu
!
IMPLICIT NONE
!
INTEGER,                     INTENT(IN)  :: KFILE        ! File index
TYPE(AIRFOIL), DIMENSION(:), INTENT(IN)  :: TPAIRFOIL    ! dummy stored airfoil data
!
INTEGER :: JA                  
!
IF (IP==1) THEN
 WRITE(KFILE,*) '---- Airfoils ----'
 WRITE(KFILE,*             ) 'Data from file                  : ', TRIM(CAIRFOIL_CSVDATA)
 WRITE(KFILE,'(1X,A,I10)'  ) 'Nb of airfoils (from data file) : ', SIZE(TPAIRFOIL)
 WRITE(KFILE,'(1X,A)'      ) 'Different airfoils              : '
 DO JA=1,SIZE(TPAIRFOIL)
  WRITE(KFILE,'(1X,A,I3,A,A)') 'Airfoil n.', JA,&
       ': ', TPAIRFOIL(JA)%CNAME
 END DO
 WRITE(KFILE,*) ''
 WRITE(KFILE,*) '==================================================================='
 WRITE(KFILE,*) ''
END IF
!
END SUBROUTINE PRINT_DATA_AIRFOIL_ALM
!#########################################################
!
!
!#########################################################
SUBROUTINE OPEN_TECOUT(KFILE, KTCOUNT, KTSUBCOUNT)
!
USE MODD_EOL_ALM, ONLY:TFARM,TTURBINE,TBLADE
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)   :: KFILE      ! File index
INTEGER, INTENT(IN)   :: KTCOUNT    ! Time step index
INTEGER, INTENT(IN)   :: KTSUBCOUNT ! Subtime step index
!
INTEGER  :: INB_WT, INB_B, INB_BELT ! Total numbers of wind turbines, blades, and blade elt
INTEGER  :: INB_TELT, INB_NELT      ! Total numbers of tower elt, and nacelle elt
INTEGER  :: ITOTELT                 ! Total number of points
!
CHARACTER(LEN=1024) :: HFILE      ! File name
!
INB_WT   = TFARM%NNB_TURBINES
INB_B    = TTURBINE%NNB_BLADES
INB_BELT = TBLADE%NNB_BLAELT
! Hard coded variables, but they will be usefull in next updates
INB_TELT = 2
INB_NELT = 2
!
ITOTELT = INB_WT*(INB_TELT+INB_NELT+INB_B*(1+INB_BELT*3))
!
! File name and opening
WRITE(HFILE, "(A18,I4.4,I2.2,A3)") "Tecplot2.0_Output_", KTCOUNT, KTSUBCOUNT,".tp"
OPEN( KFILE, file=HFILE, form="FORMATTED")
!
! Tecplot Header
WRITE(KFILE,*) 'TITLE="Wind Turbines Points"'
WRITE(KFILE,*) 'VARIABLES="X" "Y" "Z"'
WRITE(KFILE,*) 'ZONE I=',ITOTELT,' J=3 K=1 DATAPACKING=POINT'
!
END SUBROUTINE OPEN_TECOUT
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_TECOUT(KFILE,PVAR)
IMPLICIT NONE
INTEGER, INTENT(IN)            :: KFILE ! File index
REAL, DIMENSION(3), INTENT(IN) :: PVAR  ! Vector to plot
!
! It plots two points, slightly different, to get a thickness
!
WRITE(KFILE,*) PVAR(1), &
               PVAR(2), &
               PVAR(3)
!
END SUBROUTINE PRINT_TECOUT
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_TSPLIT(KNBSUBCOUNT,PTSUBSTEP)
USE MODD_LUNIT_n   , ONLY: TLUOUT
!
INTEGER, INTENT(IN)   :: KNBSUBCOUNT ! splitting value
REAL,    INTENT(IN)   :: PTSUBSTEP   ! sub-time step
!
WRITE(TLUOUT%NLU,'(A)') 'From EOL - Actuator Line Model. Time-splitting is activated:'
WRITE(TLUOUT%NLU,'(1X,A,I2)')   'Number of splitting: ', KNBSUBCOUNT
WRITE(TLUOUT%NLU,'(1X,A,F6.4)') 'Sub-time step value: ', PTSUBSTEP
!
END SUBROUTINE PRINT_TSPLIT
!#########################################################
!
!#########################################################
SUBROUTINE PRINT_ERROR_WTCFL(KFILE,PMAXTSTEP)
INTEGER, INTENT(IN) :: KFILE        ! output file
REAL,    INTENT(IN) :: PMAXTSTEP    ! maximum acceptable time-step 
WRITE(KFILE,'(A,A,A,A,A,F10.8,A)')                                 &
 'Sorry but I had to stop the simulation. '                       ,&
 'The time step XTSTEP is too high: '                             ,& 
 'the blades can jump over one or several cells. '                ,&
 'Please, turn on the time-splitting method (LTIMESPLIT=.TRUE.), ',&
 'or decrease XTSTEP to a value lower than ', PMAXTSTEP, ' sec.'
END SUBROUTINE PRINT_ERROR_WTCFL
!
