!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_EOL_ADNR
!     ##########################
!
INTERFACE
!
SUBROUTINE INI_EOL_ADNR
!
END SUBROUTINE INI_EOL_ADNR                 
!
END INTERFACE
!
END MODULE MODI_INI_EOL_ADNR
!
!     ############################################################
      SUBROUTINE INI_EOL_ADNR
!     ############################################################
!
!!****  *INI_EOL_ADNR*       
!!
!!    PURPOSE
!!    -------
!!     Routine to initialized the ADNR (wind turbine model) variables
!!
!!**  METHOD
!!    ------
!!          
!!    EXTERNAL
!!    --------   
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    **MODD_EOL_SHARED_IO:
!!    for namelist NAM_EOL_ADNR (INPUT) :
!!    CHARACTER(LEN=100)      :: CFARM_CSVDATA     ! File to read, with farm data
!!    CHARACTER(LEN=100)      :: CTURBINE_CSVDATA  ! File to read, turbine data
!!    for ouputs :
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTHRUT        ! Thrust [N]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTORQT        ! Torque [Nm]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XPOWT         ! Power [W]
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTHRU_SUM     ! Sum of thrust (N)
!!    REAL, DIMENSION(:), ALLOCATABLE :: XTORQ_SUM     ! Sum of torque (Nm)
!!    REAL, DIMENSION(:), ALLOCATABLE :: XPOW_SUM      ! Sum of power (W)
!!
!!    **MODD_EOL_ADNR (OUTPUT):
!!    TYPE(FARM)                        :: TFARM
!!    TYPE(TURBINE)                     :: TTURBINE
!!    REAL, DIMENSION(:), ALLOCATABLE   :: XA_INDU  ! Induction factor(NumEol,données)
!!    REAL, DIMENSION(:), ALLOCATABLE   :: XCT_D    ! Adapted thrust coef (for U_d) [-]
!!
!!    REFERENCE
!!    ---------
!!       
!!
!!    AUTHOR
!!    ------
!!	PA. Joulin      * Meteo France & IFPEN *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        31/05/18
!!      Modification    14/10/20 (PA. Joulin) Updated for a main version
!!
!--------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!
USE MODD_EOL_ADNR
USE MODD_EOL_SHARED_IO, ONLY: CFARM_CSVDATA, CTURBINE_CSVDATA
USE MODD_EOL_SHARED_IO, ONLY: XTHRUT, XTHRU_SUM
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_FARM_ADNR
USE MODI_EOL_READER,    ONLY: READ_CSVDATA_TURBINE_ADNR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_FARM_ADNR
USE MODI_EOL_PRINTER,   ONLY: PRINT_DATA_TURBINE_ADNR
! To print in output listing
USE MODD_LUNIT_n,       ONLY: TLUOUT
!
IMPLICIT NONE
!
! Integers 
INTEGER  :: ILUOUT    ! Output listing file
!
!
!-------------------------------------------------------------------
!
!*       1.    READING AND ALLOCATING DATA
!              --------------------------- 
! Reading in csv files
! Allocation of TFARM and TTURBINE inside the function
!
!*       1.1    Wind farm data
!
CALL READ_CSVDATA_FARM_ADNR(40,TRIM(CFARM_CSVDATA),TFARM)
!
!*       1.2    Wind turbine data
!
CALL READ_CSVDATA_TURBINE_ADNR(41,TRIM(CTURBINE_CSVDATA),TTURBINE)
!
!
!-------------------------------------------------------------------
!
!*       2.    PRINTING DATA 
!              ------------- 
!
!*       2.0    Output listing index
ILUOUT= TLUOUT%NLU
!
!*       2.1    Wind farm data
!
CALL PRINT_DATA_FARM_ADNR(ILUOUT,TFARM)
!
!*       2.2    Wind turbine data
!
CALL PRINT_DATA_TURBINE_ADNR(ILUOUT,TTURBINE)
!
!
!-------------------------------------------------------------------
!
!*       3.    ALLOCATING VARIABLES 
!              -------------------- 
!
ALLOCATE(XA_INDU  (TFARM%NNB_TURBINES))
ALLOCATE(XCT_D    (TFARM%NNB_TURBINES))
ALLOCATE(XTHRUT   (TFARM%NNB_TURBINES))
ALLOCATE(XTHRU_SUM(TFARM%NNB_TURBINES))
!
END SUBROUTINE INI_EOL_ADNR
