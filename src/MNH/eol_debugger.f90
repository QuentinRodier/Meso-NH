!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #######################
       MODULE MODI_EOL_DEBUGGER
!     #######################
!
INTERFACE
!
!
SUBROUTINE PRINTMER_ll(HNAME,PVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        REAL,             INTENT(IN)  :: PVAR   ! Variable,
END SUBROUTINE PRINTMER_ll
!
SUBROUTINE PRINTMEI_ll(HNAME,KVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        INTEGER,          INTENT(IN)  :: KVAR   ! Variable,
END SUBROUTINE PRINTMEI_ll
!
SUBROUTINE PRINTMEC_ll(HNAME,CVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        CHARACTER(LEN=*), INTENT(IN)  :: CVAR   ! Variable,
END SUBROUTINE PRINTMEC_ll
!
SUBROUTINE PRINTMER_CPU1(HNAME,PVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        REAL,             INTENT(IN)  :: PVAR   ! Variable,
END SUBROUTINE PRINTMER_CPU1
!
SUBROUTINE PRINTMEI_CPU1(HNAME,KVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        INTEGER,          INTENT(IN)  :: KVAR   ! Variable,
END SUBROUTINE PRINTMEI_CPU1
!
SUBROUTINE PRINTMEC_CPU1(HNAME,CVAR)
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        CHARACTER(LEN=*), INTENT(IN)  :: CVAR   ! Variable,
END SUBROUTINE PRINTMEC_CPU1
!
SUBROUTINE PRINTMER_ELT1(KROT,KBLA,KELT,HNAME,PVAR)
        INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        REAL,             INTENT(IN)  :: PVAR   ! Variable,
END SUBROUTINE PRINTMER_ELT1
!
SUBROUTINE PRINTMER_ELT42(KROT,KBLA,KELT,HNAME,PVAR)
        INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        REAL,             INTENT(IN)  :: PVAR   ! Variable,
END SUBROUTINE PRINTMER_ELT42
!
SUBROUTINE PRINTMER_3BELT42(KROT,KBLA,KELT,HNAME,PVAR)
        INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
        CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
        REAL,             INTENT(IN)  :: PVAR   ! Variable,
END SUBROUTINE PRINTMER_3BELT42
!
END INTERFACE
!
END MODULE MODI_EOL_DEBUGGER
!-------------------------------------------------------------------
!
!!****  *EOL_PRINTER* -
!!
!!    PURPOSE
!!    -------
!!    Some usefull toold to debbug my code
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
SUBROUTINE PRINTMER_ll(HNAME,PVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
REAL,             INTENT(IN)  :: PVAR   ! Variable,
!
!
PRINT*, 'CPU n. ', IP, ' : ', HNAME, ' = ', PVAR 
!
END SUBROUTINE PRINTMER_ll
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMEI_ll(HNAME,KVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
INTEGER,          INTENT(IN)  :: KVAR   ! Variable,
!
!
PRINT*, 'CPU n. ', IP, ' : ', HNAME, ' = ', KVAR 
!
END SUBROUTINE PRINTMEI_ll
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMEC_ll(HNAME,CVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
CHARACTER(LEN=*), INTENT(IN)  :: CVAR   ! Variable,
!
!
PRINT*, 'CPU n. ', IP, ' : ', HNAME, ' = ', CVAR 
!
END SUBROUTINE PRINTMEC_ll
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMER_CPU1(HNAME, PVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
REAL,             INTENT(IN)  :: PVAR   ! Variable,
!
IF (IP==1) THEN
 PRINT*, HNAME, ' = ', PVAR
END IF
!
END SUBROUTINE PRINTMER_CPU1
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMEI_CPU1(HNAME, KVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
INTEGER,          INTENT(IN)  :: KVAR   ! Variable,
!
IF (IP==1) THEN
 PRINT*, HNAME, ' = ', KVAR
END IF
!
END SUBROUTINE PRINTMEI_CPU1
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMEC_CPU1(HNAME, CVAR)
!        
USE MODD_VAR_ll, ONLY: IP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN) :: HNAME  ! Name of the variable,
CHARACTER(LEN=*), INTENT(IN) :: CVAR   ! Variable,
!
IF (IP==1) THEN
 PRINT*, HNAME, ' = ', CVAR
END IF
!
END SUBROUTINE PRINTMEC_CPU1
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMER_ELT1(KROT, KBLA, KELT, HNAME, PVAR)
!        
USE MODD_CST,         ONLY: XPI
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
REAL,             INTENT(IN)  :: PVAR   ! Variable,
!
IF ((KROT==1).AND.(KBLA==1).AND.(KELT==1)) THEN
 PRINT*, HNAME, PVAR
END IF
!
END SUBROUTINE PRINTMER_ELT1
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMER_ELT42(KROT, KBLA, KELT, HNAME, PVAR)
!        
USE MODD_CST,         ONLY: XPI
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
REAL,             INTENT(IN)  :: PVAR   ! Variable,
!
IF ((KROT==1).AND.(KBLA==1).AND.(KELT==42)) THEN
 PRINT*, HNAME, PVAR
END IF
!
END SUBROUTINE PRINTMER_ELT42
!#########################################################
!
!#########################################################
SUBROUTINE PRINTMER_3BELT42(KROT, KBLA, KELT, HNAME, PVAR)
!        
USE MODD_CST,         ONLY: XPI
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN)  :: KROT, KBLA, KELT ! Loop index,
CHARACTER(LEN=*), INTENT(IN)  :: HNAME  ! Name of the variable,
REAL,             INTENT(IN)  :: PVAR   ! Variable,
!
IF ((KROT==1).AND.(KBLA==1).AND.(KELT==42)) THEN
 PRINT*, HNAME, 'B1 = ', PVAR*180/XPI
END IF
IF ((KROT==1).AND.(KBLA==2).AND.(KELT==42)) THEN
 PRINT*, HNAME, 'B2 = ', PVAR*180/XPI
END IF
IF ((KROT==1).AND.(KBLA==3).AND.(KELT==42)) THEN
 PRINT*, HNAME, 'B3 = ', PVAR*180/XPI
END IF
!
END SUBROUTINE PRINTMER_3BELT42
!#########################################################
!
