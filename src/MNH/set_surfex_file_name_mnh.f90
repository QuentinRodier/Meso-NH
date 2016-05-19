!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######spl
 MODULE MODI_SET_SURFEX_FILE_NAME_MNH
!####################
INTERFACE
!
SUBROUTINE SET_SURFEX_FILE_NAME_MNH(HMASK)
  CHARACTER(LEN=4),  INTENT(IN)  :: HMASK
END SUBROUTINE SET_SURFEX_FILE_NAME_MNH
!
END INTERFACE
!
END MODULE MODI_SET_SURFEX_FILE_NAME_MNH
    

!     ######spl
      SUBROUTINE  SET_SURFEX_FILE_NAME_MNH(HMASK)
!     ###############################  
!
USE MODD_LUNIT_n,      ONLY : CMASK_SURFEX
IMPLICIT NONE
!
CHARACTER(LEN=4),  INTENT(IN)  :: HMASK

CMASK_SURFEX=HMASK      
!     ######spl
END SUBROUTINE  SET_SURFEX_FILE_NAME_MNH
!     ###############################        
