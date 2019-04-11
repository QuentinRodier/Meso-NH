!MNH_LIC Copyright 1996-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_WRITE_HGRID
!     #######################
INTERFACE
      SUBROUTINE WRITE_HGRID(KMI,TPFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
INTEGER,         INTENT(IN)  :: KMI       ! model index
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE    ! File to write
!
END SUBROUTINE WRITE_HGRID
END INTERFACE
END MODULE MODI_WRITE_HGRID
!
!     ##################################
      SUBROUTINE WRITE_HGRID(KMI,TPFILE)
!     ##################################
!
!!****  *WRITE_HGRID* - to write grid information in FM file of model KMI
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      FMREAD   : to read data in LFIFM file
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        26/09/96
!!       V.Masson       18/08/97 call to fmwrit directly with dates and strings
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_IO,             ONLY: TFILEDATA
USE MODD_PGDDIM
USE MODD_PGDGRID
!
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
USE MODE_MSG
!
USE MODI_WRITE_HGRIDn
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,         INTENT(IN)  :: KMI       ! model index
TYPE(TFILEDATA), INTENT(IN)  :: TPFILE    ! File to write
!
!
!*       0.2   declarations of local variables
!
!-------------------------------------------------------------------------------
!
!*       1.     TEST ON MODEL INDEX
!	        -------------------
! KMI may be 0
IF (KMI<0 .OR. KMI>JPMODELMAX) CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_HGRID','KMI<0 .OR. KMI>JPMODELMAX')
IF (KMI/=0) THEN
  CALL WRITE_HGRID_n(TPFILE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     WRITING FROM MODD_PGD...
!	        ----------------------
!
CALL IO_Field_write(TPFILE,'LAT0',  XLAT0)
CALL IO_Field_write(TPFILE,'LON0',  XLON0)
CALL IO_Field_write(TPFILE,'RPK',   XRPK)
CALL IO_Field_write(TPFILE,'BETA',  XBETA)
CALL IO_Field_write(TPFILE,'LATORI',XPGDLATOR)
CALL IO_Field_write(TPFILE,'LONORI',XPGDLONOR)
CALL IO_Field_write(TPFILE,'IMAX',  NPGDIMAX)
CALL IO_Field_write(TPFILE,'JMAX',  NPGDJMAX)
CALL IO_Field_write(TPFILE,'XHAT',  XPGDXHAT)
CALL IO_Field_write(TPFILE,'YHAT',  XPGDYHAT)
!
IF (CSTORAGE_TYPE=='TT') THEN
  CALL IO_Field_write(TPFILE,'THINSHELL',LTHINSHELL)
  CALL IO_Field_write(TPFILE,'CARTESIAN',LCARTESIAN)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID
