!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_WRITE_HGRID
!     #######################
INTERFACE
      SUBROUTINE WRITE_HGRID(KMI,TPFILE)
!
USE MODD_IO_ll, ONLY: TFILEDATA
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_LUNIT
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODD_PGDGRID
USE MODD_PGDDIM
USE MODD_PARAMETERS
!
USE MODI_WRITE_HGRIDn
USE MODE_FM
USE MODE_IO_ll
USE MODE_FMWRIT
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
INTEGER               :: ILUOUT0
INTEGER               :: IRESP
CHARACTER (LEN=100)   :: YCOMMENT   
!
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!*       1.     TEST ON MODEL INDEX
!	        -------------------
! KMI may be 0
IF (KMI<0 .OR. KMI>JPMODELMAX) THEN
   !callabortstop
  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
  CALL ABORT
  STOP
ENDIF
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
CALL IO_WRITE_FIELD(TPFILE,'LAT0',  CLUOUT0,XLAT0)
CALL IO_WRITE_FIELD(TPFILE,'LON0',  CLUOUT0,XLON0)
CALL IO_WRITE_FIELD(TPFILE,'RPK',   CLUOUT0,XRPK)
CALL IO_WRITE_FIELD(TPFILE,'BETA',  CLUOUT0,XBETA)
CALL IO_WRITE_FIELD(TPFILE,'LATORI',CLUOUT0,XPGDLATOR)
CALL IO_WRITE_FIELD(TPFILE,'LONORI',CLUOUT0,XPGDLONOR)
CALL IO_WRITE_FIELD(TPFILE,'IMAX',  CLUOUT0,NPGDIMAX)
CALL IO_WRITE_FIELD(TPFILE,'JMAX',  CLUOUT0,NPGDJMAX)
CALL IO_WRITE_FIELD(TPFILE,'XHAT',  CLUOUT0,XPGDXHAT)
CALL IO_WRITE_FIELD(TPFILE,'YHAT',  CLUOUT0,XPGDYHAT)
!
IF (CSTORAGE_TYPE=='TT') THEN
  CALL IO_WRITE_FIELD(TPFILE,'THINSHELL',CLUOUT0,LTHINSHELL)
  CALL IO_WRITE_FIELD(TPFILE,'CARTESIAN',CLUOUT0,LCARTESIAN)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID
