!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_WRITE_HGRIDn
!     ########################
INTERFACE
      SUBROUTINE WRITE_HGRID_n(HOUTFILE,HDADFILE)
!
CHARACTER (LEN=*), INTENT(IN) :: HOUTFILE ! name of the file n
CHARACTER (LEN=*), INTENT(IN) :: HDADFILE ! name of the father of file n
!
END SUBROUTINE WRITE_HGRID_n
!
END INTERFACE
END MODULE MODI_WRITE_HGRIDn
!
!     ###########################################
      SUBROUTINE WRITE_HGRID_n(HOUTFILE,HDADFILE)
!     ###########################################
!
!!****  *WRITE_HGRID_n* - to write grid information in FM file of model _n
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT   : to write data in LFIFM file
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_GRID : contains projection definition
!!        XLAT0
!!        XLON0
!!        XRPK
!!        XBETA
!!        XLATORI
!!        XLONORI
!!      Module MODD_GRID_n : contains domain definition
!!        XXHAT
!!        XYHAT
!!      Module MODD_DIM_n : contains domain size
!!        NIMAX
!!        NJMAX
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!      Module MODD_LUNIT_n :
!!        CLUOUT
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
!!        V.Masson      18/08/97 call to fmwrit directly with dates and strings
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_LUNIT_n
!
USE MODE_FM
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*), INTENT(IN) :: HOUTFILE ! name of the file n
CHARACTER (LEN=*), INTENT(IN) :: HDADFILE ! name of the father of file n
!
!
!*       0.2   declarations of local variables
!
INTEGER               :: ILUOUT
INTEGER               :: IRESP
CHARACTER (LEN=100)   :: YCOMMENT   
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
YCOMMENT=' '
!
!-------------------------------------------------------------------------------
CALL FMWRIT(HOUTFILE,'MASDEV',CLUOUT,'--',NMASDEV,0,LEN(YCOMMENT),YCOMMENT,IRESP)
CALL FMWRIT(HOUTFILE,'BUGFIX',CLUOUT,'--',NBUGFIX,0,LEN(YCOMMENT),YCOMMENT,IRESP)
CALL FMWRIT(HOUTFILE,'BIBUSER',CLUOUT,'--',CBIBUSER,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
CALL FMWRIT(HOUTFILE,'MY_NAME',CLUOUT,'--',HOUTFILE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
CALL FMWRIT(HOUTFILE,'DAD_NAME',CLUOUT,'--',HDADFILE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!-------------------------------------------------------------------------------
!
CALL FMWRIT(HOUTFILE,'STORAGE_TYPE',CLUOUT,'--',CSTORAGE_TYPE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LAT0',CLUOUT,'--',XLAT0,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LON0',CLUOUT,'--',XLON0,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'RPK' ,CLUOUT,'--',XRPK ,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'BETA',CLUOUT,'--',XBETA,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LATORI',CLUOUT,'--',XLATORI,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LONORI',CLUOUT,'--',XLONORI,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'IMAX' ,CLUOUT,'--',NIMAX ,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'JMAX' ,CLUOUT,'--',NJMAX ,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
YCOMMENT='METERS'
CALL FMWRIT(HOUTFILE,'XHAT',CLUOUT,'XX',XXHAT,2,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='METERS'
CALL FMWRIT(HOUTFILE,'YHAT',CLUOUT,'YY',XYHAT,3,LEN(YCOMMENT),YCOMMENT,IRESP)
!
IF (CSTORAGE_TYPE=='TT') THEN
  YCOMMENT=' '
  CALL FMWRIT(HOUTFILE,'THINSHELL',CLUOUT,'--',LTHINSHELL,0,LEN(YCOMMENT),YCOMMENT,IRESP)
  YCOMMENT=' '
  CALL FMWRIT(HOUTFILE,'CARTESIAN',CLUOUT,'--',LCARTESIAN,0,LEN(YCOMMENT),YCOMMENT,IRESP)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID_n
