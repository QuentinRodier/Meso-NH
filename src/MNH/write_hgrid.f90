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
      SUBROUTINE WRITE_HGRID(KMI,HOUTFILE,HDADFILE)
!
INTEGER,           INTENT(IN) :: KMI      ! model index
CHARACTER (LEN=*), INTENT(IN) :: HOUTFILE ! name of the file n
CHARACTER (LEN=*), INTENT(IN) :: HDADFILE ! name of the father of file n
!
END SUBROUTINE WRITE_HGRID
END INTERFACE
END MODULE MODI_WRITE_HGRID
!
!     #############################################
      SUBROUTINE WRITE_HGRID(KMI,HOUTFILE,HDADFILE)
!     #############################################
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
INTEGER,           INTENT(IN) :: KMI      ! model index
CHARACTER (LEN=*), INTENT(IN) :: HOUTFILE ! name of the file n
CHARACTER (LEN=*), INTENT(IN) :: HDADFILE ! name of the father of file n
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
  CALL WRITE_HGRID_n(HOUTFILE,HDADFILE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     WRITING FROM MODD_PGD...
!	        ----------------------
!
YCOMMENT=' '
!-------------------------------------------------------------------------------
!
CALL FMWRIT(HOUTFILE,'MASDEV',CLUOUT0,'--',NMASDEV,0,LEN(YCOMMENT),YCOMMENT,IRESP)
CALL FMWRIT(HOUTFILE,'BUGFIX',CLUOUT0,'--',NBUGFIX,0,LEN(YCOMMENT),YCOMMENT,IRESP)
CALL FMWRIT(HOUTFILE,'BIBUSER',CLUOUT0,'--',CBIBUSER,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
CALL FMWRIT(HOUTFILE,'MY_NAME',CLUOUT0,'--',HOUTFILE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
CALL FMWRIT(HOUTFILE,'DAD_NAME',CLUOUT0,'--',HDADFILE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!-------------------------------------------------------------------------------
!
CALL FMWRIT(HOUTFILE,'STORAGE_TYPE',CLUOUT0,'--',CSTORAGE_TYPE,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LAT0',CLUOUT0,'--',XLAT0,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LON0',CLUOUT0,'--',XLON0,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'RPK' ,CLUOUT0,'--',XRPK ,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'BETA',CLUOUT0,'--',XBETA,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LATOR',CLUOUT0,'--',XPGDLATOR,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='DEGREES'
CALL FMWRIT(HOUTFILE,'LONOR',CLUOUT0,'--',XPGDLONOR,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'IMAX' ,CLUOUT0,'--',NPGDIMAX,0,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT=' '
CALL FMWRIT(HOUTFILE,'JMAX' ,CLUOUT0,'--',NPGDJMAX,0,LEN(YCOMMENT),YCOMMENT,IRESP)
!
YCOMMENT='METERS'
CALL FMWRIT(HOUTFILE,'XHAT',CLUOUT0,'XX',XPGDXHAT,2,LEN(YCOMMENT),YCOMMENT,IRESP)
YCOMMENT='METERS'
CALL FMWRIT(HOUTFILE,'YHAT',CLUOUT0,'YY',XPGDYHAT,3,LEN(YCOMMENT),YCOMMENT,IRESP)
!
IF (CSTORAGE_TYPE=='MT' .OR. CSTORAGE_TYPE=='TT') THEN
  YCOMMENT=' '
  CALL FMWRIT(HOUTFILE,'THINSHELL',CLUOUT0,'--',LTHINSHELL,0,LEN(YCOMMENT),YCOMMENT,IRESP)
  YCOMMENT=' '
  CALL FMWRIT(HOUTFILE,'CARTESIAN',CLUOUT0,'--',LCARTESIAN,0,LEN(YCOMMENT),YCOMMENT,IRESP)
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_HGRID
