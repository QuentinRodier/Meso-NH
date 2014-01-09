!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 tools 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ####################
      PROGRAM LATLON_TO_XY
!     ####################
!
!!**** *LATLON_TO_XY* program to compute x and y from latitude and longiude
!!                    for a MESONH file
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    module FMLOOK        : to retrieve a logical unit number 
!!                           associated with a file
!!
!!    module MODE_GRIDPROJ : contains projection routines
!!                         SM_LATLON and SM_XYHAT
!!                           
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    module MODD_GRID     : variables for projection:
!!                         XLAT0,XLON0,XRPK,XBETA
!!
!!    module MODD_PGDDIM  : specify the dimentions of the data arrays:
!!                         NPGDIMAX and NPGDJMAX
!!
!!    module MODD_PGDGRID : grid variables:
!!                         XPGDLONOR,XPGDLATOR: longitude and latitude of the
!!                           origine point for the conformal projection.
!!                         XPGDXHAT,XPGDYHAT: position x,y in the conformal plane   
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     29/12/95
!!
!!    remove the USE MODI_DEFAULT_DESFM      Apr. 17, 1996 (J.Stein)
!!    no transfer of the file when closing   Dec. 09, 1996 (V.Masson)
!!    + changes call to READ_HGRID
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_FM
USE MODE_IO_ll
!
USE MODD_GRID      
USE MODD_PGDDIM
USE MODD_PGDGRID
USE MODD_PARAMETERS
USE MODD_LUNIT
!
USE MODE_GRIDPROJ
!
USE MODI_INI_CST
USE MODI_READ_HGRID
!
IMPLICIT NONE
!
!*    0.2    Declaration of variables
!            ------------------------
!
CHARACTER(LEN=28) :: YINIFILE  ! name of input FM file
CHARACTER(LEN=28) :: YNAME     ! true name of input FM file
CHARACTER(LEN=28) :: YDAD      ! name of dad of input FM file
CHARACTER(LEN=2)  :: YSTORAGE_TYPE
INTEGER :: INAM                ! Logical unit for namelist file
INTEGER :: ILUOUT0             ! Logical unit for output file.
INTEGER :: IRESP               ! Return-code if problem eraised.
INTEGER :: ININAR
REAL    :: ZLAT                ! input latitude
REAL    :: ZLON                ! input longitude
REAL    :: ZXHAT               ! output conformal coodinate x
REAL    :: ZYHAT               ! output conformal coodinate y
INTEGER :: II,IJ               ! indexes of the point
REAL    :: ZI,ZJ               ! fractionnal indexes of the point
!
!*    0.3    Declaration of namelists
!            ------------------------
!
NAMELIST/NAM_INIFILE/ YINIFILE
!----------------------------------------------------------------------------
!
    WRITE(*,*) '+---------------------------------+'
    WRITE(*,*) '|      program latlon_to_xy       |'
    WRITE(*,*) '+---------------------------------+'
    WRITE(*,*) ''
    WRITE(*,*) 'Warning: I and J are integer for flux points'
!
!*    1.     Initializations
!            ---------------
!
CALL INI_CST
!
!*    2.     Reading of namelist file
!            ------------------------
!
CALL INITIO_ll()
!
CALL OPEN_ll(UNIT=INAM,FILE='LATLON2XY1.nam',IOSTAT=IRESP,ACTION='READ', &
     DELIM='QUOTE',MODE=GLOBAL)
READ(INAM,NAM_INIFILE)
CALL CLOSE_ll('LATLON2XY1.nam',IOSTAT=IRESP)
!
!*    1.     Opening of MESONH file
!            ----------------------
!
CALL FMOPEN_ll(YINIFILE,'READ',CLUOUT0,0,2,2,ININAR,IRESP)
!
!*    2.     Reading of MESONH file
!            ----------------------
!
CALL READ_HGRID(0,YINIFILE,YNAME,YDAD,YSTORAGE_TYPE)
!
!*    3.     Closing of MESONH file
!            ----------------------
!
CALL FMCLOS_ll(YINIFILE,'KEEP',CLUOUT0,IRESP)
!
!-------------------------------------------------------------------------------
!
!*    4.     Reading of latitude and longitude
!            ---------------------------------
!
DO
  WRITE(*,*) '-------------------------------------------------------------------'
  WRITE(*,*) 'please enter the latitude  (in decimal degrees; quit or q to stop):'
  READ(*,*,ERR=1) ZLAT
  WRITE(*,*) 'please enter the longitude (in decimal degrees; quit or q to stop):'
  READ(*,*,ERR=1) ZLON
!
  CALL SM_XYHAT(XPGDLATOR,XPGDLONOR,   &
                ZLAT,ZLON,ZXHAT,ZYHAT)
!
  WRITE(*,*) 'x=', ZXHAT
  WRITE(*,*) 'y=', ZYHAT
!
  II=MAX(MIN(COUNT(XPGDXHAT(:)<ZXHAT),NPGDIMAX+2*JPHEXT-1),1)
  IJ=MAX(MIN(COUNT(XPGDYHAT(:)<ZYHAT),NPGDJMAX+2*JPHEXT-1),1)
  ZI=(ZXHAT-XPGDXHAT(II))/(XPGDXHAT(II+1)-XPGDXHAT(II))+FLOAT(II)
  ZJ=(ZYHAT-XPGDYHAT(IJ))/(XPGDYHAT(IJ+1)-XPGDYHAT(IJ))+FLOAT(IJ)
!
  IF (      (ZI>=1.) .AND. (ZI<=NPGDIMAX+2*JPHEXT+1) &
      .AND. (ZJ>=1.) .AND. (ZJ<=NPGDJMAX+2*JPHEXT+1) ) THEN
    WRITE(*,*) 'I=',ZI
    WRITE(*,*) 'J=',ZJ
  ELSE
    WRITE(*,*) 'point not in the domain'
    WRITE(*,*) 'I=',ZI
    WRITE(*,*) 'J=',ZJ
  END IF
END DO
1   WRITE(*,*) 'good bye'
!
!-------------------------------------------------------------------------------
!
END PROGRAM LATLON_TO_XY
