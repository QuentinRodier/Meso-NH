!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 tools 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ####################
      PROGRAM XY_TO_LATLON
!     ####################
!
!!**** *XY_TO_LATLON* program to compute latitude and longiude from x and y
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
!!    Original     26/01/96
!!
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
USE MODN_CONFIO
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
REAL    :: ZI,ZJ               ! input positions of the point
INTEGER :: II,IJ               ! integer positions of the point
REAL    :: ZXHAT               ! output conformal coodinate x
REAL    :: ZYHAT               ! output conformal coodinate y
REAL    :: ZLAT                ! output latitude
REAL    :: ZLON                ! output longitude
!
!*    0.3    Declaration of namelists
!            ------------------------
!
NAMELIST/NAM_INIFILE/ YINIFILE
!----------------------------------------------------------------------------
!
    WRITE(*,*) '+---------------------------------+'
    WRITE(*,*) '|      program xy_to_latlon       |'
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
CALL INITIO_ll()
!
CALL OPEN_ll(UNIT=INAM,FILE='XY2LATLON1.nam',IOSTAT=IRESP,ACTION='READ', &
     DELIM='QUOTE',MODE=GLOBAL)
READ(INAM,NAM_INIFILE)
READ(INAM,NAM_CONFIO)
CALL SET_CONFIO_ll(LCDF4, LLFIOUT, LLFIREAD)
CALL CLOSE_ll('XY2LATLON1.nam',IOSTAT=IRESP)
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
!*    4.     Reading of I and J
!            ------------------
!
DO
  WRITE(*,*) '-------------------------------------------------------------------'
  WRITE(*,*) 'please enter index I  (real, quit or q to stop):'
  READ(*,*,ERR=1) ZI
  WRITE(*,*) 'please enter index J  (real, quit or q to stop):'
  READ(*,*,ERR=1) ZJ
!
  II=MAX(MIN(INT(ZI),NPGDIMAX+2*JPHEXT-1),1)
  IJ=MAX(MIN(INT(ZJ),NPGDJMAX+2*JPHEXT-1),1)
  ZXHAT=XPGDXHAT(II) + (ZI-FLOAT(II)) * ( XPGDXHAT(II+1) - XPGDXHAT(II) )
  ZYHAT=XPGDYHAT(IJ) + (ZJ-FLOAT(IJ)) * ( XPGDYHAT(IJ+1) - XPGDYHAT(IJ) )
!
  WRITE(*,*) 'x=', ZXHAT
  WRITE(*,*) 'y=', ZYHAT
!
  CALL SM_LATLON(XPGDLATOR,XPGDLONOR,   &
                 ZXHAT,ZYHAT,ZLAT,ZLON)
!
  WRITE(*,*) 'lat=', ZLAT
  WRITE(*,*) 'lon=', ZLON
END DO
1   WRITE(*,*) 'good bye'
!
!-------------------------------------------------------------------------------
!
END PROGRAM XY_TO_LATLON
