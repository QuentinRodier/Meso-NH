!MNH_LIC Copyright 2020-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_STATION_READER
!     #######################
!
INTERFACE
!
SUBROUTINE READ_CSV_STATION( HFILE, TPSTATIONS, OCARTESIAN )
USE MODD_STATION_n
CHARACTER(LEN=*),                          INTENT(IN)    :: HFILE      ! file to read
TYPE(TSTATIONDATA), DIMENSION(:), POINTER, INTENT(INOUT) :: TPSTATIONS
LOGICAL,                                   INTENT(IN)    :: OCARTESIAN
END SUBROUTINE READ_CSV_STATION
!
END INTERFACE
!
END MODULE MODI_STATION_READER
!-------------------------------------------------------------------
!
!!****  *READ_CSV_STATION* -
!!
!!    PURPOSE
!!    -------
!!    Prescribe probes through a CSV file
!!
!!    AUTHOR
!!    ------
!!     E. Jezequel *CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     03/2020      Original
!  P. Wautelet 07/04/2022: rewrite types for stations
!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE READ_CSV_STATION( HFILE, TPSTATIONS, OCARTESIAN )
USE MODD_ALLSTATION_n
USE MODD_STATION_n
USE MODD_PARAMETERS
USE MODD_TYPE_STATION
USE MODI_INI_SURFSTATION_n

!
CHARACTER(LEN=*),                          INTENT(IN)    :: HFILE      ! file to read
TYPE(TSTATIONDATA), DIMENSION(:), POINTER, INTENT(INOUT) :: TPSTATIONS
LOGICAL,                                   INTENT(IN)    :: OCARTESIAN
!
INTEGER            :: INBLINE      ! Nb of line in csv file
!
CHARACTER(LEN=80)  :: YERROR
CHARACTER(LEN=400) :: YSTRING
INTEGER            :: ILU     ! logical unit of the file
!

! Open file
OPEN(NEWUNIT=ILU,FILE=HFILE, FORM='formatted')
! Count lines  
REWIND(ILU)
INBLINE=0
DO
 READ(ILU,END=101,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
 CALL FRENCH_TO_ENGLISH(YSTRING)                                         ! analyse de convention fr ou eng
 IF (LEN_TRIM(YSTRING) > 0) THEN
  INBLINE = INBLINE + 1
 END IF
END DO
!
101 CONTINUE
 IF (INBLINE == 0) THEN
  YERROR = 'Data not found in file : '//TRIM(HFILE)
  PRINT*, YERROR
 ELSE 
  ! Save number of stations
  NUMBSTAT = INBLINE - 1 

  ALLOCATE( TPSTATIONS(NUMBSTAT) )

  ! New reading
  REWIND(ILU)
  READ(ILU,FMT='(A400)') YSTRING ! Reading of header
  !
  ! Save the data
  IF (OCARTESIAN) THEN
   INBLINE = 1
   DO INBLINE=1, NUMBSTAT
    READ(ILU,FMT='(A400)') YSTRING
    READ(YSTRING,*) TPSTATIONS(INBLINE)%CNAME, & !TPSTATIONS(INBLINE)%CTYPE,&
    TPSTATIONS(INBLINE)%XX, TPSTATIONS(INBLINE)%XY, TPSTATIONS(INBLINE)%XZ
   END DO
   REWIND(ILU)
   CLOSE(ILU)
   RETURN
  ELSE
   INBLINE = 1
   DO INBLINE=1, NUMBSTAT
    READ(ILU,FMT='(A400)') YSTRING
    READ(YSTRING,*) TPSTATIONS(INBLINE)%CNAME, & !TPSTATIONS(INBLINE)%CTYPE,&
    TPSTATIONS(INBLINE)%XLAT, TPSTATIONS(INBLINE)%XLON, TPSTATIONS(INBLINE)%XZ
   END DO
   REWIND(ILU)
   CLOSE(ILU)
   RETURN
  END IF
 END IF
!
END SUBROUTINE READ_CSV_STATION
!#########################################################
SUBROUTINE FRENCH_TO_ENGLISH(HSTRING)
CHARACTER(LEN=400), INTENT(INOUT) :: HSTRING ! csv record
INTEGER :: JL
LOGICAL :: GFRENCH
!
GFRENCH = .FALSE.
!* analyses if the record has been written in French convention 
!     French  convention (separator is ;  decimal symbol is ,) 
!  or English convention (separator is ,  decimal symbol is .)
DO JL=1,400
 IF (HSTRING(JL:JL)==';') GFRENCH=.TRUE.
END DO
!
! If French convention is used in the file, transforms it in English convention
IF (GFRENCH) THEN
 DO JL=1,400
   IF (HSTRING(JL:JL)==',') HSTRING(JL:JL)='.'
   IF (HSTRING(JL:JL)==';') HSTRING(JL:JL)=','
 END DO
END IF
!
END SUBROUTINE FRENCH_TO_ENGLISH

