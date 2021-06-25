!MNH_LIC Copyright 2020-2021 CNRS, Meteo-France and Universite Paul Sabatier
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
SUBROUTINE READ_CSV_STATION(KLUNAM,HFILE,TPSTATION,OCARTESIAN)
        USE MODD_ALLSTATION_n
        USE MODD_STATION_n
        USE MODD_PARAMETERS
        USE MODD_TYPE_STATION
        USE MODI_INI_SURFSTATION_n
        INTEGER,            INTENT(IN)  :: KLUNAM       ! logical unit of the file
        CHARACTER(LEN=*),   INTENT(IN)  :: HFILE        ! file to read    
        TYPE(STATION),      INTENT(OUT) :: TPSTATION       ! stored blade data
        LOGICAL,            INTENT(IN)  :: OCARTESIAN
END SUBROUTINE READ_CSV_STATION
!
END INTERFACE
!
END MODULE MODI_STATION_READER
!-------------------------------------------------------------------
!
!!****  *EOL_READER* -
!!
!!    PURPOSE
!!    -------
!!    Prescribe probes through a CSV file
!!
!!    AUTHOR
!!    ------
!!     E. Jézéquel 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     03/2020      Original
!!
!!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE READ_CSV_STATION(KLUNAM,HFILE,TPSTATION,OCARTESIAN)
USE MODD_ALLSTATION_n
USE MODD_STATION_n
USE MODD_PARAMETERS
USE MODD_TYPE_STATION
USE MODI_INI_SURFSTATION_n

!
INTEGER,            INTENT(IN)    :: KLUNAM     ! logical unit of the file
CHARACTER(LEN=*),   INTENT(IN)    :: HFILE      ! file to read    
TYPE(STATION),      INTENT(INOUT) :: TPSTATION     ! dummy stored
LOGICAL,            INTENT(IN)    :: OCARTESIAN
!
INTEGER                           :: INBLINE      ! Nb of line in csv file
!
CHARACTER(LEN=80)                 :: YERROR
CHARACTER(LEN=400)                :: YSTRING   
!

! Open file
OPEN(UNIT=KLUNAM,FILE=HFILE, FORM='formatted')
! Count lines  
REWIND(KLUNAM)
INBLINE=0
DO
 READ(KLUNAM,END=101,FMT='(A400)') YSTRING
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
  ! Save number of station 
  NUMBSTAT = INBLINE - 1 
  !
  ! Allocation des tableaux
  ALLOCATE(TPSTATION%LAT(NUMBSTAT))
  ALLOCATE(TPSTATION%LON(NUMBSTAT)) 
  ALLOCATE(TPSTATION%X(NUMBSTAT))
  ALLOCATE(TPSTATION%Y(NUMBSTAT))
  ALLOCATE(TPSTATION%Z(NUMBSTAT))
  ALLOCATE(TPSTATION%K(NUMBSTAT))
  !ALLOCATE(TPSTATION%STEP(NUMBSTAT))
  ALLOCATE(TPSTATION%NAME(NUMBSTAT))
  ALLOCATE(TPSTATION%TYPE(NUMBSTAT))

  TPSTATION%LON  = XUNDEF
  TPSTATION%LAT  = XUNDEF
  TPSTATION%Z    = XUNDEF
  TPSTATION%K    = XUNDEF
  TPSTATION%X    = XUNDEF
  TPSTATION%Y    = XUNDEF
  TPSTATION%NAME = "        "
  TPSTATION%TYPE = "        "
  ! Nouvelle lecture 
  REWIND(KLUNAM)
  READ(KLUNAM,FMT='(A400)') YSTRING ! Lecture du header 
  !
  ! Save the data
  IF (OCARTESIAN) THEN
   INBLINE = 1
   DO INBLINE=1, NUMBSTAT
    READ(KLUNAM,FMT='(A400)') YSTRING
    READ(YSTRING,*) TPSTATION%NAME(INBLINE),TPSTATION%TYPE(INBLINE),& 
    TPSTATION%X(INBLINE), TPSTATION%Y(INBLINE), TPSTATION%Z(INBLINE)!,&
   END DO
   REWIND(KLUNAM)
   CLOSE(KLUNAM)
   RETURN
  ELSE
   INBLINE = 1
   DO INBLINE=1, NUMBSTAT
    READ(KLUNAM,FMT='(A400)') YSTRING
    READ(YSTRING,*) TPSTATION%NAME(INBLINE), TPSTATION%TYPE(INBLINE),&
    TPSTATION%LAT(INBLINE), TPSTATION%LON(INBLINE), TPSTATION%Z(INBLINE)!,&
   END DO
   REWIND(KLUNAM)
   CLOSE(KLUNAM)
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

