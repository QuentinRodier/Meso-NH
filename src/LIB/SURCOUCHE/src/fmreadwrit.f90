!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODD_FM
IMPLICIT NONE 

INTEGER, PARAMETER :: JPXKRK = 100
INTEGER, PARAMETER :: JPXFIE = 1.5E8

TYPE FMHEADER
  INTEGER               :: GRID
  INTEGER               :: COMLEN
  CHARACTER(LEN=JPXKRK) :: COMMENT 
END TYPE FMHEADER

END MODULE MODD_FM

SUBROUTINE FM_READ_ll(KFLU,HRECFM,OREAL,KLENG,KFIELD,TPFMH,KRESP)
USE MODD_FM
USE MODD_CONFZ, ONLY : NZ_VERB
!
!*      0.    DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
INTEGER,                 INTENT(IN) :: KFLU   ! Fortran Logical Unit
CHARACTER(LEN=*),        INTENT(IN) :: HRECFM ! name of the desired article
LOGICAL,                 INTENT(IN) :: OREAL  ! TRUE IF TRANSMITTED KFIELD IS REAL 
INTEGER,                 INTENT(IN) :: KLENG  ! length of the data field
INTEGER,DIMENSION(KLENG),INTENT(OUT):: KFIELD ! array containing the data field
TYPE(FMHEADER),          INTENT(OUT):: TPFMH  ! FM-File Header
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
!JUAN
INTEGER(KIND=LFI_INT) :: IRESP,ILENGA,IPOSEX,ITOTAL,INUMBR
INTEGER               :: J,IROW
!JUAN
INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE::IWORK
INTEGER,DIMENSION(1:JPXKRK)             ::ICOMMENT
!
!*      0.3   Taskcommon for logical units
!
!
!------------------------------------------------------------------

!
!*      1.2   WE LOOK FOR THE FILE'S LOGICAL UNIT
!

INUMBR = KFLU

!
!*      2.a   LET'S GET SOME INFORMATION ON THE DESIRED ARTICLE
!
CALL LFINFO(IRESP,INUMBR,HRECFM,ILENGA,IPOSEX)
IF (IRESP.NE.0) THEN
  GOTO 1000
ELSEIF (ILENGA.EQ.0) THEN
  IRESP=-47
  GOTO 1000
ELSEIF (ILENGA.GT.JPXFIE) THEN
  IRESP=-48
  GOTO 1000
ENDIF

!
!*      2.b   UNFORMATTED DIRECT ACCESS READ OPERATION
!
ITOTAL=ILENGA
IF ( NZ_VERB .GE. 5 ) print *," fmreadwrit.f90:: FM_READ_ll ILENGA=",ILENGA," HRECFM=",HRECFM
ALLOCATE(IWORK(ITOTAL))

CALL LFILEC(IRESP,INUMBR,HRECFM,IWORK,ITOTAL)
IF (IRESP.NE.0) GOTO 1000
!
!*      2.c   THE GRID INDICATOR AND THE COMMENT STRING
!*            ARE SEPARATED FROM THE DATA
!
TPFMH%GRID   = IWORK(1)
TPFMH%COMLEN = IWORK(2)

IROW=KLENG+TPFMH%COMLEN+2
IF (ITOTAL.NE.IROW) THEN
  PRINT *,'KLENG =',KLENG
  PRINT *,'diff = ',ITOTAL-(TPFMH%COMLEN+2)
  IRESP=-63
  GOTO 1000
ENDIF

SELECT CASE (TPFMH%COMLEN)
CASE(:-1)
  IRESP=-58
  GOTO 1000
CASE(0)
  IRESP = 0
CASE(1:JPXKRK)
  ICOMMENT(1:TPFMH%COMLEN)=IWORK(3:TPFMH%COMLEN+2)
  DO J=1,TPFMH%COMLEN
    TPFMH%COMMENT(J:J)=CHAR(ICOMMENT(J))
  ENDDO
CASE(JPXKRK+1:)
  IRESP=-56
  GOTO 1000
END SELECT

IF (OREAL) THEN
  CALL TRANSFR(KFIELD,IWORK(TPFMH%COMLEN+3),KLENG) 
ELSE 
  KFIELD(1:KLENG) = IWORK(TPFMH%COMLEN+3:ITOTAL)
END IF
!
!  this is a pure binary field: no uncompressing of any kind
!
!*      3.    MESSAGE PRINTING WHATEVER THE ISSUE WAS
!
1000 CONTINUE

KRESP=IRESP

IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)

RETURN
END SUBROUTINE FM_READ_ll

SUBROUTINE FM_WRIT_ll(KFLU,HRECFM,OREAL,KLENG,KFIELD,TPFMH,KRESP)

USE MODD_FM

IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
INTEGER,                 INTENT(IN) :: KFLU   ! Fortran Logical Unit
CHARACTER(LEN=*),        INTENT(IN) :: HRECFM ! name of the article to be written     
LOGICAL,                 INTENT(IN) :: OREAL  ! TRUE IF TRANSMITTED KFIELD IS REAL 
INTEGER,                 INTENT(IN) :: KLENG  ! length of the data field
INTEGER,DIMENSION(KLENG),INTENT(IN) :: KFIELD ! array containing the data field
TYPE(FMHEADER),          INTENT(IN) :: TPFMH  ! FM-File Header
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
!JUAN
INTEGER(kind=LFI_INT) :: IRESP,ITOTAL,INUMBR
INTEGER         :: J
!JUAN
INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE::IWORK
INTEGER,DIMENSION(1:JPXKRK)             ::ICOMMENT
!
!*      1.2   WE LOOK FOR THE FILE'S LOGICAL UNIT
!

INUMBR = KFLU

!
!*      2.    GRID INDICATOR, COMMENT AND DATA ARE PUT TOGETHER
!

IF (KLENG.LE.0) THEN
  IRESP=-40
  GOTO 1000
ELSEIF (KLENG.GT.JPXFIE) THEN
  IRESP=-43
  GOTO 1000
ELSEIF ((TPFMH%GRID.LT.0).OR.(TPFMH%GRID.GT.8)) THEN
  IRESP=-46
  GOTO 1000
ENDIF

ITOTAL=KLENG+1+TPFMH%COMLEN+1
ALLOCATE(IWORK(ITOTAL))

IWORK(1)=TPFMH%GRID

SELECT CASE (TPFMH%COMLEN)
CASE(:-1)
  IRESP=-55
  GOTO 1000
CASE(0)
  IWORK(2)=TPFMH%COMLEN
CASE(1:JPXKRK)
  DO J=1,TPFMH%COMLEN
    ICOMMENT(J)=ICHAR(TPFMH%COMMENT(J:J))
  ENDDO
  IWORK(2)=TPFMH%COMLEN
  IWORK(3:TPFMH%COMLEN+2)=ICOMMENT(1:TPFMH%COMLEN)
CASE(JPXKRK+1:)
  IRESP=-57
  GOTO 1000
END SELECT

IF (OREAL) THEN
  CALL TRANSFW(IWORK(TPFMH%COMLEN+3),KFIELD,KLENG)
ELSE
  IWORK(TPFMH%COMLEN+3:ITOTAL)=KFIELD(1:KLENG)
END IF

!
!  no compressing of any kind: the data is pure binary
!
!*      3.    UNFORMATTED, DIRECT ACCESS WRITE OPERATION
!
CALL LFIECR(IRESP,INUMBR,HRECFM,IWORK,ITOTAL)


!
!*      4.    MESSAGE PRINTING WHATEVER THE ISSUE WAS
!
1000 CONTINUE

KRESP=IRESP

IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)  

RETURN
END SUBROUTINE FM_WRIT_ll

SUBROUTINE TRANSFR(KDEST,KSOURCE,KSIZE)
IMPLICIT NONE 
INTEGER                          :: KSIZE
REAL(KIND=8)   , DIMENSION(KSIZE):: KSOURCE
REAL           , DIMENSION(KSIZE):: KDEST

KDEST(:) = KSOURCE(:)

END SUBROUTINE TRANSFR

SUBROUTINE TRANSFW(KDEST,KSOURCE,KSIZE)
IMPLICIT NONE 
INTEGER                          :: KSIZE
REAL(KIND=8)   , DIMENSION(KSIZE):: KDEST
REAL           , DIMENSION(KSIZE):: KSOURCE

KDEST(:) = KSOURCE(:)

END SUBROUTINE TRANSFW

