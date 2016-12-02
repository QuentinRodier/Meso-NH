!MNH_LIC Copyright 2016 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
MODULE MODE_FIELD
!
IMPLICIT NONE
!
INTEGER,PRIVATE,PARAMETER :: MAXFIELDS = 2
INTEGER,PARAMETER :: TYPEUNDEF = -1, TYPEINT = 1, TYPELOG = 2, TYPEREAL = 3, TYPECHAR = 4
!
!Structure describing the characteristics of a field
TYPE TFIELDDATA
  CHARACTER(LEN=16)  :: CMNHNAME  = '' !Name of the field (for MesoNH, non CF convention)
  CHARACTER(LEN=32)  :: CSTDNAME  = '' !Standard name (CF convention)
  CHARACTER(LEN=32)  :: CLONGNAME = '' !Long name (CF convention)
  CHARACTER(LEN=32)  :: CUNITS    = '' !Canonical units (CF convention)
  CHARACTER(LEN=2)   :: CDIR      = '' !Type of the data field (XX,XY,--...)
  CHARACTER(LEN=100) :: CCOMMENT  = '' !Comment (for MesoNH, non CF convention)
  INTEGER            :: NGRID     = -1 !Localization on the model grid
  INTEGER            :: NTYPE     = TYPEUNDEF !Datatype
END TYPE TFIELDDATA
!
LOGICAL :: LFIELDLIST_ISINIT = .FALSE.
TYPE(TFIELDDATA),DIMENSION(MAXFIELDS) :: TFIELDLIST
!
CONTAINS
!
SUBROUTINE INI_FIELD_LIST()
!
INTEGER :: IDX
!
!F90/95: TFIELDLIST(1) = TFIELDDATA('UT','x_wind','m s-1','XY','X_Y_Z_U component of wind (m/s)',2)
!F2003:
!TFIELDLIST(1) = TFIELDDATA(CMNHNAME='UT',CSTDNAME='x_wind',CUNITS='m s-1',CDIR='XY',&
!                           CCOMMENT='X_Y_Z_U component of wind (m/s)',NGRID=2)
IDX = 1
!
TFIELDLIST(IDX)%CMNHNAME   = 'MASDEV'
TFIELDLIST(IDX)%CSTDNAME   = ''
TFIELDLIST(IDX)%CLONGNAME  = 'MesoNH version (without bugfix)'
TFIELDLIST(IDX)%CUNITS     = ''
TFIELDLIST(IDX)%CDIR       = '--'
TFIELDLIST(IDX)%CCOMMENT   = ''
TFIELDLIST(IDX)%NGRID      = 0
TFIELDLIST(IDX)%NTYPE      = TYPEINT
IDX = IDX+1
!
TFIELDLIST(IDX)%CMNHNAME   = 'UT'
TFIELDLIST(IDX)%CSTDNAME   = 'x_wind'
TFIELDLIST(IDX)%CLONGNAME  = ''
TFIELDLIST(IDX)%CUNITS     = 'm s-1'
TFIELDLIST(IDX)%CDIR       = 'XY'
TFIELDLIST(IDX)%CCOMMENT   = 'X_Y_Z_U component of wind (m/s)'
TFIELDLIST(IDX)%NGRID      = 2
TFIELDLIST(IDX)%NTYPE      = TYPEREAL
IDX = IDX+1
!
LFIELDLIST_ISINIT = .TRUE.
!
END SUBROUTINE INI_FIELD_LIST
!
SUBROUTINE FIND_FIELD_ID_FROM_MNHNAME(HMNHNAME,KID,KRESP)
!
CHARACTER(LEN=*),            INTENT(IN) :: HMNHNAME !Name of the field to find
INTEGER,                     INTENT(OUT):: KID      !Index of the field
INTEGER,                     INTENT(OUT):: KRESP    !Return-code 
!
INTEGER :: JI
!
KID = 0
KRESP = 0
!
DO JI = 1,MAXFIELDS
  IF (TRIM(TFIELDLIST(JI)%CMNHNAME)==TRIM(HMNHNAME)) THEN
    KID = JI
    EXIT
  END IF
END DO
!
IF (KID==0) THEN
  !Field not found
  KRESP = -1
  PRINT *,'WARNING: FIND_FIELD_ID_FROM_MNHNAME: field ',TRIM(HMNHNAME),' not known'
END IF
!
END SUBROUTINE FIND_FIELD_ID_FROM_MNHNAME
!
END MODULE MODE_FIELD
