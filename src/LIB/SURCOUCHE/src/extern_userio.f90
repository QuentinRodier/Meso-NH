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

!
! This file contains all the user IO routines. All these external routines have
! an interface in the modi_allio.f90 file.
!

!
! Routines found in the MODE_IO_ll module : INITIO_ll, OPEN_ll, CLOSE_ll, FLUSH_ll
!
SUBROUTINE INITIO_ll()
USE MODE_IO_ll, ONLY : E_INITIO_ll=>INITIO_ll
IMPLICIT NONE 

CALL E_INITIO_ll()

END SUBROUTINE INITIO_ll

SUBROUTINE OPEN_ll(UNIT,FILE,MODE,LFIPAR,COMM,STATUS,ACCESS,  &
     IOSTAT,FORM,RECL,BLANK,POSITION,ACTION,DELIM,PAD,OPARALLELIO)
USE MODE_IO_ll, ONLY : E_OPEN_ll=>OPEN_ll
USE MODD_IO_ll, ONLY : LFIPARAM
IMPLICIT NONE 

INTEGER,         INTENT(OUT)           :: UNIT  !! Different from fortran OPEN
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: FILE
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: MODE
TYPE(LFIPARAM),  POINTER,     OPTIONAL :: LFIPAR
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: STATUS
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: ACCESS
INTEGER,         INTENT(OUT)           :: IOSTAT
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: FORM
INTEGER,         INTENT(IN),  OPTIONAL :: RECL
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: BLANK
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: POSITION
CHARACTER(len=*),INTENT(IN)            :: ACTION
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: DELIM
CHARACTER(len=*),INTENT(IN),  OPTIONAL :: PAD
INTEGER,         INTENT(IN),  OPTIONAL :: COMM
LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO

IF ( PRESENT(OPARALLELIO) ) THEN
  CALL E_OPEN_ll(UNIT,FILE,MODE,LFIPAR,COMM,STATUS,ACCESS,  &
      IOSTAT,FORM,RECL,BLANK,POSITION,ACTION,DELIM,PAD,OPARALLELIO=OPARALLELIO)
ELSE
  CALL E_OPEN_ll(UNIT,FILE,MODE,LFIPAR,COMM,STATUS,ACCESS,  &
      IOSTAT,FORM,RECL,BLANK,POSITION,ACTION,DELIM,PAD)
ENDIF

END SUBROUTINE OPEN_ll

SUBROUTINE CLOSE_ll(HFILE,IOSTAT,STATUS,OPARALLELIO)
USE MODE_IO_ll, ONLY : E_CLOSE_ll=>CLOSE_ll
IMPLICIT NONE 

CHARACTER(LEN=*), INTENT(IN)            :: HFILE
INTEGER,          INTENT(OUT), OPTIONAL :: IOSTAT
CHARACTER(LEN=*), INTENT(IN),  OPTIONAL :: STATUS
LOGICAL,          INTENT(IN),  OPTIONAL :: OPARALLELIO

IF( PRESENT(OPARALLELIO) ) THEN
  CALL E_CLOSE_ll(HFILE,IOSTAT,STATUS,OPARALLELIO=OPARALLELIO)
ELSE
  CALL E_CLOSE_ll(HFILE,IOSTAT,STATUS)
ENDIF

END SUBROUTINE CLOSE_ll

SUBROUTINE FLUSH_ll(HFILE,IRESP)
USE MODE_IO_ll, ONLY : E_FLUSH_ll=>FLUSH_ll
IMPLICIT NONE 

CHARACTER(LEN=*), INTENT(IN)            :: HFILE
INTEGER,          INTENT(OUT), OPTIONAL :: IRESP

CALL E_FLUSH_ll(HFILE,IRESP)

END SUBROUTINE FLUSH_ll


!
! Routines found in the MODE_FM module : FMATTR_ll, FMLOOK_ll, FMOPEN_ll, FMCLOS_ll
!
SUBROUTINE SET_FMPACK_ll(O1D,O2D,OPACK)
USE MODE_FM, ONLY : E_SET_FMPACK_ll=>SET_FMPACK_ll
LOGICAL, INTENT(IN) :: O1D,O2D,OPACK

CALL E_SET_FMPACK_ll(O1D,O2D,OPACK)

END SUBROUTINE SET_FMPACK_ll

SUBROUTINE FMATTR_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
USE MODE_FM, ONLY : E_FMATTR_ll=>FMATTR_ll
IMPLICIT NONE 
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP

CALL E_FMATTR_ll(HFILEM,HFIPRI,KNUMBR,KRESP)

END SUBROUTINE FMATTR_ll

SUBROUTINE FMLOOK_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
USE MODE_FM, ONLY : E_FMLOOK_ll=>FMLOOK_ll
IMPLICIT NONE 
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP

CALL E_FMLOOK_ll(HFILEM,HFIPRI,KNUMBR,KRESP)

END SUBROUTINE FMLOOK_ll

SUBROUTINE FMOPEN_ll(HFILEM,HACTION,HFIPRI,KNPRAR,KFTYPE,KVERB,KNINAR&
     & ,KRESP,OPARALLELIO)
USE MODE_FM, ONLY : E_FMOPEN_ll=>FMOPEN_ll
IMPLICIT NONE 
CHARACTER(LEN=*),INTENT(IN) ::HFILEM  ! name of the file.
CHARACTER(LEN=*),INTENT(IN) ::HACTION ! Action upon the file
                                      ! 'READ' or 'WRITE'
CHARACTER(LEN=*),INTENT(IN) ::HFIPRI  ! file for prints in FM.
INTEGER,         INTENT(IN) ::KNPRAR  ! number of predicted
                                      ! articles  (not vital).
INTEGER,         INTENT(IN) ::KFTYPE  ! type of FM-file.
INTEGER,         INTENT(IN) ::KVERB   ! level of verbose.
INTEGER,         INTENT(OUT)::KNINAR  ! number of articles initially present in the file.
INTEGER,         INTENT(OUT)::KRESP   ! return-code if a problem araised.
LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO

IF( PRESENT(OPARALLELIO) ) THEN
  CALL E_FMOPEN_ll(HFILEM,HACTION,HFIPRI,KNPRAR,KFTYPE,KVERB,KNINAR,KRESP,OPARALLELIO=OPARALLELIO)
ELSE
  CALL E_FMOPEN_ll(HFILEM,HACTION,HFIPRI,KNPRAR,KFTYPE,KVERB,KNINAR,KRESP)
ENDIF

END SUBROUTINE FMOPEN_ll

SUBROUTINE FMCLOS_ll(HFILEM,HSTATU,HFIPRI,KRESP,OPARALLELIO)
USE MODE_FM, ONLY : E_FMCLOS_ll=>FMCLOS_ll
IMPLICIT NONE
CHARACTER(LEN=*),     INTENT(IN) ::HFILEM  ! file name
CHARACTER(LEN=*),     INTENT(IN) ::HSTATU  ! status for the closed file
CHARACTER(LEN=*),     INTENT(IN) ::HFIPRI  ! file for prints in FM
INTEGER,              INTENT(OUT)::KRESP   ! return-code if problems araised
LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO

IF( PRESENT(OPARALLELIO) ) THEN
  CALL E_FMCLOS_ll(HFILEM,HSTATU,HFIPRI,KRESP,OPARALLELIO=OPARALLELIO)
ELSE
  CALL E_FMCLOS_ll(HFILEM,HSTATU,HFIPRI,KRESP)
ENDIF

END SUBROUTINE FMCLOS_ll

!
! Routines found in the MODE_FMREAD module : FMREADxxx
!
SUBROUTINE FMREADX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX0_ll=>FMREADX0_ll
IMPLICIT NONE 
CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
REAL,                         INTENT(OUT)::PFIELD ! array containing the data field 
INTEGER,                      INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                      INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),             INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                      INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX0_ll

SUBROUTINE FMREADX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP, KIMAX_ll, KJMAX_ll)
USE MODE_FMREAD, ONLY : E_FMREADX1_ll=>FMREADX1_ll
IMPLICIT NONE
CHARACTER(LEN=*),        INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),        INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),        INTENT(IN) ::HDIR     ! Field form
REAL,DIMENSION(:),TARGET,INTENT(OUT)::PFIELD   ! array containing the data field 
INTEGER,                 INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                 INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),        INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                 INTENT(OUT)::KRESP    ! return-code
INTEGER, OPTIONAL, INTENT(IN) ::KIMAX_ll
INTEGER, OPTIONAL, INTENT(IN) ::KJMAX_ll

IF( PRESENT(KIMAX_ll) .AND. PRESENT(KJMAX_ll) ) THEN
  CALL E_FMREADX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP,KIMAX_ll,KJMAX_ll)
ELSE
  CALL E_FMREADX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)
ENDIF

END SUBROUTINE FMREADX1_ll

SUBROUTINE FMREADX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX2_ll=>FMREADX2_ll
IMPLICIT NONE
CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:),TARGET, INTENT(OUT)::PFIELD   ! array containing the data field
INTEGER,                    INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                    INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),           INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP     ! return-code

CALL E_FMREADX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX2_ll

SUBROUTINE FMREADX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX3_ll=>FMREADX3_ll
IMPLICIT NONE 
CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
REAL, DIMENSION(:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                      INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),             INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                      INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX3_ll
  
SUBROUTINE FMREADX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX4_ll=>FMREADX4_ll
IMPLICIT NONE
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(OUT)::PFIELD   ! array containing the data field
INTEGER,                       INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                       INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),              INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                       INTENT(OUT)::KRESP  ! return-code if

CALL E_FMREADX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX4_ll

SUBROUTINE FMREADX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX5_ll=>FMREADX5_ll
IMPLICIT NONE 
CHARACTER(LEN=*),                INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),                INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),                INTENT(IN) ::HDIR   ! field form
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                         INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                         INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),                INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                         INTENT(OUT)::KRESP  ! return-code

CALL E_FMREADX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX5_ll

SUBROUTINE FMREADX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADX6_ll=>FMREADX6_ll
IMPLICIT NONE
CHARACTER(LEN=*),                  INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),                  INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),                  INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),                  INTENT(IN) ::HDIR   ! field form
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                           INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                           INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),                  INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                           INTENT(OUT)::KRESP  ! return-code

CALL E_FMREADX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADX6_ll
  
SUBROUTINE FMREADN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADN0_ll=>FMREADN0_ll
IMPLICIT NONE
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
INTEGER,                   INTENT(OUT)::KFIELD ! array containing the data field     
INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADN0_ll

SUBROUTINE FMREADN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADN1_ll=>FMREADN1_ll
IMPLICIT NONE 
CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! Field form
INTEGER,DIMENSION(:),TARGET,INTENT(OUT)::KFIELD ! array containing the data field     
INTEGER,                    INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                    INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),           INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                    INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADN1_ll

SUBROUTINE FMREADN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADN2_ll=>FMREADN2_ll
IMPLICIT NONE 
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HDIR   ! field form
INTEGER, DIMENSION(:,:),TARGET,INTENT(OUT)::KFIELD ! array containing the data field
INTEGER,                       INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                       INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),              INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                       INTENT(OUT)::KRESP  ! return-code

CALL E_FMREADN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADN2_ll

SUBROUTINE FMREADL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADL0_ll=>FMREADL0_ll
IMPLICIT NONE
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR   ! field form
LOGICAL,                   INTENT(OUT)::OFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADL0_ll

SUBROUTINE FMREADL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADL1_ll=>FMREADL1_ll
IMPLICIT NONE
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM  ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM  ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI  ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR    ! Field form
LOGICAL, DIMENSION(:),     INTENT(OUT)::OFIELD  ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADL1_ll

SUBROUTINE FMREADC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADC0_ll=>FMREADC0_ll
IMPLICIT NONE 
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
CHARACTER(LEN=*),          INTENT(OUT)::HFIELD   ! array containing the data field    
INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADC0_ll

SUBROUTINE FMREADT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREADT0_ll=>FMREADT0_ll
USE MODD_TYPE_DATE
IMPLICIT NONE 
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
TYPE (DATE_TIME),          INTENT(OUT)::TFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMREADT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREADT0_ll

SUBROUTINE FMREAD_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,&
     & KGRID,KLENCH,HCOMMENT,KRESP)
USE MODE_FMREAD, ONLY : E_FMREAD_LB=>FMREAD_LB
IMPLICIT NONE 
CHARACTER(LEN=*),     INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*),     INTENT(IN) ::HRECFM   ! name of the article to be written
CHARACTER(LEN=*),     INTENT(IN) ::HFIPRI   ! file for prints
CHARACTER(LEN=*),     INTENT(IN) ::HLBTYPE  ! 'LBX','LBXU','LBY' or 'LBYV'
REAL, DIMENSION(:,:,:),TARGET, INTENT(OUT)::PLB ! array containing the LB field
INTEGER,              INTENT(IN) :: KRIM  ! size of the LB area
INTEGER,              INTENT(IN) :: KL3D  ! size of the LB array in FM
INTEGER,              INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,              INTENT(OUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),     INTENT(OUT)::HCOMMENT ! comment string
INTEGER,              INTENT(OUT)::KRESP  ! return-code 

CALL E_FMREAD_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMREAD_LB

!
! Routines found in the MODE_FMWRIT module : FMWRITxxx
!
SUBROUTINE FMWRITX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX0_ll=>FMWRITX0_ll
IMPLICIT NONE 
CHARACTER(LEN=*),        INTENT(IN) ::HFILEM  ! FM-file name
CHARACTER(LEN=*),        INTENT(IN) ::HRECFM  ! name of the article to write
CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI  ! output file for error messages
CHARACTER(LEN=*),        INTENT(IN) ::HDIR    ! field form
REAL,                    INTENT(IN) ::PFIELD  ! array containing the data field
INTEGER,                 INTENT(IN) ::KGRID   ! C-grid indicator (u,v,w,T)
INTEGER,                 INTENT(IN) ::KLENCH  ! length of comment string
CHARACTER(LEN=*),        INTENT(IN) ::HCOMMENT! comment string
INTEGER,                 INTENT(OUT)::KRESP   ! return-code 

CALL E_FMWRITX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX0_ll

SUBROUTINE FMWRITX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX1_ll=>FMWRITX1_ll
IMPLICIT NONE 
CHARACTER(LEN=*),        INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),        INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),        INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                 INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                 INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),        INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                 INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX1_ll

SUBROUTINE FMWRITX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX2_ll=>FMWRITX2_ll
IMPLICIT NONE 
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                   INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX2_ll

SUBROUTINE FMWRITX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX3_ll=>FMWRITX3_ll
IMPLICIT NONE 
CHARACTER(LEN=*),            INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),            INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),            INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),            INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                     INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                     INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),            INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                     INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX3_ll

SUBROUTINE FMWRITX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX4_ll=>FMWRITX4_ll
IMPLICIT NONE 
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                       INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                       INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),              INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                       INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX4_ll

SUBROUTINE FMWRITX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX5_ll=>FMWRITX5_ll
IMPLICIT NONE 
CHARACTER(LEN=*),                INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),                INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),                INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                         INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                         INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),                INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                         INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX5_ll

SUBROUTINE FMWRITX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITX6_ll=>FMWRITX6_ll
IMPLICIT NONE 
CHARACTER(LEN=*),                INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),                INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),                INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                         INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                         INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),                INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                         INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITX6_ll

SUBROUTINE FMWRITN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITN0_ll=>FMWRITN0_ll
IMPLICIT NONE 
CHARACTER(LEN=*),   INTENT(IN) ::HFILEM  ! FM-file name
CHARACTER(LEN=*),   INTENT(IN) ::HRECFM  ! name of the article to read
CHARACTER(LEN=*),   INTENT(IN) ::HFIPRI  ! output file for error messages
CHARACTER(LEN=*),   INTENT(IN) ::HDIR    ! field form
INTEGER,            INTENT(IN) ::KFIELD  ! array containing the data field
INTEGER,            INTENT(IN) ::KGRID   ! C-grid indicator (u,v,w,T)
INTEGER,            INTENT(IN) ::KLENCH  ! length of comment string
CHARACTER(LEN=*),   INTENT(IN) ::HCOMMENT! comment string
INTEGER,            INTENT(OUT)::KRESP   ! return-code

CALL E_FMWRITN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITN0_ll

SUBROUTINE FMWRITN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITN1_ll=>FMWRITN1_ll
IMPLICIT NONE 
CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! field form
INTEGER,DIMENSION(:),TARGET,INTENT(IN) ::KFIELD   ! array containing the data field
INTEGER,                    INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                    INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),           INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                    INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITN1_ll

SUBROUTINE FMWRITN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITN2_ll=>FMWRITN2_ll
IMPLICIT NONE 
CHARACTER(LEN=*),             INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),             INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),             INTENT(IN) ::HDIR     ! field form
INTEGER,DIMENSION(:,:),TARGET,INTENT(IN) ::KFIELD ! array containing the data field
INTEGER,                      INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                      INTENT(IN) ::KLENCH   ! length of comment string
CHARACTER(LEN=*),             INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                      INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITN2_ll

SUBROUTINE FMWRITL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITL0_ll=>FMWRITL0_ll
IMPLICIT NONE 
CHARACTER(LEN=*), INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*), INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*), INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*), INTENT(IN) ::HDIR   ! field form
LOGICAL,          INTENT(IN) ::OFIELD ! array containing the data field
INTEGER,          INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,          INTENT(IN)::KLENCH ! length of comment string
CHARACTER(LEN=*), INTENT(IN)::HCOMMENT ! comment string
INTEGER,          INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITL0_ll
  
SUBROUTINE FMWRITL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITL1_ll=>FMWRITL1_ll
IMPLICIT NONE
CHARACTER(LEN=*),    INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),    INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),    INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),    INTENT(IN) ::HDIR   ! field form
LOGICAL,DIMENSION(:),INTENT(IN) ::OFIELD ! array containing the data field
INTEGER,             INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,             INTENT(IN)::KLENCH ! length of comment string
CHARACTER(LEN=*),    INTENT(IN)::HCOMMENT ! comment string
INTEGER,             INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITL1_ll
  
SUBROUTINE FMWRITC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITC0_ll=>FMWRITC0_ll
IMPLICIT NONE
CHARACTER(LEN=*),  INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),  INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),  INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),  INTENT(IN) ::HDIR   ! field form
CHARACTER(LEN=*),  INTENT(IN) ::HFIELD ! array containing the data field
INTEGER,           INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,           INTENT(IN)::KLENCH ! length of comment string
CHARACTER(LEN=*),  INTENT(IN)::HCOMMENT ! comment string
INTEGER,           INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITC0_ll

SUBROUTINE FMWRITC1_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITC1_ll=>FMWRITC1_ll
IMPLICIT NONE
CHARACTER(LEN=*),  INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),  INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),  INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),  INTENT(IN) ::HDIR   ! field form
CHARACTER(LEN=*),DIMENSION(:),  INTENT(IN) ::HFIELD ! array containing the data field
INTEGER,           INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,           INTENT(IN)::KLENCH ! length of comment string
CHARACTER(LEN=*),  INTENT(IN)::HCOMMENT ! comment string
INTEGER,           INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITC1_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITC1_ll
  
SUBROUTINE FMWRITT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITT0_ll=>FMWRITT0_ll
USE MODD_TYPE_DATE
IMPLICIT NONE 
CHARACTER(LEN=*),    INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),    INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),    INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),    INTENT(IN) ::HDIR   ! field form
TYPE (DATE_TIME),    INTENT(IN) ::TFIELD ! array containing the data field
INTEGER,             INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,             INTENT(IN) ::KLENCH ! length of comment string
CHARACTER(LEN=*),    INTENT(IN) ::HCOMMENT ! comment string
INTEGER,             INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRITT0_ll

SUBROUTINE FMWRIT_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,&
     & KGRID,KLENCH,HCOMMENT,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRIT_LB=>FMWRIT_LB
IMPLICIT NONE 
CHARACTER(LEN=*),       INTENT(IN) ::HFILEM ! file name
CHARACTER(LEN=*),       INTENT(IN) ::HRECFM ! name of the article to be written
CHARACTER(LEN=*),       INTENT(IN) ::HFIPRI ! file for prints in FM
CHARACTER(LEN=*),       INTENT(IN) ::HLBTYPE! 'LBX','LBXU','LBY' or 'LBYV'
REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) ::PLB ! array containing the LB field
INTEGER,                INTENT(IN) ::KRIM  ! size of the LB area
INTEGER,                INTENT(IN) ::KL3D  ! size of the LB array in FM
INTEGER,                INTENT(IN) ::KGRID ! C-grid indicator (u,v,w,T)
INTEGER,                INTENT(IN) ::KLENCH ! length of comment string
CHARACTER(LEN=*),       INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                INTENT(OUT)::KRESP  ! return-code

CALL E_FMWRIT_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,KGRID,KLENCH,HCOMMENT,KRESP)

END SUBROUTINE FMWRIT_LB

SUBROUTINE FMWRITBOXX2_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITBOXX2_ll=>FMWRITBOXX2_ll
IMPLICIT NONE
CHARACTER(LEN=*),            INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),            INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),            INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),            INTENT(IN) ::HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
REAL,DIMENSION(:,:),TARGET,  INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                     INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
CHARACTER(LEN=*),            INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                     INTENT(IN) ::KXOBOX   ! 
INTEGER,                     INTENT(IN) ::KXEBOX   ! Global coordinates of the box
INTEGER,                     INTENT(IN) ::KYOBOX   ! 
INTEGER,                     INTENT(IN) ::KYEBOX   ! 
INTEGER,                     INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITBOXX2_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)

END SUBROUTINE FMWRITBOXX2_ll

SUBROUTINE FMWRITBOXX3_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITBOXX3_ll=>FMWRITBOXX3_ll
IMPLICIT NONE
CHARACTER(LEN=*),            INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),            INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),            INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),            INTENT(IN) ::HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                     INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
CHARACTER(LEN=*),            INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                     INTENT(IN) ::KXOBOX   ! 
INTEGER,                     INTENT(IN) ::KXEBOX   ! Global coordinates of the box
INTEGER,                     INTENT(IN) ::KYOBOX   ! 
INTEGER,                     INTENT(IN) ::KYEBOX   ! 
INTEGER,                     INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITBOXX3_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)

END SUBROUTINE FMWRITBOXX3_ll

SUBROUTINE FMWRITBOXX4_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITBOXX4_ll=>FMWRITBOXX4_ll
IMPLICIT NONE
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                       INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
CHARACTER(LEN=*),              INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                       INTENT(IN) ::KXOBOX   ! 
INTEGER,                       INTENT(IN) ::KXEBOX   ! Global coordinates of the box
INTEGER,                       INTENT(IN) ::KYOBOX   ! 
INTEGER,                       INTENT(IN) ::KYEBOX   ! 
INTEGER,                       INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITBOXX4_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)

END SUBROUTINE FMWRITBOXX4_ll

SUBROUTINE FMWRITBOXX5_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITBOXX5_ll=>FMWRITBOXX5_ll
IMPLICIT NONE 
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                       INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
CHARACTER(LEN=*),              INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                       INTENT(IN) ::KXOBOX   ! 
INTEGER,                       INTENT(IN) ::KXEBOX   ! Global coordinates of the box
INTEGER,                       INTENT(IN) ::KYOBOX   ! 
INTEGER,                       INTENT(IN) ::KYEBOX   ! 
INTEGER,                       INTENT(OUT)::KRESP    ! return-code

CALL E_FMWRITBOXX5_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)

END SUBROUTINE FMWRITBOXX5_ll

SUBROUTINE FMWRITBOXX6_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
USE MODE_FMWRIT, ONLY : E_FMWRITBOXX6_ll=>FMWRITBOXX6_ll
IMPLICIT NONE 
CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to write
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
INTEGER,                       INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
CHARACTER(LEN=*),              INTENT(IN) ::HCOMMENT ! comment string
INTEGER,                       INTENT(IN) ::KXOBOX   ! 
INTEGER,                       INTENT(IN) ::KXEBOX   ! Global coordinates of the box
INTEGER,                       INTENT(IN) ::KYOBOX   ! 
INTEGER,                       INTENT(IN) ::KYEBOX   ! 
INTEGER,                       INTENT(OUT)::KRESP    ! return-code 

CALL E_FMWRITBOXX6_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
     HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)

END SUBROUTINE FMWRITBOXX6_ll
!
! Routines found in the MODE_GATHER_ll module : GATHERALL_xx
!
SUBROUTINE GATHERALL_X1(HDIR,PSEND,PRECV,KRESP)
USE MODE_GATHER_ll, ONLY : E_GATHERALL_X1=>GATHERALL_X1
IMPLICIT NONE 
CHARACTER(LEN=*),  INTENT(IN) :: HDIR
REAL,DIMENSION(:), INTENT(IN) :: PSEND
REAL,DIMENSION(:), INTENT(INOUT):: PRECV
INTEGER,           INTENT(INOUT):: KRESP

CALL E_GATHERALL_X1(HDIR,PSEND,PRECV,KRESP)

END SUBROUTINE GATHERALL_X1
  
SUBROUTINE GATHERALL_X2(HDIR,PSEND,PRECV,KRESP)
USE MODE_GATHER_ll, ONLY : E_GATHERALL_X2=>GATHERALL_X2
IMPLICIT NONE 
CHARACTER(LEN=*),    INTENT(IN) :: HDIR
REAL,DIMENSION(:,:), INTENT(IN) :: PSEND
REAL,DIMENSION(:,:), INTENT(INOUT):: PRECV
INTEGER,             INTENT(INOUT):: KRESP

CALL E_GATHERALL_X2(HDIR,PSEND,PRECV,KRESP)

END SUBROUTINE GATHERALL_X2
  
SUBROUTINE GATHERALL_X3(HDIR,PSEND,PRECV,KRESP)
USE MODE_GATHER_ll, ONLY : E_GATHERALL_X3=>GATHERALL_X3
IMPLICIT NONE
CHARACTER(LEN=*),      INTENT(IN) :: HDIR
REAL,DIMENSION(:,:,:), INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:), INTENT(INOUT):: PRECV
INTEGER,               INTENT(INOUT):: KRESP

CALL E_GATHERALL_X3(HDIR,PSEND,PRECV,KRESP)

END SUBROUTINE GATHERALL_X3
  
SUBROUTINE GATHERALL_N1(HDIR,KSEND,KRECV,KRESP)
USE MODE_GATHER_ll, ONLY : E_GATHERALL_N1=>GATHERALL_N1
IMPLICIT NONE
CHARACTER(LEN=*),     INTENT(IN) :: HDIR
INTEGER,DIMENSION(:), INTENT(IN) :: KSEND
INTEGER,DIMENSION(:), INTENT(INOUT):: KRECV
INTEGER,              INTENT(INOUT):: KRESP

CALL E_GATHERALL_N1(HDIR,KSEND,KRECV,KRESP)

END SUBROUTINE GATHERALL_N1

SUBROUTINE GATHERALL_N2(HDIR,KSEND,KRECV,KRESP)
USE MODE_GATHER_ll, ONLY : E_GATHERALL_N2=>GATHERALL_N2
IMPLICIT NONE
CHARACTER(LEN=*),       INTENT(IN) :: HDIR
INTEGER,DIMENSION(:,:), INTENT(IN) :: KSEND
INTEGER,DIMENSION(:,:), INTENT(INOUT):: KRECV
INTEGER,                INTENT(INOUT):: KRESP

CALL E_GATHERALL_N2(HDIR,KSEND,KRECV,KRESP)

END SUBROUTINE GATHERALL_N2
