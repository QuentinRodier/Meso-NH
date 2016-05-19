!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODI_FMREAD_ll
!
INTERFACE FMREAD
  SUBROUTINE FMREADX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
  REAL,                         INTENT(OUT)::PFIELD ! array containing the data field 
  INTEGER,                      INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                      INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),             INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                      INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADX0_ll

  SUBROUTINE FMREADX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),        INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),        INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),        INTENT(IN) ::HDIR     ! Field form
  REAL,DIMENSION(:),TARGET,INTENT(OUT)::PFIELD   ! array containing the data field 
  INTEGER,                 INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                 INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),        INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                 INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADX1_ll
  
  SUBROUTINE FMREADX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:),TARGET, INTENT(OUT)::PFIELD   ! array containing the data field
  INTEGER,                    INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                    INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),           INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP     ! return-code
  END SUBROUTINE FMREADX2_ll
  
  SUBROUTINE FMREADX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
  REAL, DIMENSION(:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
  INTEGER,                      INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                      INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),             INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                      INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADX3_ll
  
  SUBROUTINE FMREADX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),              INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:,:,:),TARGET,INTENT(OUT)::PFIELD   ! array containing the data field
  INTEGER,                       INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                       INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),              INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                       INTENT(OUT)::KRESP  ! return-code if
  END SUBROUTINE FMREADX4_ll
  
  SUBROUTINE FMREADX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),                INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),                INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),                INTENT(IN) ::HDIR   ! field form
  REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
  INTEGER,                         INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                         INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),                INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                         INTENT(OUT)::KRESP  ! return-code
  END SUBROUTINE FMREADX5_ll
  
  SUBROUTINE FMREADX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),                  INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),                  INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),                  INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),                  INTENT(IN) ::HDIR   ! field form
  REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(OUT)::PFIELD ! array containing the data field
  INTEGER,                           INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                           INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),                  INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                           INTENT(OUT)::KRESP  ! return-code
  END SUBROUTINE FMREADX6_ll
  
  SUBROUTINE FMREADN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
  INTEGER,                   INTENT(OUT)::KFIELD ! array containing the data field     
  INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADN0_ll
  
  SUBROUTINE FMREADN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! Field form
  INTEGER,DIMENSION(:),TARGET,INTENT(OUT)::KFIELD ! array containing the data field     
  INTEGER,                    INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                    INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),           INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                    INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADN1_ll
  
  SUBROUTINE FMREADN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),              INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),              INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),              INTENT(IN) ::HDIR   ! field form
  INTEGER, DIMENSION(:,:),TARGET,INTENT(OUT)::KFIELD ! array containing the data field
  INTEGER,                       INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                       INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),              INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                       INTENT(OUT)::KRESP  ! return-code
  END SUBROUTINE FMREADN2_ll

  SUBROUTINE FMREADL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR   ! field form
  LOGICAL,                   INTENT(OUT)::OFIELD ! array containing the data field
  INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string
  CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADL0_ll
  
  SUBROUTINE FMREADL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM  ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM  ! name of the article to read
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI  ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR    ! Field form
  LOGICAL, DIMENSION(:),     INTENT(OUT)::OFIELD  ! array containing the data field
  INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADL1_ll

  SUBROUTINE FMREADC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
  CHARACTER(LEN=*),          INTENT(OUT)::HFIELD   ! array containing the data field    
  INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADC0_ll
  
  SUBROUTINE FMREADT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  USE MODD_TYPE_DATE
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
  TYPE (DATE_TIME),          INTENT(OUT)::TFIELD ! array containing the data field
  INTEGER,                   INTENT(OUT)::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(OUT)::KLENCH   ! length of comment string
  CHARACTER(LEN=*),          INTENT(OUT)::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMREADT0_ll

END INTERFACE

INTERFACE 
  SUBROUTINE FMREAD_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,&
       & KGRID,KLENCH,HCOMMENT,KRESP)
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
  END SUBROUTINE FMREAD_LB
END INTERFACE
!
END MODULE MODI_FMREAD_ll
