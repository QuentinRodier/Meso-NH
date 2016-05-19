!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODI_FMWRIT_ll
!
INTERFACE FMWRIT
  SUBROUTINE FMWRITX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),        INTENT(IN) ::HFILEM  ! FM-file name
  CHARACTER(LEN=*),        INTENT(IN) ::HRECFM  ! name of the article to write
  CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI  ! output file for error messages
  CHARACTER(LEN=*),        INTENT(IN) ::HDIR    ! field form
  REAL,                    INTENT(IN) ::PFIELD  ! array containing the data field
  INTEGER,                 INTENT(IN) ::KGRID   ! C-grid indicator (u,v,w,T)
  INTEGER,                 INTENT(IN) ::KLENCH  ! length of comment string
  CHARACTER(LEN=*),        INTENT(IN) ::HCOMMENT! comment string
  INTEGER,                 INTENT(OUT)::KRESP   ! return-code 
  END SUBROUTINE FMWRITX0_ll

  SUBROUTINE FMWRITX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),        INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),        INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),        INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
  INTEGER,                 INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                 INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),        INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                 INTENT(OUT)::KRESP    ! return-code 
  END SUBROUTINE FMWRITX1_ll
  
  SUBROUTINE FMWRITX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
  INTEGER,                   INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                   INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),          INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                   INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITX2_ll
  
  SUBROUTINE FMWRITX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),            INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),            INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),            INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),            INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
  INTEGER,                     INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                     INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),            INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                     INTENT(OUT)::KRESP    ! return-code 
  END SUBROUTINE FMWRITX3_ll
  
  SUBROUTINE FMWRITX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),              INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:,:,:),TARGET,INTENT(IN) ::PFIELD   ! array containing the data field
  INTEGER,                       INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                       INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),              INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                       INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITX4_ll
  
  SUBROUTINE FMWRITX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),                INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),                INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),                INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD ! array containing the data field
  INTEGER,                         INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                         INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),                INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                         INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITX5_ll
  
  SUBROUTINE FMWRITX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),                INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),                INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),                INTENT(IN) ::HDIR     ! field form
  REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) ::PFIELD ! array containing the data field
  INTEGER,                         INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                         INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),                INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                         INTENT(OUT)::KRESP    ! return-code 
  END SUBROUTINE FMWRITX6_ll
  
  SUBROUTINE FMWRITN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),   INTENT(IN) ::HFILEM  ! FM-file name
  CHARACTER(LEN=*),   INTENT(IN) ::HRECFM  ! name of the article to read
  CHARACTER(LEN=*),   INTENT(IN) ::HFIPRI  ! output file for error messages
  CHARACTER(LEN=*),   INTENT(IN) ::HDIR    ! field form
  INTEGER,            INTENT(IN) ::KFIELD  ! array containing the data field
  INTEGER,            INTENT(IN) ::KGRID   ! C-grid indicator (u,v,w,T)
  INTEGER,            INTENT(IN) ::KLENCH  ! length of comment string
  CHARACTER(LEN=*),   INTENT(IN) ::HCOMMENT! comment string
  INTEGER,            INTENT(OUT)::KRESP   ! return-code
  END SUBROUTINE FMWRITN0_ll
  
  SUBROUTINE FMWRITN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! field form
  INTEGER,DIMENSION(:),TARGET,INTENT(IN) ::KFIELD   ! array containing the data field
  INTEGER,                    INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                    INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),           INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                    INTENT(OUT)::KRESP    ! return-code 
  END SUBROUTINE FMWRITN1_ll
  
  SUBROUTINE FMWRITN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),             INTENT(IN) ::HFILEM   ! FM-file name
  CHARACTER(LEN=*),             INTENT(IN) ::HRECFM   ! name of the article to write
  CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI   ! output file for error messages
  CHARACTER(LEN=*),             INTENT(IN) ::HDIR     ! field form
  INTEGER,DIMENSION(:,:),TARGET,INTENT(IN) ::KFIELD ! array containing the data field
  INTEGER,                      INTENT(IN) ::KGRID    ! C-grid indicator (u,v,w,T)
  INTEGER,                      INTENT(IN) ::KLENCH   ! length of comment string
  CHARACTER(LEN=*),             INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,                      INTENT(OUT)::KRESP    ! return-code 
  END SUBROUTINE FMWRITN2_ll
  
  SUBROUTINE FMWRITL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*), INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*), INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*), INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*), INTENT(IN) ::HDIR   ! field form
  LOGICAL,          INTENT(IN) ::OFIELD ! array containing the data field
  INTEGER,          INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,          INTENT(IN)::KLENCH ! length of comment string
  CHARACTER(LEN=*), INTENT(IN)::HCOMMENT ! comment string
  INTEGER,          INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITL0_ll
  
  SUBROUTINE FMWRITL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),    INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),    INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),    INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),    INTENT(IN) ::HDIR   ! field form
  LOGICAL,DIMENSION(:),INTENT(IN) ::OFIELD ! array containing the data field
  INTEGER,             INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,             INTENT(IN)::KLENCH ! length of comment string
  CHARACTER(LEN=*),    INTENT(IN)::HCOMMENT ! comment string
  INTEGER,             INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITL1_ll
  
  SUBROUTINE FMWRITC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
  CHARACTER(LEN=*),  INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),  INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),  INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),  INTENT(IN) ::HDIR   ! field form
  CHARACTER(LEN=*),  INTENT(IN) ::HFIELD ! array containing the data field
  INTEGER,           INTENT(IN)::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,           INTENT(IN)::KLENCH ! length of comment string
  CHARACTER(LEN=*),  INTENT(IN)::HCOMMENT ! comment string
  INTEGER,           INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITC0_ll
  
  SUBROUTINE FMWRITT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
       KLENCH,HCOMMENT,KRESP)
  USE MODD_TYPE_DATE
  CHARACTER(LEN=*),    INTENT(IN) ::HFILEM ! FM-file name
  CHARACTER(LEN=*),    INTENT(IN) ::HRECFM ! name of the article to read
  CHARACTER(LEN=*),    INTENT(IN) ::HFIPRI ! output file for error messages
  CHARACTER(LEN=*),    INTENT(IN) ::HDIR   ! field form
  TYPE (DATE_TIME),    INTENT(IN) ::TFIELD ! array containing the data field
  INTEGER,             INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
  INTEGER,             INTENT(IN) ::KLENCH ! length of comment string
  CHARACTER(LEN=*),    INTENT(IN) ::HCOMMENT ! comment string
  INTEGER,             INTENT(OUT)::KRESP    ! return-code
  END SUBROUTINE FMWRITT0_ll
END INTERFACE

INTERFACE 
  SUBROUTINE FMWRIT_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,&
       & KGRID,KLENCH,HCOMMENT,KRESP)
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
  END SUBROUTINE FMWRIT_LB
END INTERFACE

INTERFACE FMWRITBOX
  SUBROUTINE FMWRITBOXX2_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
       HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
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
  END SUBROUTINE FMWRITBOXX2_ll
  
  SUBROUTINE FMWRITBOXX3_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
         HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
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
  END SUBROUTINE FMWRITBOXX3_ll
  
  SUBROUTINE FMWRITBOXX4_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
         HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
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
  END SUBROUTINE FMWRITBOXX4_ll
  
  SUBROUTINE FMWRITBOXX5_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
         HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
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
  END SUBROUTINE FMWRITBOXX5_ll
  
  SUBROUTINE FMWRITBOXX6_ll(HFILEM,HRECFM,HFIPRI,HBUDGET,PFIELD,KGRID,&
         HCOMMENT,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
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
  END SUBROUTINE FMWRITBOXX6_ll
END INTERFACE
!
END MODULE MODI_FMWRIT_ll
