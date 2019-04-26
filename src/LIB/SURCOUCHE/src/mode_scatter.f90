!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  J. Escobar  10/02/2012: bug in MPI_RECV: replace MPI_STATUSES_IGNORE with MPI_STATUS_IGNORE
!  P. Wautelet 26/04/2019: use modd_precision parameters for datatypes of MPI communications
!-----------------------------------------------------------------

MODULE MODE_SCATTER_ll

USE MODD_MPIF
use modd_precision, only: MNHINT_MPI, MNHREAL_MPI

IMPLICIT NONE 


PRIVATE

INTERFACE SCATTER_XXFIELD
  MODULE PROCEDURE SCATTERXX_X1,SCATTERXX_X2,SCATTERXX_X3&
       & ,SCATTERXX_X4,SCATTERXX_X5,SCATTERXX_X6,&
       & SCATTERXX_N1,SCATTERXX_N2 
END INTERFACE

INTERFACE SCATTER_XYFIELD  
  MODULE PROCEDURE SCATTERXY_X2,SCATTERXY_X3,SCATTERXY_X4,&
       & SCATTERXY_X5,SCATTERXY_X6,SCATTERXY_N2
END INTERFACE

PUBLIC SCATTER_XXFIELD,SCATTER_XYFIELD,GET_DOMREAD_ll

CONTAINS 

SUBROUTINE SCATTERXX_X1(HDIR,PSEND,PRECV,KROOT,KCOMM, TPSPLITTING)
USE MODD_IO,            ONLY: ISP, ISNPROC
USE MODD_PARAMETERS_ll, ONLY: JPHEXT
USE MODD_STRUCTURE_ll,  ONLY: ZONE_ll
USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE

CHARACTER(LEN=*),          INTENT(IN) :: HDIR
REAL,DIMENSION(:), TARGET, INTENT(IN) :: PSEND
REAL,DIMENSION(:),         INTENT(INOUT):: PRECV
INTEGER,                   INTENT(IN) :: KROOT
INTEGER,                   INTENT(IN) :: KCOMM
TYPE(ZONE_ll), DIMENSION(ISNPROC), OPTIONAL :: TPSPLITTING  ! splitting of the domain

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:), POINTER :: TX1DP

INTEGER,ALLOCATABLE,DIMENSION(:)    :: REQ_TAB
INTEGER                           :: NB_REQ
TYPE TX_1DP
   REAL,DIMENSION(:), POINTER    :: X
END TYPE TX_1DP
TYPE(TX_1DP),ALLOCATABLE,DIMENSION(:) :: T_TX1DP

IF (ISP == KROOT) THEN
   NB_REQ=0
   ALLOCATE(REQ_TAB(ISNPROC-1))
   ALLOCATE(T_TX1DP(ISNPROC-1))  
   DO JI = 1,ISNPROC
      IF ( PRESENT(TPSPLITTING) ) THEN
        IXO = TPSPLITTING(JI)%NXOR - JPHEXT
        IXE = TPSPLITTING(JI)%NXEND + JPHEXT
        IYO = TPSPLITTING(JI)%NYOR - JPHEXT
        IYE = TPSPLITTING(JI)%NYEND + JPHEXT
      ELSE
        CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
      ENDIF
      IF (HDIR == 'XX') THEN
         TX1DP=>PSEND(IXO:IXE)
      ELSE ! HDIR ='YY'
         TX1DP=>PSEND(IYO:IYE)
      END IF
      
      IF (ISP /= JI) THEN
         NB_REQ = NB_REQ + 1
         ALLOCATE(T_TX1DP(NB_REQ)%X(SIZE(TX1DP)))
         T_TX1DP(NB_REQ)%X=TX1DP
         CALL MPI_ISEND(T_TX1DP(NB_REQ)%X,SIZE(TX1DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
              & ,REQ_TAB(NB_REQ),IERR) 
         !CALL MPI_BSEND(T12DP,SIZE(T12DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
         !     & ,IERR)
      ELSE 
         PRECV(:) = TX1DP(:)
      END IF
   END DO
   IF (NB_REQ .GT.0 ) THEN
      CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
      DO JI=1,NB_REQ ;  DEALLOCATE(T_TX1DP(JI)%X) ; ENDDO
   END IF
   DEALLOCATE(T_TX1DP)
   DEALLOCATE(REQ_TAB)
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF
  
END SUBROUTINE SCATTERXX_X1

SUBROUTINE SCATTERXX_X2(HDIR,PSEND,PRECV,KROOT,KCOMM, TPSPLITTING)
USE MODD_IO,            ONLY: ISP, ISNPROC
USE MODD_PARAMETERS_ll, ONLY: JPHEXT
USE MODD_STRUCTURE_ll,  ONLY: ZONE_ll

CHARACTER(LEN=*),           INTENT(IN) :: HDIR
REAL,DIMENSION(:,:), TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:),        INTENT(INOUT):: PRECV
INTEGER,                    INTENT(IN) :: KROOT
INTEGER,                    INTENT(IN) :: KCOMM
TYPE(ZONE_ll), DIMENSION(ISNPROC), OPTIONAL :: TPSPLITTING  ! splitting of the domain

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:), POINTER :: TX2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    IF ( PRESENT(TPSPLITTING) ) THEN
      IXO = TPSPLITTING(JI)%NXOR - JPHEXT
      IXE = TPSPLITTING(JI)%NXEND + JPHEXT
      IYO = TPSPLITTING(JI)%NYOR - JPHEXT
      IYE = TPSPLITTING(JI)%NYEND + JPHEXT
    ELSE
      CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    ENDIF
    IF (HDIR == 'XX') THEN
      TX2DP=>PSEND(IXO:IXE,:)
    ELSE ! HDIR ='YY'
      TX2DP=>PSEND(IYO:IYE,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:) = TX2DP(:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE SCATTERXX_X2

SUBROUTINE SCATTERXX_X3(HDIR,PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),              INTENT(IN) :: HDIR
REAL,DIMENSION(:,:,:), TARGET, INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:),         INTENT(INOUT):: PRECV
INTEGER,                       INTENT(IN) :: KROOT
INTEGER,                       INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:), POINTER :: TX2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TX2DP=>PSEND(IXO:IXE,:,:)
    ELSE ! HDIR ='YY'
      TX2DP=>PSEND(IYO:IYE,:,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:) = TX2DP(:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF
  
END SUBROUTINE SCATTERXX_X3

SUBROUTINE SCATTERXX_X4(HDIR,PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),              INTENT(IN) :: HDIR
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                       INTENT(IN) :: KROOT
INTEGER,                       INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:),    POINTER :: TX2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TX2DP=>PSEND(IXO:IXE,:,:,:)
    ELSE ! HDIR ='YY'
      TX2DP=>PSEND(IYO:IYE,:,:,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:) = TX2DP(:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE SCATTERXX_X4

SUBROUTINE SCATTERXX_X5(HDIR,PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),                INTENT(IN) :: HDIR
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                         INTENT(IN) :: KROOT
INTEGER,                         INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:,:), POINTER :: TX2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TX2DP=>PSEND(IXO:IXE,:,:,:,:)
    ELSE ! HDIR ='YY'
      TX2DP=>PSEND(IYO:IYE,:,:,:,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:,:) = TX2DP(:,:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF
  
END SUBROUTINE SCATTERXX_X5

SUBROUTINE SCATTERXX_X6(HDIR,PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),                  INTENT(IN) :: HDIR
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                           INTENT(IN) :: KROOT
INTEGER,                           INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:,:,:), POINTER :: TX2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TX2DP=>PSEND(IXO:IXE,:,:,:,:,:)
    ELSE ! HDIR ='YY'
      TX2DP=>PSEND(IYO:IYE,:,:,:,:,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:,:,:) = TX2DP(:,:,:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE SCATTERXX_X6

SUBROUTINE SCATTERXX_N1(HDIR,KSEND,KRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),           INTENT(IN) :: HDIR
INTEGER,DIMENSION(:),TARGET,INTENT(IN) :: KSEND
INTEGER,DIMENSION(:),       INTENT(INOUT):: KRECV
INTEGER,                    INTENT(IN) :: KROOT
INTEGER,                    INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
INTEGER, DIMENSION(:), POINTER :: TI2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TI2DP=>KSEND(IXO:IXE)
    ELSE ! HDIR ='YY'
      TI2DP=>KSEND(IYO:IYE)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TI2DP,SIZE(TI2DP),MNHINT_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      KRECV(:) = TI2DP(:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(KRECV,SIZE(KRECV),MNHINT_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF
  
END SUBROUTINE SCATTERXX_N1

SUBROUTINE SCATTERXX_N2(HDIR,KSEND,KRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

CHARACTER(LEN=*),              INTENT(IN) :: HDIR
INTEGER, DIMENSION(:,:),TARGET,INTENT(IN) :: KSEND
INTEGER, DIMENSION(:,:),       INTENT(INOUT):: KRECV
INTEGER,                       INTENT(IN) :: KROOT
INTEGER,                       INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
INTEGER, DIMENSION(:,:), POINTER :: TI2DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    IF (HDIR == 'XX') THEN
      TI2DP=>KSEND(IXO:IXE,:)
    ELSE ! HDIR ='YY'
      TI2DP=>KSEND(IYO:IYE,:)
    END IF
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TI2DP,SIZE(TI2DP),MNHINT_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      KRECV(:,:) = TI2DP(:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(KRECV,SIZE(KRECV),MNHINT_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE SCATTERXX_N2

SUBROUTINE SCATTERXY_X2(PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO,     ONLY: ISP, ISNPROC
USE MODD_VAR_ll, ONLY: MNH_STATUSES_IGNORE

REAL,DIMENSION(:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:),       INTENT(INOUT):: PRECV
INTEGER,                   INTENT(IN) :: KROOT
INTEGER,                   INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:), POINTER :: TX2DP

INTEGER,ALLOCATABLE,DIMENSION(:)    :: REQ_TAB
INTEGER                           :: NB_REQ
TYPE TX_2DP
   REAL,DIMENSION(:,:), POINTER    :: X
END TYPE TX_2DP
TYPE(TX_2DP),ALLOCATABLE,DIMENSION(:) :: T_TX2DP
  
IF (ISP == KROOT) THEN
   NB_REQ=0
   ALLOCATE(REQ_TAB(ISNPROC-1))
   ALLOCATE(T_TX2DP(ISNPROC-1))  
   DO JI = 1,ISNPROC
      CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
      TX2DP=>PSEND(IXO:IXE,IYO:IYE)    
      IF (ISP /= JI) THEN 
         NB_REQ = NB_REQ + 1
         ALLOCATE(T_TX2DP(NB_REQ)%X(IXO:IXE,IYO:IYE))
         T_TX2DP(NB_REQ)%X=TX2DP
         CALL MPI_ISEND(T_TX2DP(NB_REQ)%X,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
              & ,REQ_TAB(NB_REQ),IERR)
         !CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
         !     & ,IERR)
      ELSE 
         PRECV(:,:) = TX2DP(:,:)
      END IF
   END DO
   IF (NB_REQ .GT.0 ) THEN
      CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
      DO JI=1,NB_REQ ;  DEALLOCATE(T_TX2DP(JI)%X) ; ENDDO
   END IF
   DEALLOCATE(T_TX2DP)
   DEALLOCATE(REQ_TAB)
ELSE
   CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF
  
END SUBROUTINE  SCATTERXY_X2

SUBROUTINE SCATTERXY_X3(PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                     INTENT(IN) :: KROOT
INTEGER,                     INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:), POINTER :: TX3DP
  
IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    TX3DP=>PSEND(IXO:IXE,IYO:IYE,:)
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX3DP,SIZE(TX3DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:) = TX3DP(:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE  SCATTERXY_X3

SUBROUTINE SCATTERXY_X4(PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

REAL,DIMENSION(:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                       INTENT(IN) :: KROOT
INTEGER,                       INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:), POINTER :: TX3DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    TX3DP=>PSEND(IXO:IXE,IYO:IYE,:,:)
        
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX3DP,SIZE(TX3DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:) = TX3DP(:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE  SCATTERXY_X4

SUBROUTINE SCATTERXY_X5(PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                         INTENT(IN) :: KROOT
INTEGER,                         INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:,:), POINTER :: TX3DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    TX3DP=>PSEND(IXO:IXE,IYO:IYE,:,:,:)
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX3DP,SIZE(TX3DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:,:) = TX3DP(:,:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE  SCATTERXY_X5

SUBROUTINE SCATTERXY_X6(PSEND,PRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) :: PSEND
REAL,DIMENSION(:,:,:,:,:,:),       INTENT(INOUT):: PRECV
INTEGER,                           INTENT(IN) :: KROOT
INTEGER,                           INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:,:,:,:,:), POINTER :: TX3DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    TX3DP=>PSEND(IXO:IXE,IYO:IYE,:,:,:,:)
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TX3DP,SIZE(TX3DP),MNHREAL_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      PRECV(:,:,:,:,:,:) = TX3DP(:,:,:,:,:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(PRECV,SIZE(PRECV),MNHREAL_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE  SCATTERXY_X6

SUBROUTINE SCATTERXY_N2(KSEND,KRECV,KROOT,KCOMM)
USE MODD_IO, ONLY: ISP, ISNPROC

INTEGER,DIMENSION(:,:),TARGET,INTENT(IN) :: KSEND
INTEGER,DIMENSION(:,:),       INTENT(INOUT):: KRECV
INTEGER,                      INTENT(IN) :: KROOT
INTEGER,                      INTENT(IN) :: KCOMM

INTEGER :: IERR
INTEGER :: JI
INTEGER :: IXO,IXE,IYO,IYE
INTEGER ,DIMENSION(:,:), POINTER :: TI3DP

IF (ISP == KROOT) THEN
  DO JI = 1,ISNPROC
    CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
    TI3DP=>KSEND(IXO:IXE,IYO:IYE)
    
    IF (ISP /= JI) THEN 
      CALL MPI_BSEND(TI3DP,SIZE(TI3DP),MNHINT_MPI,JI-1,199+KROOT,KCOMM&
           & ,IERR)
    ELSE 
      KRECV(:,:) = TI3DP(:,:)
    END IF
  END DO
ELSE
  CALL MPI_RECV(KRECV,SIZE(KRECV),MNHINT_MPI,KROOT-1,199+KROOT,KCOMM&
       & ,MPI_STATUS_IGNORE,IERR)
END IF

END SUBROUTINE  SCATTERXY_N2

SUBROUTINE GET_DOMREAD_ll(KIP,KXOR,KXEND,KYOR,KYEND)
USE MODD_STRUCTURE_ll, ONLY: MODELSPLITTING_ll
USE MODD_VAR_ll,       ONLY: TCRRT_PROCONF

IMPLICIT NONE 

INTEGER, INTENT(IN)  :: KIP

INTEGER, INTENT(OUT) :: KXOR
INTEGER, INTENT(OUT) :: KXEND
INTEGER, INTENT(OUT) :: KYOR
INTEGER, INTENT(OUT) :: KYEND

TYPE(MODELSPLITTING_ll), POINTER :: TB

TB => TCRRT_PROCONF%TSPLITS_B(KIP)

KXOR  = TB%NXORE
KXEND = TB%NXENDE
KYOR  = TB%NYORE
KYEND = TB%NYENDE
  
END SUBROUTINE GET_DOMREAD_ll

END MODULE MODE_SCATTER_ll

