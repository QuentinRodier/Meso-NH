!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_SURF_FA
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURF_FA is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!      J.Escobar      10/06/2013: replace DOUBLE PRECISION by REAL to handle problem for promotion of real on IBM SP
!----------------------------------------------------------------------------
!
INTERFACE READ_SURF0_FA
        MODULE PROCEDURE READ_SURFX0_FA
        MODULE PROCEDURE READ_SURFN0_FA
        MODULE PROCEDURE READ_SURFL0_FA
        MODULE PROCEDURE READ_SURFC0_FA
END INTERFACE
INTERFACE READ_SURFX_FA
        MODULE PROCEDURE READ_SURFX1_FA
        MODULE PROCEDURE READ_SURFX2_FA
END INTERFACE
INTERFACE READ_SURFN_FA
        MODULE PROCEDURE READ_SURFN1_FA
        MODULE PROCEDURE READ_SURFL1_FA
END INTERFACE
INTERFACE READ_SURFT_FA
        MODULE PROCEDURE READ_SURFT0_FA
        MODULE PROCEDURE READ_SURFT2_FA
END INTERFACE
!
!----------------------------------------------------------------------------
!
 CONTAINS
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE SFX_FA_VERSION(ONEW)
!     #############################################################
!
!!****  *SFX_FA_VERSION* - routine to find which fa version 
!                          (convergence with GMAP var name)
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D
!
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
LOGICAL, INTENT(OUT) :: ONEW
!
LOGICAL :: GOLD, GWORK
INTEGER :: INGRIB, INBITS, ISTRON, IPUILA, IRESP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:SFX_FA_VERSION',0,ZHOOK_HANDLE)
!
IRESP=0
!
 CALL FANION(IRESP,NUNIT_FA,CPREFIX1D,0,'VERSION',ONEW,GWORK,INGRIB,INBITS,ISTRON,IPUILA)
IF (IRESP/=0) CALL ERROR_READ_SURF_FA('FULLVERSION',IRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:SFX_FA_VERSION',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFX_FA_VERSION
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFX0_FA (&
                                 HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READX0* - routine to read a real scalar
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,              INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,           INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)
ENDIF
!
 CALL FALIT_R(KRESP,NUNIT_FA,YNAME,PFIELD)
IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,KRESP)
!
YCOMMENT = TRIM(YNAME)
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX0_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFX1_FA(HREC,KL,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface 
!
USE MODD_SURFEX_OMP, ONLY : XWORKD, NWORKB, CWORK0
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, &
                            WLOG_MPI
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, NMASK, NFULL, NFULL_EXT, &
                            NDGL, NDLON, NDGUX, NDLUX, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_ERROR_READ_SURF_FA
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),    INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN)  :: KL       ! number of points
REAL, DIMENSION(:),  INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),    INTENT(IN)  :: HDIR     ! type of field :
                                             ! 'H' : field with
                                             !       horizontal spatial dim.
                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=4)           :: YPREFIX
 CHARACTER(LEN=3)           :: YPREF
LOGICAL                    :: GV8
!
INTEGER ::  I, J, INFOMPI
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK2
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX1_FA',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !
  ALLOCATE(XWORKD(NFULL))
  !
  YPREF=HREC(1:3)
  !
  IF (YPREF=='CLS' .OR. YPREF=='SUR' .OR. YPREF=='PRO' .OR. YPREF=='ATM') THEN
    ALLOCATE(ZWORK2(NFULL_EXT))
    CALL FACILE(KRESP,NUNIT_FA,HREC(1:4),0,HREC(5:16),ZWORK2,.FALSE.)
    IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,NWORKB)
    DO J=1,NDGUX
      DO I=1,NDLUX
        XWORKD((J-1)*NDLUX + I) = ZWORK2((J-1)*NDLON + I)
      ENDDO
    ENDDO
    DEALLOCATE(ZWORK2)
    CWORK0 = TRIM(HREC)
  ELSE
    CALL SFX_FA_VERSION(GV8)
    IF(GV8)THEN
      YPREFIX=CPREFIX1D
    ELSE
      YPREFIX='S1D_'
    ENDIF
    CALL FACILE(NWORKB,NUNIT_FA,YPREFIX,0,HREC,XWORKD,.FALSE.)
    IF (NWORKB/=0) CALL ERROR_READ_SURF_FA(HREC,NWORKB)  
    CWORK0 = YPREFIX//TRIM(HREC)
  ENDIF
  !
!$OMP END SINGLE
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD(0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:) = XWORKD(1:KL)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
#ifdef SFX_MPI
  IF (NPROC>1) THEN
!$OMP SINGLE
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(NFULL,KIND(NFULL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    IF ( NRANK/=NPIO ) ALLOCATE(XWORKD(NFULL))
    CALL MPI_BCAST(XWORKD(1:KL),KL*KIND(XWORKD)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
!$OMP END SINGLE
  ENDIF
#endif
  PFIELD(:) = XWORKD(1:KL)
ELSE
  CALL READ_AND_SEND_MPI(XWORKD,PFIELD,NMASK)
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX1_FA',1,ZHOOK_HANDLE)
! 
END SUBROUTINE READ_SURFX1_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFX2_FA(HREC,KL1,KL2,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_OMP, ONLY : XWORKD2, NWORKB, CWORK0
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, &
                            WLOG_MPI
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, NMASK, NFULL, CPREFIX2D
!
USE MODE_FASURFEX
!
USE MODI_ERROR_READ_SURF_FA
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),         INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                  INTENT(IN)  :: KL1      ! number of points
INTEGER,                  INTENT(IN)  :: KL2      ! 2nd dimension
REAL, DIMENSION(:,:),     INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=4)           :: YPREFIX
 CHARACTER(LEN=2)           :: YPATCH
 CHARACTER(LEN=3)           :: YNUM
LOGICAL                    :: GV8
!
INTEGER :: JL, I, INFOMPI ! loop counter
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK   ! work array read in the file
REAL:: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX2_FA',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !
  ALLOCATE(XWORKD2(NFULL,KL2)) 
  !  
  CALL SFX_FA_VERSION(GV8)
  !
  DO JL=1,KL2
    IF(GV8)THEN
      WRITE(YNUM,'(I3.3)')JL
      YPREFIX=CPREFIX2D//YNUM
    ELSE
      WRITE(YPATCH,'(I2.2)')JL
      YPREFIX='S'//YPATCH//'_'
    ENDIF
    CALL FACILE(NWORKB,NUNIT_FA,YPREFIX,JL,HREC,XWORKD2(:,JL),.FALSE.)
     IF (NWORKB/=0) THEN
       CWORK0 = YPREFIX//TRIM(HREC)
       CALL ERROR_READ_SURF_FA(CWORK0,NWORKB)
     ENDIF  
  END DO
  !
  CWORK0 = 'PATCH_'//TRIM(HREC)
  !
!$OMP END SINGLE
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD2(0,0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:,:) = XWORKD2(1:KL1,1:KL2)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(NFULL,KIND(NFULL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    IF ( NRANK/=NPIO ) ALLOCATE(XWORKD2(NFULL,KL2))    
    CALL MPI_BCAST(XWORKD2(1:KL1,1:KL2),KL1*KL2*KIND(XWORKD2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  PFIELD(:,:) = XWORKD2(1:KL1,1:KL2)
ELSE
  CALL READ_AND_SEND_MPI(XWORKD2,PFIELD,NMASK)
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD2)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFX2_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX2_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFN0_FA (&
                                 HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, NMASK, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFN0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)
ENDIF
!
 CALL FALIT_I(KRESP,NUNIT_FA,YNAME,KFIELD)
IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,KRESP)
!
YCOMMENT = YNAME
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFN0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN0_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFN1_FA (&
                                 HREC,KL,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, & 
                            WLOG_MPI
!
USE MODD_SURFEx_OMP, ONLY : LWORK0, CWORK0, NWORKD, NWORKB
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, NMASK, NFULL, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),       INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
INTEGER, DIMENSION(:),  INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
INTEGER ::  I, INFOMPI
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
!
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFN1_FA',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
NWORKB = 0
!$OMP END SINGLE
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD(KL))
!$OMP END SINGLE
ENDIF
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  CALL IO_BUFF(&
               HREC,'R',LWORK0)
  !
  CALL SFX_FA_VERSION(GV8)
  IF(GV8)THEN
    YNAME=CPREFIX1D//TRIM(HREC)
  ELSE
    YMASK=CMASK
    IF (LWORK0) YMASK='FULL  '
    YNAME=TRIM(YMASK)//TRIM(HREC)
  ENDIF
  !
  IF (HDIR=='A') THEN
    ALLOCATE(NWORKD(KL))
  ELSEIF (HDIR/='-') THEN
    ALLOCATE(NWORKD(NFULL))
  END IF    
  !
  CALL FALIT_I_D(NWORKB,NUNIT_FA,YNAME,SIZE(NWORKD),NWORKD)
  IF (NWORKB/=0) CALL ERROR_READ_SURF_FA(HREC,NWORKB)
  !
  CWORK0 = YNAME
  !
!$OMP END SINGLE
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD(0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    KFIELD(:) = NWORKD(1:KL)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(NFULL,KIND(NFULL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    IF ( NRANK/=NPIO ) ALLOCATE(NWORKD(NFULL))    
    CALL MPI_BCAST(NWORKD(1:KL),KL*KIND(NWORKD)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  KFIELD(:) = NWORKD(1:KL)
ELSE
  CALL READ_AND_SEND_MPI(NWORKD,KFIELD,NMASK)
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFN1_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN1_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFC0_FA (&
                                 HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read a character
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC      ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(OUT) :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT  ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50)       :: YCOMMENT
 CHARACTER(LEN=6)        :: YMASK
 CHARACTER(LEN=18)       :: YNAME ! Field Name
 CHARACTER,DIMENSION(40) :: YFIELD
LOGICAL                 :: GV8
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFC0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)
ENDIF
!
 CALL FALIT_C(KRESP,NUNIT_FA,YNAME,40,YFIELD)
IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,KRESP)
WRITE(HFIELD,'(40A1)') YFIELD(:)
!
YCOMMENT = YNAME
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFC0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFC0_FA
!
!
!     #############################################################
      SUBROUTINE READ_SURFL0_FA (&
                                 HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READL0* - routine to read a logical
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,            INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFL0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)
ENDIF
!
 CALL FALIT_L(KRESP,NUNIT_FA,YNAME,OFIELD)
IF (KRESP/=0)CALL ERROR_READ_SURF_FA(HREC,KRESP)
!
YCOMMENT = YNAME
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFL0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL0_FA
!
!
!     #############################################################
      SUBROUTINE READ_SURFL1_FA (&
                                 HREC,KL,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, LWORKD, NWORKB, CWORK0
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, &
                            WLOG_MPI
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),       INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
LOGICAL, DIMENSION(:),  INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
INTEGER           :: INFOMPI
REAL  :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFL1_FA',0,ZHOOK_HANDLE)
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
!$OMP SINGLE
NWORKB = 0
!
ALLOCATE(LWORKD(KL))
!$OMP END SINGLE
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !
  CALL IO_BUFF(&
               HREC,'R',LWORK0)
  !
  CALL SFX_FA_VERSION(GV8)
  IF(GV8)THEN
    YNAME=CPREFIX1D//TRIM(HREC)
  ELSE
    YMASK=CMASK
    IF (LWORK0) YMASK='FULL  '
    YNAME=TRIM(YMASK)//TRIM(HREC)
  ENDIF
  !
  CALL FALIT_L_D(NWORKB,NUNIT_FA,YNAME,KL,LWORKD)
  IF (NWORKB/=0) CALL ERROR_READ_SURF_FA(HREC,NWORKB)
  !
  CWORK0 = YNAME
  !
!$OMP END SINGLE
  !
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
#ifdef SFX_MPI
IF (NPROC>1 .AND. HDIR/='A') THEN
!$OMP SINGLE         
  XTIME0 = MPI_WTIME()
  CALL MPI_BCAST(LWORKD,KL,MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
  XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
!$OMP END SINGLE  
ENDIF
#endif
!
OFIELD = LWORKD
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFL1_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL1_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFT0_FA (&
                                 HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a date
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)   :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KYEAR    ! year
INTEGER,            INTENT(OUT) :: KMONTH   ! month
INTEGER,            INTENT(OUT) :: KDAY     ! day
REAL,               INTENT(OUT) :: PTIME    ! year
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFT0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)//'%TDATE'
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)//'%TDATE'
ENDIF
!
 CALL FALIT_I_D(KRESP,NUNIT_FA,YNAME,3,ITDATE)
IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,KRESP)
!
KYEAR  = ITDATE(1)
KMONTH = ITDATE(2)
KDAY   = ITDATE(3)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)//'%TIME'
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)//'%TIME'
ENDIF
!
 CALL FALIT_R(KRESP,NUNIT_FA,YNAME,PTIME)
IF (KRESP/=0) CALL ERROR_READ_SURF_FA(HREC,KRESP)
!
YCOMMENT = TRIM(HREC)
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFT0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT0_FA
!
!----------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE READ_SURFT2_FA (&
                                 HREC,KL1,KL2,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT2* - routine to read a date
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NLUOUT, CMASK, CPREFIX1D
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ABOR1_SFX
USE MODI_ERROR_READ_SURF_FA
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER                                  :: KL1, KL2
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KDAY     ! day
REAL,    DIMENSION(:,:), INTENT(OUT) :: PTIME    ! year
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
 CHARACTER(LEN=18) :: YNAME ! Field Name
LOGICAL           :: GV8
!
INTEGER, DIMENSION(3,SIZE(KYEAR,1),SIZE(KYEAR,2)) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFT2_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
KYEAR=0
KMONTH=0
KDAY=0
PTIME=0.
!
HCOMMENT=""
!
 CALL IO_BUFF(&
               HREC,'R',LWORK0)
!
 CALL SFX_FA_VERSION(GV8)
IF(GV8)THEN
  YNAME=CPREFIX1D//TRIM(HREC)
ELSE
  YMASK=CMASK
  IF (LWORK0) YMASK='FULL  '
  YNAME=TRIM(YMASK)//TRIM(HREC)
ENDIF
!
WRITE(NLUOUT,*) ' READ_SURFT2_FA : time in 2 dimensions not yet implemented : YNAME=',YNAME
 CALL ABOR1_SFX('MODE_READ_SURF_FA:READ_SURFT2_FA: time in 2 dimensions not yet implemented')
!
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_FA:READ_SURFT2_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT2_FA
!
END MODULE MODE_READ_SURF_FA
