!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODI_READ_SURF
!##################
!
!!****  *READX0* - routine to read a real scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READX0 is
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
  INTERFACE READ_SURF
!
     SUBROUTINE READ_SURFX0 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
REAL, INTENT(OUT) :: PFIELD            ! real scalar to be read
INTEGER,INTENT(OUT) :: KRESP             ! KRESP  : return-code if a problem appears 
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT  ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFX0
!
     SUBROUTINE READ_SURFX1 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT) ::PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR       ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
END SUBROUTINE READ_SURFX1
!
     SUBROUTINE READ_SURFX2 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM    ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC        ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP               ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
END SUBROUTINE READ_SURFX2
!
!RJ: interface to READ_SURFX2COV moved out
!
     SUBROUTINE READ_SURFX3 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM      ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC          ! name of the article to be read
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP                 ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
END SUBROUTINE READ_SURFX3
!
     SUBROUTINE READ_SURFN0 (&
                             HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
INTEGER, INTENT(OUT) :: KFIELD           ! integer to be read  
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFN0
!
     SUBROUTINE READ_SURFN1 (&
                             HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC         ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KFIELD ! integer to be read  
INTEGER, INTENT(OUT) :: KRESP                ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
END SUBROUTINE READ_SURFN1
!
     SUBROUTINE READ_SURFC0 (&
                             HPROGRAM,HREC,HFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM   ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC       ! name of the article to be read
 CHARACTER(LEN=*), INTENT(OUT) :: HFIELD    ! caracter to be read  
INTEGER, INTENT(OUT) :: KRESP              ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFC0
!
      SUBROUTINE READ_SURFL0 (&
                              HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL, INTENT(OUT)         :: OFIELD   ! array containing the data field
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFL0
!
      SUBROUTINE READ_SURFL1 (&
                              HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC         ! name of the article to be read
LOGICAL, DIMENSION(:), INTENT(OUT) :: OFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP                ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
END SUBROUTINE READ_SURFL1
!
      SUBROUTINE READ_SURFT0 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM  ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC      ! name of the article to be read
TYPE(DATE_TIME), INTENT(INOUT) ::TFIELD   ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP             ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFT0
!
      SUBROUTINE READ_SURFT1 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
TYPE (DATE_TIME), DIMENSION(:), INTENT(INOUT) :: TFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFT1
!
      SUBROUTINE READ_SURFT2 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!
!
USE MODD_TYPE_DATE_SURF
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) ::HREC      ! name of the article to be read
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(INOUT)::TFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
END SUBROUTINE READ_SURFT2
!
END INTERFACE
!
END MODULE MODI_READ_SURF
!
!     #############################################################
      SUBROUTINE READ_SURFX0 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : XWORK0, NWORKB, CWORKB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURF0_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURF0_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURF0_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURF0_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURF0_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFX0_MNH
#endif
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN)  :: HREC     ! name of the article to be read
REAL, INTENT(OUT) :: PFIELD               ! the real scalar to be read  
INTEGER, INTENT(OUT) :: KRESP             ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
REAL   :: XTIME0
INTEGER            :: INFOMPI
REAL               :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX0',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFX0_MNH(YREC,XWORK0,NWORKB,CWORKB)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFX0_ARO(YREC,XWORK0,NWORKB,CWORKB)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ') THEN 
  !
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !
!$OMP SINGLE
    !
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
      CALL READ_SURF0_OL(YREC,XWORK0,NWORKB,CWORKB)
#endif
    ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURF0_LFI(YREC,XWORK0,NWORKB,CWORKB)
#endif
    ELSEIF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURF0_ASC(&
                          YREC,XWORK0,NWORKB,CWORKB)
#endif
    ELSEIF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURF0_FA(&
                         YREC,XWORK0,NWORKB,CWORKB)
#endif
    ELSEIF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURF0_NC(YREC,XWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
!$OMP END SINGLE
    !
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME()
!$OMP SINGLE   
    CALL MPI_BCAST(XWORK0,KIND(XWORK0)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  !
ENDIF
!    
PFIELD=XWORK0
!
KRESP = NWORKB
!
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
!$OMP BARRIER
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX0
!
!     #############################################################
      SUBROUTINE READ_SURFX1 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_ASSIM, ONLY : LASSIM, LREAD_ALL, CASSIM_ISBA
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFN_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFN_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFN_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFN_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFX_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFX1_MNH
#endif
!
USE MODI_GET_IOK_ASSIM
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM  ! calling program
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC      ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP             ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: IL, IOK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL = SIZE(PFIELD)
!
IF (LASSIM .AND. CASSIM_ISBA/="OI   " .AND. .NOT.LREAD_ALL) THEN
  !
  CALL GET_IOK_ASSIM(YREC,IOK)
  !
  IF (IOK==0) THEN
    PFIELD(:) = XUNDEF
    IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX1',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
  !
ENDIF
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFX1_MNH(YREC,IL,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFX1_ARO(YREC,IL,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL READ_SURFN_OL(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL READ_SURFN_LFI(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL READ_SURFN_NC(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
  CALL READ_SURFN_ASC(&
                      YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL READ_SURFX_FA(YREC,IL,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX1
!
!     #############################################################
      SUBROUTINE READ_SURFX2 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_ASSIM, ONLY : LASSIM, LREAD_ALL, CASSIM_ISBA
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFN_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFN_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFN_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFN_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFX_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFX2_MNH
#endif
!
USE MODI_GET_IOK_ASSIM
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM    ! calling program
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC        ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP               ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: IL1, IL2, IOK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX2',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
!
IF (LASSIM .AND. CASSIM_ISBA/="OI   "  .AND. .NOT.LREAD_ALL) THEN
  !
  CALL GET_IOK_ASSIM(YREC,IOK)
  !
  IF (IOK==0) THEN
    PFIELD(:,:) = XUNDEF
    IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX2',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
  !
ENDIF
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFX2_MNH(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFX2_ARO(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL READ_SURFN_OL(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL READ_SURFN_LFI(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL READ_SURFN_NC(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
  CALL READ_SURFN_ASC(&
                      YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL READ_SURFX_FA(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX2',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX2
!
!     #############################################################
      SUBROUTINE READ_SURFX3 (&
                             HPROGRAM,HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFN_OL
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM      ! calling program
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC          ! name of the article to be read
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP                 ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: IL1, IL2, IL3
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX3',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
IL3 = SIZE(PFIELD,3)
!
!plmIF (HPROGRAM=='MESONH') THEN
!plm  CALL READ_SURFX3_MNH(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
!plmENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL READ_SURFN_OL(YREC,PFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
!plmIF (HPROGRAM=='ASCII ') THEN
!plm  CALL READ_SURFX3_ASC(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
!plmENDIF
!
!plmIF (HPROGRAM=='AROME ') THEN
!plm  CALL READ_SURFX3_ARO(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
!plmENDIF
!
!plmIF (HPROGRAM=='FA    ') THEN
!plm  CALL READ_SURFX3_FA(YREC,IL1,IL2,PFIELD,KRESP,YCOMMENT,YDIR)
!plmENDIF
!
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFX3',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX3
!
!     #############################################################
      SUBROUTINE READ_SURFN0 (&
                             HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : NWORK0, NWORKB, CWORKB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURF0_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURF0_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURF0_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURF0_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURF0_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFN0_MNH
#endif
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
INTEGER, INTENT(OUT) :: KFIELD           ! the integer to be read  
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
REAL   :: XTIME0
INTEGER            :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFN0',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFN0_MNH(YREC,NWORK0,NWORKB,CWORKB)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFN0_ARO(YREC,NWORK0,NWORKB,CWORKB)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ' ) THEN 
  !
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !
!$OMP SINGLE
    !    
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
      CALL READ_SURF0_OL(YREC,NWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
    IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURF0_LFI(YREC,NWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
    IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURF0_NC(YREC,NWORK0,NWORKB,CWORKB)
#endif
    ENDIF    
    !
    IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURF0_ASC(&
                          YREC,NWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
    IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURF0_FA(&
                         YREC,NWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
!$OMP END SINGLE
    !    
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN          
    XTIME0 = MPI_WTIME()  
!$OMP SINGLE
    CALL MPI_BCAST(NWORK0,KIND(NWORK0)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE  
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)    
  ENDIF    
#endif
  !
ENDIF
!
KFIELD=NWORK0
!
KRESP = NWORKB
!
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
!$OMP BARRIER
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFN0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN0
!
!     #############################################################
      SUBROUTINE READ_SURFN1 (&
                             HPROGRAM,HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFN_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFN_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFN_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFN_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFN_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFN1_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! calling program
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC         ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KFIELD ! the integer to be read  
INTEGER, INTENT(OUT) :: KRESP                ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFN1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL = SIZE(KFIELD,1)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFN1_MNH(YREC,IL,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFN1_ARO(YREC,IL,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL READ_SURFN_OL(YREC,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL READ_SURFN_LFI(YREC,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL READ_SURFN_NC(YREC,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
  CALL READ_SURFN_ASC(&
                      YREC,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL READ_SURFN_FA(&
                     YREC,IL,KFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFN1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN1

!     #############################################################
      SUBROUTINE READ_SURFC0 (&
                             HPROGRAM,HREC,HFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : CWORK0, NWORKB, CWORKB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURF0_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURF0_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURF0_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURF0_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURF0_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFC0_MNH
#endif
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN)  :: HREC     ! name of the article to be read
 CHARACTER(LEN=*), INTENT(OUT) :: HFIELD   ! the integer to be read  
INTEGER, INTENT(OUT) :: KRESP             ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL,INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL,INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
REAL   :: XTIME0
INTEGER            :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFC0',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFC0_MNH(YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFC0_ARO(YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ' ) THEN 
  !
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !  
!$OMP SINGLE
    !    
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
      CALL READ_SURF0_OL(YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURF0_LFI(YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURF0_NC(YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURF0_ASC(&
                          YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURF0_FA(&
                         YREC,CWORK0(1:40),NWORKB,CWORKB)
#endif
    ENDIF
    !  
!$OMP END SINGLE
    !
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)  
#endif
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME()
!$OMP SINGLE
    CALL MPI_BCAST(CWORK0(1:40),40,MPI_CHARACTER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  !
ENDIF
!
HFIELD = CWORK0(1:LEN(HFIELD))
!
KRESP = NWORKB
!
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
!$OMP BARRIER
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFC0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFC0
!
!     #############################################################
      SUBROUTINE READ_SURFL0 (&
                              HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB, CWORKB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURF0_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURF0_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURF0_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURF0_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURF0_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFL0_MNH
#endif
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL, INTENT(OUT) :: OFIELD           ! array containing the data field
INTEGER, INTENT(OUT) :: KRESP           ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT  ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
REAL   :: XTIME0
INTEGER            :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFL0',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFL0_MNH(YREC,LWORK0,NWORKB,CWORKB)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFL0_ARO(YREC,LWORK0,NWORKB,CWORKB)
#endif
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ' ) THEN 
  !  
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    ! 
!$OMP SINGLE
    !
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
      CALL READ_SURF0_OL(YREC,LWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURF0_LFI(YREC,LWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURF0_NC(YREC,LWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURF0_ASC(&
                          YREC,LWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURF0_FA(&
                         YREC,LWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
!$OMP END SINGLE
    ! 
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME()
!$OMP SINGLE
    CALL MPI_BCAST(LWORK0,1,MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)    
  ENDIF
#endif
  !
ENDIF
!
OFIELD = LWORK0
!
KRESP = NWORKB
!
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
!$OMP BARRIER
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFL0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL0
!
!     #############################################################
      SUBROUTINE READ_SURFL1 (&
                              HPROGRAM,HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFN_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFN_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFN_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFN_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFN_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFL1_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! calling program
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC         ! name of the article to be read
LOGICAL, DIMENSION(:), INTENT(OUT) :: OFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP                ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFL1',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL = SIZE(OFIELD)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFL1_MNH(YREC,IL,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFL1_ARO(YREC,IL,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL READ_SURFN_OL(YREC,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL READ_SURFN_LFI(YREC,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL READ_SURFN_NC(YREC,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
  CALL READ_SURFN_ASC(&
                      YREC,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL READ_SURFN_FA(&
                     YREC,IL,OFIELD,KRESP,YCOMMENT,YDIR)
#endif
ENDIF
!
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFL1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL1
!
!     #############################################################
      SUBROUTINE READ_SURFT0 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : NWORKD, XWORK0, NWORKB, CWORKB
!
#ifdef SFX_OL
USE MODE_READ_SURF_OL, ONLY: READ_SURFT_OL
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFT_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFT_NC
#endif
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFT_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFT_FA
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFT0_MNH
#endif
!
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
!RJ: to match actual interface used above, can silently not update values in TFIELD
!RJ TYPE(DATE_TIME), INTENT(OUT) :: TFIELD   ! array containing the data field
TYPE(DATE_TIME), INTENT(INOUT) :: TFIELD   ! array containing the data field
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
!
REAL   :: XTIME0
INTEGER :: ILUOUT
INTEGER :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT0',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
!$OMP SINGLE
ALLOCATE(NWORKD(3))
!$OMP END SINGLE
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFT0_MNH(YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFT0_ARO(YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ' ) THEN 
  !  
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !
!$OMP SINGLE
    !    
    IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
      CALL READ_SURFT_OL(YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURFT_LFI(YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURFT_NC(YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURFT_ASC(&
                          YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURFT_FA(&
                         YREC,NWORKD(1),NWORKD(2),NWORKD(3),XWORK0,NWORKB,CWORKB)
#endif
    ENDIF
    !
!$OMP END SINGLE
    ! 
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME() 
!$OMP SINGLE    
    CALL MPI_BCAST(NWORKD(1),KIND(NWORKD)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD(2),KIND(NWORKD)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD(3),KIND(NWORKD)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(XWORK0,KIND(XWORK0)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE 
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)    
  ENDIF
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
!$OMP BARRIER
!
IF (KRESP==-2) THEN
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) 'WARNING'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'Date is not present file'
  WRITE(ILUOUT,*) 'Forcing value is kept'
  WRITE(ILUOUT,*) ' '
ELSE
  TFIELD%TDATE%YEAR = NWORKD(1)
  TFIELD%TDATE%MONTH = NWORKD(2)
  TFIELD%TDATE%DAY = NWORKD(3)
  TFIELD%TIME = XWORK0
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT0
!
!     #############################################################
      SUBROUTINE READ_SURFT1 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : NWORKD2, XWORKD, NWORKB, CWORKB
!
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFT_ASC
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFT_LFI
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFT_NC
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFT1_MNH
#endif
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM   ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC       ! name of the article to be read
TYPE(DATE_TIME), DIMENSION(:), INTENT(INOUT)::TFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP              ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: ILUOUT
INTEGER            :: INFOMPI
!
REAL   :: XTIME0
INTEGER :: IL1
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT1',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
IL1 = SIZE(TFIELD,1)
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
!$OMP SINGLE
ALLOCATE(NWORKD2(IL1,3))
ALLOCATE(XWORKD(IL1))
!$OMP END SINGLE
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL READ_SURFT1_MNH(YREC,IL1,NWORKD2(:,1),NWORKD2(:,2),NWORKD2(:,3),XWORKD,NWORKB,CWORKB)
#endif
ELSE IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL READ_SURFT1_ARO(YREC,IL1,NWORKD2(:,1),NWORKD2(:,2),NWORKD2(:,3),XWORKD,NWORKB,CWORKB)
#endif
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
    HPROGRAM=='NC    ') THEN 
  !
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !
!$OMP SINGLE
    !
    IF (HPROGRAM=='OFFLIN') THEN
      CALL ABOR1_SFX('READ_SURFT1: NOT AVAILABLE FOR OFFLIN')
    ELSE IF (HPROGRAM=='FA    ') THEN
      CALL ABOR1_SFX('READ_SURFT1: NOT AVAILABLE FOR FA')      
    ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURFT_ASC(&
                          YREC,NWORKD2(:,1),NWORKD2(:,2),NWORKD2(:,3),XWORKD,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURFT_LFI(YREC,NWORKD2(:,1),NWORKD2(:,2),NWORKD2(:,3),XWORKD,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURFT_NC(YREC,NWORKD2(:,1),NWORKD2(:,2),NWORKD2(:,3),XWORKD,NWORKB,CWORKB)
#endif
    ENDIF
    !
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !
!$OMP END SINGLE
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME()         
!$OMP SINGLE    
    CALL MPI_BCAST(NWORKD2(:,1),IL1*KIND(NWORKD2)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD2(:,2),IL1*KIND(NWORKD2)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD2(:,3),IL1*KIND(NWORKD2)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(XWORKD,IL1*KIND(XWORKD)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)    
  ENDIF
#endif
  !
ENDIF  
! 
KRESP = NWORKB
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
IF (KRESP==-2) THEN
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) 'WARNING'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'Date is not present file'
  WRITE(ILUOUT,*) 'Forcing value is kept'
  WRITE(ILUOUT,*) ' '
ELSE
  TFIELD(:)%TDATE%YEAR  = NWORKD2(:,1)
  TFIELD(:)%TDATE%MONTH = NWORKD2(:,2)
  TFIELD(:)%TDATE%DAY   = NWORKD2(:,3)
  TFIELD(:)%TIME        = XWORKD(:)
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD2)
DEALLOCATE(XWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT1
!
!     #############################################################
      SUBROUTINE READ_SURFT2 (&
                              HPROGRAM,HREC,TFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_READ, XTIME_COMM_READ
USE MODD_SURFEX_OMP, ONLY : NWORKD3, XWORKD2, NWORKB, CWORKB
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
#ifdef SFX_ASC
USE MODE_READ_SURF_ASC, ONLY: READ_SURFT_ASC
#endif
#ifdef SFX_FA
USE MODE_READ_SURF_FA, ONLY: READ_SURFT_FA
#endif
#ifdef SFX_NC
USE MODE_READ_SURF_NC, ONLY: READ_SURFT_NC
#endif
#ifdef SFX_LFI
USE MODE_READ_SURF_LFI, ONLY: READ_SURFT_LFI
#endif
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC     ! name of the article to be read
TYPE(DATE_TIME), DIMENSION(:,:), INTENT(INOUT) :: TFIELD ! array containing the data field  
INTEGER, INTENT(OUT) :: KRESP            ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=LEN_HREC)  :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: ILUOUT
INTEGER            :: INFOMPI
!
INTEGER :: IL1, IL2
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT2',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
NWORKB = 0
 CWORKB = ""
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL1 = SIZE(TFIELD,1)
IL2 = SIZE(TFIELD,2)
!
!$OMP SINGLE
ALLOCATE(XWORKD2(IL1,IL2))
ALLOCATE(NWORKD3(IL1,IL2,3))
!$OMP END SINGLE
!
IF (HPROGRAM=='MESONH') THEN
  CALL ABOR1_SFX('READ_SURFT2: NOT AVAILABLE FOR MESONH')
ELSE IF (HPROGRAM=='AROME ') THEN
  CALL ABOR1_SFX('READ_SURFT2: NOT AVAILABLE FOR AROME')  
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. &
    HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' ) THEN 
  !  
  IF (NRANK==NPIO) THEN
    !
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    !
!$OMP SINGLE
    !
    IF (HPROGRAM=='OFFLIN') THEN
      CALL ABOR1_SFX('READ_SURFT2: NOT AVAILABLE FOR OFFLIN')
    ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
      CALL READ_SURFT_LFI(YREC,NWORKD3(:,:,1),NWORKD3(:,:,2),NWORKD3(:,:,3),&
        XWORKD2,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
      CALL READ_SURFT_ASC(&
                          YREC,NWORKD3(:,:,1),NWORKD3(:,:,2),NWORKD3(:,:,3),&
        XWORKD2,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
      CALL READ_SURFT_FA(&
                         YREC,IL1,IL2,NWORKD3(:,:,1),NWORKD3(:,:,2),NWORKD3(:,:,3),&
        XWORKD2,NWORKB,CWORKB)
#endif
    ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
      CALL READ_SURFT_NC(YREC,NWORKD3(:,:,1),NWORKD3(:,:,2),NWORKD3(:,:,3),&
        XWORKD2,NWORKB,CWORKB)
#endif
    ENDIF
    !
#ifdef SFX_MPI
    XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
    !    
!$OMP END SINGLE
    !
  ENDIF
  !
#ifdef SFX_MPI
  IF (YDIR/='A' .AND. NPROC>1) THEN
    XTIME0 = MPI_WTIME()   
!$OMP SINGLE
    CALL MPI_BCAST(NWORKD3(:,:,1),IL1*IL2*KIND(NWORKD3)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD3(:,:,2),IL1*IL2*KIND(NWORKD3)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NWORKD3(:,:,3),IL1*IL2*KIND(NWORKD3)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(XWORKD2,IL1*IL2*KIND(XWORKD2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE     
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  !
ENDIF
!
KRESP = NWORKB
IF (PRESENT(HCOMMENT)) HCOMMENT = CWORKB
!
IF (KRESP==-2) THEN
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) 'WARNING'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'Date is not present file'
  WRITE(ILUOUT,*) 'Forcing value is kept'
  WRITE(ILUOUT,*) ' '
ELSE
  TFIELD(:,:)%TDATE%YEAR  = NWORKD3(:,:,1)
  TFIELD(:,:)%TDATE%MONTH = NWORKD3(:,:,2)
  TFIELD(:,:)%TDATE%DAY   = NWORKD3(:,:,3)
  TFIELD(:,:)%TIME        = XWORKD2(:,:) 
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD3)
DEALLOCATE(XWORKD2)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_SURF:READ_SURFT2',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT2
