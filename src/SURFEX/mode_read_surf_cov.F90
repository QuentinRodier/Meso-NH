!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.

MODULE MODE_READ_SURF_COV

!RJ: split cover from read_surf.F90 to avoid compiler bugs
!RJ: all safety compatibility checks should be done here
PUBLIC :: READ_SURF_COV

 CONTAINS

!     #############################################################
      SUBROUTINE READ_SURF_COV (&
                                HPROGRAM,HREC,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODI_READ_SURF
#ifdef SFX_MNH
USE MODI_READ_SURFX2COV_MNH
#endif
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM    ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC        ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD ! array containing the data field
LOGICAL,DIMENSION(:), INTENT(IN) :: OFLAG   ! mask for array filling
INTEGER, INTENT(OUT) :: KRESP               ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=LEN_HREC) :: YREC
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: JJ
INTEGER            :: JCOVER
INTEGER            :: IL1, IL2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV',0,ZHOOK_HANDLE)
!
YREC = HREC
YCOMMENT="empty"
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
!
PFIELD(:,:)=XUNDEF
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
    CALL READ_SURFX2COV_MNH(YREC,IL1,IL2,PFIELD,OFLAG,KRESP,YCOMMENT,YDIR)
#endif
ELSE
  !
  JCOVER = 0
  DO JJ=1,SIZE(OFLAG)
    !
    IF (.NOT. OFLAG(JJ)) CYCLE
    !
    JCOVER = JCOVER + 1
    !
    WRITE(YREC,'(A5,I3.3)') 'COVER',JJ
    YCOMMENT='X_Y_'//YREC
!RJ: xundef is done for whole array above, to ensure status INTENT(OUT)
!RJ     PFIELD(:,JCOVER)=0.
    !
    CALL READ_SURF(HPROGRAM,YREC,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
    !
  END DO
  !
ENDIF
!
!RJ: what is a point of comment here? last field comment? Should be 'COVER_PACKED' status?
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURF_COV

END MODULE MODE_READ_SURF_COV
