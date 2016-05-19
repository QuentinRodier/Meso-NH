!------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_VECMAP(PVAR,HVAR,KCAT)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault	* Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   25/01/2005
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN
!
USE MODD_SURF_PAR, ONLY:XUNDEF
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),INTENT(IN) :: PVAR   ! variable to write in the file
 CHARACTER(LEN=30), INTENT(IN) :: HVAR   ! end name of the file
INTEGER,           INTENT(IN) :: KCAT   ! catchment number
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=50)          :: CNAME
 CHARACTER(LEN=40)          :: CFMT
INTEGER                    :: JJ,JI,JK
INTEGER                    :: IINDEX ! reference number of the pixel
INTEGER                    :: IUNIT
REAL                       :: ZOUT ! pixel not included in the catchment
REAL                       :: ZMIN,ZMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_VECMAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ZOUT = XUNDEF
ZMIN = MINVAL(PVAR)
ZMAX = MAXVAL(PVAR)
!
CNAME = TRIM(CCAT(KCAT))//TRIM(HVAR)
!
 CALL OPEN_FILE('ASCII ',IUNIT,HFILE=CNAME,HFORM='FORMATTED')
!
DO JI=1,5
  WRITE(IUNIT,*)
ENDDO
!
WRITE(IUNIT,*) XX0(KCAT)
WRITE(IUNIT,*) XY0(KCAT)
WRITE(IUNIT,*) NNXC(KCAT) 
WRITE(IUNIT,*) NNYC(KCAT)
WRITE(IUNIT,*) ZOUT
WRITE(IUNIT,*) XDXT(KCAT)
WRITE(IUNIT,*) ZMIN
WRITE(IUNIT,*) ZMAX
!
DO JI=1,NNYC(KCAT)
  DO JK=1,NNXC(KCAT)
    IINDEX = (JI - 1) * NNXC(KCAT) + JK
    IF (XTOPD(KCAT,IINDEX).EQ.XNUL(KCAT)) THEN
      WRITE(IUNIT,*) ZOUT
    ELSE
      WRITE(IUNIT,*) PVAR(NLINE(KCAT,IINDEX))
    ENDIF
  ENDDO
ENDDO
! 
 CALL CLOSE_FILE('ASCII ',IUNIT)
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_VECMAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_VECMAP

