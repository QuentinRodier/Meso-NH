!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_MAP(PVAR,HVAR)
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
USE MODD_TOPODYN, ONLY : CCAT, NNCAT, NNYC, NNXC, XX0, XY0, XDXT, NLINE, &
                         XTOPD, XNUL
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_GET_LUOUT
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
REAL, DIMENSION(:,:), INTENT(IN) :: PVAR   ! variable to write in the file
 CHARACTER(LEN=30),    INTENT(IN) :: HVAR   ! end name of the file
!
!*      0.2    declarations of local variables
 CHARACTER(LEN=50),DIMENSION(NNCAT) :: CNAME
 CHARACTER(LEN=40)                  :: CFMT
 CHARACTER(*),PARAMETER     :: YPFMT1="('(',I4,'(F10.3,')"
INTEGER                    :: JWRK1,JJ,JI,JCAT
INTEGER                    :: IINDEX ! reference number of the pixel
INTEGER                    :: IUNIT,ILUOUT
REAL                       :: ZOUT ! pixel not included in the catchment
REAL                       :: ZMIN,ZMAX
REAL                       :: ZX1, ZY1, ZX2, ZY2 ! left top and right bottom pixels coordinates
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
 CALL GET_LUOUT('OFFLIN',ILUOUT)

ZOUT = XUNDEF
!
DO JCAT=1,NNCAT
  !
  CNAME(JCAT) = TRIM(CCAT(JCAT))//TRIM(HVAR)
  !
  WRITE(ILUOUT,*) CNAME(JCAT)
  !
  CALL OPEN_FILE('ASCII ',IUNIT,HFILE=CNAME(JCAT),HFORM='FORMATTED')
  !
  !*       1.0    writing header map file
  !               --------------------------------------
  !
  IINDEX = (NNYC(JCAT)-1) * NNXC(JCAT) + 1
  !
  ZX1 = XX0(JCAT)
  ZY1 = XY0(JCAT) + ( (NNYC(JCAT)-1) * XDXT(JCAT) )
  !
  ZMIN = MINVAL(PVAR(JCAT,:))
  ZMAX = MAXVAL(PVAR(JCAT,:),MASK=PVAR(JCAT,:)/=XUNDEF)
  !
  DO JJ=1,5
    WRITE(IUNIT,*)
  ENDDO
  !
  WRITE(IUNIT,*) XX0(JCAT)
  WRITE(IUNIT,*) XY0(JCAT)
  WRITE(IUNIT,*) NNXC(JCAT) 
  WRITE(IUNIT,*) NNYC(JCAT)
  WRITE(IUNIT,*) ZOUT
  WRITE(IUNIT,*) XDXT(JCAT)
  WRITE(IUNIT,*) ZMIN
  WRITE(IUNIT,*) ZMAX
  !
  DO JJ=1,NNYC(JCAT)
    !
    DO JI=1,NNXC(JCAT)
      !
      IINDEX = (JJ - 1) * NNXC(JCAT) + JI
      ZX1 = XX0(JCAT) + ((JI-1) * XDXT(JCAT))
      ZY1 = XY0(JCAT) + ((JJ-1) * XDXT(JCAT))
      !
      IF ( XTOPD(JCAT,IINDEX).EQ.XNUL(JCAT) ) THEN
        !
        WRITE(IUNIT,*) ZOUT
        !
      ELSEIF (NLINE(JCAT,IINDEX)/=0) THEN
        !
        WRITE(IUNIT,*) PVAR(JCAT,NLINE(JCAT,IINDEX))
        !
      ELSE
        !
        WRITE(IUNIT,*) ZOUT
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',IUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_MAP
