!     #########
      SUBROUTINE AVERAGE1_CTI(KLUOUT,PLAT,PLON,PVALUE)
!     ################################################
!
!!**** *AVERAGE1_CTI* computes the sum of cti, squared cti
!!                    and subgrid cti characteristics
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,       ONLY : XSUMVAL, XSUMVAL2, XSUMVAL3, NSIZE, &
                                  XMAX_WORK, XMIN_WORK   
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
!!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                 INTENT(IN)    :: KLUOUT
REAL, DIMENSION(:),      INTENT(IN)    :: PLAT    ! latitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PLON    ! longitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PVALUE  ! value of the point to add
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain
!
INTEGER :: JLOOP        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',0,ZHOOK_HANDLE)
IF (ALLOCATED(XNUM)) DEALLOCATE(XNUM)
ALLOCATE(XNUM(SIZE(PLAT)))
!
XNUM(:)=1
!
DO WHILE(MAXVAL(XNUM).NE.0)
!
  CALL GET_MESH_INDEX(KLUOUT,PLAT,PLON,IINDEX)
!
!*    2.     Loop on all input data points
!            -----------------------------
!     
  DO JLOOP = 1 , SIZE(PLAT)
!
!*    3.     Tests on position
!            -----------------
!     
    IF (IINDEX(JLOOP)==0) CYCLE
!
!*    4.     Summation
!            ---------
!
    NSIZE(IINDEX(JLOOP))=NSIZE(IINDEX(JLOOP))+1
!
!*    5.     CTI
!            ---
!
    XSUMVAL(IINDEX(JLOOP))=XSUMVAL(IINDEX(JLOOP))+PVALUE(JLOOP)
!
!*    6.     Square of CTI
!            -------------
!
    XSUMVAL2(IINDEX(JLOOP))=XSUMVAL2(IINDEX(JLOOP))+PVALUE(JLOOP)**2
!
!
!*    7.     Cube of CTI
!            -------------
!
    XSUMVAL3(IINDEX(JLOOP))=XSUMVAL3(IINDEX(JLOOP))+PVALUE(JLOOP)**3
!
!
!*    8.     Maximum CTI in the mesh
!            -----------------------
!
    XMAX_WORK(IINDEX(JLOOP))=MAX(XMAX_WORK(IINDEX(JLOOP)),PVALUE(JLOOP))
!
!
!*    9.     Minimum CTI in the mesh
!            -----------------------
!
    XMIN_WORK(IINDEX(JLOOP))=MIN(XMIN_WORK(IINDEX(JLOOP)),PVALUE(JLOOP))
!
!
  ENDDO
!  
END DO
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_CTI
