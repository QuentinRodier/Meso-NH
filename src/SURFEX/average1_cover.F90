!     #########
      SUBROUTINE AVERAGE1_COVER(KLUOUT,PLAT,PLON,PVALUE)
!     #######################################################
!
!!**** *AVERAGE1_COVER* computes the sum of values of a cover fractions
!!                              and the nature of terrain on the grid
!!                              from a data in land-cover file
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK, ONLY : XSUMCOVER, NSIZE
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
!
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
INTEGER :: ICOVERCLASS  ! class of cover type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
!     
IF (LHOOK) CALL DR_HOOK('AVERAGE1_COVER',0,ZHOOK_HANDLE)
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
!*    4.     Test on value meaning
!            ---------------------
!
    ICOVERCLASS = NINT(PVALUE(JLOOP))
!
    IF (ICOVERCLASS<1 .OR. ICOVERCLASS > SIZE(XSUMCOVER,2) )  CYCLE
!
!*    5.     Summation
!            ---------
!
    NSIZE(IINDEX(JLOOP))=NSIZE(IINDEX(JLOOP))+1
!
!*    6.     Fraction of cover type
!            ----------------------
!
    XSUMCOVER(IINDEX(JLOOP),ICOVERCLASS)=XSUMCOVER(IINDEX(JLOOP),ICOVERCLASS)+1.
!
  END DO
ENDDO
IF (LHOOK) CALL DR_HOOK('AVERAGE1_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_COVER
