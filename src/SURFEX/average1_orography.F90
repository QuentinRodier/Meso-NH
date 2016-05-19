!     #########
      SUBROUTINE AVERAGE1_OROGRAPHY(KLUOUT,PLAT,PLON,PVALUE)
!     #######################################################
!
!!**** *AVERAGE1_OROGRAPHY* computes the sum of orography, squared orography
!!                              and subgrid orography characteristics
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
USE MODD_PGDWORK,       ONLY : XSUMVAL, XSUMVAL2, NSIZE, XSSQO, LSSQO, NSSO
USE MODD_SURF_ATM_SSO_n, ONLY : XMIN_ZS, XMAX_ZS
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
INTEGER, DIMENSION(SIZE(PLAT)) :: ISSOX  ! X submesh index in their mesh of all input points
INTEGER, DIMENSION(SIZE(PLAT)) :: ISSOY  ! Y submesh index in their mesh of all input points
INTEGER, DIMENSION(SIZE(PLAT)) :: INUM ! numbers of index
!
INTEGER :: JLOOP        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
!     
IF (LHOOK) CALL DR_HOOK('AVERAGE1_OROGRAPHY',0,ZHOOK_HANDLE)
IF (ALLOCATED(XNUM)) DEALLOCATE(XNUM)
ALLOCATE(XNUM(SIZE(PLAT)))
!
XNUM(:)=1
!
DO WHILE(MAXVAL(XNUM).NE.0)
                                         
  CALL GET_MESH_INDEX(KLUOUT,PLAT,PLON,IINDEX,NSSO,ISSOX,ISSOY)
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
!*    5.     Orography
!            ---------
!
    XSUMVAL(IINDEX(JLOOP))=XSUMVAL(IINDEX(JLOOP))+PVALUE(JLOOP)
!
!*    6.     Square of Orography
!            -------------------
!
    XSUMVAL2(IINDEX(JLOOP))=XSUMVAL2(IINDEX(JLOOP))+PVALUE(JLOOP)**2
!
!*    7.     Maximum orography in a subgrid square
!            -------------------------------------
!
    LSSQO(ISSOX(JLOOP),ISSOY(JLOOP),IINDEX(JLOOP)) = .TRUE.
    XSSQO(ISSOX(JLOOP),ISSOY(JLOOP),IINDEX(JLOOP)) = &
         MAX (  XSSQO(ISSOX(JLOOP),ISSOY(JLOOP),IINDEX(JLOOP)) , PVALUE(JLOOP) )   
!
!
!*    8.     Maximum orography in the mesh
!            -----------------------------
!
    XMAX_ZS(IINDEX(JLOOP))=MAX(XMAX_ZS(IINDEX(JLOOP)),PVALUE(JLOOP))
!
!
!*    9.     Minimum orography in the mesh
!            -----------------------------
!
    XMIN_ZS(IINDEX(JLOOP))=MIN(XMIN_ZS(IINDEX(JLOOP)),PVALUE(JLOOP))
!
!
  END DO
!
ENDDO
IF (LHOOK) CALL DR_HOOK('AVERAGE1_OROGRAPHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_OROGRAPHY
