!     #########
      SUBROUTINE WRITESURF_TEB_HYDRO_n(HSELECT, TH, HPROGRAM,HPATCH, KTEB_SOIL )
!     #####################################
!
!!****  *WRITESURF_TEB_HYDRO_n* - writes ISBA prognostic fields
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    Based on "writesurf_teb_gardenn" 
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!    A. Lemonsu
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2013 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_HYDRO_n,     ONLY : TEB_HYDRO_t
!
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TEB_HYDRO_t), INTENT(IN) :: TH 
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT 

CHARACTER(LEN=6),        INTENT(IN)    :: HPROGRAM  ! program calling
CHARACTER(LEN=3),        INTENT(IN)    :: HPATCH    ! current teb patch
INTEGER,                 INTENT(IN)    :: KTEB_SOIL ! number of soil layer
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                            :: IRESP          ! IRESP  : return-code if a problem appears
!
 CHARACTER(LEN=30)                 :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100)                :: YCOMMENT       ! Comment string
 CHARACTER(LEN=14)                 :: YFORM          ! Writing format
 CHARACTER(LEN=4 )                 :: YLVL           
!
INTEGER                           :: JLAYER         ! loop counter on soil layers
!
REAL, DIMENSION(:),  ALLOCATABLE  :: ZWORK          ! 2D array to write data in file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_HYDRO_N',0,ZHOOK_HANDLE)
!
ALLOCATE(ZWORK(SIZE(TH%XWG_ROAD,1)))
!
!* soil liquid water content for soil under roads
!
DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WG_ROAD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_WG_ROAD',JLAYER,' (m3/m3)'
  ZWORK=TH%XWG_ROAD(:,JLAYER)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* soil ice water content for soil under roads
!
DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WGI_ROAD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A12,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A12,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_WGI_ROAD',JLAYER,' (m3/m3)'
  ZWORK=TH%XWGI_ROAD(:,JLAYER)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* soil liquid water content for soil under buildings
!
DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WG_BLD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A10,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A10,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_WG_BLD',JLAYER,' (m3/m3)'
  ZWORK=TH%XWG_BLD(:,JLAYER)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil ice water content for soil under buildings
!
DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WGI_BLD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_WGI_BLD',JLAYER,' (m3/m3)'
  ZWORK=TH%XWGI_BLD(:,JLAYER)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
DEALLOCATE(ZWORK)
! 
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_HYDRO_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_HYDRO_n
