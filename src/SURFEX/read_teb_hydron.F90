!     #########
      SUBROUTINE READ_TEB_HYDRO_n(DTCO,U,TH,HPROGRAM,HPATCH,KTEB_SOIL)
!     ##################################
!
!!****  *READ_TEB_HYDRO_n* - routine to initialise ISBA variables for urban hydrology
!!                         
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!
!!      READ_SURF for general reading : 08/2003 (S.Malardel)
!!      A. LEmonsu   02/2013 : adaptation to urban hydrology
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
USE MODD_TEB_HYDRO_n,  ONLY : TEB_HYDRO_t

!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_READ_SURF
!
USE MODI_INIT_IO_SURF_n
USE MODI_SET_SURFEX_FILEIN
USE MODI_END_IO_SURF_n
USE MODI_TOWN_PRESENCE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
TYPE(TEB_HYDRO_t), INTENT(INOUT) :: TH
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current TEB patch identificator
INTEGER,           INTENT(IN)  :: KTEB_SOIL   !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILU     ! 1D physical dimension
INTEGER           :: IRESP   ! Error code after reading
CHARACTER(LEN=30) :: YRECFM  ! Name of the article to be read
CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER           :: JLAYER  ! Loop index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_HYDRO_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!
!* soil liquid water content (for roads)
!
ALLOCATE(TH%XWG_ROAD(ILU,KTEB_SOIL))

DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WG_ROAD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  TH%XWG_ROAD(:,JLAYER)=ZWORK
END DO
!
!* soil ice water content (for roads)
!
ALLOCATE(TH%XWGI_ROAD(ILU,KTEB_SOIL))

DO JLAYER=1,KTEB_SOIL
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WGI_ROAD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  TH%XWGI_ROAD(:,JLAYER)=ZWORK
END DO
!
!* soil liquid water content (for buildings)
!
ALLOCATE(TH%XWG_BLD(ILU,KTEB_SOIL))
DO JLAYER=1,KTEB_SOIL
  !WRITE(YLVL,'(I4)') JLAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WG_BLD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  TH%XWG_BLD(:,JLAYER)=ZWORK
END DO
!
!* soil ice water content (for buildings)
!
ALLOCATE(TH%XWGI_BLD(ILU,KTEB_SOIL))
DO JLAYER=1,KTEB_SOIL
  !WRITE(YLVL,'(I4)') JLAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'WGI_BLD'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  TH%XWGI_BLD(:,JLAYER)=ZWORK
END DO
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_HYDRO_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_HYDRO_n
