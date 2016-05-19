!     #########
      SUBROUTINE PACK_PGD(HPROGRAM, HSURF,                 &
                            HGRID,  PGRID_PAR,               &
                            OCOVER, PCOVER, PZS,             &
                            PLAT, PLON, PMESH_SIZE, PDIR     )  
!     ##############################################################
!
!!**** *PACK_PGD* packs ISBA physiographic fields from all surface points to ISBA points
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Escobar J.  08/02/2005 : bug declare ILU local variable
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,       ONLY : NL, CGRID, XGRID_PAR
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_GET_COVER_n
USE MODI_GET_LCOVER_n
USE MODI_GET_ZS_n
USE MODI_PACK_SAME_RANK
USE MODI_PACK_GRID
USE MODI_LATLON_GRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_GET_LUOUT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=6),        INTENT(IN) :: HSURF     ! surface type
!
 CHARACTER(LEN=10),       INTENT(OUT):: HGRID     ! grid used
REAL,    DIMENSION(:),   POINTER    :: PGRID_PAR ! grid definition
LOGICAL, DIMENSION(:),   INTENT(OUT):: OCOVER    ! list of present cover
REAL,    DIMENSION(:,:), INTENT(OUT):: PCOVER    ! cover fraction
REAL,    DIMENSION(:),   INTENT(OUT):: PZS       ! zs
REAL,    DIMENSION(:),   INTENT(OUT):: PLAT      ! latitude
REAL,    DIMENSION(:),   INTENT(OUT):: PLON      ! longitude
REAL,    DIMENSION(:),   INTENT(OUT):: PMESH_SIZE! mesh size
REAL,    DIMENSION(:),   INTENT(OUT), OPTIONAL :: PDIR ! angle of grid axis with N.
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER                        :: IL     ! number of points
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: JCOVER
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
REAL,    DIMENSION(SIZE(PLAT)) :: ZDIR
!
REAL, DIMENSION(NL)    :: ZCOVER ! cover  on all surface points
LOGICAL, DIMENSION(JPCOVER)    :: GCOVER ! list of existing cover
REAL, DIMENSION(NL)            :: ZZS    ! zs     on all surface points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_PGD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Number of points and packing
!             ----------------------------
!
 CALL GET_TYPE_DIM_n(HSURF,IL)
ALLOCATE(IMASK(IL))
ILU=0
 CALL GET_SURF_MASK_n(HSURF,IL,IMASK,ILU,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Packing of grid
!             ---------------
!
 CALL PACK_GRID(IMASK,CGRID,HGRID,XGRID_PAR,PGRID_PAR)
!
!-------------------------------------------------------------------------------
!
!*    3.      Computes geographical quantities
!             --------------------------------
!
 CALL LATLON_GRID(HGRID,SIZE(PGRID_PAR),IL,ILUOUT,PGRID_PAR,PLAT,PLON,PMESH_SIZE,ZDIR)
!
IF (PRESENT(PDIR)) PDIR = ZDIR
!
!-------------------------------------------------------------------------------
!
!*    4.      Packing of fields
!             -----------------
!
DO JCOVER=1,JPCOVER
  CALL GET_COVER_n(HPROGRAM,NL,JCOVER,ZCOVER)
  CALL PACK_SAME_RANK(IMASK,ZCOVER(:),PCOVER(:,JCOVER))
ENDDO

 CALL GET_LCOVER_n(HPROGRAM,JPCOVER,GCOVER)
 CALL GET_ZS_n(HPROGRAM,NL,ZZS)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
OCOVER=GCOVER
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL PACK_SAME_RANK(IMASK,ZZS(:),PZS(:))
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(IMASK)
IF (LHOOK) CALL DR_HOOK('PACK_PGD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_PGD
