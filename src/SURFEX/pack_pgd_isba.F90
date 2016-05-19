!     #########
      SUBROUTINE PACK_PGD_ISBA(HPROGRAM,                                    &
                                 PAOSIP, PAOSIM, PAOSJP, PAOSJM,              &
                                 PHO2IP, PHO2IM, PHO2JP, PHO2JM,              &
                                 PSSO_SLOPE                                   )  
!     ##############################################################
!
!!**** *PACK_PGD_ISBA* packs ISBA physiographic fields from all surface points to ISBA points
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
USE MODD_ISBA_n,          ONLY :XAOSIP, XAOSIM, XAOSJP, XAOSJM, &
                                  XHO2IP, XHO2IM, XHO2JP, XHO2JM, &
                                  XSSO_SLOPE  
USE MODD_ISBA_GRID_n,     ONLY : NDIM, CGRID, XGRID_PAR
!
USE MODI_PACK_SAME_RANK
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
REAL,    DIMENSION(:),   INTENT(IN) :: PAOSIP    ! A/S i+ on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PAOSIM    ! A/S i- on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PAOSJP    ! A/S j+ on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PAOSJM    ! A/S j- on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PHO2IP    ! h/2 i+ on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PHO2IM    ! h/2 i- on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PHO2JP    ! h/2 j+ on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PHO2JM    ! h/2 j- on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PSSO_SLOPE! subgrid slope on all surface points
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_PGD_ISBA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Number of points and packing
!             ----------------------------
!
 CALL GET_TYPE_DIM_n('NATURE',NDIM)
ALLOCATE(IMASK(NDIM))
ILU=0
 CALL GET_SURF_MASK_n('NATURE',NDIM,IMASK,ILU,ILUOUT)
!
!
!-------------------------------------------------------------------------------
!
!*    2.      Packing of fields
!             -----------------
!
ALLOCATE(XAOSIP(NDIM))
ALLOCATE(XAOSIM(NDIM))
ALLOCATE(XAOSJP(NDIM))
ALLOCATE(XAOSJM(NDIM))
ALLOCATE(XHO2IP(NDIM))
ALLOCATE(XHO2IM(NDIM))
ALLOCATE(XHO2JP(NDIM))
ALLOCATE(XHO2JM(NDIM))
ALLOCATE(XSSO_SLOPE(NDIM))
 CALL PACK_SAME_RANK(IMASK,PAOSIP(:),XAOSIP(:))
 CALL PACK_SAME_RANK(IMASK,PAOSIM(:),XAOSIM(:))
 CALL PACK_SAME_RANK(IMASK,PAOSJP(:),XAOSJP(:))
 CALL PACK_SAME_RANK(IMASK,PAOSJM(:),XAOSJM(:))
 CALL PACK_SAME_RANK(IMASK,PHO2IP(:),XHO2IP(:))
 CALL PACK_SAME_RANK(IMASK,PHO2IM(:),XHO2IM(:))
 CALL PACK_SAME_RANK(IMASK,PHO2JP(:),XHO2JP(:))
 CALL PACK_SAME_RANK(IMASK,PHO2JM(:),XHO2JM(:))
 CALL PACK_SAME_RANK(IMASK,PSSO_SLOPE(:),XSSO_SLOPE(:))
IF (LHOOK) CALL DR_HOOK('PACK_PGD_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_PGD_ISBA
