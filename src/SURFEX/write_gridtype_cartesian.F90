!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################
      SUBROUTINE WRITE_GRIDTYPE_CARTESIAN (DGU, U, &
                                           HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP,HDIR)
!     #################################################################
!
!!****  *WRITE_GRIDTYPE_CARTESIAN* - routine to write the horizontal grid
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
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_WRITE_SURF
!
USE MODE_GRIDTYPE_CARTESIAN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER,                    INTENT(IN)  :: KLU        ! number of points
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
INTEGER,                    INTENT(OUT) :: KRESP      ! error return code
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR       ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! 'A' : (complete) field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
INTEGER                           :: IIMAX    ! number of points in I direction
INTEGER                           :: IJMAX    ! number of points in J direction
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X conformal coordinate of grid mesh (dim IIMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y conformal coordinate of grid mesh (dim IJMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX      ! X grid mesh size (dim IIMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY      ! Y grid mesh size (dim IJMAX)
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
ALLOCATE(ZX (KLU))
ALLOCATE(ZY (KLU))
ALLOCATE(ZDX(KLU))
ALLOCATE(ZDY(KLU))
!
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0,           &
                              IIMAX,IJMAX,ZX,ZY,ZDX,ZDY        )  
!
!---------------------------------------------------------------------------
!
!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
YCOMMENT=' '
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'LAT0',ZLAT0,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'LON0',ZLON0,KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'IMAX ',IIMAX, KRESP,YCOMMENT)
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'JMAX ',IJMAX, KRESP,YCOMMENT)
IF (PRESENT(HDIR)) THEN
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'XX',ZX,KRESP,YCOMMENT,HDIR)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'YY',ZY,KRESP,YCOMMENT,HDIR)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DX',ZDX,KRESP,YCOMMENT,HDIR)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DY',ZDY,KRESP,YCOMMENT,HDIR)
ELSE
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'XX',ZX,KRESP,YCOMMENT)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'YY',ZY,KRESP,YCOMMENT)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DX',ZDX,KRESP,YCOMMENT)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,'DY',ZDY,KRESP,YCOMMENT)
END IF
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_CARTESIAN
