!     #################################################################################
SUBROUTINE PREP_OCEAN_ASCLLV(HPROGRAM,HSURF,HFILE, & 
     &                       KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_OCEAN_ASCLLDV* - prepares oceanic fields from personal data in ascii
!!                            formed as lat,lon, depth, value
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!   Read the input file which must be ascii typed, lat,lon,depth, value
!!   Version 1: 
!!              The data must be on the same grid as the pgd and on the same
!!              vertical grid as prescribed in oceanvergrid.f90
!!              NDEPTH= a definirtn nlev=NOCKMAX (modd_ocean_gridn)
!!              
!!  Version 2: (not done yet)
!!              - dummy or namlist for nb verticals levels
!!              - file prescribing the vertical grid 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. PEYRILLE
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2011
!!------------------------------------------------------------------
!
USE MODD_PREP,       ONLY : CINTERP_TYPE, CINGRID_TYPE
USE MODD_PGD_GRID,       ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_OCEAN_CSTS , ONLY : NOCKMAX
USE MODD_PGDWORK,        ONLY : XSUMVAL, NSIZE
USE MODD_SURF_ATM_n,     ONLY : XNATURE, XSEA, XTOWN, XWATER
USE MODD_SEAFLUX_GRID_n , ONLY : NDIM
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_GET_LUOUT
USE MODI_GET_LATLONMASK_n
USE MODI_PACK_SAME_RANK
!
USE MODI_GET_TYPE_DIM_n
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_GET_SURF_MASK_N
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
!CHARACTER(LEN=28),  INTENT(IN), OPTIONAL   :: HNCVARNAME!var to read 
REAL, POINTER, DIMENSION(:,:,:)   :: PFIELD    ! field to interpolate horizontally
!
!
!*      0.2    declarations of local variables
REAL,DIMENSION(:), ALLOCATABLE :: ZLAT
REAL,DIMENSION(:), ALLOCATABLE :: ZLON
REAL,DIMENSION(:), ALLOCATABLE :: ZDEPTH
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD
REAL,DIMENSION(:,:,:,:), ALLOCATABLE :: ZFIELDR
!
INTEGER :: IL
INTEGER :: IGLB  ! logical unit
INTEGER :: IDIM, ILU
INTEGER :: JI,JK
!
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
 CHARACTER(LEN=6)    :: YMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_ASCLLV',0,ZHOOK_HANDLE)
!
CINGRID_TYPE='CONF PROJ '

!*      1.    get full dimension of grid
 CALL GET_TYPE_DIM_n('FULL  ',NL)
!*      2.    get Ocean dimension
!
 CALL GET_TYPE_DIM_n('SEA   ',IL)

!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!!

WRITE(KLUOUT,*) "==================================== "
WRITE(KLUOUT,*) "Control print in prep_ocean_ascllv "
WRITE(KLUOUT,*) "NL, NOCKMAX", NL,NOCKMAX

ALLOCATE(ZLAT(NL))
ALLOCATE(ZLON(NL))
ALLOCATE(ZDEPTH(NOCKMAX))

ALLOCATE(ZFIELDR(NL,NOCKMAX,4, 1))
ALLOCATE(ZFIELD(NL,NOCKMAX, 1))


WRITE(KLUOUT,*) "ZFIELDR",SHAPE(ZFIELDR)
WRITE(KLUOUT,*) "File name used in ocean ascllv", HFILE

WRITE(KLUOUT,*)  "USURF= " , HSURF
WRITE(KLUOUT,*) "NL (dim)=", NL
WRITE(KLUOUT,*) "IL (dim)=", IL
!
!*      2.     Reading of field
!              ----------------
 CALL OPEN_FILE(HPROGRAM,IGLB,HFILE,'FORMATTED',HACTION='READ')
!

DO JI=1,NL
  DO JK=1,NOCKMAX
    READ(IGLB,*,END=99) ZLAT(JI),ZLON(JI), ZDEPTH(JK), & 
    ZFIELDR(JI,JK,1,1), ZFIELDR(JI,JK,2,1),ZFIELDR(JI,JK,3,1), & 
    ZFIELDR(JI,JK,4,1)
  END DO
END DO


 
!      3. Close the file
99 CALL CLOSE_FILE (HPROGRAM,IGLB)

WRITE(KLUOUT,*) MINVAL(ZFIELDR), MAXVAL(ZFIELDR)

!
! Get the correct varaibles
SELECT CASE (HSURF)
  CASE('TEMP_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,1,1)
  CASE('SALT_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,2,1)
  CASE('UCUR_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,3,1)
  CASE('VCUR_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,4,1)
END SELECT



!*      3.     Interpolation method
!              --------------------
!
CINTERP_TYPE='NONE  '
!CINTERP_TYPE='HORIBL'
!

YMASK = 'SEA   '
 CALL GET_TYPE_DIM_n(YMASK,IDIM)
WRITE(KLUOUT,*) "IDIM (dim sea) =", IDIM

ALLOCATE(PFIELD(1:IDIM,1:SIZE(ZFIELD,2),1:SIZE(ZFIELD,3)))

IF (IDIM/=SIZE(PFIELD,1)) THEN
   WRITE(KLUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PFIELD)
   CALL ABOR1_SFX('PGD_FIELD: WRONG DIMENSION OF MASK')
ENDIF

ALLOCATE(IMASK(IDIM))
ILU=0
 CALL GET_SURF_MASK_n(YMASK,IDIM,IMASK,ILU,KLUOUT)
DO JK=1,NOCKMAX
  CALL PACK_SAME_RANK(IMASK,ZFIELD(:,JK,1),PFIELD(:,JK,1))
END DO
DEALLOCATE(IMASK)

!*      4.     Deallocations
!              -------------
!
IF (ALLOCATED(ZLON       ))  DEALLOCATE(ZLON  )
IF (ALLOCATED(ZLAT       ))  DEALLOCATE(ZLAT  )
IF (ALLOCATED(ZDEPTH      ))  DEALLOCATE(ZDEPTH )
IF (ALLOCATED(ZFIELD      ))  DEALLOCATE(ZFIELD )
IF (ALLOCATED(ZFIELDR     ))  DEALLOCATE(ZFIELDR )
!
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_OCEAN_ASCLLV
