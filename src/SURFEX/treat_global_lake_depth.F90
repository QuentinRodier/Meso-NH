!     #########
      SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH(HPROGRAM,PDEPTH,KSTATUS)
!     ##############################################################
!
!!**** *TREAT_GLOBAL_LAKE_DEPTH* monitor for averaging and interpolations of ISBA physiographic fields
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    17/02/11
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : XTNG, NSIZE
USE MODD_SURF_ATM_n,     ONLY : XWATER
USE MODD_DATA_LAKE,      ONLY : CLAKELDB, CSTATUSLDB, NGRADDEPTH_LDB, NGRADSTATUS_LDB 
!
USE MODI_GET_LUOUT
USE MODI_TREAT_FIELD
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_GET_SURF_MASK_n
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
REAL, DIMENSION(:),INTENT(OUT):: PDEPTH    ! physiographic field
INTEGER, DIMENSION(:),INTENT(OUT):: KSTATUS   ! physiographic field
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
INTEGER                        :: IDIM   !
INTEGER                        :: JI
!
 CHARACTER(LEN=6)    :: YMASK
INTEGER, DIMENSION(NL) :: ISTATUS
REAL, DIMENSION(NL) :: ZDEPTH, ZSTATUS    ! physiographic field on full grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',0,ZHOOK_HANDLE)
ZDEPTH(:) = XUNDEF
ZSTATUS(:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    4.      Averages the field
!             ------------------
!
ALLOCATE(NSIZE     (NL))
ALLOCATE(XTNG      (NL,NGRADDEPTH_LDB))
!
NSIZE  (:) = 0.
XTNG   (:,:) = 0.
!
 CALL TREAT_FIELD(HPROGRAM,'SURF  ','DIRECT','A_LDBD', CLAKELDB,   &
                 'water depth         ',ZDEPTH,'WAT'              ) 
!
DEALLOCATE(XTNG)
ALLOCATE(XTNG      (NL,NGRADSTATUS_LDB))
!
NSIZE  (:) = 0.
XTNG   (:,:) = 0.
!
 CALL TREAT_FIELD(HPROGRAM,'SURF  ','DIRECT','A_LDBS', CSTATUSLDB,  &
                 'water status        ',ZSTATUS,'WAT'              )
!
ISTATUS = NINT(ZSTATUS)
!
DEALLOCATE(NSIZE)
DEALLOCATE(XTNG)
!
!-------------------------------------------------------------------------------
!
!*    5.      Consistancy check
!             ------------------
!
DO JI = 1, SIZE(ZDEPTH)
  IF (XWATER(JI).GT.0.) THEN
    IF (ISTATUS(JI).LE.2) ZDEPTH(JI) = 10.
    IF (ISTATUS(JI)==3.AND.ZDEPTH(JI)==0.) ZDEPTH(JI) = 10.
  ELSE
    ZDEPTH(JI) = 0.
  ENDIF
ENDDO
!
!*    6.      Mask for the field
!             ------------------
!
YMASK='WATER '
 CALL GET_TYPE_DIM_n(YMASK,IDIM)
IF (IDIM/=SIZE(PDEPTH) .OR. IDIM/=SIZE(KSTATUS)) THEN
   WRITE(ILUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PDEPTH),SIZE(KSTATUS)
   CALL ABOR1_SFX('TREAT_GLOBAL_LAKE_DEPTH: WRONG DIMENSION OF MASK')
ENDIF

ALLOCATE(IMASK(IDIM))
ILU=0
 CALL GET_SURF_MASK_n(YMASK,IDIM,IMASK,ILU,ILUOUT)
 CALL PACK_SAME_RANK(IMASK,ZDEPTH(:),PDEPTH(:))
 CALL PACK_SAME_RANK(IMASK,ISTATUS(:),KSTATUS(:))
DEALLOCATE(IMASK)
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH
