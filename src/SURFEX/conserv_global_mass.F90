      SUBROUTINE CONSERV_GLOBAL_MASS(ILUOUT,PZDG,PZDG_OLD,PFIELD,PFIELD_OLD)
!!
!!****  *CONSERV_GLOBAL_MASS* - routine to conserve global 3D mass (LAND USE case)
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
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    R. Alkama        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!    Original    07/2011
!!
!!
!!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_ISBA_GRID_n,     ONLY : XMESH_SIZE  
USE MODD_SURF_ATM_n,      ONLY : XNATURE
USE MODD_SURF_ATM_n,      ONLY : NSIZE_FULL
USE MODD_ISBA_n,          ONLY : XPATCH_OLD, XPATCH
!
USE MODI_PACK_SAME_RANK
USE MODI_GET_SURF_MASK_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                        INTENT(IN   ) :: ILUOUT
REAL, DIMENSION(:,:,:),         INTENT(IN   ) :: PFIELD_OLD,PZDG,PZDG_OLD
REAL, DIMENSION(:,:,:),         INTENT(INOUT) :: PFIELD
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,    DIMENSION(SIZE(PFIELD,1)) :: ZFIELD,ZFIELD_OLD, ZFRAC_NAT
INTEGER, DIMENSION(SIZE(XNATURE))  :: IMASK  ! mask for packing from complete field to nature field
INTEGER                            :: INI, IPATCH, IFULL, ILEV
INTEGER                            :: JLEV, JPATCH, JJ  ! loop counter 
REAL                               :: ZRATIO_TOT, ZWORK1,ZWORK2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONSERV_GLOBAL_MASS',0,ZHOOK_HANDLE)
!
INI   = SIZE(PFIELD,1)
ILEV  = SIZE(PFIELD,2)
IPATCH = SIZE(PFIELD,3)
IFULL = SIZE(XNATURE )
!
ZFRAC_NAT = 1.
 CALL GET_SURF_MASK_n('NATURE',IFULL,IMASK,NSIZE_FULL,ILUOUT)
 CALL PACK_SAME_RANK(IMASK,XNATURE,ZFRAC_NAT)  
ZFRAC_NAT(:)=ZFRAC_NAT(:)*XMESH_SIZE(:)
!
ZFIELD(:)    =0.0
ZFIELD_OLD(:)=0.0
DO JPATCH=1,IPATCH
  DO JLEV=1,ILEV
     DO JJ=1,INI
        ZFIELD(JJ)    = ZFIELD(JJ)     + PFIELD(JJ,JLEV,JPATCH)*PZDG(JJ,JLEV,JPATCH)*XPATCH(JJ,JPATCH)
        ZFIELD_OLD(JJ)= ZFIELD_OLD(JJ) + PFIELD_OLD(JJ,JLEV,JPATCH)*PZDG_OLD(JJ,JLEV,JPATCH)*XPATCH_OLD(JJ,JPATCH)
     ENDDO
  ENDDO
ENDDO
!
ZWORK1=0.0
ZWORK2=0.0
ZRATIO_TOT = 1.0
!
DO JJ=1,INI
   ZWORK1=ZWORK1+ZFIELD    (JJ)*ZFRAC_NAT(JJ)
   ZWORK2=ZWORK1+ZFIELD_OLD(JJ)*ZFRAC_NAT(JJ)
ENDDO
!
IF(ZWORK2/= 0.)THEN
   ZRATIO_TOT = ZWORK1/ZWORK2
ENDIF
!
WHERE(PFIELD(:,:,:)/=XUNDEF) PFIELD(:,:,:)= PFIELD(:,:,:) * ZRATIO_TOT
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONSERV_GLOBAL_MASS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONSERV_GLOBAL_MASS
