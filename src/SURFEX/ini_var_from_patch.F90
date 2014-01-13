!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
      SUBROUTINE INI_VAR_FROM_PATCH(HPROGRAM,KLUOUT,HNAME,PFIELD,KPTS,PDEF)
!!
!!    PURPOSE
!!    -------
!!
!!      (1) KPTS=n  interpol field with n pts
!!      (2) KPTS=0  conserve cells mass  
!!   Case 2 : simple extrapolation based on the inside cell informations.
!!             this is donne before conserving cell or global mass
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
!!    Original    12/2010
!!
!----------------------------------------------------------------------------
!!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_SURF_ATM_n,      ONLY : NSIZE_FULL
USE MODD_ISBA_n,          ONLY : XPATCH_OLD, XPATCH, XLAI
!
USE MODI_GET_SURF_MASK_n
USE MODI_INTERPOL_FIELD
USE MODI_UNPACK_SAME_RANK
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),             INTENT(IN)    :: HPROGRAM  ! host model
INTEGER,                      INTENT(IN   ) :: KLUOUT
INTEGER,                      INTENT(IN   ) :: KPTS
 CHARACTER(LEN=*),             INTENT(IN   ) :: HNAME
REAL, DIMENSION(:,:),         INTENT(INOUT) :: PFIELD
!
REAL, DIMENSION(:  ), OPTIONAL, INTENT(IN) :: PDEF 
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL, DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: GVEG
REAL,    DIMENSION(SIZE(PFIELD,1)) :: ZFIELD1_TOT, ZFIELD2_TOT
INTEGER, DIMENSION(SIZE(PFIELD,1)) :: IMASK  ! mask for packing from complete field to nature field
INTEGER, DIMENSION(SIZE(PFIELD,1)) :: NSIZE
INTEGER, DIMENSION(NSIZE_FULL)     :: NSIZE_TOT
REAL,    DIMENSION(NSIZE_FULL)     :: ZFIELD_TOT
INTEGER                            :: INI, IPATCH, IFULL, INPTS
INTEGER                            :: JPATCH  ! loop counter on patch
REAL                               :: ZRATIO_TOT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! (1) & (2) INTERPOL FILED
!-------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_PATCH',0,ZHOOK_HANDLE)
!
INI=SIZE(PFIELD,1)
IPATCH=SIZE(PFIELD,2)
!
IF (KPTS>0)THEN
  !
  CALL GET_SURF_MASK_n('NATURE',INI,IMASK,NSIZE_FULL,KLUOUT)
  !
  DO JPATCH=1,IPATCH
    NSIZE(:)=0
    WHERE (PFIELD(:,JPATCH).NE.XUNDEF) NSIZE(:)=1
    WHERE (XPATCH(:,JPATCH)==0.) NSIZE(:)=-1
    CALL UNPACK_SAME_RANK(IMASK,NSIZE,NSIZE_TOT,-1)
    CALL UNPACK_SAME_RANK(IMASK,PFIELD(:,JPATCH),ZFIELD_TOT)
    IF(PRESENT(PDEF))THEN
      CALL INTERPOL_FIELD(HPROGRAM,KLUOUT,NSIZE_TOT,ZFIELD_TOT,HNAME,PDEF=PDEF(JPATCH),KNPTS=KPTS)
    ELSE
      CALL INTERPOL_FIELD(HPROGRAM,KLUOUT,NSIZE_TOT,ZFIELD_TOT,HNAME,KNPTS=KPTS)
    ENDIF
    CALL PACK_SAME_RANK(IMASK,ZFIELD_TOT,PFIELD(:,JPATCH))  
  ENDDO
  !
ELSE
!
!-------------------------------------------------------------------------------
! (3) Cell mass conservative + simple interpolation based on global cell
!     informations
!----------------------------
!                 
  !
  ZFIELD1_TOT(:)=0.
  ZFIELD2_TOT(:)=0.
  !
  GVEG(:,:)=.TRUE.
  !
  IF (TRIM(HNAME)=='WR')THEN
    !no interception over soil(1), roc(2) and glaciers(3)
    DO JPATCH=1,IPATCH
      WHERE(XPATCH(:,JPATCH) /=0. .AND. XPATCH_OLD(:,JPATCH) ==0..AND.XLAI(:,JPATCH)==0.)
          PFIELD(:,JPATCH) = 0.  
          GVEG  (:,JPATCH) = .FALSE.
      ENDWHERE
    END DO
  END IF
  !
  !quantity of water before restart in each grid point
  DO JPATCH=1,IPATCH 
    ZFIELD1_TOT(:)=ZFIELD1_TOT(:)+ XPATCH_OLD(:,JPATCH)*PFIELD(:,JPATCH)
  END DO
  !
  DO JPATCH=1,IPATCH
    !if a patch appears in a grid point, it takes the quantity of water in the
    !whole grid point before
    WHERE(XPATCH(:,JPATCH) /=0. .AND. XPATCH_OLD(:,JPATCH)==0. .AND. GVEG  (:,JPATCH))
          PFIELD(:,JPATCH)=ZFIELD1_TOT(:)
    ENDWHERE
    !quantity of water after restart and landuse in each grid point 
    ZFIELD2_TOT(:)=ZFIELD2_TOT(:)+ XPATCH(:,JPATCH)*PFIELD(:,JPATCH)           
  END DO
  !
  ! Conserve cell mass if not WG and WGI
  ! If WG or WGI conserve global mass via CONSERV_GLOBAL_MASS routine
  !    is recomanded 
  !
  IF (TRIM(HNAME)/='WG' .AND. TRIM(HNAME)/='WGI') THEN
    DO JPATCH=1,IPATCH
      WHERE(ZFIELD2_TOT(:) > 1.E-12)
        PFIELD(:,JPATCH)=(ZFIELD1_TOT(:)/ZFIELD2_TOT(:))*PFIELD(:,JPATCH)
      ENDWHERE
    END DO
  ENDIF
  !
  WHERE(XPATCH(:,:) ==0.)PFIELD(:,:)=XUNDEF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_PATCH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_PATCH
