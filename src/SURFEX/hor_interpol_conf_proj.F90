!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_CONF_PROJ(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
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
!!    MODIFICATION
!!    ------------
!!
!!    02/04/12 M. Tomasini  Add an index in the second dimension of the ISBA 
!!                          array variables for BILIN interpolation routine to 
!!                          not bug in case 2D (this is not the more beautiful
!!                          method; the BILIN routine should better be adapted)
!!                          Search  ! Ajout MT
!!    10/02/15 M.Moge  using SIZE(PFIELDOUT,1) instead of SIZE(XLAT_OUT)
!-------------------------------------------------------------------------------
!
!
USE MODD_PREP,           ONLY : XLAT_OUT, XLON_OUT, LINTERP
USE MODD_GRID_CONF_PROJ, ONLY : XX, XY, NX, NY, XLAT0, XLON0, XLATORI, &
                                  XLONORI, XRPK, XBETA  
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODI_BILIN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE :: ZX,ZY        !   coordinate of the output field
REAL, DIMENSION(:), ALLOCATABLE :: ZX_DUPLIQUE  ! X coordinate of the output field  ! Ajout MT
REAL, DIMENSION(:), ALLOCATABLE :: ZY_DUPLIQUE  ! Y coordinate of the output field  ! Ajout MT
REAL, DIMENSION(:), ALLOCATABLE :: ZXY_DUPLIQUE ! Y coordinate of the  input field  ! Ajout MT 
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELDIN           ! input field
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELDIN_DUPLIQUE  ! input field               ! Ajout MT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFIELDOUT_DUPLIQUE ! interpolated output field ! Ajout MT
!
INTEGER                           :: INO          ! output number of points
INTEGER                           :: JI,JJ,JL     ! loop index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_DUPLIQUE ! .true. where physical value is needed ! Ajout MT
!-------------------------------------------------------------------------------------
!
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CONF_PROJ',0,ZHOOK_HANDLE)
INO = SIZE(PFIELDOUT,1)
!
ALLOCATE(ZX      (INO))
ALLOCATE(ZY      (INO))
!
IF (NY==1) THEN                         ! Ajout MT
   ALLOCATE(ZXY_DUPLIQUE(2),ZFIELDIN_DUPLIQUE(NX,2,SIZE(PFIELDIN,2))) 
   ALLOCATE(ZX_DUPLIQUE(2*INO),ZY_DUPLIQUE(2*INO),ZFIELDOUT_DUPLIQUE(2*INO,SIZE(PFIELDIN,2)))    
   ALLOCATE(GINTERP_DUPLIQUE(SIZE(ZFIELDOUT_DUPLIQUE,1)))    
END IF
!
!*      2.    Transformation of latitudes/longitudes into metric coordinates of output grid
!
 CALL XY_CONF_PROJ(XLAT0,XLON0,XRPK,XBETA,XLATORI,XLONORI, &
                    ZX,ZY,XLAT_OUT,XLON_OUT          )
!
!*      3.    Put input field on its squared grid
!
ALLOCATE(ZFIELDIN(NX,NY,SIZE(PFIELDIN,2)))
!
DO JJ=1,NY
  DO JI=1,NX
    ZFIELDIN(JI,JJ,:) = PFIELDIN(JI+NX*(JJ-1),:)
  END DO
END DO
!
IF (NY==1) THEN                  ! Ajout MT
   ZFIELDIN_DUPLIQUE(:,1,:)=ZFIELDIN(:,1,:)
   ZFIELDIN_DUPLIQUE(:,2,:)=ZFIELDIN(:,1,:)
   ZXY_DUPLIQUE(1)=XY(1)
   ZXY_DUPLIQUE(2)=XY(1)+10000.
   ZX_DUPLIQUE(1:INO)      =ZX(:)
   ZX_DUPLIQUE(INO+1:2*INO)=ZX(:)
   ZY_DUPLIQUE(1:INO)      =ZY(:)
   ZY_DUPLIQUE(INO+1:2*INO)=ZY(:)+10000.
   GINTERP_DUPLIQUE(1:INO)      =LINTERP(1:INO)
   GINTERP_DUPLIQUE(INO+1:2*INO)=LINTERP(1:INO)
END IF
!
!*      4.    Interpolation with bilinear
!
IF (NY==1) THEN                  ! Ajout MT
   DO JL=1,SIZE(PFIELDIN,2)
       CALL BILIN(KLUOUT,XX,ZXY_DUPLIQUE,ZFIELDIN_DUPLIQUE(:,:,JL), &
             ZX_DUPLIQUE,ZY_DUPLIQUE,ZFIELDOUT_DUPLIQUE(:,JL),GINTERP_DUPLIQUE)

       PFIELDOUT(1:INO,JL)=ZFIELDOUT_DUPLIQUE(1:INO,JL)
   END DO
ELSE
   DO JL=1,SIZE(PFIELDIN,2)
      CALL BILIN(KLUOUT,XX,XY,ZFIELDIN(:,:,JL),ZX,ZY,PFIELDOUT(:,JL),LINTERP)
   END DO
END IF
!
!
!*      5.    Deallocations
!
!
DEALLOCATE(ZX,ZY)
DEALLOCATE(ZFIELDIN)
IF (NY==1) DEALLOCATE(ZXY_DUPLIQUE,ZX_DUPLIQUE,ZY_DUPLIQUE,       &
               ZFIELDIN_DUPLIQUE,ZFIELDOUT_DUPLIQUE,GINTERP_DUPLIQUE) ! Ajout MT
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_CONF_PROJ
