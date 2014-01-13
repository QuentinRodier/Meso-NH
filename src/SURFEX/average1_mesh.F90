!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_MESH(KLUOUT,PLAT,PLON,PVALUE)
!     #######################################################
!
!!**** *AVERAGE1_MESH* computes the sum of orography, squared orography
!!                              and subgrid orography characteristics
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
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,       ONLY : XSUMVAL, NSIZE, CATYPE, &
                               NVALNBR, NVALCOUNT, XVALLIST, JPVALMAX
USE MODD_DATA_COVER_PAR,ONLY : XCDREF
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
USE MODI_ABOR1_SFX
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                 INTENT(IN)    :: KLUOUT
REAL, DIMENSION(:),      INTENT(IN)    :: PLAT    ! latitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PLON    ! longitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PVALUE  ! value of the point to add
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain
INTEGER :: JVAL         ! loop counter on encoutered values
INTEGER :: JLOOP        ! loop index on input arrays
REAL    :: ZEPS=1.E-10  ! a small value
LOGICAL :: GFOUND       ! T : Value already found in this grid point
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE1_MESH',0,ZHOOK_HANDLE)
IF (ALLOCATED(XNUM)) DEALLOCATE(XNUM)
ALLOCATE(XNUM(SIZE(PLAT)))
!
XNUM(:)=1
!
DO WHILE(MAXVAL(XNUM).NE.0)
!
  CALL GET_MESH_INDEX(KLUOUT,PLAT,PLON,IINDEX)
!
!*    2.     Loop on all input data points
!            -----------------------------
!     
  DO JLOOP = 1 , SIZE(PLAT)
!
!*    3.     Tests on position
!            -----------------
!     
    IF (IINDEX(JLOOP)==0) CYCLE
!
!*    4.     Summation
!            ---------
!
    NSIZE(IINDEX(JLOOP))=NSIZE(IINDEX(JLOOP))+1
!
!*    5.     Choice of type of summation
!            ---------------------------
!
    SELECT CASE (CATYPE)
      CASE ('ARI')
        XSUMVAL(IINDEX(JLOOP))=XSUMVAL(IINDEX(JLOOP))+   PVALUE(JLOOP)
      CASE ('INV')
        XSUMVAL(IINDEX(JLOOP))=XSUMVAL(IINDEX(JLOOP))+1./PVALUE(JLOOP)
      CASE ('CDN')
        XSUMVAL(IINDEX(JLOOP))=XSUMVAL(IINDEX(JLOOP))+1./(LOG(XCDREF/PVALUE(JLOOP)))**2
      CASE ('MAJ')
        GFOUND=.FALSE.
        DO JVAL=1,NVALNBR(IINDEX(JLOOP))
          IF (ABS( XVALLIST(IINDEX(JLOOP),JVAL) - PVALUE(JLOOP)) < ZEPS) THEN
            NVALCOUNT(IINDEX(JLOOP),JVAL) = NVALCOUNT(IINDEX(JLOOP),JVAL) + 1
            GFOUND=.TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT. GFOUND) THEN
          IF (NVALNBR(IINDEX(JLOOP))==JPVALMAX) &
            CALL ABOR1_SFX('TOO MANY DIFFERENT VALUES TO AGGREGATE WITH THE MAJORITY RULE')
          NVALNBR(IINDEX(JLOOP)) = NVALNBR(IINDEX(JLOOP)) +1
          JVAL = NVALNBR(IINDEX(JLOOP))
          NVALCOUNT(IINDEX(JLOOP),JVAL) = 1
          XVALLIST (IINDEX(JLOOP),JVAL) = PVALUE(JLOOP)
        END IF
    END SELECT
!
  ENDDO
END DO
IF (LHOOK) CALL DR_HOOK('AVERAGE1_MESH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_MESH
