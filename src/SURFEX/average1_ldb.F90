!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_LDB(KLUOUT,PLAT,PLON,PVALUE,HTYPE)
!     #######################################################
!
!!**** *AVERAGE1_LDB* 
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
!!    S. Faroux         Meteo-France
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
USE MODD_PGDWORK, ONLY : XTNG, NSIZE
USE MODD_DATA_LAKE, ONLY : XBOUNDGRADDEPTH_LDB, XBOUNDGRADSTATUS_LDB
!
USE MODD_POINT_OVERLAY
!
USE MODI_GET_MESH_INDEX
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=1),        INTENT(IN)    :: HTYPE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZBOUND
!
INTEGER, DIMENSION(SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain                              
!
REAL    :: ZCUT
INTEGER :: JLOOP, JGRAD        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
!     
IF (LHOOK) CALL DR_HOOK('AVERAGE1_LDB',0,ZHOOK_HANDLE)
!
SELECT CASE (HTYPE)
!
  CASE('D')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADDEPTH_LDB)))
    ZBOUND(:) = XBOUNDGRADDEPTH_LDB(:)
!
  CASE('S')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADSTATUS_LDB)))
    ZBOUND(:) = XBOUNDGRADSTATUS_LDB(:)
!
  CASE DEFAULT
    CALL ABOR1_SFX("AVERAGE1_LDB: HTYPE NOT SUPPORTED")
!
END SELECT
!
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
!*    4.     Test on value meaning
!            ---------------------
!
    ZCUT = PVALUE(JLOOP)
!
    DO JGRAD = 1, SIZE(ZBOUND)-1
      IF (ZCUT.GT.ZBOUND(JGRAD) .AND. ZCUT.LE.ZBOUND(JGRAD+1)) THEN
        XTNG(IINDEX(JLOOP),JGRAD) = XTNG(IINDEX(JLOOP),JGRAD) + 1
        EXIT
      ENDIF
    ENDDO
!
!*    5.     Summation
!            ---------
!
    NSIZE(IINDEX(JLOOP))=NSIZE(IINDEX(JLOOP))+1
!
  END DO
ENDDO
!
DEALLOCATE(ZBOUND)
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_LDB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_LDB
