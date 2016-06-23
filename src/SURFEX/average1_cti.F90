!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_CTI(KLUOUT,KNBLINES,PLAT,PLON,PVALUE,PNODATA)
!     ################################################
!
!!**** *AVERAGE1_CTI* computes the sum of cti, squared cti
!!                    and subgrid cti characteristics
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
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,       ONLY : XSUMVAL, XSUMVAL2, XSUMVAL3, NSIZE, &
                                  XMAX_WORK, XMIN_WORK   
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
!!
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
INTEGER,                 INTENT(IN)    :: KNBLINES
REAL, DIMENSION(:),      INTENT(IN)    :: PLAT    ! latitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PLON    ! longitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PVALUE  ! value of the point to add
REAL, OPTIONAL, INTENT(IN) :: PNODATA
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain
!
REAL, DIMENSION(SIZE(PLAT)) :: ZVALUE
REAL :: ZNODATA
!
INTEGER :: JLOOP, JOVER        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',0,ZHOOK_HANDLE)
!
IF (PRESENT(PNODATA)) THEN
  ZVALUE(:) = PVALUE(:)
  ZNODATA = PNODATA
  CALL GET_MESH_INDEX(KLUOUT,KNBLINES,PLAT,PLON,IINDEX,ZVALUE,ZNODATA)
ELSE
  ZVALUE(:) = 1.
  ZNODATA = 0.
  CALL GET_MESH_INDEX(KLUOUT,KNBLINES,PLAT,PLON,IINDEX)
ENDIF
!
!*    2.     Loop on all input data points
!            -----------------------------
!     
bloop: &
DO JLOOP = 1 , SIZE(PLAT)
!
  DO JOVER = 1, NOVMX
!
!*    3.     Tests on position
!            -----------------
!
    IF (IINDEX(JOVER,JLOOP)==0) CYCLE bloop
!
!*    4.     Summation
!            ---------
!
    NSIZE(IINDEX(JOVER,JLOOP))=NSIZE(IINDEX(JOVER,JLOOP))+1
!
!*    5.     CTI
!            ---
!
    XSUMVAL(IINDEX(JOVER,JLOOP))=XSUMVAL(IINDEX(JOVER,JLOOP))+PVALUE(JLOOP)
!
!*    6.     Square of CTI
!            -------------
!
    XSUMVAL2(IINDEX(JOVER,JLOOP))=XSUMVAL2(IINDEX(JOVER,JLOOP))+PVALUE(JLOOP)**2
!
!
!*    7.     Cube of CTI
!            -------------
!
    XSUMVAL3(IINDEX(JOVER,JLOOP))=XSUMVAL3(IINDEX(JOVER,JLOOP))+PVALUE(JLOOP)**3
!
!
!*    8.     Maximum CTI in the mesh
!            -----------------------
!
    XMAX_WORK(IINDEX(JOVER,JLOOP))=MAX(XMAX_WORK(IINDEX(JOVER,JLOOP)),PVALUE(JLOOP))
!
!
!*    9.     Minimum CTI in the mesh
!            -----------------------
!
    XMIN_WORK(IINDEX(JOVER,JLOOP))=MIN(XMIN_WORK(IINDEX(JOVER,JLOOP)),PVALUE(JLOOP))
!
!
  ENDDO
!  
ENDDO bloop
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_CTI
