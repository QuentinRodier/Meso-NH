!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_COVER(KLUOUT,KNBLINES,PLAT,PLON,PVALUE,PNODATA)
!     #######################################################
!
!!**** *AVERAGE1_COVER* computes the sum of values of a cover fractions
!!                              and the nature of terrain on the grid
!!                              from a data in land-cover file
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
USE MODD_PGDWORK, ONLY : XSUMCOVER, NSIZE
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
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
INTEGER :: JLOOP, JOVER        ! loop index on input arrays
INTEGER :: ICOVERCLASS  ! class of cover type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
!     
IF (LHOOK) CALL DR_HOOK('AVERAGE1_COVER',0,ZHOOK_HANDLE)
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
!*    3.     Tests on position
!            -----------------
!    
  DO JOVER = 1, NOVMX

    IF (IINDEX(JOVER,JLOOP)==0) CYCLE bloop
!
!*    4.     Test on value meaning
!            ---------------------
!
    ICOVERCLASS = NINT(PVALUE(JLOOP))
!
    IF (ICOVERCLASS<1 .OR. ICOVERCLASS > SIZE(XSUMCOVER,2) )  CYCLE
!
!*    5.     Summation
!            ---------
!
    NSIZE(IINDEX(JOVER,JLOOP))=NSIZE(IINDEX(JOVER,JLOOP))+1
!
!*    6.     Fraction of cover type
!            ----------------------
!
    XSUMCOVER(IINDEX(JOVER,JLOOP),ICOVERCLASS)=XSUMCOVER(IINDEX(JOVER,JLOOP),ICOVERCLASS)+1.
!
  ENDDO
!
END DO bloop
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_COVER
