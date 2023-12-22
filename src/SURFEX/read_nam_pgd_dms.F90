!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_DMS(HPROGRAM, KDMS_NBR, HDMS_NAME, HDMS_AREA, &
                                    HDMS_ATYPE, HDMS_FILE, HDMS_FILETYPE      )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_DMS* reads namelist NAM_DMS_PGD
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
!!      P. Tulet *LAERO*
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2021
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
USE MODD_SURF_PAR, ONLY: NFILENAMELGTMAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)   :: HPROGRAM     ! Type of program
INTEGER,                            INTENT(OUT)  :: KDMS_NBR
!                          ! number of megan pgd fields chosen by user
 CHARACTER(LEN=20), DIMENSION(1000), INTENT(OUT)  :: HDMS_NAME
!                          ! name of the megan pgd fields (for information)
 CHARACTER(LEN=3),  DIMENSION(1000), INTENT(OUT)  :: HDMS_AREA
!                          ! areas where megan pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
 CHARACTER(LEN=3),  DIMENSION(1000), INTENT(OUT)  :: HDMS_ATYPE    ! avg type for megan pgd fields
!                                                                   ! 'ARI' , 'INV'
 CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(1000), INTENT(OUT)  :: HDMS_FILE     ! data files
 CHARACTER(LEN=6),  DIMENSION(1000), INTENT(OUT)  :: HDMS_FILETYPE ! type of these files
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                             :: NDMS_NBR
!                          ! number of megan pgd fields chosen by user
 CHARACTER(LEN=20), DIMENSION(1000)  :: CDMS_NAME
!                          ! name of the megan pgd fields (for information)
 CHARACTER(LEN=3),  DIMENSION(1000)  :: CDMS_AREA
!                          ! areas where megan pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
 CHARACTER(LEN=3),  DIMENSION(1000)  :: CDMS_ATYPE    ! avg type for megan pgd fields
!                                                      ! 'ARI' , 'INV'
 CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(1000)  :: CDMS_FILE     ! data files
 CHARACTER(LEN=6),               DIMENSION(1000)  :: CDMS_FILETYPE ! type of these files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DMS_PGD/ NDMS_NBR, CDMS_NAME, CDMS_AREA,       &
                          CDMS_ATYPE, CDMS_FILE, CDMS_FILETYPE  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_DMS',0,ZHOOK_HANDLE)
NDMS_NBR = 0
!
CDMS_NAME     = "                    "
CDMS_FILE     = ""
CDMS_FILETYPE = "      "
CDMS_AREA     = "ALL"
CDMS_ATYPE    = "ARI"
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DMS_PGD',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DMS_PGD)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Fills output arguments
!             ----------------------
!
KDMS_NBR         = NDMS_NBR
HDMS_NAME(:)     = CDMS_NAME(:)
HDMS_AREA(:)     = CDMS_AREA(:)
HDMS_ATYPE(:)    = CDMS_ATYPE(:)
HDMS_FILE(:)     = CDMS_FILE(:)
HDMS_FILETYPE(:) = CDMS_FILETYPE(:)
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_DMS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_DMS
