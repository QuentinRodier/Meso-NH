!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_WRITE_BALLOON_n
!     ###########################
!
INTERFACE
!
SUBROUTINE WRITE_BALLOON_n(HFMFILE)
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
END SUBROUTINE WRITE_BALLOON_n
!
END INTERFACE
!
END MODULE MODI_WRITE_BALLOON_n
!
!
!     ###################################
      SUBROUTINE WRITE_BALLOON_n(HFMFILE)
!     ###################################
!
!!****  *WRITE_BALLOON_n* - routine to write balloon records in a LFIFM file
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      FMWRIT     : FM-routine to write a record
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_AIRCRAFT_BALLOON_n : contains balloon and aircraft variables
!!      Module MODD_GRID_n : contains spatial grid variables
!!      Module MODD_LUNIT_n   : contains logical unit variables
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!  	G.Jaubert   *Meteo France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/06/01 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_GRID, ONLY: XLONORI,XLATORI
USE MODD_LUNIT_n
!
USE MODE_GRIDPROJ
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
!*       0.2   Declarations of local variables
!
!
IF (TBALLOON1%FLY) CALL WRITE_LFI_BALLOON(TBALLOON1)
IF (TBALLOON2%FLY) CALL WRITE_LFI_BALLOON(TBALLOON2)
IF (TBALLOON3%FLY) CALL WRITE_LFI_BALLOON(TBALLOON3)
IF (TBALLOON4%FLY) CALL WRITE_LFI_BALLOON(TBALLOON4)
IF (TBALLOON5%FLY) CALL WRITE_LFI_BALLOON(TBALLOON5)
IF (TBALLOON6%FLY) CALL WRITE_LFI_BALLOON(TBALLOON6)
IF (TBALLOON7%FLY) CALL WRITE_LFI_BALLOON(TBALLOON7)
IF (TBALLOON8%FLY) CALL WRITE_LFI_BALLOON(TBALLOON8)
IF (TBALLOON9%FLY) CALL WRITE_LFI_BALLOON(TBALLOON9)
!
!
CONTAINS
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
SUBROUTINE WRITE_LFI_BALLOON(TPFLYER)
!
USE MODE_FMWRIT
!
TYPE(FLYER),        INTENT(IN)       :: TPFLYER
!
!
!*       0.2   Declarations of local variables
!
INTEGER            :: IRESP         ! IRESP  : return-code for fmwrit
INTEGER            :: IGRID         ! IGRID : grid indicator
CHARACTER (LEN=2)  :: YDIR          ! Type of the data field
INTEGER            :: ILENCH        ! ILENCH : length of comment string
CHARACTER(LEN=16)  :: YRECFM        ! Name of the article to be written
CHARACTER(LEN=100) :: YCOMMENT      ! Comment string
!
REAL               :: ZLAT          ! latitude of the balloon
REAL               :: ZLON          ! longitude of the balloon
!
!
CALL SM_LATLON(XLATORI,XLONORI,  &
     TPFLYER%X_CUR,TPFLYER%Y_CUR,ZLAT,ZLON)
!
!
YRECFM=ADJUSTL(ADJUSTR(TPFLYER%TITLE)//'LAT')
YCOMMENT='DEGREES'
IGRID=0
YDIR='--'
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZLAT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM=ADJUSTL(ADJUSTR(TPFLYER%TITLE)//'LON')
YCOMMENT='DEGREES'
IGRID=0
YDIR='--'
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,ZLON,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM=ADJUSTL(ADJUSTR(TPFLYER%TITLE)//'ALT')
YCOMMENT='METERS'
IGRID=0
YDIR='--'
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TPFLYER%Z_CUR,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM=ADJUSTL(ADJUSTR(TPFLYER%TITLE)//'WASCENT')
YCOMMENT='METERS/SECONDE'
IGRID=0
YDIR='--'
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TPFLYER%WASCENT,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM=ADJUSTL(ADJUSTR(TPFLYER%TITLE)//'RHO')
YCOMMENT='KG/M3'
IGRID=0
YDIR='--'
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,YDIR,TPFLYER%RHO,IGRID,ILENCH,YCOMMENT,IRESP)
!
!
!
END SUBROUTINE WRITE_LFI_BALLOON
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_BALLOON_n
