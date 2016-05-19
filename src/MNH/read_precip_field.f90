!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2007/03/22 18:24:54
!-----------------------------------------------------------------
!     #############################
      MODULE MODI_READ_PRECIP_FIELD
!     #############################
!
!
!
INTERFACE
!
      SUBROUTINE READ_PRECIP_FIELD(HINIFILE,HLUOUT,HGETRCT,HGETRRT,HGETRST,HGETRGT,HGETRHT, &
                              PINPRC,PACPRC,PINPRR,PINPRR3D,PEVAP3D,                        &
                              PACPRR,PINPRS,PACPRS,PINPRG,PACPRG,PINPRH,PACPRH )           
!
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*),          INTENT(IN)  :: HINIFILE    ! name of the initial file
CHARACTER (LEN=*),          INTENT(IN)  :: HLUOUT      ! name for output-listing
                                                       ! of nested models
!                    
CHARACTER (LEN=*),          INTENT(IN)  :: HGETRCT, HGETRRT, HGETRST, HGETRGT, HGETRHT
                                                 ! Get indicator RCT,RRT,RST,RGT,RHT
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Droplet instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRC! Droplet accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRR3D! Rain precipitation flux 3D
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D! Rain evaporation flux 3D
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRR! Rain accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRS! Snow accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRG! Graupel accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRH! Hail accumulated precip
!
END SUBROUTINE READ_PRECIP_FIELD
!
END INTERFACE
!
END MODULE MODI_READ_PRECIP_FIELD 
!
!     ################################################################################
      SUBROUTINE READ_PRECIP_FIELD(HINIFILE,HLUOUT,HGETRCT,HGETRRT,HGETRST,HGETRGT,HGETRHT, &
                              PINPRC,PACPRC,PINPRR,PINPRR3D,PEVAP3D,                        &
                              PACPRR,PINPRS,PACPRS,PINPRG,PACPRG,PINPRH,PACPRH )           
!     ################################################################################
!
!!****  *READ_PRECIP_FIELD* - routine to read precipitation surface fields
!!
!!    PURPOSE
!!    -------
!       Initialize precipitation fields by reading their value in an initial
!     MNH file.
!
!!**  METHOD
!!    ------
!!    
!!    
!!
!!    EXTERNAL
!!    --------
!!      FMREAD   : to read data in LFIFM file
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (routine READ_PRECIP_FIELD)
!!
!!    AUTHOR
!!    ------
!!  	J.-P. Pinty     *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       13/06/96 
!!      (J. Viviand)   04/02/97  convert precipitation rates in m/s
!!      (V. Ducrocq)   14/08/98  // remove KIINF,KJINF,KISUP,KJSUP
!!      (JP Pinty)     29/11/02  add C3R5, ICE2, ICE4
!!
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
USE MODE_FM
!
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*),          INTENT(IN)  :: HINIFILE    ! name of the initial file
CHARACTER (LEN=*),          INTENT(IN)  :: HLUOUT      ! name for output-listing
                                                       ! of nested models
!                    
CHARACTER (LEN=*),          INTENT(IN)  :: HGETRCT,HGETRRT, HGETRST, HGETRGT, HGETRHT
                                                 ! Get indicator RRT,RST,RGT,RHT
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Droplet instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRC! Droplet accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRR3D! Rain precipitation flux 3D
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D! Rain evaporation flux 3D
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRR! Rain accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRS! Snow accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRG! Graupel accumulated precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PACPRH! Hail accumulated precip
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(SIZE(PINPRR,1),SIZE(PINPRR,2)) :: Z2D ! 2D array to read  data
REAL, DIMENSION(SIZE(PINPRR3D,1),SIZE(PINPRR3D,2),SIZE(PINPRR3D,3)) :: Z3D ! 3D array to read  data
                                                  ! in initial file 
INTEGER             :: IGRID,ILENCH,IRESP         !   File 
CHARACTER (LEN=16)  :: YRECFM                     ! management
CHARACTER (LEN=100) :: YCOMMENT                   ! variables   
CHARACTER(LEN=2)    :: YDIR
INTEGER             :: ILUOUT                     ! Unit number for prints
!
!-------------------------------------------------------------------------------
!
!*       1..    INITIALIZATION
!              ----------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
YDIR='XY'
!
!-------------------------------------------------------------------------------
!
!*       2..    READ PROGNOSTIC VARIABLES
!              -------------------------
!
IF (SIZE(PINPRC) /= 0 ) THEN
  SELECT CASE(HGETRCT)
  CASE ('READ')
    YRECFM = 'INPRC'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRC(:,:)=Z2D(:,:)/(1000.*3600.)
    YRECFM = 'ACPRC'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PACPRC(:,:)=Z2D(:,:)/(1000.)
  CASE ('INIT')
    PINPRC(:,:) = 0.0
    PACPRC(:,:) = 0.0
  END SELECT
END IF
!
IF (SIZE(PINPRR) /= 0 ) THEN
  SELECT CASE(HGETRRT)
  CASE ('READ')
    YRECFM = 'INPRR'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRR(:,:)=Z2D(:,:)/(1000.*3600.)
    YRECFM = 'INPRR3D'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRR3D(:,:,:)=Z3D(:,:,:)
    YRECFM = 'EVAP3D'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z3D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PEVAP3D(:,:,:)=Z3D(:,:,:)
    YRECFM = 'ACPRR'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PACPRR(:,:)=Z2D(:,:)/(1000.)
  CASE ('INIT')
    PINPRR(:,:) = 0.0
    PINPRR3D(:,:,:) = 0.0
    PEVAP3D(:,:,:) = 0.0
    PACPRR(:,:) = 0.0
  END SELECT
END IF
!
IF (SIZE(PINPRS) /= 0 ) THEN
  SELECT CASE(HGETRST)
  CASE ('READ')
    YRECFM = 'INPRS'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRS(:,:)=Z2D(:,:)/(1000.*3600.)
    YRECFM = 'ACPRS'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PACPRS(:,:)=Z2D(:,:)/(1000.)
  CASE ('INIT')
    PINPRS(:,:) = 0.0
    PACPRS(:,:) = 0.0
  END SELECT
END IF
!
IF (SIZE(PINPRG) /= 0 ) THEN
  SELECT CASE(HGETRGT)
  CASE ('READ')
    YRECFM = 'INPRG'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRG(:,:)=Z2D(:,:)/(1000.*3600.)
    YRECFM = 'ACPRG'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PACPRG(:,:)=Z2D(:,:)/(1000.)
  CASE ('INIT')
    PINPRG(:,:) = 0.0
    PACPRG(:,:) = 0.0
  END SELECT
END IF
!
IF (SIZE(PINPRH) /= 0 ) THEN
  SELECT CASE(HGETRHT)
  CASE ('READ')
    YRECFM = 'INPRH'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PINPRH(:,:)=Z2D(:,:)/(1000.*3600.)
    YRECFM = 'ACPRH'
    CALL FMREAD(HINIFILE,YRECFM,HLUOUT,YDIR,Z2D,IGRID,ILENCH,YCOMMENT,IRESP)
    IF (IRESP == 0) PACPRH(:,:)=Z2D(:,:)/(1000.)
  CASE ('INIT')
    PINPRH(:,:) = 0.0
    PACPRH(:,:) = 0.0
  END SELECT
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PRECIP_FIELD
