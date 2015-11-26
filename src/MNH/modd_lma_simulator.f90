!     ############################
      MODULE MODD_LMA_SIMULATOR
!     ############################
!
!!****  *MODD_LMA_SIMULATOR* - declaration of LMA network 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define the storage of the
!     LMA simulator
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!	J.-P. Pinty *Laboratoire Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/02/13
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE
!
IMPLICIT NONE
!
!
!* general information
!
LOGICAL, SAVE                          :: LLMA=.FALSE.! Flag to record LMA-like
                                                      ! data along the
                                                      ! simulation
REAL, SAVE                             :: XDTLMA ! Time length of a LMA record
TYPE (DATE_TIME), SAVE                 :: TDTLMA ! Date and Time of LMA file
CHARACTER (LEN=31), SAVE               :: CLMA_FILE   ! File name
INTEGER, SAVE                          :: ILMA_UNIT   ! File information
INTEGER, SAVE                          :: ILMA_IOSTAT ! File information
!
!* storage monitoring
!
INTEGER, DIMENSION(:,:,:), SAVE, ALLOCATABLE :: ISLMA_SEG_GLOB ! Global indexes
                                                           ! of the LMA segments
!
!* data records
!
LOGICAL, SAVE                             :: LWRITE_LMA
REAL, DIMENSION(:,:),   SAVE, ALLOCATABLE :: ZLMA_LAT, ZLMA_LON
REAL, DIMENSION(:,:),   SAVE, ALLOCATABLE :: ZSLMA_NEUT_POS, ZSLMA_NEUT_NEG
REAL, DIMENSION(:,:,:), SAVE, ALLOCATABLE :: ZSLMA_QMT
REAL, DIMENSION(:,:,:), SAVE, ALLOCATABLE :: ZSLMA_PRT
!
!------------------------------------------------------------------------------
!
END MODULE MODD_LMA_SIMULATOR
