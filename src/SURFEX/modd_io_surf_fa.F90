!     ##################
      MODULE MODD_IO_SURF_FA
!     ##################
!
!!****  *MODD_IO_SURF_FA - 
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
 CHARACTER(LEN=28),SAVE  :: CFILEIN_FA       ='SURFIN.fa'  ! Name of the input
 CHARACTER(LEN=28),SAVE  :: CFILEIN_FA_SAVE  ='SURFIN.fa'  ! Name of the input
 CHARACTER(LEN=28),SAVE  :: CFILEOUT_FA      ='SURFOUT.fa' ! Name of the input
 CHARACTER(LEN=28),SAVE  :: CFILEPGD_FA      ='PGD.fa'     ! Name of the pgd file
INTEGER                 :: NUNIT_FA         =19           ! logical unit of surface file (FA part)
INTEGER                 :: NLUOUT         ! logical unit of output file
INTEGER                 :: IVERBFA=0      ! amount of message from FA lib 
!
LOGICAL, SAVE           :: LFANOCOMPACT=.FALSE. 
LOGICAL, SAVE           :: LPREP       =.FALSE.
!      
INTEGER, DIMENSION(:), POINTER :: NMASK     ! 1D mask to read only interesting
!$OMP THREADPRIVATE(NMASK)
 CHARACTER(LEN=6)               :: CMASK     ! surface mask type
!$OMP THREADPRIVATE(CMASK)
INTEGER                        :: NFULL     ! total number of points of surface
!$OMP THREADPRIVATE(NFULL)
INTEGER                        :: NFULL_EXT ! total number of points including the extension zone (ALADIN)
INTEGER                        :: NDGL,NDLON,NDLUX,NDGUX        ! dimensions of ALADIN domain
REAL                           :: PERPK,PEBETA,PELON0,PELAT0,  &! grid projection parameters
                                    PEDELX,PEDELY,PELON1,PELAT1   ! for the ALADIN domain  
!
LOGICAL                 :: LOPEN   ! flag to know if the file has been openned during the surface call
 CHARACTER(LEN=6), SAVE  :: CDNOMC ='header'     ! Name of the header
!
END MODULE MODD_IO_SURF_FA
