
!     ########################
      MODULE MODI_READ_ASCP
!     ########################
INTERFACE
      SUBROUTINE READ_ASCP(HFILENAME,KLEV,PTHDF,PRVF)
      
!
CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
INTEGER , INTENT(IN)      :: KLEV
REAL , DIMENSION(:)   , INTENT(OUT)      :: PTHDF
REAL , DIMENSION(:)   , INTENT(OUT)      :: PRVF
!
!
END SUBROUTINE READ_ASCP
END INTERFACE
END MODULE MODI_READ_ASCP
!
!
!     ##############################################################
      SUBROUTINE READ_ASCP(HFILENAME,KLEV,PTHDF,PRVF)
                          
!     ##############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Inspired from read_ascllv : reads  vertical profile of theta and rv 
!!                                on pressure levels (pressure must be from 
!!                                bottom to top)
!! 
!!    AUTHOR
!!    ------
! !       P. Peyrille
!!
!!    MODIFICATION
!!    ------------
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!


!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
INTEGER , INTENT(IN)      :: KLEV
REAL , DIMENSION(:)   , INTENT(OUT)      :: PTHDF
REAL , DIMENSION(:)   , INTENT(OUT)      :: PRVF
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER      :: KUNIT                       ! logical unit
!
INTEGER      :: ILUOUT                     ! output listing
INTEGER :: JK
REAL  :: ZLEV
!----------------------------------------------------------------------------
!
!*    1.      Open the file
!             -------------
!
KUNIT=222
OPEN(KUNIT,FILE=HFILENAME)
!
!*    3.     Reading of a data point
!            -----------------------
!
  DO JK=1,KLEV
  READ(KUNIT,*) ZLEV, PTHDF(JK),PRVF(JK)
  END DO
!
!----------------------------------------------------------------------------

!
!*    8.    Closing of the data file
!           ------------------------
!
!99 CLOSE(KUNIT)
 CLOSE(KUNIT)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ASCP

