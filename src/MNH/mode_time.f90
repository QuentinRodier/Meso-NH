!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 mode 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ####################
      MODULE MODE_TIME
!     ####################
!
!!****  *MODE_TIME* -  module for time routines 
!!
!!    PURPOSE
!!    -------
!       The purpose of this executive module  is to package 
!     the routines SM_PRINT_TIME 
!    
!      
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_TIME : contains definition of types for time variables          
!!                          and time variable for all model
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/07/94 
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TIME
USE MODE_FM
USE MODE_IO_ll
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
CONTAINS
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*       1.   ROUTINE SM_PRINT_TIME
!             ---------------------
!-------------------------------------------------------------------------------
!     #################################################
      SUBROUTINE SM_PRINT_TIME(TPDATETIME,HLUOUT,HTITLE)
!     ################################################
!
!!****  *SM_PRINT_TIME * - routine to print a variable of type DATE_TIME
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to print a variable of type DATE_TIME
!      
!
!!**  METHOD
!!    ------
!!       The logical unit number of output-listing file is retrieved (by FMLOOK)
!!   If a logical unit number have never been attributed to this output-listing
!!   file, a logical unit number is attributed (by FMATTR) and  this file is 
!!   opened  
!!       Then the date and time are printed with or without a title.
!!   If it is an idealized case, no date is printed (only time).
!!   
!!    EXTERNAL
!!    --------
!!      FMLOOK : to retrieve a logical unit number for a file
!!      FMATTR : to associate  a logical unit number to  a file name 
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_TYPE_TIME : contains definition of types for time variables
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                        07/07/94 
!!      updated    V. Ducrocq           23/08/94                   
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE (DATE_TIME),  INTENT(IN)           :: TPDATETIME   ! Date and time variable
CHARACTER (LEN=*), INTENT(IN)           :: HLUOUT      ! Name of output listing
CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: HTITLE      ! Title for Date and time
                                                       ! variable 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IHOUR,IMINUTE
REAL    :: ZSECOND,ZREMAIN
INTEGER :: ILUOUT,IRESP
!-------------------------------------------------------------------------------
!
!*       1.    CONVERT TIME IN HOURS,MINUTES AND SECONDS :
!              ------------------------------------------
!
IHOUR   = INT(TPDATETIME%TIME/3600.)
ZREMAIN = MOD(TPDATETIME%TIME,3600.)
IMINUTE = INT(ZREMAIN/60.)
ZSECOND = MOD(ZREMAIN,60.)
!
!-------------------------------------------------------------------------------
!
!*       2.    PRINT ON OUTPUT-LISTING
!              -----------------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
IF (IRESP /= 0) THEN
  CALL OPEN_ll(UNIT=ILUOUT,FILE=HLUOUT,IOSTAT=IRESP,FORM='FORMATTED',ACTION='WRITE', &
     MODE=GLOBAL)
END IF
IF (PRESENT(HTITLE)) THEN
  IF ((TPDATETIME%TDATE%YEAR < 0).OR.(TPDATETIME%TDATE%MONTH < 0).OR.    &
     (TPDATETIME%TDATE%DAY < 0) ) THEN 
    WRITE(UNIT=ILUOUT,FMT='(1X,A," :",2X,I2.2,"H",I2.2,"M", &
         & F5.2,"S")') HTITLE, IHOUR,IMINUTE,ZSECOND
  ELSE
    WRITE(UNIT=ILUOUT,FMT='(1X,A," :",I4.4,I2.2,I2.2,2X,I2.2,"H",I2.2,"M", &
         & F5.2,"S")') HTITLE, TPDATETIME%TDATE, IHOUR,IMINUTE,ZSECOND
  END IF
ELSE
  IF ((TPDATETIME%TDATE%YEAR < 0).OR.(TPDATETIME%TDATE%MONTH < 0).OR.    &
     (TPDATETIME%TDATE%DAY < 0) ) THEN 
    WRITE(UNIT=ILUOUT,FMT='(1X,2X,I2.2,"H",I2.2,"M", &
         & F5.2,"S")') IHOUR,IMINUTE,ZSECOND  
  ELSE 
    WRITE(UNIT=ILUOUT,FMT='(1X,I4.4,I2.2,I2.2,2X,I2.2,"H",I2.2,"M", &
         & F5.2,"S")') TPDATETIME%TDATE, IHOUR,IMINUTE,ZSECOND  
  END IF
END IF
!-------------------------------------------------------------------------------
!
END SUBROUTINE SM_PRINT_TIME
!-------------------------------------------------------------------------------
!
END MODULE MODE_TIME
