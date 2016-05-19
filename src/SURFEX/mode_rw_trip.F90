!######################
MODULE MODE_RW_TRIP
!######################
!
!!****  *MODE_RW_TRIP*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all routines 
!      used by TRIP for read/write variables.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	B. Decharme       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/04/08
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
  INTERFACE READ_TRIP
      MODULE PROCEDURE READ_TRIP_XY
      MODULE PROCEDURE READ_TRIP_XYZ
  END INTERFACE
!
  INTERFACE WRITE_TRIP
      MODULE PROCEDURE WRITE_TRIP_XY
      MODULE PROCEDURE WRITE_TRIP_XYZ
  END INTERFACE
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
!     ##################################################
      SUBROUTINE READ_TRIP_XY(KLUOUT,HFILE,HVNAME,PREAD)
!     ##################################################
!
!!    PURPOSE
!!    -------
!    
!     Read a XY variable in a netcdf file
!
USE MODE_TRIP_NETCDF
USE MODD_TRIP_n, ONLY : LNCPRINT
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE
 CHARACTER(LEN=*), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN) :: KLUOUT
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PREAD
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YFILE
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
LOGICAL, PARAMETER     :: LRW = .FALSE.
!
INTEGER :: INCID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:READ_TRIP_XY',0,ZHOOK_HANDLE)
YFILE  = HFILE (1:LEN_TRIM(HFILE ))
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
 CALL NCOPEN(KLUOUT,LRW,LNCPRINT,YFILE,INCID)
 CALL NCREAD(KLUOUT,INCID,YVNAME,PREAD,LNCPRINT)
 CALL NCCLOSE(KLUOUT,LNCPRINT,YFILE,INCID)
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:READ_TRIP_XY',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_TRIP_XY
!
!-------------------------------------------------------------------------------
!
!     ##################################################
      SUBROUTINE READ_TRIP_XYZ(KLUOUT,HFILE,HVNAME,PREAD)
!     ##################################################
!
!!    PURPOSE
!!    -------
!    
!     Read a XYZ variable in a netcdf file
!
USE MODE_TRIP_NETCDF
USE MODD_TRIP_n, ONLY : LNCPRINT
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE
 CHARACTER(LEN=*), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN) :: KLUOUT
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PREAD
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YFILE
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
LOGICAL, PARAMETER       :: LRW = .FALSE.
!
INTEGER :: INCID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:READ_TRIP_XYZ',0,ZHOOK_HANDLE)
YFILE  = HFILE (1:LEN_TRIM(HFILE ))
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
 CALL NCOPEN(KLUOUT,LRW,LNCPRINT,YFILE,INCID)
 CALL NCREAD(KLUOUT,INCID,YVNAME,PREAD,LNCPRINT)
 CALL NCCLOSE(KLUOUT,LNCPRINT,YFILE,INCID)
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:READ_TRIP_XYZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_TRIP_XYZ
!
!-------------------------------------------------------------------------------
!
!     ######################################################################
      SUBROUTINE WRITE_TRIP_XY(KLUOUT,HFILE,HVNAME,PMASK,PWRITE,KTNUM,KTVAL)
!     ######################################################################
!
!!    PURPOSE
!!    -------
!    
!     Write a XY variable in HFILE
!
USE MODE_TRIP_NETCDF
USE MODD_TRIP_n,   ONLY : LNCPRINT
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE
 CHARACTER(LEN=*), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN) :: KLUOUT
!
REAL, DIMENSION(:,:), INTENT(IN) :: PMASK
REAL, DIMENSION(:,:), INTENT(IN) :: PWRITE
!
INTEGER, INTENT(IN), OPTIONAL :: KTNUM
INTEGER, INTENT(IN), OPTIONAL :: KTVAL
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YFILE
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
LOGICAL, PARAMETER     :: LRW = .TRUE.
!
REAL, DIMENSION(SIZE(PWRITE,1),SIZE(PWRITE,2)) :: ZWRITE
!
INTEGER :: INCID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:WRITE_TRIP_XY',0,ZHOOK_HANDLE)
YFILE  = HFILE (1:LEN_TRIM(HFILE ))
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
ZWRITE = PWRITE
WHERE(PMASK(:,:)==0.0) ZWRITE(:,:)=XTRIP_UNDEF
!
 CALL NCOPEN(KLUOUT,LRW,LNCPRINT,YFILE,INCID)
IF(PRESENT(KTNUM).AND.PRESENT(KTVAL))THEN
  CALL NCSTORE(KLUOUT,INCID,YVNAME,ZWRITE,LNCPRINT,KTNUM,KTVAL)
ELSE
  CALL NCSTORE(KLUOUT,INCID,YVNAME,ZWRITE,LNCPRINT)
ENDIF
 CALL NCCLOSE(KLUOUT,LNCPRINT,YFILE,INCID)
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:WRITE_TRIP_XY',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TRIP_XY
!
!-------------------------------------------------------------------------------
!
!     #######################################################################
      SUBROUTINE WRITE_TRIP_XYZ(KLUOUT,HFILE,HVNAME,PMASK,PWRITE,KTNUM,KTVAL)
!     #######################################################################
!
!!    PURPOSE
!!    -------
!    
!     Write a XY variable in HFILE
!
USE MODE_TRIP_NETCDF
USE MODD_TRIP_n,   ONLY : LNCPRINT
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE
 CHARACTER(LEN=*), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN) :: KLUOUT
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PMASK
REAL, DIMENSION(:,:,:), INTENT(IN) :: PWRITE
!
INTEGER, INTENT(IN), OPTIONAL :: KTNUM
INTEGER, INTENT(IN), OPTIONAL :: KTVAL
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YFILE
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
LOGICAL, PARAMETER     :: LRW = .TRUE.
LOGICAL, PARAMETER     :: LZW = .TRUE.
!
REAL, DIMENSION(SIZE(PWRITE,1),SIZE(PWRITE,2)) :: ZWRITE
!
INTEGER :: INCID, IZLEN, J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:WRITE_TRIP_XYZ',0,ZHOOK_HANDLE)
IZLEN=SIZE(PWRITE,3)
!
YFILE  = HFILE (1:LEN_TRIM(HFILE ))
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
 CALL NCOPEN(KLUOUT,LRW,LNCPRINT,YFILE,INCID)
DO J=1,IZLEN
   WHERE(PMASK(:,:)>0.0)
         ZWRITE(:,:) = PWRITE(:,:,J)
   ELSEWHERE
         ZWRITE(:,:)=XTRIP_UNDEF
   ENDWHERE
   IF(PRESENT(KTNUM).AND.PRESENT(KTVAL))THEN
     CALL NCSTORE(KLUOUT,INCID,YVNAME,ZWRITE,LNCPRINT,KTNUM,KTVAL,J,LZW)
   ELSE
     CALL NCSTORE(KLUOUT,INCID,YVNAME,ZWRITE,LNCPRINT,KLEVEL=J,OVARZDIM=LZW)
   ENDIF
ENDDO
 CALL NCCLOSE(KLUOUT,LNCPRINT,YFILE,INCID)
!
IF (LHOOK) CALL DR_HOOK('MODE_RW_TRIP:WRITE_TRIP_XYZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TRIP_XYZ
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_RW_TRIP
