!######################
MODULE MODE_TRIP_NETCDF
!######################
!
!!****  *MODE_TRIP_NETCDF*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all routines 
!      used by TRIP for read/store variables in netcdf.
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
  INTERFACE NCOPEN
      MODULE PROCEDURE NCOPEN
  END INTERFACE
!
  INTERFACE NCCLOSE
      MODULE PROCEDURE NCCLOSE
  END INTERFACE
!
  INTERFACE NCREAD
      MODULE PROCEDURE NCREAD_X
      MODULE PROCEDURE NCREAD_XY
      MODULE PROCEDURE NCREAD_XYZ
  END INTERFACE
!
  INTERFACE NCCREATE
      MODULE PROCEDURE NCCREATE
  END INTERFACE
!
  INTERFACE NCSTORE
      MODULE PROCEDURE NCSTORE
  END INTERFACE
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
!     ######################################################
      SUBROUTINE NCOPEN(KLUOUT,ORW,OVERBOSE,HFILENAME,KNCID)
!     ######################################################
!
!!    PURPOSE
!!    -------
!
!     Open a netcdf file name YFILENAME
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HFILENAME
!
LOGICAL, INTENT(IN)          :: ORW, OVERBOSE
!
INTEGER, INTENT(IN)          :: KLUOUT
!
INTEGER, INTENT(OUT)         :: KNCID
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YFNAME
!
INTEGER :: IC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCOPEN',0,ZHOOK_HANDLE)
YFNAME = HFILENAME(1:LEN_TRIM(HFILENAME))      
!
IF(ORW)THEN
  IC = NF_OPEN(YFNAME,NF_WRITE,KNCID)
ELSE
  IC = NF_OPEN(YFNAME,NF_NOWRITE,KNCID)
ENDIF
!
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCOPEN for TRIP : Error opening file ',HFILENAME(1:LEN_TRIM(HFILENAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSEIF(OVERBOSE)THEN
  WRITE(KLUOUT,*)'NCOPEN for TRIP : Opening file ',HFILENAME(1:LEN_TRIM(HFILENAME))
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCOPEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCOPEN
!
!-------------------------------------------------------------------------------
!
!     ###################################################
      SUBROUTINE NCCLOSE(KLUOUT,OVERBOSE,HFILENAME,KNCID)
!     ###################################################
!
!!    PURPOSE
!!    -------
!    
!     Close a netcdf file
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HFILENAME
!
LOGICAL, INTENT(IN)          :: OVERBOSE
INTEGER, INTENT(IN)          :: KLUOUT
INTEGER, INTENT(IN)          :: KNCID
!
!*      declarations of local variables
!
INTEGER :: IC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCCLOSE',0,ZHOOK_HANDLE)
IC = NF_CLOSE(KNCID)
IF(OVERBOSE)WRITE(KLUOUT,*)'NCCLOSE for TRIP : Close file ',HFILENAME(1:LEN_TRIM(HFILENAME))
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCCLOSE',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCCLOSE
!
!-------------------------------------------------------------------------------
!
!     #########################################################################
      SUBROUTINE NCREAD_X(KLUOUT,KNCID,HVNAME,PVECT,OVERBOSE)
!     #########################################################################
!
!!    PURPOSE
!!    -------
!    
!     Read a XY variable in a netcdf file
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN)          :: KLUOUT
INTEGER, INTENT(IN)          :: KNCID
!
LOGICAL, INTENT(IN)          :: OVERBOSE
!
REAL, DIMENSION(:), INTENT(OUT) :: PVECT
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
INTEGER :: IC, ID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_X',0,ZHOOK_HANDLE)
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
IC = NF_INQ_VARID(KNCID,YVNAME,ID)
!
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCREAD_X for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSE
  IC=NF_GET_VAR_DOUBLE(KNCID,ID,PVECT)
  IF(IC/=NF_NOERR)THEN
    WRITE(KLUOUT,*)'NCREAD_X for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
    WRITE(KLUOUT,*)NF_STRERROR(IC)
    STOP
  ELSEIF(OVERBOSE)THEN
    WRITE(KLUOUT,*)'NCREAD_X for TRIP : Success in reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_X',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCREAD_X
!-------------------------------------------------------------------------------
!
!     #########################################################################
      SUBROUTINE NCREAD_XY(KLUOUT,KNCID,HVNAME,PVECT,OVERBOSE)
!     #########################################################################
!
!!    PURPOSE
!!    -------
!    
!     Read a XY variable in a netcdf file
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN)          :: KLUOUT
INTEGER, INTENT(IN)          :: KNCID
!
LOGICAL, INTENT(IN)          :: OVERBOSE
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PVECT
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
INTEGER :: IC, ID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_XY',0,ZHOOK_HANDLE)
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
IC = NF_INQ_VARID(KNCID,YVNAME,ID)
!
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCREAD_XY for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSE
  IC=NF_GET_VAR_DOUBLE(KNCID,ID,PVECT)
  IF(IC/=NF_NOERR)THEN
    WRITE(KLUOUT,*)'NCREAD_XY for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
    WRITE(KLUOUT,*)NF_STRERROR(IC)
    STOP
  ELSEIF(OVERBOSE)THEN
    WRITE(KLUOUT,*)'NCREAD_XY for TRIP : Success in reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_XY',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCREAD_XY
!-------------------------------------------------------------------------------
!
!     #########################################################################
      SUBROUTINE NCREAD_XYZ(KLUOUT,KNCID,HVNAME,PVECT,OVERBOSE)
!     #########################################################################
!
!!    PURPOSE
!!    -------
!    
!     Read a XYZ variable in a netcdf file
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HVNAME
!
INTEGER, INTENT(IN)          :: KLUOUT
INTEGER, INTENT(IN)          :: KNCID
!
LOGICAL, INTENT(IN)          :: OVERBOSE
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PVECT
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YVNAME
!
INTEGER :: IC, ID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_XYZ',0,ZHOOK_HANDLE)
YVNAME = HVNAME(1:LEN_TRIM(HVNAME))
!
IC = NF_INQ_VARID(KNCID,YVNAME,ID)
!
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCREAD_XYZ for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSE
  IC=NF_GET_VAR_DOUBLE(KNCID,ID,PVECT)
  IF(IC/=NF_NOERR)THEN
    WRITE(KLUOUT,*)'NCREAD_XYZ for TRIP : Error reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
    WRITE(KLUOUT,*)NF_STRERROR(IC)
    STOP
  ELSEIF(OVERBOSE)THEN
    WRITE(KLUOUT,*)'NCREAD_XYZ for TRIP : Success in reading variable ',HVNAME(1:LEN_TRIM(HVNAME))
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCREAD_XYZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCREAD_XYZ
!
!-------------------------------------------------------------------------------
!
!     ########################################################
      SUBROUTINE NCCREATE(KLUOUT,HFILENAME,HTITLE,HTIMEUNIT, &
                            HVNAME,HVLNAME,HUNIT,PLON,PLAT,    &
                            PMISSVAL,OVERBOSE,KNCID,OTIME,     &
                            KZLEN,OVARZDIM)  
!     ########################################################
!
!!    PURPOSE
!!    -------
!    
!     Open a netcdf file name YFILENAME
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HFILENAME, HTITLE, HTIMEUNIT
!
 CHARACTER(LEN=nf_max_name), DIMENSION(:), INTENT(IN) :: HVNAME, HVLNAME, HUNIT  
!
REAL, DIMENSION(:), INTENT(IN) :: PLON
REAL, DIMENSION(:), INTENT(IN) :: PLAT
!
LOGICAL, INTENT(IN)  :: OVERBOSE
!
REAL,    INTENT(IN)  :: PMISSVAL
!
INTEGER, INTENT(IN)  :: KLUOUT
!
INTEGER, INTENT(OUT) :: KNCID
!
LOGICAL, INTENT(IN)  :: OTIME
!
INTEGER,               INTENT(IN), OPTIONAL  :: KZLEN
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL  :: OVARZDIM
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YWORK
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK
!
INTEGER :: ILONDIM, ILATDIM, ILEVDIM, ITIMEDIM
INTEGER :: ILON_ID, ILAT_ID, ILEV_ID, ITIME_ID, VAR_ID
INTEGER :: IC, IWORK, INVAR
INTEGER :: JVAR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
!
!Creation
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCCREATE',0,ZHOOK_HANDLE)
YWORK = HFILENAME(1:LEN_TRIM(HFILENAME))
IC = NF_CREATE(YWORK,NF_CLOBBER,KNCID)
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCCREATE for TRIP : Error create file ',HFILENAME(1:LEN_TRIM(HFILENAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSEIF(OVERBOSE)THEN
    WRITE(KLUOUT,*)'NCCREATE for TRIP : Success in creating file ',HFILENAME(1:LEN_TRIM(HFILENAME))
ENDIF
!
!Attributs
YWORK = HTITLE(1:LEN_TRIM(HTITLE))
IC = NF_PUT_ATT_TEXT(KNCID,NF_GLOBAL,'title',LEN_TRIM(YWORK),YWORK)
YWORK = 'COARDS'
IC = NF_PUT_ATT_TEXT(KNCID,NF_GLOBAL,'Conventions',LEN_TRIM(YWORK),YWORK)
!
!Dimensions
IWORK = SIZE(PLON)
IC = NF_DEF_DIM(KNCID,'longitude',IWORK,ILONDIM)
IWORK = SIZE(PLAT)
IC = NF_DEF_DIM(KNCID,'latitude',IWORK,ILATDIM)
IF(PRESENT(KZLEN)) IC = NF_DEF_DIM(KNCID,'level',KZLEN,ILEVDIM)
IF(OTIME) IC = NF_DEF_DIM(KNCID,'time',NF_UNLIMITED,ITIMEDIM)
!
!Variable attributs
!
IC = NF_DEF_VAR(KNCID,'longitude',NF_DOUBLE,1,ILONDIM,ILON_ID)
IC = NF_DEF_VAR(KNCID,'latitude' ,NF_DOUBLE,1,ILATDIM,ILAT_ID)
YWORK = 'degrees_east'
IC = NF_PUT_ATT_TEXT(KNCID,ILON_ID,'units',LEN_TRIM(YWORK),YWORK)
YWORK = 'degrees_north'
IC = NF_PUT_ATT_TEXT(KNCID,ILAT_ID,'units',LEN_TRIM(YWORK),YWORK)
IF(PRESENT(KZLEN))THEN
  IC = NF_DEF_VAR(KNCID,'level',NF_DOUBLE,1,ILEVDIM,ILEV_ID)
  YWORK = 'level'
  IC = NF_PUT_ATT_TEXT(KNCID,ILEV_ID,'units',LEN_TRIM(YWORK),YWORK)
ENDIF
!
IF(OTIME)THEN
   YWORK = HTIMEUNIT(1:LEN_TRIM(HTIMEUNIT))
   IC = NF_DEF_VAR(KNCID,'time',NF_INT,1,ITIMEDIM,ITIME_ID)
   IC = NF_PUT_ATT_TEXT(KNCID,ITIME_ID,'units',LEN_TRIM(YWORK),YWORK)
ENDIF
!
!Variables parametres
!
INVAR = SIZE(HVNAME)
!
DO JVAR=1,INVAR
   YWORK = HVNAME(JVAR)(1:LEN_TRIM(HVNAME(JVAR)))
   IF(PRESENT(KZLEN))THEN  
     IF(OTIME)THEN
       IF(OVARZDIM(JVAR))THEN
         IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,4,(/ILONDIM,ILATDIM,ILEVDIM,ITIMEDIM/),VAR_ID)
       ELSE
         IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,3,(/ILONDIM,ILATDIM,ITIMEDIM/),VAR_ID)
       ENDIF
     ELSE
       IF(OVARZDIM(JVAR))THEN
         IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,3,(/ILONDIM,ILATDIM,ILEVDIM/),VAR_ID)
       ELSE
         IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,2,(/ILONDIM,ILATDIM/),VAR_ID)
       ENDIF
     ENDIF
   ELSE
     IF(OTIME)THEN
       IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,3,(/ILONDIM,ILATDIM,ITIMEDIM/),VAR_ID)
     ELSE
       IC = NF_DEF_VAR(KNCID,YWORK,NF_DOUBLE,2,(/ILONDIM,ILATDIM/),VAR_ID)
     ENDIF
   ENDIF
   YWORK = HVLNAME(JVAR)(1:LEN_TRIM(HVLNAME(JVAR)))
   IC = NF_PUT_ATT_TEXT(KNCID,VAR_ID,'long_name',LEN_TRIM(YWORK),YWORK)
   YWORK = HUNIT(JVAR)(1:LEN_TRIM(HUNIT(JVAR)))
   IC = NF_PUT_ATT_TEXT(KNCID,VAR_ID,'units',LEN_TRIM(YWORK),YWORK)
   IC = NF_PUT_ATT_DOUBLE(KNCID,VAR_ID,'missing_value',NF_DOUBLE,1,PMISSVAL)
ENDDO
!
IC = NF_ENDDEF(KNCID)
!
!Write dimensions
IC = NF_PUT_VAR_DOUBLE(KNCID,ILON_ID,PLON)
IC = NF_PUT_VAR_DOUBLE(KNCID,ILAT_ID,PLAT)
IF(PRESENT(KZLEN))THEN
  ALLOCATE(ZWORK(KZLEN))
  DO JVAR = 1,KZLEN
     ZWORK(JVAR)=JVAR
  ENDDO
  IC = NF_PUT_VAR_DOUBLE(KNCID,ILEV_ID,ZWORK)
  DEALLOCATE(ZWORK)
ENDIF
!
WRITE(KLUOUT,*)'NCCREATE ',HFILENAME(1:LEN_TRIM(HFILENAME)),' for TRIP OK !'
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCCREATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCCREATE
!
!-------------------------------------------------------------------------------
!
!     ################################################
      SUBROUTINE NCSTORE(KLUOUT,KNCID,HVNAME,PWRITE, &
                           OVERBOSE,KTIMENUM,KTIMEVAL, &
                           KLEVEL,OVARZDIM)  
!     ################################################
!
!!    PURPOSE
!!    -------
!    
!     Write in a netcdf file with illimited time if this this the case
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      declarations of arguments
!
 CHARACTER(LEN=nf_max_name), INTENT(IN) :: HVNAME  
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWRITE
!
INTEGER, INTENT(IN)           :: KLUOUT, KNCID
!
LOGICAL, INTENT(IN)           :: OVERBOSE
!
INTEGER, INTENT(IN), OPTIONAL :: KTIMENUM
INTEGER, INTENT(IN), OPTIONAL :: KTIMEVAL

INTEGER, INTENT(IN), OPTIONAL :: KLEVEL
LOGICAL, INTENT(IN), OPTIONAL :: OVARZDIM
!
!*      declarations of local variables
!
 CHARACTER(LEN=nf_max_name) :: YWORK
!
INTEGER, DIMENSION(4) :: ISTART, ICOUNT
!
INTEGER :: IUNLIMID, ITIMEID, ILENGHT, INDIM
INTEGER :: IC, IVAR_ID
REAL    :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCSTORE',0,ZHOOK_HANDLE)
IF(PRESENT(KLEVEL).AND..NOT.PRESENT(OVARZDIM))THEN
  WRITE(KLUOUT,*)'NCSTORE for TRIP : Error writing variable ',HVNAME(1:LEN_TRIM(HVNAME))
  WRITE(KLUOUT,*)'ILEVEL present but not LVARZDIM'
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ENDIF       
!
IF(PRESENT(KTIMENUM).AND.PRESENT(KTIMEVAL))THEN
  IC = NF_INQ_UNLIMDIM(KNCID,IUNLIMID)
  IF(IUNLIMID/=-1)THEN
    IC = NF_INQ_DIMLEN(KNCID,IUNLIMID,ILENGHT)
    IF(KTIMENUM/=ILENGHT)THEN
      IC = NF_INQ_VARID(KNCID,'time',ITIMEID)
      ZWORK = KTIMEVAL
      IC = NF_PUT_VAR1_DOUBLE(KNCID,ITIMEID,(/KTIMENUM/),ZWORK)
      IF(OVERBOSE)THEN
        WRITE(KLUOUT,*)'NCSTORE : re-writing of time variable number=',&
                          KTIMENUM,' and value=',KTIMEVAL  
      ENDIF
    ENDIF
  ENDIF
ENDIF
!
YWORK = HVNAME(1:LEN_TRIM(HVNAME))
!
IC = NF_INQ_VARID(KNCID,YWORK,IVAR_ID)
IC = NF_INQ_VARNDIMS(KNCID,IVAR_ID,INDIM)
!
ICOUNT(1) = SIZE(PWRITE,1)
ICOUNT(2) = SIZE(PWRITE,2)
ICOUNT(3) = 1
ICOUNT(4) = 1
!  
ISTART(1) = 1
ISTART(2) = 1
!
IF(PRESENT(KLEVEL).AND.OVARZDIM)THEN
  ISTART(3) = KLEVEL
  IF(PRESENT(KTIMENUM).AND.PRESENT(KTIMEVAL))THEN
    ISTART(4) = KTIMENUM
    IC = NF_PUT_VARA_DOUBLE(KNCID,IVAR_ID,ISTART(1:4),ICOUNT(1:4),PWRITE)
  ELSE
    IC = NF_PUT_VARA_DOUBLE(KNCID,IVAR_ID,ISTART(1:3),ICOUNT(1:3),PWRITE)
  ENDIF 
ELSE
  IF(PRESENT(KTIMENUM).AND.PRESENT(KTIMEVAL))THEN
    ISTART(3) = KTIMENUM
    IC = NF_PUT_VARA_DOUBLE(KNCID,IVAR_ID,ISTART(1:3),ICOUNT(1:3),PWRITE)
  ELSE
    IC = NF_PUT_VARA_DOUBLE(KNCID,IVAR_ID,ISTART(1:2),ICOUNT(1:2),PWRITE)
  ENDIF          
ENDIF
!
IF(IC/=NF_NOERR)THEN
  WRITE(KLUOUT,*)'NCSTORE for TRIP : Error writing variable ',HVNAME(1:LEN_TRIM(HVNAME))
  WRITE(KLUOUT,*)NF_STRERROR(IC)
  STOP
ELSEIF(OVERBOSE)THEN
  WRITE(KLUOUT,*)'NCSTORE for TRIP : Success in writing variable ',HVNAME(1:LEN_TRIM(HVNAME))
  IF(PRESENT(KLEVEL))WRITE(KLUOUT,*)'                   level: ',KLEVEL
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_NETCDF:NCSTORE',1,ZHOOK_HANDLE)
!
END SUBROUTINE NCSTORE
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_NETCDF
