MODULE MODE_READ_NETCDF_MERCATOR
!!!=============================================================================
!-------------------------------------------------------------------------------
!
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE HANDLE_ERR_MER(status,line)
!     ####################
IMPLICIT NONE
INTEGER, INTENT(IN)           :: status
 CHARACTER(LEN=80), INTENT(IN) :: line
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:HANDLE_ERR_MER',0,ZHOOK_HANDLE)
IF (status /= NF_NOERR) THEN
  CALL ABOR1_SFX('MODE_READ_NETCDF_MERCATOR: HANDLE_ERR_MER')
END IF
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:HANDLE_ERR_MER',1,ZHOOK_HANDLE)
END SUBROUTINE HANDLE_ERR_MER
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET1DCDF(KCDF_ID,IDVAR,PMISSVALUE,PVALU1D)
!     ####################
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: KCDF_ID !netcdf file identifiant
INTEGER,INTENT(IN) :: IDVAR   !variable to read identifiant
REAL, INTENT(OUT) ::  PMISSVALUE !undefined value
REAL,DIMENSION(:),INTENT(OUT) :: PVALU1D !value array
!
integer :: status
character(len=80) :: HACTION
integer,save :: NDIMS=1
integer :: KVARTYPE
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
integer :: JLOOP
integer :: NGATTS   
character(len=80),DIMENSION(:),ALLOCATABLE :: HNAME
REAL,DIMENSION(:),ALLOCATABLE :: ZVALU1D !value array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET1DCDF',0,ZHOOK_HANDLE)
PMISSVALUE=-9999.9
ALLOCATE(NVARDIMID (NDIMS))
ALLOCATE(NVARDIMLEN(NDIMS))
ALLOCATE(NVARDIMNAM(NDIMS))
NVARDIMID (:)=0
NVARDIMLEN(:)=0
NVARDIMNAM(:)=' '
!
HACTION='get variable type'
status=nf_inq_vartype(KCDF_ID,IDVAR,KVARTYPE)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable type = ',KVARTYPE
!
HACTION='get variable dimensions name'
status=nf_inq_dimname(KCDF_ID,IDVAR,NVARDIMNAM(NDIMS))
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
HACTION='get variable dimensions length'
status=nf_inq_dimlen(KCDF_ID,IDVAR,NVARDIMLEN(NDIMS))
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimension ',NDIMS,' named ',NVARDIMNAM(NDIMS),&
!     &'has a length of',NVARDIMLEN(NDIMS)
!!
HACTION='get attributs'
status=nf_inq_varnatts(KCDF_ID,IDVAR,NGATTS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'number of attributes = ',NGATTS
allocate(hname(1:NGATTS))
!
ALLOCATE(ZVALU1D(1:NVARDIMLEN(NDIMS)))
ZVALU1D=0.
!
IF (KVARTYPE>=5) then
  HACTION='get variable values (1D)'
  status=nf_get_var_double(KCDF_ID,IDVAR,ZVALU1D(:))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
ENDIF
!
PVALU1D(:)=ZVALU1D(:)
!
IF (ALLOCATED(ZVALU1D  ))  DEALLOCATE(ZVALU1D)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET1DCDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET1DCDF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET2DCDF(KCDF_ID,IDVAR,PDIM1,HDIM1NAME,PDIM2,HDIM2NAME,&
             PMISSVALUE,PVALU2D)  
!     ####################
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: KCDF_ID !netcdf file identifiant
INTEGER,INTENT(IN) :: IDVAR   !variable to read identifiant
REAL,DIMENSION(:),INTENT(OUT) :: PDIM1,PDIM2 !dimensions for PVALU2D array
 CHARACTER(len=80),INTENT(OUT) :: HDIM1NAME,HDIM2NAME     !dimensions names
REAL, INTENT(OUT) :: PMISSVALUE
REAL,DIMENSION(:,:),INTENT(OUT) :: PVALU2D !value array
!
integer :: status
character(len=80) :: HACTION
integer,save :: NDIMS=2
integer :: KVARTYPE
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
integer :: JLOOP2, JLOOP, J1, J2
integer :: NGATTS   
character(len=80),DIMENSION(:),ALLOCATABLE :: HNAME
real :: ZMISS1,ZMISS2
real :: ZSCFA, ZOFFS
REAL,DIMENSION(:,:),ALLOCATABLE :: ZVALU2D !value array
INTEGER,DIMENSION(:,:),ALLOCATABLE :: IVALU2D
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET2DCDF',0,ZHOOK_HANDLE)
PMISSVALUE=-9999.9 
ALLOCATE(NVARDIMID (NDIMS))
ALLOCATE(NVARDIMLEN(NDIMS))
ALLOCATE(NVARDIMNAM(NDIMS))
NVARDIMID (:)=0
NVARDIMLEN(:)=0
NVARDIMNAM(:)=' '
!
HACTION='get variable type'
status=nf_inq_vartype(KCDF_ID,IDVAR,KVARTYPE)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable type = ',KVARTYPE
!
HACTION='get variable dimensions identifiant'
status=nf_inq_vardimid(KCDF_ID,IDVAR,NVARDIMID)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
HACTION='get attributs'
status=nf_inq_varnatts(KCDF_ID,IDVAR,NGATTS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'number of attributes = ',NGATTS
allocate(hname(1:NGATTS))
!
ZSCFA=1.
ZOFFS=0.
DO JLOOP=1,NGATTS
  status=nf_inq_attname(KCDF_ID,IDVAR,JLOOP,hname(JLOOP))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'attributes names = ', hname(JLOOP)
  if (TRIM(hname(JLOOP))=='missing_value') then
    !write(0,*) 'missing value search '
    HACTION='get missing value'
    status=nf_get_att_double(KCDF_ID,IDVAR,"missing_value",PMISSVALUE)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  else
    if (TRIM(hname(JLOOP))=='_FillValue') then
      !write(0,*) 'missing value found '
      HACTION='get _FillValue'
      status=nf_get_att_double(KCDF_ID,IDVAR,"_FillValue",PMISSVALUE)
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
      !write(0,*) 'missing value = ',PMISSVALUE
    endif    
  endif
  if (TRIM(hname(JLOOP))=='scale_factor') then
    !write(0,*) 'missing value found '
    HACTION='get scale factor'
    status=nf_get_att_double(KCDF_ID,IDVAR,"scale_factor",ZSCFA)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif   
  if (TRIM(hname(JLOOP))=='add_offset') then
    !write(0,*) 'missing value found '
    HACTION='get offset'
    status=nf_get_att_double(KCDF_ID,IDVAR,"add_offset",ZOFFS)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif    
ENDDO
!
!
DO JLOOP2=1,NDIMS
  HACTION='get variable dimensions name'
  status=nf_inq_dimname(KCDF_ID,NVARDIMID(JLOOP2),NVARDIMNAM(JLOOP2))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  HACTION='get variable dimensions length'
  status=nf_inq_dimlen(KCDF_ID,NVARDIMID(JLOOP2),NVARDIMLEN(JLOOP2))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'variable dimension ',JLOOP2,' named ',NVARDIMNAM(JLOOP2),&
  !     &'has a length of',NVARDIMLEN(JLOOP2)
ENDDO
! 
IF (KVARTYPE>=5) then
  ALLOCATE(ZVALU2D(1:NVARDIMLEN(1),1:NVARDIMLEN(2)))
  ZVALU2D=0.        
  HACTION='get variable values (2D)'
  status=nf_get_var_double(KCDF_ID,IDVAR,ZVALU2D(:,:))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
ELSE
  ALLOCATE(IVALU2D(1:NVARDIMLEN(1),1:NVARDIMLEN(2)))
  IVALU2D=0.         
  HACTION='get variable values (2D)'
  status=nf_get_var_int(KCDF_ID,IDVAR,IVALU2D(:,:))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)  
ENDIF
PVALU2D(:,:)=XUNDEF
DO J1=1,NVARDIMLEN(1)
  DO J2=1,NVARDIMLEN(2)
    IF (KVARTYPE>=5) THEN
      IF (ZVALU2D(J1,J2)/=PMISSVALUE) PVALU2D(J1,J2)=ZVALU2D(J1,J2)*ZSCFA+ZOFFS
    ELSE
      IF (ZVALU2D(J1,J2)/=PMISSVALUE) PVALU2D(J1,J2)=IVALU2D(J1,J2)*ZSCFA+ZOFFS
    ENDIF
  ENDDO
ENDDO
!
 CALL GET1DCDF(KCDF_ID,NVARDIMID(1),ZMISS1,PDIM1)
 CALL GET1DCDF(KCDF_ID,NVARDIMID(2),ZMISS2,PDIM2)
HDIM1NAME=NVARDIMNAM(1)
HDIM2NAME=NVARDIMNAM(2)
IF (ALLOCATED(ZVALU2D  ))  DEALLOCATE(ZVALU2D)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET2DCDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET2DCDF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET3DCDF(KCDF_ID,IDVAR,PDIM1,HDIM1NAME,PDIM2,HDIM2NAME,&
             PDIM3,HDIM3NAME,PMISSVALUE,PVALU3D)  
!     ####################
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: KCDF_ID !netcdf file identifiant
INTEGER,INTENT(IN) :: IDVAR   !variable to read identifiant
REAL,DIMENSION(:),INTENT(OUT) :: PDIM1,PDIM2,PDIM3 !dimensions for PVALU2D array
 CHARACTER(len=80),INTENT(OUT) :: HDIM1NAME,HDIM2NAME,HDIM3NAME    !dimensions names
REAL, INTENT(OUT) :: PMISSVALUE
REAL,DIMENSION(:,:,:),INTENT(OUT) :: PVALU3D !value array
!
integer :: status
character(len=80) :: HACTION
integer,save :: NDIMS=3
integer :: KVARTYPE
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
integer :: JLOOP2, JLOOP
integer :: J1,J2,J3
integer :: NGATTS   
character(len=80),DIMENSION(:),ALLOCATABLE :: HNAME
real :: ZMISS1,ZMISS2,ZMISS3
real :: ZSCFA, ZOFFS
REAL,DIMENSION(:,:,:),ALLOCATABLE :: ZVALU3D !value array
INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: IVALU3D
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET3DCDF',0,ZHOOK_HANDLE)
PMISSVALUE=-9999.9 
ALLOCATE(NVARDIMID (NDIMS))
ALLOCATE(NVARDIMLEN(NDIMS))
ALLOCATE(NVARDIMNAM(NDIMS))
NVARDIMID (:)=0
NVARDIMLEN(:)=0
NVARDIMNAM(:)=' '
!
HACTION='get variable type'
status=nf_inq_vartype(KCDF_ID,IDVAR,KVARTYPE)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable type = ',KVARTYPE
!
HACTION='get variable dimensions identifiant'
status=nf_inq_vardimid(KCDF_ID,IDVAR,NVARDIMID)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimension identifiant ',NVARDIMID
!
HACTION='get attributs'
status=nf_inq_varnatts(KCDF_ID,IDVAR,NGATTS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'number of attributes = ',NGATTS
allocate(hname(1:NGATTS))
!
ZSCFA=1.
ZOFFS=0.
DO JLOOP=1,NGATTS
  status=nf_inq_attname(KCDF_ID,IDVAR,JLOOP,hname(JLOOP))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'attributes names = ', hname(JLOOP)
  if (TRIM(hname(JLOOP))=='missing_value') then
    !write(0,*) 'missing value found '
    HACTION='get missing value'
    status=nf_get_att_double(KCDF_ID,IDVAR,"missing_value",PMISSVALUE)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  else
    if (TRIM(hname(JLOOP))=='_FillValue') then
      !write(0,*) 'missing value found '
      HACTION='get _FillValue'
      status=nf_get_att_double(KCDF_ID,IDVAR,"_FillValue",PMISSVALUE)
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
      !write(0,*) 'missing value = ',PMISSVALUE
    endif
  endif
  if (TRIM(hname(JLOOP))=='scale_factor') then
    !write(0,*) 'missing value found '
    HACTION='get scale factor'
    status=nf_get_att_double(KCDF_ID,IDVAR,"scale_factor",ZSCFA)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif  
  if (TRIM(hname(JLOOP))=='add_offset') then
    !write(0,*) 'missing value found '
    HACTION='get offset'
    status=nf_get_att_double(KCDF_ID,IDVAR,"add_offset",ZOFFS)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif 
ENDDO
!
!
DO JLOOP2=1,NDIMS
  HACTION='get variable dimensions name'
  status=nf_inq_dimname(KCDF_ID,NVARDIMID(JLOOP2),NVARDIMNAM(JLOOP2))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  HACTION='get variable dimensions length'
  status=nf_inq_dimlen(KCDF_ID,NVARDIMID(JLOOP2),NVARDIMLEN(JLOOP2))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'variable dimension ',JLOOP2,' named ',NVARDIMNAM(JLOOP2),&
  !     &'has a length of',NVARDIMLEN(JLOOP2)
ENDDO
! 
IF (KVARTYPE>=5) then
  ALLOCATE(ZVALU3D(1:NVARDIMLEN(1),1:NVARDIMLEN(2),1:NVARDIMLEN(3)))
  ZVALU3D=0.        
  HACTION='get variable values (3D)'
  status=nf_get_var_double(KCDF_ID,IDVAR,ZVALU3D(:,:,:))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
ELSE
  ALLOCATE(IVALU3D(1:NVARDIMLEN(1),1:NVARDIMLEN(2),1:NVARDIMLEN(3)))
  IVALU3D=0.         
  HACTION='get variable values (3D)'
  status=nf_get_var_int(KCDF_ID,IDVAR,IVALU3D(:,:,:))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
ENDIF
!
PVALU3D(:,:,:)=XUNDEF
DO J1=1,NVARDIMLEN(1)
  DO J2=1,NVARDIMLEN(2)
    DO J3=1,NVARDIMLEN(3)
      IF (KVARTYPE>=5) THEN
        IF (ZVALU3D(J1,J2,J3)/=PMISSVALUE) PVALU3D(J1,J2,J3)=ZVALU3D(J1,J2,J3)*ZSCFA+ZOFFS
      ELSE
        IF (IVALU3D(J1,J2,J3)/=PMISSVALUE) PVALU3D(J1,j2,J3)=IVALU3D(J1,J2,J3)*ZSCFA+ZOFFS
      ENDIF
    ENDDO
  ENDDO
ENDDO
!
 CALL GET1DCDF(KCDF_ID,NVARDIMID(1),ZMISS1,PDIM1)
 CALL GET1DCDF(KCDF_ID,NVARDIMID(2),ZMISS2,PDIM2)
 CALL GET1DCDF(KCDF_ID,NVARDIMID(3),ZMISS3,PDIM3)
HDIM1NAME=NVARDIMNAM(1)
HDIM2NAME=NVARDIMNAM(2)
HDIM3NAME=NVARDIMNAM(3)
IF (ALLOCATED(ZVALU3D  ))  DEALLOCATE(ZVALU3D)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:GET3DCDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET3DCDF
!--------------------------------------------------------------------
!-------------------------------------------------------------------
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_DIM_CDF(HFILENAME,HNCVARNAME,KDIM)
!     ####################
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
INTEGER,           INTENT(OUT):: KDIM        ! value of dimension to get
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer ::JLOOP1,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer,DIMENSION(2) ::NLEN2D
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
!*    1.      Open the netcdf file 
!             --------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_DIM_CDF',0,ZHOOK_HANDLE)
HACTION='open netcdf'
status=NF_OPEN(HFILENAME,nf_nowrite,kcdf_id)
!write(0,*) 'identifiant de ',HFILENAME,'=',kcdf_id
if (status/=NF_NOERR) then 
  CALL HANDLE_ERR_MER(status,HACTION)
!else
!  write(0,*) 'netcdf file opened: ',HFILENAME
endif
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF_INQ_NVARS(kcdf_id,NBVARS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF_INQ_VARNAME(kcdf_id,JLOOP1,VARNAME(JLOOP1))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf_close(kcdf_id)
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  CALL ABOR1_SFX('MODE_READ_NETCDF_MERCATOR: READ_DIM_CDF')
endif
!-----------
!
!*    4.      get the total dimension of HNCVARNAME 
!             -------------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf_inq_varndims(kcdf_id,ID_VARTOGET,NVARDIMS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimensions number = ',NVARDIMS
!
!     4.2      get the variable dimensions length
!              ----------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D
  CASE (1) 
    HACTION='get variable dimensions length'
    status=nf_inq_dimlen(kcdf_id,NVARDIMS,KDIM)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
!CAS 2D
  CASE (2)
    KDIM=1
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf_inq_dimlen(kcdf_id,JLOOP,NLEN2D(JLOOP))
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
      KDIM=KDIM*NLEN2D(JLOOP)
    ENDDO
END SELECT
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
status=nf_close(kcdf_id)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_DIM_CDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_DIM_CDF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
       SUBROUTINE PREP_NETCDF_GRID(HFILENAME,HNCVARNAME)
!     ####################
!
USE MODD_GRID_LATLONREGUL
USE MODD_SURF_PAR
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID
integer ::JLOOP1,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer,DIMENSION(3) ::NDIMLEN
character(LEN=80),DIMENSION(3) :: NDIMNAM
integer :: IDIM
integer :: INLON
real :: ZZLAMISS,ZZLOMISS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:PREP_NETCDF_GRID',0,ZHOOK_HANDLE)
NINLAT  =-NUNDEF
NINDEPTH=-NUNDEF
NILENGTH=-NUNDEF
!
XILAT1=XUNDEF
XILON1=XUNDEF
XILAT2=XUNDEF
XILON2=XUNDEF
!*    1.      Open the netcdf file 
!             --------------------
HACTION='open netcdf'
status=NF_OPEN(HFILENAME,nf_nowrite,kcdf_id)
!write(0,*) 'identifiant de ',HFILENAME,'=',kcdf_id
if (status/=NF_NOERR) then 
  CALL HANDLE_ERR_MER(status,HACTION)
!else
!  write(0,*) 'netcdf file opened: ',HFILENAME
endif
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF_INQ_NVARS(kcdf_id,NBVARS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF_INQ_VARNAME(kcdf_id,JLOOP1,VARNAME(JLOOP1))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf_close(kcdf_id)
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:PREP_NETCDF_GRID',1,ZHOOK_HANDLE)
  RETURN
endif
NILENGTH=0
!-----------
!
!*    4.      get the total dimension of HNCVARNAME 
!             -------------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf_inq_varndims(kcdf_id,ID_VARTOGET,NVARDIMS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimensions number = ',NVARDIMS
ALLOCATE(NVARDIMID(NVARDIMS))
HACTION='get variable dimensions identifiant'
status=nf_inq_vardimid(kcdf_id,ID_VARTOGET,NVARDIMID)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
!     4.2      get the variable dimensions length
!              ----------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D
  CASE (1) 
    HACTION='get variable dimensions length'
    status=nf_inq_dimlen(kcdf_id,NVARDIMS,IDIM)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
!CAS 2D,3D
  CASE (2,3)
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf_inq_dimlen(kcdf_id,NVARDIMID(JLOOP),NDIMLEN(JLOOP))
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
      HACTION='get variable dimensions names'
      status=nf_inq_dimname(kcdf_id,NVARDIMID(JLOOP),NDIMNAM(JLOOP))
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
      if ((NDIMNAM(JLOOP)=='lat').OR.(NDIMNAM(JLOOP)=='latitude')) then
        NINLAT=NDIMLEN(JLOOP)
        if (.not.allocated(XILATARRAY)) allocate(XILATARRAY(NDIMLEN(JLOOP)))
        if (.not.allocated(NINLON)) allocate(NINLON(NINLAT))
        CALL GET1DCDF(kcdf_id,NVARDIMID(JLOOP),ZZLAMISS,XILATARRAY(:))
      endif
      if ((NDIMNAM(JLOOP)=='lon').OR.(NDIMNAM(JLOOP)=='longitude')) then
        INLON=NDIMLEN(JLOOP)
        if (.not.allocated(XILONARRAY)) allocate(XILONARRAY(NDIMLEN(JLOOP)))
        CALL GET1DCDF(kcdf_id,NVARDIMID(JLOOP),ZZLOMISS,XILONARRAY(:))
      endif
      if (NDIMNAM(JLOOP)=='depth') NINDEPTH=NDIMLEN(JLOOP)
    ENDDO
    NINLON(:)=INLON
END SELECT
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
status=nf_close(kcdf_id)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!GRID PARAM FOR HORIBL_SURF
DO JLOOP1=1,NINLAT
  NILENGTH = NILENGTH + NINLON(JLOOP1)
ENDDO
XILAT1=XILATARRAY(1)
XILON1=XILONARRAY(1)
XILAT2=XILATARRAY(SIZE(XILATARRAY))
XILON2=XILONARRAY(SIZE(XILONARRAY))
!
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:PREP_NETCDF_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_NETCDF_GRID
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_LATLONVAL_CDF(HFILENAME,HNCVARNAME,PLON,PLAT,PVAL)
!     ####################
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
REAL, DIMENSION(:), INTENT(OUT) :: PLON,PLAT ! Longitudes/latitudes in netcdf file 
REAL, DIMENSION(:), INTENT(OUT) :: PVAL      ! value to get
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer ::JLOOP1,JDIM1,JDIM2,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer ::NLEN
integer,DIMENSION(2) ::NLEN2D
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
real,DIMENSION(:),ALLOCATABLE   :: ZVALU
real,DIMENSION(:,:),ALLOCATABLE :: ZVALU2D
real :: ZMISS
real,DIMENSION(:),ALLOCATABLE :: ZDIM1
real,DIMENSION(:),ALLOCATABLE :: ZDIM2
character(len=80) :: YDIM1NAME,YDIM2NAME
integer :: ILONFOUND,ILATFOUND, IARG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
!
!
!*    1.      Open the netcdf file 
!             --------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_LATLONVAL_CDF',0,ZHOOK_HANDLE)
status=-9999
kcdf_id=-9999
HACTION='open netcdf'
status=NF_OPEN(HFILENAME,nf_nowrite,kcdf_id)
!write(0,*) 'status=',status
!write(0,*) 'identifiant de ',HFILENAME,'=',kcdf_id
if (status/=NF_NOERR) then 
  CALL HANDLE_ERR_MER(status,HACTION)
!else
!  write(0,*) 'netcdf file opened: ',HFILENAME
endif
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF_INQ_NVARS(kcdf_id,NBVARS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF_INQ_VARNAME(kcdf_id,JLOOP1,VARNAME(JLOOP1))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf_close(kcdf_id)
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  CALL ABOR1_SFX('MODE_READ_NETCDF_MERCATOR: READ_LATLONVAL_CDF')
endif
!-----------
!
!*    4.      get the variable in netcdf file 
!             -------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf_inq_varndims(kcdf_id,ID_VARTOGET,NVARDIMS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimensions number = ',NVARDIMS
!
!     4.2      get the variable dimensions length and values
!              ----------------------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D
  CASE (1) 
    HACTION='get variable dimensions length'
    status=nf_inq_dimlen(kcdf_id,NVARDIMS,NLEN)
    if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    ALLOCATE(ZVALU(NLEN))
    !write(0,*) 'call GET1DCDF'
    CALL GET1DCDF(kcdf_id,ID_VARTOGET,ZMISS,ZVALU)
    PVAL(:)=ZVALU(:)
!CAS 2D
  CASE (2)
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf_inq_dimlen(kcdf_id,JLOOP,NLEN2D(JLOOP))
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    ENDDO
    ALLOCATE(ZVALU2D(NLEN2D(1),NLEN2D(2)))
    ALLOCATE(ZDIM1(NLEN2D(1)))
    ALLOCATE(ZDIM2(NLEN2D(2)))
    !write(0,*) 'call GET2DCDF'
    CALL GET2DCDF(kcdf_id,ID_VARTOGET,ZDIM1,YDIM1NAME,ZDIM2,YDIM2NAME,&
           ZMISS,ZVALU2D)  
    !write(0,*) 'YDIM1NAME: ',YDIM1NAME
    !write(0,*) 'YDIM2NAME: ',YDIM2NAME
    if ((YDIM1NAME=='lon').OR.(YDIM1NAME=='longitude')) ILONFOUND=1
    if ((YDIM2NAME=='lon').OR.(YDIM2NAME=='longitude')) ILONFOUND=2
    if ((YDIM1NAME=='lat').OR.(YDIM1NAME=='latitude'))  ILATFOUND=1
    if ((YDIM2NAME=='lat').OR.(YDIM2NAME=='latitude'))  ILATFOUND=2
    IARG=0
!
!     4.3      complete arrays
!              ---------------
    IF ((ILONFOUND==1).AND.(ILATFOUND==2)) then
      !write(0,*) 'ILONFOUND',ILONFOUND,'ILATFOUND',ILATFOUND
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PVAL(IARG)=ZVALU2D(JDIM1,JDIM2)
          PLON(IARG)=ZDIM1(JDIM1)
          PLAT(IARG)=ZDIM2(JDIM2)
        ENDDO
      ENDDO
    ELSEIF ((ILONFOUND==2).AND.(ILATFOUND==1)) then
      !write(0,*) 'ILONFOUND',ILONFOUND,'ILATFOUND',ILATFOUND
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PVAL(IARG)=ZVALU2D(JDIM1,JDIM2)
          PLAT(IARG)=ZDIM1(JDIM1)
          PLON(IARG)=ZDIM2(JDIM2)
        ENDDO
      ENDDO
    ELSE
      write(0,*) '*****WARNING*****: incompatible dimensions to lat/lon/value arrays'
    ENDIF
!
END SELECT
!
!
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
status=nf_close(kcdf_id)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (ALLOCATED(ZVALU       ))  DEALLOCATE(ZVALU  )
IF (ALLOCATED(ZVALU2D     ))  DEALLOCATE(ZVALU2D)
IF (ALLOCATED(ZDIM1       ))  DEALLOCATE(ZDIM1  )
IF (ALLOCATED(ZDIM2       ))  DEALLOCATE(ZDIM2  )
!
!
IF (ALLOCATED(NVARDIMID   ))  DEALLOCATE(NVARDIMID )
IF (ALLOCATED(NVARDIMNAM  ))  DEALLOCATE(NVARDIMNAM)
IF (ALLOCATED(NVARDIMLEN  ))  DEALLOCATE(NVARDIMLEN)
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_LATLONVAL_CDF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_LATLONVAL_CDF
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_LATLONDEPVAL_CDF(HFILENAME,HNCVARNAME,PLON,PLAT,PDEP,PVAL)
!     ####################
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
REAL, DIMENSION(:), INTENT(OUT) :: PLON,PLAT ! Longitudes/latitudes in netcdf file 
REAL, DIMENSION(:), INTENT(OUT) :: PDEP      ! depth in netcdf file
REAL, DIMENSION(:,:), INTENT(OUT) :: PVAL      ! value to get
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer ::JLOOP1,JDIM1,JDIM2,JDIM3,JLOOP
!integer ::JLOOP2,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer,DIMENSION(3) ::NLEN3D
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
real,DIMENSION(:,:,:),ALLOCATABLE :: ZVALU3D
real :: ZMISS
real,DIMENSION(:),ALLOCATABLE :: ZDIM1
real,DIMENSION(:),ALLOCATABLE :: ZDIM2
real,DIMENSION(:),ALLOCATABLE :: ZDIM3
character(len=80) :: YDIM1NAME,YDIM2NAME,YDIM3NAME
integer :: ILONFOUND,ILATFOUND,IDEPFOUND
integer ::  IARG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
include 'netcdf.inc'
!
!
!
!*    1.      Open the netcdf file 
!             --------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_LATLONDEPVAL_CDF',0,ZHOOK_HANDLE)
HACTION='open netcdf'
status=NF_OPEN(HFILENAME,nf_nowrite,kcdf_id)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'netcdf file opened: ',HFILENAME
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF_INQ_NVARS(kcdf_id,NBVARS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF_INQ_VARNAME(kcdf_id,JLOOP1,VARNAME(JLOOP1))
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf_close(kcdf_id)
  if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
  CALL ABOR1_SFX('MODE_READ_NETCDF_MERCATOR: READ_LATLONDEPVAL_CDF')
endif
!-----------
!
!*    4.      get the variable in netcdf file 
!             -------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf_inq_varndims(kcdf_id,ID_VARTOGET,NVARDIMS)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'variable dimensions number = ',NVARDIMS
ALLOCATE(NVARDIMID(NVARDIMS))
HACTION='get variable dimensions identifiant'
status=nf_inq_vardimid(kcdf_id,ID_VARTOGET,NVARDIMID)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!
!
!     4.2      get the variable dimensions length and values
!              ----------------------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D, 2D
  CASE (1,2) 
    write(0,*) '********************************************'
    write(0,*) '* number of dimension to low: ',NVARDIMS,' *'
    write(0,*) '* you need a 3-dimension variable          *'
    write(0,*) '********************************************'
!CAS 3D
  CASE (3)
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf_inq_dimlen(kcdf_id,NVARDIMID(JLOOP),NLEN3D(JLOOP))
      if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
    ENDDO
    ALLOCATE(ZVALU3D(NLEN3D(1),NLEN3D(2),NLEN3D(3)))
    ALLOCATE(ZDIM1(NLEN3D(1)))
    ALLOCATE(ZDIM2(NLEN3D(2)))
    ALLOCATE(ZDIM3(NLEN3D(3)))
    !write(0,*) 'call GET3DCDF'
    CALL GET3DCDF(kcdf_id,ID_VARTOGET,ZDIM1,YDIM1NAME,ZDIM2,YDIM2NAME,&
           ZDIM3,YDIM3NAME,ZMISS,ZVALU3D) 
    !write(0,*) 'YDIM1NAME: ',YDIM1NAME
    !write(0,*) 'YDIM2NAME: ',YDIM2NAME
    !write(0,*) 'YDIM3NAME: ',YDIM3NAME
    if ((YDIM1NAME=='lon').OR.(YDIM1NAME=='longitude')) ILONFOUND=1
    if ((YDIM2NAME=='lon').OR.(YDIM2NAME=='longitude')) ILONFOUND=2
    if ((YDIM3NAME=='lon').OR.(YDIM3NAME=='longitude')) ILONFOUND=3
    if ((YDIM1NAME=='lat').OR.(YDIM1NAME=='latitude'))  ILATFOUND=1
    if ((YDIM2NAME=='lat').OR.(YDIM2NAME=='latitude'))  ILATFOUND=2
    if ((YDIM3NAME=='lat').OR.(YDIM3NAME=='latitude'))  ILATFOUND=3
    if (YDIM1NAME=='depth')                             IDEPFOUND=1
    if (YDIM2NAME=='depth')                             IDEPFOUND=2
    if (YDIM3NAME=='depth')                             IDEPFOUND=3
    IARG=0
    !write(0,*) 'ILONFOUND',ILONFOUND,'ILATFOUND',ILATFOUND,'IDEPFOUND',IDEPFOUND
!!
!!     4.3      complete arrays
!!              ---------------
    IF ((ILONFOUND==1).AND.(ILATFOUND==2).AND.(IDEPFOUND==3)) then
      !write(0,*) 'SIZE LON=',SIZE(ZDIM1),'SIZE LAT',SIZE(ZDIM2),'SIZE DEP',SIZE(ZDIM3)
      !write(0,*) 'SIZE PLON=',SIZE(PLON),'SIZE PLAT',SIZE(PLAT),'SIZE PDEP',SIZE(PDEP)
      PDEP(:)=ZDIM3(:)
      DO JDIM2=1,SIZE(ZDIM2)
        DO JDIM1=1,SIZE(ZDIM1)
          IARG=IARG+1
          PLON(IARG)=ZDIM1(JDIM1)
          PLAT(IARG)=ZDIM2(JDIM2)
          DO JDIM3=1,SIZE(ZDIM3)
            PVAL(IARG,JDIM3)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
      !write(0,*) 'END complete arrays'
!
    ELSEIF ((ILONFOUND==2).AND.(ILATFOUND==1).AND.(IDEPFOUND==3)) then
      PDEP(:)=ZDIM3(:)
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PLON(IARG)=ZDIM2(JDIM2)
          PLAT(IARG)=ZDIM1(JDIM1)
          DO JDIM3=1,SIZE(ZDIM3)
            PVAL(IARG,JDIM3)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
!
    ELSEIF ((ILONFOUND==1).AND.(ILATFOUND==3).AND.(IDEPFOUND==2)) then
      PDEP(:)=ZDIM2(:)
      DO JDIM3=1,SIZE(ZDIM3)
        DO JDIM1=1,SIZE(ZDIM1)
          IARG=IARG+1
          PLON(IARG)=ZDIM1(JDIM1)
          PLAT(IARG)=ZDIM3(JDIM3)
          DO JDIM2=1,SIZE(ZDIM2)
            PVAL(IARG,JDIM2)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
!
    ELSEIF ((ILATFOUND==1).AND.(ILONFOUND==3).AND.(IDEPFOUND==2)) then
      PDEP(:)=ZDIM2(:)
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM3=1,SIZE(ZDIM3)
          IARG=IARG+1
          PLON(IARG)=ZDIM3(JDIM3)
          PLAT(IARG)=ZDIM1(JDIM1)
          DO JDIM2=1,SIZE(ZDIM2)
            PVAL(IARG,JDIM2)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
!
    ELSEIF ((ILONFOUND==2).AND.(ILATFOUND==3).AND.(IDEPFOUND==1)) then
      PDEP(:)=ZDIM1(:)
      DO JDIM3=1,SIZE(ZDIM3)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PLON(IARG)=ZDIM2(JDIM2)
          PLAT(IARG)=ZDIM3(JDIM3)
          DO JDIM1=1,SIZE(ZDIM1)
            PVAL(IARG,JDIM1)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
!
    ELSEIF ((ILATFOUND==2).AND.(ILONFOUND==3).AND.(IDEPFOUND==1)) then
      PDEP(:)=ZDIM1(:)
      DO JDIM2=1,SIZE(ZDIM2)
        DO JDIM3=1,SIZE(ZDIM3)
          IARG=IARG+1
          PLON(IARG)=ZDIM3(JDIM3)
          PLAT(IARG)=ZDIM2(JDIM2)
          DO JDIM1=1,SIZE(ZDIM1)
            PVAL(IARG,JDIM1)=ZVALU3D(JDIM1,JDIM2,JDIM3)
          ENDDO
        ENDDO
      ENDDO
!
    ELSE
      write(0,*) '*****WARNING*****: incompatible dimensions to lat/lon/value arrays'
    ENDIF
!
END SELECT
!
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
!write(0,*) HACTION
status=nf_close(kcdf_id)
if (status/=NF_NOERR) CALL HANDLE_ERR_MER(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (ALLOCATED(ZVALU3D     ))  DEALLOCATE(ZVALU3D)
IF (ALLOCATED(ZDIM1       ))  DEALLOCATE(ZDIM1  )
IF (ALLOCATED(ZDIM2       ))  DEALLOCATE(ZDIM2  )
IF (ALLOCATED(ZDIM3       ))  DEALLOCATE(ZDIM3  )
!
!
IF (ALLOCATED(NVARDIMID   ))  DEALLOCATE(NVARDIMID )
IF (ALLOCATED(NVARDIMNAM  ))  DEALLOCATE(NVARDIMNAM)
IF (ALLOCATED(NVARDIMLEN  ))  DEALLOCATE(NVARDIMLEN)
201   FORMAT(4(3X,F10.4))
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_LATLONDEPVAL_CDF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_LATLONDEPVAL_CDF
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_NETCDF_SST(HFILENAME,HNCVARNAME,PFIELD)
!     ####################
!
USE MODD_GRID_LATLONREGUL, ONLY : NINDEPTH,NILENGTH
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_CSTS,             ONLY : XTT
USE MODD_PREP,       ONLY : CINTERP_TYPE
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
REAL, POINTER,DIMENSION(:) :: PFIELD      ! value to get
!
REAL,DIMENSION(:), ALLOCATABLE :: ZLATI
REAL,DIMENSION(:), ALLOCATABLE :: ZLONG
REAL,TARGET,DIMENSION(:,:), ALLOCATABLE :: ZVALUE
REAL,DIMENSION(:), ALLOCATABLE :: ZDEPTH
REAL,TARGET,DIMENSION(:), ALLOCATABLE :: ZVAL
integer :: jloop
!PLM
REAL :: ZUNDEF=999.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_NETCDF_SST',0,ZHOOK_HANDLE)
if (NILENGTH<0) then
  ALLOCATE(PFIELD(1))
  PFIELD(:)=XUNDEF
  CINTERP_TYPE='UNIF  ' !!prescribed uniform field
elseif (NINDEPTH<0) then
  ALLOCATE(ZLATI(NILENGTH) )
  ALLOCATE(ZLONG(NILENGTH) )
  ALLOCATE(ZVAL (NILENGTH) )
  CALL READ_LATLONVAL_CDF(HFILENAME,HNCVARNAME,ZLONG,ZLATI,ZVAL)
  ALLOCATE(PFIELD(NILENGTH))
  PFIELD(:)=XUNDEF
  PFIELD(:) = ZVAL(:)
  WHERE (ZVAL(:)/=ZUNDEF .AND. ZVAL(:)<100.) PFIELD(:)=PFIELD(:)+XTT
  CINTERP_TYPE='HORIBL' !!interpolation from gaussian, legendre or regular grid
!                       !!CINGRID_TYPE='GAUSS  ' ou ='AROME '
!                       !!CINGRID_TYPE='LATLON '
else 
  ALLOCATE(ZVALUE(NILENGTH,NINDEPTH))
  ALLOCATE(ZLATI(NILENGTH) )
  ALLOCATE(ZLONG(NILENGTH) )
  ALLOCATE(ZDEPTH(NINDEPTH))
!
 CALL READ_LATLONDEPVAL_CDF(HFILENAME,HNCVARNAME,ZLONG,ZLATI,ZDEPTH,ZVALUE)
!
  ALLOCATE(PFIELD(NILENGTH))
  PFIELD(:)=XUNDEF
  PFIELD(:)=ZVALUE(:,1)
  WHERE (ZVALUE(:,1)/=ZUNDEF .AND. ZVALUE(:,1)<100.) PFIELD(:)=PFIELD(:)+XTT
  CINTERP_TYPE='HORIBL' !!interpolation from gaussian, legendre or regular grid
!                       !!CINGRID_TYPE='GAUSS  ' ou ='AROME '
!                       !!CINGRID_TYPE='LATLON '
endif
!
IF (ALLOCATED(ZVALUE      ))  DEALLOCATE(ZVALUE )
IF (ALLOCATED(ZLONG       ))  DEALLOCATE(ZLONG  )
IF (ALLOCATED(ZLATI       ))  DEALLOCATE(ZLATI  )
IF (ALLOCATED(ZDEPTH      ))  DEALLOCATE(ZDEPTH )
IF (ALLOCATED(ZVAL        ))  DEALLOCATE(ZVAL   )
!
202   FORMAT(3(3X,F10.4))
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_NETCDF_SST',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NETCDF_SST
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_NETCDF_ZS_SEA(HFILENAME,HNCVARNAME,PFIELD)
!     ####################
!
USE MODD_GRID_LATLONREGUL, ONLY : NINLAT,NINLON,NINDEPTH,NILENGTH
USE MODD_PREP,       ONLY : CINTERP_TYPE
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
REAL, POINTER, DIMENSION(:)   :: PFIELD      ! value to get
!
REAL,DIMENSION(:), ALLOCATABLE :: ZLATI
REAL,DIMENSION(:), ALLOCATABLE :: ZLONG
REAL,TARGET, DIMENSION(:), ALLOCATABLE:: ZVALUE
integer :: jloop
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
include 'netcdf.inc'
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_NETCDF_ZS_SEA',0,ZHOOK_HANDLE)
if(NINDEPTH>0) then
  !write(0,*) '*****warning*****',HNCVARNAME,' is a 3D field'
  ALLOCATE(PFIELD(1))
  PFIELD(:)=0.
  CINTERP_TYPE='UNIF  ' !!prescribed uniform field
elseif(NILENGTH>0) then 
  ALLOCATE(ZVALUE(NILENGTH))
  ALLOCATE(ZLATI(NILENGTH) )
  ALLOCATE(ZLONG(NILENGTH) )
!
  CALL READ_LATLONVAL_CDF(HFILENAME,HNCVARNAME,ZLONG,ZLATI,ZVALUE)
  ALLOCATE(PFIELD(NILENGTH))
  PFIELD(:)=ZVALUE(:)
  CINTERP_TYPE='HORIBL' !!interpolation from gaussian, legendre or regular grid
!                       !!CINGRID_TYPE='GAUSS  ' ou ='AROME '
!                       !!CINGRID_TYPE='LATLON '
else
  ALLOCATE(PFIELD(1))
  PFIELD(:)=0.
  CINTERP_TYPE='UNIF  ' !!prescribed uniform field
endif
!
IF (ALLOCATED(ZVALUE      ))  DEALLOCATE(ZVALUE )
IF (ALLOCATED(ZLONG       ))  DEALLOCATE(ZLONG  )
IF (ALLOCATED(ZLATI       ))  DEALLOCATE(ZLATI  )
!
202   FORMAT(3(3X,F10.4))
IF (LHOOK) CALL DR_HOOK('MODE_READ_NETCDF_MERCATOR:READ_NETCDF_ZS_SEA',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NETCDF_ZS_SEA
!------------------------------------------------------------------------------
!==============================================================================
END MODULE MODE_READ_NETCDF_MERCATOR
