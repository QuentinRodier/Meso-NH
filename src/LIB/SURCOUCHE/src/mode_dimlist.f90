#ifdef MNH_NCWRIT
MODULE mode_dimlist
! ------ BEGIN MODIF SB -----------
USE MODD_PARAM
!USE MODD_DIM_ll, ONLY: NIMAX_ll,NJMAX_ll
USE MODD_DIM_n
! ------  END  MODIF SB -----------
  IMPLICIT NONE 
  
  TYPE dimCDF
! ------ BEGIN MODIF SB -----------
     CHARACTER(LEN=FM_FIELD_SIZE)      :: name
! ------  END  MODIF SB -----------
     INTEGER               :: len
     INTEGER               :: id
     LOGICAL               :: create
     INTEGER               :: ndims  ! number of dim reference (when create=.FALSE.)
     TYPE(dimCDF), POINTER :: next
  END TYPE dimCDF

  TYPE(dimCDF), POINTER, PRIVATE, SAVE :: dimlist
  INTEGER, PRIVATE, SAVE               :: nbelt = 0
  INTEGER, SAVE :: IDIMX = 0
  INTEGER, SAVE :: IDIMY = 0
  INTEGER, SAVE :: IDIMZ = 0
  INTEGER, SAVE :: IDIMT = 0
  INTEGER, SAVE :: IDIMN = 0
  INTEGER, SAVE :: IDIMP = 0
  INTEGER, SAVE :: CDIMT = 0
  INTEGER, SAVE :: NUMDIM = 0
  INTEGER, SAVE :: NDIMX = 0
  INTEGER, SAVE :: NDIMXR = 0
  INTEGER, SAVE :: NDIMY = 0
  INTEGER, SAVE :: NDIMZ = 0
  INTEGER, SAVE :: NDIMZR = 0
  INTEGER, SAVE :: NDIMD = 0
  INTEGER, SAVE :: NDIMT = 0
  INTEGER, SAVE :: NDIMN = 0
  INTEGER, SAVE :: NDIMP = 0
  INTEGER, SAVE :: NDIMDATE = 0
  INTEGER, SAVE :: NDIMWL = 0
  LOGICAL, SAVE :: CREATET = .TRUE.
  LOGICAL, SAVE :: CREATEX = .TRUE.
  LOGICAL, SAVE :: CREATEY = .TRUE.
  LOGICAL, SAVE :: CREATEZ = .TRUE.
  LOGICAL, SAVE :: CREATEXR = .TRUE.
  LOGICAL, SAVE :: CREATEZR = .TRUE.
  LOGICAL, SAVE :: CREATEN = .TRUE.
  LOGICAL, SAVE :: CREATEDATE = .TRUE.
  LOGICAL, SAVE :: CREATEWL = .TRUE.
  LOGICAL, SAVE :: CREATED = .TRUE.
  LOGICAL, SAVE :: WRITETIME = .TRUE.
  LOGICAL, SAVE :: NC_WRITE = .FALSE.
  CHARACTER(LEN=3), SAVE      :: NC_FILE = ''
  CHARACTER(LEN=FM_FIELD_SIZE), SAVE  :: first_var = ''
  CHARACTER(LEN=FM_FIELD_SIZE), SAVE  :: last_var = ''
  LOGICAL, SAVE :: DEF_NC = .TRUE.
  INTEGER, SAVE :: KCDF_ID = 0
  INTEGER,DIMENSION(8), SAVE :: KCDF_IOID = 0 
  TYPE(dimCDF), POINTER :: ptdimx, ptdimy, ptdimz, ptdimt, ptdimn, ptdimp,&
    &ptdimd , ptdimxr, ptdimzr, ptdimwl

CONTAINS 
  
  SUBROUTINE init_dimCDF()
  
  NULLIFY(dimlist)
  NULLIFY(ptdimx,ptdimy,ptdimz,ptdimt,ptdimn,ptdimp,ptdimd,ptdimzr,ptdimxr)
    ! reservation for DIMX,DIMY,DIMZ
   IF ( IDIMX == NIMAX_ll+2 ) THEN 
    ptdimx=>get_dimCDF(IDIMX,.TRUE.)
!    print * , 'get dim X'
! ------ BEGIN MODIF SB -----------
! change name of dimensions
    ptdimx%name = 'X'
   ELSEIF ( IDIMX == NIMAX_ll .AND. NIMAX_ll> 1 ) THEN
    ptdimxr=>get_dimCDF(IDIMX,.TRUE.)
    ptdimxr%name = 'XR'
!    print * , 'get dim XR'
   ELSEIF ( IDIMX == 16 ) THEN
    ptdimd=>get_dimCDF(IDIMX,.TRUE.)
    ptdimd%name = 'DATE'
   END IF
! ------  END  MODIF SB -----------
    IF (IDIMY == NJMAX_ll+2) THEN
    ptdimy=>get_dimCDF(IDIMY,.TRUE.)
! ------ BEGIN MODIF SB -----------
    ptdimy%name = 'Y'
!    print * , 'get dim Y'
    END IF
! ------  END  MODIF SB -----------
    ! PGD MesoNH files doesn't contain KMAX
    IF (IDIMZ == NKMAX+2) THEN
!      print * , ' IDIMZ == NKMAX+2 '
      ptdimz=>get_dimCDF(IDIMZ,.TRUE.)
! ------ BEGIN MODIF SB -----------
      ptdimz%name = 'Z'
!    print * , 'get dim Z'
    ELSEIF (IDIMZ == NKMAX ) THEN
     !print * , ' IDIMZ == NKMAX '
     ptdimzr=>get_dimCDF(IDIMZ,.TRUE.)
     ptdimzr%name = 'ZR'
!    print * , 'get dim ZR'
    ELSEIF (IDIMZ == 6 ) THEN
     ptdimwl=>get_dimCDF(IDIMZ,.TRUE.)
     ptdimwl%name = 'WL'
    ELSEIF (IDIMZ == 3 .AND. IDIMX /= NKMAX ) THEN
     ptdimd=>get_dimCDF(IDIMZ,.TRUE.)
     ptdimd%name = 'D'
    END IF
!    print * , ' IDIMT = ' , IDIMT
    IF (IDIMT > 1) THEN
            ptdimt=>get_dimCDF(IDIMT,.TRUE.)
            ptdimt%name = 'T'
    END IF
    IF (IDIMN > 1) THEN
            ptdimt=>get_dimCDF(IDIMN,.TRUE.)
            ptdimt%name = 'N'
    END IF
  END SUBROUTINE init_dimCDF

  FUNCTION size_dimCDF()
    INTEGER :: size_dimCDF

    size_dimCDF = nbelt

  END FUNCTION size_dimCDF

  FUNCTION first_dimCDF()
    TYPE(dimCDF), POINTER :: first_dimCDF

    first_dimCDF=>dimlist

  END FUNCTION first_dimCDF
  
  
  FUNCTION get_dimCDF(len,ocreate)
    INTEGER, INTENT(IN)   :: len
    LOGICAL, INTENT(IN), OPTIONAL :: ocreate ! when .TRUE. create a dim CELL 
    TYPE(dimCDF), POINTER :: get_dimCDF
    

    TYPE(dimCDF), POINTER :: tmp
    INTEGER               :: count
    CHARACTER(LEN=5)      :: yndim
    LOGICAL               :: gforce

    IF (PRESENT(ocreate)) THEN
      gforce = ocreate
    ELSE
      gforce = .FALSE.
    ENDIF
    IF (len /= 1) THEN 
       IF (gforce) THEN
         NULLIFY(tmp)
       ELSE 
         count = 0
         tmp=>dimlist
      DO WHILE(ASSOCIATED(tmp))
           IF (tmp%len == len) EXIT
           tmp=>tmp%next
           count = count+1
         END DO
       END IF
       IF (.NOT. ASSOCIATED(tmp)) THEN
          ALLOCATE(tmp)
          nbelt = nbelt+1
          WRITE(yndim,'(i5)') count
          tmp%name = 'DIM'//ADJUSTL(yndim)
          tmp%len  = len
          tmp%id   = 0
          IF (len == IDIMX*IDIMY .AND. IDIMY > 1 .AND. IDIMY /= CDIMT ) THEN
             tmp%create = .FALSE.
             tmp%ndims = 2
             !print * , ' GUSEDIM 2D '
          ELSEIF (len == IDIMX*IDIMY*IDIMZ .AND. IDIMY > 1 .AND. IDIMZ > 6 ) THEN
             tmp%ndims = 3
             tmp%create = .FALSE.
             !print * , ' GUSEDIM 3D '
          ELSEIF (len == IDIMX*IDIMY*IDIMZ .AND. IDIMY > 1 .AND. IDIMZ == 3 ) THEN
             tmp%ndims = 4
             tmp%create = .FALSE.
             !print * , ' GUSEDIM 3D DEPTH '
          ELSEIF (len == IDIMX*IDIMY*IDIMZ .AND. IDIMY > 1 .AND. IDIMZ == 6 ) THEN
             tmp%ndims = 9
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XYWL '
          ELSEIF (IDIMZ == 6 .AND. len == IDIMX*IDIMZ ) THEN
             tmp%ndims = 6
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XWL '
          ELSEIF (IDIMX > 1 .AND. len == IDIMX*IDIMZ .AND. &
         & IDIMZ == NKMAX  ) THEN
             tmp%ndims = 10 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XZR '
          ELSEIF (IDIMX > 1 .AND. len == IDIMX*IDIMZ .AND. &
         & IDIMZ == NKMAX+2  ) THEN
             tmp%ndims = 12 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XZ '
          ELSEIF (IDIMX == 16 .AND. IDIMY == CDIMT ) THEN
             tmp%ndims = 16 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XT '
          ELSEIF (IDIMT > 1 .AND. len == IDIMZ*IDIMT &
         & .AND. IDIMZ == NKMAX ) THEN
             tmp%ndims = 14 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM ZRT '
          ELSEIF (IDIMT > 1 .AND. len == IDIMZ*IDIMT*IDIMN &
         & .AND. IDIMZ == NKMAX .AND. IDIMN > 1 ) THEN
             tmp%ndims = 17 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM ZRTN '
          ELSEIF (IDIMT > 1 .AND. len == IDIMX*IDIMT &
         & .AND. IDIMX > 1 ) THEN
             tmp%ndims = 16 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM XT '
          ELSEIF (IDIMZ == NKMAX .AND. len == IDIMX*IDIMZ &
         & .AND. IDIMT == 1 ) THEN
             tmp%ndims = 15 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM ZR '
          ELSEIF (IDIMZ == NKMAX+2 .AND. IDIMX == 1 ) THEN
             tmp%ndims = 18 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
             !print * , ' GUSEDIM Z '
          ELSE
            tmp%ndims = 1
            tmp%create = .TRUE.
             !print * , ' GUSEDIM 1D'
          END IF
          tmp%next => dimlist
          dimlist  => tmp
       END IF
    
       get_dimCDF=>tmp

    ELSE

       NULLIFY(get_dimCDF)
    END IF

  END FUNCTION get_dimCDF
END MODULE mode_dimlist
#endif
