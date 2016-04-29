MODULE mode_dimlist
  IMPLICIT NONE 
  
  TYPE dimCDF
     CHARACTER(LEN=8)      :: name
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
  LOGICAL, SAVE :: GUSEDIM = .FALSE.
  TYPE(dimCDF), POINTER :: ptdimx, ptdimy, ptdimz

CONTAINS 
  
  SUBROUTINE init_dimCDF()
  
  NULLIFY(dimlist)
  NULLIFY(ptdimx, ptdimy, ptdimz)
  IF (GUSEDIM) THEN
    ! reservation for DIMX,DIMY,DIMZ
    ptdimx=>get_dimCDF(IDIMX,.TRUE.)
    ptdimx%name = 'DIMX'
    ptdimy=>get_dimCDF(IDIMY,.TRUE.)
    ptdimy%name = 'DIMY'
    ! PGD MesoNH files doesn't contain KMAX
    IF (IDIMZ > 0) THEN
      ptdimz=>get_dimCDF(IDIMZ,.TRUE.)
      ptdimz%name = 'DIMZ'
    END IF
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
    !
    IF (len /= 1) THEN 
       IF (gforce) THEN
         NULLIFY(tmp)
       ELSE 
         count = 1
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
          IF (GUSEDIM .AND. len == IDIMX*IDIMY) THEN
             tmp%create = .FALSE.
             tmp%ndims = 2
          ELSEIF (GUSEDIM .AND. len == IDIMX*IDIMY*IDIMZ) THEN
             tmp%ndims = 3
             tmp%create = .FALSE.
          ELSEIF (GUSEDIM .AND. IDIMY == 3 .AND. len == IDIMX*IDIMZ) THEN
             tmp%ndims = 12 ! faux mais reconnu dans def_ncdf
             tmp%create = .FALSE.
          ELSE
            tmp%ndims = 0
            tmp%create = .TRUE.
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
