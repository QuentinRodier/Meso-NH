#ifdef MNH_NCWRIT
MODULE mode_util
  USE MODE_FIELDTYPE
  USE MODE_DIMLIST
  USE MODD_PARAM

  IMPLICIT NONE 

  TYPE workfield
     CHARACTER(LEN=FM_FIELD_SIZE)            :: name   ! nom du champ
     CHARACTER(LEN=FM_FIELD_SIZE)            :: ncname   ! nom du champ
     CHARACTER(LEN=FM_FIELD_SIZE)            :: ncunit   ! unité du champ
     CHARACTER(LEN=64)                       :: ncdes    ! description du champ
     CHARACTER(LEN=64)                       :: long_name    ! description du champ
     INTEGER                                 :: TYPE   ! type (entier ou reel)    
     CHARACTER(LEN=1), DIMENSION(:), POINTER :: comment
     TYPE(dimCDF),                   POINTER :: dim
     INTEGER                                 :: id
     INTEGER                                 :: grid
  END TYPE workfield

  INCLUDE 'netcdf.inc'

CONTAINS 
  FUNCTION str_replace(hstr, hold, hnew)
    CHARACTER(LEN=*) :: hstr
    CHARACTER(LEN=*) ::  hold, hnew
    CHARACTER(LEN=LEN_TRIM(hstr)+MAX(0,LEN(hnew)-LEN(hold))) :: str_replace
    
    INTEGER :: pos
    
    pos = INDEX(hstr,hold)
    IF (pos /= 0) THEN
       str_replace = hstr(1:pos-1)//hnew//hstr(pos+LEN(hold):)
    ELSE 
       str_replace = hstr 
    END IF

  END FUNCTION str_replace

  SUBROUTINE PARSE_PFIELD(yrecfm,kgrid,pfield,tpreclist,hcomment)
    REAL,    DIMENSION(:), INTENT(IN)    :: pfield
    TYPE(workfield), DIMENSION(:), POINTER   :: tpreclist    
    INTEGER                                  :: kgrid

    INTEGER                                  :: ich
    INTEGER                                  :: fsize
    CHARACTER(LEN=*)                         :: yrecfm
    CHARACTER(LEN=*)                         :: hcomment

    ALLOCATE(tpreclist(1))
    fsize = IDIMX*IDIMY*IDIMZ*IDIMT*IDIMN*IDIMP
    tpreclist(1)%name = yrecfm

!    ! Phase 2 : Extract comments and dimensions for valid articles.
!    !           Infos are put in tpreclist.
    CALL init_dimCDF()
    tpreclist(1)%TYPE = get_ftype(yrecfm)               
    tpreclist(1)%ncname = get_ncname(yrecfm)               
    tpreclist(1)%ncunit = get_ncunit(yrecfm)
    tpreclist(1)%ncdes = get_ncdes(yrecfm)
    tpreclist(1)%grid = kgrid
    tpreclist(1)%long_name = hcomment
!    tpreclist(1)%long_name = tpreclist(1)%ncdes
    tpreclist(1)%dim=>get_dimCDF(fsize)
!  
  END SUBROUTINE PARSE_PFIELD
  
  SUBROUTINE HANDLE_ERR(status,line)
    INTEGER :: status,line

    IF (status /= NF_NOERR) THEN
       PRINT *, 'line ',line,': ',NF_STRERROR(status)
       STOP
    END IF
  END SUBROUTINE HANDLE_ERR

  SUBROUTINE DEF_NCDF(tpreclist)
    USE MODD_TIME_n, ONLY: TDTCUR,TDTMOD
    USE MODD_DIM_n
    USE MODD_GRID
    USE MODD_CONF, ONLY: LCARTESIAN
    TYPE(workfield),DIMENSION(:),POINTER :: tpreclist    

    INTEGER :: status
    TYPE(dimCDF), POINTER :: tzdim
    INTEGER(KIND=4)     :: invdims
    INTEGER(KIND=4), DIMENSION(10) :: ivdims
    CHARACTER(LEN=20)                 :: ycdfvar
    INTEGER (KIND=4)                  :: levId,timeId
    INTEGER (KIND=4)                  :: hh,mm,ss
    CHARACTER(len=50)                 :: date_string
    CHARACTER(len=30)                 :: year,month,day,hour,minute,second

    ! global attributes
    status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'TITLE'&
         & ,LEN(VERSION_ID),VERSION_ID)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_INT(kcdf_id,NF_GLOBAL,'CURRENT_DATE' &
         & ,NF_INT,3,TDTCUR%TDATE)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'CURRENT_TIME' &
         & ,NF_DOUBLE,1,TDTCUR%TIME)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_INT(kcdf_id,NF_GLOBAL,'SIMULATION_START_DATE' &
         & ,NF_INT,3,TDTMOD%TDATE)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'SIMULATION_START_TIME' &
         & ,NF_DOUBLE,1,TDTMOD%TIME)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'GRID_TYPE' &
         & ,1,"C")
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    IF ( LCARTESIAN ) THEN
    status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,9,"cartesian")
    ELSE
      IF ( XRPK == 0 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,8,"mercator")
      ELSEIF ( XRPK == 1 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,35,"polar-stereographic from north pole")
      ELSEIF ( XRPK == -1 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,35,"polar-stereographic from south pole")
      ELSEIF ( XRPK > -1 .AND. XRPK < 0 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,33,"lambert conformal from south pole")
      ELSEIF ( XRPK > 0 .AND. XRPK < 1 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'PROJECTION'&
         & ,33,"lambert conformal from north pole")
      END IF
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'RPK' &
         & ,NF_DOUBLE,1,XRPK)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'BETA' &
         & ,NF_DOUBLE,1,XBETA)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'LAT0' &
         & ,NF_DOUBLE,1,XLAT0)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'LON0' &
         & ,NF_DOUBLE,1,XLON0)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'LATORI' &
         & ,NF_DOUBLE,1,XLATORI)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    status = NF_PUT_ATT_DOUBLE(kcdf_id,NF_GLOBAL,'LONORI' &
         & ,NF_DOUBLE,1,XLONORI)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END IF

! define DIMENSIONS
!    print * , 'Defining dimensions def_ncdf'
    tzdim=>first_DimCDF()
    DO WHILE(ASSOCIATED(tzdim))
      IF (tzdim%create) THEN
              !print * , ' CREATE '
        IF ( IDIMX > 1 .AND. IDIMY == 1 .AND. IDIMZ == 1 .AND. IDIMT == 1  ) THEN
          IF ( IDIMX == NKMAX+2 .AND. IDIMX /= NIMAX_ll+2 ) THEN
                   tzdim%name = 'Z'
          ELSEIF ( tpreclist(1)%name == 'YHAT' ) THEN
                   tzdim%name = 'Y'
          ELSEIF ( tpreclist(1)%name == 'ZHAT' ) THEN
                   tzdim%name = 'Z'
          ELSEIF ( IDIMX == NIMAX_ll+2 ) THEN
                 tzdim%name = 'X'
          ELSEIF ( NIMAX_ll == 0 ) THEN
                  !print * , 'CASE PGD'
                   tzdim%name = 'X'
          ELSEIF ( IDIMX == NIMAX_ll ) THEN
                   tzdim%name = 'XR'
          ELSEIF ( IDIMX == 16 .AND. IDIMX /= NIMAX_ll+2 ) THEN
                   tzdim%name = 'DATE'
          END IF
       END IF
      IF ( tzdim%name == 'X' .AND. CREATEX ) THEN
!        print * , ' create dim X' 
        CREATEX = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMX=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'Y' .AND. CREATEY ) THEN
        !print * , ' create dim Y' 
        CREATEY = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMY=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'Z' .AND. CREATEZ ) THEN
        !print * , ' create dim Z' 
        NUMDIM=NUMDIM+1
        NDIMZ=NUMDIM
        CREATEZ = .FALSE.
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'XR' .AND. CREATEXR ) THEN
           !print * , ' create dim XR' 
        CREATEXR = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMXR=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'ZR' .AND. CREATEZR ) THEN
           !print * , ' create dim ZR' 
        CREATEZR = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMZR=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'DATE' .AND. CREATEDATE ) THEN
           !print * , ' create dim DATE' 
        CREATEDATE = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMDATE=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'WL' .AND. CREATEWL ) THEN
        !print * , ' create dim WAVELENGTH' 
        CREATEWL = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMWL=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( tzdim%name == 'D' .AND. CREATED ) THEN
        !print * , ' create dim DEPTH' 
        CREATED = .FALSE.
        NUMDIM=NUMDIM+1
        NDIMWL=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      ELSEIF ( IDIMT > 1 .AND. CREATET ) THEN
           !print * , ' create dim T' 
        tzdim%name = 'T'
        NUMDIM=NUMDIM+1
        NDIMT=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
        CREATET = .FALSE.
        CDIMT = IDIMT 
      ELSEIF ( IDIMN > 1 .AND. CREATEN ) THEN
           !print * , ' create dim N' 
        tzdim%name = 'N'
        NUMDIM=NUMDIM+1
        NDIMN=NUMDIM
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
        CREATEN = .FALSE.
      END IF
    END IF
    tzdim=>tzdim%next
  END DO

!    PRINT *,'------------- NetCDF DEFINITION ---------------'
    ! FIRST WRITE -> DEFINE TIME ORIGIN
  IF ( WRITETIME ) THEN

    status = NF_DEF_VAR(kcdf_id,"time",NF_INT,&
               0,0,timeId)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    write(year,*) TDTMOD%TDATE%YEAR
    write(month,*) TDTMOD%TDATE%MONTH
    write(day,*) TDTMOD%TDATE%DAY
    year = adjustl(year)
    month = adjustl(month)
    day = adjustl(day)
    date_string='seconds since '//trim(year)//'-'//trim(month)//'-'//trim(day)
    hh= TDTMOD%TIME / 3600
    mm =  ( TDTMOD%TIME/1 - hh*3600 )  / 60
    ss =  TDTMOD%TIME/1 - hh*3600 - mm*60 
    write(hour,"(I2.2)") hh
    hour = adjustl(hour)
    write(minute,"(I2.2)") mm
    minute = adjustl(minute)
    write(second,"(I2.2)") ss
    second = adjustl(second)
    date_string = trim(date_string)//' ' &
    &   //trim(hour)//':'//trim(minute)//':'//trim(second)
    status = NF_PUT_ATT_TEXT(kcdf_id,timeId,'units'&
      & ,50,date_string)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    WRITETIME = .FALSE.
  END IF

! define VARIABLES and ATTRIBUTES
   !print * , 'Defining variables and attributes def_ncdf'
      
    IF (ASSOCIATED(tpreclist(1)%dim)) THEN
      IF ( tpreclist(1)%dim%create  ) THEN
        IF ( tpreclist(1)%name .NE. 'XHAT' .AND. &
           & tpreclist(1)%name .NE. 'YHAT' .AND. &
           & tpreclist(1)%name .NE. 'ZHAT' ) THEN
           tpreclist(1)%dim%create= .FALSE.
           tpreclist(1)%dim%ndims= 11
        END IF
        IF ( NIMAX_ll /=0 .AND. NIMAX_ll == NKMAX )  THEN
          print * , ' WARNING NIMAX=NKMAX '
          print * , ' NIMAX = ' , NIMAX_ll
          print * , ' NKMAX = ' , NKMAX
          print * , ' IT IS BETTER TO USE NKMAX NOT EQUAL TO NIMAX'
        END IF 
        IF ( tpreclist(1)%name .EQ. 'YHAT' ) THEN
          ivdims(1) = NDIMY
        ELSEIF ( tpreclist(1)%name .EQ. 'ZHAT' ) THEN
          ivdims(1) = NDIMZ
        ELSEIF ( IDIMX == NKMAX+2 ) THEN
          ivdims(1) = NDIMZ
        ELSEIF ( IDIMX == CDIMT .OR. IDIMT == CDIMT ) THEN
          ivdims(1) = NDIMT
        ELSEIF (IDIMX == 3 .AND. IDIMY == 3 .AND. IDIMZ == 1 ) THEN
            tpreclist(1)%dim%ndims = 2
        ELSEIF ( IDIMX == NIMAX_ll+2 ) THEN
          ivdims(1) = NDIMX
        ELSEIF ( IDIMX == 16 ) THEN
          ivdims(1) = 5
        ELSE 
           !print * , 'CASE PGD'
          ivdims(1) = NDIMX
        END IF
      END IF
      IF (tpreclist(1)%dim%create) THEN
        invdims   = 1
        ivdims(1) = tpreclist(1)%dim%id
      ELSE
        IF ( tpreclist(1)%dim%ndims == 1 ) THEN 
          IF ( tpreclist(1)%name .NE. 'XHAT' .AND. &
             & tpreclist(1)%name .NE. 'YHAT' .AND. &
             & tpreclist(1)%name .NE. 'ZHAT') THEN
             invdims = 11
          END IF
        ELSE
          invdims = tpreclist(1)%dim%ndims
        END IF
!
        SELECT CASE(invdims)
!
          CASE(2)
             status = NF_INQ_DIMID(kcdf_id,'X', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Y', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
!
          CASE(3)
             status = NF_INQ_DIMID(kcdf_id,'X', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Y', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Z', ivdims(3))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 

          CASE(4)
             status = NF_INQ_DIMID(kcdf_id,'X', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Y', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension

          CASE(5)
             status = NF_INQ_DIMID(kcdf_id,'ZR',ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'X',ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension

          CASE(6)
             status = NF_INQ_DIMID(kcdf_id,'X', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'WL', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension

          CASE(9)
             status = NF_INQ_DIMID(kcdf_id,'X', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Y', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'WL', ivdims(3))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 3 ! on retablit la bonne valeur du nbre de dimension

          CASE(10)
             status = NF_INQ_DIMID(kcdf_id,'X',ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'ZR',ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension
!
          CASE(11)
              invdims=1 ! on retablit la bonne valeur du nbre de dimension
!
          CASE(12)
             status = NF_INQ_DIMID(kcdf_id,'X',ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'Z',ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension
!
          CASE(14)
             status = NF_INQ_DIMID(kcdf_id,'ZR', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'T', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension

          CASE(15)
             status = NF_INQ_DIMID(kcdf_id,'ZR', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 1 ! on retablit la bonne valeur du nbre de dimension
!
          CASE(16)
             status = NF_INQ_DIMID(kcdf_id,'X',ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'T', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 2 ! on retablit la bonne valeur du nbre de dimension

          CASE(17)
             status = NF_INQ_DIMID(kcdf_id,'ZR',ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'T', ivdims(2))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             status = NF_INQ_DIMID(kcdf_id,'N', ivdims(3))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 3 ! on retablit la bonne valeur du nbre de dimension

          CASE(18)
             status = NF_INQ_DIMID(kcdf_id,'Z', ivdims(1))
             IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
             invdims = 1 ! on retablit la bonne valeur du nbre de dimension
!
          CASE default
             PRINT *,'Fatal error in NetCDF dimension definition ?'
             STOP
        END SELECT
      END IF
    ELSE
         ! scalar variables
          invdims   = 0
          ivdims(1) = 0 ! ignore dans ce cas
    END IF
       
! Variables definition

!! NetCDF n'aime pas les '%' dans le nom des variables
!! "%" remplaces par '_' 
!! ni les '.' remplaces par '_'
    ycdfvar = TRIM(tpreclist(1)%name)
    ycdfvar = str_replace(ycdfvar,'%','_')
    ycdfvar = str_replace(ycdfvar,'.','_')

    SELECT CASE(tpreclist(1)%TYPE)
!
      CASE (TEXT)
        status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_CHAR,&
                   invdims,ivdims,tpreclist(1)%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
!
      CASE (INT2,BOOL)
        !PRINT *,'INT,BOOL : ',tpreclist(1)%name
        status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_INT,&
                   invdims,ivdims,tpreclist(1)%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
!
      CASE (FLOAT2)
        !PRINT *,'FLOAT : ',tpreclist(1)%name
        IF ( tpreclist(1)%name == 'XHAT' ) THEN
          ivdims(1) = NDIMX
        END IF
        IF ( tpreclist(1)%name == 'YHAT' ) THEN
          ivdims(1) = NDIMY
        END IF
        IF ( tpreclist(1)%name == 'ZHAT' ) THEN
          ivdims(1) = NDIMZ
        END IF
        status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_FLOAT,&
                 invdims,ivdims,tpreclist(1)%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
!
      CASE default
!        PRINT *,'ATTENTION : ',TRIM(tpreclist(1)%name),' est de&
!             & TYPE inconnu --> force a REAL'
        status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_FLOAT,&
                 invdims,ivdims,tpreclist(1)%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
!          
    END SELECT

       !  attribute definition
       !print * , ' attribute definition '
    status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'standard_name',&
          LEN(TRIM(tpreclist(1)%ncname)),TRIM(tpreclist(1)%ncname))
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
!
    IF (LEN(TRIM(tpreclist(1)%ncunit)) > 0 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'units',&
        LEN(TRIM(tpreclist(1)%ncunit)),TRIM(tpreclist(1)%ncunit))
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END IF
!
    status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'long_name',&
     LEN(TRIM(tpreclist(1)%long_name)),TRIM(tpreclist(1)%long_name))
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
!
    IF ( tpreclist(1)%grid == 2 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'stagger',&
                               1,"X")
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    ELSEIF ( tpreclist(1)%grid == 3 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'stagger',&
                               1,"Y")
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    ELSEIF ( tpreclist(1)%grid == 4 ) THEN
      status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(1)%id,'stagger',&
                               1,"Z")
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END IF
!!
    
  END SUBROUTINE def_ncdf

  SUBROUTINE FILL_NCDF(varID,tpreclist,pfield)
    USE MODD_TIME_n, ONLY: TDTCUR,TDTMOD
    USE MODD_GRID
    USE MODD_GRID_n
    USE MODD_CONF, ONLY: LCARTESIAN
    TYPE(workfield), DIMENSION(:), INTENT(IN):: tpreclist    
    REAL,DIMENSION(IDIMX*IDIMY*IDIMZ*IDIMT*IDIMN*IDIMP),TARGET,INTENT(IN) ::PFIELD
    REAL(KIND=4),DIMENSION(IDIMX*IDIMY*IDIMZ*IDIMT*IDIMN*IDIMP) ::WFIELD
    REAL(KIND=4)                             ::tfield
    INTEGER                                  :: i
    INTEGER                                  :: status
    INTEGER                                  :: ileng
    INTEGER                                  :: ipos
    INTEGER                                  :: extent
    INTEGER                                  :: ich,iiu,iju
    INTEGER (KIND=4)                         :: levId,timeId
    INTEGER (KIND=4)                         :: varID
    INTEGER (KIND=4)                         :: yy,mm,dd,ss,yy_cur,mm_cur
    REAL                                     :: D,D_cur ! day of month (+ fraction)
    REAL                                     :: JD,JD_cur ! Julian day
    INTEGER                                  :: A, B, A_cur, B_cur  ! intermediate variables


    !

      IF (ASSOCIATED(tpreclist(1)%dim)) THEN
        extent = tpreclist(1)%dim%len
      ELSE
        extent = 1
      END IF

    ! FIRST WRITE TIME
      IF ( WRITETIME ) THEN
        yy = TDTMOD%TDATE%YEAR
        mm = TDTMOD%TDATE%MONTH
        D = TDTMOD%TDATE%DAY
        yy_cur = TDTCUR%TDATE%YEAR
        mm_cur = TDTCUR%TDATE%MONTH
        D_cur = TDTCUR%TDATE%DAY
!!!! Calcul of Julian day
        IF (mm .LE. 2) THEN
          yy = yy - 1
          mm = mm + 12
        END IF
        IF (mm_cur .LE. 2) THEN
          yy_cur = yy_cur - 1
          mm_cur = mm_cur + 12
        END IF
        A = yy/100
        B = 2 - A + A/4
        JD = INT(365.25D0*(yy+4716)) + INT(30.6001D0*(mm+1)) + D + B - 1524.5D0
        A_cur = yy_cur/100
        B_cur = 2 - A_cur + A_cur/4
        JD_cur = INT(365.25D0*(yy_cur+4716)) + INT(30.6001D0*(mm_cur+1)) + D_cur + B_cur - 1524.5D0
        ss = TDTCUR%TIME-TDTMOD%TIME
        ss = ss + (JD_cur - JD ) * (24*3600)
        tfield=ss
        status = NF_PUT_VAR_REAL(kcdf_id,1,tfield)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
        WRITETIME = .FALSE.
        !!!!!!!!! Write also vertical level
      !  status = NF_PUT_VAR_REAL(kcdf_id,2,XZZ)
      !  IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      END IF


      SELECT CASE(tpreclist(1)%TYPE)
        CASE (INT2,BOOL)
          status = NF_PUT_VAR_INT(kcdf_id,varID,pfield)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
          
        CASE (FLOAT2)
          wfield=pfield
          status = NF_PUT_VAR_REAL(kcdf_id,varID,wfield)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

        CASE (TEXT)
          status = NF_PUT_VAR_TEXT(kcdf_id,varID,pfield)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

        CASE default
          wfield=pfield
          status = NF_PUT_VAR_REAL(kcdf_id,varID,wfield)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      END SELECT

    !!!! Add vertical levels ??

  END SUBROUTINE fill_ncdf

  SUBROUTINE NC_WRIT_ll(hvnam,hfnam,hgrid,tdim,pfield,oreal,tpreclist,hlen,hcomment)

  CHARACTER(LEN=*)                         :: hvnam
  CHARACTER(LEN=*), INTENT(IN)             :: hfnam
  INTEGER, DIMENSION(6), INTENT(IN)        :: tdim
  REAL, DIMENSION(tdim(1)*tdim(2)*tdim(3)*tdim(4)*tdim(5)*tdim(6)), INTENT(IN)     :: pfield
  TYPE(workfield), DIMENSION(:),POINTER    :: tpreclist    
  INTEGER          , INTENT(IN)            :: hgrid,hlen
  LOGICAL,                 INTENT(IN) :: OREAL  ! TRUE IF TRANSMITTED KFIELD IS
  CHARACTER(LEN=hlen), INTENT(IN)          :: hcomment
  INTEGER                                  :: status
  INTEGER (KIND=4)                         :: varID
  CHARACTER(LEN=42)                        :: filename, basename
  INTEGER                                  :: omode
  LOGICAL                                  :: FEXIST
  CHARACTER(LEN=4)                         :: ypextsrc, ypextdest
  INTEGER                                  :: iverb
  INTEGER                                  :: pos


  IDIMX = tdim(1)
  IDIMY = tdim(2)
  IDIMZ = tdim(3)
  IDIMT = tdim(4)
  IDIMN = tdim(5)
  IDIMP = tdim(6)

  !   OPEN FILE IF NOT OPEN YET
  filename = trim(hfnam)
  ypextdest = '.nc'
  IF ( LEN(NC_FILE) .eq. 0 ) THEN
    filename = trim(hfnam)//ypextdest
  ELSE
    filename = trim(hfnam)//TRIM(NC_FILE)//ypextdest
  END IF
  pos = INDEX(hvnam,'%')
  IF ( pos /= 0  ) THEN
    hvnam = trim(hvnam)
    hvnam = str_replace(hvnam,'%','_')
  END IF
  pos = INDEX(hvnam,'.')
  IF ( pos /= 0  ) THEN
    hvnam = trim(hvnam)
    hvnam = str_replace(hvnam,'.','_')
  END IF

  IF ( DEF_NC .AND. NC_WRITE ) THEN

    INQUIRE(FILE=TRIM(filename), EXIST=FEXIST)
!    print * , ' FEXIST = ' , FEXIST

    IF ( .not.FEXIST ) then
      PRINT *,'--> Fichier converti : ',TRIM(filename)
      iverb = 0
      CALL init_sysfield()

! -> NetCDF
    
      status = NF_CREATE(TRIM(filename), &
            & IOR(NF_CLOBBER,NF_64BIT_OFFSET),kcdf_id)
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      status = NF_SET_FILL(kcdf_id,NF_NOFILL,omode)
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

!!! Status for dim creation
      CREATEX = .TRUE.
      CREATEY = .TRUE.
      CREATEZ = .TRUE.
      CREATEXR = .TRUE.
      CREATEDATE = .TRUE.
      CREATEWL = .TRUE.
      WRITETIME = .TRUE.
      NUMDIM = 0
      first_var=hvnam

    END IF
!  END IF

!  IF ( DEF_NC .AND. NC_WRITE ) THEN
    status=NF_INQ_VARID(kcdf_id,hvnam,varId)
! Check to see if variable already exist
! because some variables a written twice ..!!??
    IF ( status /= 0 ) THEN
      CALL PARSE_PFIELD(hvnam,hgrid,pfield,tpreclist,hcomment)
      CALL DEF_NCDF(tpreclist)
      last_var=hvnam
    END IF
  ELSE
    IF (hvnam.eq.first_var) then
      status = NF_ENDDEF(kcdf_id)
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      WRITETIME=.TRUE.
    END IF
    CALL PARSE_PFIELD(hvnam,hgrid,pfield,tpreclist,hcomment)
    status=NF_INQ_VARID(kcdf_id,hvnam,varId)
    CALL FILL_NCDF(varID,tpreclist,pfield)
!   CLOSE NETCDF FILE
    IF (hvnam.eq.last_var) then
      status = NF_CLOSE(kcdf_id)
      IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      NC_WRITE = .FALSE.
    END IF
  END IF

  END SUBROUTINE NC_WRIT_ll

END MODULE mode_util
#endif
