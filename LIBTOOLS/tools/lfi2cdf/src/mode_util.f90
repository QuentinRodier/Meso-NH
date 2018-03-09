MODULE mode_util
  USE MODD_IO_ll, ONLY: TFILE_ELT
  USE MODD_PARAM

  USE mode_dimlist
  USE MODE_FIELD
  USE MODE_FIELDTYPE
  USE MODE_FMREAD
  USE MODE_FMWRIT
  USE mode_options
  USE netcdf

  IMPLICIT NONE 

  INTEGER,PARAMETER :: MAXRAW=10
  INTEGER,PARAMETER :: MAXLEN=512
  INTEGER,PARAMETER :: MAXFILES=100
  INTEGER,PARAMETER :: MAXLFICOMMENTLENGTH=100

  INTEGER,PARAMETER :: UNDEFINED = -1, READING = 1, WRITING = 2
  INTEGER,PARAMETER :: UNKNOWN_FORMAT = -1, NETCDF_FORMAT = 1, LFI_FORMAT = 2

  TYPE filestruct
    INTEGER :: lun_id                  ! Logical ID of file
    INTEGER :: format = UNKNOWN_FORMAT ! NETCDF, LFI
    INTEGER :: status = UNDEFINED      ! Opened for reading or writing
    INTEGER :: var_id                  ! Position of the variable in the workfield structure
    LOGICAL :: opened = .false.
  END TYPE filestruct

  TYPE filelist_struct
    INTEGER :: nbfiles = 0
!    TYPE(filestruct),DIMENSION(:),ALLOCATABLE :: files
    TYPE(filestruct),DIMENSION(MAXFILES) :: files
    TYPE(TFILE_ELT),DIMENSION(MAXFILES) :: TFILES
  END TYPE filelist_struct


  TYPE workfield
     CHARACTER(LEN=FM_FIELD_SIZE)            :: name   ! nom du champ
     TYPE(dimCDF),                   POINTER :: dim
     INTEGER                                 :: id_in = -1, id_out = -1
     LOGICAL                                 :: found  ! T if found in the input file
     LOGICAL                                 :: calc   ! T if computed from other variables
     LOGICAL                                 :: tbw    ! to be written or not
     LOGICAL                                 :: tbr    ! to be read or not
     INTEGER,DIMENSION(MAXRAW)               :: src    ! List of variables used to compute the variable (needed only if calc=.true.)
     INTEGER                                 :: tgt    ! Target: id of the variable that use it (calc variable)
     TYPE(TFIELDDATA)                        :: TFIELD ! Metadata about the field
  END TYPE workfield

  LOGICAL(KIND=LFI_INT), PARAMETER :: ltrue  = .TRUE.
  LOGICAL(KIND=LFI_INT), PARAMETER :: lfalse = .FALSE.

  CHARACTER(LEN=6) :: CPROGRAM_ORIG

CONTAINS 
  FUNCTION str_replace(hstr, hold, hnew)
    CHARACTER(LEN=*) :: hstr, hold, hnew
    CHARACTER(LEN=LEN_TRIM(hstr)+MAX(0,LEN(hnew)-LEN(hold))) :: str_replace
    
    INTEGER :: pos
    
    pos = INDEX(hstr,hold)
    IF (pos /= 0) THEN
       str_replace = hstr(1:pos-1)//hnew//hstr(pos+LEN(hold):)
    ELSE 
       str_replace = hstr 
    END IF

  END FUNCTION str_replace

  SUBROUTINE parse_infiles(infiles, nbvar_infile, nbvar_tbr, nbvar_calc, nbvar_tbw, tpreclist, kbuflen, options, icurrent_level)
    USE MODD_DIM_n,         ONLY: NIMAX_ll, NJMAX_ll, NKMAX
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT, JPVEXT

    TYPE(filelist_struct),      INTENT(IN) :: infiles
    INTEGER,                    INTENT(IN) :: nbvar_infile, nbvar_tbr, nbvar_calc, nbvar_tbw
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist
    INTEGER,                   INTENT(OUT) :: kbuflen
    TYPE(option),DIMENSION(:), INTENT(IN)  :: options
    INTEGER,          INTENT(IN), OPTIONAL :: icurrent_level

    INTEGER                                  :: ji,jj, kcdf_id, itype
    INTEGER                                  :: ndb, nde, ndey, idx, idx_var, maxvar
    INTEGER                                  :: idims, idimtmp, jdim, status, var_id
    LOGICAL                                  :: ladvan
    INTEGER                                  :: ich, current_level, leng
    INTEGER                                  :: comment_size, fsize, sizemax
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm, YDATENAME
    CHARACTER(LEN=4)                         :: suffix
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
    INTEGER                                  :: IID, IRESP
    INTEGER(KIND=LFI_INT)                    :: iresp2,ilu,ileng,ipos
    CHARACTER(LEN=FM_FIELD_SIZE)             :: var_calc
    CHARACTER(LEN=FM_FIELD_SIZE),dimension(MAXRAW) :: var_raw
    INTEGER, DIMENSION(10)                   :: idim_id
    INTEGER                                  :: IDXDATE, IDXTIME, IDX1
    LOGICAL                                  :: GISDATE, GISTIME

    IF (infiles%files(1)%format == LFI_FORMAT) THEN
      ilu = infiles%files(1)%lun_id
      ! update IDIMX,IDIMY,IDIMZ
      IDIMX = NIMAX_ll+2*JPHEXT
      IDIMY = NJMAX_ll+2*JPHEXT
      IDIMZ = NKMAX   +2*JPVEXT
    ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
      kcdf_id = infiles%files(1)%lun_id

      status = NF90_INQ_DIMID(kcdf_id, "DIMX", idim_id(1))
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
      status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(1),len = IDIMX)

      status = NF90_INQ_DIMID(kcdf_id, "DIMY", idim_id(2))
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
      status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(2),len = IDIMY)

      status = NF90_INQ_DIMID(kcdf_id, "DIMZ", idim_id(3))
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
      status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(3),len = IDIMZ)
    END IF

    GUSEDIM = (IDIMX*IDIMY > 0)
    IF (GUSEDIM) THEN
      PRINT *,'MESONH 3D, 2D articles DIMENSIONS used :'
      PRINT *,'DIMX =',IDIMX
      PRINT *,'DIMY =',IDIMY
      PRINT *,'DIMZ =',IDIMZ ! IDIMZ may be equal to 0 (PGD files)
    ELSE
      PRINT *,'BEWARE : ALL MesoNH arrays are handled as 1D arrays !'
    END IF

    sizemax = 0

    IF (present(icurrent_level)) THEN
      write(suffix,'(I4.4)') icurrent_level
      current_level = icurrent_level
    ElSE
      suffix=''
      current_level = -1
    END IF

    ! Phase 1 : build articles list to convert.
    !
    !    Pour l'instant tous les articles du fichier LFI sont
    !    convertis. On peut modifier cette phase pour prendre en
    !    compte un sous-ensemble d'article (liste definie par
    !    l'utilisateur par exemple)
    !
    IF (options(OPTVAR)%set) THEN
      ALLOCATE(tpreclist(nbvar_tbr+nbvar_calc))
      DO ji=1,nbvar_tbr+nbvar_calc
        tpreclist(ji)%found  = .FALSE.
        tpreclist(ji)%calc   = .FALSE. !By default variables are not computed from others
        tpreclist(ji)%tbw    = .TRUE.  !By default variables are written
        tpreclist(ji)%tbr    = .TRUE.  !By default variables are read
        tpreclist(ji)%src(:) = -1
        tpreclist(ji)%tgt    = -1
      END DO

       ! A variable list is provided with -v var1,...
       ndb  = 1
       idx_var = 1
       DO ji=1,nbvar_tbw
          !crash compiler GCC 4.2.0: nde = INDEX(TRIM(options(OPTVAR)%cvalue(ndb:)),',')
          nde = INDEX(TRIM(options(OPTVAR)%cvalue(ndb:len(trim(options(OPTVAR)%cvalue)))),',')
          IF (nde == 0) nde = LEN( TRIM(options(OPTVAR)%cvalue(ndb:len(trim(options(OPTVAR)%cvalue)))) ) + 1
          yrecfm = options(OPTVAR)%cvalue(ndb:ndb+nde-2)
          !Detect operations on variables (only + is supported now)
          ndey = INDEX(TRIM(yrecfm),'=')
          idx = 1
          IF (ndey /= 0) THEN
            var_calc = yrecfm(1:ndey-1)
            DO WHILE (ndey /= 0)
              IF (idx>MAXRAW) THEN
                print *,'Error: MAXRAW exceeded (too many raw variables for 1 computed one)'
                STOP
              END IF
              yrecfm = yrecfm(ndey+1:)
              ndey = INDEX(TRIM(yrecfm),'+')
              IF (ndey /= 0) THEN
                var_raw(idx) = yrecfm(1:ndey-1)
              ELSE
                var_raw(idx) = TRIM(yrecfm)
              END IF
              idx = idx + 1
            END DO

            tpreclist(idx_var)%name = trim(var_calc)
            tpreclist(idx_var)%calc = .TRUE.
            tpreclist(idx_var)%tbw  = .TRUE.
            tpreclist(idx_var)%tbr  = .FALSE.
            idx_var=idx_var+1
            DO jj = 1, idx-1
              tpreclist(idx_var-jj)%src(jj) = idx_var
              tpreclist(idx_var)%name = trim(var_raw(jj))
              tpreclist(idx_var)%calc = .FALSE.
              tpreclist(idx_var)%tbw  = .FALSE.
              tpreclist(idx_var)%tbr  = .TRUE.
              tpreclist(idx_var)%tgt  = idx_var-jj
              idx_var=idx_var+1
            END DO

          ELSE
            tpreclist(idx_var)%name = trim(yrecfm)
            tpreclist(idx_var)%calc = .FALSE.
            tpreclist(idx_var)%tbw  = .TRUE.
            idx_var=idx_var+1

          END IF

          ndb = nde+ndb
       END DO

       DO ji=1,nbvar_tbr+nbvar_calc
          IF (tpreclist(ji)%calc) CYCLE

          yrecfm = TRIM(tpreclist(ji)%name)
          IF (infiles%files(1)%format == LFI_FORMAT) THEN
            CALL LFINFO(iresp2,ilu,trim(yrecfm)//trim(suffix),ileng,ipos)
            IF (iresp2 == 0 .AND. ileng /= 0) tpreclist(ji)%found = .true.
            leng = ileng
          ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
            status = NF90_INQ_VARID(kcdf_id,trim(yrecfm)//trim(suffix),tpreclist(ji)%id_in)
            IF (status == NF90_NOERR) THEN
              tpreclist(ji)%found = .true.
              status = NF90_INQUIRE_VARIABLE(kcdf_id,tpreclist(ji)%id_in,ndims = idims,dimids = idim_id)
              IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!TODO:useful?
!DUPLICATED
              IF (idims == 0) THEN
                 ! variable scalaire
                 leng = 1
              ELSE
                 ! infos sur dimensions
                 leng = 1
                 DO jdim=1,idims
                   status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(jdim),len = idimtmp)
                   IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
                   leng = leng*idimtmp
                END DO
              END IF
            END IF
            !Add maximum comment size (necessary when writing LFI files because the comment is stored with the field)
            leng = leng + MAXLFICOMMENTLENGTH
          END IF

          IF (.NOT.tpreclist(ji)%found) THEN
             PRINT *,'Article ',TRIM(yrecfm), ' not found!'
             tpreclist(ji)%tbw   = .FAlSE.
             tpreclist(ji)%tbr   = .FAlSE.
          ELSE
             ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
             IF (leng > sizemax) sizemax = leng
          END IF
       END DO

       maxvar = nbvar_tbr+nbvar_calc
DO ji=1,nbvar_tbr+nbvar_calc
  print *,ji,'name=',trim(tpreclist(ji)%name),' calc=',tpreclist(ji)%calc,' tbw=',tpreclist(ji)%tbw,&
          ' tbr=',tpreclist(ji)%tbr,' found=',tpreclist(ji)%found
END DO

    ELSE
       ! Entire file is converted
       ALLOCATE(tpreclist(nbvar_infile))
       DO ji=1,nbvar_infile
         tpreclist(ji)%calc   = .FALSE. !By default variables are not computed from others
         tpreclist(ji)%tbw    = .TRUE.  !By default variables are written
         tpreclist(ji)%src(:) = -1
       END DO

       IF (infiles%files(1)%format == LFI_FORMAT) THEN
         CALL LFIPOS(iresp2,ilu)
         ladvan = .TRUE.

         GISDATE = .FALSE.
         GISTIME = .FALSE.
         YDATENAME = ''
         DO ji=1,nbvar_infile
           CALL LFICAS(iresp2,ilu,yrecfm,ileng,ipos,ladvan)
           ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
           tpreclist(ji)%name = trim(yrecfm)
           tpreclist(ji)%found  = .TRUE.
           IF (ileng > sizemax) sizemax = ileng

           !Detect if date variable
           IDXDATE = INDEX(trim(yrecfm),"%TDATE",.TRUE.)
           IDXTIME = INDEX(trim(yrecfm),"%TIME", .TRUE.)
           IF (IDXDATE/=0 .AND. IDXTIME/=0) &
             CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','field in LFI file with %TDATE and %TIME in name '//TRIM(YRECFM))
           IDX = MAX(IDXDATE,IDXTIME)
           IF (IDX>0) THEN
             IF (LEN_TRIM(YDATENAME) == 0) THEN
               !New date name detected
               IDX1 = ji
               YDATENAME = YRECFM(1:IDX-1)
               IF (IDXDATE>0) GISDATE = .TRUE.
               IF (IDXTIME>0) GISTIME = .TRUE.
             ELSE
               !Was already found => other field (date or time) is detected
               IF (TRIM(YDATENAME)/=YRECFM(1:IDX-1)) STOP
               IF (IDXDATE>0) THEN
                 IF (.NOT.GISDATE) THEN
                   GISDATE = .TRUE.
                   IF (GISTIME) THEN
                     tpreclist(ji)%name  = 'removed_time'
                     tpreclist(ji)%tbw   = .FALSE.
                     tpreclist(ji)%tbr   = .FALSE.
                     tpreclist(ji)%found = .FALSE.
                     tpreclist(IDX1)%name = YDATENAME
                     !
                     GISDATE = .FALSE.
                     GISTIME = .FALSE.
                     YDATENAME = ''
                   END IF
                 ELSE
                   CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','GISDATE is already TRUE for '//TRIM(YDATENAME))
                 END IF
               ELSE IF (IDXTIME>0) THEN
                 IF (.NOT.GISTIME) THEN
                   GISTIME = .TRUE.
                   IF (GISDATE) THEN
                     tpreclist(ji)%name  = 'removed_date'
                     tpreclist(ji)%tbw   = .FALSE.
                     tpreclist(ji)%tbr   = .FALSE.
                     tpreclist(ji)%found = .FALSE.
                     tpreclist(IDX1)%name = YDATENAME
                     !
                     GISDATE = .FALSE.
                     GISTIME = .FALSE.
                     YDATENAME = ''
                   END IF
                 ELSE
                   CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','GISTIME is already TRUE for '//TRIM(YDATENAME))
                 END IF
               END IF
             END IF
           END IF
         END DO
         !
         ! Check if variable is in TFIELDLIST and populate corresponding metadata
         DO ji=1,nbvar_infile
           IF (.NOT.tpreclist(ji)%found) CYCLE
           !
           CALL FIND_FIELD_ID_FROM_MNHNAME(tpreclist(ji)%name,IID,IRESP)
           IF (IRESP==0) THEN
             tpreclist(ji)%TFIELD = TFIELDLIST(IID)
           ELSE !Field not found in list
             CALL PRINT_MSG(NVERB_WARNING,'IO','parse_infiles','variable '//TRIM(tpreclist(ji)%name)//' is not known => ignored')
             tpreclist(ji)%tbw   = .FALSE.
             tpreclist(ji)%tbr   = .FALSE.
             tpreclist(ji)%found = .FALSE.
           END IF
         END DO
         !
       ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         DO ji=1,nbvar_infile
           tpreclist(ji)%id_in = ji
           status = NF90_INQUIRE_VARIABLE(kcdf_id,tpreclist(ji)%id_in, name = tpreclist(ji)%name, ndims = idims, &
                                          dimids = idim_id)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
           ! PRINT *,'Article ',ji,' : ',TRIM(tpreclist(ji)%name),', longueur = ',ileng
           tpreclist(ji)%found  = .TRUE.
!TODO:useful?
!DUPLICATED
           IF (idims == 0) THEN
             ! variable scalaire
             leng = 1
           ELSE
             ! infos sur dimensions
             leng = 1
             DO jdim=1,idims
               status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(jdim),len = idimtmp)
               IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
               leng = leng*idimtmp
             END DO
           END IF
           IF (leng > sizemax) sizemax = leng
         END DO
         !Add maximum comment size (necessary when writing LFI files because the comment is stored with the field)
         sizemax = sizemax + MAXLFICOMMENTLENGTH
       END IF

       maxvar = nbvar_infile
    END IF

    kbuflen = sizemax

    WRITE(*,'("Taille maximale du buffer :",f10.3," Mio")') sizemax*8./1048576.
    ALLOCATE(iwork(sizemax))

    ! Phase 2 : Extract comments and dimensions for valid articles.
    !           Infos are put in tpreclist.
    CALL init_dimCDF()
    DO ji=1,maxvar
       IF (tpreclist(ji)%calc .OR. .NOT.tpreclist(ji)%found) CYCLE

       IF (infiles%files(1)%format == LFI_FORMAT) THEN
         yrecfm = trim(tpreclist(ji)%name)//trim(suffix)

         !(temporary) workaround for DATE fields
         IF (tpreclist(ji)%TFIELD%NTYPE == TYPEDATE) YRECFM = TRIM(YRECFM)//'%TDATE'

         CALL LFINFO(iresp2,ilu,yrecfm,ileng,ipos)
         CALL LFILEC(iresp2,ilu,yrecfm,iwork,ileng)
         comment_size = iwork(2)
         fsize = ileng-(2+comment_size)

       ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
!DUPLICATED
         IF (idims == 0) THEN
           ! variable scalaire
           leng = 1
         ELSE
           ! infos sur dimensions
           leng = 1
           DO jdim=1,idims
             status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(jdim),len = idimtmp)
             IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
             leng = leng*idimtmp
           END DO
         END IF

         fsize = leng
       END IF

       tpreclist(ji)%dim=>get_dimCDF(fsize)
    END DO

    !Complete info for calculated variables
    IF (nbvar_calc>0) THEN
    DO ji=1,maxvar
       IF (.NOT.tpreclist(ji)%calc) CYCLE
       tpreclist(ji)%dim  => tpreclist(tpreclist(ji)%src(1))%dim
    END DO
    END IF

    PRINT *,'Nombre de dimensions = ', size_dimCDF()
    DEALLOCATE(iwork)
  END SUBROUTINE parse_infiles
  
  SUBROUTINE HANDLE_ERR(status,line)
    INTEGER :: status,line

    IF (status /= NF90_NOERR) THEN
       PRINT *, 'line ',line,': ',NF90_STRERROR(status)
           STOP
    END IF
  END SUBROUTINE HANDLE_ERR

  SUBROUTINE def_ncdf(outfiles,tpreclist,nbvar,options)
    TYPE(filelist_struct),       INTENT(IN) :: outfiles
    TYPE(workfield),DIMENSION(:),INTENT(INOUT) :: tpreclist
    INTEGER,                     INTENT(IN) :: nbvar
    TYPE(option),DIMENSION(:),   INTENT(IN) :: options

    INTEGER :: compress_level, status
    INTEGER :: idx, ji, nbfiles
    INTEGER:: kcdf_id
    TYPE(dimCDF), POINTER :: tzdim
    INTEGER               :: invdims
    INTEGER               :: type_float
    INTEGER, DIMENSION(10) :: ivdims
    CHARACTER(LEN=20)     :: ycdfvar


    nbfiles = outfiles%nbfiles

    IF (options(OPTREDUCE)%set) THEN
      type_float = NF90_REAL
    ELSE
      type_float = NF90_DOUBLE
    END IF

    DO ji = 1,nbfiles
      kcdf_id = outfiles%files(ji)%lun_id

      ! global attributes
      status = NF90_PUT_ATT(kcdf_id,NF90_GLOBAL,'Title',VERSION_ID)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

      ! define DIMENSIONS
      tzdim=>first_DimCDF()
      DO WHILE(ASSOCIATED(tzdim))
        IF (tzdim%create) THEN
          status = NF90_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
        END IF
        tzdim=>tzdim%next
      END DO
    END DO

    PRINT *,'------------- NetCDF DEFINITION ---------------'

    ! define VARIABLES and ATTRIBUTES
    idx = 1
    DO ji=1,nbvar
       IF (.NOT.tpreclist(ji)%tbw) CYCLE

       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
         IF (tpreclist(ji)%dim%create) THEN
           invdims   = 1
           ivdims(1) = tpreclist(ji)%dim%id
         ELSE
           invdims = tpreclist(ji)%dim%ndims
           IF(options(OPTMERGE)%set) invdims=invdims+1 !when merging variables from LFI splitted files
           SELECT CASE(invdims)
           CASE(2)
              ivdims(1)=ptdimx%id
              ivdims(2)=ptdimy%id
           CASE(3)
              ivdims(1)=ptdimx%id
              ivdims(2)=ptdimy%id
              ivdims(3)=ptdimz%id
           CASE(12)
              ivdims(1)=ptdimx%id
              ivdims(2)=ptdimz%id
              invdims = 2 ! on retablit la bonne valeur du nbre de dimension
           CASE default
             PRINT *,'Fatal error in NetCDF dimension definition'
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
       !! "%" remplaces par '__' 
       ycdfvar = str_replace(tpreclist(ji)%name,'%','__')
       !! ni les '.' remplaces par '--'
       ycdfvar = str_replace(ycdfvar,'.','--')

       kcdf_id = outfiles%files(idx)%lun_id

       SELECT CASE(tpreclist(ji)%TFIELD%NTYPE)
       CASE (TYPECHAR)
!          PRINT *,'TYPECHAR : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_CHAR,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE (TYPEINT)
!          PRINT *,'TYPEINT : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_INT,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE (TYPELOG)
!          PRINT *,'TYPELOG : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_INT1,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE(TYPEREAL)
!          PRINT *,'TYPEREAL : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,type_float,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE(TYPEDATE)
!          PRINT *,'TYPEDATE : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_DOUBLE,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

          
       CASE default
          PRINT *,'ATTENTION : ',TRIM(tpreclist(ji)%name),' est de&
               & TYPE inconnu --> force a REAL'
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,type_float,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
          

       END SELECT

       ! Compress data (costly operation for the CPU)
       IF (options(OPTCOMPRESS)%set .AND. invdims>0) THEN
         compress_level = options(OPTCOMPRESS)%ivalue
         status = NF90_DEF_VAR_DEFLATE(kcdf_id,tpreclist(ji)%id_out,1,1,compress_level)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       END IF


       IF (options(OPTSPLIT)%set) idx = idx + 1
    END DO
    
    DO ji = 1,nbfiles
      kcdf_id = outfiles%files(ji)%lun_id
      status = NF90_ENDDEF(kcdf_id)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END DO

  END SUBROUTINE def_ncdf

  SUBROUTINE fill_ncdf(infiles,outfiles,tpreclist,knaf,kbuflen,options,current_level)
    USE MODD_TYPE_DATE

    TYPE(filelist_struct),        INTENT(IN):: infiles, outfiles
    TYPE(workfield), DIMENSION(:),INTENT(IN):: tpreclist
    INTEGER,                      INTENT(IN):: knaf
    INTEGER,                      INTENT(IN):: kbuflen
    TYPE(option),DIMENSION(:),    INTENT(IN):: options
    INTEGER, INTENT(IN), OPTIONAL           :: current_level

    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
    INTEGER                                  :: idx, ji,jj
    INTEGER                                  :: kcdf_id
    INTEGER                                  :: status
    INTEGER                                  :: extent, ndims
    INTEGER                                  :: ich
    INTEGER                                  :: IID, IRESP2
    INTEGER                                  :: src
    INTEGER                                  :: level
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
    CHARACTER(LEN=4)                         :: suffix
    INTEGER,DIMENSION(3)                     :: idims, start
    INTEGER,DIMENSION(:),ALLOCATABLE         :: itab
    REAL(KIND=8),DIMENSION(:),ALLOCATABLE    :: xtab
    CHARACTER, DIMENSION(:), ALLOCATABLE     :: ytab
    REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: xtab3d, xtab3d2
    INTEGER,      DIMENSION(:,:,:), ALLOCATABLE :: itab3d, itab3d2
    TYPE(DATE_TIME)                          :: TZDATE

    !
    IF (infiles%files(1)%format == LFI_FORMAT) ilu = infiles%files(1)%lun_id
    !

    IF (present(current_level)) THEN
      write(suffix,'(I4.4)') current_level
      level = current_level
    ElSE
      suffix=''
      level = 1
    END IF

    ALLOCATE(iwork(kbuflen))
    ALLOCATE(itab(kbuflen))
    ALLOCATE(xtab(kbuflen))

    idx = 1
    DO ji=1,knaf
       IF (.NOT.tpreclist(ji)%tbw) CYCLE

       kcdf_id = outfiles%files(idx)%lun_id

       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
          extent = tpreclist(ji)%dim%len
          ndims = tpreclist(ji)%dim%ndims
       ELSE
          extent = 1
          ndims = 0
       END IF

! PRINT *,'Dimensions (',ndims,') for ',TRIM(tpreclist(ji)%name)
       idims(:) = 1
       if(ndims>0) idims(1) = ptdimx%len
       if(ndims>1) idims(2) = ptdimy%len
       if(ndims>2) idims(3) = ptdimz%len
       if(ndims>3) then
         if(ndims==12) then
           ndims = 2
           idims(2) = ptdimz%len
         else
           PRINT *,'Too many dimensions (',ndims,') for ',TRIM(tpreclist(ji)%name)
           STOP
         endif
       endif

!write(*,"( '----------------------------------------' )")
!write(*,"( 'Field :',A )") trim(tpreclist(ji)%name)

       SELECT CASE(tpreclist(ji)%TFIELD%NTYPE)
       CASE (TYPEINT)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
             itab(1:extent) = itab(1:extent) + iwork(3+iwork(2):3+iwork(2)+extent-1)
             jj=jj+1
           END DO
         ENDIF

!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab(1:extent),count=(/extent/))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(itab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(itab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         ALLOCATE( itab3d(idims(1),idims(2),idims(3)) )
         IF (.NOT.tpreclist(ji)%calc) THEN
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(ji)%id_in,itab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         ELSE
           ALLOCATE( itab3d2(idims(1),idims(2),idims(3)) )
           src=tpreclist(ji)%src(1)
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,itab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,itab3d2)
             IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
             itab3d(:,:,:) = itab3d(:,:,:) + itab3d2(:,:,:)
             jj=jj+1
           END DO
           DEALLOCATE(itab3d2)
         END IF

!TODO: not clean, should be done only if merging z-levels
         IF (ndims == 2) THEN
           start = (/1,1,level/)
         ELSE
           start = (/1,1,1/)
         ENDIF
         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab3d,start=start)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         DEALLOCATE(itab3d)
        END IF

         
       CASE (TYPELOG)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
             itab(1:extent) = itab(1:extent) + iwork(3+iwork(2):3+iwork(2)+extent-1)
             jj=jj+1
           END DO
         ENDIF
         SELECT CASE(ndims)
         CASE (0)
             status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab(1:extent),count=(/extent/))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab(1:extent),count=(/extent/))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         ALLOCATE( itab3d(idims(1),idims(2),idims(3)) )
         IF (.NOT.tpreclist(ji)%calc) THEN
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(ji)%id_in,itab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         ELSE
           ALLOCATE( itab3d2(idims(1),idims(2),idims(3)) )
           src=tpreclist(ji)%src(1)
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,itab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,itab3d2)
             IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
             itab3d(:,:,:) = itab3d(:,:,:) + itab3d2(:,:,:)
             jj=jj+1
           END DO
           DEALLOCATE(itab3d2)
         END IF

         start = (/1,1,1/)
         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab3d,start=start)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         DEALLOCATE(itab3d)
        END IF


       CASE (TYPEREAL)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
           xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
           xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
             xtab(1:extent) = xtab(1:extent) + TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
             jj=jj+1
           END DO
         ENDIF
!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,xtab(1:extent),count=(/extent/))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,xtab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(xtab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(xtab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         ALLOCATE( xtab3d(idims(1),idims(2),idims(3)) )
         IF (.NOT.tpreclist(ji)%calc) THEN
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(ji)%id_in,xtab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         ELSE
           ALLOCATE( xtab3d2(idims(1),idims(2),idims(3)) )
           src=tpreclist(ji)%src(1)
           status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,xtab3d)
           IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(src)%id_in,xtab3d2)
             IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
             xtab3d(:,:,:) = xtab3d(:,:,:) + xtab3d2(:,:,:)
             jj=jj+1
           END DO
           DEALLOCATE(xtab3d2)
         END IF

!TODO: not clean, should be done only if merging z-levels
         IF (ndims == 2) THEN
           start = (/1,1,level/)
         ELSE
           start = (/1,1,1/)
         ENDIF
         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,xtab3d,start=start)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         DEALLOCATE(xtab3d)
        END IF

       CASE (TYPECHAR)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
         CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
         ALLOCATE(ytab(extent))
         DO jj=1,extent
           ich = iwork(2+iwork(2)+jj)
           ytab(jj) = CHAR(ich)
         END DO
         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,ytab,count=(/extent/))
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         DEALLOCATE(ytab)
        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         status = NF90_GET_VAR(infiles%files(1)%lun_id,tpreclist(ji)%id_in,ytab,count=(/extent/))
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,ytab,count=(/extent/))
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
        END IF

       CASE (TYPEDATE)
         IF (ndims/=0) CALL PRINT_MSG(NVERB_FATAL,'IO','fill_ncdf','only ndims=0 is supported for TYPEDATE')
         CALL FIND_FIELD_ID_FROM_MNHNAME(trim(tpreclist(ji)%name),IID,IRESP2)
         IF (IRESP2/=0) &
           CALL PRINT_MSG(NVERB_FATAL,'IO','fill_ncdf','TYPEDATE variable '//TRIM(tpreclist(ji)%name)//' not found in TFIELDLIST')
         CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   TFIELDLIST(IID),TZDATE)
         CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,TFIELDLIST(IID),TZDATE)

       CASE default
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
           xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
           xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name)//trim(suffix),iwork,ileng)
             xtab(1:extent) = xtab(1:extent) + TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
             jj=jj+1
           END DO
         ENDIF
!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,xtab(1:extent),count=(/extent/))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,xtab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(xtab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(xtab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         print *,'Error: unknown datatype'
         STOP
        END IF

       END SELECT

       if (options(OPTSPLIT)%set) idx = idx + 1
    END DO
    DEALLOCATE(itab,xtab)
    DEALLOCATE(iwork)
  END SUBROUTINE fill_ncdf

  SUBROUTINE build_lfi(infiles,outfiles,tpreclist,kbuflen)
    TYPE(filelist_struct),         INTENT(IN) :: infiles, outfiles
    TYPE(workfield), DIMENSION(:), INTENT(IN) :: tpreclist
    INTEGER,                       INTENT(IN) :: kbuflen
    
    INTEGER :: kcdf_id, status
    INTEGER :: ivar,ji,jj,ndims
    INTEGER,DIMENSION(3) :: idims
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: iwork
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: idata
    REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: xtab3d
    INTEGER,      DIMENSION(:,:,:), ALLOCATABLE :: itab3d
    CHARACTER,    DIMENSION(:), ALLOCATABLE :: ytab
    CHARACTER(LEN=FM_FIELD_SIZE)            :: yrecfm

    INTEGER :: iartlen, idlen, icomlen
    INTEGER(KIND=LFI_INT) :: iresp,ilu,iartlen8


    ilu = outfiles%files(1)%lun_id
    kcdf_id = infiles%files(1)%lun_id

    ! Un article LFI est compose de :
    !   - 1 entier identifiant le numero de grille
    !   - 1 entier contenant la taille du commentaire
    !   - le commentaire code en entier 64 bits
    !   - les donnees proprement dites

    PRINT *,'Taille buffer = ',2+kbuflen

    ALLOCATE(iwork(2+kbuflen))

    DO ivar=1,SIZE(tpreclist)
       icomlen = LEN(tpreclist(ivar)%TFIELD%CCOMMENT)
       IF (icomlen > MAXLFICOMMENTLENGTH) THEN
         PRINT *,'ERROR: comment length is too big. Please increase MAXLFICOMMENTLENGTH'
         STOP
       END IF

       ! traitement Grille et Commentaire
       iwork(1) = tpreclist(ivar)%TFIELD%NGRID
       iwork(2) = icomlen
       DO jj=1,iwork(2)
          iwork(2+jj)=ICHAR(tpreclist(ivar)%TFIELD%CCOMMENT(jj:jj))
       END DO

       IF (ASSOCIATED(tpreclist(ivar)%dim)) THEN
          idlen = tpreclist(ivar)%dim%len
          ndims = tpreclist(ivar)%dim%ndims
       ELSE 
          idlen = 1
          ndims = 0
       END IF
       
       idims(:) = 1
       if(ndims>0) idims(1) = ptdimx%len
       if(ndims>1) idims(2) = ptdimy%len
       if(ndims>2) idims(3) = ptdimz%len
       if(ndims>3) then
         PRINT *,'Too many dimensions'
         STOP
       endif

       iartlen = 2+icomlen+idlen
       idata=>iwork(3+icomlen:iartlen)


       SELECT CASE(tpreclist(ivar)%TFIELD%NTYPE)
       CASE(TYPEINT,TYPELOG)
          ALLOCATE( itab3d(idims(1),idims(2),idims(3)) )
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,itab3d)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TYPEINT,TYPELOG --> ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = RESHAPE( itab3d , (/ idims(1)*idims(2)*idims(3) /) )

          DEALLOCATE(itab3d)

       CASE(TYPEREAL)
          ALLOCATE( xtab3d(idims(1),idims(2),idims(3)) )
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,xtab3d)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TYPEREAL -->    ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = RESHAPE( TRANSFER(xtab3d,(/ 0_8 /),idlen) , (/ idims(1)*idims(2)*idims(3) /) )

          DEALLOCATE(xtab3d)

       CASE(TYPECHAR)
          ALLOCATE(ytab(idlen))
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,ytab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TYPECHAR -->     ',tpreclist(ivar)%name,',len = ',idlen
          DO jj=1,idlen
             idata(jj) = ICHAR(ytab(jj))
          END DO
          
          DEALLOCATE(ytab)

       CASE default
          ALLOCATE( xtab3d(idims(1),idims(2),idims(3)) )
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,xtab3d)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

          PRINT *,'Default (ERROR) -->',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = RESHAPE( TRANSFER(xtab3d,(/ 0_8 /),idlen) , (/ idims(1)*idims(2)*idims(3) /) )

          DEALLOCATE(xtab3d)

       END SELECT
       
       ! Attention restoration des '%' dans le nom des champs LFI
       yrecfm = str_replace(tpreclist(ivar)%name,'__','%')
       ! et des '.'
       yrecfm = str_replace(yrecfm,'--','.')
       iartlen8 = iartlen
       CALL LFIECR(iresp,ilu,yrecfm,iwork,iartlen8)

    END DO
    DEALLOCATE(iwork)

  END SUBROUTINE build_lfi

  SUBROUTINE UPDATE_VARID_IN(infiles,hinfile,tpreclist,nbvar,current_level)
    !Update the id_in for netCDF files (could change from one file to the other)
    TYPE(filelist_struct),         INTENT(IN)    :: infiles
    CHARACTER(LEN=*),              INTENT(IN)    :: hinfile
    TYPE(workfield), DIMENSION(:), INTENT(INOUT) :: tpreclist
    INTEGER,                       INTENT(IN)    :: nbvar
    INTEGER,                       INTENT(IN)    :: current_level

    INTEGER :: ji, status
    CHARACTER(len=4) :: suffix


    if (infiles%files(1)%format /= NETCDF_FORMAT) return

    write(suffix,'(I4.4)') current_level

    DO ji=1,nbvar
      IF (.NOT.tpreclist(ji)%tbr) CYCLE
      status = NF90_INQ_VARID(infiles%files(1)%lun_id,trim(tpreclist(ji)%name)//trim(suffix),tpreclist(ji)%id_in)
      IF (status /= NF90_NOERR .AND. tpreclist(ji)%found) THEN
        tpreclist(ji)%found=.false.
        tpreclist(ji)%tbr=.false.
        tpreclist(ji)%tbw=.false.
        print *,'Error: variable ',trim(tpreclist(ji)%name),' not found anymore in split file'
      END IF
    END DO
  END SUBROUTINE UPDATE_VARID_IN

  SUBROUTINE OPEN_FILES(infiles,outfiles,hinfile,houtfile,nbvar_infile,options,runmode)
    USE MODD_CONF,          ONLY: LCARTESIAN
    USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
    USE MODD_DIM_n,         ONLY: NIMAX_ll, NJMAX_ll, NKMAX
    USE MODD_GRID,          ONLY: XBETA, XRPK, XLAT0, XLON0, XLATORI, XLONORI
    USE MODD_GRID_n,        ONLY: LSLEVE, XXHAT, XYHAT, XZHAT
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT, JPVEXT

    USE MODE_FM,               ONLY: IO_FILE_OPEN_ll, IO_FILE_CLOSE_ll
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST
    TYPE(filelist_struct),INTENT(OUT) :: infiles, outfiles
    CHARACTER(LEN=*), INTENT(IN)  :: hinfile
    CHARACTER(LEN=*), INTENT(IN)  :: houtfile
    INTEGER         , INTENT(OUT) :: nbvar_infile
    TYPE(option),DIMENSION(:),INTENT(IN) :: options
    INTEGER         , INTENT(IN)  :: runmode

    INTEGER                     :: IRESP
    INTEGER                     :: extindex
    INTEGER(KIND=LFI_INT)       :: ilu,iresp2,iverb,inap,inaf
    INTEGER                     :: idx,status
    CHARACTER(LEN=4)            :: ypextsrc, ypextdest
    LOGICAL                     :: fexist
    INTEGER                     :: omode

    iverb = 0

    CALL init_sysfield()

    IF (runmode == MODELFI2CDF) THEN
       ! Cas LFI -> NetCDF
       infiles%nbfiles = infiles%nbfiles + 1
       idx = infiles%nbfiles
       CALL IO_FILE_ADD2LIST(INFILES%TFILES(idx)%TFILE,HINFILE(1:LEN_TRIM(HINFILE)-4),'UNKNOWN','READ', &
                             HFORMAT='LFI',KLFIVERB=0)
       CALL IO_FILE_OPEN_ll(INFILES%TFILES(idx)%TFILE)
       infiles%files(idx)%lun_id = INFILES%TFILES(idx)%TFILE%NLFIFLU
       infiles%files(idx)%format = LFI_FORMAT
       infiles%files(idx)%status = READING
       ilu = infiles%files(idx)%lun_id
       infiles%files(idx)%opened  = .TRUE.

       nbvar_infile = INFILES%TFILES(idx)%TFILE%NLFININAR

       IF (options(OPTLIST)%set) THEN
          CALL LFILAF(iresp2,ilu,lfalse)
          CALL IO_FILE_CLOSE_ll(INFILES%TFILES(idx)%TFILE)
          return
       END IF

       !Read problem dimensions and some grid variables (needed by IO_FILE_OPEN_ll for netCDF files)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'JPHEXT',JPHEXT)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'JPVEXT',JPVEXT,IRESP)
       IF(IRESP/=0) JPVEXT=1
       !
       ALLOCATE(NIMAX_ll,NJMAX_ll,NKMAX)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'IMAX',NIMAX_ll)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'JMAX',NJMAX_ll)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'KMAX',NKMAX)
       !
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'PROGRAM',CPROGRAM_ORIG)
       !
       ALLOCATE(CSTORAGE_TYPE)
       CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'STORAGE_TYPE',CSTORAGE_TYPE)
       !
       IF ( TRIM(CPROGRAM_ORIG)/='PGD' &
         .AND. .NOT.(TRIM(CPROGRAM_ORIG)=='REAL' .AND. CSTORAGE_TYPE=='SU') ) THEN !condition to detect PREP_SURFEX
         ALLOCATE(XXHAT(NIMAX_ll+2*JPHEXT))
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'XHAT',XXHAT)
         ALLOCATE(XYHAT(NJMAX_ll+2*JPHEXT))
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'YHAT',XYHAT)
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'CARTESIAN',LCARTESIAN)
         !
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'LAT0',XLAT0)
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'LON0',XLON0)
         CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'BETA',XBETA)
         !
         IF (.NOT.LCARTESIAN) THEN
           CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'RPK',   XRPK)
           CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'LATORI',XLATORI)
           CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'LONORI',XLONORI)
         ENDIF
         !
         IF (TRIM(CPROGRAM_ORIG)/='NESPGD') THEN
           ALLOCATE(XZHAT(NKMAX+2*JPVEXT))
           CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'ZHAT',XZHAT)
           ALLOCATE(LSLEVE)
           CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'SLEVE',LSLEVE)
         END IF
       END IF

       IF (.NOT.options(OPTSPLIT)%set) THEN
         outfiles%nbfiles = outfiles%nbfiles + 1

         idx = outfiles%nbfiles
         CALL IO_FILE_ADD2LIST(OUTFILES%TFILES(idx)%TFILE,HOUTFILE,'UNKNOWN','WRITE', &
                               HFORMAT='NETCDF4')
         CALL IO_FILE_OPEN_ll(OUTFILES%TFILES(idx)%TFILE,HPROGRAM_ORIG=CPROGRAM_ORIG)
         outfiles%files(idx)%lun_id = OUTFILES%TFILES(idx)%TFILE%NNCID
         outfiles%files(idx)%format = NETCDF_FORMAT
         outfiles%files(idx)%status = WRITING
         outfiles%files(idx)%opened  = .TRUE.

         status = NF90_SET_FILL(outfiles%files(idx)%lun_id,NF90_NOFILL,omode)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
!!$       SELECT CASE(omode)
!!$       CASE (NF90_FILL)
!!$          PRINT *,'Ancien mode : NF90_FILL'
!!$       CASE (NF90_NOFILL)
!!$          PRINT *,'Ancien mode : NF90_NOFILL'
!!$       CASE default
!!$          PRINT *, 'Ancien mode : inconnu'
!!$       END SELECT
         END IF ! .NOT.osplit
       
    ELSE IF (runmode == MODECDF2CDF) THEN
       ! Cas netCDF -> netCDF

       infiles%nbfiles = infiles%nbfiles + 1
       idx = infiles%nbfiles
       status = NF90_OPEN(hinfile,NF90_NOWRITE,infiles%files(idx)%lun_id)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       infiles%files(idx)%opened  = .TRUE.
       infiles%files(idx)%format = NETCDF_FORMAT
       infiles%files(idx)%status = READING

       status = NF90_INQUIRE(infiles%files(idx)%lun_id, nvariables = nbvar_infile)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)


       IF (.NOT.options(OPTSPLIT)%set) THEN
         outfiles%nbfiles = outfiles%nbfiles + 1
         idx = outfiles%nbfiles
         status = NF90_CREATE(houtfile, IOR(NF90_CLOBBER,NF90_NETCDF4), outfiles%files(idx)%lun_id)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         outfiles%files(idx)%opened  = .TRUE.
         outfiles%files(idx)%format = NETCDF_FORMAT
         outfiles%files(idx)%status = WRITING

         status = NF90_SET_FILL(outfiles%files(idx)%lun_id,NF90_NOFILL,omode)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       END IF ! .NOT.osplit

    ELSE
       ! Cas NetCDF -> LFI
       infiles%nbfiles = infiles%nbfiles + 1
       idx = infiles%nbfiles
       status = NF90_OPEN(hinfile,NF90_NOWRITE,infiles%files(idx)%lun_id)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       infiles%files(idx)%opened  = .TRUE.
       infiles%files(idx)%format = NETCDF_FORMAT
       infiles%files(idx)%status = READING
       
       status = NF90_INQUIRE(infiles%files(idx)%lun_id, nvariables = nbvar_infile)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       inap = 100
       outfiles%nbfiles = outfiles%nbfiles + 1
       idx = outfiles%nbfiles
       outfiles%files(idx)%lun_id = 11
       outfiles%files(idx)%format = LFI_FORMAT
       outfiles%files(idx)%status = WRITING
       ilu = outfiles%files(idx)%lun_id
       CALL LFIOUV(iresp2,ilu,ltrue,TRIM(houtfile)//'.lfi','NEW' ,lfalse,lfalse,iverb,inap,inaf)
       outfiles%files(idx)%opened  = .TRUE.
    END IF

    PRINT *,'--> Fichier converti : ', TRIM(houtfile)

  END SUBROUTINE OPEN_FILES

  SUBROUTINE OPEN_SPLIT_LFIFILE_IN(infiles,hinfile,current_level)
    TYPE(filelist_struct), INTENT(INOUT) :: infiles
    CHARACTER(LEN=*), INTENT(IN) :: hinfile
    INTEGER,          INTENT(IN) :: current_level

    INTEGER(KIND=LFI_INT) :: ilu,iresp,iverb,inap,nbvar

    CHARACTER(LEN=3)      :: suffix
    CHARACTER(LEN=:),ALLOCATABLE :: filename


    iverb = 0 !Verbosity level for LFI

    ALLOCATE(character(len=len(hinfile)) :: filename)

    ilu = infiles%files(1)%lun_id !We assume only 1 infile

    write(suffix,'(I3.3)') current_level
    filename=hinfile(1:len(hinfile)-7)//suffix//'.lfi'
    CALL LFIOUV(iresp,ilu,ltrue,filename,'OLD',lfalse,lfalse,iverb,inap,nbvar)
    infiles%files(1)%opened = .TRUE.

    DEALLOCATE(filename)
  END SUBROUTINE OPEN_SPLIT_LFIFILE_IN

  SUBROUTINE OPEN_SPLIT_NCFILE_IN(infiles,hinfile,current_level)
    TYPE(filelist_struct), INTENT(INOUT) :: infiles
    CHARACTER(LEN=*), INTENT(IN) :: hinfile
    INTEGER,          INTENT(IN) :: current_level

    INTEGER :: status
    CHARACTER(LEN=3)      :: suffix
    CHARACTER(LEN=:),ALLOCATABLE :: filename


    ALLOCATE(character(len=len(hinfile)) :: filename)

    write(suffix,'(I3.3)') current_level
    filename=hinfile(1:len(hinfile)-6)//suffix//'.nc'
    status = NF90_OPEN(filename,NF90_NOWRITE,infiles%files(1)%lun_id)
    IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    infiles%files(1)%opened  = .TRUE.

    DEALLOCATE(filename)
  END SUBROUTINE OPEN_SPLIT_NCFILE_IN

  SUBROUTINE OPEN_SPLIT_NCFILES_OUT(outfiles,houtfile,nbvar,tpreclist,options)
    TYPE(filelist_struct),         INTENT(INOUT) :: outfiles
    CHARACTER(LEN=*),              INTENT(IN)    :: houtfile
    INTEGER,                       INTENT(IN)    :: nbvar
    TYPE(workfield), DIMENSION(:), INTENT(IN)    :: tpreclist
    TYPE(option),DIMENSION(:),     INTENT(IN)    :: options

    INTEGER :: ji, idx
    INTEGER :: status
    INTEGER :: omode
    CHARACTER(LEN=MAXLEN) :: filename


    DO ji = 1,nbvar
      IF (tpreclist(ji)%tbw) outfiles%nbfiles = outfiles%nbfiles + 1
    END DO

    idx = 1
    DO ji = 1,nbvar
      IF (.NOT.tpreclist(ji)%tbw) CYCLE
      outfiles%files(idx)%var_id = ji

      filename = trim(houtfile)//'.'//trim(tpreclist(ji)%name)//'.nc'
      status = NF90_CREATE(trim(filename), IOR(NF90_CLOBBER,NF90_NETCDF4), outfiles%files(idx)%lun_id)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

      status = NF90_SET_FILL(outfiles%files(idx)%lun_id,NF90_NOFILL,omode)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

      outfiles%files(idx)%opened  = .TRUE.
      outfiles%files(idx)%format = NETCDF_FORMAT
      outfiles%files(idx)%status = WRITING

      idx = idx + 1
    END DO

  END SUBROUTINE OPEN_SPLIT_NCFILES_OUT
  
  SUBROUTINE CLOSE_FILES(filelist)
    TYPE(filelist_struct),INTENT(INOUT) :: filelist
    
    INTEGER(KIND=LFI_INT) :: ilu,iresp
    INTEGER               :: ji,status

    DO ji=1,filelist%nbfiles
      IF ( .NOT.filelist%files(ji)%opened ) CYCLE

      IF ( filelist%files(ji)%format == LFI_FORMAT ) THEN
        ilu = filelist%files(ji)%lun_id
        CALL LFIFER(iresp,ilu,'KEEP')
      ELSE IF ( filelist%files(ji)%format == NETCDF_FORMAT ) THEN
        status = NF90_CLOSE(filelist%files(ji)%lun_id)
        IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
      END IF
      filelist%files(ji)%opened=.false.
    END DO

  END SUBROUTINE CLOSE_FILES

END MODULE mode_util
