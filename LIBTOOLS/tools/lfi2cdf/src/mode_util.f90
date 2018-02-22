MODULE mode_util
  USE MODE_FIELDTYPE
  USE mode_dimlist
  USE mode_options
  USE MODD_PARAM
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
  END TYPE filelist_struct


  TYPE workfield
     CHARACTER(LEN=FM_FIELD_SIZE)            :: name   ! nom du champ
     INTEGER                                 :: TYPE   ! type (entier ou reel)    
     CHARACTER(LEN=:), POINTER               :: comment
     TYPE(dimCDF),                   POINTER :: dim
     INTEGER                                 :: id_in = -1, id_out = -1
     INTEGER                                 :: grid
     LOGICAL                                 :: found  ! T if found in the input file
     LOGICAL                                 :: calc   ! T if computed from other variables
     LOGICAL                                 :: tbw    ! to be written or not
     LOGICAL                                 :: tbr    ! to be read or not
     INTEGER,DIMENSION(MAXRAW)               :: src    ! List of variables used to compute the variable (needed only if calc=.true.)
     INTEGER                                 :: tgt    ! Target: id of the variable that use it (calc variable)
  END TYPE workfield

#ifndef LOWMEM
  TYPE lfidata
     INTEGER(KIND=8), DIMENSION(:), POINTER :: iwtab
  END TYPE lfidata
  TYPE(lfidata), DIMENSION(:), ALLOCATABLE :: lfiart
#endif

  LOGICAL(KIND=LFI_INT), PARAMETER :: ltrue  = .TRUE.
  LOGICAL(KIND=LFI_INT), PARAMETER :: lfalse = .FALSE.

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

  SUBROUTINE FMREADLFIN1(klu,hrecfm,kval,kresp)
  INTEGER(KIND=LFI_INT), INTENT(IN) :: klu ! logical fortran unit au lfi file
  CHARACTER(LEN=*),INTENT(IN)       :: hrecfm ! article name to be read
  INTEGER, INTENT(OUT)        :: kval ! integer value for hrecfm article
  INTEGER(KIND=LFI_INT), INTENT(OUT):: kresp! return code null if OK
  !
  INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE::iwork
  INTEGER :: icomlen
  INTEGER(KIND=LFI_INT) :: iresp,ilenga,iposex
  !
  CALL LFINFO(iresp,klu,hrecfm,ilenga,iposex)
  IF (iresp /=0 .OR. ilenga == 0) THEN
    kresp = -1
    kval = 0
  ELSE
    ALLOCATE(IWORK(ilenga))
    CALL LFILEC(iresp,klu,hrecfm,iwork,ilenga)
    icomlen = iwork(2)
    kval = iwork(3+icomlen)
    kresp = iresp
    DEALLOCATE(IWORK)
  END IF
  END SUBROUTINE FMREADLFIN1

  SUBROUTINE parse_infiles(infiles, nbvar_infile, nbvar_tbr, nbvar_calc, nbvar_tbw, tpreclist, kbuflen, options, icurrent_level)
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
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm
    CHARACTER(LEN=4)                         :: suffix
#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
    CHARACTER(LEN=FM_FIELD_SIZE)             :: var_calc
    CHARACTER(LEN=FM_FIELD_SIZE),dimension(MAXRAW) :: var_raw
    INTEGER, DIMENSION(10)                   :: idim_id
    INTEGER                                  :: JPHEXT

    IF (infiles%files(1)%format == LFI_FORMAT) THEN
      ilu = infiles%files(1)%lun_id
      CALL FMREADLFIN1(ilu,'JPHEXT',JPHEXT,iresp)
      IF (iresp /= 0) JPHEXT=1

      ! First check if IMAX,JMAX,KMAX exist in LFI file
      ! to handle 3D, 2D variables -> update IDIMX,IDIMY,IDIMZ
      CALL FMREADLFIN1(ilu,'IMAX',IDIMX,iresp)
      IF (iresp == 0) IDIMX = IDIMX+2*JPHEXT  ! IMAX + 2*JPHEXT
       !
      CALL FMREADLFIN1(ilu,'JMAX',IDIMY,iresp)
      IF (iresp == 0) IDIMY = IDIMY+2*JPHEXT  ! JMAX + 2*JPHEXT
      !
      CALL FMREADLFIN1(ilu,'KMAX',IDIMZ,iresp)
      IF (iresp == 0) IDIMZ = IDIMZ+2  ! KMAX + 2*JPVEXT
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
#ifndef LOWMEM
      IF(.NOT.ALLOCATED(lfiart) .AND. infiles%files(1)%format == LFI_FORMAT) ALLOCATE(lfiart(nbvar_tbr+nbvar_calc))
#endif
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
            CALL LFINFO(iresp,ilu,trim(yrecfm)//trim(suffix),ileng,ipos)
            IF (iresp == 0 .AND. ileng /= 0) tpreclist(ji)%found = .true.
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
#ifndef LOWMEM
!TODO:useful for netcdf?
             IF (infiles%files(1)%format == LFI_FORMAT) ALLOCATE(lfiart(ji)%iwtab(leng))
#endif
          END IF
       END DO

       maxvar = nbvar_tbr+nbvar_calc
DO ji=1,nbvar_tbr+nbvar_calc
  print *,ji,'name=',trim(tpreclist(ji)%name),' calc=',tpreclist(ji)%calc,' tbw=',tpreclist(ji)%tbw,&
          ' tbr=',tpreclist(ji)%tbr,' found=',tpreclist(ji)%found
END DO

    ELSE
       ! Entire file is converted
#ifndef LOWMEM
       IF(.NOT.ALLOCATED(lfiart) .AND. infiles%files(1)%format == LFI_FORMAT) ALLOCATE(lfiart(nbvar_infile))
#endif
       ALLOCATE(tpreclist(nbvar_infile))
       DO ji=1,nbvar_infile
         tpreclist(ji)%calc   = .FALSE. !By default variables are not computed from others
         tpreclist(ji)%tbw    = .TRUE.  !By default variables are written
         tpreclist(ji)%src(:) = -1
       END DO

       IF (infiles%files(1)%format == LFI_FORMAT) THEN
         CALL LFIPOS(iresp,ilu)
         ladvan = .TRUE.

         DO ji=1,nbvar_infile
           CALL LFICAS(iresp,ilu,yrecfm,ileng,ipos,ladvan)
           ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
           tpreclist(ji)%name = trim(yrecfm)
           tpreclist(ji)%found  = .TRUE.
           IF (ileng > sizemax) sizemax = ileng
#ifndef LOWMEM
           ALLOCATE(lfiart(ji)%iwtab(ileng))
#endif
         END DO
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

#ifdef LOWMEM
    WRITE(*,'("Taille maximale du buffer :",f10.3," Mio")') sizemax*8./1048576.
    ALLOCATE(iwork(sizemax))
#endif

    ! Phase 2 : Extract comments and dimensions for valid articles.
    !           Infos are put in tpreclist.
    CALL init_dimCDF()
    DO ji=1,maxvar
       IF (tpreclist(ji)%calc .OR. .NOT.tpreclist(ji)%found) CYCLE

       IF (infiles%files(1)%format == LFI_FORMAT) THEN
         yrecfm = trim(tpreclist(ji)%name)//trim(suffix)
         CALL LFINFO(iresp,ilu,yrecfm,ileng,ipos)
#ifdef LOWMEM
         CALL LFILEC(iresp,ilu,yrecfm,iwork,ileng)
         tpreclist(ji)%grid = iwork(1)
         comment_size = iwork(2)
#else
         CALL LFILEC(iresp,ilu,yrecfm,lfiart(ji)%iwtab,ileng)
         tpreclist(ji)%grid = lfiart(ji)%iwtab(1)
         comment_size = lfiart(ji)%iwtab(2)
#endif
         tpreclist(ji)%TYPE = get_ftype(yrecfm,current_level)

         ALLOCATE(character(len=comment_size) :: tpreclist(ji)%comment)
         DO jj=1,comment_size
#ifdef LOWMEM
           ich = iwork(2+jj)
#else
           ich = lfiart(ji)%iwtab(2+jj)
#endif
           tpreclist(ji)%comment(jj:jj) = CHAR(ich)
         END DO

         fsize = ileng-(2+comment_size)

       ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
         ! GRID attribute definition
         status = NF90_GET_ATT(kcdf_id,tpreclist(ji)%id_in,'GRID',tpreclist(ji)%grid)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         ! COMMENT attribute definition
         status = NF90_INQUIRE_ATTRIBUTE(kcdf_id,tpreclist(ji)%id_in,'COMMENT',len=comment_size)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         ALLOCATE(character(len=comment_size) :: tpreclist(ji)%comment)
         status = NF90_GET_ATT(kcdf_id,tpreclist(ji)%id_in,'COMMENT',tpreclist(ji)%comment)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         status = NF90_INQUIRE_VARIABLE(kcdf_id,tpreclist(ji)%id_in, xtype = itype, ndims = idims, &
                                        dimids = idim_id)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

         SELECT CASE(itype)
         CASE(NF90_CHAR)
           tpreclist(ji)%TYPE = TEXT
         CASE(NF90_INT)
           tpreclist(ji)%TYPE = INT
         CASE(NF90_FLOAT,NF90_DOUBLE)
           tpreclist(ji)%TYPE = FLOAT
         CASE default
           PRINT *, 'Attention : variable ',TRIM(tpreclist(ji)%name), ' a un TYPE non reconnu par le convertisseur.'
           PRINT *, '--> TYPE force a REAL(KIND 8) dans LFI !'
         END SELECT

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
       tpreclist(ji)%TYPE = tpreclist(tpreclist(ji)%src(1))%TYPE
       tpreclist(ji)%grid = tpreclist(tpreclist(ji)%src(1))%grid
       tpreclist(ji)%dim  => tpreclist(tpreclist(ji)%src(1))%dim

!TODO: cleaner length!
       ALLOCATE(character(len=256) :: tpreclist(ji)%comment)
       tpreclist(ji)%comment='Constructed from'
       jj = 1
       DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
         tpreclist(ji)%comment = trim(tpreclist(ji)%comment)//' '//trim(tpreclist(tpreclist(ji)%src(jj))%name)
         IF (jj<MAXRAW .AND. tpreclist(ji)%src(jj+1)>0) THEN
           tpreclist(ji)%comment = trim(tpreclist(ji)%comment)//' +'
         END IF
         jj=jj+1
       END DO
    END DO
    END IF


    PRINT *,'Nombre de dimensions = ', size_dimCDF()
#ifdef LOWMEM
    DEALLOCATE(iwork)
#endif
  END SUBROUTINE parse_infiles
  
  SUBROUTINE read_data_lfi(infiles, nbvar, tpreclist, kbuflen, current_level)
    TYPE(filelist_struct),      INTENT(IN) :: infiles
    INTEGER, INTENT(INOUT)                 :: nbvar
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist
    INTEGER, INTENT(IN)                    :: kbuflen
    INTEGER, INTENT(IN), OPTIONAL          :: current_level

    INTEGER                                  :: ji,jj
    INTEGER                                  :: ndb, nde
    LOGICAL                                  :: ladvan
    INTEGER                                  :: ich
    INTEGER                                  :: fsize,sizemax
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm
    CHARACTER(LEN=4)                         :: suffix
#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
    CHARACTER(LEN=FM_FIELD_SIZE)             :: var_calc
    CHARACTER(LEN=FM_FIELD_SIZE),dimension(MAXRAW) :: var_raw


    ilu = infiles%files(1)%lun_id

    IF (present(current_level)) THEN
      write(suffix,'(I4.4)') current_level
    ElSE
      suffix=''
    END IF

#ifdef LOWMEM
    ALLOCATE(iwork(kbuflen))
#endif

    DO ji=1,nbvar
       IF (.NOT.tpreclist(ji)%tbr) CYCLE
       yrecfm = trim(tpreclist(ji)%name)//trim(suffix)
       CALL LFINFO(iresp,ilu,yrecfm,ileng,ipos)
#ifdef LOWMEM
       CALL LFILEC(iresp,ilu,yrecfm,iwork,ileng)
       tpreclist(ji)%grid = iwork(1)
#else
       CALL LFILEC(iresp,ilu,yrecfm,lfiart(ji)%iwtab,ileng)
       tpreclist(ji)%grid = lfiart(ji)%iwtab(1)
#endif
    END DO

#ifdef LOWMEM
    DEALLOCATE(iwork)
#endif
  END SUBROUTINE read_data_lfi

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

       SELECT CASE(tpreclist(ji)%TYPE)
       CASE (TEXT)
!          PRINT *,'TEXT : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_CHAR,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE (INT,BOOL)
!          PRINT *,'INT,BOOL : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_INT,&
                   ivdims(:invdims),tpreclist(ji)%id_out)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE(FLOAT)
!          PRINT *,'FLOAT : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,type_float,&
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

       ! GRID attribute definition
       status = NF90_PUT_ATT(kcdf_id,tpreclist(ji)%id_out,'GRID',tpreclist(ji)%grid)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       ! COMMENT attribute definition
       status = NF90_PUT_ATT(kcdf_id,tpreclist(ji)%id_out,'COMMENT',trim(tpreclist(ji)%comment))
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       IF (options(OPTSPLIT)%set) idx = idx + 1
    END DO
    
    DO ji = 1,nbfiles
      kcdf_id = outfiles%files(ji)%lun_id
      status = NF90_ENDDEF(kcdf_id)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END DO

  END SUBROUTINE def_ncdf

  SUBROUTINE fill_ncdf(infiles,outfiles,tpreclist,knaf,kbuflen,options,current_level)
    TYPE(filelist_struct),        INTENT(IN):: infiles, outfiles
    TYPE(workfield), DIMENSION(:),INTENT(IN):: tpreclist
    INTEGER,                      INTENT(IN):: knaf
    INTEGER,                      INTENT(IN):: kbuflen
    TYPE(option),DIMENSION(:),    INTENT(IN):: options
    INTEGER, INTENT(IN), OPTIONAL           :: current_level

#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER                                  :: idx, ji,jj
    INTEGER                                  :: kcdf_id
    INTEGER                                  :: status
    INTEGER                                  :: extent, ndims
    INTEGER                                  :: ich
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

#if LOWMEM
    ALLOCATE(iwork(kbuflen))
#endif
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

       SELECT CASE(tpreclist(ji)%TYPE)
       CASE (INT,BOOL)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
#if LOWMEM
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
#else
         IF (.NOT.tpreclist(ji)%calc) THEN
           itab(1:extent) = lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):)
         ELSE
           src=tpreclist(ji)%src(1)
           itab(1:extent) = lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             itab(1:extent) = xtab(1:extent) + lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):)
             jj=jj+1
           END DO
         END IF
#endif

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

         
       CASE (FLOAT)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
#if LOWMEM
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
#else
         IF (.NOT.tpreclist(ji)%calc) THEN
           xtab(1:extent) = TRANSFER(lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):),(/ 0.0_8 /))
         ELSE
           src=tpreclist(ji)%src(1)
           xtab(1:extent) = TRANSFER(lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):),(/ 0.0_8 /))
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             xtab(1:extent) = xtab(1:extent) + TRANSFER(lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):),(/ 0.0_8 /))
             jj=jj+1
           END DO
         END IF
#endif
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

       CASE (TEXT)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
#if LOWMEM
         CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
         CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
#endif
         ALLOCATE(ytab(extent))
         DO jj=1,extent
#if LOWMEM
           ich = iwork(2+iwork(2)+jj)
#else
           ich = lfiart(ji)%iwtab(2+lfiart(ji)%iwtab(2)+jj)
#endif
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

       CASE default
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
#if LOWMEM
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
#else         
         IF (.NOT.tpreclist(ji)%calc) THEN
           xtab(1:extent) = TRANSFER(lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):),(/ 0.0_8 /))
         ELSE
           src=tpreclist(ji)%src(1)
           xtab(1:extent) = TRANSFER(lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):),(/ 0.0_8 /))
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             xtab(1:extent) = xtab(1:extent) + TRANSFER(lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):),(/ 0.0_8 /))
             jj=jj+1
           END DO
         END IF
#endif
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
#if LOWMEM
    DEALLOCATE(iwork)
#endif 
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
       icomlen = LEN(tpreclist(ivar)%comment)
       IF (icomlen > MAXLFICOMMENTLENGTH) THEN
         PRINT *,'ERROR: comment length is too big. Please increase MAXLFICOMMENTLENGTH'
         STOP
       END IF

       ! traitement Grille et Commentaire
       iwork(1) = tpreclist(ivar)%grid
       iwork(2) = icomlen
       DO jj=1,iwork(2)
          iwork(2+jj)=ICHAR(tpreclist(ivar)%comment(jj:jj))
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


       SELECT CASE(tpreclist(ivar)%TYPE)
       CASE(INT,BOOL)
          ALLOCATE( itab3d(idims(1),idims(2),idims(3)) )
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,itab3d)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'INT,BOOL --> ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = RESHAPE( itab3d , (/ idims(1)*idims(2)*idims(3) /) )

          DEALLOCATE(itab3d)

       CASE(FLOAT)
          ALLOCATE( xtab3d(idims(1),idims(2),idims(3)) )
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,xtab3d)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'FLOAT -->    ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = RESHAPE( TRANSFER(xtab3d,(/ 0_8 /),idlen) , (/ idims(1)*idims(2)*idims(3) /) )

          DEALLOCATE(xtab3d)

       CASE(TEXT)
          ALLOCATE(ytab(idlen))
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id_in,ytab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TEXT -->     ',tpreclist(ivar)%name,',len = ',idlen
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
    TYPE(filelist_struct),INTENT(OUT) :: infiles, outfiles
    CHARACTER(LEN=*), INTENT(IN)  :: hinfile
    CHARACTER(LEN=*), INTENT(IN)  :: houtfile
    INTEGER         , INTENT(OUT) :: nbvar_infile
    TYPE(option),DIMENSION(:),INTENT(IN) :: options
    INTEGER         , INTENT(IN)  :: runmode

    INTEGER                     :: extindex
    INTEGER(KIND=LFI_INT)       :: ilu,iresp,iverb,inap,inaf
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
       infiles%files(idx)%lun_id = 11
       infiles%files(idx)%format = LFI_FORMAT
       infiles%files(idx)%status = READING
       ilu = infiles%files(idx)%lun_id
       CALL LFIOUV(iresp,ilu,ltrue,hinfile,'OLD',lfalse&
            & ,lfalse,iverb,inap,inaf)
       infiles%files(idx)%opened  = .TRUE.

       nbvar_infile = inaf

       IF (options(OPTLIST)%set) THEN
          CALL LFILAF(iresp,ilu,lfalse)
          CALL LFIFER(iresp,ilu,'KEEP')
          return
       END IF

       IF (.NOT.options(OPTSPLIT)%set) THEN
         outfiles%nbfiles = outfiles%nbfiles + 1

         idx = outfiles%nbfiles
         outfiles%files(idx)%format = NETCDF_FORMAT
         outfiles%files(idx)%status = WRITING
         status = NF90_CREATE(TRIM(houtfile)//'.nc', IOR(NF90_CLOBBER,NF90_NETCDF4), outfiles%files(idx)%lun_id)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
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
       CALL LFIOUV(iresp,ilu,ltrue,TRIM(houtfile)//'.lfi','NEW' ,lfalse,lfalse,iverb,inap,inaf)
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
