MODULE mode_util
  USE MODD_IO_ll,  ONLY: TFILE_ELT
  USE MODD_NETCDF, ONLY: DIMCDF, IDCDF_KIND

  USE MODE_FIELD
  USE MODE_FIELDTYPE
  USE MODE_FMREAD
  USE MODE_FMWRIT

  USE mode_options

  USE NETCDF

  IMPLICIT NONE 

  INTEGER,PARAMETER :: MAXRAW=10
  INTEGER,PARAMETER :: MAXLEN=512
  INTEGER,PARAMETER :: MAXFILES=100
  INTEGER,PARAMETER :: MAXLFICOMMENTLENGTH=100

  INTEGER,PARAMETER :: FM_FIELD_SIZE = 32

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
     CHARACTER(LEN=FM_FIELD_SIZE)          :: name   ! nom du champ
     INTEGER                               :: id_in = -1, id_out = -1
     LOGICAL                               :: found  ! T if found in the input file
     LOGICAL                               :: calc   ! T if computed from other variables
     LOGICAL                               :: tbw    ! to be written or not
     LOGICAL                               :: tbr    ! to be read or not
     LOGICAL                               :: LSPLIT = .FALSE. ! TRUE if variable is split by vertical level
     INTEGER                               :: NSRC = 0 ! Number of variables used to compute the variable (needed only if calc=.true.)
     INTEGER,DIMENSION(MAXRAW)             :: src    ! List of variables used to compute the variable (needed only if calc=.true.)
     INTEGER                               :: tgt    ! Target: id of the variable that use it (calc variable)
     TYPE(TFIELDDATA)                      :: TFIELD ! Metadata about the field
     TYPE(DIMCDF),DIMENSION(:),ALLOCATABLE :: TDIMS  ! Dimensions of the field
  END TYPE workfield

  LOGICAL(KIND=LFI_INT), PARAMETER :: ltrue  = .TRUE.
  LOGICAL(KIND=LFI_INT), PARAMETER :: lfalse = .FALSE.

  CHARACTER(LEN=6) :: CPROGRAM_ORIG

  INTEGER, SAVE :: IDIMX = 0
  INTEGER, SAVE :: IDIMY = 0
  INTEGER, SAVE :: IDIMZ = 0

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

  SUBROUTINE parse_infiles(infiles, nbvar_infile, nbvar_tbr, nbvar_calc, nbvar_tbw, tpreclist, kbuflen, options)
    USE MODD_DIM_n,      ONLY: NIMAX_ll, NJMAX_ll, NKMAX
    USE MODD_PARAMETERS, ONLY: JPHEXT, JPVEXT

    TYPE(filelist_struct),      INTENT(IN) :: infiles
    INTEGER,                    INTENT(IN) :: nbvar_infile, nbvar_tbr, nbvar_calc, nbvar_tbw
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist
    INTEGER,                   INTENT(OUT) :: kbuflen
    TYPE(option),DIMENSION(:), INTENT(IN)  :: options

    INTEGER                                  :: ji,jj, kcdf_id, kcdf_id2, itype
    INTEGER                                  :: ndb, nde, ndey, idx, idx_var, maxvar
    INTEGER                                  :: idims, idimtmp, jdim, status, var_id
    LOGICAL                                  :: ladvan
    INTEGER                                  :: ich, leng
    INTEGER                                  :: comment_size, fsize, sizemax
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm, YDATENAME
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
    INTEGER                                  :: IID, IRESP
    INTEGER(KIND=LFI_INT)                    :: iresp2,ilu,ileng,ipos
    CHARACTER(LEN=FM_FIELD_SIZE)             :: var_calc
    CHARACTER(LEN=FM_FIELD_SIZE),dimension(MAXRAW) :: var_raw
    INTEGER, DIMENSION(10)                   :: idim_id
    INTEGER                                  :: IDXDATE, IDXTIME, IDX1
    LOGICAL                                  :: GISDATE, GISTIME
    LOGICAL                                  :: GOK

    IF (infiles%files(1)%format == LFI_FORMAT) THEN
      ilu = infiles%files(1)%lun_id
    ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
      kcdf_id = infiles%files(1)%lun_id
    END IF

    ! update IDIMX,IDIMY,IDIMZ
    IDIMX = NIMAX_ll+2*JPHEXT
    IDIMY = NJMAX_ll+2*JPHEXT
    IDIMZ = NKMAX   +2*JPVEXT

    PRINT *,'MESONH 3D, 2D articles DIMENSIONS used :'
    PRINT *,'DIMX =',IDIMX
    PRINT *,'DIMY =',IDIMY
    PRINT *,'DIMZ =',IDIMZ ! IDIMZ may be equal to 0 (PGD files)

    sizemax = 0

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
            tpreclist(idx_var)%NSRC = idx-1
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
            CALL LFINFO(iresp2,ilu,trim(yrecfm),ileng,ipos)
            IF (iresp2 == 0 .AND. ileng /= 0) tpreclist(ji)%found = .true.
            IF (iresp2==0 .AND. ileng == 0 .AND. ipos==0 .AND. infiles%TFILES(1)%TFILE%NSUBFILES_IOZ>0) THEN
              !Variable not found with no error (iresp2==0 .AND. ileng == 0 .AND. ipos==0)
              !If we are merging, maybe it is one of the split variable
              !In that case, the 1st part of the variable is in the 1st split file with a 0001 suffix
              CALL LFINFO(iresp2,infiles%TFILES(1)%TFILE%TFILES_IOZ(1)%TFILE%NLFIFLU,trim(yrecfm)//'0001',ileng,ipos)
              IF (iresp2 == 0 .AND. ileng /= 0) THEN
                tpreclist(ji)%found  = .true.
                tpreclist(ji)%LSPLIT = .true.
                IF (tpreclist(ji)%tgt > 0) THEN !If this variable is used for a calculated one
                  tpreclist(tpreclist(ji)%tgt)%LSPLIT = .true.
                END IF
              END IF
              ileng = ileng * IDIMZ !Real size is slightly overestimated due to comment size
            END IF

            leng = ileng
          ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
            status = NF90_INQ_VARID(kcdf_id,trim(yrecfm),tpreclist(ji)%id_in)
            IF (status /= NF90_NOERR .AND. infiles%TFILES(1)%TFILE%NSUBFILES_IOZ>0) THEN
              !Variable probably not found (other error possible...)
              !If we are merging, maybe it is one of the split variable
              !In that case, the 1st part of the variable is in the 1st split file with a 0001 suffix
              kcdf_id2 = infiles%TFILES(1)%TFILE%TFILES_IOZ(1)%TFILE%NNCID
              status = NF90_INQ_VARID(kcdf_id2,trim(yrecfm)//'0001',tpreclist(ji)%id_in)
              IF (status == NF90_NOERR) THEN
                tpreclist(ji)%LSPLIT = .true.
                IF (tpreclist(ji)%tgt > 0) THEN !If this variable is used for a calculated one
                  tpreclist(tpreclist(ji)%tgt)%LSPLIT = .true.
                END IF
              END IF
            ELSE
              kcdf_id2 = kcdf_id
            ENDIF
            !
            IF (status == NF90_NOERR) THEN
              tpreclist(ji)%found = .true.
              status = NF90_INQUIRE_VARIABLE(kcdf_id2,tpreclist(ji)%id_in,ndims = idims,dimids = idim_id)
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
                   status = NF90_INQUIRE_DIMENSION(kcdf_id2,idim_id(jdim),len = idimtmp)
                   IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
                   leng = leng*idimtmp
                END DO
                IF (tpreclist(ji)%LSPLIT) THEN
                  IF(idims/=2) CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','split variables can only be 3D')
                  !Split variables are Z-split
                  leng = leng * IDIMZ
                END IF
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

    ! Check if variable is in TFIELDLIST and populate corresponding metadata
    DO ji=1,maxvar
      IF (.NOT.tpreclist(ji)%found .OR. tpreclist(ji)%calc ) CYCLE
      !
      !Do not treat dimension variables (they are automatically added when creating netCDF file)
      IF (      tpreclist(ji)%name == 'ni'          &
           .OR. tpreclist(ji)%name == 'nj'          &
           .OR. tpreclist(ji)%name == 'ni_u'        &
           .OR. tpreclist(ji)%name == 'nj_u'        &
           .OR. tpreclist(ji)%name == 'ni_v'        &
           .OR. tpreclist(ji)%name == 'nj_v'        &
           .OR. tpreclist(ji)%name == 'latitude'    &
           .OR. tpreclist(ji)%name == 'longitude'   &
           .OR. tpreclist(ji)%name == 'latitude_u'  &
           .OR. tpreclist(ji)%name == 'longitude_u' &
           .OR. tpreclist(ji)%name == 'latitude_v'  &
           .OR. tpreclist(ji)%name == 'longitude_v' &
           .OR. tpreclist(ji)%name == 'latitude_f'  &
           .OR. tpreclist(ji)%name == 'longitude_f' &
           .OR. tpreclist(ji)%name == 'level'       &
           .OR. tpreclist(ji)%name == 'level_w'     ) THEN
        tpreclist(ji)%tbw   = .FALSE.
        tpreclist(ji)%tbr   = .FALSE.
        tpreclist(ji)%found = .FALSE.
      ELSE
        CALL FIND_FIELD_ID_FROM_MNHNAME(tpreclist(ji)%name,IID,IRESP)
        IF (IRESP==0) THEN
          tpreclist(ji)%TFIELD = TFIELDLIST(IID)
          ALLOCATE(tpreclist(ji)%TDIMS(tpreclist(ji)%TFIELD%NDIMS))
        ELSE !Field not found in list
          CALL PRINT_MSG(NVERB_WARNING,'IO','parse_infiles','variable '//TRIM(tpreclist(ji)%name)//' is not known => ignored')
          tpreclist(ji)%tbw   = .FALSE.
          tpreclist(ji)%tbr   = .FALSE.
          tpreclist(ji)%found = .FALSE.
        END IF
      END IF
    END DO

    IF (nbvar_calc>0) THEN
    !Calculated variables
    !Done after previous loop to reuse metadata from component variables
    !Derive metadata from its components
    !If same value for all components => take it
    !If not => nothing or default value
    !Check sizes: must be the same for all
    DO ji=1,maxvar
      IF (.NOT.tpreclist(ji)%calc ) CYCLE
      !
      tpreclist(ji)%TFIELD%CMNHNAME  = tpreclist(ji)%name
      tpreclist(ji)%TFIELD%CSTDNAME  = ''
      tpreclist(ji)%TFIELD%CLONGNAME = tpreclist(ji)%name
      !
      GOK = .TRUE.
      DO jj=1,tpreclist(ji)%NSRC
        idx_var = tpreclist(ji)%src(jj)
        IF(.NOT.tpreclist(idx_var)%found) THEN
          CALL PRINT_MSG(NVERB_WARNING,'IO','parse_infiles','some components for calculated variable ' &
                         //TRIM(tpreclist(ji)%name)//' are not known => ignored')
          tpreclist(ji)%tbw   = .FALSE.
          tpreclist(ji)%tbr   = .FALSE.
          tpreclist(ji)%found = .FALSE.
          GOK = .FALSE.
          EXIT
        END IF
      END DO
      !
      IF (GOK) THEN
        idx_var = tpreclist(ji)%src(1)
        tpreclist(ji)%TFIELD%CUNITS   = tpreclist(idx_var)%TFIELD%CUNITS
        tpreclist(ji)%TFIELD%CDIR     = tpreclist(idx_var)%TFIELD%CDIR
        tpreclist(ji)%TFIELD%CLBTYPE  = tpreclist(idx_var)%TFIELD%CLBTYPE
        tpreclist(ji)%TFIELD%CCOMMENT = TRIM(tpreclist(ji)%name)//'='//TRIM(tpreclist(idx_var)%name)
        IF (tpreclist(ji)%NSRC>1) tpreclist(ji)%TFIELD%CCOMMENT = TRIM(tpreclist(ji)%TFIELD%CCOMMENT)//'+'
        tpreclist(ji)%TFIELD%NGRID    = tpreclist(idx_var)%TFIELD%NGRID
        tpreclist(ji)%TFIELD%NTYPE    = tpreclist(idx_var)%TFIELD%NTYPE
        tpreclist(ji)%TFIELD%NDIMS    = tpreclist(idx_var)%TFIELD%NDIMS
#if 0
!PW: TODO?
        tpreclist(ji)%TFIELD%NFILLVALUE
        tpreclist(ji)%TFIELD%XFILLVALUE
        tpreclist(ji)%TFIELD%NVALIDMIN
        tpreclist(ji)%TFIELD%NVALIDMAX
        tpreclist(ji)%TFIELD%XVALIDMIN
        tpreclist(ji)%TFIELD%XVALIDMAX
#endif
        DO jj=2,tpreclist(ji)%NSRC
          idx_var = tpreclist(ji)%src(jj)
          !
          IF (tpreclist(ji)%TFIELD%CUNITS /= tpreclist(idx_var)%TFIELD%CUNITS) THEN
            CALL PRINT_MSG(NVERB_WARNING,'IO','parse_infiles','CUNITS is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name)//' => CUNITS not set')
            tpreclist(ji)%TFIELD%CUNITS = ''
          END IF
          !
          IF (tpreclist(ji)%TFIELD%CDIR /= tpreclist(idx_var)%TFIELD%CDIR) THEN
            CALL PRINT_MSG(NVERB_ERROR,'IO','parse_infiles','CDIR is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name)//' => CDIR=--')
            tpreclist(ji)%TFIELD%CDIR = '--'
          END IF
          !
          IF (tpreclist(ji)%TFIELD%CLBTYPE /= tpreclist(idx_var)%TFIELD%CLBTYPE) THEN
            CALL PRINT_MSG(NVERB_ERROR,'IO','parse_infiles','CLBTYPE is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name)//' => CLBTYPE=NONE')
            tpreclist(ji)%TFIELD%CLBTYPE = 'NONE'
          END IF
          !
          tpreclist(ji)%TFIELD%CCOMMENT = TRIM(tpreclist(ji)%TFIELD%CCOMMENT)//TRIM(tpreclist(idx_var)%name)
          IF (jj<tpreclist(ji)%NSRC) tpreclist(ji)%TFIELD%CCOMMENT = TRIM(tpreclist(ji)%TFIELD%CCOMMENT)//'+'
          !
          IF (tpreclist(ji)%TFIELD%NGRID /= tpreclist(idx_var)%TFIELD%NGRID) THEN
            CALL PRINT_MSG(NVERB_WARNING,'IO','parse_infiles','NGRID is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name)//' => NGRID=1')
            tpreclist(ji)%TFIELD%NGRID = 1
          END IF
          !
          IF (tpreclist(ji)%TFIELD%NTYPE /= tpreclist(idx_var)%TFIELD%NTYPE) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','NTYPE is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name))
            tpreclist(ji)%TFIELD%NTYPE = TYPEUNDEF
          END IF
          !
          IF (tpreclist(ji)%TFIELD%NDIMS /= tpreclist(idx_var)%TFIELD%NDIMS) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','parse_infiles','NDIMS is not uniform between components of calculated variable '&
                           //TRIM(tpreclist(ji)%name))
          END IF
        END DO
        !
        ALLOCATE(tpreclist(ji)%TDIMS(tpreclist(ji)%TFIELD%NDIMS))
        !
      END IF
    END DO !ji=1,maxvar
    END IF !nbvar_calc>0

    kbuflen = sizemax

    WRITE(*,'("Taille maximale du buffer :",f10.3," Mio")') sizemax*8./1048576.

  END SUBROUTINE parse_infiles
  
  SUBROUTINE HANDLE_ERR(status,line)
    INTEGER :: status,line

    IF (status /= NF90_NOERR) THEN
       PRINT *, 'line ',line,': ',NF90_STRERROR(status)
           STOP
    END IF
  END SUBROUTINE HANDLE_ERR

  SUBROUTINE def_ncdf(outfiles,tpreclist,nbvar,options)
    USE MODE_NETCDF, ONLY: IO_WRITE_HEADER_NC4

    TYPE(filelist_struct),       INTENT(IN) :: outfiles
    TYPE(workfield),DIMENSION(:),INTENT(INOUT) :: tpreclist
    INTEGER,                     INTENT(IN) :: nbvar
    TYPE(option),DIMENSION(:),   INTENT(IN) :: options

    INTEGER :: compress_level, status
    INTEGER :: idx, ji, nbfiles
    INTEGER :: kcdf_id
    INTEGER :: IID, IRESP
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
      CALL IO_WRITE_HEADER_NC4(outfiles%TFILES(ji)%TFILE)
      !
!       status = NF90_PUT_ATT(kcdf_id,NF90_GLOBAL,'Title',VERSION_ID)
!       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    END DO
    
  END SUBROUTINE def_ncdf

  SUBROUTINE fill_ncdf(infiles,outfiles,tpreclist,knaf,kbuflen,options)
    USE MODD_TYPE_DATE

    USE MODE_NETCDF, ONLY: IO_GUESS_DIMIDS_NC4

    TYPE(filelist_struct),        INTENT(IN)    :: infiles, outfiles
    TYPE(workfield), DIMENSION(:),INTENT(INOUT) :: tpreclist
    INTEGER,                      INTENT(IN)    :: knaf
    INTEGER,                      INTENT(IN)    :: kbuflen
    TYPE(option),DIMENSION(:),    INTENT(IN)    :: options

    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
    INTEGER                                  :: idx, ji,jj
    INTEGER                                  :: kcdf_id
!     INTEGER                                  :: status
    INTEGER                                  :: extent, ndims
    INTEGER                                  :: ich
    INTEGER                                  :: IID, IRESP2
    INTEGER                                  :: src
    INTEGER                                  :: level
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
!     INTEGER,DIMENSION(3)                     :: idims, start
    INTEGER,DIMENSION(3)                     :: start
    INTEGER,DIMENSION(:),ALLOCATABLE         :: itab
    LOGICAL,DIMENSION(:),ALLOCATABLE         :: gtab
    REAL,DIMENSION(:),ALLOCATABLE    :: xtab
!     CHARACTER, DIMENSION(:), ALLOCATABLE     :: ytab
    CHARACTER(LEN=:), ALLOCATABLE               :: ytab
    REAL, DIMENSION(:,:), ALLOCATABLE :: xtab2d
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: xtab3d, xtab3d2
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: xtab4d
    INTEGER,      DIMENSION(:,:,:), ALLOCATABLE :: itab3d, itab3d2
    TYPE(DATE_TIME)                          :: TZDATE

INTEGER(KIND=IDCDF_KIND) :: STATUS
INTEGER(KIND=IDCDF_KIND) :: INCID
INTEGER(KIND=IDCDF_KIND) :: IVARID
INTEGER(KIND=IDCDF_KIND) :: IDIMS   ! number of dimensions
INTEGER(KIND=IDCDF_KIND),DIMENSION(NF90_MAX_VAR_DIMS) :: IVDIMS
INTEGER(KIND=IDCDF_KIND),DIMENSION(NF90_MAX_VAR_DIMS) :: IDIMLEN
    !
    IF (infiles%files(1)%format == LFI_FORMAT) ilu = infiles%files(1)%lun_id
    !
    ALLOCATE(iwork(kbuflen))
    ALLOCATE(itab(kbuflen))
    ALLOCATE(gtab(kbuflen))
    ALLOCATE(xtab(kbuflen))

    idx = 1
    DO ji=1,knaf
       IF (.NOT.tpreclist(ji)%tbw) CYCLE

       kcdf_id = outfiles%files(idx)%lun_id

       ndims = tpreclist(ji)%TFIELD%NDIMS

       SELECT CASE(tpreclist(ji)%TFIELD%NTYPE)
       CASE (TYPEINT)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name),iwork,ileng)
           extent = ileng - 2 - iwork(2) !iwork(2) = comment length
           ! Determine TDIMS
           CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,extent,tpreclist(ji)%TDIMS,IRESP2)
           IF (IRESP2/=0) THEN
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CYCLE
           END IF
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
           extent = ileng - 2 - iwork(2) !iwork(2) = comment length
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
           ! Determine TDIMS
           CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(src)%TFIELD,extent,tpreclist(src)%TDIMS,IRESP2)
           IF (IRESP2/=0) THEN
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(src)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CYCLE
           ELSE
             tpreclist(ji)%TDIMS = tpreclist(src)%TDIMS
           END IF
           jj = 2
           DO jj=2,tpreclist(ji)%NSRC
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
!PW: TODO: check same dimensions
             itab(1:extent) = itab(1:extent) + iwork(3+iwork(2):3+iwork(2)+extent-1)
           END DO
         ENDIF

!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,itab(1))
         CASE (1)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,itab(1:extent))
         CASE (2)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,reshape(itab,tpreclist(ji)%TDIMS(1:2)%LEN))
!            status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,reshape(itab,(/ptdimx%len,ptdimy%len/)), &
!                                  start = (/1,1,level/) )
         CASE (3)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,reshape(itab,tpreclist(ji)%TDIMS(1:3)%LEN))
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
INCID = infiles%TFILES(1)%TFILE%NNCID
STATUS = NF90_INQ_VARID(INCID,tpreclist(ji)%TFIELD%CMNHNAME,IVARID)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
STATUS = NF90_INQUIRE_VARIABLE(INCID, IVARID, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
if (ndims/=idims) then
print *,'aieeeeeee'
stop
end if
DO JJ=1,IDIMS
  STATUS = NF90_INQUIRE_DIMENSION(infiles%TFILES(1)%TFILE%NNCID, IVDIMS(JJ), LEN=IDIMLEN(JJ))
  IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
END DO
         SELECT CASE(ndims)
         CASE (0)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,itab(1))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,itab(1))
         CASE (1)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,itab(1:IDIMLEN(1)))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,itab(1:IDIMLEN(1)))
         CASE (2)
print *,'PW:TODO'
         CASE (3)
print *,'PW:TODO'
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT

#if 0
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
#endif
        END IF

         
       CASE (TYPELOG)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name),iwork,ileng)
           extent = ileng - 2 - iwork(2) !iwork(2) = comment length
           ! Determine TDIMS
           CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,extent,tpreclist(ji)%TDIMS,IRESP2)
           IF (IRESP2/=0) THEN
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CYCLE
           END IF
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
         ELSE
           src=tpreclist(ji)%src(1)
           CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name),ileng,ipos)
           CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
           extent = ileng - 2 - iwork(2) !iwork(2) = comment length
           ! Determine TDIMS
           CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(src)%TFIELD,extent,tpreclist(src)%TDIMS,IRESP2)
           IF (IRESP2/=0) THEN
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(src)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                            ' => ignored')
             CYCLE
           ELSE
             tpreclist(ji)%TDIMS = tpreclist(src)%TDIMS
           END IF
           itab(1:extent) = iwork(3+iwork(2):3+iwork(2)+extent-1)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
!PW: TODO: check same dimensions
             itab(1:extent) = itab(1:extent) + iwork(3+iwork(2):3+iwork(2)+extent-1)
             jj=jj+1
           END DO
         ENDIF

         DO JJ=1,EXTENT
           IF (ITAB(JJ)==0) THEN
             GTAB(JJ) = .FALSE.
           ELSE
             GTAB(JJ) = .TRUE.
           END IF
         END DO

         SELECT CASE(ndims)
         CASE (0)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,gtab(1))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id_out,itab(1:extent),count=(/extent/))
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
INCID = infiles%TFILES(1)%TFILE%NNCID
STATUS = NF90_INQ_VARID(INCID,tpreclist(ji)%TFIELD%CMNHNAME,IVARID)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
STATUS = NF90_INQUIRE_VARIABLE(INCID, IVARID, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
if (ndims/=idims) then
print *,'aieeeeeee'
stop
end if
DO JJ=1,IDIMS
  STATUS = NF90_INQUIRE_DIMENSION(infiles%TFILES(1)%TFILE%NNCID, IVDIMS(JJ), LEN=IDIMLEN(JJ))
  IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
END DO
         SELECT CASE(ndims)
         CASE (0)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,gtab(1))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,gtab(1))
         CASE (1)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,gtab(1:IDIMLEN(1)))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,gtab(1:IDIMLEN(1)))
         CASE (2)
print *,'PW:TODO'
         CASE (3)
print *,'PW:TODO'
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT
#if 0
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
#endif
        END IF


       CASE (TYPEREAL)
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         IF (.NOT.tpreclist(ji)%calc) THEN
           IF (.NOT.tpreclist(ji)%LSPLIT) THEN
             CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name),ileng,ipos)
             CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name),iwork,ileng)
             extent = ileng - 2 - iwork(2) !iwork(2) = comment length
             ! Determine TDIMS
             CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,extent,tpreclist(ji)%TDIMS,IRESP2)
             IF (IRESP2/=0) THEN
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CYCLE
             END IF
             xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
           ELSE
             !We assume that split variables are always of size(IDIMX,IDMIY,IDIMZ)
             ALLOCATE(xtab3d(IDIMX,IDIMY,IDIMZ))
             CALL IO_READ_FIELD(infiles%tfiles(1)%TFILE,tpreclist(ji)%TFIELD,XTAB3D)
             extent = IDIMX*IDIMY*IDIMZ
             ! Determine TDIMS
             CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,extent,tpreclist(ji)%TDIMS,IRESP2)
             IF (IRESP2/=0) THEN
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CYCLE
             END IF
             xtab(1:extent) = RESHAPE( xtab3d, (/extent/) )
             DEALLOCATE(xtab3d)
           END IF
         ELSE
           src=tpreclist(ji)%src(1)
           IF (.NOT.tpreclist(ji)%LSPLIT) THEN
             CALL LFINFO(iresp,ilu,trim(tpreclist(src)%name),ileng,ipos)
             CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
             extent = ileng - 2 - iwork(2) !iwork(2) = comment length
             ! Determine TDIMS
             CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(src)%TFIELD,extent,tpreclist(src)%TDIMS,IRESP2)
             IF (IRESP2/=0) THEN
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(src)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CYCLE
             ELSE
               tpreclist(ji)%TDIMS = tpreclist(src)%TDIMS
             END IF
             xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
             jj = 2
             DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
               src=tpreclist(ji)%src(jj)
               CALL LFILEC(iresp,ilu,trim(tpreclist(src)%name),iwork,ileng)
!PW: TODO: check same dimensions
               xtab(1:extent) = xtab(1:extent) + TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
               jj=jj+1
             END DO
           ELSE !Split variable
             !We assume that split variables are always of size(IDIMX,IDMIY,IDIMZ)
             ALLOCATE(xtab3d(IDIMX,IDIMY,IDIMZ))
             ALLOCATE(xtab3d2(IDIMX,IDIMY,IDIMZ))
             CALL IO_READ_FIELD(infiles%tfiles(1)%TFILE,tpreclist(tpreclist(ji)%src(1))%TFIELD,XTAB3D)
             extent = IDIMX*IDIMY*IDIMZ
             ! Determine TDIMS
             CALL IO_GUESS_DIMIDS_NC4(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,extent,tpreclist(src)%TDIMS,IRESP2)
             IF (IRESP2/=0) THEN
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(src)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CALL PRINT_MSG(NVERB_WARNING,'IO','fill_ncdf','can not guess dimensions for '//tpreclist(ji)%TFIELD%CMNHNAME// &
                              ' => ignored')
               CYCLE
             ELSE
               tpreclist(ji)%TDIMS = tpreclist(src)%TDIMS
             END IF
             jj = 2
             DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
               CALL IO_READ_FIELD(infiles%tfiles(1)%TFILE,tpreclist(tpreclist(ji)%src(jj))%TFIELD,XTAB3D2)
               XTAB3D(:,:,:) = XTAB3D(:,:,:) + XTAB3D2(:,:,:)
               jj=jj+1
             END DO
             xtab(1:extent) = RESHAPE( xtab3d, (/extent/) )
             DEALLOCATE(xtab3d,xtab3d2)
           END IF
         ENDIF
!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,xtab(1))
         CASE (1)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,xtab(1:extent))
         CASE (2)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,reshape(xtab,tpreclist(ji)%TDIMS(1:2)%LEN))
         CASE (3)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,reshape(xtab,tpreclist(ji)%TDIMS(1:3)%LEN))
         CASE (4)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,reshape(xtab,tpreclist(ji)%TDIMS(1:4)%LEN))
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT

        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
IF (.NOT.tpreclist(ji)%LSPLIT) THEN
  INCID = infiles%TFILES(1)%TFILE%NNCID
  STATUS = NF90_INQ_VARID(INCID,tpreclist(ji)%TFIELD%CMNHNAME,IVARID)
  IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
  STATUS = NF90_INQUIRE_VARIABLE(INCID, IVARID, NDIMS=IDIMS, DIMIDS=IVDIMS)
  IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
  if (ndims/=idims) then
  print *,'aieeeeeee'
  stop
  end if
  DO JJ=1,IDIMS
    STATUS = NF90_INQUIRE_DIMENSION(infiles%TFILES(1)%TFILE%NNCID, IVDIMS(JJ), LEN=IDIMLEN(JJ))
    IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
  END DO
ELSE
  !Split variables are always 3D variables
  IDIMLEN(1) = IDIMX
  IDIMLEN(2) = IDIMY
  IDIMLEN(3) = IDIMZ
END IF
         SELECT CASE(ndims)
         CASE (0)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,xtab(1))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,xtab(1))
         CASE (1)
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,xtab(1:IDIMLEN(1)))
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,xtab(1:IDIMLEN(1)))
         CASE (2)
           ALLOCATE(XTAB2D(IDIMLEN(1),IDIMLEN(2)))
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,XTAB2D)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,XTAB2D)
           DEALLOCATE(XTAB2D)
         CASE (3)
           ALLOCATE(XTAB3D(IDIMLEN(1),IDIMLEN(2),IDIMLEN(3)))
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,XTAB3D)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,XTAB3D)
           DEALLOCATE(XTAB3D)
         CASE (4)
           ALLOCATE(XTAB4D(IDIMLEN(1),IDIMLEN(2),IDIMLEN(3),IDIMLEN(4)))
           CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,XTAB4D)
           CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,XTAB4D)
           DEALLOCATE(XTAB4D)
         CASE DEFAULT
           print *,'Error: arrays with ',ndims,' dimensions are not supported'
         END SELECT

#if 0
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
#endif
        END IF

       CASE (TYPECHAR)
         IF (ndims/=0) CALL PRINT_MSG(NVERB_FATAL,'IO','fill_ncdf','only ndims=0 is supported for TYPECHAR')
        IF (infiles%files(1)%format == LFI_FORMAT) THEN
         CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name),ileng,ipos)
         CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name),iwork,ileng)
         extent = ileng - 2 - iwork(2) !iwork(2) = comment length
!          ALLOCATE(ytab(extent))
         allocate(character(len=extent)::ytab)
         DO jj=1,extent
           ich = iwork(2+iwork(2)+jj)
!            ytab(jj) = CHAR(ich)
           ytab(jj:jj) = CHAR(ich)
         END DO
         CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,trim(ytab))
         DEALLOCATE(ytab)
        ELSE IF (infiles%files(1)%format == NETCDF_FORMAT) THEN
INCID = infiles%TFILES(1)%TFILE%NNCID
STATUS = NF90_INQ_VARID(INCID,tpreclist(ji)%TFIELD%CMNHNAME,IVARID)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
STATUS = NF90_INQUIRE_VARIABLE(INCID, IVARID, NDIMS=IDIMS, DIMIDS=IVDIMS)
IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
! if (ndims/=idims) then
if (idims/=1) then
print *,'aieeeeeee'
stop
end if
DO JJ=1,IDIMS
  STATUS = NF90_INQUIRE_DIMENSION(infiles%TFILES(1)%TFILE%NNCID, IVDIMS(JJ), LEN=IDIMLEN(JJ))
  IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
END DO
         allocate(character(len=IDIMLEN(1))::ytab)
         CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD,ytab)
         CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,ytab)
         DEALLOCATE(ytab)
        END IF

       CASE (TYPEDATE)
         IF (ndims/=0) CALL PRINT_MSG(NVERB_FATAL,'IO','fill_ncdf','only ndims=0 is supported for TYPEDATE')
!PW: TODO: tpreclist(ji)%TFIELD%CMNHNAME => tpreclist(ji)%TFIELD
         CALL IO_READ_FIELD (infiles%tfiles(1)%TFILE,   tpreclist(ji)%TFIELD%CMNHNAME,TZDATE)
         CALL IO_WRITE_FIELD(outfiles%tfiles(idx)%TFILE,tpreclist(ji)%TFIELD,TZDATE)

       CASE default
         CALL PRINT_MSG(NVERB_FATAL,'IO','fill_ncdf','invalid datatype')

       END SELECT

       if (options(OPTSPLIT)%set) idx = idx + 1
    END DO
    DEALLOCATE(itab,gtab,xtab)
    DEALLOCATE(iwork)
  END SUBROUTINE fill_ncdf

  SUBROUTINE build_lfi(infiles,outfiles,tpreclist,knaf,kbuflen)
    TYPE(filelist_struct),         INTENT(IN) :: infiles, outfiles
    TYPE(workfield), DIMENSION(:), INTENT(IN) :: tpreclist
    INTEGER,                       INTENT(IN) :: knaf
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
    DO ivar=1,knaf
       IF (.NOT.tpreclist(ivar)%tbw) CYCLE

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


stop


       
       ! Attention restoration des '%' dans le nom des champs LFI
       yrecfm = str_replace(tpreclist(ivar)%name,'__','%')
       ! et des '.'
       yrecfm = str_replace(yrecfm,'--','.')
       iartlen8 = iartlen
       CALL LFIECR(iresp,ilu,yrecfm,iwork,iartlen8)

    END DO
    DEALLOCATE(iwork)

  END SUBROUTINE build_lfi

  SUBROUTINE OPEN_FILES(infiles,outfiles,hinfile,houtfile,nbvar_infile,options,runmode)
    USE MODD_CONF,          ONLY: LCARTESIAN
    USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
    USE MODD_DIM_n,         ONLY: NIMAX_ll, NJMAX_ll, NKMAX
    USE MODD_GRID,          ONLY: XBETA, XRPK, XLAT0, XLON0, XLATORI, XLONORI
    USE MODD_GRID_n,        ONLY: LSLEVE, XXHAT, XYHAT, XZHAT
    USE MODD_IO_ll,         ONLY: LIOCDF4
    USE MODD_PARAMETERS,    ONLY: JPHEXT
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT_ll=>JPHEXT, JPVEXT_ll=>JPVEXT

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
    !
    ! Infiles
    !
    IF (runmode == MODECDF2CDF .OR. runmode == MODECDF2LFI) THEN
       !
       ! NetCDF
       !
       infiles%nbfiles = infiles%nbfiles + 1
       idx = infiles%nbfiles
       CALL IO_FILE_ADD2LIST(INFILES%TFILES(idx)%TFILE,HINFILE,'UNKNOWN','READ',HFORMAT='NETCDF4')
       CALL IO_FILE_OPEN_ll(INFILES%TFILES(idx)%TFILE)
       infiles%files(idx)%lun_id = INFILES%TFILES(idx)%TFILE%NNCID
       infiles%files(idx)%format = NETCDF_FORMAT
       infiles%files(idx)%status = READING
       infiles%files(idx)%opened  = .TRUE.

       status = NF90_INQUIRE(infiles%files(idx)%lun_id, nvariables = nbvar_infile)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
   ELSE
       !
       ! LFI
       !
       infiles%nbfiles = infiles%nbfiles + 1
       idx = infiles%nbfiles
       CALL IO_FILE_ADD2LIST(INFILES%TFILES(idx)%TFILE,HINFILE,'UNKNOWN','READ', &
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
   END IF
   !
   !Read problem dimensions and some grid variables (needed by IO_FILE_OPEN_ll to create netCDF files but also to determine IDIMX/Y/Z)
   CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'JPHEXT',JPHEXT)
   JPHEXT_ll = JPHEXT
   !CALL IO_READ_FIELD(INFILES%TFILES(idx)%TFILE,'JPVEXT',JPVEXT,IRESP)
   !IF(IRESP/=0) JPVEXT=1
   JPVEXT_ll = JPVEXT
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
   !
   ! Outfiles
   !
   IF (runmode == MODELFI2CDF .OR. runmode == MODECDF2CDF) THEN
       !
       ! NetCDF
       !
       IF (.NOT.options(OPTSPLIT)%set) THEN
         outfiles%nbfiles = outfiles%nbfiles + 1

         idx = outfiles%nbfiles
         CALL IO_FILE_ADD2LIST(OUTFILES%TFILES(idx)%TFILE,HOUTFILE,'UNKNOWN','WRITE', &
                               HFORMAT='NETCDF4',OOLD=.TRUE.)
         CALL IO_FILE_OPEN_ll(OUTFILES%TFILES(idx)%TFILE,HPROGRAM_ORIG=CPROGRAM_ORIG)
         outfiles%files(idx)%lun_id = OUTFILES%TFILES(idx)%TFILE%NNCID
         outfiles%files(idx)%format = NETCDF_FORMAT
         outfiles%files(idx)%status = WRITING
         outfiles%files(idx)%opened  = .TRUE.

         IF (options(OPTCOMPRESS)%set) THEN
           outfiles%tfiles(idx)%tfile%LNCCOMPRESS       = .TRUE.
           outfiles%tfiles(idx)%tfile%NNCCOMPRESS_LEVEL = options(OPTCOMPRESS)%ivalue
         END IF

         IF (options(OPTREDUCE)%set) THEN
           outfiles%tfiles(idx)%tfile%LNCREDUCE_FLOAT_PRECISION = .TRUE.
         END IF

         status = NF90_SET_FILL(outfiles%files(idx)%lun_id,NF90_NOFILL,omode)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       END IF ! .NOT.osplit
    ELSE
       !
       ! LFI
       !
       outfiles%nbfiles = outfiles%nbfiles + 1
       idx = outfiles%nbfiles
       CALL IO_FILE_ADD2LIST(OUTFILES%TFILES(idx)%TFILE,houtfile,'UNKNOWN','WRITE', &
                             HFORMAT='LFI',KLFIVERB=0,OOLD=.TRUE.)
       LIOCDF4 = .FALSE. !Necessary to open correctly the LFI file
       CALL IO_FILE_OPEN_ll(OUTFILES%TFILES(idx)%TFILE)
       LIOCDF4 = .TRUE.
       outfiles%files(idx)%lun_id = OUTFILES%TFILES(idx)%TFILE%NLFIFLU
       outfiles%files(idx)%format = LFI_FORMAT
       outfiles%files(idx)%status = WRITING
       infiles%files(idx)%opened  = .TRUE.
   END IF

   PRINT *,'--> Fichier converti : ', TRIM(houtfile)

  END SUBROUTINE OPEN_FILES

  SUBROUTINE OPEN_SPLIT_NCFILES_OUT(outfiles,houtfile,nbvar,tpreclist,options)
    USE MODE_FM,               ONLY: IO_FILE_OPEN_ll
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST
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

      filename = trim(houtfile)//'.'//trim(tpreclist(ji)%name)
      CALL IO_FILE_ADD2LIST(OUTFILES%TFILES(idx)%TFILE,filename,'UNKNOWN','WRITE', &
                            HFORMAT='NETCDF4')
      CALL IO_FILE_OPEN_ll(OUTFILES%TFILES(idx)%TFILE,HPROGRAM_ORIG=CPROGRAM_ORIG)
      outfiles%files(idx)%lun_id = OUTFILES%TFILES(idx)%TFILE%NNCID
      outfiles%files(idx)%format = NETCDF_FORMAT
      outfiles%files(idx)%status = WRITING
      outfiles%files(idx)%opened  = .TRUE.

      IF (options(OPTCOMPRESS)%set) THEN
        outfiles%tfiles(idx)%tfile%LNCCOMPRESS       = .TRUE.
        outfiles%tfiles(idx)%tfile%NNCCOMPRESS_LEVEL = options(OPTCOMPRESS)%ivalue
      END IF

      IF (options(OPTREDUCE)%set) THEN
        outfiles%tfiles(idx)%tfile%LNCREDUCE_FLOAT_PRECISION = .TRUE.
      END IF

      status = NF90_SET_FILL(outfiles%files(idx)%lun_id,NF90_NOFILL,omode)
      IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

      idx = idx + 1
    END DO

  END SUBROUTINE OPEN_SPLIT_NCFILES_OUT
  
  SUBROUTINE CLOSE_FILES(filelist)
    USE MODE_FM,    ONLY: IO_FILE_CLOSE_ll

    TYPE(filelist_struct),INTENT(INOUT) :: filelist
    
    INTEGER(KIND=LFI_INT) :: ilu,iresp
    INTEGER               :: ji,status

    DO ji=1,filelist%nbfiles
      IF ( .NOT.filelist%files(ji)%opened ) CYCLE

      CALL IO_FILE_CLOSE_ll(filelist%TFILES(ji)%TFILE)
      filelist%files(ji)%opened=.false.
    END DO

  END SUBROUTINE CLOSE_FILES

END MODULE mode_util
