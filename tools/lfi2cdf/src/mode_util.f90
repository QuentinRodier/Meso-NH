MODULE mode_util
  USE MODE_FIELDTYPE
  USE mode_dimlist
  USE MODD_PARAM
  USE netcdf

  IMPLICIT NONE 

  INTEGER,PARAMETER :: MAXRAW=10

  TYPE workfield
     CHARACTER(LEN=FM_FIELD_SIZE)            :: name   ! nom du champ
     INTEGER                                 :: TYPE   ! type (entier ou reel)    
     CHARACTER(LEN=:), POINTER               :: comment
     TYPE(dimCDF),                   POINTER :: dim
     INTEGER                                 :: id
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

  SUBROUTINE parse_lfi(klu, hvarlist, nbvar_lfi, nbvar_tbr, nbvar_calc, nbvar_tbw, tpreclist, kbuflen, current_level)
    INTEGER, INTENT(IN)                    :: klu
    INTEGER, INTENT(IN)                    :: nbvar_lfi, nbvar_tbr, nbvar_calc, nbvar_tbw
    CHARACTER(LEN=*), intent(IN)           :: hvarlist
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist    
    INTEGER, INTENT(OUT)                   :: kbuflen
    INTEGER, INTENT(IN), OPTIONAL          :: current_level

    INTEGER                                  :: ji,jj
    INTEGER                                  :: ndb, nde, ndey, idx, idx_var, maxvar
    LOGICAL                                  :: ladvan
    INTEGER                                  :: ich
    INTEGER                                  :: fsize,sizemax
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm
    CHARACTER(LEN=4)                         :: suffix
#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
    !JUAN CYCCL3
    INTEGER                        :: JPHEXT

    ilu = klu

    CALL FMREADLFIN1(klu,'JPHEXT',JPHEXT,iresp)
    IF (iresp /= 0) JPHEXT=1
    ! First check if IMAX,JMAX,KMAX exist in LFI file
    ! to handle 3D, 2D variables -> update IDIMX,IDIMY,IDIMZ
    CALL FMREADLFIN1(klu,'IMAX',IDIMX,iresp)
    IF (iresp == 0) IDIMX = IDIMX+2*JPHEXT  ! IMAX + 2*JPHEXT
    !
    CALL FMREADLFIN1(klu,'JMAX',IDIMY,iresp)
    IF (iresp == 0) IDIMY = IDIMY+2*JPHEXT  ! JMAX + 2*JPHEXT
    !
    CALL FMREADLFIN1(ilu,'KMAX',IDIMZ,iresp)
    IF (iresp == 0) IDIMZ = IDIMZ+2  ! KMAX + 2*JPVEXT
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

    IF (present(current_level)) THEN
      write(suffix,'(I4.4)') current_level
    ElSE
      suffix=''
    END IF

    ! Phase 1 : build articles list to convert.
    !
    !    Pour l'instant tous les articles du fichier LFI sont
    !    convertis. On peut modifier cette phase pour prendre en
    !    compte un sous-ensemble d'article (liste definie par
    !    l'utilisateur par exemple)  
    !
    IF (LEN_TRIM(hvarlist) > 0) THEN
#ifndef LOWMEM
      IF(.NOT.ALLOCATED(lfiart)) ALLOCATE(lfiart(nbvar_tbr+nbvar_calc))
#endif
      ALLOCATE(tpreclist(nbvar_tbr+nbvar_calc))
      DO ji=1,nbvar_tbr+nbvar_calc
        tpreclist(ji)%found  = .FALSE.
        tpreclist(ji)%calc   = .FALSE. !By default variables are not computed from others
        tpreclist(ji)%tbw    = .TRUE.  !By default variables are written
        tpreclist(ji)%tbr    = .TRUE.  !By default variables are written
        tpreclist(ji)%src(:) = -1
        tpreclist(ji)%tgt    = -1
      END DO

       ! A variable list is provided with -v var1,...
       ndb  = 1
       idx_var = 1
       DO ji=1,nbvar_tbw
          nde = INDEX(TRIM(hvarlist(ndb:)),',')
          yrecfm = hvarlist(ndb:ndb+nde-2)

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

!TODO: merge loop?
       DO ji=1,nbvar_tbr+nbvar_calc
          IF (tpreclist(ji)%calc) CYCLE
          yrecfm = TRIM(tpreclist(ji)%name)
          CALL LFINFO(iresp,ilu,trim(yrecfm)//trim(suffix),ileng,ipos)
          
          IF (iresp /= 0 .OR. ileng == 0) THEN
             PRINT *,'Article ',TRIM(yrecfm), ' not found!'
             tpreclist(ji)%found = .FAlSE.
             tpreclist(ji)%tbw   = .FAlSE.
             tpreclist(ji)%tbr   = .FAlSE.
          ELSE
             tpreclist(ji)%found = .TRUE.
             ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
             IF (ileng > sizemax) sizemax = ileng        
#ifndef LOWMEM
             ALLOCATE(lfiart(ji)%iwtab(ileng))
#endif
          end IF
       END DO

       maxvar = nbvar_tbr+nbvar_calc

DO ji=1,nbvar_tbr+nbvar_calc
  print *,ji,'name=',trim(tpreclist(ji)%name),' calc=',tpreclist(ji)%calc,' tbw=',tpreclist(ji)%tbw,&
          ' tbr=',tpreclist(ji)%tbr,' found=',tpreclist(ji)%found
END DO

    ELSE
       ! Entire file is converted
#ifndef LOWMEM
       IF(.NOT.ALLOCATED(lfiart)) ALLOCATE(lfiart(nbvar_lfi))
#endif
       ALLOCATE(tpreclist(nbvar_lfi))
       DO ji=1,nbvar_lfi
         tpreclist(ji)%calc   = .FALSE. !By default variables are not computed from others
         tpreclist(ji)%tbw    = .TRUE.  !By default variables are written
         tpreclist(ji)%src(:) = -1
       END DO

       CALL LFIPOS(iresp,ilu)
       ladvan = .TRUE.
       
       DO ji=1,nbvar_lfi
          CALL LFICAS(iresp,ilu,yrecfm,ileng,ipos,ladvan)
          ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
          tpreclist(ji)%name = trim(yrecfm)
          tpreclist(ji)%found  = .TRUE.
          IF (ileng > sizemax) sizemax = ileng        
#ifndef LOWMEM       
          ALLOCATE(lfiart(ji)%iwtab(ileng))
#endif
       END DO
       maxvar = nbvar_lfi
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

       yrecfm = trim(tpreclist(ji)%name)//trim(suffix)
       CALL LFINFO(iresp,ilu,yrecfm,ileng,ipos)
#ifdef LOWMEM
       CALL LFILEC(iresp,ilu,yrecfm,iwork,ileng)
       tpreclist(ji)%TYPE = get_ftype(yrecfm)               
       tpreclist(ji)%grid = iwork(1)

       ALLOCATE(character(len=iwork(2)) :: tpreclist(ji)%comment)
       DO jj=1,iwork(2)
          ich = iwork(2+jj)
          tpreclist(ji)%comment(jj:jj) = CHAR(ich)
       END DO
       fsize = ileng-(2+iwork(2))
#else
       CALL LFILEC(iresp,ilu,yrecfm,lfiart(ji)%iwtab,ileng)
       tpreclist(ji)%TYPE = get_ftype(yrecfm)               
       tpreclist(ji)%grid = lfiart(ji)%iwtab(1)

       ALLOCATE(character(len=lfiart(ji)%iwtab(2)) :: tpreclist(ji)%comment)
       DO jj=1,lfiart(ji)%iwtab(2)
          ich = lfiart(ji)%iwtab(2+jj)
          tpreclist(ji)%comment(jj:jj) = CHAR(ich)
       END DO
       fsize = ileng-(2+lfiart(ji)%iwtab(2))
#endif
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
  END SUBROUTINE parse_lfi
  
  SUBROUTINE read_data_lfi(klu, hvarlist, nbvar, tpreclist, kbuflen, current_level)
    INTEGER, INTENT(IN)                    :: klu
    INTEGER, INTENT(INOUT)                 :: nbvar
    CHARACTER(LEN=*), intent(IN)           :: hvarlist
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

    ilu = klu

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

  SUBROUTINE def_ncdf(tpreclist,nbvar,oreduceprecision,kcdf_id,omerge,ocompress,compress_level)
    TYPE(workfield),DIMENSION(:),INTENT(INOUT) :: tpreclist
    INTEGER,                     INTENT(IN) :: nbvar
    LOGICAL,                     INTENT(IN) :: oreduceprecision
    INTEGER,                     INTENT(OUT):: kcdf_id
    LOGICAL,                     INTENT(IN) :: omerge
    LOGICAL,                     INTENT(IN) :: ocompress
    INTEGER,                     INTENT(IN) :: compress_level

    INTEGER :: status
    INTEGER :: ji
    TYPE(dimCDF), POINTER :: tzdim
    INTEGER               :: invdims
    INTEGER               :: type_float
    INTEGER, DIMENSION(10) :: ivdims
    CHARACTER(LEN=20)     :: ycdfvar


    IF (oreduceprecision) THEN
      type_float = NF90_REAL
    ELSE
      type_float = NF90_DOUBLE
    END IF

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

    PRINT *,'------------- NetCDF DEFINITION ---------------'

    ! define VARIABLES and ATTRIBUTES
    DO ji=1,nbvar
       IF (.NOT.tpreclist(ji)%tbw) CYCLE

       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
         IF (tpreclist(ji)%dim%create) THEN
           invdims   = 1
           ivdims(1) = tpreclist(ji)%dim%id
         ELSE
           invdims = tpreclist(ji)%dim%ndims
           IF(omerge) invdims=invdims+1 !when merging variables from LFI splitted files
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

       SELECT CASE(tpreclist(ji)%TYPE)
       CASE (TEXT)
!          PRINT *,'TEXT : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_CHAR,&
                   ivdims(:invdims),tpreclist(ji)%id)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE (INT,BOOL)
!          PRINT *,'INT,BOOL : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,NF90_INT,&
                   ivdims(:invdims),tpreclist(ji)%id)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE(FLOAT)
!          PRINT *,'FLOAT : ',tpreclist(ji)%name
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,type_float,&
                   ivdims(:invdims),tpreclist(ji)%id)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

          
       CASE default
          PRINT *,'ATTENTION : ',TRIM(tpreclist(ji)%name),' est de&
               & TYPE inconnu --> force a REAL'
          status = NF90_DEF_VAR(kcdf_id,ycdfvar,type_float,&
                   ivdims(:invdims),tpreclist(ji)%id)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
          

       END SELECT

       ! Compress data (costly operation for the CPU)
       IF (ocompress .AND. invdims>0) THEN
         status = NF90_DEF_VAR_DEFLATE(kcdf_id,tpreclist(ji)%id,1,1,compress_level)
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       END IF

       ! GRID attribute definition
       status = NF90_PUT_ATT(kcdf_id,tpreclist(ji)%id,'GRID',tpreclist(ji)%grid)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       ! COMMENT attribute definition
       status = NF90_PUT_ATT(kcdf_id,tpreclist(ji)%id,'COMMENT',trim(tpreclist(ji)%comment))
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

    END DO
    
    status = NF90_ENDDEF(kcdf_id)
    IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

    
  END SUBROUTINE def_ncdf

  SUBROUTINE fill_ncdf(klu,kcdf_id,tpreclist,knaf,kbuflen,current_level)
    INTEGER,                      INTENT(IN):: klu
    INTEGER,                      INTENT(IN):: kcdf_id
    TYPE(workfield), DIMENSION(:),INTENT(IN):: tpreclist
    INTEGER,                      INTENT(IN):: knaf
    INTEGER,                      INTENT(IN):: kbuflen
    INTEGER, INTENT(IN), OPTIONAL           :: current_level

#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER                                  :: ji,jj
    INTEGER,DIMENSION(:),ALLOCATABLE         :: itab
    REAL   (KIND=8),DIMENSION(:),ALLOCATABLE :: xtab
    CHARACTER, DIMENSION(:), ALLOCATABLE     :: ytab
    INTEGER                                  :: status
    INTEGER                                  :: extent, ndims
    INTEGER                                  :: ich
    INTEGER                                  :: src
    INTEGER                                  :: level
    INTEGER(KIND=LFI_INT)                    :: iresp,ilu,ileng,ipos
    CHARACTER(LEN=4)                         :: suffix

    !
    ilu = klu
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

    DO ji=1,knaf
       IF (.NOT.tpreclist(ji)%tbw) CYCLE

#if LOWMEM
       CALL LFINFO(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),ileng,ipos)
       CALL LFILEC(iresp,ilu,trim(tpreclist(ji)%name)//trim(suffix),iwork,ileng)
#endif
       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
          extent = tpreclist(ji)%dim%len
          ndims = tpreclist(ji)%dim%ndims
       ELSE
          extent = 1
          ndims = 0
       END IF

       SELECT CASE(tpreclist(ji)%TYPE)
       CASE (INT,BOOL)
#if LOWMEM
***
print *,'lowmem: not tested!!!!!' (to be compared with no low mem version)
         itab(1:extent) = iwork(3+iwork(2):)
#else
         IF (.NOT.tpreclist(ji)%calc) THEN
           itab(1:extent) = lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):)
         ELSE
           src=tpreclist(ji)%src(1)
           xtab(1:extent) = lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):)
           jj = 2
           DO WHILE (tpreclist(ji)%src(jj)>0 .AND. jj.LE.MAXRAW)
             src=tpreclist(ji)%src(jj)
             xtab(1:extent) = xtab(1:extent) + lfiart(src)%iwtab(3+lfiart(src)%iwtab(2):)
             jj=jj+1
           END DO
         END IF
         itab(1:extent) = lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):)
#endif
!TODO: works in all cases??? (X, Y, Z dimensions assumed to be ptdimx,y or z)
         SELECT CASE(ndims)
         CASE (0)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,itab(1))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,itab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(itab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(itab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT
         
       CASE (FLOAT)
#if LOWMEM
***
print *,'lowmem: not tested!!!!!' (to be compared with no low mem version)
         xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
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
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,xtab(1))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,xtab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(xtab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(xtab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT

       CASE (TEXT)
         ALLOCATE(ytab(extent))
         DO jj=1,extent
#if LOWMEM
***
print *,'lowmem: not tested!!!!!' (to be compared with no low mem version)
           ich = iwork(2+iwork(2)+jj)
#else
           ich = lfiart(ji)%iwtab(2+lfiart(ji)%iwtab(2)+jj)
#endif
           ytab(jj) = CHAR(ich)
         END DO
         status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,ytab,count=(/extent/))
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
         DEALLOCATE(ytab)

       CASE default
#if LOWMEM
***
print *,'lowmem: not tested!!!!!' (to be compared with no low mem version)
         xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
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
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,xtab(1))
         CASE (1)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,xtab(1:extent),count=(/extent/))
         CASE (2)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(xtab,(/ptdimx%len,ptdimy%len/)), &
                                 start = (/1,1,level/) )
         CASE (3)
           status = NF90_PUT_VAR(kcdf_id,tpreclist(ji)%id,reshape(xtab,(/ptdimx%len,ptdimy%len,ptdimz%len/)))
         CASE DEFAULT
           print *,'Error: arrays with ',tpreclist(ji)%dim%ndims,' dimensions are not supported'
         END SELECT
         IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       END SELECT

    END DO
    DEALLOCATE(itab,xtab)
#if LOWMEM
    DEALLOCATE(iwork)
#endif 
  END SUBROUTINE fill_ncdf

  SUBROUTINE parse_cdf(kcdf_id,tpreclist,kbuflen)
    INTEGER, INTENT(IN)                    :: kcdf_id
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist
    INTEGER, INTENT(OUT)                   :: kbuflen


    INTEGER :: status
    INTEGER :: nvars, var_id
    INTEGER :: jdim
    INTEGER :: sizemax
    INTEGER :: itype
    INTEGER, DIMENSION(10) :: idim_id
    INTEGER :: icomlen,idimlen,idims,idimtmp
    
    status = NF90_INQUIRE(kcdf_id, nvariables = nvars)
    IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    ALLOCATE(tpreclist(nvars))

    sizemax = 0

    CALL init_dimCDF()
    
    ! Parcours de toutes les variables et extraction des infos
    !      - nom de dimension
    !      - dimension, etendue
    !      - attributs
    DO var_id = 1, nvars
       ! Pour la forme
       tpreclist(var_id)%id = var_id  
       
       ! Nom, type et dimensions de la variable
       status = NF90_INQUIRE_VARIABLE(kcdf_id, var_id, name = tpreclist(var_id)%name, xtype = itype, ndims = idims, &
                                      dimids = idim_id)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       SELECT CASE(itype)
       CASE(NF90_CHAR)
          tpreclist(var_id)%TYPE = TEXT
       CASE(NF90_INT)
          tpreclist(var_id)%TYPE = INT
       CASE(NF90_FLOAT,NF90_DOUBLE)
          tpreclist(var_id)%TYPE = FLOAT
       CASE default 
          PRINT *, 'Attention : variable ',TRIM(tpreclist(var_id)&
               & %name), ' a un TYPE non reconnu par le convertisseur.'
          PRINT *, '--> TYPE force a REAL(KIND 8) dans LFI !'
       END SELECT
      
       IF (idims == 0) THEN
          ! variable scalaire
          NULLIFY(tpreclist(var_id)%dim)
	  idimlen = 1
       ELSE
          ! infos sur dimensions
          idimlen = 1
          DO jdim=1,idims
            status = NF90_INQUIRE_DIMENSION(kcdf_id,idim_id(jdim),len = idimtmp)
            IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
            idimlen = idimlen*idimtmp
          END DO
          
          tpreclist(var_id)%dim=>get_dimCDF(idimlen)
          ! seul le champ 'len' de dimCDF sera utilise par la suite
       END IF
       
       ! GRID et COMMENT attributes
       status = NF90_GET_ATT(kcdf_id,var_id,'GRID',tpreclist(var_id)%grid)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       status = NF90_INQUIRE_ATTRIBUTE(kcdf_id,var_id,'COMMENT',len = icomlen)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       ALLOCATE(character(len=icomlen) :: tpreclist(var_id)%comment)
       status = NF90_GET_ATT(kcdf_id,var_id,'COMMENT',tpreclist(var_id)%comment)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       
       IF (sizemax < icomlen+idimlen) sizemax = icomlen+idimlen 

    END DO
    
    kbuflen = sizemax

  END SUBROUTINE parse_cdf

  SUBROUTINE build_lfi(kcdf_id,klu,tpreclist,kbuflen)
    INTEGER,                       INTENT(IN) :: kcdf_id 
    INTEGER,                       INTENT(IN) :: klu
    TYPE(workfield), DIMENSION(:), INTENT(IN) :: tpreclist
    INTEGER,                       INTENT(IN) :: kbuflen
    
    INTEGER :: status
    INTEGER :: ivar,jj
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: iwork
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: idata
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xtab
    INTEGER,      DIMENSION(:), ALLOCATABLE :: itab
    CHARACTER,    DIMENSION(:), ALLOCATABLE :: ytab
    CHARACTER(LEN=FM_FIELD_SIZE)            :: yrecfm

    INTEGER :: iartlen, idlen, icomlen
    INTEGER(KIND=LFI_INT) :: iresp,ilu,iartlen8

    ! Un article LFI est compose de :
    !   - 1 entier identifiant le numero de grille
    !   - 1 entier contenant la taille du commentaire
    !   - le commentaire code en entier 64 bits
    !   - les donnees proprement dites

    PRINT *,'Taille buffer = ',2+kbuflen

    ALLOCATE(iwork(2+kbuflen))
    ALLOCATE(itab(2+kbuflen))
    ALLOCATE(xtab(2+kbuflen))

    DO ivar=1,SIZE(tpreclist)
       icomlen = LEN(tpreclist(ivar)%comment)

       ! traitement Grille et Commentaire
       iwork(1) = tpreclist(ivar)%grid
       iwork(2) = icomlen
       DO jj=1,iwork(2)
          iwork(2+jj)=ICHAR(tpreclist(ivar)%comment(jj:jj))
       END DO

       IF (ASSOCIATED(tpreclist(ivar)%dim)) THEN
          idlen = tpreclist(ivar)%dim%len
       ELSE 
          idlen = 1
       END IF
       
       iartlen = 2+icomlen+idlen
       idata=>iwork(3+icomlen:iartlen)


       SELECT CASE(tpreclist(ivar)%TYPE)
       CASE(INT,BOOL)
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id,itab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'INT,BOOL --> ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = itab(1:idlen)

       CASE(FLOAT)
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id,xtab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
          
!          PRINT *,'FLOAT    --> ',tpreclist(ivar)%name,',len = ',idlen
          ! La ligne suivante ne pose aucun pb sur Cray alors que sur
          ! fuji, elle genere une erreur d'execution
!          idata(1:idlen) = TRANSFER(xtab(1:idlen),(/ 0_8 /))
          
          ! la correction pour Fuji (valable sur CRAY) est :
          idata(1:idlen) = TRANSFER(xtab,(/ 0_8 /),idlen)

!          IF (idlen < 10) PRINT *,'xtab = ',xtab(1:idlen)

       CASE(TEXT)
          ALLOCATE(ytab(idlen))
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id,ytab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TEXT -->     ',tpreclist(ivar)%name,',len = ',idlen

          DO jj=1,idlen
             idata(jj) = ICHAR(ytab(jj))
          END DO
          
          DEALLOCATE(ytab)

       CASE default
          status = NF90_GET_VAR(kcdf_id,tpreclist(ivar)%id,xtab)
          IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

          PRINT *,'Default (ERROR) -->',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = TRANSFER(xtab,(/ 0_8 /),idlen)

       END SELECT
       
       ! Attention restoration des '%' dans le nom des champs LFI
       yrecfm = str_replace(tpreclist(ivar)%name,'__','%')
       ! et des '.'
       yrecfm = str_replace(yrecfm,'--','.')
       ilu = klu
       iartlen8 = iartlen
       CALL LFIECR(iresp,ilu,yrecfm,iwork,iartlen8)

    END DO
    DEALLOCATE(iwork,itab,xtab)

  END SUBROUTINE build_lfi

  SUBROUTINE OPEN_FILES(hinfile,houtfile,olfi2cdf,olfilist,ohdf5,kcdf_id,klu,knaf)
    LOGICAL,          INTENT(IN)  :: olfi2cdf, olfilist, ohdf5
    CHARACTER(LEN=*), INTENT(IN)  :: hinfile
    CHARACTER(LEN=*), INTENT(IN)  :: houtfile
    INTEGER         , INTENT(OUT) :: kcdf_id,klu,knaf

    INTEGER                     :: extindex
    INTEGER(KIND=LFI_INT)       :: ilu,iresp,iverb,inap,inaf
    INTEGER                     :: status
    CHARACTER(LEN=4)            :: ypextsrc, ypextdest
    LOGICAL                     :: fexist
    INTEGER                     :: omode

    iverb = 0
    ilu   = 11

    CALL init_sysfield()

    IF (olfi2cdf) THEN 
       ! Cas LFI -> NetCDF
       CALL LFIOUV(iresp,ilu,ltrue,hinfile,'OLD',lfalse&
            & ,lfalse,iverb,inap,inaf)

       IF (olfilist) THEN
          CALL LFILAF(iresp,ilu,lfalse)
          CALL LFIFER(iresp,ilu,'KEEP')
          return
       end IF

       IF (ohdf5) THEN
          status = NF90_CREATE(houtfile, IOR(NF90_CLOBBER,NF90_NETCDF4), kcdf_id)
       ELSE
          status = NF90_CREATE(houtfile, IOR(NF90_CLOBBER,NF90_64BIT_OFFSET), kcdf_id) 
       end IF
       
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)

       status = NF90_SET_FILL(kcdf_id,NF90_NOFILL,omode)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
!!$       SELECT CASE(omode)
!!$       CASE (NF90_FILL)
!!$          PRINT *,'Ancien mode : NF90_FILL'
!!$       CASE (NF90_NOFILL)
!!$          PRINT *,'Ancien mode : NF90_NOFILL'
!!$       CASE default
!!$          PRINT *, 'Ancien mode : inconnu'
!!$       END SELECT
       
    ELSE
       ! Cas NetCDF -> LFI
       status = NF90_OPEN(hinfile,NF90_NOWRITE,kcdf_id)
       IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       inap = 100
       CALL LFIOUV(iresp,ilu,ltrue,houtfile,'NEW'&
            & ,lfalse,lfalse,iverb,inap,inaf)
    END IF

    klu  = ilu
    knaf = inaf

    PRINT *,'--> Fichier converti : ', houtfile

  END SUBROUTINE OPEN_FILES
  
  SUBROUTINE CLOSE_FILES(klu,kcdf_id)
    INTEGER, INTENT(IN) :: klu, kcdf_id
    
    INTEGER(KIND=LFI_INT) :: iresp,ilu
    INTEGER               :: status

    ilu = klu
    ! close LFI file
    CALL LFIFER(iresp,ilu,'KEEP')

    ! close NetCDF file
    status = NF90_CLOSE(kcdf_id)
    IF (status /= NF90_NOERR) CALL HANDLE_ERR(status,__LINE__)
    
  END SUBROUTINE CLOSE_files
  
END MODULE mode_util
