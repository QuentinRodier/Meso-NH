MODULE mode_util
  USE MODE_FIELDTYPE
  USE mode_dimlist
  USE MODD_PARAM

  IMPLICIT NONE 

  TYPE workfield
     CHARACTER(LEN=FM_FIELD_SIZE)            :: name   ! nom du champ
     INTEGER                                 :: TYPE   ! type (entier ou reel)    
     CHARACTER(LEN=1), DIMENSION(:), POINTER :: comment
     TYPE(dimCDF),                   POINTER :: dim
     INTEGER                                 :: id
     INTEGER                                 :: grid
  END TYPE workfield

#ifndef LOWMEM
  TYPE lfidata
     INTEGER(KIND=8), DIMENSION(:), POINTER :: iwtab
  END TYPE lfidata
  TYPE(lfidata), DIMENSION(:), ALLOCATABLE :: lfiart
#endif
  
  INCLUDE 'netcdf.inc'

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
  INTEGER, INTENT(IN)         :: klu ! logical fortran unit au lfi file
  CHARACTER(LEN=*),INTENT(IN) :: hrecfm ! article name to be read
  INTEGER, INTENT(OUT)        :: kval ! integer value for hrecfm article
  INTEGER, INTENT(OUT)        :: kresp! return code null if OK
  !
  INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE::iwork
  INTEGER :: iresp,ilenga,iposex,icomlen
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

  SUBROUTINE parse_lfi(klu, knaf, tpreclist, kbuflen)
    INTEGER, INTENT(IN)                    :: klu
    INTEGER, INTENT(IN)                    :: knaf
    TYPE(workfield), DIMENSION(:), POINTER :: tpreclist    
    INTEGER, INTENT(OUT)                   :: kbuflen

    INTEGER                                  :: ji,jj
    INTEGER                                  :: ileng,ipos
    LOGICAL                                  :: ladvan
    INTEGER                                  :: iresp
    INTEGER                                  :: ich
    INTEGER                                  :: fsize,sizemax
    CHARACTER(LEN=FM_FIELD_SIZE)             :: yrecfm
#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    !JUAN CYCCL3
    INTEGER,PARAMETER                        :: JPHEXT=1 ! 3

    ! First check if IMAX,JMAX,KMAX exist in LFI file
    ! to handle 3D, 2D variables -> update IDIMX,IDIMY,IDIMZ
    CALL FMREADLFIN1(klu,'IMAX',IDIMX,iresp)
    IF (iresp == 0) IDIMX = IDIMX+2*JPHEXT  ! IMAX + 2*JPHEXT
    !
    CALL FMREADLFIN1(klu,'JMAX',IDIMY,iresp)
    IF (iresp == 0) IDIMY = IDIMY+2*JPHEXT  ! JMAX + 2*JPHEXT
    !
    CALL FMREADLFIN1(klu,'KMAX',IDIMZ,iresp)
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

#ifndef LOWMEM
    ALLOCATE(lfiart(knaf))
#endif
    ALLOCATE(tpreclist(knaf))
    sizemax = 0

    CALL LFIPOS(iresp,klu)
    ladvan = .TRUE.

    ! Phase 1 : build articles list to convert.
    !
    !    Pour l'instant tous les articles du fichier LFI sont
    !    convertis. On peut modifier cette phase pour prendre en
    !    compte un sous-ensemble d'article (liste definie par
    !    l'utilisateur par exemple)  
    !
    DO ji=1,knaf
       CALL LFICAS(iresp,klu,yrecfm,ileng,ipos,ladvan)
       ! PRINT *,'Article ',ji,' : ',TRIM(yrecfm),', longueur = ',ileng
       tpreclist(ji)%name = yrecfm
       IF (ileng > sizemax) sizemax = ileng        
#ifndef LOWMEM       
       ALLOCATE(lfiart(ji)%iwtab(ileng))
#endif
    END DO
    kbuflen = sizemax
#ifdef LOWMEM
    WRITE(*,'("Taille maximale du buffer :",f10.3," Mo")') sizemax*8./1048576.
    ALLOCATE(iwork(sizemax))
#endif
    ! Phase 2 : Extract comments and dimensions for valid articles.
    !           Infos are put in tpreclist.
    CALL init_dimCDF()
    DO ji=1,knaf
       yrecfm = tpreclist(ji)%name
       CALL LFINFO(iresp,klu,yrecfm,ileng,ipos)
#ifdef LOWMEM
       CALL LFILEC(iresp,klu,yrecfm,iwork,ileng)
       tpreclist(ji)%TYPE = get_ftype(yrecfm)               
       tpreclist(ji)%grid = iwork(1)

       ALLOCATE(tpreclist(ji)%comment(iwork(2)))
       DO jj=1,iwork(2)
          ich = iwork(2+jj)
          tpreclist(ji)%comment(jj:jj) = CHAR(ich)
       END DO
       fsize = ileng-(2+iwork(2))
#else
       CALL LFILEC(iresp,klu,yrecfm,lfiart(ji)%iwtab,ileng)
       tpreclist(ji)%TYPE = get_ftype(yrecfm)               
       tpreclist(ji)%grid = lfiart(ji)%iwtab(1)

       ALLOCATE(tpreclist(ji)%comment(lfiart(ji)%iwtab(2)))
       DO jj=1,lfiart(ji)%iwtab(2)
          ich = lfiart(ji)%iwtab(2+jj)
          tpreclist(ji)%comment(jj:jj) = CHAR(ich)
       END DO
       fsize = ileng-(2+lfiart(ji)%iwtab(2))
#endif
       tpreclist(ji)%dim=>get_dimCDF(fsize)
    END DO
  
    PRINT *,'Nombre de dimensions = ', size_dimCDF()
#ifdef LOWMEM
    DEALLOCATE(iwork)
#endif
  END SUBROUTINE parse_lfi
  
  SUBROUTINE HANDLE_ERR(status,line)
    INTEGER :: status,line

    IF (status /= NF_NOERR) THEN
       PRINT *, 'line ',line,': ',NF_STRERROR(status)
       STOP
    END IF
  END SUBROUTINE HANDLE_ERR

  SUBROUTINE def_ncdf(tpreclist,kcdf_id)
    TYPE(workfield),DIMENSION(:),INTENT(IN) :: tpreclist    
    INTEGER,                     INTENT(OUT):: kcdf_id

    INTEGER :: status
    INTEGER :: ji
    TYPE(dimCDF), POINTER :: tzdim
    INTEGER               :: invdims
    INTEGER, DIMENSION(10) :: ivdims
    CHARACTER(LEN=20)     :: ycdfvar


    ! global attributes
    status = NF_PUT_ATT_TEXT(kcdf_id,NF_GLOBAL,'Title'&
         & ,LEN(VERSION_ID),VERSION_ID)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

    ! define DIMENSIONS
    tzdim=>first_DimCDF()
    DO WHILE(ASSOCIATED(tzdim))
      IF (tzdim%create) THEN
        status = NF_DEF_DIM(kcdf_id,tzdim%name,tzdim%len,tzdim%id)
        IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
      END IF
      tzdim=>tzdim%next
    END DO

    PRINT *,'------------- NetCDF DEFINITION ---------------'

    ! define VARIABLES and ATTRIBUTES
    DO ji=1,SIZE(tpreclist)
      
       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
         IF (tpreclist(ji)%dim%create) THEN
           invdims   = 1
           ivdims(1) = tpreclist(ji)%dim%id
         ELSE
           invdims = tpreclist(ji)%dim%ndims
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
          status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_CHAR,&
                   invdims,ivdims,tpreclist(ji)%id)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 

       CASE (INT,BOOL)
!          PRINT *,'INT,BOOL : ',tpreclist(ji)%name
          status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_INT,&
                   invdims,ivdims,tpreclist(ji)%id)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 

       CASE(FLOAT)
!          PRINT *,'FLOAT : ',tpreclist(ji)%name
          status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_DOUBLE,&
                   invdims,ivdims,tpreclist(ji)%id)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 

          
       CASE default
          PRINT *,'ATTENTION : ',TRIM(tpreclist(ji)%name),' est de&
               & TYPE inconnu --> force a REAL'
          status = NF_DEF_VAR(kcdf_id,ycdfvar,NF_DOUBLE,&
                   invdims,ivdims,tpreclist(ji)%id)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__) 
          

       END SELECT

       ! GRID attribute definition
       status = NF_PUT_ATT_INT(kcdf_id,tpreclist(ji)%id,'GRID',NF_INT,&
                               1,tpreclist(ji)%grid)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       ! COMMENT attribute definition
       status = NF_PUT_ATT_TEXT(kcdf_id,tpreclist(ji)%id,'COMMENT',&
            SIZE(tpreclist(ji)%comment),tpreclist(ji)%comment(1))
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
    END DO
    
    status = NF_ENDDEF(kcdf_id)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

    
  END SUBROUTINE def_ncdf

  SUBROUTINE fill_ncdf(klu,kcdf_id,tpreclist,kbuflen)
    INTEGER,                      INTENT(IN):: klu
    INTEGER,                      INTENT(IN):: kcdf_id
    TYPE(workfield), DIMENSION(:),INTENT(IN):: tpreclist    
    INTEGER,                      INTENT(IN):: kbuflen
#ifdef LOWMEM
    INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE :: iwork
#endif
    INTEGER                                  :: ji,jj
    INTEGER,DIMENSION(:),ALLOCATABLE :: itab
    REAL   (KIND=8),DIMENSION(:),ALLOCATABLE :: xtab
    CHARACTER, DIMENSION(:), ALLOCATABLE     :: ytab
    INTEGER                                  :: status
    INTEGER                                  :: iresp
    INTEGER                                  :: ileng
    INTEGER                                  :: ipos
    INTEGER                                  :: extent
    INTEGER                                  :: ich
    !
#if LOWMEM
    ALLOCATE(iwork(kbuflen))
#endif
    ALLOCATE(itab(kbuflen))
    ALLOCATE(xtab(kbuflen))

    DO ji=1,SIZE(tpreclist)
#if LOWMEM
       CALL LFINFO(iresp,klu,tpreclist(ji)%name,ileng,ipos)
       CALL LFILEC(iresp,klu,tpreclist(ji)%name,iwork,ileng)
#endif
       IF (ASSOCIATED(tpreclist(ji)%dim)) THEN
          extent = tpreclist(ji)%dim%len
       ELSE
          extent = 1
       END IF

       SELECT CASE(tpreclist(ji)%TYPE)
       CASE (INT,BOOL)
#if LOWMEM
         itab(1:extent) = iwork(3+iwork(2):)
#else
         itab(1:extent) = lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):)
#endif
         status = NF_PUT_VAR_INT(kcdf_id,tpreclist(ji)%id,itab)
         IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
         
       CASE (FLOAT)
#if LOWMEM
         xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
#else
         xtab(1:extent) = TRANSFER(lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):),(/ 0.0_8 /))
#endif
         status = NF_PUT_VAR_DOUBLE(kcdf_id,tpreclist(ji)%id,xtab)
         IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       CASE (TEXT)
         ALLOCATE(ytab(extent))
         DO jj=1,extent
#if LOWMEM
           ich = iwork(2+iwork(2)+jj)
#else
           ich = lfiart(ji)%iwtab(2+lfiart(ji)%iwtab(2)+jj)
#endif
           ytab(jj) = CHAR(ich)
         END DO

         status = NF_PUT_VAR_TEXT(kcdf_id,tpreclist(ji)%id,ytab)
         IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
         DEALLOCATE(ytab)
       CASE default
#if LOWMEM
         xtab(1:extent) = TRANSFER(iwork(3+iwork(2):),(/ 0.0_8 /))
#else         
         xtab(1:extent) = TRANSFER(lfiart(ji)%iwtab(3+lfiart(ji)%iwtab(2):),(/ 0.0_8 /))
#endif
         status = NF_PUT_VAR_DOUBLE(kcdf_id,tpreclist(ji)%id,xtab)
         IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
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
    
    status = NF_INQ_NVARS(kcdf_id, nvars)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
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
       
       ! Nom de la variable
       status = NF_INQ_VARNAME(kcdf_id, var_id, tpreclist(var_id)%name)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       ! Type de la variable
       status = NF_INQ_VARTYPE(kcdf_id, var_id, itype)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       SELECT CASE(itype)
       CASE(NF_CHAR)
          tpreclist(var_id)%TYPE = TEXT
       CASE(NF_INT)
          tpreclist(var_id)%TYPE = INT
       CASE(NF_FLOAT,NF_DOUBLE)
          tpreclist(var_id)%TYPE = FLOAT
       CASE default 
          PRINT *, 'Attention : variable ',TRIM(tpreclist(var_id)&
               & %name), ' a un TYPE non reconnu par le convertisseur.'
          PRINT *, '--> TYPE force a REAL(KIND 8) dans LFI !'
       END SELECT
      
       ! Dimension de la variable
       status = NF_INQ_VARNDIMS(kcdf_id, var_id, idims)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       IF (idims == 0) THEN
          ! variable scalaire
          NULLIFY(tpreclist(var_id)%dim)
	  idimlen = 1
       ELSE
          ! infos sur dimensions
          status = NF_INQ_VARDIMID(kcdf_id, var_id, idim_id)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

          idimlen = 1
          DO jdim=1,idims
            status = NF_INQ_DIMLEN(kcdf_id,idim_id(jdim),idimtmp)
            IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
            idimlen = idimlen*idimtmp
          END DO
          
          tpreclist(var_id)%dim=>get_dimCDF(idimlen)
          ! seul le champ 'len' de dimCDF sera utilise par la suite
       END IF
       
       ! GRID et COMMENT attributes
       status = NF_GET_ATT_INT(kcdf_id,var_id,'GRID',tpreclist(var_id)%grid)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       status = NF_INQ_ATTLEN(kcdf_id,var_id,'COMMENT',icomlen)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       ALLOCATE(tpreclist(var_id)%comment(icomlen))
       status = NF_GET_ATT_TEXT(kcdf_id,var_id,'COMMENT',tpreclist(var_id)%comment)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       
       IF (sizemax < icomlen+idimlen) sizemax = icomlen+idimlen 

    END DO
    
    kbuflen = sizemax

  END SUBROUTINE parse_cdf

  SUBROUTINE build_lfi(kcdf_id,klu,tpreclist,kbuflen)
    INTEGER,                       INTENT(IN) :: kcdf_id 
    INTEGER,                       INTENT(IN) :: klu
    TYPE(workfield), DIMENSION(:), INTENT(IN) :: tpreclist
    INTEGER,                       INTENT(IN) :: kbuflen
    
    INTEGER :: iresp
    INTEGER :: status
    INTEGER :: ivar,jj
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: iwork
    INTEGER(KIND=8), DIMENSION(:), POINTER  :: idata
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xtab
    INTEGER,      DIMENSION(:), ALLOCATABLE :: itab
    CHARACTER,    DIMENSION(:), ALLOCATABLE :: ytab
    CHARACTER(LEN=FM_FIELD_SIZE)            :: yrecfm

    INTEGER :: iartlen, idlen, icomlen

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
       icomlen = SIZE(tpreclist(ivar)%comment)

       ! traitement Grille et Commentaire
       iwork(1) = tpreclist(ivar)%grid
       iwork(2) = icomlen
       DO jj=1,iwork(2)
          iwork(2+jj)=ICHAR(tpreclist(ivar)%comment(jj))
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
          status = NF_GET_VAR_INT(kcdf_id,tpreclist(ivar)%id,itab)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'INT,BOOL --> ',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = itab(1:idlen)

       CASE(FLOAT)
          status = NF_GET_VAR_DOUBLE(kcdf_id,tpreclist(ivar)%id,xtab)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
          
!          PRINT *,'FLOAT    --> ',tpreclist(ivar)%name,',len = ',idlen
          ! La ligne suivante ne pose aucun pb sur Cray alors que sur
          ! fuji, elle genere une erreur d'execution
!          idata(1:idlen) = TRANSFER(xtab(1:idlen),(/ 0_8 /))
          
          ! la correction pour Fuji (valable sur CRAY) est :
          idata(1:idlen) = TRANSFER(xtab,(/ 0_8 /),idlen)

!          IF (idlen < 10) PRINT *,'xtab = ',xtab(1:idlen)

       CASE(TEXT)
          ALLOCATE(ytab(idlen))
          status = NF_GET_VAR_TEXT(kcdf_id,tpreclist(ivar)%id,ytab)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

!          PRINT *,'TEXT -->     ',tpreclist(ivar)%name,',len = ',idlen

          DO jj=1,idlen
             idata(jj) = ICHAR(ytab(jj))
          END DO
          
          DEALLOCATE(ytab)

       CASE default
          status = NF_GET_VAR_DOUBLE(kcdf_id,tpreclist(ivar)%id,xtab)
          IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

          PRINT *,'Default (ERROR) -->',tpreclist(ivar)%name,',len = ',idlen
          idata(1:idlen) = TRANSFER(xtab,(/ 0_8 /),idlen)

       END SELECT
       
       ! Attention restoration des '%' dans le nom des champs LFI
       yrecfm = str_replace(tpreclist(ivar)%name,'__','%')
       ! et des '.'
       yrecfm = str_replace(yrecfm,'--','.')
       CALL LFIECR(iresp,klu,yrecfm,iwork,iartlen)

    END DO
    DEALLOCATE(iwork,itab,xtab)

  END SUBROUTINE build_lfi

  SUBROUTINE OPEN_FILES(olfi2cdf,hfnam,kcdf_id,klu,knaf)
    LOGICAL,          INTENT(IN)  :: olfi2cdf
    CHARACTER(LEN=*), INTENT(IN)  :: hfnam
    INTEGER         , INTENT(OUT) :: kcdf_id,klu,knaf

    INTEGER :: iverb,inap
    INTEGER                     :: extindex
    INTEGER                     :: status
    CHARACTER(LEN=4)            :: ypextsrc, ypextdest
    INTEGER, PARAMETER          :: ilu=11
    CHARACTER(LEN(hfnam))       :: filename, basename
    LOGICAL                     :: fexist
    INTEGER                     :: omode
    filename = hfnam

    IF (olfi2cdf) THEN 
       ypextsrc  = '.lfi'
       ypextdest = '.cdf'
    ELSE 
       ypextsrc  = '.cdf'
       ypextdest = '.lfi'
    END IF

    extindex = INDEX(filename,ypextsrc,.TRUE.)
    IF (extindex /= 0) THEN
       basename = filename(1:extindex-1)
    ELSE
       basename = filename
    END IF
    
    INQUIRE(FILE=filename,EXIST=fexist)
    IF (.NOT. fexist) THEN
       filename = TRIM(basename)//ypextsrc
       INQUIRE(FILE=filename,EXIST=fexist)     
    END IF
    
    IF (.NOT. fexist) THEN
       PRINT *, 'Erreur, le fichier ',TRIM(filename),' n''existe&
            & pas...'
       STOP
    END IF
    
    PRINT *,'--> Fichier converti : ',TRIM(basename)//ypextdest
    
    iverb = 0
    
    CALL init_sysfield()

    IF (olfi2cdf) THEN 
       ! Cas LFI -> NetCDF
       CALL LFIOUV(status,ilu,.TRUE.,filename,'UNKNOWN',.FALSE.&
            & ,.FALSE.,iverb,inap,knaf)
    
       status = NF_CREATE(TRIM(basename)//ypextdest,&
                IOR(NF_CLOBBER,NF_64BIT_OFFSET), kcdf_id)

       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)

       status = NF_SET_FILL(kcdf_id,NF_NOFILL,omode)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
!!$       SELECT CASE(omode)
!!$       CASE (NF_FILL)
!!$          PRINT *,'Ancien mode : NF_FILL'
!!$       CASE (NF_NOFILL)
!!$          PRINT *,'Ancien mode : NF_NOFILL'
!!$       CASE default
!!$          PRINT *, 'Ancien mode : inconnu'
!!$       END SELECT
       
    ELSE
       ! Cas NetCDF -> LFI
       status = NF_OPEN(filename,NF_NOWRITE,kcdf_id)
       IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
       
       inap = 100
       CALL LFIOUV(status,ilu,.TRUE.,TRIM(basename)//ypextdest,'NEW'&
            & ,.FALSE.,.FALSE.,iverb,inap,knaf)
    END IF

    klu = ilu

  END SUBROUTINE OPEN_FILES
  
  SUBROUTINE CLOSE_FILES(klu,kcdf_id)
    INTEGER, INTENT(IN) :: klu, kcdf_id
    
    INTEGER :: status

    ! close LFI file
    CALL LFIFER(status,klu,'KEEP')

    ! close NetCDF file
    status = NF_CLOSE(kcdf_id)
    IF (status /= NF_NOERR) CALL HANDLE_ERR(status,__LINE__)
    
  END SUBROUTINE CLOSE_files
  
END MODULE mode_util
