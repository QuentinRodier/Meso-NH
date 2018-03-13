program LFI2CDF
  USE MODD_CONF,  ONLY: CPROGRAM
  USE MODD_TIMEZ, ONLY: TIMEZ

  USE MODE_FIELD,  ONLY: INI_FIELD_LIST
  USE MODE_IO_ll,  ONLY: INITIO_ll, SET_CONFIO_ll
  USE mode_options
  USE mode_util

  USE MODN_CONFIO, ONLY: LCDF4, LLFIOUT, LLFIREAD

  IMPLICIT NONE 

  INTEGER :: ibuflen
  INTEGER :: ji
  INTEGER :: nbvar_infile ! number of variables available in the input file
  INTEGER :: nbvar_tbr  ! number of variables to be read
  INTEGER :: nbvar_calc ! number of variables to be computed from others
  INTEGER :: nbvar_tbw  ! number of variables to be written
  INTEGER :: nbvar      ! number of defined variables
  INTEGER :: first_level, current_level, last_level, nb_levels
  CHARACTER(LEN=:),allocatable :: hvarlist
  TYPE(filelist_struct) :: infiles, outfiles
  TYPE(workfield), DIMENSION(:), POINTER :: tzreclist

  type(option),dimension(:),allocatable :: options
  character(len=:),allocatable :: hinfile, houtfile
  integer                      :: runmode


  CPROGRAM = 'LFICDF'

  CALL INITIO_ll()
  CALL VERSION
  CALL INI_CST

  ALLOCATE(TIMEZ) !Used by IO_WRITE_FIELD

  call read_commandline(options,hinfile,houtfile,runmode)

  IF (runmode == MODELFI2CDF) THEN
     LCDF4    = .TRUE.
     LLFIOUT  = .FALSE.
     LLFIREAD = .TRUE.
     CALL SET_CONFIO_ll()
  ELSE IF (runmode == MODECDF2CDF) THEN
     LCDF4    = .TRUE.
     LLFIOUT  = .FALSE.
     LLFIREAD = .FALSE.
     CALL SET_CONFIO_ll()
  ELSE
     LCDF4    = .TRUE.
     LLFIOUT  = .TRUE.
     LLFIREAD = .FALSE.
     CALL SET_CONFIO_ll()
  END IF

  CALL INI_FIELD_LIST(1)

  CALL OPEN_FILES(infiles, outfiles, hinfile, houtfile, nbvar_infile, options, runmode)
  IF (options(OPTLIST)%set) STOP

  IF (runmode == MODELFI2CDF .OR. runmode == MODECDF2CDF) THEN
     IF (options(OPTVAR)%set) THEN
        ! nbvar_tbr is computed from number of requested variables
        ! by counting commas, = and +
        nbvar_tbr  = 1
        nbvar_calc = 0
        nbvar_tbw = 1
        hvarlist = options(OPTVAR)%cvalue
        DO ji=1,len(hvarlist)
           IF (hvarlist(ji:ji) == ',' .OR.hvarlist(ji:ji) == '+') THEN
              nbvar_tbr = nbvar_tbr+1
           END IF
           IF (hvarlist(ji:ji) == ',') THEN
              nbvar_tbw = nbvar_tbw+1
           END IF
           IF (hvarlist(ji:ji) == '=') THEN
              nbvar_calc = nbvar_calc+1
           END IF
        END DO
        nbvar = nbvar_calc + nbvar_tbr
     ELSE
        nbvar = nbvar_infile
     END IF
  END IF

  IF (runmode == MODELFI2CDF) THEN
     ! Conversion LFI -> NetCDF

     !Standard treatment (one LFI file only)
     IF (.not.options(OPTMERGE)%set) THEN
       CALL parse_infiles(infiles,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,options)
       IF (options(OPTSPLIT)%set) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,options)
       CALL def_ncdf(outfiles,tzreclist,nbvar,options)
       CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,options)

     ELSE
     !Treat several LFI files and merge into 1 NC file

       !Determine first level (eg needed to find suffix of the variable name)
       read( hinfile(len(hinfile)-6:len(hinfile)-4) , "(I3)" ) first_level
       nb_levels = options(OPTMERGE)%ivalue
       current_level = first_level
       last_level    = first_level + nb_levels - 1

       !Read 1st LFI file
       CALL parse_infiles(infiles,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,options,current_level)
       IF (options(OPTSPLIT)%set) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,options)
       !Define NC variables
       CALL def_ncdf(outfiles,tzreclist,nbvar,options)

       DO current_level = first_level,last_level
         print *,'Treating level ',current_level
         IF (current_level/=first_level) THEN
           CALL open_split_lfifile_in(infiles,hinfile,current_level)
         END IF
         CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,options,current_level)
         IF (current_level/=last_level) CALL close_files(infiles)
       END DO
     END IF

  ELSE IF (runmode == MODECDF2CDF) THEN
     ! Conversion netCDF -> netCDF

     !Standard treatment (one netCDF file only)
     IF (.not.options(OPTMERGE)%set) THEN
       CALL parse_infiles(infiles,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,options,current_level)
       IF (options(OPTSPLIT)%set) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,options)
       CALL def_ncdf(outfiles,tzreclist,nbvar,options)
       CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,options)

     ELSE
     !Treat several NC files and merge into 1 NC file

       !Determine first level (eg needed to find suffix of the variable name)
       read( hinfile(len(hinfile)-5:len(hinfile)-3) , "(I3)" ) first_level
       nb_levels = options(OPTMERGE)%ivalue
       current_level = first_level
       last_level    = first_level + nb_levels - 1

       !Read 1st NC file
       CALL parse_infiles(infiles,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,options,current_level)
       IF (options(OPTSPLIT)%set) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,options)
       !Define NC variables
       CALL def_ncdf(outfiles,tzreclist,nbvar,options)

       DO current_level = first_level,last_level
         print *,'Treating level ',current_level
         IF (current_level/=first_level) THEN
           CALL open_split_ncfile_in(infiles,hinfile,current_level)
           CALL update_varid_in(infiles,hinfile,tzreclist,nbvar,current_level)
         END IF
         CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,options,current_level)
         IF (current_level/=last_level) CALL close_files(infiles)
       END DO
     END IF

  ELSE
     ! Conversion NetCDF -> LFI
     CALL parse_infiles(infiles,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,options,current_level)
     CALL build_lfi(infiles,outfiles,tzreclist,nbvar_infile,ibuflen)
  END IF
  
  CALL CLOSE_FILES(infiles)
  CALL CLOSE_FILES(outfiles)
  
end program LFI2CDF
