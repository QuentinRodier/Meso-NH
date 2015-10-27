subroutine  LFI2CDFMAIN(hinfile,iiflen,ooutname,houtfile,ioflen,hvarlist,ivlen,ocdf2cdf,olfi2cdf,olfilist,ohdf5,omerge,&
                        nb_levels,oreduceprecision,osplit,ocompress,compress_level)
  USE mode_util
  IMPLICIT NONE 
  INTEGER :: iiflen, ioflen, ivlen
  INTEGER :: nb_levels !Number of vertical levels to merge (for LFI splitted files)
  CHARACTER(LEN=iiflen) :: hinfile
  CHARACTER(LEN=ioflen) :: houtfile
  CHARACTER(LEN=ivlen)  :: hvarlist
  LOGICAL :: ooutname, ocdf2cdf, olfi2cdf, olfilist, ohdf5, omerge, oreduceprecision, osplit, ocompress
  INTEGER :: compress_level

  INTEGER :: ibuflen
  INTEGER :: ji
  INTEGER :: nbvar_infile ! number of variables available in the input file
  INTEGER :: nbvar_tbr  ! number of variables to be read
  INTEGER :: nbvar_calc ! number of variables to be computed from others
  INTEGER :: nbvar_tbw  ! number of variables to be written
  INTEGER :: nbvar      ! number of defined variables
  INTEGER :: first_level, current_level, last_level
  TYPE(filelist_struct) :: infiles, outfiles
  TYPE(workfield), DIMENSION(:), POINTER :: tzreclist


  !Remove level in the filename if merging LFI splitted files
  if (.NOT.ooutname) then
    if (omerge .AND. .NOT.osplit) then
       houtfile=houtfile(1:len(houtfile)-9)//houtfile(len(houtfile)-3:)
    end if
    if (.NOT.omerge .AND. osplit) then
       if (ohdf5) then
         ji=4
       else
         ji=3
       end if
       houtfile=houtfile(1:len(houtfile)-ji)
    end if
    if (omerge .AND. osplit) then
       if (ohdf5) then
         ji=9
       else
         ji=8
       end if
       houtfile=houtfile(1:len(houtfile)-ji)
    end if
  end if

  CALL OPEN_FILES(infiles, outfiles, hinfile, houtfile, ocdf2cdf, olfi2cdf, olfilist, ohdf5, nbvar_infile, osplit)
  IF (olfilist) return

  IF (olfi2cdf .OR. ocdf2cdf) THEN
     IF (ivlen > 0) THEN
        ! nbvar_tbr is computed from number of requested variables
        ! by counting commas, = and +
        nbvar_tbr  = 0
        nbvar_calc = 0
        nbvar_tbw = 0
        DO ji=1,ivlen
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

  IF (olfi2cdf) THEN
     ! Conversion LFI -> NetCDF
     
     !Standard treatment (one LFI file only)
     IF (.not.omerge) THEN
       CALL parse_infiles(infiles,hvarlist,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen)
       IF (osplit) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,ohdf5)
       CALL def_ncdf(outfiles,tzreclist,nbvar,oreduceprecision,omerge,osplit,ocompress,compress_level)
       CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,osplit)

     ELSE
     !Treat several LFI files and merge into 1 NC file

       !Determine first level (eg needed to find suffix of the variable name)
       read( hinfile(len(hinfile)-6:len(hinfile)-4) , "(I3)" ) first_level
       current_level = first_level
       last_level    = first_level + nb_levels - 1

       !Read 1st LFI file
       CALL parse_infiles(infiles,hvarlist,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,current_level)
       IF (osplit) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,ohdf5)
       !Define NC variables
       CALL def_ncdf(outfiles,tzreclist,nbvar,oreduceprecision,omerge,osplit,ocompress,compress_level)

       DO current_level = first_level,last_level
         print *,'Treating level ',current_level
         IF (current_level/=first_level) THEN
           CALL open_split_lfifile_in(infiles,hinfile,current_level)
           CALL read_data_lfi(infiles,hvarlist,nbvar,tzreclist,ibuflen,current_level)
         END IF
         CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,osplit,current_level)
         IF (current_level/=last_level) CALL close_files(infiles)
       END DO
     END IF

  ELSE IF (ocdf2cdf) THEN
     ! Conversion netCDF -> netCDF

     !Standard treatment (one netCDF file only)
     IF (.not.omerge) THEN
       CALL parse_infiles(infiles,hvarlist,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,current_level)
       IF (osplit) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,ohdf5)
       CALL def_ncdf(outfiles,tzreclist,nbvar,oreduceprecision,omerge,osplit,ocompress,compress_level)
       CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,osplit)

     ELSE
     !Treat several NC files and merge into 1 NC file

       !Determine first level (eg needed to find suffix of the variable name)
       read( hinfile(len(hinfile)-5:len(hinfile)-3) , "(I3)" ) first_level
       current_level = first_level
       last_level    = first_level + nb_levels - 1

       !Read 1st NC file
       CALL parse_infiles(infiles,hvarlist,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,current_level)
       IF (osplit) call open_split_ncfiles_out(outfiles,houtfile,nbvar,tzreclist,ohdf5)
       !Define NC variables
       CALL def_ncdf(outfiles,tzreclist,nbvar,oreduceprecision,omerge,osplit,ocompress,compress_level)

       DO current_level = first_level,last_level
         print *,'Treating level ',current_level
         IF (current_level/=first_level) THEN
           CALL open_split_ncfile_in(infiles,hinfile,current_level)
           CALL update_varid_in(infiles,hinfile,tzreclist,nbvar,current_level)
         END IF
         CALL fill_ncdf(infiles,outfiles,tzreclist,nbvar,ibuflen,osplit,current_level)
         IF (current_level/=last_level) CALL close_files(infiles)
       END DO
     END IF

  ELSE
     ! Conversion NetCDF -> LFI
     CALL parse_infiles(infiles,hvarlist,nbvar_infile,nbvar_tbr,nbvar_calc,nbvar_tbw,tzreclist,ibuflen,current_level)
     CALL build_lfi(infiles,outfiles,tzreclist,ibuflen)
  END IF
  
  CALL CLOSE_FILES(infiles)
  CALL CLOSE_FILES(outfiles)
  
end subroutine LFI2CDFMAIN

