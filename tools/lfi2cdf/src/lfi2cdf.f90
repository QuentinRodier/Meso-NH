subroutine  LFI2CDFMAIN(hinfile,iiflen,ooutname,houtfile,ioflen,hvarlist,ivlen,olfi2cdf,olfilist,ohdf5,omerge,nb_levels,&
                        oreduceprecision)
  USE mode_util
  IMPLICIT NONE 
  INTEGER :: iiflen, ioflen, ivlen
  INTEGER :: nb_levels !Number of vertical levels to merge (for LFI splitted files)
  CHARACTER(LEN=iiflen) :: hinfile
  CHARACTER(LEN=ioflen) :: houtfile
  CHARACTER(LEN=ivlen)  :: hvarlist
  LOGICAL :: ooutname, olfi2cdf, olfilist, ohdf5, omerge, oreduceprecision
  
  INTEGER :: ibuflen
  INTEGER :: ilu
  INTEGER :: inaf, ji
  INTEGER :: nbvar_lfi  ! number of variables available in the LFI file
  INTEGER :: icdf_id
  INTEGER :: first_level, current_level, last_level
  INTEGER(KIND=LFI_INT) :: iresp,iverb,inap
  CHARACTER(LEN=3)      :: suffix
  CHARACTER(LEN=iiflen) :: filename
  TYPE(workfield), DIMENSION(:), POINTER :: tzreclist

  !Remove level in the filename if merging LFI splitted files
  if (omerge .AND. .NOT.ooutname) then
       houtfile=houtfile(1:len(houtfile)-9)//houtfile(len(houtfile)-3:)
  end if

  CALL OPEN_FILES(hinfile, houtfile, olfi2cdf, olfilist, ohdf5, icdf_id, ilu, inaf)
  IF (olfilist) return

  IF (olfi2cdf) THEN
     ! Conversion LFI -> NetCDF
     IF (ivlen > 0) THEN
        ! inaf is computed from number of requested variables
        ! by counting commas.
        inaf = 0
        DO ji=1,ivlen
           if (hvarlist(ji:ji) == ',') THEN
              inaf = inaf+1
           END IF
        END DO
     END IF
     
     !Standard treatment (one LFI file only)
     IF (.not.omerge) THEN
       CALL parse_lfi(ilu,hvarlist,inaf,tzreclist,ibuflen)
       CALL def_ncdf(tzreclist,inaf,oreduceprecision,icdf_id,omerge)
       CALL fill_ncdf(ilu,icdf_id,tzreclist,inaf,ibuflen)
     ELSE
     !Treat several LFI files and merge into 1 NC file
       iverb = 0 !Verbosity level for LFI

       !Determine first level (eg needed to find suffix of the variable name)
       read( hinfile(len(hinfile)-6:len(hinfile)-4) , "(I3)" ) first_level
       current_level = first_level
       last_level    = first_level + nb_levels - 1

       !Read 1st LFI file
       CALL parse_lfi(ilu,hvarlist,inaf,tzreclist,ibuflen,current_level)
       CALL def_ncdf(tzreclist,inaf,oreduceprecision,icdf_id,omerge)

       DO current_level = first_level,last_level
         print *,'Treating level ',current_level
         IF (current_level/=first_level) THEN
           write(suffix,'(I3.3)') current_level
           filename=hinfile(1:len(hinfile)-7)//suffix//'.lfi'
           CALL LFIOUV(iresp,ilu,ltrue,filename,'OLD',lfalse,lfalse,iverb,inap,nbvar_lfi)
           CALL read_data_lfi(ilu,hvarlist,inaf,tzreclist,ibuflen,current_level)
         END IF
         CALL fill_ncdf(ilu,icdf_id,tzreclist,inaf,ibuflen,current_level)
         IF (current_level/=last_level) CALL LFIFER(iresp,ilu,'KEEP')
       END DO
     END IF

  ELSE
     ! Conversion NetCDF -> LFI
     CALL parse_cdf(icdf_id,tzreclist,ibuflen)
     CALL build_lfi(icdf_id,ilu,tzreclist,ibuflen)
  END IF
  
  CALL CLOSE_FILES(ilu,icdf_id)
  
end subroutine LFI2CDFMAIN

