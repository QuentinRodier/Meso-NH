subroutine  LFI2CDFMAIN(hinfile,iiflen,houtfile,ioflen,hvarlist,ivlen,olfi2cdf,olfilist,ohdf5)
  USE mode_util
  IMPLICIT NONE 
  INTEGER :: iiflen, ioflen, ivlen
  CHARACTER(LEN=iiflen) :: hinfile
  CHARACTER(LEN=ioflen) :: houtfile
  CHARACTER(LEN=ivlen)  :: hvarlist
  LOGICAL :: olfi2cdf, olfilist, ohdf5
  
  INTEGER :: ibuflen
  INTEGER :: ilu
  INTEGER :: inaf, ji
  INTEGER :: icdf_id
  TYPE(workfield), DIMENSION(:), POINTER :: tzreclist

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
     
     CALL parse_lfi(ilu,hvarlist,inaf,tzreclist,ibuflen)
     CALL def_ncdf(tzreclist,inaf,icdf_id)
     CALL fill_ncdf(ilu,icdf_id,tzreclist,inaf,ibuflen)

  ELSE
     ! Conversion NetCDF -> LFI
     CALL parse_cdf(icdf_id,tzreclist,ibuflen)
     CALL build_lfi(icdf_id,ilu,tzreclist,ibuflen)
  END IF
  
  CALL CLOSE_FILES(ilu,icdf_id)
  
end subroutine LFI2CDFMAIN

