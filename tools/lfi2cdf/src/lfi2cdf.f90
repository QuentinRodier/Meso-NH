PROGRAM testinfo
  USE mode_util
#ifdef NAGf95
  USE F90_UNIX
#endif
  IMPLICIT NONE 

  CHARACTER(LEN=80) :: yfilename
  CHARACTER(LEN=50) :: yexe

  INTEGER :: ibuflen
#ifndef NAGf95
  INTEGER :: IARGC
  ! CRAY specific
  INTEGER :: arglen
  INTEGER :: iresp
  !!!!!!!!!!!!!!!!!
#endif
  INTEGER :: inarg
  INTEGER :: ilu
  INTEGER :: inaf
  INTEGER :: icdf_id
  TYPE(workfield), DIMENSION(:), POINTER :: tzreclist
  LOGICAL :: glfi2cdf

  INARG = IARGC()

#if defined(F90HP)
#define HPINCR 1
#else
#define HPINCR 0
#endif

#if defined(FUJI) || defined(NAGf95) || defined(NEC) || defined(HP) || defined(pgf) || defined(G95) || defined(GFORTRAN)
  CALL GETARG(0+HPINCR,yexe)
  IF (LEN_TRIM(yexe) == 0) THEN
    PRINT *, 'FATAL ERROR : Activer la macro -DF90HP dans le Makefile et recompiler'
    STOP
  END IF
#else
  CALL PXFGETARG(0,yexe,arglen,iresp)
#endif
!  PRINT *,yexe, ' avec ',INARG,' arguments.'
  IF (INARG == 1) THEN 
#if defined(FUJI) || defined(NAGf95) || defined(NEC) || defined(HP) || defined(pgf) || defined(G95) || defined(GFORTRAN)
     CALL GETARG(1+HPINCR,yfilename)
#else
     CALL PXFGETARG(1,yfilename,arglen,iresp)
#endif
  ELSE 
     PRINT *,'Usage : ', TRIM(yexe), ' [fichier lfi]'
     STOP
  END IF
  
  glfi2cdf = (INDEX(yexe,'lfi2cdf') /= 0)

!  CALL SAMPSTART

  CALL OPEN_FILES(glfi2cdf, yfilename, icdf_id, ilu, inaf)

  IF (glfi2cdf) THEN
     ! Conversion LFI -> NetCDF
     CALL parse_lfi(ilu,inaf,tzreclist,ibuflen)
     CALL def_ncdf(tzreclist,icdf_id)
     CALL fill_ncdf(ilu,icdf_id,tzreclist,ibuflen)

  ELSE
     ! Conversion NetCDF -> LFI
     CALL parse_cdf(icdf_id,tzreclist,ibuflen)
     CALL build_lfi(icdf_id,ilu,tzreclist,ibuflen)
  END IF
  
  CALL CLOSE_FILES(ilu,icdf_id)
  
!  CALL SAMPSTOP
 
END PROGRAM testinfo
