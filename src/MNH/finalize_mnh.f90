!MNH_LIC Copyright 2021-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Wautelet 06/07/2021
! Modifications:
!  P. Wautelet 13/01/2023: fix for LCHECK
!-----------------------------------------------------------------
MODULE MODE_FINALIZE_MNH

IMPLICIT NONE

CONTAINS

SUBROUTINE FINALIZE_MNH
  USE MODD_CONF,             only: LCHECK, NMODEL
  USE MODD_IO,               only: NIO_VERB, NVERB_DEBUG
  USE MODD_LUNIT,            only: TLUOUT0
  USE MODD_LUNIT_n,          only: LUNIT_MODEL
  USE MODD_MNH_SURFEX_n,     only: SURFEX_DEALLO_LIST
#ifdef CPLOASIS
  USE MODD_SFX_OASIS,        only: LOASIS
#endif

  USE MODE_INIT_ll,          only: END_PARA_ll
  USE MODE_IO_FILE,          only: IO_File_close
  USE MODE_IO_MANAGE_STRUCT, only: IO_Filelist_print
  USE MODE_MPPDB,            only: MPPDB_BARRIER
  USE MODE_MSG,              only: MSG_STATS

#ifdef CPLOASIS
  USE MODI_SFX_OASIS_END
#endif

  IMPLICIT NONE

  INTEGER :: IRESP
  INTEGER :: JMODEL

  !Print the list of all files and some statistics on them
  IF ( NIO_VERB >= NVERB_DEBUG ) CALL IO_Filelist_print()

  !Print the number of printed messages via Print_msg
  CALL MSG_STATS()

  !Close all the opened 'output listing' files
  IF ( TLUOUT0%LOPENED ) CALL IO_File_close(TLUOUT0)
  DO JMODEL = 1, NMODEL
    IF ( ASSOCIATED( LUNIT_MODEL(JMODEL)%TLUOUT ) ) THEN
      IF ( LUNIT_MODEL(JMODEL)%TLUOUT%LOPENED ) CALL IO_File_close( LUNIT_MODEL(JMODEL)%TLUOUT)
    END IF
  END DO

  IF ( LCHECK ) CALL MPPDB_BARRIER()

  !Finalize the parallel libraries
  CALL END_PARA_ll( IRESP )

#ifdef CPLOASIS
  IF ( LOASIS ) CALL SFX_OASIS_END()
#endif

  !Free SURFEX structures if necessary
  CALL SURFEX_DEALLO_LIST()

END SUBROUTINE FINALIZE_MNH

END MODULE MODE_FINALIZE_MNH
