!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/06/19 11:51:08
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_RESET_EXSEG
!     #######################
!
INTERFACE
!
      SUBROUTINE RESET_EXSEG(HLUOUT)
!
CHARACTER (LEN=*),  INTENT(IN) :: HLUOUT ! Name  for output listing
!
END SUBROUTINE RESET_EXSEG
!
END INTERFACE
!
END MODULE MODI_RESET_EXSEG
!
!     ##############################
      SUBROUTINE RESET_EXSEG(HLUOUT)
!     ##############################
!
!!****  *RESET_EXSEG* - routine used to mofify the EXSEG1.nam informations
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to modify the informations read in
!     the DESFM file and corrected according to the EXSEG1.nam file.
!     For the DIAG program, we use the informations read in the DIAG1.nam
!     file to correct or reset these informations before the allocations
!     which are performed in ini_modeln
!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      J. Stein       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    14/09/00
!!      Modifications  04/06/02  (P Jabouille) reset radiation and convective options
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODE_POS
USE MODE_IO_ll
USE MODE_FMREAD
!
USE MODD_DIAG_FLAG
USE MODD_LUNIT_n, ONLY: CINIFILE
USE MODD_CH_MNHC_n, ONLY: LUSECHEM
USE MODD_CONF_n, ONLY: LUSERV
USE MODD_GET_n
USE MODD_PARAM_n, ONLY: CDCONV, CRAD
USE MODN_PARAM_KAFR_n
USE MODN_PARAM_RAD_n
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*),  INTENT(IN) :: HLUOUT ! Name for output listing
!
!*       0.2   declarations of local variables
!
CHARACTER (LEN=9) :: YNAM      ! name of the namelist file
INTEGER :: IRESP,ILUNAM        ! return code of FMLOOK and logical unit number
LOGICAL :: GFOUND              ! Return code when searching namelist
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
!
!-------------------------------------------------------------------------------
!
!*       1.    OPENING NAMELIST FILE
!              ---------------------
!
YNAM  = 'DIAG1.nam'
CALL OPEN_ll (UNIT=ILUNAM,FILE=YNAM,IOSTAT=IRESP,STATUS="OLD",ACTION='READ', &
     FORM="FORMATTED",POSITION="REWIND",MODE=GLOBAL)
!
!-------------------------------------------------------------------------------
!
!*       2.    CONVECTION INITIALIZATION CORRECTION
!              ------------------------------------
!
! if we ask to compute the convection diagnostics then the fields used in
! Meso-NH to store these diagnostics must be allocated by the ini_model1 subroutine
!
IF (NCONV_KF>=0) THEN
  CALL POSNAM(ILUNAM,'NAM_PARAM_KAFRN',GFOUND)
  IF (GFOUND) THEN
    CALL INIT_NAM_PARAM_KAFRn
    READ(UNIT=ILUNAM,NML=NAM_PARAM_KAFRN)
    PRINT*, '  namelist NAM_PARAM_KAFRN read'
  END IF
  IF (LUSERV) THEN
    LDIAGCONV=.TRUE.
  ELSE
    NCONV_KF=-1
  END IF
  CALL UPDATE_NAM_PARAM_KAFRn  !because of LDIAGCONV
  IF (CDCONV=='NONE' ) THEN
    CDCONV='KAFR'
    CGETCONV='INIT'
  END IF
END IF
!
PRINT*,'RESET_EXSEG OUTPUT: NCONV_KF=',NCONV_KF,' CDCONV=',CDCONV,' CGETCONV=',CGETCONV
!
!-------------------------------------------------------------------------------
!
!*       3.    RADIATION INITIALIZATION CORRECTION
!              -----------------------------------
!
CGETRAD='READ'
!
IF (CRAD=='NONE') THEN
  CRAD='ECMW'
  LCLEAR_SKY=.FALSE.
  CGETRAD='INIT'
END IF
!
IF(NRAD_3D>=1) THEN
  CALL POSNAM(ILUNAM,'NAM_PARAM_RADN',GFOUND)
  IF (GFOUND) THEN
    CALL INIT_NAM_PARAM_RADn
    READ(UNIT=ILUNAM,NML=NAM_PARAM_RADN)
    CALL UPDATE_NAM_PARAM_RADn
    PRINT*, '  namelist NAM_PARAM_RADN read'
  END IF
ENDIF
!

IF ( NRAD_3D>=1 ) THEN
  CRAD='ECMW'
  CGETRAD='INIT'
END IF
!
IF(LEN_TRIM(CRAD_SAT) /= 0) THEN
  CRAD='ECMW'
END IF
!
PRINT*,'RESET_EXSEG OUTPUT: NRAD_3D =',NRAD_3D,' CRAD =',CRAD,' CGETRAD =',CGETRAD
!
!-------------------------------------------------------------------------------
!
!*       4.    CHEMISTRY INITIALIZATION CORRECTION
!              -----------------------------------
!
IF (LUSECHEM .AND. .NOT.LCHEMDIAG) LUSECHEM =.FALSE. 
!
PRINT*,'RESET_EXSEG OUTPUT: LUSECHEM =',LUSECHEM,' LCHEMDIAG =',LCHEMDIAG
PRINT*,' '
!
!-------------------------------------------------------------------------------
!
CALL CLOSE_ll(YNAM)
!
END SUBROUTINE RESET_EXSEG
