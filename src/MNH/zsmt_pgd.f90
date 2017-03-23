!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######################
      MODULE MODI_ZSMT_PGD
!     ######################
!
INTERFACE 
!
      SUBROUTINE ZSMT_PGD(TPFILE,KZSFILTER,KSLEVE,PSMOOTH_ZS)
!
USE MODD_IO_ll,      ONLY : TFILEDATA
!
TYPE(TFILEDATA),     INTENT(IN)  :: TPFILE     ! File characteristics
INTEGER,             INTENT(IN)  :: KZSFILTER  ! number of iterations for fine orography
INTEGER,             INTENT(IN)  :: KSLEVE     ! number of iterations
REAL,                INTENT(IN)  :: PSMOOTH_ZS ! optional uniform smooth orography for SLEVE coordinate
!
END SUBROUTINE ZSMT_PGD
!
END INTERFACE
!
END MODULE MODI_ZSMT_PGD
!
!
!
!     #############################
      SUBROUTINE ZSMT_PGD(TPFILE,KZSFILTER,KSLEVE,PSMOOTH_ZS)
!     #############################
!
!!****  *ZSMT_PGD* computes smoothed orography for SLEVE coordinate
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	G. Zangler      * LA *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        nov 2005
!!      J.Escobar  23/06/2015 : correction for JPHEXT<>1
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_IO_ll,      ONLY : TFILEDATA
USE MODD_LUNIT,      ONLY : CLUOUT0
USE MODD_PARAMETERS, ONLY : JPHEXT, XUNDEF
!
USE MODI_MNHGET_SURF_PARAM_n
USE MODE_FMREAD
USE MODE_FMWRIT
USE MODE_ll        , ONLY : GET_DIM_EXT_ll , ADD2DFIELD_ll , CLEANLIST_ll , UPDATE_HALO_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll 
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
TYPE(TFILEDATA),     INTENT(IN)  :: TPFILE     ! File characteristics
INTEGER,             INTENT(IN)  :: KZSFILTER  ! number of iterations for fine orography
INTEGER,             INTENT(IN)  :: KSLEVE     ! number of iterations
REAL,                INTENT(IN)  :: PSMOOTH_ZS ! optional uniform smooth orography for SLEVE coordinate
!
!
!*       0.2   declarations of local variables
!
!
INTEGER :: JN         ! loop counter on iterations
INTEGER :: JI         ! loop counter on X coordinate
INTEGER :: JJ         ! loop counter on Y coordinate
!
INTEGER :: IIMAX      ! number of physical points in X direction
INTEGER :: IJMAX      ! number of physical points in Y direction
INTEGER :: IIU        ! number of points in X direction
INTEGER :: IJU        ! number of points in Y direction
!
INTEGER           :: IRESP    ! return code for I/O
CHARACTER(LEN=16) :: YRECFM   ! name of record
INTEGER           :: IGRID    ! grid location
INTEGER           :: ILENCH   ! length of comment string
CHARACTER(LEN=100):: YCOMMENT ! comment string

REAL, DIMENSION(:,:), ALLOCATABLE :: ZSMOOTH_ZS ! smooth orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFINE_ZS   ! smoothed fine orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLEVE_ZS  ! smooth orography for sleve coordinate
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS        ! orography at previous iteration
REAL, DIMENSION(:,:), ALLOCATABLE :: ZMASK      ! sea mask
INTEGER                           :: JIM, JIP, JJM, JJP

TYPE(LIST_ll)     , POINTER      :: THALO_ll => NULL()     ! halo
INTEGER                          :: INFO_ll                ! error return code
INTEGER :: IIB,IIE,IJB,IJE
!-------------------------------------------------------------------------------
!
!*       1.    Read orography in the file
!              --------------------------
!
!*       1.1 dimensions
!            ----------
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
!
!*       1.2 orography
!            ---------
!
ALLOCATE(ZZS(IIU,IJU))
ALLOCATE(ZSMOOTH_ZS(IIU,IJU))
ALLOCATE(ZFINE_ZS(IIU,IJU))
ALLOCATE(ZSLEVE_ZS(IIU,IJU))
ALLOCATE(ZMASK(IIU,IJU))
!
YRECFM = 'ZS              '
CALL FMREAD(TPFILE%CNAME,YRECFM,CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
DO JI=1,JPHEXT
ZZS(JI,:) = ZZS(IIB,:)
ZZS(IIE+JI,:) = ZZS(IIE,:)
ZZS(:,JI ) = ZZS(:,IJB)
ZZS(:,IJE+JI) = ZZS(:,IJE)
ENDDO
!
ZFINE_ZS = ZZS
ZSLEVE_ZS= ZZS
!
CALL MNHGET_SURF_PARAM_n(PSEA=ZMASK)
!
DO JI=1,JPHEXT
ZMASK(JI  ,:) = ZMASK(IIB,:)
ZMASK(IIE+JI,:) = ZMASK(IIE,:)
ZMASK(:,JI  ) = ZMASK(:,IJB)
ZMASK(:,IJE+JI) = ZMASK(:,IJE)
ENDDO
!
ZMASK= 1.-ZMASK
CALL ADD2DFIELD_ll(THALO_ll,ZMASK)
CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)
CALL CLEANLIST_ll(THALO_ll)
!
!-------------------------------------------------------------------------------
!
!*       3.    Computes smoothed orography
!              ---------------------------
!
!
ZSMOOTH_ZS = ZZS
!
CALL ADD2DFIELD_ll(THALO_ll,ZZS)
CALL ADD2DFIELD_ll(THALO_ll,ZSMOOTH_ZS)
!
CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)

  DO JN = 1,MAX(KSLEVE,KZSFILTER)
  !
   DO JJ = IJB-1,IJE+1
      DO JI = IIB-1,IIE+1
        JIP = MIN(JI+1,IIE+1)
        JIM = MAX(JI-1,IIB-1  )
        JJP = MIN(JJ+1,IJE+1)
        JJM = MAX(JJ-1,IJB-1  )
        ZSMOOTH_ZS(JI,JJ) =  ZZS(JI,JJ)                &
           + 0.125* ZMASK(JI,JJ)                       &
            * (     ZMASK(JIM,JJ)   * ZZS(JIM,JJ)      &
                +   ZMASK(JIP,JJ)   * ZZS(JIP,JJ)      &
                +   ZMASK(JI,JJM)   * ZZS(JI,JJM)      &
                +   ZMASK(JI,JJP)   * ZZS(JI,JJP)      &
                - ( ZMASK(JIM,JJ)                      &
                   +ZMASK(JIP,JJ)                      &
                   +ZMASK(JI,JJM)                      &
                   +ZMASK(JI,JJP) ) * ZZS(JI,JJ)       )  
      ENDDO
    ENDDO
    !
    ZZS(:,:) = ZSMOOTH_ZS(:,:)
    CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)
    IF (JN==KZSFILTER) ZFINE_ZS = ZSMOOTH_ZS
    IF (JN==KSLEVE)    ZSLEVE_ZS= ZSMOOTH_ZS
  ENDDO
!
CALL CLEANLIST_ll(THALO_ll)
!-------------------------------------------------------------------------------
!
!*       3.    Case of uniform smooth orography prescribed
!              -------------------------------------------
!
IF (PSMOOTH_ZS /= XUNDEF) THEN
!
  ZSLEVE_ZS = PSMOOTH_ZS
!
END IF
!-------------------------------------------------------------------------------
!
!*       4.    writes smoothed orographies in the file
!              ---------------------------------------
!
!
CALL IO_WRITE_FIELD(TPFILE,'ZS',  CLUOUT0,IRESP,ZFINE_ZS)
CALL IO_WRITE_FIELD(TPFILE,'ZSMT',CLUOUT0,IRESP,ZSLEVE_ZS)
!
DEALLOCATE(ZZS)
DEALLOCATE(ZFINE_ZS)
DEALLOCATE(ZSMOOTH_ZS)
DEALLOCATE(ZSLEVE_ZS)
DEALLOCATE(ZMASK)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZSMT_PGD
