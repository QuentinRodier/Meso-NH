!MNH_LIC Copyright 2005-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_ZSMT_PGD
!     ######################
!
INTERFACE 
!
      SUBROUTINE ZSMT_PGD(TPFILE,KZSFILTER,KSLEVE,KLOCZSFILTER,OHSLOP,PHSLOP,PSMOOTH_ZS)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),     INTENT(IN)  :: TPFILE       ! File characteristics
INTEGER,             INTENT(IN)  :: KZSFILTER    ! number of iterations for fine orography
INTEGER,             INTENT(IN)  :: KSLEVE       ! number of iterations
INTEGER,             INTENT(IN)  :: KLOCZSFILTER ! number of iteration for filter of local fine orography
LOGICAL,             INTENT(IN)  :: OHSLOP       ! filtering of local slopes higher than XHSLOP   
REAL,                INTENT(IN)  :: PHSLOP       ! slopes where the local fine filtering is applied
REAL,                INTENT(IN)  :: PSMOOTH_ZS   ! optional uniform smooth orography for SLEVE coordinate
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
      SUBROUTINE ZSMT_PGD(TPFILE,KZSFILTER,KSLEVE,KLOCZSFILTER,OHSLOP,PHSLOP,PSMOOTH_ZS)
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
!!      Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!      Q. Rodier 01/2019 : add a new filtering for very high slopes (applied locally)
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
use modd_field,          only: tfieldmetadata, TYPEREAL
USE MODD_IO,         ONLY : TFILEDATA
USE MODD_PARAMETERS, ONLY : JPHEXT, XUNDEF
!
USE MODI_MNHGET_SURF_PARAM_n
USE MODE_IO_FIELD_READ,  only: IO_Field_read
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
USE MODE_ll        , ONLY : GET_DIM_EXT_ll , ADD2DFIELD_ll , CLEANLIST_ll , UPDATE_HALO_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll 
USE MODE_SUM_ll
use mode_tools_ll,        only: GET_INDICE_ll
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
TYPE(TFILEDATA),     INTENT(IN)  :: TPFILE       ! File characteristics
INTEGER,             INTENT(IN)  :: KZSFILTER    ! number of iterations for fine orography
INTEGER,             INTENT(IN)  :: KSLEVE       ! number of iterations
INTEGER,             INTENT(IN)  :: KLOCZSFILTER ! number of iteration for filter of local fine orography
LOGICAL,             INTENT(IN)  :: OHSLOP       ! filtering of local slopes higher than XHSLOP   
REAL,                INTENT(IN)  :: PHSLOP       ! slopes where the local fine filtering is applied
REAL,                INTENT(IN)  :: PSMOOTH_ZS   ! optional uniform smooth orography for SLEVE coordinate
!
!
!*       0.2   declarations of local variables
!
!
INTEGER :: JN         ! loop counter on iterations
INTEGER :: JI         ! loop counter on X coordinate
INTEGER :: JJ         ! loop counter on Y coordinate
!
INTEGER :: IIU        ! number of points in X direction
INTEGER :: IJU        ! number of points in Y direction
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSMOOTH_ZS ! smooth orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFINE_ZS   ! smoothed fine orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLEVE_ZS  ! smooth orography for sleve coordinate
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS        ! orography at previous iteration
REAL, DIMENSION(:,:), ALLOCATABLE :: ZMASK      ! sea mask
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLOPEX      ! terrain slope along x (flux pt)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLOPEY      ! terrain slope along y (flux pt)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLOPEMX     ! terrain slope along x (max. of flux, at mass pt)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSLOPEMY     ! terrain slope along y (max. of flux, at mass pt)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOFIL       ! coefficient for filtering high slopes
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSMOOTH_ZSINI ! initial orography before slope filtering
!
INTEGER                           :: JIM, JIP, JJM, JJP

TYPE(LIST_ll)     , POINTER      :: THALO_ll => NULL()     ! halo
INTEGER                          :: INFO_ll                ! error return code
INTEGER :: IIB,IIE,IJB,IJE
REAL, DIMENSION(:), ALLOCATABLE  :: ZXHAT
REAL, DIMENSION(:), ALLOCATABLE  :: ZYHAT
TYPE(TFIELDMETADATA) :: TZFIELD
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
ALLOCATE(ZSLOPEX(IIU,IJU))
ALLOCATE(ZSLOPEY(IIU,IJU))
ALLOCATE(ZSLOPEMX(IIU,IJU))
ALLOCATE(ZSLOPEMY(IIU,IJU))
ALLOCATE(ZCOFIL(IIU,IJU))
ALLOCATE(ZSMOOTH_ZSINI(IIU,IJU))
ALLOCATE(ZXHAT(IIU))
ALLOCATE(ZYHAT(IJU))
!
CALL IO_Field_read(TPFILE,'XHAT',ZXHAT)
CALL IO_Field_read(TPFILE,'YHAT',ZYHAT)

CALL IO_Field_read(TPFILE,'ZS',ZZS)
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
CALL ADD2DFIELD_ll( THALO_ll, ZMASK, 'ZSMT_PGD::ZMASK' )
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
CALL ADD2DFIELD_ll( THALO_ll, ZZS,        'ZSMT_PGD::ZZS' )
CALL ADD2DFIELD_ll( THALO_ll, ZSMOOTH_ZS, 'ZSMT_PGD::ZSMOOTH_ZS' )
CALL ADD2DFIELD_ll( THALO_ll, ZSLOPEX,    'ZSMT_PGD::ZSLOPEX' )
CALL ADD2DFIELD_ll( THALO_ll, ZSLOPEY,    'ZSMT_PGD::ZSLOPEY' )
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
!*       4.    Filtering very high slopes for steep orography
!              -------------------------------------------
!
IF(OHSLOP) THEN
 ZSMOOTH_ZSINI=ZFINE_ZS
 ZZS=ZFINE_ZS
 ZSLOPEMX=0.
 ZSLOPEMY=0.
 ZSLOPEX=0.
 ZSLOPEY=0.
 ! 
 DO JN=1, KLOCZSFILTER
  ZCOFIL=0.
  ZSLOPEX=0.
  ZSLOPEY=0.
  ZSLOPEMX=0.
  ZSLOPEMY=0.
  !
  !Slope calculation along Y at flux and mass point
  DO JI=1,IIU
   DO JJ=2,IJU-1
    ZSLOPEY(JI,JJ) = (ZZS(JI,JJ)-ZZS(JI,JJ-1))/(0.5*(ZYHAT(JJ+1)-ZYHAT(JJ-1)))
   END DO
  END DO
  !
  CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)
  !
  DO JI=1,IIU
   DO JJ=1,IJU-1
    ZSLOPEMY(JI,JJ) = MAX(ABS(ZSLOPEY(JI,JJ)),ABS(ZSLOPEY(JI,JJ+1)))
   END DO
  END DO
  !
  !Slope calculation along X at flux and mass point
  DO JJ=1,IJU
   DO JI=2,IIU-1
    ZSLOPEX(JI,JJ) = (ZZS(JI,JJ)-ZZS(JI-1,JJ))/(0.5*(ZXHAT(JI+1)-ZXHAT(JI-1)))
   END DO
  END DO
  !
  CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)
  !
  DO JJ=1,IJU
   DO JI=1,IIU-1
    ZSLOPEMX(JI,JJ) = MAX(ABS(ZSLOPEX(JI+1,JJ)),ABS(ZSLOPEX(JI,JJ)))
   END DO
  END DO
  !
  !Filtering coefficient
  DO JI=1,IIU
   DO JJ=1,IJU
    IF(ZSLOPEMX(JI,JJ)>=PHSLOP .OR. ZSLOPEMY(JI,JJ)>=PHSLOP) THEN
     ZCOFIL(JI,JJ) = 0.25 !Fixed coefficient
    END IF
   END DO
  END DO
  !
  !Filtering
  DO JJ = 1,IJU
       DO JI = 1,IIU
         JIP = MIN(JI+1,IIU)
         JIM = MAX(JI-1,1  )
         JJP = MIN(JJ+1,IJU)
         JJM = MAX(JJ-1,1  )
         ZSMOOTH_ZS(JI,JJ) =  ZZS(JI,JJ)                &
            + ZCOFIL(JI,JJ)* ZMASK(JI,JJ)               &
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
  CALL UPDATE_HALO_ll(THALO_ll,INFO_ll)
  ZZS=ZSMOOTH_ZS
 END DO
 !
 ZFINE_ZS=ZSMOOTH_ZS
 !
 !Filtered slopes computed again for output
 DO JI=1,IIU
   DO JJ=2,IJU-1
     ZSLOPEY(JI,JJ) = (ZZS(JI,JJ)-ZZS(JI,JJ-1))/(0.5*(ZYHAT(JJ+1)-ZYHAT(JJ-1)))
   END DO
 END DO
 !
 DO JJ=1,IJU
   DO JI=2,IIU-1
     ZSLOPEX(JI,JJ) = (ZZS(JI,JJ)-ZZS(JI-1,JJ))/(0.5*(ZXHAT(JI+1)-ZXHAT(JI-1)))
   END DO
 END DO
 !
  ! Writes filtred orography and slopes along i and j
  TZFIELD = TFIELDMETADATA(                 &
    CMNHNAME   = 'ZSLOPEX',                 &
    CSTDNAME   = '',                        &
    CLONGNAME  = 'ZSLOPEX',                 &
    CUNITS     = '',                        &
    CDIR       = 'XY',                      &
    CCOMMENT   = 'orography slope along x', &
    NGRID      = 4,                         &
    NTYPE      = TYPEREAL,                  &
    NDIMS      = 2,                         &
    LTIMEDEP   = .FALSE.                    )
 CALL IO_Field_write(TPFILE,TZFIELD,ZSLOPEX)
 !
  TZFIELD = TFIELDMETADATA(                 &
    CMNHNAME   = 'ZSLOPEY',                 &
    CSTDNAME   = '',                        &
    CLONGNAME  = 'ZSLOPEY',                 &
    CUNITS     = '',                        &
    CDIR       = 'XY',                      &
    CCOMMENT   = 'orography slope along y', &
    NGRID      = 4,                         &
    NTYPE      = TYPEREAL,                  &
    NDIMS      = 2,                         &
    LTIMEDEP   = .FALSE.                    )
 CALL IO_Field_write(TPFILE,TZFIELD,ZSLOPEY)
 !
  TZFIELD = TFIELDMETADATA(           &
    CMNHNAME   = 'ZS_FILTR',          &
    CSTDNAME   = '',                  &
    CLONGNAME  = 'ZS_FILTR',          &
    CUNITS     = 'm',                 &
    CDIR       = 'XY',                &
    CCOMMENT   = 'filtred orography', &
    NGRID      = 4,                   &
    NTYPE      = TYPEREAL,            &
    NDIMS      = 2,                   &
    LTIMEDEP   = .FALSE.              )
 CALL IO_Field_write(TPFILE,TZFIELD,ZSMOOTH_ZSINI-ZFINE_ZS)
END IF
!-------------------------------------------------------------------------------
!
!*       5.    writes smoothed orographies in the file
!              ---------------------------------------
!
!
CALL IO_Field_write(TPFILE,'ZS',  ZFINE_ZS)
CALL IO_Field_write(TPFILE,'ZSMT',ZSLEVE_ZS)
!
DEALLOCATE(ZZS)
DEALLOCATE(ZFINE_ZS)
DEALLOCATE(ZSMOOTH_ZS)
DEALLOCATE(ZSLEVE_ZS)
DEALLOCATE(ZMASK)
DEALLOCATE(ZSLOPEX)
DEALLOCATE(ZSLOPEY)
DEALLOCATE(ZSLOPEMX)
DEALLOCATE(ZSLOPEMY)
DEALLOCATE(ZCOFIL)
DEALLOCATE(ZSMOOTH_ZSINI)
!
CALL CLEANLIST_ll(THALO_ll)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZSMT_PGD
