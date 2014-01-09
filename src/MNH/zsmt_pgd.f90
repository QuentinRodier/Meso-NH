!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######################
      MODULE MODI_ZSMT_PGD
!     ######################
!
INTERFACE 
!
      SUBROUTINE ZSMT_PGD(HFILE,KZSFILTER,KSLEVE,PSMOOTH_ZS)
!
CHARACTER(LEN=28),   INTENT(IN)  :: HFILE      ! name of the input/output file
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
      SUBROUTINE ZSMT_PGD(HFILE,KZSFILTER,KSLEVE,PSMOOTH_ZS)
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
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
CHARACTER(LEN=28),   INTENT(IN)  :: HFILE      ! name of the input/output file
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
!-------------------------------------------------------------------------------
!
!*       1.    Read orography in the file
!              --------------------------
!
!*       1.1 dimensions
!            ----------
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
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
CALL FMREAD(HFILE,YRECFM,CLUOUT0,'XY',ZZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
ZZS(1  ,:) = ZZS(2    ,:)
ZZS(IIU,:) = ZZS(IIU-1,:)
ZZS(:,1  ) = ZZS(:,2    )
ZZS(:,IJU) = ZZS(:,IJU-1)
!
ZFINE_ZS = ZZS
ZSLEVE_ZS= ZZS
!
CALL MNHGET_SURF_PARAM_n(PSEA=ZMASK)
!
ZMASK(1  ,:) = ZMASK(2    ,:)
ZMASK(IIU,:) = ZMASK(IIU-1,:)
ZMASK(:,1  ) = ZMASK(:,2    )
ZMASK(:,IJU) = ZMASK(:,IJU-1)
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
    DO JJ = 1,IJU
      DO JI = 1,IIU
        JIP = MIN(JI+1,IIU)
        JIM = MAX(JI-1,1  )
        JJP = MIN(JJ+1,IJU)
        JJM = MAX(JJ-1,1  )
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
YRECFM='ZS'
YCOMMENT='X_Y_ZS (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFILE,YRECFM,CLUOUT0,'XY',ZFINE_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='ZSMT'
YCOMMENT='X_Y_ZSMT (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(HFILE,YRECFM,CLUOUT0,'XY',ZSLEVE_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
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
