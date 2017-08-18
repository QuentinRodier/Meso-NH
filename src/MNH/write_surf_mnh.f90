!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     #############################################################
      SUBROUTINE WRITE_SURFX0_MNH(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEX0* - routine to write a real scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITEX0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!        06/08 P. Peyrille, V. Masson : change test for writing 
!!                                       YY, XY, DX, DY in 1D or 2D configuration
!!        03/09, G.Tanguy              : add write_surft1_mnh
!!                                       replace ZUNDEF(surfex) by XUNDEF(MNH)
!!        08/2015 M.Moge    write the COVERS as 2D fields because SURFEX cannot write/read 3D fields 
!!                          with Z-splitting using NB_PROC_IO_W / NB_PROC_IO_W
!!        J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEREAL
USE MODE_MSG
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
REAL,                   INTENT(IN)  :: PFIELD   ! the real scalar to write
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX0_MNH','writing '//TRIM(HREC))
!
IF( ( HREC=='LAT0' .OR. HREC=='LON0' .OR. HREC=='RPK' .OR. HREC=='BETA'  &
                 .OR. HREC=='LATORI'.OR. HREC=='LONORI'                  )&
   .AND. CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU'                   ) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX0_MNH',TRIM(HREC)//' not written in file by externalized surface')
    RETURN
!
ELSE
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,PFIELD,KRESP)
!
  IF (KRESP /=0) THEN
    WRITE ( YMSG, '( I5 )' ) KRESP
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFX0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
  END IF
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_MNH(HREC,KL,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX1* - routine to fill a real 1D array for the externalised surface
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITE_SURFX1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEREAL
USE MODE_MSG
USE MODE_TOOLS_ll
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY :NMASK, CMASK,                          &
                              NIU, NJU, NIB, NJB, NIE, NJE,          &
                              NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                              NIE_ALL, NJE_ALL, NMASK_ALL, NHALO
USE MODD_PARAMETERS,    ONLY: XUNDEF, JPHEXT
!
USE MODI_GET_SURF_UNDEF
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                INTENT(IN)  :: KL       ! number of points
REAL, DIMENSION(KL),    INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
!                                               ! 'H' : field with
!                                               !       horizontal spatial dim.
!                                               ! 'A' : entire field with
!                                               !       horizontal spatial dim. :
!                                               !       It is not distributed on
!                                               !       the processors
!                                               ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: J1D            ! loop counter
INTEGER           :: JILOOP,JJLOOP  ! loop indexes
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK  ! work array written in the file
REAL, DIMENSION(:),   ALLOCATABLE :: ZW1D   ! 1D work array
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
REAL              :: ZUNDEF                       ! undefined value in SURFEX
!
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX1_MNH','writing '//TRIM(HREC))
!
!*       1.    Special cases with no writing
!        -----------------------------------
!
IF(        HREC=='LAT'                                  &
      .OR. HREC=='LON'                                  &
      .OR. HREC=='MESH_SIZE'                            &
      .OR. HREC=='DX'                                   &
      .OR. HREC=='DY'                                   ) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX1_MNH',TRIM(HREC)//' with mask '// &
                   TRIM(CMASK)//' not written in file by externalized surface')
    RETURN
!
ELSE IF( (   (CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU') &
              .OR. CMASK/='FULL  ')                          &
          .AND. ( HREC=='ZS' .OR. HREC=='XX' .OR. HREC=='YY') ) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX1_MNH',TRIM(HREC)//' with mask '// &
                   TRIM(CMASK)//' not written in file by externalized surface')
    RETURN
!
END IF
!
!*       2.    Ecriture
!        --------------
!
!
IF (HDIR=='A') THEN
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  IIU = NIU+2*NHALO
  IJU = NJU+2*NHALO
  IIB = NIB
  IJB = NJB
  IIE = NIE+2*NHALO
  IJE = NJE+2*NHALO
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
ALLOCATE(ZWORK(IIU,IJU))
ZWORK(:,:) = XUNDEF
!
IF (HDIR=='H' .OR. HDIR=='A') THEN
  CALL UNPACK_1D_2D(IMASK,PFIELD,ZWORK(IIB:IIE,IJB:IJE))
  IF ( HREC=='ZS' ) THEN
    IF (LWEST_ll())  THEN
      DO JILOOP = 1,JPHEXT
        ZWORK(JILOOP,:) = ZWORK(IIB,:)
      END DO
    END IF
    IF (LEAST_ll()) THEN
      DO JILOOP = IIU-JPHEXT+1,IIU
        ZWORK(JILOOP,:)=ZWORK(IIU-JPHEXT,:)
      END DO
    END IF
    IF (LSOUTH_ll()) THEN
      DO JJLOOP = 1,JPHEXT
        ZWORK(:,JJLOOP)=ZWORK(:,IJB)
      END DO
    END IF
    IF (LNORTH_ll()) THEN
      DO JJLOOP =IJU-JPHEXT+1,IJU
        ZWORK(:,JJLOOP)=ZWORK(:,IJU-JPHEXT)
      END DO
    END IF
  END IF
END IF

 CALL GET_SURF_UNDEF(ZUNDEF)
 WHERE (ZWORK==ZUNDEF) ZWORK=XUNDEF
!
!! Add cases in 2D (IJB=IJE) and 1D (IJB=IJE and IIB=IIE) 
!! to write the correct mesh
IF (      (CSTORAGE_TYPE=='PG' .OR. CSTORAGE_TYPE=='SU')  &
    .AND. CMASK=='FULL  ' .AND. (HREC=='XX' .OR. HREC=='DX') ) THEN
  ALLOCATE(ZW1D(IIU))
  IF (IIB<IIE .AND. HREC=='XX') THEN
    ZW1D(IIB+1:IIE) = 0.5 * ZWORK(IIB:IIE-1,1+JPHEXT) + 0.5 * ZWORK(IIB+1:IIE,1+JPHEXT)
    ZW1D(IIB)       = 1.5 * ZWORK(IIB      ,1+JPHEXT) - 0.5 * ZWORK(IIB+1    ,1+JPHEXT)
    DO J1D=JPHEXT,1,-1
      ZW1D(      J1D) = 2. * ZW1D(J1D+1)   - ZW1D(J1D+2)
      ZW1D(IIU+1-J1D) = 2. * ZW1D(IIU-J1D) - ZW1D(IIU-J1D-1)
    END DO
  ELSE IF (IIB==IIE .AND. HREC=='DX') THEN
    ZW1D(IIB-1) = - 0.5 * ZWORK(IIB,1+JPHEXT)
    ZW1D(IIB)   =   0.5 * ZWORK(IIB,1+JPHEXT)
    ZW1D(IIB+1) =   1.5 * ZWORK(IIB,1+JPHEXT)
  END IF
!
  IF (HDIR=='A') THEN
    TZFIELD%CMNHNAME   = 'XHAT'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: XHAT'
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZW1D(:),KRESP)
  END IF
  IF (HDIR=='H') THEN
    TZFIELD%CMNHNAME   = 'XHAT'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: XHAT'
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = 'XX'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZW1D(1+NHALO:IIU-NHALO),KRESP)
  END IF
  DEALLOCATE(ZW1D)
ELSE IF ( (CSTORAGE_TYPE=='PG' .OR. CSTORAGE_TYPE=='SU')  &
         .AND. CMASK=='FULL  ' .AND. (HREC=='YY' .OR. HREC=='DY') ) THEN
  ALLOCATE(ZW1D(IJU))
  IF (IJB<IJE .AND. HREC=='YY') THEN
    ZW1D(IJB+1:IJE) = 0.5 * ZWORK(1+JPHEXT,IJB:IJE-1) + 0.5 * ZWORK(1+JPHEXT,IJB+1:IJE)
    ZW1D(IJB)       = 1.5 * ZWORK(1+JPHEXT,IJB      ) - 0.5 * ZWORK(1+JPHEXT,IJB+1    )
    DO J1D=JPHEXT,1,-1
      ZW1D(      J1D) = 2. * ZW1D(J1D+1)   - ZW1D(J1D+2)
      ZW1D(IJU+1-J1D) = 2. * ZW1D(IJU-J1D) - ZW1D(IJU-J1D-1)
    END DO
  ELSE IF (IJB==IJE .AND. (HREC=='DY' .OR. HREC=='YY')) THEN
    ZW1D(IJB-1) = - 0.5 * ZWORK(1+JPHEXT,IJB)
    ZW1D(IJB)   =   0.5 * ZWORK(1+JPHEXT,IJB)
    ZW1D(IJB+1) =   1.5 * ZWORK(1+JPHEXT,IJB)
  END IF
  IF (HDIR=='A') THEN
    TZFIELD%CMNHNAME   = 'YHAT'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: YHAT'
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZW1D(:),KRESP)
  END IF
  IF (HDIR=='H') THEN
    TZFIELD%CMNHNAME   = 'YHAT'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: YHAT'
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = 'YY'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 1
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZW1D(1+NHALO:IJU-NHALO),KRESP)
  END IF
  DEALLOCATE(ZW1D)
ELSE IF (HDIR=='H') THEN
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 2
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO),KRESP)
ELSE IF (HDIR=='A') THEN
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 2
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK(:,:),KRESP)
ELSE
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 1
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,PFIELD(:),KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFX1_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
DEALLOCATE(ZWORK)
DEALLOCATE(IMASK)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX1_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2COV_MNH(HREC,KL1,KL2,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX1* - routine to fill a real 1D array for the externalised surface
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITE_SURFX1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!  06/2016     (G.Delautier) phasage surfex 8
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPELOG,TYPEREAL
USE MODE_MSG
USE MODE_TOOLS_ll
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_DATA_COVER_PAR,ONLY : JPCOVER
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY :NMASK, CMASK,                          &
                              NIU, NJU, NIB, NJB, NIE, NJE,          &
                              NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                              NIE_ALL, NJE_ALL, NMASK_ALL, NHALO
USE MODD_PARAMETERS,    ONLY: XUNDEF, JPHEXT
!
USE MODI_GET_SURF_UNDEF
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),   INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                   INTENT(IN)  :: KL1,KL2  ! number of points
REAL, DIMENSION(KL1,KL2),  INTENT(IN)  :: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(JPCOVER),INTENT(IN)  :: OFLAG    ! mask for array filling
INTEGER,                   INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),        INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),          INTENT(IN)  :: HDIR     ! type of field :
   !                                               ! 'H' : field with
!                                                  !       horizontal spatial dim.
!                                                  ! 'A' : entire field with
!                                                  !       horizontal spatial dim. :
!                                                  !       It is not distributed on
!                                                  !       the processors
!                                                  ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: J1D            ! loop counter
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK  ! work array written in the file
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
REAL              :: ZUNDEF                       ! undefined value in SURFEX
CHARACTER(LEN=2)  :: YDIR
CHARACTER(LEN=LEN_HREC) :: YREC
!
!JUANZ
INTEGER           :: NCOVER,ICOVER,IKL2, JL2
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZWORK3D
!JUANZ
LOGICAL           :: GCOVER_PACKED   ! .T. if cover fields are all packed together
!
CHARACTER(LEN=5)  :: YMSG
TYPE(TFIELDDATA)  :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX2COV_MNH','writing '//TRIM(HREC))
!
!*       2.    Ecriture
!        --------------
!
IF (CMASK/='FULL') RETURN
!
IF (HDIR=='A') THEN
  YDIR='--'
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  YDIR='XY'
  IIU = NIU+2*NHALO
  IJU = NJU+2*NHALO
  IIB = NIB
  IJB = NJB
  IIE = NIE+2*NHALO
  IJE = NJE+2*NHALO
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
! we write the COVERS as 2D fields because SURFEX cannot write/read 3D fields 
! with Z-splitting using NB_PROC_IO_W / NB_PROC_IO_W, so we do not use GCOVER_PACKED 
!GCOVER_PACKED = ( NB_PROCIO_W /= 1 )
GCOVER_PACKED = .FALSE.
!
TZFIELD%CMNHNAME   = 'COVER_PACKED'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = 'SURFEX: COVER_PACKED'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = ''
TZFIELD%NGRID      = 0
TZFIELD%NTYPE      = TYPELOG
TZFIELD%NDIMS      = 0
CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,GCOVER_PACKED,KRESP)
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFX2COV_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
ALLOCATE(ZWORK(IIU,IJU))
ZWORK(:,:) = XUNDEF
NCOVER=COUNT(OFLAG)
ALLOCATE(ZWORK3D(IIU,IJU,NCOVER))
ZWORK3D = XUNDEF
!
ICOVER=0
DO IKL2=1,NCOVER
  CALL UNPACK_1D_2D(IMASK,PFIELD(:,IKL2),ZWORK3D(IIB:IIE,IJB:IJE,IKL2))
END DO
!
IF (.NOT. GCOVER_PACKED) THEN
  ICOVER=0
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CUNITS     = ''
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 2
  DO JL2=1,SIZE(OFLAG)
    WRITE(YREC,'(A5,I3.3)') 'COVER',JL2
    TZFIELD%CMNHNAME   = TRIM(YREC)
    TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(YREC)
    TZFIELD%CDIR       = YDIR
    IF (OFLAG(JL2)) THEN
      ICOVER=ICOVER+1
      CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK3D(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,ICOVER),KRESP)
    END IF
  END DO
ELSE 
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = YDIR
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK3D(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,:),KRESP)
END IF
!
DEALLOCATE(ZWORK3D)
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFX2COV_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
DEALLOCATE(ZWORK)
DEALLOCATE(IMASK)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX2COV_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_MNH(HREC,KL1,KL2,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX2* - routine to fill a real 2D array for the externalised surface
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITE_SURFX2 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!      G.TANGUY 03/2009   add replace ZUNDEF(surfex) by XUNDEF(MNH)
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEREAL
USE MODE_MSG
USE MODE_TOOLS_ll
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_DATA_COVER_PAR,ONLY : JPCOVER
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY :NMASK, CMASK,                          &
                              NIU, NJU, NIB, NJB, NIE, NJE,          &
                              NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                              NIE_ALL, NJE_ALL, NMASK_ALL, NHALO
USE MODD_PARAMETERS,    ONLY: XUNDEF
!
USE MODI_GET_SURF_UNDEF
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),   INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                   INTENT(IN)  :: KL1,KL2  ! number of points
REAL, DIMENSION(KL1,KL2),  INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,                   INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),        INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),          INTENT(IN)  :: HDIR     ! type of field :
   !                                               ! 'H' : field with
!                                                  !       horizontal spatial dim.
!                                                  ! 'A' : entire field with
!                                                  !       horizontal spatial dim. :
!                                                  !       It is not distributed on
!                                                  !       the processors
!                                                  ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK  ! work array written in the file
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZFIELD ! work array written in the file
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
REAL              :: ZUNDEF                       ! undefined value in SURFEX
!
CHARACTER(LEN=5)  :: YMSG
TYPE(TFIELDDATA)  :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFX2_MNH','writing '//TRIM(HREC))
!
IF (HDIR=='A') THEN
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  IIU = NIU+2*NHALO
  IJU = NJU+2*NHALO
  IIB = NIB
  IJB = NJB
  IIE = NIE+2*NHALO
  IJE = NJE+2*NHALO
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
CALL GET_SURF_UNDEF(ZUNDEF)
!
IF (HDIR=='H' .OR. HDIR=='A') THEN
  ALLOCATE(ZWORK(IIU,IJU,SIZE(PFIELD,2)))
  ZWORK(:,:,:) = XUNDEF
  CALL UNPACK_1D_2D(NMASK,PFIELD(:,:),ZWORK(IIB:IIE,IJB:IJE,:))
  WHERE (ZWORK==ZUNDEF) ZWORK=XUNDEF
!
  IF (HDIR=='H') THEN
    TZFIELD%CMNHNAME   = TRIM(HREC)
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,:),KRESP)
  END IF
  IF (HDIR=='A') THEN
    TZFIELD%CMNHNAME   = TRIM(HREC)
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 4
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZWORK(:,:,:),KRESP)
  END IF
!
  DEALLOCATE(ZWORK)
  DEALLOCATE(IMASK)
ELSE IF (HDIR=='-') THEN
  ALLOCATE(ZFIELD(KL1,KL2))
  ZFIELD=PFIELD
  WHERE (ZFIELD==ZUNDEF) ZFIELD=XUNDEF
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 2
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ZFIELD(:,:),KRESP)
!
  DEALLOCATE(ZFIELD)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFX2_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX2_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_MNH(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEN0* - routine to write an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITEN0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEINT
USE MODE_MSG
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY: NIU_ALL, NJU_ALL
USE MODD_PARAMETERS,    ONLY: JPHEXT
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                INTENT(IN)  :: KFIELD   ! the integer to write
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
INTEGER          :: IFIELD
TYPE(TFIELDDATA) :: TZFIELD
CHARACTER(LEN=5) :: YMSG
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFN0_MNH','writing '//TRIM(HREC))
!
IF( (HREC=='IMAX' .OR. HREC=='JMAX' .OR. HREC=='KMAX') .AND.  &
     CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU'            ) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFN0_MNH',TRIM(HREC)//' not written in file by externalized surface')
    RETURN
!
ELSE IF (HREC=='VERSION' .OR. HREC=='BUG') THEN
  !Field is in fieldlist
  CALL IO_WRITE_FIELD(TFILE_SURFEX,HREC,KFIELD,KRESP)
ELSE
  IFIELD = KFIELD
  IF (HREC=='IMAX') IFIELD = NIU_ALL-2*JPHEXT
  IF (HREC=='JMAX') IFIELD = NJU_ALL-2*JPHEXT
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,IFIELD,KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFN0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFN0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_MNH(HREC,KL,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEN0* - routine to write an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITEN0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEINT
USE MODE_MSG
!
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY: NMASK, CMASK, &
                              NIU, NJU, NIB, NJB, NIE, NJE
USE MODD_PARAMETERS,    ONLY: NUNDEF
!
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                INTENT(IN)  :: KL       ! number of points
INTEGER, DIMENSION(KL), INTENT(IN)  :: KFIELD   ! the integers to be written
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
!                                               ! 'H' : field with
!                                               !       horizontal spatial dim.
!                                               ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWORK  ! work array written in the file
!
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFN1_MNH','writing '//TRIM(HREC))
!
IF (HDIR=='-') THEN
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 1
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,KFIELD,KRESP)
!
ELSE IF (HDIR=='H') THEN
!
  ALLOCATE(IWORK(NIU,NJU))
  IWORK(:,:) = NUNDEF
!
  CALL UNPACK_1D_2D(NMASK,KFIELD,IWORK(NIB:NIE,NJB:NJE))
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 2
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,IWORK(:,:),KRESP)
!
  DEALLOCATE(IWORK)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFN1_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFN1_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_MNH(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEC0* - routine to write an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITEC0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPECHAR,TYPELOG
USE MODE_MSG
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY: NIU_ALL, NJU_ALL
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
CHARACTER(LEN=40),      INTENT(IN)  :: HFIELD   ! the string to write
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL          :: GCARTESIAN
TYPE(TFIELDDATA) :: TZFIELD
CHARACTER(LEN=5) :: YMSG
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFC0_MNH','writing '//TRIM(HREC))
!
IF ( (CSTORAGE_TYPE=='PG' .OR. CSTORAGE_TYPE=='SU')  &
     .AND. HREC=='GRID_TYPE       '                  ) THEN
  IF (HFIELD(1:10)=='CONF PROJ ') THEN
    GCARTESIAN = .FALSE.
  ELSE IF (HFIELD(1:10)=='CARTESIAN ') THEN
    GCARTESIAN = .TRUE.
  END IF
  !
  TZFIELD%CMNHNAME   = 'CARTESIAN'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: CARTESIAN'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPELOG
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,GCARTESIAN,KRESP)
  !
  IF (KRESP /=0) THEN
    WRITE ( YMSG, '( I5 )' ) KRESP
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFC0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
  END IF
  !
END IF
!
TZFIELD%CMNHNAME   = TRIM(HREC)
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
TZFIELD%NGRID      = 0
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 0
CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,HFIELD,KRESP)
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFC0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFC0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_MNH(HREC,KL,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEL1* - routine to write a logical array
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITEL1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPEINT,TYPELOG
USE MODE_MSG
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY: NMASK, CMASK, &
                              NIU, NJU, NIB, NJB, NIE, NJE
!
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
INTEGER,                INTENT(IN)  :: KL       ! number of points
LOGICAL, DIMENSION(KL), INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
!                                               ! 'H' : field with
!                                               !       horizontal spatial dim.
!                                               ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GWORK  ! work array written in the file
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWORK  ! work array written in the file
!
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFL1_MNH','writing '//TRIM(HREC))
!
IF (HDIR=='-') THEN
  IF( (CMASK /= 'FULL  ').AND. (HREC=='COVER') ) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFL1_MNH',TRIM(HREC)//' with mask '// &
                   TRIM(CMASK)//' not written in file by externalized surface')
    RETURN
  ELSE
    TZFIELD%CMNHNAME   = TRIM(HREC)
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
    TZFIELD%CUNITS     = ''
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
    TZFIELD%NGRID      = 0
    TZFIELD%NTYPE      = TYPELOG
    TZFIELD%NDIMS      = 1
    CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,OFIELD(:),KRESP)
  END IF
!
ELSE IF (HDIR=='H') THEN
!
  ALLOCATE(GWORK(NIU,NJU))
  GWORK(:,:) = .FALSE.
!
  CALL UNPACK_1D_2D(NMASK,OFIELD,GWORK(NIB:NIE,NJB:NJE))
!
  ALLOCATE(IWORK(NIU,NJU))
  IWORK = 0
  WHERE(GWORK) IWORK = 1
!
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 4
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 2
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,IWORK(:,:),KRESP)
  !
  DEALLOCATE(IWORK)
  DEALLOCATE(GWORK)
!
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFL1_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFL1_MNH
!
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_MNH(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEL1* - routine to write a logical
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_IO_WRITE_FIELD
USE MODE_FIELD,         ONLY: TFIELDDATA,TYPELOG
USE MODE_MSG
!
USE MODD_CONF_n,        ONLY: CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_IO_SURF_MNH,   ONLY: CMASK
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),INTENT(IN)  :: HREC     ! name of the article to write
LOGICAL,                INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! return-code if a problem appears
CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFL0_MNH','writing '//TRIM(HREC))
!
IF( (CMASK /= 'FULL  ').AND. (HREC=='COVER') ) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFL0_MNH',TRIM(HREC)//' with mask '// &
                 TRIM(CMASK)//' not written in file by externalized surface')
  RETURN
ELSE
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPELOG
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,OFIELD,KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFL0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFL0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_MNH(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITET0* - routine to write a MESO-NH date_time scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of WRITET0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. MASSON      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     18/08/97
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FIELD,       ONLY: TFIELDDATA, TYPEDATE
USE MODE_FM
USE MODE_FMWRIT
USE MODE_MSG
!
USE MODD_CONF_n,      ONLY : CSTORAGE_TYPE
USE MODD_IO_ll,         ONLY: TFILE_SURFEX
USE MODD_TYPE_DATE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),  INTENT(IN)  :: HREC     ! name of the article to be written
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string

!*      0.2   Declarations of local variables
!
!
CHARACTER(LEN=LEN_HREC)      :: YRECFM    ! Name of the article to be written
INTEGER, DIMENSION(3)  :: ITDATE
CHARACTER(LEN=5) :: YMSG
TYPE (DATE_TIME) :: TZDATA
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFT0_MNH','writing '//TRIM(HREC))
!
IF( HREC=='DTCUR' .AND. CSTORAGE_TYPE/='SU' ) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFT0_MNH',TRIM(HREC)//' not written in file by externalized surface')
  RETURN
ELSE
  TZDATA%TDATE = DATE(KYEAR,KMONTH,KDAY)
  TZDATA%TIME  = PTIME
  !  
  TZFIELD%CMNHNAME   = TRIM(HREC)
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(HREC)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEDATE
  TZFIELD%NDIMS      = 0
  !
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,TZDATA,KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE ( YMSG, '( I5 )' ) KRESP
  CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFT0_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
END IF
!
END SUBROUTINE WRITE_SURFT0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFT1_MNH(HREC,KL1,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date vector
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      G.TANGUY      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     03/03/09
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FIELD,        ONLY: TFIELDDATA, TYPEINT, TYPEREAL
USE MODE_FM
USE MODE_FMWRIT
USE MODE_MSG
!
USE MODD_IO_ll,        ONLY: TFILE_SURFEX
USE MODD_CONF_n,       ONLY : CSTORAGE_TYPE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=LEN_HREC),  INTENT(IN) :: HREC     ! name of the article to be written
INTEGER,            INTENT(IN) :: KL1       ! number of points
INTEGER, DIMENSION(KL1), INTENT(IN)  :: KYEAR    ! year
INTEGER, DIMENSION(KL1), INTENT(IN)  :: KMONTH   ! month
INTEGER, DIMENSION(KL1), INTENT(IN)  :: KDAY     ! day
REAL,    DIMENSION(KL1), INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string

!*      0.2   Declarations of local variables
!
!
INTEGER, DIMENSION(3,KL1)  :: ITDATE
CHARACTER(LEN=5) :: YMSG
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFT1_MNH','writing '//TRIM(HREC))
!
IF( HREC=='DTCUR' .AND. CSTORAGE_TYPE/='SU' ) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','WRITE_SURFT1_MNH',TRIM(HREC)//' not written in file by externalized surface')
  RETURN
ELSE
  !
  ITDATE(1,:) = KYEAR  (:)
  ITDATE(2,:) = KMONTH (:)
  ITDATE(3,:) = KDAY   (:)
  !
  TZFIELD%CMNHNAME   = TRIM(HREC)//'%TDATE'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(TZFIELD%CMNHNAME)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 2
  !
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,ITDATE(:,:),KRESP)
  !
  IF (KRESP /=0) THEN
    WRITE ( YMSG, '( I5 )' ) KRESP
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFT1_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
  END IF
  !
  TZFIELD%CMNHNAME   = TRIM(HREC)//'%TIME'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'SURFEX: '//TRIM(TZFIELD%CMNHNAME)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(HCOMMENT)
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 1
  !
  CALL IO_WRITE_FIELD(TFILE_SURFEX,TZFIELD,PTIME(:),KRESP)
!
  IF (KRESP /=0) THEN
    WRITE ( YMSG, '( I5 )' ) KRESP
    CALL PRINT_MSG(NVERB_ERROR,'IO','WRITE_SURFT1_MNH','error when writing article '//TRIM(HREC)//' KRESP='//YMSG)
  END IF
!
END IF
!
END SUBROUTINE WRITE_SURFT1_MNH
