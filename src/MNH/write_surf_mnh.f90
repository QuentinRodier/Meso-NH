!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:30
!-----------------------------------------------------------------
!     #############################################################
      SUBROUTINE WRITE_SURFX0_MNH(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READX0* - routine to read a real scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READX0 is
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
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMWRIT
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, COUTFILE, NLUOUT
USE MODD_CONF_n,               ONLY : CSTORAGE_TYPE
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12), INTENT(IN)  :: HREC     ! name of the article to be read
REAL,              INTENT(IN)  :: PFIELD   ! the real scalar to be read
INTEGER,           INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
!-------------------------------------------------------------------------------
!

IF( ( HREC=='LAT0' .OR. HREC=='LON0' .OR. HREC=='RPK' .OR. HREC=='BETA'  &
                 .OR. HREC=='LATORI'.OR. HREC=='LONORI'                  )&
   .AND. CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU'                   ) THEN

!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE

  CALL FMWRIT(COUTFILE,HREC,COUT,'--',PFIELD,0,LEN(HCOMMENT),HCOMMENT,KRESP)

  IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
 !callabortstop
    CALL ABORT
    STOP

  END IF
END IF

!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_MNH(HREC,KL,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface
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
USE MODE_FM
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_PARAMETERS,  ONLY : XUNDEF, JPHEXT
USE MODD_CONF_n,        ONLY : CSTORAGE_TYPE
!
USE MODD_IO_SURF_MNH, ONLY :COUT, COUTFILE , NLUOUT, NMASK, CMASK, &
                            NIU, NJU, NIB, NJB, NIE, NJE,          &
                            NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                            NIE_ALL, NJE_ALL, NMASK_ALL, NHALO

USE MODI_UNPACK_1D_2D
!
USE MODI_GET_SURF_UNDEF
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),   INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN)  :: KL       ! number of points
REAL, DIMENSION(KL), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),    INTENT(IN)  :: HDIR     ! type of field :
!                                            ! 'H' : field with
!                                            !       horizontal spatial dim.
!                                            ! 'A' : entire field with
!                                            !       horizontal spatial dim. :
!                                            !       It is not distributed on
!                                            !       the processors
!                                            ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: J1D            ! loop counter
INTEGER           :: I1D            ! 1D array size
INTEGER           :: JILOOP,JJLOOP  ! loop indexes

REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK  ! work array read in the file
REAL, DIMENSION(:),   ALLOCATABLE :: ZW1D   ! 1D work array
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
REAL              :: ZUNDEF         ! undefined value in SURFEX

!-------------------------------------------------------------------------------
!
!*       1.    Special cases with no writing
!        -----------------------------------
!
IF(        HREC=='LAT'                                  &
      .OR. HREC=='LON'                                  &
      .OR. HREC=='MESH_SIZE'                            &
      .OR. HREC=='DX'                                   &
      .OR. HREC=='DY'                                   ) THEN

!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC,'  with mask ', CMASK
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE IF( (   (CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU') &
              .OR. CMASK/='FULL  ')                          &
          .AND. ( HREC=='ZS' .OR. HREC=='XX' .OR. HREC=='YY') ) THEN

!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC,'  with mask ', CMASK
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
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

IGRID=4

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
  IF (HDIR=='A') &
  CALL FMWRIT(COUTFILE,'XHAT',COUT,'--', ZW1D(:),4,LEN(HCOMMENT),HCOMMENT,KRESP)
  IF (HDIR=='H') &
  CALL FMWRIT(COUTFILE,'XHAT',COUT,'XX', ZW1D(1+NHALO:IIU-NHALO),4,LEN(HCOMMENT),HCOMMENT,KRESP)
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
  IF (HDIR=='A') &
  CALL FMWRIT(COUTFILE,'YHAT',COUT,'--', ZW1D(:),4,LEN(HCOMMENT),HCOMMENT,KRESP)
  IF (HDIR=='H') &
  CALL FMWRIT(COUTFILE,'YHAT',COUT,'YY', ZW1D(1+NHALO:IJU-NHALO),4,LEN(HCOMMENT),HCOMMENT,KRESP)
  DEALLOCATE(ZW1D)
ELSE IF (HDIR=='H') THEN
  CALL FMWRIT(COUTFILE,HREC,COUT,'XY', ZWORK(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
ELSE IF (HDIR=='A') THEN
  CALL FMWRIT(COUTFILE,HREC,COUT,'--', ZWORK(:,:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
ELSE
  CALL FMWRIT(COUTFILE,HREC,COUT,'--', PFIELD(:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
END IF
!

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    stop

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
!!****  *READX1* - routine to fill a real 1D array for the externalised surface
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
USE MODE_FM
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_PARAMETERS,  ONLY : XUNDEF, JPHEXT
USE MODD_CONF_n,        ONLY : CSTORAGE_TYPE
USE MODD_CONFZ ,        ONLY : NB_PROCIO_W
!
USE MODD_IO_SURF_MNH, ONLY :COUT, COUTFILE , NLUOUT, NMASK, CMASK, &
                            NIU, NJU, NIB, NJB, NIE, NJE,          &
                            NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                            NIE_ALL, NJE_ALL, NMASK_ALL, NHALO

USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),   INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN)  :: KL1,KL2       ! number of points
REAL, DIMENSION(KL1,KL2), INTENT(IN)  :: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(KL2),   INTENT(IN)  ::OFLAG  ! mask for array filling
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),    INTENT(IN)  :: HDIR     ! type of field :
!                                            ! 'H' : field with
!                                            !       horizontal spatial dim.
!                                            ! 'A' : entire field with
!                                            !       horizontal spatial dim. :
!                                            !       It is not distributed on
!                                            !       the processors
!                                            ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: J1D            ! loop counter
INTEGER           :: I1D            ! 1D array size
INTEGER           :: JILOOP,JJLOOP  ! loop indexes

REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK  ! work array read in the file
REAL, DIMENSION(:),   ALLOCATABLE :: ZW1D   ! 1D work array
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
!
CHARACTER(LEN=16) :: YREC
CHARACTER(LEN=100):: YCOMMENT
!JUANZ
INTEGER           :: NCOVER,ICOVER,IKL2, JL2
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZWORK3D
!JUANZ
CHARACTER(LEN=2)  :: YDIR
LOGICAL           :: GCOVER_PACKED   ! .T. if cover fields are all packed together
!-------------------------------------------------------------------------------
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
GCOVER_PACKED = ( NB_PROCIO_W /= 1 )
IGRID=0
YREC='COVER_PACKED'
YCOMMENT=''
CALL FMWRIT(COUTFILE,YREC,COUT,'--',GCOVER_PACKED,IGRID,LEN(YCOMMENT),YCOMMENT,KRESP)
!
ALLOCATE(ZWORK(IIU,IJU))
ZWORK(:,:) = XUNDEF
NCOVER=COUNT(OFLAG)
ALLOCATE(ZWORK3D(IIU,IJU,NCOVER))
ZWORK3D = XUNDEF
!
ICOVER=0
DO IKL2=1,KL2
  IF (OFLAG(IKL2)) THEN
    ICOVER=ICOVER+1   
    CALL UNPACK_1D_2D(IMASK,PFIELD(:,IKL2),ZWORK3D(IIB:IIE,IJB:IJE,ICOVER))
  END IF
END DO

IGRID=4

IF (.NOT. GCOVER_PACKED) THEN
  ICOVER=0
  DO JL2=1,KL2
    WRITE(YREC,'(A5,I3.3)') 'COVER',JL2
    IF (OFLAG(JL2)) THEN
      ICOVER=ICOVER+1
      CALL FMWRIT(COUTFILE,YREC,COUT,YDIR, ZWORK3D(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,ICOVER),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
    END IF
  END DO
ELSE 
  CALL FMWRIT(COUTFILE,HREC,COUT,YDIR, ZWORK3D(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
END IF
!
DEALLOCATE(ZWORK3D)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    stop

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
!!****  *READX2* - routine to fill a real 2D array for the externalised surface
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
USE MODE_FM
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_PARAMETERS,     ONLY : XUNDEF
!
USE MODD_IO_SURF_MNH, ONLY :COUT, COUTFILE , NLUOUT, NMASK,        &
                            NIU, NJU, NIB, NJB, NIE, NJE,          &
                            NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL,    &
                            NIE_ALL, NJE_ALL, NMASK_ALL, NHALO
!
!
USE MODI_UNPACK_1D_2D
!
USE MODI_GET_SURF_UNDEF
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                  INTENT(IN)  :: KL1      ! number of points
INTEGER,                  INTENT(IN)  :: KL2      ! 2nd dimension
REAL, DIMENSION(KL1,KL2), INTENT(IN)  :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),       INTENT(IN)  :: HCOMMENT ! Comment string
CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
!                                                 ! 'H' : field with
!                                                 !       horizontal spatial dim.
!                                                 ! 'A' : entire field with
!                                                 !       horizontal spatial dim. :
!                                                 !       It is not distributed on
!                                                 !       the processors
!                                                 ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator

REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK  ! work array read in the file
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD  ! work array read in the file
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for unpacking
REAL              :: ZUNDEF         ! undefined value in SURFEX

INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
!-------------------------------------------------------------------------------
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
IGRID=4
CALL GET_SURF_UNDEF(ZUNDEF)

IF (HDIR=='H' .OR. HDIR=='A') THEN
  ALLOCATE(ZWORK(IIU,IJU,SIZE(PFIELD,2)))
  ZWORK(:,:,:) = XUNDEF
  CALL UNPACK_1D_2D(NMASK,PFIELD(:,:),ZWORK(IIB:IIE,IJB:IJE,:))
  WHERE (ZWORK==ZUNDEF) ZWORK=XUNDEF

  IF (HDIR=='H') &
  CALL FMWRIT(COUTFILE,HREC,COUT,'XY', ZWORK(1+NHALO:IIU-NHALO,1+NHALO:IJU-NHALO,:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
  IF (HDIR=='A') &
  CALL FMWRIT(COUTFILE,HREC,COUT,'--', ZWORK(:,:,:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)

  DEALLOCATE(ZWORK)
  DEALLOCATE(IMASK)
ELSE IF (HDIR=='-') THEN
  ALLOCATE(ZFIELD(KL1,KL2))
  ZFIELD=PFIELD
  WHERE (ZFIELD==ZUNDEF) ZFIELD=XUNDEF
  CALL FMWRIT(COUTFILE,HREC,COUT,'--', ZFIELD(:,:),IGRID,LEN(HCOMMENT),HCOMMENT,KRESP)
  DEALLOCATE(ZFIELD)
END IF
!
!
IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

 END IF
!
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFX2_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_MNH(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READN0 is
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
USE MODE_FM
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
!
!
USE MODD_PARAMETERS,     ONLY : XUNDEF
!
USE MODD_IO_SURF_MNH,    ONLY : COUT, COUTFILE, NLUOUT, NIU_ALL, NJU_ALL
USE MODD_CONF_n,           ONLY : CSTORAGE_TYPE
!
!
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),   INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN)  :: KFIELD   ! the integer to be read
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
INTEGER :: IFIELD
!-------------------------------------------------------------------------------
!
IF( (HREC=='IMAX' .OR. HREC=='JMAX' .OR. HREC=='KMAX') .AND.  &
     CSTORAGE_TYPE/='PG' .AND. CSTORAGE_TYPE/='SU'            ) THEN


!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE
  IFIELD = KFIELD
  IF (HREC=='IMAX') IFIELD = NIU_ALL-2
  IF (HREC=='JMAX') IFIELD = NJU_ALL-2
  CALL FMWRIT(COUTFILE,HREC,COUT,'--',IFIELD,0,LEN(HCOMMENT),HCOMMENT,KRESP)

  IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

  END IF
 END IF

!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFN0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_MNH(HREC,KL,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READN0 is
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
USE MODE_FM
USE MODE_FMWRIT
USE MODE_ll
USE MODE_IO_ll
!
!
USE MODD_PARAMETERS,  ONLY : NUNDEF
!
USE MODD_IO_SURF_MNH, ONLY :COUT, COUTFILE , NLUOUT, NMASK, CMASK, &
                            NIU, NJU, NIB, NJB, NIE, NJE
!
!
USE MODI_UNPACK_1D_2D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
INTEGER, DIMENSION(KL), INTENT(IN)  :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
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
!-------------------------------------------------------------------------------
!
IF (HDIR=='-') THEN
!
 CALL FMWRIT(COUTFILE,HREC,COUT,'--',KFIELD,0,LEN(HCOMMENT),HCOMMENT,KRESP)
!
ELSE IF (HDIR=='H') THEN

  ALLOCATE(IWORK(NIU,NJU))
  IWORK(:,:) = NUNDEF
  !
  !
  CALL UNPACK_1D_2D(NMASK,KFIELD,IWORK(NIB:NIE,NJB:NJE))

  CALL FMWRIT(COUTFILE,HREC,COUT,'XY', IWORK(:,:),4,LEN(HCOMMENT),HCOMMENT,KRESP)
  !
  DEALLOCATE(IWORK)

  IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

  END IF

END IF

!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFN1_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_MNH(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READC0 is
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
USE MODE_FM
USE MODE_FMWRIT
!
USE MODD_CONF_n,               ONLY : CSTORAGE_TYPE
USE MODD_IO_SURF_MNH,        ONLY : COUT, COUTFILE, NLUOUT
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
CHARACTER(LEN=40),  INTENT(IN)  :: HFIELD   ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL            :: GCARTESIAN
CHARACTER(LEN=100) :: YCOMMENT
!-------------------------------------------------------------------------------
!
IF ( (CSTORAGE_TYPE=='PG' .OR. CSTORAGE_TYPE=='SU')  &
     .AND. HREC=='GRID_TYPE       '                  ) THEN
  IF (HFIELD(1:10)=='CONF PROJ ') THEN
    GCARTESIAN = .FALSE.
  ELSE IF (HFIELD(1:10)=='CARTESIAN ') THEN
    GCARTESIAN = .TRUE.
  END IF
  YCOMMENT = '(-)'
  CALL FMWRIT(COUTFILE,'CARTESIAN',COUT,'--',GCARTESIAN,0,LEN(YCOMMENT),YCOMMENT,KRESP)
END IF

CALL FMWRIT(COUTFILE,HREC,COUT,'--',HFIELD,0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

 END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFC0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_MNH(HREC,KL,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READL1 is
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
USE MODE_FM
USE MODE_FMWRIT
USE MODI_UNPACK_1D_2D
!
USE MODD_IO_SURF_MNH, ONLY :COUT, COUTFILE , NLUOUT, NMASK, CMASK, &
                            NIU, NJU, NIB, NJB, NIE, NJE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
LOGICAL, DIMENSION(KL), INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
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

!-------------------------------------------------------------------------------

IF (HDIR=='-') THEN
  IF( (CMASK /= 'FULL  ').AND. (HREC=='COVER') ) THEN
!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC,'  with MASK ',CMASK
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
  !
  ELSE
    CALL FMWRIT(COUTFILE,HREC,COUT,'--',OFIELD(:),0,LEN(HCOMMENT),HCOMMENT,KRESP)

    IF (KRESP /=0) THEN
!
      WRITE(NLUOUT,*) ' '
      WRITE(NLUOUT,*) 'WARNING'
      WRITE(NLUOUT,*) '-------'
      WRITE(NLUOUT,*) ' '
      WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
      WRITE(NLUOUT,*) ' '
      CALL ABORT
      STOP

    END IF

  END IF

ELSE IF (HDIR=='H') THEN

  ALLOCATE(GWORK(NIU,NJU))
  GWORK(:,:) = .FALSE.
  !
  !
  CALL UNPACK_1D_2D(NMASK,OFIELD,GWORK(NIB:NIE,NJB:NJE))

  ALLOCATE(IWORK(NIU,NJU))
  IWORK = 0
  WHERE(GWORK) IWORK = 1
  CALL FMWRIT(COUTFILE,HREC,COUT,'XY', IWORK(:,:),4,LEN(HCOMMENT),HCOMMENT,KRESP)
  DEALLOCATE(IWORK)
  !
  DEALLOCATE(GWORK)

  IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

  END IF

END IF
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFL1_MNH
!
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_MNH(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEL1* - routine to read a logical
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
USE MODE_FM
USE MODE_FMWRIT
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, COUTFILE, NLUOUT, CMASK
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,            INTENT(IN)  :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string
!
!*      0.2   Declarations of local variables
!


IF( (CMASK /= 'FULL  ').AND. (HREC=='COVER') ) THEN
!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC,'  with MASK ',CMASK
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE
CALL FMWRIT(COUTFILE,HREC,COUT,'--',OFIELD,0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

 END IF

END IF
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_SURFL0_MNH
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_MNH(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a MESO-NH date_time scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READT0 is
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
USE MODE_FM
USE MODE_FMWRIT
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, COUTFILE, NLUOUT
USE MODD_CONF_n,               ONLY : CSTORAGE_TYPE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! Comment string

!*      0.2   Declarations of local variables
!
!
CHARACTER(LEN=16)      :: YRECFM    ! Name of the article to be written
INTEGER, DIMENSION(3)  :: ITDATE
!-------------------------------------------------------------------------------
!
!
IF( HREC=='DTCUR' .AND. CSTORAGE_TYPE/='SU' ) THEN
!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE


YRECFM=TRIM(HREC)//'%TDATE'
!
ITDATE(1)=KYEAR
ITDATE(2)=KMONTH
ITDATE(3)=KDAY
CALL FMWRIT(COUTFILE,YRECFM,COUT,'--',ITDATE,0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

 END IF
!
YRECFM=TRIM(HREC)//'%TIME'
CALL FMWRIT(COUTFILE,YRECFM,COUT,'--',PTIME,0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    CALL ABORT
    STOP

 END IF

END IF

END SUBROUTINE WRITE_SURFT0_MNH

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
USE MODE_FM
USE MODE_FMWRIT
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, COUTFILE, NLUOUT
USE MODD_CONF_n,               ONLY : CSTORAGE_TYPE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
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
CHARACTER(LEN=16)      :: YRECFM    ! Name of the article to be written
INTEGER, DIMENSION(3,KL1)  :: ITDATE
!-------------------------------------------------------------------------------
!
!
IF( HREC=='DTCUR' .AND. CSTORAGE_TYPE/='SU' ) THEN
!    WRITE(NLUOUT,*) ' MESO-NH writing'
!    WRITE(NLUOUT,*) '-------'
!    WRITE(NLUOUT,*) ' '
!    WRITE(NLUOUT,*) 'article ', HREC
!    WRITE(NLUOUT,*) 'not written in file by externalized surface'
!    WRITE(NLUOUT,*) ' '
    RETURN
!
ELSE


YRECFM=TRIM(HREC)//'%TDATE'
!
ITDATE(1,:) = KYEAR  (:)
ITDATE(2,:) = KMONTH (:)
ITDATE(3,:) = KDAY   (:)
CALL FMWRIT(COUTFILE,YRECFM,COUT,'--',ITDATE(:,:),0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    stop

 END IF
!
YRECFM=TRIM(HREC)//'%TIME'
CALL FMWRIT(COUTFILE,YRECFM,COUT,'--',PTIME(:),0,LEN(HCOMMENT),HCOMMENT,KRESP)

 IF (KRESP /=0) THEN
!
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) ' '
    WRITE(NLUOUT,*) 'error when writing article', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) ' '
    stop

 END IF

END IF

END SUBROUTINE WRITE_SURFT1_MNH
