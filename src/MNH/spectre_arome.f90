!MNH_LIC Copyright 2011-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN
!-----------------------------------------------------------------
!     ####################
      MODULE MODI_SPECTRE_AROME
!     ####################
!
INTERFACE
!
SUBROUTINE SPECTRE_AROME(HINIFILE,HOUTFILE,PDELTAX,PDELTAY,KI,KJ,KK)
CHARACTER(LEN=*) , INTENT(IN)  :: HINIFILE
CHARACTER(LEN=*) , INTENT(IN) :: HOUTFILE
INTEGER,INTENT(IN) :: KI,KJ,KK
REAL,INTENT(IN) :: PDELTAX,PDELTAY

END SUBROUTINE SPECTRE_AROME
      !
END INTERFACE
!
END MODULE    

SUBROUTINE SPECTRE_AROME(HINIFILE,HOUTFILE,PDELTAX,PDELTAY,KI,KJ,KK)
!     ######################################################################
!
USE MODD_CONF
USE MODE_IO, only: IO_Pack_set
use mode_init_ll, only: SET_DAD0_ll, SET_JP_ll, SET_SPLITTING_ll
USE MODD_SPECTRE
USE MODI_COMPUTE_SPECTRE
USE MODD_PARAMETERS
USE MODD_DIM_ll
!
IMPLICIT NONE
!
CHARACTER(LEN=*) , INTENT(IN)  :: HINIFILE
CHARACTER(LEN=*) , INTENT(IN) :: HOUTFILE
INTEGER,INTENT(IN) :: KI,KJ,KK
REAL,INTENT(IN) :: PDELTAX,PDELTAY
!
CHARACTER(LEN=5) :: YFIELDSP
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZWORK         ! work array
REAL,DIMENSION(:,:,:),ALLOCATABLE:: ZWORK1         ! work array
CHARACTER (LEN=NFILENAMELGTMAX)  :: YFILE         ! file to open
REAL :: ZLATB, ZLONB
INTEGER :: ILU
INTEGER :: JJJ,III,JERR
!
!
CALL SET_SPLITTING_ll(CSPLIT)
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT, NHALO)
CALL SET_DAD0_ll()
CALL IO_Pack_set(L1D,L2D,LPACK)
ALLOCATE(ZWORK1(KI+2,KJ+2,KK+2))
!
IF (LSPECTRE_U) THEN
  YFIELDSP="U"  
  ZWORK1(:,:,:)=999
  WRITE(YFILE, FMT='(A,A,A)') TRIM(ADJUSTL(HINIFILE)),"_",TRIM(ADJUSTL(YFIELDSP))
  OPEN(NEWUNIT=ILU,FILE=YFILE,ACCESS='SEQUENTIAL', IOSTAT=JERR)
  DO JJJ=1+JPHEXT,SIZE(ZWORK1,2)-JPHEXT
    DO III=1+JPHEXT,SIZE(ZWORK1,1)-JPHEXT
       READ(UNIT=ILU,IOSTAT=JERR, FMT=*) ZLATB, ZLONB, ZWORK1(III,JJJ,1+JPVEXT:SIZE(ZWORK1,3)-JPVEXT)
    END DO
  END DO
  CLOSE(UNIT=ILU, IOSTAT=JERR)
!
   IF (LZOOM) THEN
     ALLOCATE(ZWORK(NITOT+2,NJTOT+2,SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(NXDEB-JPHEXT+1:NXDEB+NITOT+2*JPHEXT-1,NYDEB-JPHEXT+1:NYDEB+NJTOT+2*JPHEXT-1,:)
   ELSE
     ALLOCATE(ZWORK(SIZE(ZWORK1,1),SIZE(ZWORK1,2),SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(:,:,:)
   ENDIF
 print*,"CALL COMPUTE_SPECTRE FOR ",YFIELDSP
 CALL COMPUTE_SPECTRE(PDELTAX,PDELTAY,ZWORK,YFIELDSP,HOUTFILE)
 DEALLOCATE(ZWORK)
ENDIF
!
IF (LSPECTRE_V) THEN
  YFIELDSP="V"  
  ZWORK1(:,:,:)=999.
  WRITE(YFILE, FMT='(A,A,A)') TRIM(ADJUSTL(HINIFILE)),"_",TRIM(ADJUSTL(YFIELDSP))
  OPEN(NEWUNIT=ILU,FILE=YFILE,ACCESS='SEQUENTIAL', IOSTAT=JERR)
  DO JJJ=1+JPHEXT,SIZE(ZWORK1,2)-JPHEXT
    DO III=1+JPHEXT,SIZE(ZWORK1,1)-JPHEXT
       READ(UNIT=ILU,IOSTAT=JERR, FMT=*) ZLATB, ZLONB, ZWORK1(III,JJJ,1+JPVEXT:SIZE(ZWORK1,3)-JPVEXT)
    END DO
  END DO
  CLOSE(UNIT=ILU, IOSTAT=JERR)
!
   IF (LZOOM) THEN
     ALLOCATE(ZWORK(NITOT+2,NJTOT+2,SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(NXDEB-JPHEXT+1:NXDEB+NITOT+2*JPHEXT-1,NYDEB-JPHEXT+1:NYDEB+NJTOT+2*JPHEXT-1,:)
   ELSE
     ALLOCATE(ZWORK(SIZE(ZWORK1,1),SIZE(ZWORK1,2),SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(:,:,:)
   ENDIF
 print*,"CALL COMPUTE_SPECTRE FOR ",YFIELDSP
 CALL COMPUTE_SPECTRE(PDELTAX,PDELTAY,ZWORK,YFIELDSP,HOUTFILE)
 DEALLOCATE(ZWORK)
ENDIF
!
IF (LSPECTRE_W) THEN
  YFIELDSP="W"  
  ZWORK1(:,:,:)=999.
  WRITE(YFILE, FMT='(A,A,A)') TRIM(ADJUSTL(HINIFILE)),"_",TRIM(ADJUSTL(YFIELDSP))
  OPEN(NEWUNIT=ILU,FILE=YFILE,ACCESS='SEQUENTIAL', IOSTAT=JERR)
  DO JJJ=1+JPHEXT,SIZE(ZWORK1,2)-JPHEXT
    DO III=1+JPHEXT,SIZE(ZWORK1,1)-JPHEXT
       READ(UNIT=ILU,IOSTAT=JERR, FMT=*) ZLATB, ZLONB, ZWORK1(III,JJJ,1+JPVEXT:SIZE(ZWORK1,3)-JPVEXT)
    END DO
  END DO
  CLOSE(UNIT=ILU, IOSTAT=JERR)
!
   IF (LZOOM) THEN
     ALLOCATE(ZWORK(NITOT+2,NJTOT+2,SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(NXDEB-JPHEXT+1:NXDEB+NITOT+2*JPHEXT-1,NYDEB-JPHEXT+1:NYDEB+NJTOT+2*JPHEXT-1,:)
   ELSE
     ALLOCATE(ZWORK(SIZE(ZWORK1,1),SIZE(ZWORK1,2),SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(:,:,:)
   ENDIF
 print*,"CALL COMPUTE_SPECTRE FOR ",YFIELDSP
 CALL COMPUTE_SPECTRE(PDELTAX,PDELTAY,ZWORK,YFIELDSP,HOUTFILE)
 DEALLOCATE(ZWORK)
ENDIF
!
IF (LSPECTRE_RV) THEN
  YFIELDSP="Rv"  
  ZWORK1(:,:,:)=999.
  WRITE(YFILE, FMT='(A,A,A)') TRIM(ADJUSTL(HINIFILE)),"_",TRIM(ADJUSTL(YFIELDSP))
  OPEN(NEWUNIT=ILU,FILE=YFILE,ACCESS='SEQUENTIAL', IOSTAT=JERR)
  DO JJJ=1+JPHEXT,SIZE(ZWORK1,2)-JPHEXT
    DO III=1+JPHEXT,SIZE(ZWORK1,1)-JPHEXT
       READ(UNIT=ILU,IOSTAT=JERR, FMT=*) ZLATB, ZLONB, ZWORK1(III,JJJ,1+JPVEXT:SIZE(ZWORK1,3)-JPVEXT)
    END DO
  END DO
  CLOSE(UNIT=ILU, IOSTAT=JERR)
!
   IF (LZOOM) THEN
     ALLOCATE(ZWORK(NITOT+2,NJTOT+2,SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(NXDEB-JPHEXT+1:NXDEB+NITOT+2*JPHEXT-1,NYDEB-JPHEXT+1:NYDEB+NJTOT+2*JPHEXT-1,:)
   ELSE
     ALLOCATE(ZWORK(SIZE(ZWORK1,1),SIZE(ZWORK1,2),SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(:,:,:)
   ENDIF
 print*,"CALL COMPUTE_SPECTRE FOR ",YFIELDSP
 CALL COMPUTE_SPECTRE(PDELTAX,PDELTAY,ZWORK,YFIELDSP,HOUTFILE)
 DEALLOCATE(ZWORK)
ENDIF
!
IF (LSPECTRE_TH) THEN
  YFIELDSP="Theta"  
  ZWORK1(:,:,:)=999.
  WRITE(YFILE, FMT='(A,A,A)') TRIM(ADJUSTL(HINIFILE)),"_",TRIM(ADJUSTL(YFIELDSP))
  OPEN(NEWUNIT=ILU,FILE=YFILE,ACCESS='SEQUENTIAL', IOSTAT=JERR)
  DO JJJ=1+JPHEXT,SIZE(ZWORK1,2)-JPHEXT
    DO III=1+JPHEXT,SIZE(ZWORK1,1)-JPHEXT
       READ(UNIT=ILU,IOSTAT=JERR, FMT=*) ZLATB, ZLONB, ZWORK1(III,JJJ,1+JPVEXT:SIZE(ZWORK1,3)-JPVEXT)
    END DO
  END DO
  CLOSE(UNIT=ILU, IOSTAT=JERR)
!
   IF (LZOOM) THEN
     ALLOCATE(ZWORK(NITOT+2,NJTOT+2,SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(NXDEB-JPHEXT+1:NXDEB+NITOT+2*JPHEXT-1,NYDEB-JPHEXT+1:NYDEB+NJTOT+2*JPHEXT-1,:)
   ELSE
     ALLOCATE(ZWORK(SIZE(ZWORK1,1),SIZE(ZWORK1,2),SIZE(ZWORK1,3)))
     ZWORK(:,:,:)=ZWORK1(:,:,:)
   ENDIF
 print*,"CALL COMPUTE_SPECTRE FOR ",YFIELDSP
 CALL COMPUTE_SPECTRE(PDELTAX,PDELTAY,ZWORK,YFIELDSP,HOUTFILE)
 DEALLOCATE(ZWORK)
ENDIF
!
!
END SUBROUTINE SPECTRE_AROME

