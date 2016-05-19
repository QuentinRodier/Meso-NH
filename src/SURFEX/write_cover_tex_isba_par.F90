!     #########
      SUBROUTINE WRITE_COVER_TEX_ISBA_PAR(KPATCH,KLAYER,HISBA,HPHOTO,PSOILGRID)
!     ##########################
!
!!**** *WRITE_COVER_TEX* writes the ISBA data arrays into a tex file
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/01/98
!!
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      B. Decharme    2008 Bug if ZDMAX = XUNDEF
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODE_WRITE_COVER_TEX

USE MODI_CONVERT_COVER_ISBA

USE MODD_TYPE_DATE_SURF
USE MODD_WRITE_COVER_TEX,ONLY : NTEX, CNAME, CLANG, NLINES
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XDAY

USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVEGTYPE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,          INTENT(IN) :: KPATCH! number of patch
INTEGER,          INTENT(IN) :: KLAYER! number of soil layers
 CHARACTER(LEN=*), INTENT(IN) :: HISBA ! type of soil (Force-Restore OR Diffusion)
 CHARACTER(LEN=*), INTENT(IN) :: HPHOTO! type of photosynthesis
REAL, DIMENSION(:),INTENT(IN) :: PSOILGRID ! reference grid for DIF
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: I,J,IP
!
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_VEGPARAM! vegetation parameters
 CHARACTER(LEN=6)                      :: YSTRING6
 CHARACTER(LEN=24)                     :: YDATA_CV      ! Cv
 CHARACTER(LEN=24)                     :: YDATA_RE25    ! Re25
 CHARACTER(LEN=4)                      :: YDATA_STRESS  ! Stress def.
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_MONTH   ! monthly veg. parameters
 CHARACTER(LEN=6), DIMENSION(10      ) :: YDATA_LAYER   ! soil layer parameters
!
 CHARACTER(LEN=20) :: YFMT  ! fortran format
!
INTEGER           :: JPAGE ! current page when the number of classes
                           ! is too long to stand on one page only
INTEGER           :: JPATCH   ! loop counter
!
!
LOGICAL           :: GLINE ! flag to write an additional horizontal line
!
REAL, DIMENSION(JPCOVER,JPCOVER     ) :: ZCOVER
REAL, DIMENSION(JPCOVER,KPATCH, 12  ) :: ZVEG, ZLAI, ZZ0VEG, ZEMIS_ECO, ZF2I
REAL, DIMENSION(JPCOVER,KPATCH      ) :: ZRSMIN,ZGAMMA,ZRGL,ZCV,             &
                                           ZALBNIR_VEG,ZALBVIS_VEG,ZALBUV_VEG, &
                                           ZGMES,ZBSLAI,ZLAIMIN,ZSEFOLD,       &
                                           ZH_TREE, ZGC, ZZ0_O_Z0H,            &
                                           ZWRMAX_CF, ZDMAX, ZRE25  
REAL, DIMENSION(JPCOVER,KLAYER,KPATCH):: ZDG, ZROOTFRAC
REAL, DIMENSION(JPCOVER,KPATCH)       :: ZDROOT
REAL, DIMENSION(JPCOVER,KPATCH)       :: ZDG2
INTEGER, DIMENSION(JPCOVER,KPATCH)    :: IWG_LAYER
LOGICAL, DIMENSION(JPCOVER,KPATCH, 12   ) :: GSTRESS
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
GLINE=.FALSE.
!
ZCOVER(:,:) = 0.
DO I=1,JPCOVER
  ZCOVER(I,I) = 1.
END DO
!
!ocl scalar
!
 CALL CONVERT_COVER_ISBA(HISBA,2,ZCOVER,HPHOTO, 'NAT',                    &
                        PRSMIN=ZRSMIN,PGAMMA=ZGAMMA,PWRMAX_CF=ZWRMAX_CF, &
                        PRGL=ZRGL,PCV=ZCV,PSOILGRID=PSOILGRID,           &
                        PDG=ZDG,KWG_LAYER=IWG_LAYER,PDROOT=ZDROOT,       &
                        PDG2=ZDG2,PZ0_O_Z0H=ZZ0_O_Z0H,                   &
                        PALBNIR_VEG=ZALBNIR_VEG,PALBVIS_VEG=ZALBVIS_VEG, &
                        PALBUV_VEG=ZALBUV_VEG,                           &
                        PROOTFRAC=ZROOTFRAC,                             &
                        PGMES=ZGMES,PBSLAI=ZBSLAI,PLAIMIN=ZLAIMIN,       &
                        PSEFOLD=ZSEFOLD,PGC=ZGC,PDMAX=ZDMAX,             &
                        PH_TREE=ZH_TREE,PRE25=ZRE25                       )  
!
DO J=1,12
  CALL CONVERT_COVER_ISBA(HISBA,3*J-1,ZCOVER,HPHOTO, 'NAT',                &
                            PVEG=ZVEG(:,:,J), PLAI=ZLAI(:,:,J),            &
                            PZ0=ZZ0VEG(:,:,J), PEMIS_ECO=ZEMIS_ECO(:,:,J), &
                            PF2I=ZF2I(:,:,J),OSTRESS=GSTRESS(:,:,J)        )  
END DO
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
DO JPATCH=1,KPATCH
!
 IF (       (KPATCH>=2  .AND. JPATCH==1) &
         .OR. (KPATCH>=7  .AND. JPATCH==2) &
         .OR. (KPATCH>=10 .AND. JPATCH==3) ) CYCLE  
!
!
!
 I=0
 DO 

  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf averaged leaf area index} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf indice foliaire moyenn\'e} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||c|c||c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12&$d_2$&$d_3$&$h$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (ZLAI(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,12
        IF (ZLAI(I,JPATCH,J)==0. .OR. ZLAI(I,JPATCH,J)==XUNDEF) THEN
          YDATA_MONTH(J) = ' -  '
        ELSE
          WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZLAI(I,JPATCH,J),1),'.',DEC(ZLAI(I,JPATCH,J),1),')'
          WRITE(YSTRING6, FMT=YFMT) ZLAI(I,JPATCH,J)
          YDATA_MONTH(J) = YSTRING6
        END IF
      END DO

      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDG(I,2,JPATCH),1),'.',DEC(ZDG(I,2,JPATCH),1),')'
      WRITE(YSTRING6, FMT=YFMT) ZDG(I,2,JPATCH)
      YDATA_VEGPARAM(1) = YSTRING6
      IF (KLAYER>=3) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDG(I,3,JPATCH),1),'.',DEC(ZDG(I,3,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZDG(I,3,JPATCH)
        YDATA_VEGPARAM(2) = YSTRING6
      ELSE
        YDATA_VEGPARAM(2) = ' -    '
      END IF

      IF (ZH_TREE(I,JPATCH)==XUNDEF .OR. ZH_TREE(I,JPATCH)==0.) THEN
        YDATA_VEGPARAM(3) = ' -    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZH_TREE(I,JPATCH)),'.',DEC(ZH_TREE(I,JPATCH)),')'
        WRITE(YSTRING6, FMT=YFMT) ZH_TREE(I,JPATCH)
        YDATA_VEGPARAM(3) = YSTRING6
      END IF

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' & ',YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' & ',YDATA_VEGPARAM(3),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT  
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf ground layer depth} (from surface, patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf profondeur des couche de sol} (depuis la surface, partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    YDATA_LAYER(:) = ' -  '
    IF (ZDG(I,1,JPATCH)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,MIN(KLAYER,10)
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZDG(I,J,JPATCH),1),'.',DEC(ZDG(I,J,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZDG(I,J,JPATCH)
        YDATA_LAYER(J) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_LAYER(1), ' & ',YDATA_LAYER(2), ' & ', &
            YDATA_LAYER(3),' & ',YDATA_LAYER(4), ' & ',YDATA_LAYER(5), ' & ', &
            YDATA_LAYER(6),' & ',YDATA_LAYER(7), ' & ',YDATA_LAYER(8), ' & ', &
            YDATA_LAYER(9),' & ',YDATA_LAYER(10),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
IF (HISBA=='DIF') THEN
!
I=0
DO 
  IF (I==JPCOVER) EXIT  

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf cumulative root fraction} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf fraction de racines cumul\'ee} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    YDATA_LAYER(:) = ' -  '
    IF (ZROOTFRAC(I,1,JPATCH)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,MIN(KLAYER,10)
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZROOTFRAC(I,J,JPATCH),1),'.',DEC(ZROOTFRAC(I,J,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZROOTFRAC(I,J,JPATCH)
        YDATA_LAYER(J) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_LAYER(1), ' & ',YDATA_LAYER(2), ' & ', &
            YDATA_LAYER(3),' & ',YDATA_LAYER(4), ' & ',YDATA_LAYER(5), ' & ', &
            YDATA_LAYER(6),' & ',YDATA_LAYER(7), ' & ',YDATA_LAYER(8), ' & ', &
            YDATA_LAYER(9),' & ',YDATA_LAYER(10),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'

ENDDO
END IF
!
!-------------------------------------------------------------------------------
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf vegetation fraction (over natural or agricultural areas)} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf fraction de v\'eg\'etation (sur les surfaces naturelles ou cultiv\'ees)} (partition ", &
  JPATCH,'/',KPATCH,') \\'  
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (ZLAI(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,12
        IF (ZVEG(I,JPATCH,J)==0.) THEN
          YDATA_MONTH(J) = ' -  '
        ELSE
          WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZVEG(I,JPATCH,J),2),'.',DEC(ZVEG(I,JPATCH,J),2),')'
          WRITE(YSTRING6, FMT=YFMT) ZVEG(I,JPATCH,J)

          YDATA_MONTH(J) = YSTRING6
        END IF
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf surface roughness length for momentum: $z_{0}$} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf longueur de rugosit\'e de la surface (qdm): $z_{0}$} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1

    IF (ZLAI(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,12
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZZ0VEG(I,JPATCH,J)),'.',DEC(ZZ0VEG(I,JPATCH,J)),')'
        WRITE(YSTRING6, FMT=YFMT) ZZ0VEG(I,JPATCH,J)
        YDATA_MONTH(J) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
   END DO

  WRITE(NTEX,*) '\end{tabular}'

!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf emissivity of natural continental surfaces} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf \'emissivit\'e des surfaces continentales naturelles} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (ZLAI(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      DO J=1,12
        IF (ZEMIS_ECO(I,JPATCH,J)==1.) THEN
          YDATA_MONTH(J) = ' 1.  '
        ELSE
          WRITE(YSTRING6, FMT='(F3.2)') ZEMIS_ECO(I,JPATCH,J)
          YDATA_MONTH(J) = YSTRING6
        END IF
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf other vegetation parameters} (1) (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf autres param\`etres de v\'eg\'etation} (1) (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) &
    '&&$\alpha_{nir}$&$\alpha_{vis}$&$\alpha_{UV}$&$r_{s_{min}}$&$\gamma$&$rgl$&$C_{w_{r_{max}}}$&$z_0$/$z_{0_h}$&$C_v$\\'  
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (ZLAI(I,JPATCH,1)>0. .AND. ZVEG(I,JPATCH,1)>0. .AND. ZVEG(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      WRITE(YSTRING6, FMT='(F3.2)') ZALBNIR_VEG(I,JPATCH)
      YDATA_VEGPARAM(4) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') ZALBVIS_VEG(I,JPATCH)
      YDATA_VEGPARAM(5) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') ZALBUV_VEG (I,JPATCH)
      YDATA_VEGPARAM(6) = YSTRING6
      ZRSMIN(I,JPATCH) = NINT(ZRSMIN(I,JPATCH))
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZRSMIN(I,JPATCH)),'.',DEC(ZRSMIN(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZRSMIN(I,JPATCH)
      YDATA_VEGPARAM(7) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZGAMMA(I,JPATCH)),'.',DEC(ZGAMMA(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZGAMMA(I,JPATCH)
      YDATA_VEGPARAM(8) = YSTRING6
      ZRGL(I,JPATCH) = NINT(ZRGL(I,JPATCH))
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZRGL(I,JPATCH)),'.',DEC(ZRGL(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZRGL(I,JPATCH)
      YDATA_VEGPARAM(9) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZWRMAX_CF(I,JPATCH)),'.',DEC(ZWRMAX_CF(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZWRMAX_CF(I,JPATCH)
      YDATA_VEGPARAM(10) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZZ0_O_Z0H(I,JPATCH)),'.',DEC(ZZ0_O_Z0H(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZZ0_O_Z0H(I,JPATCH)
      YDATA_VEGPARAM(11) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000. *ZCV(I,JPATCH)),'.',DEC(10000. *ZCV(I,JPATCH)),',A10)'
      WRITE(YDATA_CV, FMT=YFMT) 10000. *ZCV(I,JPATCH),' $10^{-4}$'

      WRITE(NTEX, FMT=*) &
             I,' & ',CNAME(I),' & ', &
                     YDATA_VEGPARAM(4),' & ',YDATA_VEGPARAM(5),' & ', &
                     YDATA_VEGPARAM(6),' & ',YDATA_VEGPARAM(7),' & ', &
                     YDATA_VEGPARAM(8),' & ',YDATA_VEGPARAM(9),' & ', &
                     YDATA_VEGPARAM(10),' & ',YDATA_VEGPARAM(11),' & ', &
                     YDATA_CV,' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf other vegetation parameters} (2) (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf autres param\`etres de v\'eg\'etation} (2) (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$gm$&$B/lai$&$lai_{_{m}}$&$e_{_{fold}}$&$G_c$&$D_{max}$&$f_{2i}$&stress&Re$_{25}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1

    IF (ZLAI(I,JPATCH,1)>0. .AND. ZVEG(I,JPATCH,1)>0. .AND. ZVEG(I,JPATCH,1)/=XUNDEF) THEN
      IP=IP+1
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZGMES(I,JPATCH)),'.',DEC(ZGMES(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZGMES(I,JPATCH)
      YDATA_VEGPARAM(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZBSLAI(I,JPATCH)),'.',DEC(ZBSLAI(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZBSLAI(I,JPATCH)
      YDATA_VEGPARAM(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZLAIMIN(I,JPATCH)),'.',DEC(ZLAIMIN(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZLAIMIN(I,JPATCH)
      YDATA_VEGPARAM(3) = YSTRING6
      ZSEFOLD(I,JPATCH) = NINT(ZSEFOLD(I,JPATCH)/XDAY)
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZSEFOLD(I,JPATCH)),'.',DEC(ZSEFOLD(I,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZSEFOLD(I,JPATCH)
      YDATA_VEGPARAM(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000. *ZGC(I,JPATCH)),'.',DEC(10000. *ZGC(I,JPATCH)),',A10)'
      WRITE(YDATA_CV, FMT=YFMT) 10000. *ZGC(I,JPATCH),' $10^{-4}$'
      IF (ZDMAX(I,JPATCH) /= XUNDEF)THEN
         WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZDMAX(I,JPATCH)),'.',DEC(ZDMAX(I,JPATCH)),')'
         WRITE(YSTRING6, FMT=YFMT) ZDMAX(I,JPATCH)
      ELSE
         YSTRING6 ='  -   '
      ENDIF
      YDATA_VEGPARAM(5) = YSTRING6
      IF (GSTRESS(I,JPATCH,1)) THEN
        YDATA_STRESS='def.'
      ELSE
        YDATA_STRESS='off.'
      END IF
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZF2I(I,JPATCH,1)),'.',DEC(ZF2I(I,JPATCH,1)),')'
      WRITE(YSTRING6, FMT=YFMT) ZF2I(I,JPATCH,1)
      YDATA_VEGPARAM(6) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000000. *ZRE25(I,JPATCH)),'.',DEC(10000000. *ZRE25(I,JPATCH)),',A10)'
      WRITE(YDATA_RE25, FMT=YFMT) 10000000. *ZRE25(I,JPATCH),' $10^{-7}$'
      WRITE(NTEX, FMT=*) &
             I,' & ',CNAME(I),' & ', &
                     YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' & ', &
                     YDATA_VEGPARAM(3),' & ',YDATA_VEGPARAM(4),' & ', &
                     YDATA_CV,         ' & ',YDATA_VEGPARAM(5),' & ', &
                     YDATA_VEGPARAM(6),' & ',                         &
                     YDATA_STRESS ,' & ',YDATA_RE25,' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
  
 END DO
!
END DO 
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_COVER_TEX_ISBA_PAR
