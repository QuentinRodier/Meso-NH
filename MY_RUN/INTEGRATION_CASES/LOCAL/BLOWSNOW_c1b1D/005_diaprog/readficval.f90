!     ##################
      PROGRAM READFICVAL
!     ##################
!
!!****  *READFICVAL* - 
!!
!!    PURPOSE
!!    -------
!       Programme de lecture du fichier FICVAL
!
!!**  METHOD
!!    ------
!!     
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!USE 
!
IMPLICIT NONE
!
!*      0.1    Local variables 
!
!
INTEGER           :: JLOOP, JLOOPJ, JLOOPI, JLOOPITER, J,JJ
INTEGER           :: INUM, IRESP, INUMOUT
INTEGER           :: IND, ICOD
INTEGER           :: IVERB
INTEGER           :: IIINF, IISUP,IJINF, IJSUP, IKREF
INTEGER           :: INBI, INBJ, ITER, INBK
INTEGER           :: INDJ, IDEB, IFIN
INTEGER           :: II, IIRS, IJRS
INTEGER           :: IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IKU,IPROFILE
INTEGER           :: ITEST
!
REAL              :: ZTIME, ZTIMED, ZTIMEF
REAL              :: ZIDEBCOU, ZJDEBCOU
REAL              :: ZIRS, ZJRS
REAL              :: ZIDEBCV, ZJDEBCV, ZIFINCV, ZJFINCV
REAL              :: ZIDEBCVLL, ZJDEBCVLL, ZIFINCVLL, ZJFINCVLL
REAL              :: IIDEBCV, IJDEBCV, IIFINCV, IJFINCV
REAL,DIMENSION(:,:),ALLOCATABLE,SAVE      :: ZTABCH, ZTABCV, ZTABCVXZ
REAL,DIMENSION(:),ALLOCATABLE,SAVE        :: ZX, ZY, ZZ, ZRELIEF, ZVAL, ZLA,ZLO
REAL,DIMENSION(:),ALLOCATABLE,SAVE        :: ZTRS, ZPRS, ZRVRS, ZURS, ZVRS
!
CHARACTER(LEN=20) :: YNOM,YNOMOUT
CHARACTER(LEN=80) :: CAR80, YTIT
CHARACTER(LEN=16) :: YGROUP
CHARACTER(LEN=19) :: YGROUPRS
CHARACTER(LEN=60) :: YPROC
CHARACTER(LEN=40) :: YTITX,YTITY
CHARACTER(LEN=1)  :: YTYPHOR
!
!-------------------------------------------------------------------------------
!
!*      1.0    Ouverture du fichier
!
YNOM(1:LEN(YNOM))=' '
YNOM='FICVAL'
INUM=8
IVERB=5
YNOMOUT='SNOW_DATA'
INUMOUT=9
!CALL FMATTR(YNOM,YNOM,INUM,IRESP)
OPEN(UNIT=INUM,FILE=YNOM,FORM='FORMATTED',STATUS='OLD')
OPEN(UNIT=INUMOUT,FILE=YNOMOUT,FORM='FORMATTED',STATUS='NEW')
!
!*      1.1    Lecture des informations
!
IND=0
DO JLOOP = 1,1000000

  CAR80(1:80)=' '
  YTIT(1:LEN(YTIT))=' '
  READ(INUM,'(A80)',END=99)CAR80
  IND=IND+1
  YGROUP(1:LEN(YGROUP))=' '
  YPROC(1:LEN(YPROC))=' '

  SELECT CASE(CAR80(1:6))
!------------------------------------------------------------------------------
    CASE('CH  G:')
!------------------------------------------------------------------------------
! 1er enregistrement
      YGROUP=CAR80(7:22)
      YPROC=CAR80(26:50)
      READ(CAR80,'(53X,F8.0)')ZTIME
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YGROUP,YPROC,ZTIME ',YGROUP(1:LEN_TRIM(YGROUP)), &
	YPROC(1:LEN_TRIM(YPROC)),ZTIME
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      YTYPHOR=CAR80(43:43)
      READ(CAR80,'(5X,I4,6X,I4,6X,I4,6X,I4,4X,I7)')IIINF,IJINF,IISUP,IJSUP,IKREF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' IIINF,IJINF,IISUP,IJSUP,YTYPHOR,IKREF ', &
	IIINF,IJINF,IISUP,IJSUP,YTYPHOR,IKREF
      ENDIF
! 4eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(CAR80,'(11X,I4,13X,I4,7X,I3)')INBI,INBJ,ITER
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' INBI,INBJ,ITER ',INBI,INBJ,ITER
      ENDIF
      IF(ALLOCATED(ZTABCH))THEN
	DEALLOCATE(ZTABCH)
      ENDIF
      ALLOCATE(ZTABCH(INBI,INBJ))
      DO JLOOPITER=1,ITER

        IF(JLOOPITER==1)THEN
	  IDEB=1; IFIN=5
	ELSE
	  IDEB=IDEB+5; IFIN=IFIN+5
	ENDIF
        IF(JLOOPITER== ITER)THEN
	  IFIN=SIZE(ZTABCH,1)
	ENDIF
! 5eme - 7eme  enregistrements
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        IF(IVERB >0 )THEN
  	  print *,' Enr N: ',IND
        ENDIF
! 8eme - neme  enregistrements
        DO JLOOPJ=SIZE(ZTABCH,2),1,-1
          READ(INUM,'(I3,2X,5(1X,E14.7))',END=99)INDJ,(ZTABCH(JLOOPI,JLOOPJ), &
	  JLOOPI=IDEB,IFIN)
          IND=IND+1
	ENDDO
! n+1eme enregistrement
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        IF(IVERB >0 )THEN
  	  print *,' Enr N: ',IND
        ENDIF

      ENDDO
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND
	print *,' ----------- CH ligne indice 1 ' 
        print *,(ZTABCH(JLOOPI,1),JLOOPI=1,SIZE(ZTABCH,1))
	print *,' ----------- CH ligne dernier indice'
        print *,(ZTABCH(JLOOPI,SIZE(ZTABCH,2)),JLOOPI=1,SIZE(ZTABCH,1))
      ENDIF
!------------------------------------------------------------------------------
    CASE('CH XY ')
!------------------------------------------------------------------------------
! 1er enregistrement
      YGROUP=CAR80(10:25)
      YPROC=CAR80(29:53)
      READ(CAR80,'(56X,F8.0)')ZTIME
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YGROUP,YPROC,ZTIME ',YGROUP(1:LEN_TRIM(YGROUP)), &
	YPROC(1:LEN_TRIM(YPROC)),ZTIME
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      YTYPHOR=CAR80(43:43)
      READ(CAR80,'(5X,I4,6X,I4,6X,I4,6X,I4,4X,I7)')IIINF,IJINF,IISUP,IJSUP,IKREF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' IIINF,IJINF,IISUP,IJSUP,YTYPHOR,IKREF ', &
	IIINF,IJINF,IISUP,IJSUP,YTYPHOR,IKREF
      ENDIF
! 4eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(CAR80,'(11X,I4,13X,I4)')INBI,INBJ
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' INBI,INBJ ',INBI,INBJ
      ENDIF
      IF(ALLOCATED(ZX))THEN
	DEALLOCATE(ZX)
      ENDIF
      IF(ALLOCATED(ZY))THEN
	DEALLOCATE(ZY)
      ENDIF
      ALLOCATE(ZX(INBI),ZY(INBJ))
! 5eme - 7eme  enregistrements
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND
      ENDIF
! 8eme - neme  enregistrements
      DO JLOOPJ=1,MAX(INBI,INBJ)
	IF(INBI == INBJ)THEN
	  READ(INUM,'(19X,E15.8,24X,E15.8)')ZX(JLOOPJ),ZY(JLOOPJ)
	  IND=IND+1
	ELSE IF(INBI > INBJ)THEN
	  IF(JLOOPJ <= INBJ)THEN
	    READ(INUM,'(19X,E15.8,24X,E15.8)')ZX(JLOOPJ),ZY(JLOOPJ)
	    IND=IND+1
	  ELSE
	    READ(INUM,'(19X,E15.8)')ZX(JLOOPJ)
	    IND=IND+1
	  ENDIF
	ELSE IF(INBJ > INBI)THEN
	  IF(JLOOPJ <= INBI)THEN
	    READ(INUM,'(19X,E15.8,24X,E15.8)')ZX(JLOOPJ),ZY(JLOOPJ)
	    IND=IND+1
	  ELSE
	    READ(INUM,'(58X,E15.8)')ZY(JLOOPJ)
	    IND=IND+1
	  ENDIF
	ENDIF
      ENDDO
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND,' X Y '
	print *,'------------ CHXY ZX'
	print *,ZX
	print *,'------------ CHXY ZY'
	print *,ZY
      ENDIF
!------------------------------------------------------------------------------
    CASE('CV  G:')
!------------------------------------------------------------------------------
! 1er enregistrement
      YGROUP=CAR80(7:22)
      YPROC=CAR80(26:50)
      ICOD=0
      IF(CAR80(62:62) /= ' ')THEN
        READ(CAR80,'(53X,F8.0)')ZTIME
      ELSE
	ICOD=1
      ENDIF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YGROUP,YPROC ',YGROUP(1:LEN_TRIM(YGROUP)), &
	YPROC(1:LEN_TRIM(YPROC))
	IF(ICOD == 0)THEN
	  print *,' ZTIME ',ZTIME
	ENDIF
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      ICOD=0
      IF(ALLOCATED(ZTABCV))THEN
	DEALLOCATE(ZTABCV)
      ENDIF
      IF(CAR80(1:8) == 'nidebcou')THEN
        READ(CAR80,'(8X,I4,9X,I4,6X,I5,8X,I4,4X,I4,8X,I3)')IIDEBCOU,IJDEBCOU, &
        ILMAX,ILANGLE,IKU,ITER
	ALLOCATE(ZTABCV(ILMAX,IKU))
      ELSE IF(CAR80(1:8) == 'xidebcou')THEN
        READ(CAR80,'(8X,F8.0,9X,F8.0,6X,I5,8X,I4,4X,I4,8X,I3)')ZIDEBCOU, &
	ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER
	ALLOCATE(ZTABCV(ILMAX,IKU))
      ELSE IF(CAR80(1:8) == 'cc(deb)-')THEN
        READ(CAR80,'(15X,F8.0,1X,F8.0,3X,F8.0,1X,F8.0,7X,I5,4X,I4,5X,I3)')ZIDEBCV, &
	ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER
	ALLOCATE(ZTABCV(ILMAX,IKU))
      ELSE IF(CAR80(1:8) == 'll(deb)-')THEN
        READ(CAR80,'(15X,F8.3,1X,F8.3,3X,F8.3,1X,F8.3,7X,I5,4X,I4,5X,I3)')ZIDEBCVLL, &
	ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER
	ALLOCATE(ZTABCV(ILMAX,IKU))
      ELSE IF(CAR80(1:8) == 'ij(deb)-')THEN
        READ(CAR80,'(15X,I4,1X,I4,3X,I4,1X,I4,7X,I5,4X,I4,5X,I3)')IIDEBCV, &
	IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER
	ALLOCATE(ZTABCV(ILMAX,IKU))
      ELSE
	ICOD=1
	READ(CAR80,'(18X,I4,15X,I4,8X,I3)')INBI,INBK,ITER
	ALLOCATE(ZTABCV(INBI,INBK))
      ENDIF
      IF(IVERB >0 )THEN
	IF(ICOD == 0)THEN
          IF(CAR80(1:8) == 'nidebcou')THEN
	  print *,' Enr N: ',IND,' IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IKU,ITER ', &
	  IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IKU,ITER
          ELSE IF(CAR80(1:8) == 'xidebcou')THEN
	  print *,' Enr N: ',IND,' ZIDEBCOU,ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER ', &
	  ZIDEBCOU,ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER
          ELSE IF(CAR80(1:8) == 'cc(deb)-')THEN
	  print *,' Enr N: ',IND,' ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER',&
	  ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER
          ELSE IF(CAR80(1:8) == 'll(deb)-')THEN
	  print *,' Enr N: ',IND,' ZIDEBCVLL,ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER',&
	  ZIDEBCVLL,ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER
          ELSE IF(CAR80(1:8) == 'ij(deb)-')THEN
	  print *,' Enr I: ',IND,' IIDEBCV,IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER',&
	  IIDEBCV,IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER
	  ENDIF
	ELSE
	  print *,' Enr N: ',IND,' INBI,INBK,ITER ',INBI,INBK,ITER
	ENDIF
      ENDIF
! 4eme - neme enregistrements
      DO JLOOPI=1,ITER
	IF(JLOOPI == 1)THEN
	  IDEB=1; IFIN=5
	ELSE
	  IDEB=IDEB+5; IFIN=IFIN+5
	ENDIF
	IF(JLOOPI == ITER)THEN
	  IFIN=SIZE(ZTABCV,1)
	ENDIF
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        DO JLOOPJ=SIZE(ZTABCV,2),1,-1
	  READ(INUM,'(5X,5(1X,E14.7))')(ZTABCV(II,JLOOPJ),II=IDEB,IFIN)
	  IND=IND+1
        ENDDO
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
      ENDDO
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND
	print *,' ----------- CV ligne indice 1'
        print *,(ZTABCV(JLOOPI,1),JLOOPI=1,SIZE(ZTABCV,1))
	print *,' ----------- CV ligne dernier indice'
        print *,(ZTABCV(JLOOPI,SIZE(ZTABCV,2)),JLOOPI=1,SIZE(ZTABCV,1))
      ENDIF
!------------------------------------------------------------------------------
    CASE('CV XZ ','CV TIM')
!------------------------------------------------------------------------------
! 1er enregistrement
      ICOD=0
      IF(CAR80(64:64) == 's')THEN
        YGROUP=CAR80(9:24)
        YPROC=CAR80(28:52)
        READ(CAR80,'(55X,F8.0)')ZTIME
      ELSE
	ICOD=1
        YGROUP=CAR80(15:30)
        YPROC=CAR80(34:73)
      ENDIF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YGROUP,YPROC ',YGROUP(1:LEN_TRIM(YGROUP)), &
	YPROC(1:LEN_TRIM(YPROC))
	IF(ICOD == 0)THEN
	  print *,' ZTIME ',ZTIME
	ENDIF
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      ICOD=0
      IF(CAR80(1:8) == 'nidebcou')THEN
        READ(CAR80,'(8X,I4,9X,I4,6X,I5,8X,I4,4X,I4,8X,I3)')IIDEBCOU,IJDEBCOU, &
        ILMAX,ILANGLE,IKU,ITER
      ELSE IF(CAR80(1:8) == 'xidebcou')THEN
        READ(CAR80,'(8X,F8.0,9X,F8.0,6X,I5,8X,I4,4X,I4,8X,I3)')ZIDEBCOU, &
	ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER
      ELSE IF(CAR80(1:8) == 'cc(deb)-')THEN
        READ(CAR80,'(15X,F8.0,1X,F8.0,3X,F8.0,1X,F8.0,7X,I5,4X,I4,5X,I3)')ZIDEBCV, &
	ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER
      ELSE IF(CAR80(1:8) == 'll(deb)-')THEN
        READ(CAR80,'(15X,F8.3,1X,F8.3,3X,F8.3,1X,F8.3,7X,I5,4X,I4,5X,I3)')ZIDEBCVLL, &
	ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER
      ELSE IF(CAR80(1:8) == 'ij(deb)-')THEN
        READ(CAR80,'(15X,I4,1X,I4,3X,I4,1X,I4,7X,I5,4X,I4,5X,I3)')IIDEBCV, &
	IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER
      ELSE
	ICOD=1
	READ(CAR80,'(18X,I4,15X,I4,8X,I3)')INBI,INBK,ITER
      ENDIF
      IF(ICOD == 0)THEN
	IF(ALLOCATED(ZX))THEN
	  DEALLOCATE(ZX)
	ENDIF
	IF(ALLOCATED(ZLA))THEN
	  DEALLOCATE(ZLA)
	ENDIF
	IF(ALLOCATED(ZLO))THEN
	  DEALLOCATE(ZLO)
	ENDIF
	IF(ALLOCATED(ZRELIEF))THEN
	  DEALLOCATE(ZRELIEF)
	ENDIF
	IF(ALLOCATED(ZTABCVXZ))THEN
	  DEALLOCATE(ZTABCVXZ)
	ENDIF
	ALLOCATE(ZX(ILMAX),ZRELIEF(ILMAX),ZLA(ILMAX),ZLO(ILMAX))
	ALLOCATE(ZTABCVXZ(ILMAX,IKU))
      ENDIF
      IF(IVERB >0 )THEN
	IF(ICOD == 0)THEN
          IF(CAR80(1:8) == 'nidebcou')THEN
	  print *,' Enr N: ',IND,' IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IKU,ITER ', &
	  IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IKU,ITER
          ELSE IF(CAR80(1:8) == 'xidebcou')THEN
	  print *,' Enr N: ',IND,' ZIDEBCOU,ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER ', &
	  ZIDEBCOU,ZJDEBCOU,ILMAX,ILANGLE,IKU,ITER
          ELSE IF(CAR80(1:8) == 'cc(deb)-')THEN
	  print *,' Enr N: ',IND,' ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER',&
	  ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IKU,ITER
          ELSE IF(CAR80(1:8) == 'll(deb)-')THEN
	  print *,' Enr N: ',IND,' ZIDEBCVLL,ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER',&
	  ZIDEBCVLL,ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IKU,ITER
          ELSE IF(CAR80(1:8) == 'ij(deb)-')THEN
	  print *,' Enr I: ',IND,' IIDEBCV,IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER',&
	  IIDEBCV,IJDEBCV,IIFINCV,IJFINCV,ILMAX,IKU,ITER
	  ENDIF
	ELSE
	  print *,' Enr N: ',IND,' INBI,INBK,ITER ',INBI,INBK,ITER
	ENDIF
      ENDIF
! 4eme - neme enregistrements
      IF(ICOD == 0)THEN

        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
	IF(CAR80(50:50) /= ' ')THEN
	  DO JLOOPI=1,ILMAX
	    READ(INUM,'(10X,2(1X,E15.8),2(2X,F10.5))')ZX(JLOOPI),ZRELIEF(JLOOPI),&
	    ZLA(JLOOPI),ZLO(JLOOPI)
            IND=IND+1
	  ENDDO
          IF(IVERB >0 )THEN
	    print *,' Enr N: ',IND,' ZX ZRELIEF ZLA ZLO'
	    print *,'! -----------------  CVXZ ZX'
	    print *,ZX
	    print *,'! -----------------  CVXZ ZRELIEF'
	    print *,ZRELIEF
	    print *,'! -----------------  CVXZ ZLA'
	    print *,ZLA
	    print *,'! -----------------  CVXZ ZLO'
	    print *,ZLO
	    print *,'! -----------------'
	  ENDIF
	ELSE
	  DO JLOOPI=1,ILMAX
	    READ(INUM,'(10X,2(1X,E15.8))')ZX(JLOOPI),ZRELIEF(JLOOPI)
            IND=IND+1
	  ENDDO
          IF(IVERB >0 )THEN
	    print *,' Enr N: ',IND,' ZX ZRELIEF '
	    print *,'! ----------------- CVXZ ZX'
	    print *,ZX
	    print *,'! ----------------- CVXZ ZRELIEF'
	    print *,ZRELIEF
	    print *,'! -----------------'
	  ENDIF
	ENDIF
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
	DO JLOOPI=1,ITER
	  IF(JLOOPI == 1)THEN
	    IDEB=1; IFIN=5
	  ELSE
	    IDEB=IDEB+5; IFIN=IFIN+5
	  ENDIF
	  IF(JLOOPI == ITER)THEN
	    IFIN=SIZE(ZTABCV,1)
	  ENDIF
          READ(INUM,'(A80)',END=99)CAR80
          IND=IND+1
          READ(INUM,'(A80)',END=99)CAR80
          IND=IND+1
          READ(INUM,'(A80)',END=99)CAR80
          IND=IND+1
          READ(INUM,'(A80)',END=99)CAR80
          IND=IND+1
          DO JLOOPJ=SIZE(ZTABCVXZ,2),1,-1
	    READ(INUM,'(5X,5(1X,E14.7))')(ZTABCVXZ(II,JLOOPJ),II=IDEB,IFIN)
            IND=IND+1
          ENDDO
          READ(INUM,'(A80)',END=99)CAR80
          IND=IND+1
        ENDDO
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND
	  print *,' ----------- CVXZ ligne indice 1'
          print *,(ZTABCVXZ(JLOOPI,1),JLOOPI=1,SIZE(ZTABCVXZ,1))
	  print *,' ----------- CVXZ ligne dernier indice'
          print *,(ZTABCVXZ(JLOOPI,SIZE(ZTABCVXZ,2)),JLOOPI=1,SIZE(ZTABCVXZ,1))
        ENDIF

      ELSE

        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1

      ENDIF
!------------------------------------------------------------------------------
    CASE('TRAPRO')
!------------------------------------------------------------------------------
! 1er enregistrement
      ICOD=0
      YGROUP=CAR80(11:26)
      YPROC=CAR80(30:54)
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' YGROUP,YPROC ',YGROUP,YPROC
        ENDIF
      IF(CAR80(56:60) /= 'NBVAL')THEN
	READ(CAR80(62:80),*)ZTIME
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' ZTIME ',ZTIME
        ENDIF
      ELSE
	ICOD=1
	READ(CAR80,*)ZTIME,INBK
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' ZTIME NBVAL ',ZTIME,INBK
        ENDIF
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(ICOD == 0)THEN
      IF(CAR80(1:8) == 'nidebcou')THEN
        READ(CAR80,'(8X,I4,9X,I4,6X,I5,8X,I4,8X,I4)')IIDEBCOU,IJDEBCOU, &
        ILMAX,ILANGLE,IPROFILE
        IF(IVERB >0 )THEN
	  print *,' Enr N: ',IND
	  print *,' IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IPROFILE ',IIDEBCOU,IJDEBCOU,&
	  ILMAX,ILANGLE,IPROFILE
        ENDIF
      ELSE IF(CAR80(1:8) == 'xidebcou')THEN
        READ(CAR80,'(8X,F8.0,9X,F8.0,6X,I5,8X,I4,8X,I4)')ZIDEBCOU,ZJDEBCOU, &
        ILMAX,ILANGLE,IPROFILE
        IF(IVERB >0 )THEN
	  print *,' Enr N: ',IND
	  print *,' ZIDEBCOU,ZJDEBCOU,ILMAX,ILANGLE,IPROFILE ',ZIDEBCOU,ZJDEBCOU,&
	  ILMAX,ILANGLE,IPROFILE
        ENDIF
      ELSE IF(CAR80(1:8) == 'cc(deb)-')THEN
        READ(CAR80,'(15X,F8.0,1X,F8.0,3X,F8.0,1X,F8.0,7X,I5,8X,I4)')ZIDEBCV,ZJDEBCV, &
        ZIFINCV,ZJFINCV,ILMAX,IPROFILE
        IF(IVERB >0 )THEN
	  print *,' Enr N: ',IND
	  print *,' ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IPROFILE ',ZIDEBCV,ZJDEBCV,&
	  ZIFINCV,ZJFINCV,ILMAX,ILANGLE,IPROFILE
        ENDIF
      ELSE IF(CAR80(1:8) == 'll(deb)-')THEN
        READ(CAR80,'(15X,F8.3,1X,F8.3,3X,F8.3,1X,F8.3,7X,I5,8X,I4)')ZIDEBCVLL,ZJDEBCVLL, &
        ZIFINCVLL,ZJFINCVLL,ILMAX,IPROFILE
        IF(IVERB >0 )THEN
	  print *,' Enr N: ',IND
	  print *,' ZIDEBCVLL,ZJDEBCVLL,ZIFINCVLL,ZJFINCVLL,ILMAX,IPROFILE ',ZIDEBCVLL,ZJDEBCVLL,&
	  ZIFINCVLL,ZJFINCVLL,ILMAX,ILANGLE,IPROFILE
        ENDIF
      ELSE IF(CAR80(1:8) == 'ij(deb)-')THEN
        READ(CAR80,'(15X,I4,1X,I4,3X,I4,1X,I4,7X,I5,8X,I4)')IIDEBCV,IJDEBCV, &
        IIFINCV,IJFINCV,ILMAX,IPROFILE
        IF(IVERB >0 )THEN
	  print *,' Enr N: ',IND
	  print *,' ZIDEBCV,ZJDEBCV,ZIFINCV,ZJFINCV,ILMAX,IPROFILE ',ZIDEBCV,ZJDEBCV,&
	  ZIFINCV,ZJFINCV,ILMAX,ILANGLE,IPROFILE
        ENDIF
      ENDIF
      ENDIF
! 4eme - 6eme  enregistrements
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
	READ(CAR80(54:80),*)INBK
	IF(ALLOCATED(ZZ))THEN
	  DEALLOCATE(ZZ)
	ENDIF
	IF(ALLOCATED(ZVAL))THEN
	  DEALLOCATE(ZVAL)
	ENDIF
	ALLOCATE(ZVAL(INBK),ZZ(INBK))
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        IF(IVERB >0 )THEN
  	  print *,' Enr N: ',IND
        ENDIF
! 7eme - neme  enregistrements
        DO JLOOPJ=INBK,1,-1
	  READ(INUM,'(9X,E15.8,4X,E15.8)')ZVAL(JLOOPJ),ZZ(JLOOPJ)
	  IND=IND+1
	ENDDO
        IF(IVERB >0 )THEN
  	  print *,' Enr N: ',IND,' ZVAL ZZ '
	  print *,' ------------------- TRAPRO ZVAL' 
	  print *,ZVAL
	  print *,' ------------------- TRAPRO ZZ'
	  print *,ZZ
        ENDIF
        ITEST=1
        DO WHILE(ZZ(ITEST)<100)
           ITEST=ITEST+1
        END DO
        IF(IVERB >0 )THEN
!         write(INUMOUT,'(F7.2,A1,A4)') ZTIME,'','TEST'
         write(INUMOUT,'(A20,A1,A20)') YGROUP,'',YPROC
         write(INUMOUT,'(I4,A1,F7.2)') ITEST,'',ZTIME
         DO JJ=2,ITEST
         IF(YGROUP(1:6)=='SNWNOA') THEN         
          write(INUMOUT,'(F7.2,A1,F16.3)') ZZ(JJ),'',ZVAL(JJ)
         ELSE
          write(INUMOUT,'(F7.2,A1,F16.10)') ZZ(JJ),'',ZVAL(JJ)
         END IF
         END DO
!         write(INUMOUT,'(A3,A1,A4)') 'FIN','','TEST' 
        ENDIF

!------------------------------------------------------------------------------
    CASE('TRAXY ')
!------------------------------------------------------------------------------
! 1er enregistrement
      YGROUP=CAR80(10:25)
      YPROC=CAR80(29:53)
      READ(CAR80(58:65),*)ZTIMED
      READ(CAR80(71:78),*)ZTIMEF
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' YGROUP,YPROC,ZTIMED,ZTIMEF ',YGROUP,YPROC,&
	  ZTIMED,ZTIMEF
        ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      YTITX(1:LEN(YTITX))=' '
      YTITY(1:LEN(YTITY))=' '
      YTITX=CAR80(6:30)
      YTITY=CAR80(37:61)
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTITX,YTITY ',YTITX(1:LEN_TRIM(YTITX)), &
	YTITY(1:LEN_TRIM(YTITY))
      ENDIF
      READ(CAR80(69:80),*)INBI
      IF(IVERB >0 )THEN
	print *,' NB VALEURS ',INBI
      ENDIF
      IF(ALLOCATED(ZX))THEN
	DEALLOCATE(ZX)
      ENDIF
      IF(ALLOCATED(ZY))THEN
	DEALLOCATE(ZY)
      ENDIF
      ALLOCATE(ZX(INBI),ZY(INBI))
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)YTIT
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      CAR80(1:80)=' '
! 4eme - 6eme  enregistrements
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND
      ENDIF
      CAR80(1:80)=' '
! 7eme - neme  enregistrements
      DO JLOOPJ=1,INBI
	READ(INUM,'(9X,E15.8,4X,E15.8)',END=99)ZX(JLOOPJ),ZY(JLOOPJ)
        IND=IND+1
      ENDDO
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND,' ZX ZY '
        print *,' ------------------- TRAXY ZX'
        print *,ZX
        print *,' ------------------- TRAXY ZY'
        print *,ZY
        write(INUMOUT,'(A20,A1,A20)') YGROUP,'',YPROC
        DO JJ=1,size(ZX,1)
         IF(YGROUP(1:9)=='CANSNW_NU') THEN         
          write(INUMOUT,'(F7.2,A1,F16.3)') ZTIMEF,'',ZY(JJ)
         ELSE
          write(INUMOUT,'(F7.2,A1,F16.10)') ZTIMEF,'',ZY(JJ)
         END IF
        END DO
!        write(INUMOUT,'(F7.2,A1,F11.6)') ZTIMEF,'',ZY
      ENDIF
!------------------------------------------------------------------------------
    CASE('VARFCT')
!------------------------------------------------------------------------------
! 1er enregistrement
      ICOD=0
      YGROUP=CAR80(16:31)
      YPROC=CAR80(35:64)
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' YGROUP,YPROC ',YGROUP,YPROC
        ENDIF
      IF(CAR80(66:70) == 'NBVAL')THEN
	READ(CAR80(72:80),*)INBI
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' NB VALEURS ',INBI
        ENDIF
      ELSE
	ICOD=1
	READ(CAR80(68:80),*)INBK
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' K = ',INBK
        ENDIF
      ENDIF
! 2eme enregistrement
      IF(ICOD == 0)THEN
        READ(INUM,'(A80)',END=99)YTIT
        IND=IND+1
      ELSE
        READ(INUM,'(A80)',END=99)YTIT
        IND=IND+1
	IF(YTIT(73:80) /= ' ')THEN
	  READ(YTIT(73:80),*)INBI
	ELSE
	  READ(YTIT(53:80),*)INBI
	ENDIF
        IF(IVERB >0 )THEN
          print *,' Enr N: ',IND,' NB VALEURS ',INBI
        ENDIF
      ENDIF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' YTIT ',YTIT(1:LEN_TRIM(YTIT))
      ENDIF
      IF(ALLOCATED(ZX))THEN
	DEALLOCATE(ZX)
      ENDIF
      IF(ALLOCATED(ZY))THEN
	DEALLOCATE(ZY)
      ENDIF
      ALLOCATE(ZX(INBI),ZY(INBI))
      CAR80(1:80)=' '
! Eventuel 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(CAR80(1:8) == 'nidebcou')THEN
        READ(CAR80,'(8X,I4,9X,I4,6X,I5,8X,I4,8X,I4)')IIDEBCOU,IJDEBCOU, &
        ILMAX,ILANGLE,IPROFILE
        IF(IVERB >0 )THEN
  	  print *,' Enr N: ',IND
	  print *,' IIDEBCOU,IJDEBCOU,ILMAX,ILANGLE,IPROFILE ',IIDEBCOU,IJDEBCOU,&
	  ILMAX,ILANGLE,IPROFILE
        ENDIF
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
      ELSE
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
      ENDIF
! 4eme - neme enregistrements
      DO JLOOPJ=1,INBI
	READ(INUM,'(9X,E15.8,4X,E15.8)')ZX(JLOOPJ),ZY(JLOOPJ)
      ENDDO
! n+1eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(IVERB >0 )THEN
        print *,' Enr N: ',IND,' ZX ZY '
        print *,' ------------------- VARFCT ZX'
        print *,ZX
        print *,' ------------------- VARFCT ZY'
        print *,ZY
      ENDIF
   !   IF(YPROC(LEN(YPROC)-3:LEN(YPROC))=='WIND') THEN
         write(INUMOUT,'(A20,A1,A20)') YGROUP,'',YPROC
         write(INUMOUT,'(I4,A1,I4)') INBI,'',INBI
         DO JJ=1,size(ZX,1)
          write(INUMOUT,'(F10.2,A1,F16.10)') ZX(JJ),'',ZY(JJ)
         END DO
!      END IF
      
!------------------------------------------------------------------------------
    CASE('RS  G:')
!------------------------------------------------------------------------------
! 1er enregistrement
      IF(CAR80(25:25) /= ' ')THEN
        YGROUPRS=CAR80(7:25)
        ICOD=0
      ELSE
	YGROUP=CAR80(7:22)
	ICOD=1
      ENDIF
      READ(CAR80,'(53X,F8.0)')ZTIME
      IF(IVERB >0 )THEN
	IF(ICOD == 0)THEN
	  print *,' Enr N: ',IND,' YGROUPRS ',YGROUPRS
	ELSE
	  print *,' Enr N: ',IND,' YGROUP ',YGROUP
	ENDIF
	print *,' ZTIME ',ZTIME
      ENDIF
! 2eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(ICOD == 0)THEN
	READ(CAR80,'(55X,I5)')IKU
      ELSE
	READ(CAR80,'(5X,I5)')IKU
      ENDIF
      IF(IVERB >0 )THEN
	print *,' Enr N: ',IND,' NBVAL ',IKU
      ENDIF
      CAR80(1:80)=' '
! 3eme enregistrement
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(CAR80(2:8) == '*******')THEN
      ELSE
      IF(CAR80(1:4) == 'xirs')THEN
	READ(CAR80,'(4X,F10.5,5X,F10.5)')ZIRS,ZJRS
	ICOD=0
      ELSE
	READ(CAR80,'(4X,I5,5X,I5)')IIRS,IJRS
	ICOD=1
      ENDIF
      IF(IVERB >0 )THEN
	IF(ICOD == 0)THEN
	  print *,' Enr N: ',IND,' xirs ',ZIRS,' xjrs ',ZJRS
	ELSE
	  print *,' Enr N: ',IND,' nirs ',IIRS,' njrs ',IJRS
	ENDIF
      ENDIF
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      ENDIF
! 4eme (ou 5eme) enregistrement - niemes enr.
      IF(ALLOCATED(ZTRS))THEN
        DEALLOCATE(ZTRS)
      ENDIF
      IF(ALLOCATED(ZPRS))THEN
        DEALLOCATE(ZPRS)
      ENDIF
      IF(ALLOCATED(ZRVRS))THEN
        DEALLOCATE(ZRVRS)
      ENDIF
      IF(ALLOCATED(ZURS))THEN
        DEALLOCATE(ZURS)
      ENDIF
      IF(ALLOCATED(ZVRS))THEN
        DEALLOCATE(ZVRS)
      ENDIF
      ALLOCATE(ZTRS(IKU),ZPRS(IKU),ZRVRS(IKU),ZURS(IKU),ZVRS(IKU))
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      DO J=SIZE(ZTRS,1),1,-1
        CAR80(1:80)=' '
        READ(INUM,'(A80)',END=99)CAR80
        IND=IND+1
	READ(CAR80,'(12X,F7.2,3X,F7.0,3X,E15.8,3X,F7.2,3X,F7.2)')ZTRS(J),ZPRS(J),ZRVRS(J),ZURS(J),ZVRS(J)
      ENDDO
      READ(INUM,'(A80)',END=99)CAR80
      IND=IND+1
      IF(IVERB >0 )THEN
	print *,' ZTRS(2),...',ZTRS(2),ZPRS(2),ZRVRS(2),ZURS(2),ZVRS(2)
      ENDIF
  END SELECT

ENDDO
!
99 CONTINUE
PRINT *,' Fin de Fichier , Enregistrement N: ',IND
!----------------------------------------------------------------------------
!
!*       4.     EXIT
!               ----
!
STOP
END PROGRAM READFICVAL
