!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_NATURE_ISBA_EKF (I, &
                                  HPROGRAM, KI, PT2M, PHU2M, HTEST)

! -----------------------------------------------------------------------------
!
! Land Data Assimilation System based on an Extended Kalman Filter
!
! Revised version : JFM (15 September 2008)
!
! The control vector can be any element of (TG1,TG2,WG1,WG2) - Choice in namelist
!
! The observations can be any element of (T2M,HU2M,WG1) - Choice in namelist
!
! Possibility to evolve the B matrix in the cycling - otherwise SEKF
!
! First version including patches (15 October 2008)
! Trygve Aspelien, Separating IO  06/2013
! Alina Barbu: bug correction of B matrix, otherwise understimation of the gain matrix (11/2013) 
! Alina Barbu: equivalent analysis of B matrix to ensure its symetric and positiv definiteness properties (11/2013) 
  
! -----------------------------------------------------------------------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURFEX_MPI,    ONLY : NRANK, NPIO
!
USE MODD_ASSIM,         ONLY : LBEV, LBFIXED, NOBSTYPE, XERROBS, XQCOBS, NNCO, NVAR, NNCV, &
                               XSCALE_Q, NPRINTLEV, CVAR, XSIGMA, CBIO, XI,        &
                               XF_PATCH, XF, COBS, XSCALE_QLAI,CFILE_FORMAT_OBS,   &
                               XALPH,NECHGU, NBOUTPUT, XTPRT, XLAI_PASS, XBIO_PASS,&
                               NOBS, XYO
! 
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
!
#ifdef SFX_ARO
USE YOMMP0,             ONLY : MYPROC 
#endif
!
USE YOMHOOK,            ONLY : LHOOK,DR_HOOK
USE PARKIND1,           ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_ADD_FORECAST_TO_DATE_SURF
!
USE MODE_EKF
!
! -----------------------------------------------------------
!
IMPLICIT NONE
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM     ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL, DIMENSION(:), INTENT(IN) :: PT2M
REAL, DIMENSION(:), INTENT(IN) :: PHU2M
 CHARACTER(LEN=2),   INTENT(IN) :: HTEST        ! must be equal to 'OK'
!
!    Declarations of local variables
!
 CHARACTER(LEN=30)  :: YBGFILE
 CHARACTER(LEN=19)  :: YLFNAME
 CHARACTER(LEN=9)   :: YFNAME
 CHARACTER(LEN=7)   :: YMYPROC
 CHARACTER(LEN=1)   :: YCHAR
!
! Local Matrix for Analysis calculation
!
!  Allocation
!  Perturbed simulations
!
! Initial values (to be analysed)
! Observations
!
! Temporary vectors used by the EKF approach
REAL,DIMENSION(KI) :: ZCOFSWI                     ! dynamic range (Wfc - Wwilt)
!REAL,DIMENSION(KI) :: ZSMSAT                      ! saturation  
!REAL,DIMENSION(KI) :: ZWILT
!
REAL,DIMENSION(KI,I%NPATCH,NVAR) :: ZCOEF
REAL,DIMENSION(KI,I%NPATCH,NVAR) :: ZEPS            ! The perturbation amplitude
!
REAL,DIMENSION(NVAR+1,NOBSTYPE) :: ZYF            ! Vector of model observations (averaged) 
!
REAL,DIMENSION(KI*I%NPATCH*NVAR*I%NPATCH*NVAR) :: ZBLONG
REAL,DIMENSION(KI,I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZB           ! background error covariance matrix
!
REAL,DIMENSION(I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZLTM         ! linear tangent matrix for the f'ward model
REAL,DIMENSION(I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZQ           ! model error matrix
!
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,I%NPATCH*NVAR) :: ZHO, ZHOWR             ! Jacobian of observation operator
REAL,DIMENSION(I%NPATCH*NVAR,NOBSTYPE*NBOUTPUT) :: ZHOT            ! Transpose of HO
REAL,DIMENSION(I%NPATCH*NVAR,NOBSTYPE*NBOUTPUT) :: ZGAIN           ! Kalman gain (used explicitly for Ba) 
!
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZR        ! covariance matrix of observation errors
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZK1
REAL,DIMENSION(NOBSTYPE*NBOUTPUT) :: ZX,ZB2,ZP
!
REAL,DIMENSION(I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZKRK
REAL,DIMENSION(I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZIDKH
REAL,DIMENSION(I%NPATCH*NVAR,I%NPATCH*NVAR) :: ZIDENT          ! identitiy matrix, used for Ba
!
REAL,DIMENSION(I%NPATCH) :: ZVLAIMIN
!
REAL :: ZTIME                      ! current time since start of the run (s)

INTEGER :: IOBSCOUNT
INTEGER :: IYEAR                      ! current year (UTC)
INTEGER :: IMONTH                     ! current month (UTC)
INTEGER :: IDAY                       ! current day (UTC)
INTEGER :: IHOUR
INTEGER :: IRESP                      ! return code
INTEGER :: ISTEP                      ! 
INTEGER :: IMYPROC
INTEGER :: IOBS
INTEGER :: ISTAT, ICPT, IUNIT
!
INTEGER :: JI,JJ,JK,JJJ,JL,K1,L1
!
LOGICAL :: GBEXISTS
!
REAL(KIND=JPRB)                            :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_EKF',0,ZHOOK_HANDLE)

#ifdef USE_SODA

!
!############################# BEGINNING ###############################
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_ISBA_EKF: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF ( NPRINTLEV>0  .AND. NRANK==NPIO ) THEN
  WRITE(*,*)
  WRITE(*,*) '   --------------------------'
  WRITE(*,*) '   |   ENTERING  VARASSIM   |'
  WRITE(*,*) '   --------------------------'
  WRITE(*,*)
ENDIF
!
#ifdef SFX_ARO
IF ( MYPROC > 0 ) THEN 
  IMYPROC = MYPROC
ELSE
  IMYPROC = 1
ENDIF
#else
IMYPROC = NRANK+1
#endif
!
WRITE(YMYPROC(1:7),'(I7.7)') IMYPROC
!
IF ( NPRINTLEV > 0  .AND. NRANK==NPIO ) WRITE(*,*) 'number of patches =',I%NPATCH
!
!############################# INITIALISATIONS ###############################
!
!   Read CLAY fraction to  compute the SWI range (Wfc - Wwilt)
!   (XSIGMA is defined in terms of SWI), need to convert to equivalent v/v
!   using same clay fraction in both layers
!   Read SAND fraction to compute the saturation for conversion of ERS SWI
!
 CALL COFSWI(I%XCLAY(:,1),ZCOFSWI)
 !
!DO JI=1,KI
  !ZSMSAT (I) = 0.001 * (-1.08*100.*XSAND(I,1) + 494.305)
  !ZWILT  (I) = 0.001 * 37.1342 * ((100.*XCLAY(I,1))**0.5) 
!ENDDO
!
! Set control variables
ZIDENT(:,:) = 0.                   ! identity matrix
DO JL = 1,NVAR
  !
  DO JJ = 1,I%NPATCH
    ZIDENT(JJ+I%NPATCH*(JL-1),JJ+I%NPATCH*(JL-1)) = 1.0
  ENDDO
  !
  WHERE ( XI(:,:,JL)/=XUNDEF )
    ZEPS(:,:,JL) = XTPRT(JL) * XI(:,:,JL)
  ELSEWHERE
    ZEPS(:,:,JL) = 1.
  ENDWHERE
  !
  IF ( TRIM(CVAR(JL))=='WG2' .OR. TRIM(CVAR(JL))=='WG1' ) THEN
    !
    DO JI = 1,KI
      ZCOEF(JI,:,JL) = ZCOFSWI(JI)*ZCOFSWI(JI)
    ENDDO
    !
  ELSEIF ( TRIM(CVAR(JL))=='LAI' .AND. LBFIXED ) THEN
    !
    DO JI = 1,KI
      DO JJ = 1,I%NPATCH
        IF ( XLAI_PASS(JI,JJ)/=XUNDEF .AND. XLAI_PASS(JI,JJ)>=2. ) THEN
          ZCOEF(JI,JJ,JL) = XLAI_PASS(JI,JJ)*XLAI_PASS(JI,JJ)
        ELSE 
          ZCOEF(JI,JJ,JL) = 0.4*0.4/(XSIGMA(JL)*XSIGMA(JL))
        ENDIF
      ENDDO
    ENDDO
    !
  ELSE
    !
    ZCOEF(:,:,JL) = 1.
    !
  ENDIF
  !
ENDDO
!
!############################# B CALCULATION ###############################
!
! ----------------------
! VARASSIM OPTION : LBEV
! ----------------------
!   Calculate the LTM, and evolve B. 
!
! Set the B input file depending of an existing B was found or not
YBGFILE = "BGROUNDin."//TRIM(YMYPROC)
INQUIRE (FILE=TRIM(YBGFILE),EXIST=GBEXISTS)
!
IF ( LBEV .AND. GBEXISTS ) THEN
  !
  ZB(:,:,:) = 0.
  CALL B_BIG_LOOP(I, &
                  "READ",YBGFILE,ZB)
  IF ( NPRINTLEV > 0 ) WRITE(*,*) 'read previous B matrix  ==>',ZB(1,1,1),NVAR
  !
ELSEIF ( LBEV .OR. LBFIXED ) THEN
  !
  ! Initialization of B 
  ZB(:,:,:) = 0.
  DO JI = 1,KI
    DO JL = 1,NVAR
      DO JJ = 1,I%NPATCH   
        !
        L1 = JJ + I%NPATCH *(JL-1)
        ZB(JI,L1,L1) = XSIGMA(JL)*XSIGMA(JL) * ZCOEF(JI,JJ,JL)
        !
      ENDDO
    ENDDO
    !
  ENDDO
  !
  IF ( LBEV ) THEN
    !
    ZBLONG(:) = 0.
    ICPT = 0
    !
    DO JI = 1,KI
      DO JJ = 1,I%NPATCH
        DO JJJ = 1,I%NPATCH
          DO JL = 1,NVAR
            DO JK = 1,NVAR
              !
              L1 = JJ + I%NPATCH * (JL-1)
              !
              ICPT = ICPT + 1
              ZBLONG(ICPT) = ZB(JI,L1,L1)
              !
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    !
    ZB(:,:,:) = 0.
    CALL B_BIG_LOOP(I, &
                  "BUIL","",ZB,ZBLONG)
    IF ( NPRINTLEV > 0 ) WRITE(*,*) 'Initialized B'
    !
  ENDIF
  !
ELSE
  !
  CALL ABOR1_SFX("LBEV or LBFIXED should be .TRUE.!")
  !
ENDIF
!
IF ( LBEV ) THEN
  !
!//////////////////////TO WRITE LTM/////////////////////////////////////
  IF (NPRINTLEV>0) THEN
    IUNIT = 120
    DO JL=1,NVAR
      DO JK=1,NVAR
        IUNIT = IUNIT + 1
        WRITE(YCHAR,'(I1)') JK
        YLFNAME='LTM_del'//TRIM(CVAR(JK))//'_del'//TRIM(CVAR(JL))//"."//TRIM(YMYPROC)
        OPEN(UNIT=IUNIT,FILE=YLFNAME,FORM='FORMATTED',STATUS='UNKNOWN',POSITION='APPEND')
      ENDDO
    ENDDO
  ENDIF
!/////////////////////TO WRITE LTM////////////////////////////////////////
  DO JI = 1,KI
    !
    ! calculate LTM
    ZLTM(:,:) = 0.0
    IUNIT = 120
    DO JL = 1,NVAR    ! control variable (x at previous time step)
      DO JK = 1,NVAR 
        IUNIT = IUNIT + 1
        DO JJ = 1,I%NPATCH 
          !
          L1 = JJ + I%NPATCH*(JL-1)
          K1 = JJ + I%NPATCH*(JK-1)
          !
          IF ( I%XPATCH(JI,JJ)>0.0 .AND. XF(JI,JJ,JL+1,JK).NE.XUNDEF .AND. XF(JI,JJ,1,JK).NE.XUNDEF ) THEN
            !
            ! Jacobian of fwd model
            ZLTM(L1,K1) = ( XF(JI,JJ,JL+1,JK) - XF(JI,JJ,1,JK) ) / ZEPS(JI,JJ,JL)
            ! impose upper/lower limits 
            ZLTM(L1,K1) = MAX(-0.1, ZLTM(L1,K1))
            ZLTM(L1,K1) = MIN( 1.0, ZLTM(L1,K1))
            !
          ENDIF
          !
          IF (NPRINTLEV>0) WRITE (IUNIT,*) ZLTM(L1,K1)
          !
        ENDDO
      ENDDO
    ENDDO
    !
!//////////////////////TO WRITE LTM/////////////////////////////////////   
    IF (NPRINTLEV>0) THEN
      IUNIT = 120
      DO JL=1,NVAR
        DO JK=1,NVAR
          IUNIT = IUNIT + 1
          CLOSE(IUNIT)
        ENDDO
      ENDDO
      !//////////////////////TO WRITE LTM/////////////////////////////////////
      WRITE(*,*) 'LTM d(wg2)/d(wg2)', ZLTM(1,1)
    ENDIF
    !
    ! evolve B 
    ZB(JI,:,:) = MATMUL(ZLTM(:,:),MATMUL(ZB(JI,:,:),TRANSPOSE(ZLTM(:,:))))
    !
    !
    !   Adding model error to background error matrix 
    ZQ(:,:) = 0.0
    DO JL=1,NVAR
      DO JJ=1,I%NPATCH
        !
        L1 = JJ+I%NPATCH*(JL-1)
        !
        ZQ(L1,L1) = XSIGMA(JL)*XSIGMA(JL)
        !
        IF (TRIM(CVAR(JL)) == 'LAI') THEN
          ZQ(L1,L1) = XSCALE_QLAI*XSCALE_QLAI * ZQ(L1,L1)
        ELSE
          ZQ(L1,L1) = XSCALE_Q*XSCALE_Q * ZQ(L1,L1) * ZCOEF(JI,JJ,JL)
        ENDIF
        !
      ENDDO
    ENDDO
    !
    ! B is the forecast matrix - need to add Q
    IF ( NPRINTLEV > 0 ) THEN
      WRITE(*,*) 'B before wg2 wg2 ==> ',ZB(JI,1,1)/ZCOFSWI(JI),ZB(JI,1,1)
      WRITE(*,*) 'Q value wg2 wg2 ==> ',ZQ(1,1)/ZCOFSWI(JI),ZQ(1,1)
    ENDIF
    !
    ZB(JI,:,:) = ZB(JI,:,:) + ZQ(:,:)
    !
    IF ( NPRINTLEV > 0 ) WRITE(*,*) 'B after wg2 wg2 ==>',ZB(JI,1,1)/ZCOFSWI(1),ZB(JI,1,1)
    !
  ENDDO
  !
  ! write out the LTM for the forward model
  ! Write out current B
  IF (NPRINTLEV>0) THEN
    YBGFILE="BGROUNDout_LBEV."//TRIM(YMYPROC)
    CALL B_BIG_LOOP(I, &
                    "WRIT",YBGFILE,ZB)
    WRITE(*,*) 'store B matrix after TL evolution ==>',ZB(1,1,1)
    WRITE(*,*) 'writing out B'
  ENDIF
  !
ENDIF
!
! ====================================================================
!
! Analysis
!
! ====================================================================
!
!   Time reinitialization 
IYEAR  = I%TTIME%TDATE%YEAR
IMONTH = I%TTIME%TDATE%MONTH
IDAY   = I%TTIME%TDATE%DAY
!
IHOUR = 0
ZTIME = FLOAT(NECHGU) * 3600.
!
!############################# READS OBSERVATIONS ###############################
!
! Map the variables in case we read them from CANARI inline/offline FA files
! At the moment only T2M and HU2M can be used. If other variables should be used 
! they must be added to the interface or be read from file.
IF ( TRIM(CFILE_FORMAT_OBS) == "FA" ) THEN
  !
  DO IOBS = 1,NOBSTYPE
    SELECT CASE (TRIM(COBS(IOBS)))   
      CASE("T2M") 
        XYO(:,IOBS) = PT2M(:)
      CASE("HU2M")   
        XYO(:,IOBS) = PHU2M(:)
      CASE("WG1","LAI")  
        CALL ABOR1_SFX("Mapping of "//COBS(IOBS)//" is not defined in ASSIM_NATURE_ISBA_EKF!")
    END SELECT                 
  ENDDO
  !  
ENDIF
!
!
!//////////////////////TO WRITE OBS/////////////////////////////////////
IF ( NPRINTLEV > 0 ) OPEN (UNIT=111,FILE='OBSout.'//TRIM(YMYPROC),STATUS='unknown',IOSTAT=ISTAT)
DO JI = 1,KI
  IF ( MINVAL(I%XWGI(JI,1,:))>0. ) THEN
    XYO (JI,:) = XUNDEF
    IF ( NPRINTLEV > 0 ) WRITE(*,*) 'OBSERVATION FOR POINT ',JI,' REMOVED'
  ENDIF
  IF ( NPRINTLEV > 0 ) WRITE (111,*) XYO(JI,:)
ENDDO
IF ( NPRINTLEV > 0 ) CLOSE(111)
!//////////////////////TO WRITE OBS/////////////////////////////////////
!
!############################# ANALYSIS ###############################
!
IF ( NPRINTLEV > 0 ) THEN
  IF (NRANK==NPIO) THEN
    WRITE(*,*) 'calculating jacobians',NOBS
    WRITE(*,*) ' and then PERFORMING ANALYSIS'
  ENDIF
  !
  !//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
  ! WRITE OUT OBS AND YERROR FOR DIAGNOSTIC PURPOSES
  OPEN (UNIT=111,FILE='OBSERRORout.'//TRIM(YMYPROC),STATUS='unknown',IOSTAT=ISTAT)
  ! *** Write innovations in ASCII file ***
  OPEN (unit=112,file='INNOV.'//TRIM(YMYPROC),status='unknown',IOSTAT=ISTAT)
  ! Write analysis results and increments in ASCII file
  OPEN (unit=113,file='ANAL_INCR.'//TRIM(YMYPROC),status='unknown',IOSTAT=ISTAT)
  ! **** Write out the observation operator + Gain matrix ****
  IUNIT = 150
  DO JL = 1,NVAR
    DO JK=1,NOBSTYPE
      IUNIT = IUNIT + 1
      WRITE(YCHAR,'(I1)') JK
      YFNAME='HO_'//TRIM(CVAR(JL))//'_v'//YCHAR
      OPEN(UNIT=IUNIT,FILE=YFNAME,FORM='FORMATTED',STATUS='UNKNOWN',IOSTAT=ISTAT)
    ENDDO
  ENDDO
ENDIF
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
!
IF (I%NPATCH==12) THEN
  ZVLAIMIN = (/0.3,0.3,0.3,0.3,1.0,1.0,0.3,0.3,0.3,0.3,0.3,0.3/)
ELSE
  ZVLAIMIN = (/0.3/)
ENDIF
!
ALLOCATE(I%XINCR(KI,I%NPATCH*NVAR))
I%XINCR(:,:) = 0.
!
IOBSCOUNT = 0
DO JI=1,KI
  !
!---------------- MEAN SIMULATED OBS AVERAGED OVER TILES-----------------------
  ZYF(:,:) = 0. 
  DO JJ=1,I%NPATCH
    IF (I%XPATCH(JI,JJ) > 0.0) THEN
      WHERE ( XF_PATCH(JI,JJ,:,:)/=XUNDEF ) 
        ZYF(:,:) = ZYF(:,:) + I%XPATCH(JI,JJ)*XF_PATCH(JI,JJ,:,:)
      ENDWHERE
    ENDIF
  ENDDO
  IF ( NPRINTLEV > 0 ) WRITE(*,*) 'read in sim obs yf', ZYF(:,1)
  !
  !
  ZR   (:,:) = 0. ! Observation error matrix
  !
  ZHO  (:,:) = XUNDEF  ! Linearized observation matrix
  ZHOWR  (:,:) = XUNDEF
  ZB2  (:)   = XUNDEF  ! Innovation vector
  
  DO ISTEP=1,NBOUTPUT
    !
    DO JK = 1,NOBSTYPE
      !
      K1 = (ISTEP-1)*NOBSTYPE + JK
      !
!--------------------- SET OBSERVATION ERROR ------------------      
      ZR(K1,K1) = XERROBS(JK)*XERROBS(JK)
      IF ( COBS(JK) .EQ. "LAI" ) THEN
        ZR(K1,K1) = ZR(K1,K1) * XYO(JI,K1)*XYO(JI,K1)
      ELSEIF (COBS(JK) .EQ. "WG1") THEN
        ! convert R for wg1 from SWI  to abs value
        ZR(K1,K1) = ZR(K1,K1) * ZCOFSWI(JI)*ZCOFSWI(JI)
      ENDIF
      !
      ! Apply quality control
      IF ( ( ABS( XYO(JI,K1)-ZYF(1,JK) ) > XQCOBS(JK) ) .OR. (ZR(K1,K1) .LT. 0 ) ) THEN 
        XYO(JI,K1) = 999.0
      ENDIF
      !      
!--------------------- CALCULATE JACOBIANS ------------------         
      DO JL=1,NVAR
        DO JJ=1,I%NPATCH
          !
          L1 = JJ + I%NPATCH*(JL-1)
          !
          IF ( I%XPATCH(JI,JJ)>0.0 .AND. XF_PATCH(JI,JJ,JL+1,JK).NE.XUNDEF .AND. XF_PATCH(JI,JJ,1,JK).NE.XUNDEF ) THEN 
            ZHOWR(K1,L1) = I%XPATCH(JI,JJ)*(XF_PATCH(JI,JJ,JL+1,JK) - XF_PATCH(JI,JJ,1,JK))/ZEPS(JI,JJ,JL)
          ENDIF
          !
          IF( (XYO(JI,K1).NE.XUNDEF) .AND. (XYO(JI,K1).NE.999.0) ) THEN         !if obs available
            ! Jacobian of obs operator
            ZHO(K1,L1) = I%XPATCH(JI,JJ)*(XF_PATCH(JI,JJ,JL+1,JK) - XF_PATCH(JI,JJ,1,JK))/ZEPS(JI,JJ,JL)
            ! impose limits  
            ZHO(K1,L1) = MAX(-0.1, ZHO(K1,L1))
            ZHO(K1,L1) = MIN( 1.0, ZHO(K1,L1))
            ! innovation vector
            ZB2(K1) = XYO(JI,K1) - ZYF(1,JK)
            IOBSCOUNT = IOBSCOUNT + 1
          ELSE  !if no obs available
            ! set obs operator and innovation to zero if no obs available
            ZHO(K1,L1) = 0.0
            ZB2(K1) = 0.0 
          ENDIF
          !
        ENDDO
      ENDDO
      !
    ENDDO
    !
  ENDDO
  !
  IF ( NPRINTLEV > 0 ) THEN
    WRITE(111,*) ZR(:,:)
    WRITE(112,*) ZB2(:)
  ENDIF
  
!---------------******  SOIL ANALYSIS *******--------------------------
  ZHOT(:,:) = 0.
  ZK1(:,:) = 0.
  ZP(:) = 0.
  ZX(:) = 0.
  !
  ZHOT(:,:) = TRANSPOSE(ZHO(:,:))
  ZK1 (:,:) = MATMUL(ZHO(:,:),MATMUL(ZB(JI,:,:),ZHOT(:,:))) + ZR(:,:)
  CALL CHOLDC(NOBSTYPE,ZK1(:,:),ZP(:))                         ! Cholesky decomposition (1)
  CALL CHOLSL(NOBSTYPE,ZK1(:,:),ZP(:),ZB2(:),ZX(:))            ! Cholesky decomposition (2)
  I%XINCR(JI,:) = MATMUL(ZB(JI,:,:),MATMUL(ZHOT(:,:),ZX(:)))
  DO JL=1,NVAR
    DO JJ=1,I%NPATCH
      !
      L1 = JJ+I%NPATCH*(JL-1)
      !
      ! Update the modified values
      IF ( TRIM(CVAR(JL))=="LAI" ) THEN
        I%XINCR(JI,L1) = MAX( I%XINCR(JI,L1), ZVLAIMIN(JJ)-XF(JI,JJ,1,JL) )
        XBIO_PASS(JI,JJ) = XBIO_PASS(JI,JJ) + I%XINCR(JI,L1)*XALPH(JJ)
      ELSEIF ( XF(JI,JJ,1,JL)+I%XINCR(JI,L1)<0. ) THEN
        I%XINCR(JI,L1) = 0.
      ENDIF
      !
      XF(JI,JJ,1,JL) = XF(JI,JJ,1,JL) + I%XINCR(JI,L1)
      !
      ! For no only warn if we have negative values.
      IF ( NPRINTLEV > 0 ) THEN
        IF ( XF(JI,JJ,1,JL) < 0. ) WRITE(*,*) "WARNING X<0. for ",JI,JJ," for variable ",TRIM(CVAR(JL))
      ENDIF
      !
    ENDDO
  ENDDO
  !
  IF ( NPRINTLEV > 0 ) THEN
    DO JJ=1,I%NPATCH
      WRITE(113,*) (XF(JI,JJ,1,JL),JL=1,NVAR), (I%XINCR(JI,JJ+I%NPATCH*(JL-1)),JL=1,NVAR)
    ENDDO
  ENDIF
  
!--------------------ANALYSIS OF B (FOR USE IN NEXT CYCLE)-------------------
  ! Ba = (I-KH)Bf(I-KH)t+KRKt
  ! K = BfHt{K1}**-1
  ! K1 needs PATCH dim added
  
  ZGAIN(:,:) = 0.
  ZIDKH(:,:) = 0.
  ZKRK(:,:) = 0.
  !
  ! K1 = (R+H.B.HT) (calculate inverse -> output goes to K1)
  CALL INVERSE_MATRIX(NOBS,ZK1(:,:),ZP(:))
  ZGAIN(:,:) = MATMUL(ZB(JI,:,:),MATMUL(ZHOT(:,:),ZK1(:,:)))
  ZIDKH(:,:) = ZIDENT(:,:) - MATMUL(ZGAIN(:,:),ZHO(:,:))
  ZKRK (:,:) = MATMUL(ZGAIN(:,:),MATMUL(ZR(:,:),TRANSPOSE(ZGAIN(:,:))))
  IF (.NOT.LBFIXED)  ZB(JI,:,:) = MATMUL(ZIDKH(:,:),MATMUL(ZB(JI,:,:),TRANSPOSE(ZIDKH(:,:)))) + ZKRK(:,:)
  
  IF ( NPRINTLEV > 0 ) THEN
    IUNIT = 150
    DO JL = 1,NVAR
      DO JK = 1,NOBSTYPE
        IUNIT = IUNIT + 1
        DO JJ=1,I%NPATCH
          WRITE(IUNIT,*) ZHOWR(JK,JJ+I%NPATCH*(JL-1)),ZGAIN(JJ+I%NPATCH*(JL-1),JK)
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  !  
ENDDO
!
!
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
IF ( NPRINTLEV > 0 ) THEN
  CLOSE(111)
  CLOSE(112)
  CLOSE(113)
  IUNIT = 150
  DO JL = 1,NVAR
    DO JK = 1,NOBSTYPE
      IUNIT = IUNIT + 1
      CLOSE(IUNIT)
    ENDDO
  ENDDO
ENDIF
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
!
IF (LBEV .OR. NPRINTLEV>0) THEN
  ! Write out analysed B (for use in next cycle)
  YBGFILE = "BGROUNDout_ASSIM."//TRIM(YMYPROC)
  CALL B_BIG_LOOP(I, &
                    "WRIT",YBGFILE,ZB)
ENDIF
!
IF ( NPRINTLEV > 0 ) THEN
  IOBSCOUNT = IOBSCOUNT / I%NPATCH / NVAR
  IF (NRANK==NPIO) THEN
    WRITE(*,*)
    WRITE(*,*) '   ---------------------------------------'
    WRITE(*,*) '   |   EXITING VARASSIM AFTER ANALYSIS   |'
    WRITE(*,*) '   ---------------------------------------'
    WRITE(*,*)
  ENDIF
  WRITE(*,*) 'Number of assimilated observations =',IOBSCOUNT
  WRITE(*,*)
ENDIF
!
!############################# GET VARIABLES FOR OUTPUT WRITING ###############################
DO JL=1,NVAR
  !
  ! Update the modified values
  SELECT CASE (TRIM(CVAR(JL)))
    CASE("TG1")
      I%XTG(:,1,:) = XF(:,:,1,JL)
    CASE("TG2")
      I%XTG(:,2,:) = XF(:,:,1,JL)
    CASE("WG1")
      I%XWG(:,1,:) = XF(:,:,1,JL)
    CASE("WG2")
      I%XWG(:,2,:) = XF(:,:,1,JL)
    CASE("LAI") 
      I%XLAI(:,:) = XF(:,:,1,JL)
      SELECT CASE (TRIM(CBIO))
        CASE("BIOMA1","BIOMASS1")
          I%XBIOMASS(:,1,:) = XBIO_PASS(:,:)
        CASE("BIOMA2","BIOMASS2")
          I%XBIOMASS(:,2,:) = XBIO_PASS(:,:)
        CASE("RESPI1","RESP_BIOM1")
          I%XRESP_BIOMASS(:,1,:) = XBIO_PASS(:,:)
        CASE("RESPI2","RESP_BIOM2")
          I%XRESP_BIOMASS(:,2,:) = XBIO_PASS(:,:)
        CASE("LAI")
          I%XLAI(:,:) = XBIO_PASS(:,:)
        CASE DEFAULT
          CALL ABOR1_SFX("Mapping of "//CBIO//" is not defined in EKF!")
      END SELECT
    CASE DEFAULT
      CALL ABOR1_SFX("Mapping of "//TRIM(CVAR(JL))//" is not defined in EKF!")
  END SELECT
ENDDO
!
#endif
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_EKF',1,ZHOOK_HANDLE)
!
END SUBROUTINE ASSIM_NATURE_ISBA_EKF
