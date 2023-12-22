!     #########################
SUBROUTINE READ_CSVDATA_COMPO_TEB(BDD, HPROGRAM, HFILE)
  !     #########################
  !
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
  !!    R. Schoetter        Meteo-France
  !!
  !!    MODIFICATION
  !!    ------------
  !!
  !
  !----------------------------------------------------------------------------
  !
  !*    0.     DECLARATIONS
  !            -----------
  !
  USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
  !
  USE MODD_CSTS, ONLY : XSURF_EPSILON
  USE MODD_SURF_PAR, ONLY : NFILENAMELGTMAX, XUNDEF, NUNDEF
  !
  USE MODI_OPEN_NAMELIST
  USE MODI_CLOSE_NAMELIST
  !
  USE MODI_GET_LUOUT
  USE MODI_ABOR1_SFX
  !
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  !
  IMPLICIT NONE
  !
  !*    0.1    Declaration of arguments
  !            ------------------------
  !
  TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
  CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
  CHARACTER(LEN=NFILENAMELGTMAX), INTENT(IN) :: HFILE    ! file to read
  !
  !*    0.2    Declaration of local variables
  !      ------------------------------
  !
  INTEGER :: NAUX    ! Auxiliary variable
  INTEGER :: ILUNAM  ! Unit of the file
  INTEGER :: IFLAG_SCHED ! Flag for read of schedules
  INTEGER :: IFLAG_HOLID ! Flag for read of holidays
  INTEGER :: IFLAG_BLDOC ! Flag for read of building occupation
  INTEGER :: IFLAG_THAVG ! Flag for read of average heating desing temperature
  INTEGER :: IFLAG_THMOD ! Flag for read of modulation of heating desing temperature
  INTEGER :: IFLAG_CONDP ! Flag for read of conditional probabilities
  INTEGER :: IFLAG_TCOOL ! Flag for read of cooling design temperature
  INTEGER :: IFLAG_QINHE ! Flag for read of internal heat release
  INTEGER :: IFLAG_SHADV ! Flag for read of shading and ventilation
  !
  INTEGER :: ILUOUT  ! Unit of the output listing file
  INTEGER :: KK,LL,MM,II,OO
  !
  CHARACTER(LEN=2000) :: YSTRING ! One line of .csv file
  CHARACTER(LEN=2000) :: YSTREAD ! One line of .csv file
  !
  CHARACTER(LEN=50), ALLOCATABLE :: CUSEVCHECK(:)
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  ! -------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_COMPO_TEB',0,ZHOOK_HANDLE)
  !-------------------------------------------------------------------------------
  IF (LEN_TRIM(HFILE)==0) THEN
     IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_COMPO_TEB',1,ZHOOK_HANDLE)
     RETURN
  ENDIF
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  !
  ! Initialise flags
  !
  IFLAG_SCHED=0
  IFLAG_HOLID=0
  IFLAG_BLDOC=0
  IFLAG_THAVG=0
  IFLAG_THMOD=0
  IFLAG_CONDP=0
  IFLAG_TCOOL=0
  IFLAG_QINHE=0
  IFLAG_SHADV=0
  !
  ALLOCATE(CUSEVCHECK(BDD%NDESC_USE))
  !
  ! Open .csv-file
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
  !
  ! Loop over lines in .csv-file
  !
  DO KK=1,1000
     !
     READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
     !
     ! Loop over all entries in one line
     !
     DO LL=1,1000
        !
        YSTREAD=YSTRING(1:(INDEX(YSTRING,",")-1))
        YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
        !
        ! #####################################################
        ! Read schedules
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of schedules") THEN
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           DO OO=1,1000
              YSTREAD=YSTRING(1:(INDEX(YSTRING,",")-1))
              IF (YSTREAD.EQ."Number of distinguished days") THEN
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 READ(YSTREAD,*) BDD%NDESC_NDAY_SCHED
              ENDIF
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
           ENDDO
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           DO OO=1,1000
              YSTREAD=YSTRING(1:(INDEX(YSTRING,",")-1))
              IF (YSTREAD.EQ."Number of schedules per day") THEN
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 READ(YSTREAD,*) BDD%NDESC_NCRE_SCHED
              ENDIF
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
           ENDDO
           !
           ALLOCATE(BDD%XDESC_DAYWBEG_SCHED(BDD%NDESC_USE,BDD%NDESC_NDAY_SCHED))
           ALLOCATE(BDD%XDESC_HOURBEG_SCHED(BDD%NDESC_USE,BDD%NDESC_NDAY_SCHED*BDD%NDESC_NCRE_SCHED))
           !
           BDD%XDESC_DAYWBEG_SCHED(:,:) = XUNDEF
           BDD%XDESC_HOURBEG_SCHED(:,:) = XUNDEF
           CUSEVCHECK(:)                = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
              !
              CUSEVCHECK(MM) = TRIM(YSTREAD)
              !
              DO II=1,BDD%NDESC_NDAY_SCHED
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 READ(YSTREAD,*) NAUX
                 BDD%XDESC_DAYWBEG_SCHED(MM,II) = NAUX
                 !
                 DO OO=1,BDD%NDESC_NCRE_SCHED
                    !
                    YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                    IF (INDEX(YSTRING,",").EQ.0) YSTREAD=TRIM(YSTRING)
                    YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                    !
                    READ(YSTREAD,*) BDD%XDESC_HOURBEG_SCHED(MM,((II-1)*BDD%NDESC_NCRE_SCHED+OO))
                    !
                 ENDDO
              ENDDO
              !
              DO OO=1,BDD%NDESC_NDAY_SCHED
                 IF ((BDD%XDESC_DAYWBEG_SCHED(MM,OO).LT.0.999).OR.(BDD%XDESC_DAYWBEG_SCHED(MM,OO).GT.07.001)) THEN
                    CALL ABOR1_SFX('Wrong schedule')
                 ENDIF
              ENDDO
              !
              DO OO=1,(BDD%NDESC_NDAY_SCHED*BDD%NDESC_NCRE_SCHED)
                 IF ((BDD%XDESC_HOURBEG_SCHED(MM,OO).LT.-0.01).OR.(BDD%XDESC_HOURBEG_SCHED(MM,OO).GT.24.001)) THEN
                    CALL ABOR1_SFX('Wrong schedule')
                 ENDIF
              ENDDO
              !
           ENDDO
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of schedules')
              ENDIF
           ENDDO
           !
           IFLAG_SCHED = 1
           !
        ENDIF
        !
        ! #####################################################
        ! Read information related to holidays
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of holiday periods") THEN
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           DO OO=1,1000
              YSTREAD=YSTRING(1:(INDEX(YSTRING,",")-1))
              IF (YSTREAD.EQ."Number of holiday periods") THEN
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 READ(YSTREAD,*) BDD%NDESC_HOLIDAY
              ENDIF
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
           ENDDO
           !
           ALLOCATE(BDD%XDESC_MOD_HOLIDAY(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_BEG_HOLIDAY(BDD%NDESC_USE,BDD%NDESC_HOLIDAY))
           ALLOCATE(BDD%XDESC_END_HOLIDAY(BDD%NDESC_USE,BDD%NDESC_HOLIDAY))
           !
           BDD%XDESC_MOD_HOLIDAY(:)   = XUNDEF
           BDD%XDESC_BEG_HOLIDAY(:,:) = NUNDEF
           BDD%XDESC_END_HOLIDAY(:,:) = NUNDEF
           CUSEVCHECK(:)              = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
              !
              CUSEVCHECK(MM) = TRIM(YSTREAD)
              !
              DO II=1,BDD%NDESC_HOLIDAY
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 READ(YSTREAD,*) BDD%XDESC_BEG_HOLIDAY(MM,II)
                 !
                 IF ((BDD%XDESC_BEG_HOLIDAY(MM,II).LT.0).OR.(BDD%XDESC_BEG_HOLIDAY(MM,II).GT.401)) THEN
                    CALL ABOR1_SFX('Wrong schedule for holidays')
                 ENDIF
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 READ(YSTREAD,*) BDD%XDESC_END_HOLIDAY(MM,II)
                 !
                 IF ((BDD%XDESC_END_HOLIDAY(MM,II).LT.0).OR.(BDD%XDESC_END_HOLIDAY(MM,II).GT.401)) THEN
                    CALL ABOR1_SFX('Wrong schedule for holidays')
                 ENDIF
                 !
              ENDDO
              !
              YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
              !
              READ(YSTREAD,*) BDD%XDESC_MOD_HOLIDAY(MM)
              !
              IF ((BDD%XDESC_MOD_HOLIDAY(MM).LT.-0.00001).OR.(BDD%XDESC_MOD_HOLIDAY(MM).GT.1.0001)) THEN
                 CALL ABOR1_SFX('Wrong modulation factor for holidays')
              ENDIF
              !
           ENDDO
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of holidays')
              ENDIF
           ENDDO
           !
           IFLAG_HOLID = 1          
           !
        ENDIF
        !
        ! #####################################################
        ! Read building occupation probabilities
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of building occupation") THEN
           !
           ALLOCATE(BDD%XDESC_PROBOCC(BDD%NDESC_USE,BDD%NDESC_NCRE_SCHED*BDD%NDESC_NDAY_SCHED))
           !
           BDD%XDESC_PROBOCC(:,:) = XUNDEF
           CUSEVCHECK(:)          = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,(BDD%NDESC_NCRE_SCHED*BDD%NDESC_NDAY_SCHED+1)
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.1) CUSEVCHECK(MM) = TRIM(YSTREAD)
                 IF (II.GT.1) READ(YSTREAD,*) BDD%XDESC_PROBOCC(MM,II-1)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_USE
              DO II=1,(BDD%NDESC_NDAY_SCHED*BDD%NDESC_NCRE_SCHED)
                 IF ((BDD%XDESC_PROBOCC(MM,II).LT.-0.001).OR.(BDD%XDESC_PROBOCC(MM,II).GT.1.001)) THEN
                    CALL ABOR1_SFX('Wrong occupation probability')
                 ENDIF
              ENDDO
           ENDDO
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of building occupation')
              ENDIF
           ENDDO
           !
           IFLAG_BLDOC = 1   
           !
        ENDIF
        !
        ! #####################################################
        ! Read average design temperature for heating
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of average heating design temperature") THEN
           !
           ALLOCATE(BDD%XDESC_THEAT_OCCD_AVG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_OCCN_AVG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCDD_AVG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCDN_AVG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCLD_AVG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FNOHEAT_AVG   (BDD%NDESC_USE))
           !
           BDD%XDESC_THEAT_OCCD_AVG(:) = XUNDEF
           BDD%XDESC_THEAT_OCCN_AVG(:) = XUNDEF
           BDD%XDESC_THEAT_VCDD_AVG(:) = XUNDEF
           BDD%XDESC_THEAT_VCDN_AVG(:) = XUNDEF
           BDD%XDESC_THEAT_VCLD_AVG(:) = XUNDEF
           BDD%XDESC_FNOHEAT_AVG(:)    = XUNDEF
           CUSEVCHECK(:)               = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,7
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.1) CUSEVCHECK(MM) = TRIM(YSTREAD)
                 IF (II.EQ.2) READ(YSTREAD,*) BDD%XDESC_THEAT_OCCD_AVG(MM)
                 IF (II.EQ.3) READ(YSTREAD,*) BDD%XDESC_THEAT_OCCN_AVG(MM)
                 IF (II.EQ.4) READ(YSTREAD,*) BDD%XDESC_THEAT_VCDD_AVG(MM)
                 IF (II.EQ.5) READ(YSTREAD,*) BDD%XDESC_THEAT_VCDN_AVG(MM)
                 IF (II.EQ.6) READ(YSTREAD,*) BDD%XDESC_THEAT_VCLD_AVG(MM)
                 IF (II.EQ.7) READ(YSTREAD,*) BDD%XDESC_FNOHEAT_AVG(MM)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_USE
              !
              IF ( (BDD%XDESC_THEAT_OCCD_AVG(MM).LT.-100.0).OR.(BDD%XDESC_THEAT_OCCD_AVG(MM).GT.60.0).OR. &
                   (BDD%XDESC_THEAT_OCCN_AVG(MM).LT.-100.0).OR.(BDD%XDESC_THEAT_OCCN_AVG(MM).GT.60.0).OR. &
                   (BDD%XDESC_THEAT_VCDD_AVG(MM).LT.-100.0).OR.(BDD%XDESC_THEAT_VCDD_AVG(MM).GT.60.0).OR. &
                   (BDD%XDESC_THEAT_VCDN_AVG(MM).LT.-100.0).OR.(BDD%XDESC_THEAT_VCDN_AVG(MM).GT.60.0).OR. &
                   (BDD%XDESC_THEAT_VCLD_AVG(MM).LT.-100.0).OR.(BDD%XDESC_THEAT_VCLD_AVG(MM).GT.60.0)) THEN
                 CALL ABOR1_SFX('Wrong heating design temperature')
              ENDIF
              !
              IF ((BDD%XDESC_FNOHEAT_AVG(MM).LT.0.0).OR.(BDD%XDESC_FNOHEAT_AVG(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong non heated fraction')
              ENDIF
              !
           ENDDO
           !
           ! Conversion °C -> K
           !
           BDD%XDESC_THEAT_OCCD_AVG(:) = BDD%XDESC_THEAT_OCCD_AVG(:) + 273.16
           BDD%XDESC_THEAT_OCCN_AVG(:) = BDD%XDESC_THEAT_OCCN_AVG(:) + 273.16
           BDD%XDESC_THEAT_VCDD_AVG(:) = BDD%XDESC_THEAT_VCDD_AVG(:) + 273.16
           BDD%XDESC_THEAT_VCDN_AVG(:) = BDD%XDESC_THEAT_VCDN_AVG(:) + 273.16
           BDD%XDESC_THEAT_VCLD_AVG(:) = BDD%XDESC_THEAT_VCLD_AVG(:) + 273.16
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of heating design temperature')
              ENDIF
           ENDDO
           !
           IFLAG_THAVG = 1
           !
        ENDIF
        !
        ! ############################################################################
        ! Read modulation of design temperature for heating by behavioural indicators
        ! ############################################################################
        !
        IF (YSTREAD.EQ."Definition of heating design temperature modulation by behavioural indicators") THEN
           !
           ALLOCATE(BDD%XDESC_THEAT_OCCD_MOD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_OCCN_MOD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCDD_MOD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCDN_MOD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_THEAT_VCLD_MOD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FNOHEAT_MOD   (BDD%NDESC_USE))
           !
           BDD%XDESC_THEAT_OCCD_MOD(:) = XUNDEF
           BDD%XDESC_THEAT_OCCN_MOD(:) = XUNDEF
           BDD%XDESC_THEAT_VCDD_MOD(:) = XUNDEF
           BDD%XDESC_THEAT_VCDN_MOD(:) = XUNDEF
           BDD%XDESC_THEAT_VCLD_MOD(:) = XUNDEF
           BDD%XDESC_FNOHEAT_MOD(:)    = XUNDEF
           CUSEVCHECK(:)               = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,7
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.1) CUSEVCHECK(MM) = TRIM(YSTREAD)
                 IF (II.EQ.2) READ(YSTREAD,*) BDD%XDESC_THEAT_OCCD_MOD(MM)
                 IF (II.EQ.3) READ(YSTREAD,*) BDD%XDESC_THEAT_OCCN_MOD(MM)
                 IF (II.EQ.4) READ(YSTREAD,*) BDD%XDESC_THEAT_VCDD_MOD(MM)
                 IF (II.EQ.5) READ(YSTREAD,*) BDD%XDESC_THEAT_VCDN_MOD(MM)
                 IF (II.EQ.6) READ(YSTREAD,*) BDD%XDESC_THEAT_VCLD_MOD(MM)
                 IF (II.EQ.7) READ(YSTREAD,*) BDD%XDESC_FNOHEAT_MOD(MM)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check entries
           !
           DO MM=1,BDD%NDESC_USE
              !
              IF ( (ABS(BDD%XDESC_THEAT_OCCD_MOD(MM)).GT.5.0).OR. &
                   (ABS(BDD%XDESC_THEAT_OCCN_MOD(MM)).GT.5.0).OR. &
                   (ABS(BDD%XDESC_THEAT_VCDD_MOD(MM)).GT.5.0).OR. &
                   (ABS(BDD%XDESC_THEAT_VCDN_MOD(MM)).GT.5.0).OR. &
                   (ABS(BDD%XDESC_THEAT_VCLD_MOD(MM)).GT.5.0)) THEN
                 CALL ABOR1_SFX('Wrong heating design temperature modulation')
              ENDIF
              !
              IF (ABS(BDD%XDESC_FNOHEAT_MOD(MM)).GT.0.1) CALL ABOR1_SFX("Wrong non heated fraction modulation")
              !
           ENDDO
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of heating desing temperature')
              ENDIF
           ENDDO
           !
           IFLAG_THMOD = 1
           !
        ENDIF
        !
        ! ###########################################################################
        ! Read conditional probabilities for heating to different design temperatures
        ! ###########################################################################
        !
        IF (YSTREAD.EQ."Definition of conditional probabilities for heating design temperature") THEN
           !
           ! The number and positions are hardcoded to 2 for individual and collective housing respectively
           !
           BDD%NDESC_CONDP=2*2
           !
           BDD%NDESC_POS_HAI_FORTCRE = 1
           BDD%NDESC_POS_HAI_FAIBCRE = 2
           BDD%NDESC_POS_HAC_FORTCRE = 3
           BDD%NDESC_POS_HAC_FAIBCRE = 4
           !
           ALLOCATE(BDD%XDESC_FLDT(BDD%NDESC_CONDP))
           ALLOCATE(BDD%XDESC_FIDT(BDD%NDESC_CONDP))
           ALLOCATE(BDD%XDESC_FHDT(BDD%NDESC_CONDP))
           !
           BDD%XDESC_FLDT(:) = XUNDEF
           BDD%XDESC_FIDT(:) = XUNDEF
           BDD%XDESC_FHDT(:) = XUNDEF
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_CONDP
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,5
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.03) READ(YSTREAD,*) BDD%XDESC_FLDT(MM)
                 IF (II.EQ.04) READ(YSTREAD,*) BDD%XDESC_FIDT(MM)
                 IF (II.EQ.05) READ(YSTREAD,*) BDD%XDESC_FHDT(MM)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_CONDP
              !
              IF ((BDD%XDESC_FLDT(MM).LT.0.0).OR.(BDD%XDESC_FLDT(MM).GT.1.0)) CALL ABOR1_SFX("Wrong conditional probability")
              IF ((BDD%XDESC_FIDT(MM).LT.0.0).OR.(BDD%XDESC_FIDT(MM).GT.1.0)) CALL ABOR1_SFX("Wrong conditional probability")
              IF ((BDD%XDESC_FHDT(MM).LT.0.0).OR.(BDD%XDESC_FHDT(MM).GT.1.0)) CALL ABOR1_SFX("Wrong conditional probability")
              !
              IF (ABS(1.0-BDD%XDESC_FLDT(MM)-BDD%XDESC_FIDT(MM)-BDD%XDESC_FHDT(MM)).GT.XSURF_EPSILON) THEN
                 CALL ABOR1_SFX('Wrong sum of conditional probabilities')
              ENDIF
              !
           ENDDO
           !
           IFLAG_CONDP = 1
           !
        ENDIF
        !
        ! #####################################################
        ! Read design temperature for cooling
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of air conditioning design temperature") THEN
           !
           ALLOCATE(BDD%XDESC_TCOOL_OCCD   (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_TCOOL_OCCN   (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_TCOOL_VCDD   (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_TCOOL_VCDN   (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_TCOOL_VCLD   (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_F_WATER_COND (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_F_WASTE_CAN  (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_COP_RAT      (BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_HR_TARGET    (BDD%NDESC_USE))
           !
           BDD%XDESC_TCOOL_OCCD(:)   = XUNDEF
           BDD%XDESC_TCOOL_OCCN(:)   = XUNDEF
           BDD%XDESC_TCOOL_VCDD(:)   = XUNDEF
           BDD%XDESC_TCOOL_VCDN(:)   = XUNDEF
           BDD%XDESC_TCOOL_VCLD(:)   = XUNDEF
           BDD%XDESC_F_WATER_COND(:) = XUNDEF
           BDD%XDESC_F_WASTE_CAN(:)  = XUNDEF
           BDD%XDESC_COP_RAT(:)      = XUNDEF
           BDD%XDESC_HR_TARGET(:)    = XUNDEF
           CUSEVCHECK(:)             = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,10
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.01) CUSEVCHECK(MM) = TRIM(YSTREAD)
                 IF (II.EQ.02) READ(YSTREAD,*) BDD%XDESC_TCOOL_OCCD(MM)
                 IF (II.EQ.03) READ(YSTREAD,*) BDD%XDESC_TCOOL_OCCN(MM)
                 IF (II.EQ.04) READ(YSTREAD,*) BDD%XDESC_TCOOL_VCDD(MM)
                 IF (II.EQ.05) READ(YSTREAD,*) BDD%XDESC_TCOOL_VCDN(MM)
                 IF (II.EQ.06) READ(YSTREAD,*) BDD%XDESC_TCOOL_VCLD(MM)
                 IF (II.EQ.07) READ(YSTREAD,*) BDD%XDESC_F_WATER_COND(MM)
                 IF (II.EQ.08) READ(YSTREAD,*) BDD%XDESC_F_WASTE_CAN(MM)
                 IF (II.EQ.09) READ(YSTREAD,*) BDD%XDESC_COP_RAT(MM)
                 IF (II.EQ.10) READ(YSTREAD,*) BDD%XDESC_HR_TARGET(MM)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_USE
              IF ( (BDD%XDESC_TCOOL_OCCD(MM).LT.0.0).OR.(BDD%XDESC_TCOOL_OCCD(MM).GT.100.0).OR. &
                   (BDD%XDESC_TCOOL_OCCN(MM).LT.0.0).OR.(BDD%XDESC_TCOOL_OCCN(MM).GT.100.0).OR. &
                   (BDD%XDESC_TCOOL_VCDD(MM).LT.0.0).OR.(BDD%XDESC_TCOOL_VCDD(MM).GT.100.0).OR. &
                   (BDD%XDESC_TCOOL_VCDN(MM).LT.0.0).OR.(BDD%XDESC_TCOOL_VCDN(MM).GT.100.0).OR. &
                   (BDD%XDESC_TCOOL_VCLD(MM).LT.0.0).OR.(BDD%XDESC_TCOOL_VCLD(MM).GT.100.0)) THEN 
                 CALL ABOR1_SFX('Wrong cooling design temperature')
              ENDIF
              !
              IF ((BDD%XDESC_F_WATER_COND(MM).LT.0.0).OR.(BDD%XDESC_F_WATER_COND(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong evaporative fraction')
              ENDIF
              !
              IF ((BDD%XDESC_F_WASTE_CAN(MM).LT.0.0).OR.(BDD%XDESC_F_WASTE_CAN(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong fraction for waste heat')
              ENDIF
              !
              IF ((BDD%XDESC_COP_RAT(MM).LT.0.0).OR.(BDD%XDESC_COP_RAT(MM).GT.10.0)) THEN
                 CALL ABOR1_SFX('Wrong COP for climatisation')
              ENDIF
              !
              IF ((BDD%XDESC_HR_TARGET(MM).LT.0.0).OR.(BDD%XDESC_HR_TARGET(MM).GT.100.0)) THEN
                 CALL ABOR1_SFX('Wrong cooling design relative humidity')
              ENDIF
              !
           ENDDO
           !
           ! Conversion °C -> K
           !
           BDD%XDESC_TCOOL_OCCD(:) = BDD%XDESC_TCOOL_OCCD(:) + 273.16
           BDD%XDESC_TCOOL_OCCN(:) = BDD%XDESC_TCOOL_OCCN(:) + 273.16
           BDD%XDESC_TCOOL_VCDD(:) = BDD%XDESC_TCOOL_VCDD(:) + 273.16
           BDD%XDESC_TCOOL_VCDN(:) = BDD%XDESC_TCOOL_VCDN(:) + 273.16
           BDD%XDESC_TCOOL_VCLD(:) = BDD%XDESC_TCOOL_VCLD(:) + 273.16
           !
           ! Conversion % -> fraction
           !
           BDD%XDESC_HR_TARGET(:) = 0.01*BDD%XDESC_HR_TARGET(:)
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of cooling design temperature')
              ENDIF
           ENDDO
           !
           IFLAG_TCOOL = 1
           !
        ENDIF
        !
        ! #####################################################
        ! Read internal heat release
        ! #####################################################
        !
        IF (YSTREAD.EQ."Definition of internal heat release") THEN
           !
           ALLOCATE(BDD%XDESC_QIN(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_QIN_ADDBEHAV(BDD%NDESC_USE))           
           ALLOCATE(BDD%XDESC_QIN_FRAD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_QIN_FLAT(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_MODQIN_VCD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_MODQIN_VLD(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_MODQIN_NIG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_HOTWAT(BDD%NDESC_USE))           
           !
           BDD%XDESC_QIN(:)          = XUNDEF
           BDD%XDESC_QIN_ADDBEHAV(:) = XUNDEF          
           BDD%XDESC_QIN_FRAD(:)     = XUNDEF
           BDD%XDESC_QIN_FLAT(:)     = XUNDEF
           BDD%XDESC_MODQIN_VCD(:)   = XUNDEF
           BDD%XDESC_MODQIN_VLD(:)   = XUNDEF
           BDD%XDESC_MODQIN_NIG(:)   = XUNDEF
           BDD%XDESC_HOTWAT(:)       = XUNDEF           
           CUSEVCHECK(:)             = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,9
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded
                 !
                 IF (II.EQ.1) CUSEVCHECK(MM) = TRIM(YSTREAD)      
                 IF (II.EQ.2) READ(YSTREAD,*) BDD%XDESC_QIN(MM)
                 IF (II.EQ.3) READ(YSTREAD,*) BDD%XDESC_QIN_ADDBEHAV(MM)                 
                 IF (II.EQ.4) READ(YSTREAD,*) BDD%XDESC_QIN_FRAD(MM)
                 IF (II.EQ.5) READ(YSTREAD,*) BDD%XDESC_QIN_FLAT(MM)
                 IF (II.EQ.6) READ(YSTREAD,*) BDD%XDESC_MODQIN_VCD(MM)
                 IF (II.EQ.7) READ(YSTREAD,*) BDD%XDESC_MODQIN_VLD(MM)
                 IF (II.EQ.8) READ(YSTREAD,*) BDD%XDESC_MODQIN_NIG(MM)
                 IF (II.EQ.9) READ(YSTREAD,*) BDD%XDESC_HOTWAT(MM)                 
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_USE
              !
              IF ((BDD%XDESC_QIN(MM).LT.0.0).OR.(BDD%XDESC_QIN(MM).GT.20.0)) THEN
                 CALL ABOR1_SFX('Wrong value for nominative heat release')
              ENDIF
              !
              IF (ABS(BDD%XDESC_QIN_ADDBEHAV(MM)).GT.5.0) THEN
                 CALL ABOR1_SFX('Wrong value for nominative heat release modulation')
              ENDIF
              !
              IF ((BDD%XDESC_QIN_FRAD(MM).LT.0.0).OR.(BDD%XDESC_QIN_FRAD(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong value for radiative fraction')
              ENDIF
              !
              IF ((BDD%XDESC_QIN_FLAT(MM).LT.0.0).OR.(BDD%XDESC_QIN_FLAT(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong value for latent fraction')
              ENDIF
              !
              IF ( (BDD%XDESC_MODQIN_VCD(MM).LT.0.0).OR.(BDD%XDESC_MODQIN_VCD(MM).GT.1.0).OR. & 
                   (BDD%XDESC_MODQIN_VLD(MM).LT.0.0).OR.(BDD%XDESC_MODQIN_VLD(MM).GT.1.0).OR. & 
                   (BDD%XDESC_MODQIN_NIG(MM).LT.0.0).OR.(BDD%XDESC_MODQIN_NIG(MM).GT.1.0) ) THEN
                 CALL ABOR1_SFX('Wrong value for modulation factor')
              ENDIF
              !
              IF ((BDD%XDESC_HOTWAT(MM).LT.0.0).OR.(BDD%XDESC_HOTWAT(MM).GT.20.0)) THEN
                 CALL ABOR1_SFX('Wrong value for residential warm water energy consumption')
              ENDIF              
              !
           ENDDO
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of internal heat release')
              ENDIF
           ENDDO
           !
           IFLAG_QINHE = 1
           !
        ENDIF
        !
        ! ###############################################################
        ! Read entries related to the use of windows and shading elements
        ! ###############################################################
        !
        IF (YSTREAD.EQ."Definition of shading and ventilation") THEN
           !
           ALLOCATE(BDD%XDESC_NATVENT(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FVSUM(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FVNIG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_TDESV(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FVVAC(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FOPEN(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FSSUM(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FSNIG(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_FSVAC(BDD%NDESC_USE))
           ALLOCATE(BDD%XDESC_WIN_SW_MAX(BDD%NDESC_USE))
           !
           BDD%XDESC_NATVENT(:)    = XUNDEF
           BDD%XDESC_FVSUM(:)      = XUNDEF
           BDD%XDESC_FVNIG(:)      = XUNDEF
           BDD%XDESC_TDESV(:)      = XUNDEF
           BDD%XDESC_FVVAC(:)      = XUNDEF
           BDD%XDESC_FOPEN(:)      = XUNDEF
           BDD%XDESC_FSSUM(:)      = XUNDEF
           BDD%XDESC_FSNIG(:)      = XUNDEF
           BDD%XDESC_FSVAC(:)      = XUNDEF
           BDD%XDESC_WIN_SW_MAX(:) = XUNDEF
           CUSEVCHECK(:)           = "DUMMY"
           !
           READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
           !
           DO MM=1,BDD%NDESC_USE
              !
              READ(ILUNAM,END=9999,FMT='(A1000)') YSTRING
              !
              DO II=1,11
                 !
                 YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
                 YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
                 !
                 ! The positions of the entries are hardcoded,
                 ! only the building use is read as additional information
                 !
                 IF (II.EQ.01) CUSEVCHECK(MM) = TRIM(YSTREAD)
                 IF (II.EQ.02) READ(YSTREAD,*) BDD%XDESC_NATVENT(MM)
                 IF (II.EQ.03) READ(YSTREAD,*) BDD%XDESC_FVSUM(MM)
                 IF (II.EQ.04) READ(YSTREAD,*) BDD%XDESC_FVNIG(MM)
                 IF (II.EQ.05) READ(YSTREAD,*) BDD%XDESC_TDESV(MM)
                 IF (II.EQ.06) READ(YSTREAD,*) BDD%XDESC_FVVAC(MM)
                 IF (II.EQ.07) READ(YSTREAD,*) BDD%XDESC_FOPEN(MM)
                 IF (II.EQ.08) READ(YSTREAD,*) BDD%XDESC_FSSUM(MM)
                 IF (II.EQ.09) READ(YSTREAD,*) BDD%XDESC_FSNIG(MM)
                 IF (II.EQ.10) READ(YSTREAD,*) BDD%XDESC_FSVAC(MM)
                 IF (II.EQ.11) READ(YSTREAD,*) BDD%XDESC_WIN_SW_MAX(MM)
                 !
              ENDDO
              !
           ENDDO
           !
           ! Check all entries
           !
           DO MM=1,BDD%NDESC_USE
              !
              IF ( (BDD%XDESC_NATVENT(MM).LT.0.0).OR.(BDD%XDESC_NATVENT(MM).GT.3.001)) THEN
                 CALL ABOR1_SFX('Wrong value for NATVENT switch')
              ENDIF
              !
              IF ( (BDD%XDESC_FVSUM(MM).LT.0.0).OR.(BDD%XDESC_FVSUM(MM).GT.1.0).OR. &
                   (BDD%XDESC_FVNIG(MM).LT.0.0).OR.(BDD%XDESC_FVNIG(MM).GT.1.0).OR. &
                   (BDD%XDESC_FVVAC(MM).LT.0.0).OR.(BDD%XDESC_FVVAC(MM).GT.1.0).OR. &
                   (BDD%XDESC_FOPEN(MM).LT.0.0).OR.(BDD%XDESC_FOPEN(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong value for ventilation use')
              ENDIF
              !
              IF ( (BDD%XDESC_FSSUM(MM).LT.0.0).OR.(BDD%XDESC_FSSUM(MM).GT.1.0).OR. &
                   (BDD%XDESC_FSNIG(MM).LT.0.0).OR.(BDD%XDESC_FSNIG(MM).GT.1.0).OR. & 
                   (BDD%XDESC_FSVAC(MM).LT.0.0).OR.(BDD%XDESC_FSVAC(MM).GT.1.0)) THEN
                 CALL ABOR1_SFX('Wrong value for shading use')
              ENDIF
              !
              IF ((BDD%XDESC_TDESV(MM).LT.15.0).OR.(BDD%XDESC_TDESV(MM).GT.35.0)) THEN
                 CALL ABOR1_SFX('Wrong value for ventilation design temperature')
              ENDIF
              !
              IF ((BDD%XDESC_WIN_SW_MAX(MM).LT.0.0).OR.(BDD%XDESC_WIN_SW_MAX(MM).GT.10000.0)) THEN
                 CALL ABOR1_SFX('Wrong value for radiation threshold')
              ENDIF
              !
           ENDDO
           !
           ! Conversion °C -> K
           !
           BDD%XDESC_TDESV(:) = BDD%XDESC_TDESV(:) + 273.16
           !
           ! Check consistancy of use names
           !
           DO MM=1,BDD%NDESC_USE
              IF (CUSEVCHECK(MM).NE.BDD%YDESC_USENAME(MM)) THEN
                 CALL GET_LUOUT(HPROGRAM,ILUOUT)
                 WRITE(ILUOUT,*) "   "
                 WRITE(ILUOUT,*) "In read_csvdata_compo:   "
                 WRITE(ILUOUT,*) "Inconsistancy between building uses from Definitions and use table"                
                 WRITE(ILUOUT,*) "   "                 
                 WRITE(ILUOUT,*) "CUSEVCHECK(MM)        ",CUSEVCHECK(MM)
                 WRITE(ILUOUT,*) "BDD%YDESC_USENAME(MM) ",BDD%YDESC_USENAME(MM)
                 CALL FLUSH(ILUOUT)
                 CALL ABOR1_SFX('READ_CSV_DATA_COMPO:Inconsistant use names in read of shading')
              ENDIF
           ENDDO
           !
           IFLAG_SHADV = 1
           !
        ENDIF
        !
        ! Exit inner loop when entire line read
        !      
        IF (INDEX(YSTRING,",").EQ.0) EXIT
        !
     ENDDO
     !
     ! Exit outer loop when all fields have been read
     ! Otherwise there will be an end of file
     !
     IF ((IFLAG_SCHED.EQ.1).AND.(IFLAG_HOLID.EQ.1).AND.(IFLAG_BLDOC.EQ.1).AND. &
         (IFLAG_THAVG.EQ.1).AND.(IFLAG_THMOD.EQ.1).AND.(IFLAG_CONDP.EQ.1).AND. &
         (IFLAG_TCOOL.EQ.1).AND.(IFLAG_QINHE.EQ.1).AND.(IFLAG_SHADV.EQ.1)) EXIT
     !
  ENDDO
  !
  ! Close file
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_COMPO_TEB',1,ZHOOK_HANDLE)
  !
  RETURN
  !
9999 CONTINUE
  !
  CALL ABOR1_SFX('End of file in read of .csv file')
  !
END SUBROUTINE READ_CSVDATA_COMPO_TEB
