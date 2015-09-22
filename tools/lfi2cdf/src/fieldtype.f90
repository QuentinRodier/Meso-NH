MODULE MODE_FIELDTYPE
  USE MODD_PARAM
  
  IMPLICIT NONE 

  PRIVATE

  
  TYPE field
     CHARACTER(LEN=FM_FIELD_SIZE) :: name ! Le nom de l'article LFI
     INTEGER                      :: TYPE ! Type :entier(INT) ou reel(FLOAT)
     INTEGER                      :: dim  ! Dimension de l'article  
  END TYPE field
  
  TYPE(field), DIMENSION(:), ALLOCATABLE :: userfield

  ! Les champs contenant %TDATE et %TIME sont traites en dur
  ! dans la routine de recherche de type
  TYPE(field), DIMENSION(2),   PARAMETER  :: datefield = (/&
               field('%TDA', INT, D0), &
               field('%TIM', FLOAT, D0) &
             /)

  TYPE(field), DIMENSION(219), SAVE  :: sysfield

  PUBLIC :: get_ftype, init_sysfield

CONTAINS 
SUBROUTINE init_sysfield()
sysfield(1) =  field('LBXSVMxxx', FLOAT , D0)
sysfield(2) =  field('LBYSVMxxx', FLOAT , D0)
sysfield(3) =  field('LBXUM', FLOAT, D0)
sysfield(4) =  field('LBYUM', FLOAT, D0)
sysfield(5) =  field('LBXVM', FLOAT, D0)
sysfield(6) =  field('LBYVM', FLOAT, D0)
sysfield(7) =  field('LBXWM', FLOAT, D0)
sysfield(8) =  field('LBYWM', FLOAT, D0)
sysfield(9) =  field('LBXTHM', FLOAT, D0)
sysfield(10) =  field('LBYTHM', FLOAT, D0)
sysfield(11) =  field('LBXRVM', FLOAT, D0)
sysfield(12) =  field('LBYRVM', FLOAT, D0)
sysfield(13) =  field('AVG_ZS', FLOAT, D0)
sysfield(14) =  field('SIL_ZS', FLOAT, D0)
sysfield(15) =  field('AOSIP', FLOAT, D0)
sysfield(16) =  field('AOSIM', FLOAT, D0)
sysfield(17) =  field('AOSJP', FLOAT, D0)
sysfield(18) =  field('AOSJM', FLOAT, D0)
sysfield(19) =  field('HO2IP', FLOAT, D0)
sysfield(20) =  field('HO2IM', FLOAT, D0)
sysfield(21) =  field('HO2JP', FLOAT, D0)
sysfield(22) =  field('HO2JM', FLOAT, D0)
sysfield(23) =  field('RIMX',INT, D0)
sysfield(24) =  field('RIMY',INT, D0)
sysfield(25) =  field('HORELAX_UVWTH',BOOL, D0)
sysfield(26) =  field('HORELAX_R',BOOL, D0)
sysfield(27) =  field('I2D_XY', INT, D0)
sysfield(28) =  field('MENU_BUDGET',TEXT, D0)
sysfield(29) =  field('IE', INT, D0)
sysfield(30) =  field('ZR', FLOAT, D0)
sysfield(31) =  field('GOK', BOOL, D0)
sysfield(32) =  field('YTEXT', TEXT, D0)
sysfield(33) =  field('X1D', FLOAT, D0)
sysfield(34) =  field('I1D', INT, D0)
sysfield(35) =  field('DEB', INT, D0)
sysfield(36) =  field('3D1', FLOAT, D0)
sysfield(37) =  field('3D2', FLOAT, D0)
sysfield(38) =  field('3D3', FLOAT, D0)
sysfield(39) =  field('3D4', FLOAT, D0)
sysfield(40) =  field('3D5', FLOAT, D0)
sysfield(41) =  field('RHODREFZ', FLOAT, D0)
sysfield(42) =  field('RSVS', FLOAT, D0)
sysfield(43) =  field('RUS', FLOAT, D0)
sysfield(44) =  field('MY_NAME', TEXT, D0)
sysfield(45) =  field('DAD_NAME', TEXT, D0)
sysfield(46) =  field('STORAGE_TYPE', TEXT, D0)
sysfield(47) =  field('IMAX', INT, D0)
sysfield(48) =  field('JMAX', INT, D0)
sysfield(49) =  field('KMAX', INT, D0)
sysfield(50) =  field('RPK', FLOAT, D0)
sysfield(51) =  field('NEB', FLOAT , D0)
sysfield(52) =  field('LONOR', FLOAT, D0)
sysfield(53) =  field('LATOR', FLOAT, D0)
sysfield(54) =  field('THINSHELL', BOOL, D0)
sysfield(55) =  field('LAT0', FLOAT, D0)
sysfield(56) =  field('LON0', FLOAT, D0)
sysfield(57) =  field('BETA', FLOAT, D0)
sysfield(58) =  field('XHAT', FLOAT, D0)
sysfield(59) =  field('YHAT', FLOAT, D0)
sysfield(60) =  field('ZHAT', FLOAT, D0)
sysfield(61) =  field('ZS', FLOAT, D0)
sysfield(62) =  field('CARTESIAN', BOOL, D0)
sysfield(63) =  field('UM', FLOAT, D0)
sysfield(64) =  field('VM', FLOAT, D0)
sysfield(65) =  field('WM', FLOAT, D0)
sysfield(66) =  field('THM', FLOAT, D0)
sysfield(67) =  field('TKEM', FLOAT, D0)
sysfield(68) =  field('EPSM', FLOAT, D0)
sysfield(69) =  field('PABSM',FLOAT, D0)
sysfield(70) =  field('RVM', FLOAT, D0)
sysfield(71) =  field('RCM', FLOAT, D0)
sysfield(72) =  field('RRM', FLOAT, D0)
sysfield(73) =  field('RIM', FLOAT, D0)
sysfield(74) =  field('RSM', FLOAT, D0)
sysfield(75) =  field('RGM', FLOAT, D0)
sysfield(76) =  field('RHM', FLOAT, D0)
sysfield(77) =  field('SVMxxx', FLOAT, D0)
sysfield(78) =  field('LSUM', FLOAT, D0)
sysfield(79) =  field('LSVM', FLOAT, D0)
sysfield(80) =  field('LSWM',FLOAT , D0)
sysfield(81) =  field('LSTHM',FLOAT, D0)
sysfield(82) =  field('LSRVM',FLOAT, D0)
sysfield(83) =  field('LSXTKEM',FLOAT, D0)
sysfield(84) =  field('LSYTKEM',FLOAT, D0)
sysfield(85) =  field('LSXEPSM',FLOAT, D0)
sysfield(86) =  field('LSYEPSM',FLOAT, D0)
sysfield(87) =  field('LSXRCM',FLOAT , D0)
sysfield(88) =  field('LSYRCM', FLOAT, D0)
sysfield(89) =  field('LSXRRM', FLOAT, D0)
sysfield(90) =  field('LSYRRM', FLOAT, D0)
sysfield(91) =  field('LSXRIM', FLOAT, D0)
sysfield(92) =  field('LSYRIM', FLOAT, D0)
sysfield(93) =  field('LSXRSM', FLOAT, D0)
sysfield(94) =  field('LSYRSM', FLOAT, D0)
sysfield(95) =  field('LSXRGM', FLOAT, D0)
sysfield(96) =  field('LSYRGM', FLOAT, D0)
sysfield(97) =  field('LSXRHM', FLOAT, D0)
sysfield(98) =  field('LSYRHM', FLOAT, D0)
sysfield(99) =  field('LSXSVMxxx', FLOAT, D0)
sysfield(100) =  field('LSYSVMxxx', FLOAT, D0)
sysfield(101) =  field('UT',FLOAT, D0)
sysfield(102) =  field('VT',FLOAT, D0)
sysfield(103) =  field('WT',FLOAT, D0)
sysfield(104) =  field('THT',FLOAT, D0)
sysfield(105) =  field('TKET',FLOAT, D0)
sysfield(106) =  field('EPST',FLOAT, D0)
sysfield(107) =  field('PABST',FLOAT, D0)
sysfield(108) =  field('RVT',FLOAT, D0)
sysfield(109) =  field('RCT',FLOAT, D0)
sysfield(110) =  field('RRT',FLOAT, D0)
sysfield(111) =  field('RIT',FLOAT, D0)
sysfield(112) =  field('CIT',FLOAT, D0)
sysfield(113) =  field('RST',FLOAT, D0)
sysfield(114) =  field('RGT',FLOAT, D0)
sysfield(115) =  field('RHT',FLOAT, D0)
sysfield(116) =  field('SVTxxx',FLOAT, D0)
sysfield(117) =  field('DRYMASST',FLOAT, D0)
sysfield(118) =  field('SRCM',FLOAT, D0)
sysfield(119) =  field('SRCT',FLOAT, D0)
sysfield(120) =  field('SIGS',FLOAT, D0)
sysfield(121) =  field('RHOREFZ',FLOAT, D0)
sysfield(122) =  field('THVREFZ',FLOAT, D0)
sysfield(123) =  field('EXNTOP',FLOAT, D0)
sysfield(124) =  field('RESA', FLOAT , D0)
sysfield(125) =  field('Z0SEA', FLOAT , D0)
sysfield(126) =  field('TS', FLOAT , D0)
sysfield(127) =  field('WG', FLOAT , D0)
sysfield(128) =  field('SST', FLOAT , D0)
sysfield(129) =  field('T2', FLOAT , D0)
sysfield(130) =  field('W2', FLOAT , D0)
sysfield(131) =  field('WR', FLOAT , D0)
sysfield(132) =  field('WS', FLOAT , D0)
sysfield(133) =  field('ALBS', FLOAT , D0)
sysfield(134) =  field('RHOS', FLOAT , D0)
sysfield(135) =  field('LAND', FLOAT , D0)
sysfield(136) =  field('SEA', FLOAT , D0)
sysfield(137) =  field('Z0VEG', FLOAT , D0)
sysfield(138) =  field('Z0HVEG', FLOAT , D0)
sysfield(139) =  field('Z0REL', FLOAT , D0)
sysfield(140) =  field('Z0EFFIP', FLOAT , D0)
sysfield(141) =  field('Z0EFFIM', FLOAT , D0)
sysfield(142) =  field('Z0EFFJP', FLOAT , D0)
sysfield(143) =  field('Z0EFFJM', FLOAT , D0)
sysfield(144) =  field('SSO_STDEV', FLOAT , D0)
sysfield(145) =  field('SSO_ANIS', FLOAT , D0)
sysfield(146) =  field('SSO_DIRECTION', FLOAT , D0)
sysfield(147) =  field('SSO_SLOPE', FLOAT , D0)
sysfield(148) =  field('ALBVIS', FLOAT , D0)
sysfield(149) =  field('ALBNIR', FLOAT , D0)
sysfield(150) =  field('EMIS', FLOAT , D0)
sysfield(151) =  field('CLAY', FLOAT , D0)
sysfield(152) =  field('SAND', FLOAT , D0)
sysfield(153) =  field('D2', FLOAT , D0)
sysfield(154) =  field('VEG', FLOAT , D0)
sysfield(155) =  field('LAI', FLOAT , D0)
sysfield(156) =  field('RSMIN', FLOAT , D0)
sysfield(157) =  field('GAMMA', FLOAT , D0)
sysfield(158) =  field('RGL', FLOAT , D0)
sysfield(159) =  field('CV', FLOAT , D0)
sysfield(160) =  field('SFTHT', FLOAT , D0)
sysfield(161) =  field('SFTHP', FLOAT , D0)
sysfield(162) =  field('SFRT', FLOAT , D0)
sysfield(163) =  field('SFRP', FLOAT , D0)
sysfield(164) =  field('SFSVT', FLOAT , D0)
sysfield(165) =  field('SFSVP', FLOAT , D0)
sysfield(166) =  field('DTHRAD', FLOAT , D0)
sysfield(167) =  field('SRFLWD', FLOAT , D0)
sysfield(168) =  field('SRFSWD', FLOAT , D0)
sysfield(169) =  field('CLDFR', FLOAT , D0)
sysfield(170) =  field('COUNTCONV', INT , D0)
sysfield(171) =  field('DTHCONV', FLOAT , D0)
sysfield(172) =  field('DRVCONV', FLOAT , D0)
sysfield(173) =  field('DRCCONV', FLOAT , D0)
sysfield(174) =  field('DRICONV', FLOAT , D0)
sysfield(175) =  field('PRCONV', FLOAT , D0)
sysfield(176) =  field('PACCONV', FLOAT , D0)
sysfield(177) =  field('WSUBCONV', FLOAT , D0)
sysfield(178) =  field('INPRR', FLOAT , D0)
sysfield(179) =  field('ACPRR', FLOAT , D0)
sysfield(180) =  field('INPRS', FLOAT , D0)
sysfield(181) =  field('ACPRS', FLOAT , D0)
sysfield(182) =  field('INPRG', FLOAT , D0)
sysfield(183) =  field('ACPRG', FLOAT , D0)
sysfield(184) =  field('INPRT', FLOAT , D0)
sysfield(185) =  field('ACPRT', FLOAT , D0)
sysfield(186) =  field('FRC', INT, D0)
sysfield(187) =  field('UFRCxx', FLOAT , D0)
sysfield(188) =  field('VFRCxx', FLOAT , D0)
sysfield(189) =  field('WFRCxx', FLOAT , D0)
sysfield(190) =  field('THFRCxx', FLOAT , D0)
sysfield(191) =  field('RVFRCxx', FLOAT , D0)
sysfield(192) =  field('GXRVFRCxx', FLOAT , D0)
sysfield(193) =  field('GYRVFRCxx', FLOAT , D0)
sysfield(194) =  field('GXTHFRCxx', FLOAT , D0)
sysfield(195) =  field('GYTHFRCxx', FLOAT , D0)
sysfield(196) =  field('DUMMY_GRxxx', FLOAT , D0)
sysfield(197) =  field('MASDEV', INT , D0)
sysfield(198) =  field('EMISFILE_GR_NBR', INT , D0)
sysfield(199) =  field('EMISPEC_GR_NBR', INT , D0)
sysfield(200) =  field('EMISNAMExxx', TEXT , D0)
sysfield(201) =  field('EMISTIMESxxx', INT , D0)
sysfield(202) =  field('DUMMY_GR_NBR', INT , D0)
sysfield(203) =  field('COVERxxx', FLOAT , D0)
sysfield(204) =  field('TGx', FLOAT, D0)
sysfield(205) =  field('T_ROOFx', FLOAT, D0)
sysfield(206) =  field('T_ROADx', FLOAT, D0)
sysfield(207) =  field('T_WALLx', FLOAT, D0)
sysfield(208) =  field('WGx', FLOAT, D0)
sysfield(209) =  field('WGIx', FLOAT, D0)
sysfield(210) =  field('MAX_ZS', FLOAT, D0)
sysfield(211) =  field('MIN_ZS', FLOAT, D0)
sysfield(212) =  field('XOR', INT, D0)
sysfield(213) =  field('YOR', INT, D0)
sysfield(214) =  field('DXRATIO', INT, D0)
sysfield(215) =  field('DYRATIO', INT, D0)
sysfield(216) =  field('PATCH_NUMBER', INT, D0)
sysfield(217) =  field('BUGFIX', INT, D0)
sysfield(218) =  field('BIBUSER', TEXT, D0)
sysfield(219) =  field('LFI_COMPRESSED', INT, D0)
END SUBROUTINE init_sysfield

  FUNCTION get_ftype(hfname,level)
    CHARACTER(LEN=*) :: hfname
    INTEGER          :: get_ftype
    INTEGER,INTENT(IN) :: level

    TYPE(field) :: tzf

    ! Is this a diachronic field ?
    IF (INDEX(hfname,".TY",.TRUE.)     /=0 .OR.& 
    &   INDEX(hfname,".TI",.TRUE.)  /=0 .OR.& 
    &   INDEX(hfname,".UN",.TRUE.)  /=0 .OR.&
    &   INDEX(hfname,".CO",.TRUE.)/=0) THEN
      get_ftype = TEXT
    ELSE IF (INDEX(hfname,".DI",.TRUE.) /= 0) THEN 
      get_ftype = INT
    ELSE IF (INDEX(hfname,".PR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".TR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".DA",.TRUE.)/= 0) THEN
      get_ftype = FLOAT
    ELSE IF (searchfield(hfname,tzf,level)) THEN
    ! search in databases  
      get_ftype = tzf%TYPE
    ELSE
      get_ftype = -1
    END IF
    
  END FUNCTION get_ftype
  
  FUNCTION searchfield(hfname, tpf, level)
    CHARACTER(LEN=*), INTENT(IN) :: hfname
    TYPE(field), INTENT(OUT)     :: tpf
    INTEGER,INTENT(IN)           :: level
    LOGICAL                      :: searchfield

    INTEGER :: ji,iposx
    LOGICAL :: found
    CHARACTER(LEN=4) :: clevel

    found = .FALSE.
    
    ! First is this a date field ?
    DO ji=1,SIZE(datefield)
       IF (INDEX(hfname,TRIM(datefield(ji)%name)) /= 0) THEN 
          found = .TRUE.
          tpf = datefield(ji)
          EXIT
       END IF
    END DO

    IF (.NOT. found) THEN
       ! Next, search in user field tab
       IF (ALLOCATED(userfield)) THEN
          DO ji=1,SIZE(userfield)
             IF (hfname==userfield(ji)%name) THEN
                found = .TRUE.
                tpf = userfield(ji)
                EXIT
             END IF
          END DO
       END IF
       
       IF (.NOT. found) THEN
          ! then search in system field tab
          DO ji=1,SIZE(sysfield)
             IF (hfname==sysfield(ji)%name) THEN
                found = .TRUE.
                tpf = sysfield(ji)
                EXIT
             ELSE
                iposx = INDEX(sysfield(ji)%name,'x')
                IF (iposx /= 0) THEN
                   IF (isnumeric(hfname(iposx:LEN_TRIM(sysfield(ji)%name))) .AND. &
                        sysfield(ji)%name(1:iposx-1)//&
                        hfname(iposx:LEN_TRIM(sysfield(ji)%name))==hfname) THEN 
                      found = .TRUE.
                      tpf = sysfield(ji)
                      EXIT
                   END IF
                ELSE IF (level>-1) THEN
                  !Maybe it is a z-level splitted field
                  !Warning: false positives are possible (but should be rare)
                  write(clevel,'(I4.4)') level
                  iposx = INDEX(hfname,clevel)
                  IF (iposx /= 0) THEN
                    IF (hfname(:iposx-1)==sysfield(ji)%name) THEN
                      found = .TRUE.
                      tpf = sysfield(ji)
                      EXIT
                    END IF
                  END IF
                END IF
             END IF
          END DO
       END IF
    END IF
    
    searchfield = found

  END FUNCTION searchfield
  
  FUNCTION isnumeric(hname)
    CHARACTER(LEN=*) :: hname
    LOGICAL          :: isnumeric

    INTEGER :: ji
    
    isnumeric = .TRUE.

    DO ji = 1,LEN(hname)
       IF (hname(ji:ji) > '9' .OR. hname(ji:ji) < '0') THEN
          isnumeric = .FALSE.
          EXIT
       END IF
    END DO
    
  END FUNCTION isnumeric

END MODULE MODE_FIELDTYPE
