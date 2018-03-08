MODULE MODE_FIELDTYPE
  USE MODD_PARAM
  
  IMPLICIT NONE 

  PRIVATE

  
  TYPE field
     CHARACTER(LEN=FM_FIELD_SIZE) :: name ! Le nom de l'article LFI
     INTEGER                      :: TYPE ! Type :entier(TYPEINT) ou reel(TYPEREAL)
     INTEGER                      :: dim  ! Dimension de l'article  
  END TYPE field
  
  TYPE(field), DIMENSION(:), ALLOCATABLE :: userfield

  ! Les champs contenant %TDATE et %TIME sont traites en dur
  ! dans la routine de recherche de type
  TYPE(field), DIMENSION(2),   PARAMETER  :: datefield = (/&
               field('%TDA', TYPEINT, D0), &
               field('%TIM', TYPEREAL, D0) &
             /)

  TYPE(field), DIMENSION(219), SAVE  :: sysfield

  PUBLIC :: get_ftype, init_sysfield

CONTAINS 
SUBROUTINE init_sysfield()
sysfield(1) =  field('LBXSVMxxx', TYPEREAL , D0)
sysfield(2) =  field('LBYSVMxxx', TYPEREAL , D0)
sysfield(3) =  field('LBXUM', TYPEREAL, D0)
sysfield(4) =  field('LBYUM', TYPEREAL, D0)
sysfield(5) =  field('LBXVM', TYPEREAL, D0)
sysfield(6) =  field('LBYVM', TYPEREAL, D0)
sysfield(7) =  field('LBXWM', TYPEREAL, D0)
sysfield(8) =  field('LBYWM', TYPEREAL, D0)
sysfield(9) =  field('LBXTHM', TYPEREAL, D0)
sysfield(10) =  field('LBYTHM', TYPEREAL, D0)
sysfield(11) =  field('LBXRVM', TYPEREAL, D0)
sysfield(12) =  field('LBYRVM', TYPEREAL, D0)
sysfield(13) =  field('AVG_ZS', TYPEREAL, D0)
sysfield(14) =  field('SIL_ZS', TYPEREAL, D0)
sysfield(15) =  field('AOSIP', TYPEREAL, D0)
sysfield(16) =  field('AOSIM', TYPEREAL, D0)
sysfield(17) =  field('AOSJP', TYPEREAL, D0)
sysfield(18) =  field('AOSJM', TYPEREAL, D0)
sysfield(19) =  field('HO2IP', TYPEREAL, D0)
sysfield(20) =  field('HO2IM', TYPEREAL, D0)
sysfield(21) =  field('HO2JP', TYPEREAL, D0)
sysfield(22) =  field('HO2JM', TYPEREAL, D0)
sysfield(23) =  field('RIMX',TYPEINT, D0)
sysfield(24) =  field('RIMY',TYPEINT, D0)
sysfield(25) =  field('HORELAX_UVWTH',TYPELOG, D0)
sysfield(26) =  field('HORELAX_R',TYPELOG, D0)
sysfield(27) =  field('I2D_XY', TYPEINT, D0)
sysfield(28) =  field('MENU_BUDGET',TYPECHAR, D0)
sysfield(29) =  field('IE', TYPEINT, D0)
sysfield(30) =  field('ZR', TYPEREAL, D0)
sysfield(31) =  field('GOK', TYPELOG, D0)
sysfield(32) =  field('YTEXT', TYPECHAR, D0)
sysfield(33) =  field('X1D', TYPEREAL, D0)
sysfield(34) =  field('I1D', TYPEINT, D0)
sysfield(35) =  field('DEB', TYPEINT, D0)
sysfield(36) =  field('3D1', TYPEREAL, D0)
sysfield(37) =  field('3D2', TYPEREAL, D0)
sysfield(38) =  field('3D3', TYPEREAL, D0)
sysfield(39) =  field('3D4', TYPEREAL, D0)
sysfield(40) =  field('3D5', TYPEREAL, D0)
sysfield(41) =  field('RHODREFZ', TYPEREAL, D0)
sysfield(42) =  field('RSVS', TYPEREAL, D0)
sysfield(43) =  field('RUS', TYPEREAL, D0)
sysfield(44) =  field('MY_NAME', TYPECHAR, D0)
sysfield(45) =  field('DAD_NAME', TYPECHAR, D0)
sysfield(46) =  field('STORAGE_TYPE', TYPECHAR, D0)
sysfield(47) =  field('IMAX', TYPEINT, D0)
sysfield(48) =  field('JMAX', TYPEINT, D0)
sysfield(49) =  field('KMAX', TYPEINT, D0)
sysfield(50) =  field('RPK', TYPEREAL, D0)
sysfield(51) =  field('NEB', TYPEREAL , D0)
sysfield(52) =  field('LONOR', TYPEREAL, D0)
sysfield(53) =  field('LATOR', TYPEREAL, D0)
sysfield(54) =  field('THINSHELL', TYPELOG, D0)
sysfield(55) =  field('LAT0', TYPEREAL, D0)
sysfield(56) =  field('LON0', TYPEREAL, D0)
sysfield(57) =  field('BETA', TYPEREAL, D0)
sysfield(58) =  field('XHAT', TYPEREAL, D0)
sysfield(59) =  field('YHAT', TYPEREAL, D0)
sysfield(60) =  field('ZHAT', TYPEREAL, D0)
sysfield(61) =  field('ZS', TYPEREAL, D0)
sysfield(62) =  field('CARTESIAN', TYPELOG, D0)
sysfield(63) =  field('UM', TYPEREAL, D0)
sysfield(64) =  field('VM', TYPEREAL, D0)
sysfield(65) =  field('WM', TYPEREAL, D0)
sysfield(66) =  field('THM', TYPEREAL, D0)
sysfield(67) =  field('TKEM', TYPEREAL, D0)
sysfield(68) =  field('EPSM', TYPEREAL, D0)
sysfield(69) =  field('PABSM',TYPEREAL, D0)
sysfield(70) =  field('RVM', TYPEREAL, D0)
sysfield(71) =  field('RCM', TYPEREAL, D0)
sysfield(72) =  field('RRM', TYPEREAL, D0)
sysfield(73) =  field('RIM', TYPEREAL, D0)
sysfield(74) =  field('RSM', TYPEREAL, D0)
sysfield(75) =  field('RGM', TYPEREAL, D0)
sysfield(76) =  field('RHM', TYPEREAL, D0)
sysfield(77) =  field('SVMxxx', TYPEREAL, D0)
sysfield(78) =  field('LSUM', TYPEREAL, D0)
sysfield(79) =  field('LSVM', TYPEREAL, D0)
sysfield(80) =  field('LSWM',TYPEREAL , D0)
sysfield(81) =  field('LSTHM',TYPEREAL, D0)
sysfield(82) =  field('LSRVM',TYPEREAL, D0)
sysfield(83) =  field('LSXTKEM',TYPEREAL, D0)
sysfield(84) =  field('LSYTKEM',TYPEREAL, D0)
sysfield(85) =  field('LSXEPSM',TYPEREAL, D0)
sysfield(86) =  field('LSYEPSM',TYPEREAL, D0)
sysfield(87) =  field('LSXRCM',TYPEREAL , D0)
sysfield(88) =  field('LSYRCM', TYPEREAL, D0)
sysfield(89) =  field('LSXRRM', TYPEREAL, D0)
sysfield(90) =  field('LSYRRM', TYPEREAL, D0)
sysfield(91) =  field('LSXRIM', TYPEREAL, D0)
sysfield(92) =  field('LSYRIM', TYPEREAL, D0)
sysfield(93) =  field('LSXRSM', TYPEREAL, D0)
sysfield(94) =  field('LSYRSM', TYPEREAL, D0)
sysfield(95) =  field('LSXRGM', TYPEREAL, D0)
sysfield(96) =  field('LSYRGM', TYPEREAL, D0)
sysfield(97) =  field('LSXRHM', TYPEREAL, D0)
sysfield(98) =  field('LSYRHM', TYPEREAL, D0)
sysfield(99) =  field('LSXSVMxxx', TYPEREAL, D0)
sysfield(100) =  field('LSYSVMxxx', TYPEREAL, D0)
sysfield(101) =  field('UT',TYPEREAL, D0)
sysfield(102) =  field('VT',TYPEREAL, D0)
sysfield(103) =  field('WT',TYPEREAL, D0)
sysfield(104) =  field('THT',TYPEREAL, D0)
sysfield(105) =  field('TKET',TYPEREAL, D0)
sysfield(106) =  field('EPST',TYPEREAL, D0)
sysfield(107) =  field('PABST',TYPEREAL, D0)
sysfield(108) =  field('RVT',TYPEREAL, D0)
sysfield(109) =  field('RCT',TYPEREAL, D0)
sysfield(110) =  field('RRT',TYPEREAL, D0)
sysfield(111) =  field('RIT',TYPEREAL, D0)
sysfield(112) =  field('CIT',TYPEREAL, D0)
sysfield(113) =  field('RST',TYPEREAL, D0)
sysfield(114) =  field('RGT',TYPEREAL, D0)
sysfield(115) =  field('RHT',TYPEREAL, D0)
sysfield(116) =  field('SVTxxx',TYPEREAL, D0)
sysfield(117) =  field('DRYMASST',TYPEREAL, D0)
sysfield(118) =  field('SRCM',TYPEREAL, D0)
sysfield(119) =  field('SRCT',TYPEREAL, D0)
sysfield(120) =  field('SIGS',TYPEREAL, D0)
sysfield(121) =  field('RHOREFZ',TYPEREAL, D0)
sysfield(122) =  field('THVREFZ',TYPEREAL, D0)
sysfield(123) =  field('EXNTOP',TYPEREAL, D0)
sysfield(124) =  field('RESA', TYPEREAL , D0)
sysfield(125) =  field('Z0SEA', TYPEREAL , D0)
sysfield(126) =  field('TS', TYPEREAL , D0)
sysfield(127) =  field('WG', TYPEREAL , D0)
sysfield(128) =  field('SST', TYPEREAL , D0)
sysfield(129) =  field('T2', TYPEREAL , D0)
sysfield(130) =  field('W2', TYPEREAL , D0)
sysfield(131) =  field('WR', TYPEREAL , D0)
sysfield(132) =  field('WS', TYPEREAL , D0)
sysfield(133) =  field('ALBS', TYPEREAL , D0)
sysfield(134) =  field('RHOS', TYPEREAL , D0)
sysfield(135) =  field('LAND', TYPEREAL , D0)
sysfield(136) =  field('SEA', TYPEREAL , D0)
sysfield(137) =  field('Z0VEG', TYPEREAL , D0)
sysfield(138) =  field('Z0HVEG', TYPEREAL , D0)
sysfield(139) =  field('Z0REL', TYPEREAL , D0)
sysfield(140) =  field('Z0EFFIP', TYPEREAL , D0)
sysfield(141) =  field('Z0EFFIM', TYPEREAL , D0)
sysfield(142) =  field('Z0EFFJP', TYPEREAL , D0)
sysfield(143) =  field('Z0EFFJM', TYPEREAL , D0)
sysfield(144) =  field('SSO_STDEV', TYPEREAL , D0)
sysfield(145) =  field('SSO_ANIS', TYPEREAL , D0)
sysfield(146) =  field('SSO_DIRECTION', TYPEREAL , D0)
sysfield(147) =  field('SSO_SLOPE', TYPEREAL , D0)
sysfield(148) =  field('ALBVIS', TYPEREAL , D0)
sysfield(149) =  field('ALBNIR', TYPEREAL , D0)
sysfield(150) =  field('EMIS', TYPEREAL , D0)
sysfield(151) =  field('CLAY', TYPEREAL , D0)
sysfield(152) =  field('SAND', TYPEREAL , D0)
sysfield(153) =  field('D2', TYPEREAL , D0)
sysfield(154) =  field('VEG', TYPEREAL , D0)
sysfield(155) =  field('LAI', TYPEREAL , D0)
sysfield(156) =  field('RSMIN', TYPEREAL , D0)
sysfield(157) =  field('GAMMA', TYPEREAL , D0)
sysfield(158) =  field('RGL', TYPEREAL , D0)
sysfield(159) =  field('CV', TYPEREAL , D0)
sysfield(160) =  field('SFTHT', TYPEREAL , D0)
sysfield(161) =  field('SFTHP', TYPEREAL , D0)
sysfield(162) =  field('SFRT', TYPEREAL , D0)
sysfield(163) =  field('SFRP', TYPEREAL , D0)
sysfield(164) =  field('SFSVT', TYPEREAL , D0)
sysfield(165) =  field('SFSVP', TYPEREAL , D0)
sysfield(166) =  field('DTHRAD', TYPEREAL , D0)
sysfield(167) =  field('SRFLWD', TYPEREAL , D0)
sysfield(168) =  field('SRFSWD', TYPEREAL , D0)
sysfield(169) =  field('CLDFR', TYPEREAL , D0)
sysfield(170) =  field('COUNTCONV', TYPEINT , D0)
sysfield(171) =  field('DTHCONV', TYPEREAL , D0)
sysfield(172) =  field('DRVCONV', TYPEREAL , D0)
sysfield(173) =  field('DRCCONV', TYPEREAL , D0)
sysfield(174) =  field('DRICONV', TYPEREAL , D0)
sysfield(175) =  field('PRCONV', TYPEREAL , D0)
sysfield(176) =  field('PACCONV', TYPEREAL , D0)
sysfield(177) =  field('WSUBCONV', TYPEREAL , D0)
sysfield(178) =  field('INPRR', TYPEREAL , D0)
sysfield(179) =  field('ACPRR', TYPEREAL , D0)
sysfield(180) =  field('INPRS', TYPEREAL , D0)
sysfield(181) =  field('ACPRS', TYPEREAL , D0)
sysfield(182) =  field('INPRG', TYPEREAL , D0)
sysfield(183) =  field('ACPRG', TYPEREAL , D0)
sysfield(184) =  field('INPRT', TYPEREAL , D0)
sysfield(185) =  field('ACPRT', TYPEREAL , D0)
sysfield(186) =  field('FRC', TYPEINT, D0)
sysfield(187) =  field('UFRCxx', TYPEREAL , D0)
sysfield(188) =  field('VFRCxx', TYPEREAL , D0)
sysfield(189) =  field('WFRCxx', TYPEREAL , D0)
sysfield(190) =  field('THFRCxx', TYPEREAL , D0)
sysfield(191) =  field('RVFRCxx', TYPEREAL , D0)
sysfield(192) =  field('GXRVFRCxx', TYPEREAL , D0)
sysfield(193) =  field('GYRVFRCxx', TYPEREAL , D0)
sysfield(194) =  field('GXTHFRCxx', TYPEREAL , D0)
sysfield(195) =  field('GYTHFRCxx', TYPEREAL , D0)
sysfield(196) =  field('DUMMY_GRxxx', TYPEREAL , D0)
sysfield(197) =  field('MASDEV', TYPEINT , D0)
sysfield(198) =  field('EMISFILE_GR_NBR', TYPEINT , D0)
sysfield(199) =  field('EMISPEC_GR_NBR', TYPEINT , D0)
sysfield(200) =  field('EMISNAMExxx', TYPECHAR , D0)
sysfield(201) =  field('EMISTIMESxxx', TYPEINT , D0)
sysfield(202) =  field('DUMMY_GR_NBR', TYPEINT , D0)
sysfield(203) =  field('COVERxxx', TYPEREAL , D0)
sysfield(204) =  field('TGx', TYPEREAL, D0)
sysfield(205) =  field('T_ROOFx', TYPEREAL, D0)
sysfield(206) =  field('T_ROADx', TYPEREAL, D0)
sysfield(207) =  field('T_WALLx', TYPEREAL, D0)
sysfield(208) =  field('WGx', TYPEREAL, D0)
sysfield(209) =  field('WGIx', TYPEREAL, D0)
sysfield(210) =  field('MAX_ZS', TYPEREAL, D0)
sysfield(211) =  field('MIN_ZS', TYPEREAL, D0)
sysfield(212) =  field('XOR', TYPEINT, D0)
sysfield(213) =  field('YOR', TYPEINT, D0)
sysfield(214) =  field('DXRATIO', TYPEINT, D0)
sysfield(215) =  field('DYRATIO', TYPEINT, D0)
sysfield(216) =  field('PATCH_NUMBER', TYPEINT, D0)
sysfield(217) =  field('BUGFIX', TYPEINT, D0)
sysfield(218) =  field('BIBUSER', TYPECHAR, D0)
sysfield(219) =  field('LFI_COMPRESSED', TYPEINT, D0)
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
      get_ftype = TYPECHAR
    ELSE IF (INDEX(hfname,".DI",.TRUE.) /= 0) THEN 
      get_ftype = TYPEINT
    ELSE IF (INDEX(hfname,".PR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".TR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".DA",.TRUE.)/= 0) THEN
      get_ftype = TYPEREAL
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
