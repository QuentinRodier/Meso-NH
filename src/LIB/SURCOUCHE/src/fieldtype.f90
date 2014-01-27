!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
#ifdef MNH_NCWRIT
MODULE MODE_FIELDTYPE
  USE MODD_PARAM
  
  IMPLICIT NONE 

  PRIVATE

  
  TYPE field
     CHARACTER(LEN=FM_FIELD_SIZE) :: name ! Le nom de l'article LFI
     INTEGER                      :: TYPE ! Type :entier(INT) ou reel(FLOAT2)
     CHARACTER(LEN=FM_FIELD_SIZE) :: ncname ! Le nom de la variable netcdf
     CHARACTER(LEN=FM_FIELD_SIZE) :: ncunit ! Unité de la variable netcdf
     CHARACTER(LEN=64) :: ncdes  ! Description de la variable netcdf
  END TYPE field
  
  TYPE(field), DIMENSION(:), ALLOCATABLE :: userfield

  ! Les champs contenant %TDATE et %TIME sont traites en dur
  ! dans la routine de recherche de type
  TYPE(field), DIMENSION(2),   PARAMETER  :: datefield = (/&
               field('%TDA', INT2, '', '',''), &
               field('%TIM', FLOAT2, '', '','') &
             /)

  TYPE(field), DIMENSION(233), SAVE  :: sysfield

  PUBLIC :: get_ftype, get_ncname, get_ncunit, get_ncdes, init_sysfield

CONTAINS 
SUBROUTINE init_sysfield()
sysfield(1) =  field('3D1', FLOAT2, '3D1','','')
sysfield(2) =  field('3D2', FLOAT2, '3D2','','')
sysfield(3) =  field('3D3', FLOAT2, '3D3','','')
sysfield(4) =  field('3D4', FLOAT2, '3D4','','')
sysfield(5) =  field('3D5', FLOAT2, '3D5','','')
sysfield(6) =  field('ACPRC', FLOAT2 , 'ACPRC','mm -h','ACcumulated Cloud&
 & Precipitation Rain Rate')
sysfield(7) =  field('ACPRG', FLOAT2 , 'ACPRG','mm','ACcumulated PRecipitation&
 & Graupel Rate') 
sysfield(8) =  field('ACPRR', FLOAT2 , 'ACPRR','mm','ACcumulated Precipitation&
 & Rain Rate')
sysfield(9) =  field('ACPRS', FLOAT2 , 'ACPRS','mm','ACcumulated PRecipitation&
 & Snow Rate')
sysfield(10) =  field('ACPRT', FLOAT2 , 'ACPRT','mm','Total ACcumulated&
 & PRecipitation rate')
sysfield(11) =  field('ALBNIR', FLOAT2 , 'ALBNIR','','')
sysfield(12) =  field('ALBS', FLOAT2 , 'ALBS','','')
sysfield(13) =  field('ALBVIS', FLOAT2 , 'ALBVIS','','')
sysfield(14) =  field('AOSIM', FLOAT2, 'AOSIM','','')
sysfield(15) =  field('AOSIP', FLOAT2, 'AOSIP','','')
sysfield(16) =  field('AOSJM', FLOAT2, 'AOSJM','','')
sysfield(17) =  field('AOSJP', FLOAT2, 'AOSJP','','')
sysfield(18) =  field('AVG_ZS', FLOAT2, 'AVG_ZS','m','')
sysfield(19) =  field('AZIM', FLOAT2, 'AZIM','rad','azimuth')
sysfield(20) =  field('BETA', FLOAT2, 'BETA','','')
sysfield(21) =  field('CIT',FLOAT2, 'CIT','m-3','cloud ice concentration')
sysfield(22) =  field('CLAY', FLOAT2 , 'CLAY','','')
sysfield(23) =  field('CLDFR', FLOAT2 , 'CLDFR','','Cloud fraction')
sysfield(24) =  field('COUNTCONV', INT2 , 'COUNTCONV','','')
sysfield(25) =  field('COVERxxx', FLOAT2 , 'COVERxxx','','')
sysfield(26) =  field('CV', FLOAT2 , 'CV','','')
sysfield(27) =  field('D2', FLOAT2 , 'D2','','')
sysfield(28) =  field('DEB', FLOAT2, 'DEB','','')
sysfield(29) =  field('DIR_ALB', FLOAT2, 'DIR_ALB','','direct albedo')
sysfield(30) =  field('DIRFLASWD', FLOAT2, 'DIRFLASWD','W m-2','DIRect Downward&
 &Long Waves on FLAT surface')
sysfield(31) =  field('DIRSRFSWD', FLOAT2 , 'DIRSRFSWD','W m-2','DIRect&
 &Downward Long Waves')
sysfield(32) =  field('DRICONV', FLOAT2 , 'DRICONV','','')
sysfield(33) =  field('DRCCONV', FLOAT2 , 'DRCCONV','','')
sysfield(34) =  field('DRVCONV', FLOAT2 , 'DRVCONV','','')
sysfield(35) =  field('DRYMASST',FLOAT2, 'DRYMASST','kg','')
sysfield(36) =  field('DTHCONV', FLOAT2 , 'DTHCONV','','')
sysfield(37) =  field('DTHRAD', FLOAT2 , 'DTHRAD','K s-1','radiative&
 &heating/cooling rate')
sysfield(38) =  field('DUMMY_GR_NBR', INT2 , 'DUMMY_GR_NBR','','')
sysfield(39) =  field('DUMMY_GRxxx', FLOAT2 , 'DUMMY_GRxxx','','')
sysfield(40) =  field('DXRATIO', INT2, 'DXRATIO','','')
sysfield(41) =  field('DYRATIO', INT2, 'DYRATIO','','')
sysfield(42) =  field('EMIS', FLOAT2 , 'EMIS','','EMISsivity')
sysfield(43) =  field('EMISFILE_GR_NBR', INT2 , 'EMISFILE_GR_NBR','','')
sysfield(44) =  field('EMISPEC_GR_NBR', INT2 , 'EMISPEC_GR_NBR','','')
sysfield(45) =  field('EMISTIMESxxx', INT2 , 'EMISTIMESxxx','','')
sysfield(46) =  field('EPSM', FLOAT2, 'EPSM','','')
sysfield(47) =  field('EPST',FLOAT2, 'EPST','','')
sysfield(48) =  field('EVAP3D',FLOAT2, 'EVAP3D','kg kg-1 s-1','INstantaneous 3D&
 &Rain Evaporation flux')
sysfield(49) =  field('EXNTOP',FLOAT2, 'EXNTOP','','exner function at model top')
sysfield(50) =  field('FLALWD', FLOAT2, 'FLALWD','W m-2','Downward Long Waves on&
 &FLAT surface')
sysfield(51) =  field('FRC', INT2, 'FRC','','')
sysfield(52) =  field('FMU', FLOAT2, 'FMU','kg m-1 s-2','')
sysfield(53) =  field('FMV', FLOAT2, 'FMV','kg m-1 s-2','')
sysfield(54) =  field('GAMMA', FLOAT2 , 'GAMMA','','')
sysfield(55) =  field('GFLUX',FLOAT2, 'GFLUX','W m-2','')
sysfield(56) =  field('GXRVFRCxx', FLOAT2 , 'GXRVFRCxx','','')
sysfield(57) =  field('GXTHFRCxx', FLOAT2 , 'GXTHFRCxx','','')
sysfield(58) =  field('GYRVFRCxx', FLOAT2 , 'GYRVFRCxx','','')
sysfield(59) =  field('GYTHFRCxx', FLOAT2 , 'GYTHFRCxx','','')
sysfield(60) =  field('H', FLOAT2, 'H','W m-2','')
sysfield(61) =  field('HO2IM', FLOAT2, 'HO2IM','m','')
sysfield(62) =  field('HO2IP', FLOAT2, 'HO2IP','m','')
sysfield(63) =  field('HO2JM', FLOAT2, 'HO2JM','m','')
sysfield(64) =  field('HO2JP', FLOAT2, 'HO2JP','m','')
sysfield(65) =  field('I1D', INT2, 'I1D','','')
sysfield(66) =  field('I2D_XY', INT2, 'I2D_XY','','')
sysfield(67) =  field('IE', INT2, 'IE','','')
sysfield(68) =  field('IMAX', INT2, 'IMAX','','')
sysfield(69) =  field('INPRR3D', FLOAT2, 'INPRR3D','m s-1','INstantaneous 3D Rain&
 &Precipitation flux')
sysfield(70) =  field('INPRC', FLOAT2 , 'INPRC','mm h-1','INstantaneous Cloud&
 &Precipitation Rain Rate')
sysfield(71) =  field('INPRG', FLOAT2 , 'INPRG','mm h-1','INstantaneous&
 &PRecipitation Graupel Rate')
sysfield(72) =  field('INPRR', FLOAT2 , 'INPRR','mm h-1','INstantaneous&
 &Precipitation Rain Rate')
sysfield(73) =  field('INPRS', FLOAT2 , 'INPRS','mm h-1','INstantaneous&
 &PRecipitation Snow Rate')
sysfield(74) =  field('INPRT', FLOAT2 , 'INPRT','mm h-1','Total INstantaneaous&
 &PRecipitation rate')
sysfield(75) =  field('JMAX', INT2, 'JMAX','','')
sysfield(76) =  field('KMAX', INT2, 'KMAX','','')
sysfield(77) =  field('LAT', FLOAT2, 'LAT','degrees-north','')
sysfield(78) =  field('LBXSVMxxx', FLOAT2 , 'LBXSVM','','')
sysfield(79) =  field('LBYSVMxxx', FLOAT2 , 'LBYSVM','','')
sysfield(80) =  field('LBXUM', FLOAT2, 'LBXUM','m s-1','')
sysfield(81) =  field('LBYUM', FLOAT2, 'LBYUM','m s-1','')
sysfield(82) =  field('LBXVM', FLOAT2,'LBXVM','m s-1' ,'')
sysfield(83) =  field('LBYVM', FLOAT2, 'LBYVM','m s-1','')
sysfield(84) =  field('LBXWM', FLOAT2, 'LBXWM','m s-1','')
sysfield(85) =  field('LBYWM', FLOAT2, 'LBYWM','m s-1','')
sysfield(86) =  field('LBXTHM', FLOAT2, 'LBXTHM','K','')
sysfield(87) =  field('LBYTHM', FLOAT2, 'LBYTHM','K','')
sysfield(88) =  field('LBXRVM', FLOAT2, 'LBXRVM','kg kg-1','')
sysfield(89) =  field('LBYRVM', FLOAT2, 'LBYRVM','kg kg-1','')
sysfield(90) =  field('LON', FLOAT2, 'LON','degrees-east','')
sysfield(91) =  field('LONOR', FLOAT2, 'LONOR','','')
sysfield(92) =  field('LATOR', FLOAT2, 'LATOR','','')
sysfield(93) =  field('LE', FLOAT2, 'LE','W m-2','')
sysfield(94) =  field('LSUM', FLOAT2, 'LSUM','m s-1','large scale x-wind&
 &component')
sysfield(95) =  field('LSVM', FLOAT2, 'LSVM','m s-1','large scale y-wind&
 &component')
sysfield(96) =  field('LSWM',FLOAT2 , 'LSWM','m s-1','large scale z-wind&
 &component')
sysfield(97) =  field('LSTHM',FLOAT2, 'LSTHM','K','large scale &
          & potential temperature')
sysfield(98) =  field('LSRVM',FLOAT2, 'LSRVM','kg kg-1','large scale vapor&
 &mixing ratio')
sysfield(99) =  field('LSXTKEM',FLOAT2, 'LSXTKEM','','')
sysfield(100) =  field('LSYTKEM',FLOAT2, 'LSYTKEM','','')
sysfield(101) =  field('LSXEPSM',FLOAT2, 'LSXEPSM','','')
sysfield(102) =  field('LSXRIM', FLOAT2, 'LSXRIM','','')
sysfield(103) =  field('LSYRIM', FLOAT2, 'LSYRIM','','')
sysfield(104) =  field('LSXRSM', FLOAT2, 'LSXRSM','','')
sysfield(105) =  field('LSYRSM', FLOAT2, 'LSYRSM','','')
sysfield(106) =  field('LSYRCM', FLOAT2, 'LSYRCM','','')
sysfield(107) =  field('LSXRRM', FLOAT2, 'LSXRRM','','')
sysfield(108) =  field('LSYRRM', FLOAT2, 'LSYRRM','','')
sysfield(109) =  field('LAI', FLOAT2 , 'LAI','','')
sysfield(110) =  field('LSXRGM', FLOAT2, 'LSXRGM','','')
sysfield(111) =  field('LSYRGM', FLOAT2, 'LSYRGM','','')
sysfield(112) =  field('LSXRHM', FLOAT2, 'LSXRHM','','')
sysfield(113) =  field('LSYRHM', FLOAT2, 'LSYRHM','','')
sysfield(114) =  field('LSXSVMxxx', FLOAT2, 'LSXSVM','','')
sysfield(115) =  field('LSYSVMxxx', FLOAT2, 'LSYSVM','','')
sysfield(116) =  field('LSYEPSM',FLOAT2, 'LSYEPSM','','')
sysfield(117) =  field('LSXRCM',FLOAT2 , 'LSXRCM','','')
sysfield(118) =  field('LAT0', FLOAT2, 'LAT0','','')
sysfield(119) =  field('LAND', FLOAT2 , 'LAND','','')
sysfield(120) =  field('LON0', FLOAT2, 'LON0','','')
sysfield(121) =  field('MASDEV', INT2 , 'MASDEV','','')
sysfield(122) =  field('MAX_ZS', FLOAT2, 'MAX_ZS','m','')
sysfield(123) =  field('MIN_ZS', FLOAT2, 'MIN_ZS','m','')
sysfield(124) =  field('MRC', FLOAT2, 'MRC','g kg-1','')
sysfield(125) =  field('MRR', FLOAT2, 'MRR','g kg-1','')
sysfield(126) =  field('MRV', FLOAT2, 'MRV','g kg-1','')
sysfield(127) =  field('NEB', FLOAT2 , 'NEB','','')
sysfield(128) =  field('PABSM',FLOAT2, 'PABSM','Pa','Absolute pressure')
sysfield(129) =  field('PABST',FLOAT2, 'PABST','','')
sysfield(130) =  field('PACCONV', FLOAT2 , 'PACCONV','','')
sysfield(131) =  field('PRCONV', FLOAT2 , 'PRCONV','','')
sysfield(132) =  field('PATCH_NUMBER', INT2, 'PATCH_NUMBER','','')
sysfield(133) =  field('RCM', FLOAT2, 'RCM','kg kg-1','cloud mixing ratio')
sysfield(134) =  field('RCT',FLOAT2, 'RCT','kg kg-1','cloud mixing ratio')
sysfield(135) =  field('RESA', FLOAT2 , 'RESA','','')
sysfield(136) =  field('RGL', FLOAT2 , 'RGL','','')
sysfield(137) =  field('RGM', FLOAT2, 'RGM','kg kg-1','graupel mixing ratio')
sysfield(138) =  field('RGT',FLOAT2, 'RGT','kg kg-1','graupel mixing ratio')
sysfield(139) =  field('RHM', FLOAT2, 'RHM','kg kg-1','')
sysfield(140) =  field('RHT',FLOAT2, 'RHT','kg kg-1','')
sysfield(141) =  field('RHOS', FLOAT2 , 'RHOS','','')
sysfield(142) =  field('RIM', FLOAT2, 'RIM','kg kg-1','ice mixing ratio')
sysfield(143) =  field('RIMX',INT2, 'RIMX','','')
sysfield(144) =  field('RIMY',INT2, 'RIMY','','')
sysfield(145) =  field('RIT',FLOAT2, 'RIT','kg kg-1','ice mixing ratio')
sysfield(146) =  field('RN', FLOAT2, 'RN','w m-2','')
sysfield(147) =  field('RPK', FLOAT2, 'RPK','','')
sysfield(148) =  field('RRM', FLOAT2, 'RRM','kg kg-1','rain mixing ratio')
sysfield(149) =  field('RRT',FLOAT2, 'RRT','kg kg-1','rain mixing ratio')
sysfield(150) =  field('RSMIN', FLOAT2 , 'RSMIN','','')
sysfield(151) =  field('RSM', FLOAT2, 'RSM','kg kg-1','snow mixing ratio')
sysfield(152) =  field('RST',FLOAT2, 'RST','kg kg-1','snow mixing ratio')
sysfield(153) =  field('RSVS', FLOAT2, 'RSVS','','')
sysfield(154) =  field('RVFRCxx', FLOAT2 , 'RVFRCxx','','')
sysfield(155) =  field('RVM', FLOAT2, 'RVM','kg kg-1','vapor mixing ratio')
sysfield(156) =  field('RVT',FLOAT2, 'RVT','kg kg-1','vapor mixing ratio')
sysfield(157) =  field('RHODREF', FLOAT2, 'RHODREF','kg m-3','Dry density for&
 &reference state with orography')
sysfield(158) =  field('RUS', FLOAT2, 'RUS','','')
sysfield(159) =  field('RHOREFZ',FLOAT2, 'RHOREFZ','kg m-3','hodz for reference&
 &state without orography')
sysfield(160) =  field('SAND', FLOAT2 , 'SAND','','')
sysfield(161) =  field('SCA_ALB', FLOAT2 , 'SCA_ALB','','SCAttered ALBedo')
sysfield(162) =  field('SCAFLASWD', FLOAT2 , 'SCAFLASWD','W m-2','scattered&
 &Downward Long Waves on FLAT surface')
sysfield(163) =  field('SEA', FLOAT2 , 'SEA','','')
sysfield(164) =  field('SFTHT', FLOAT2 , 'SFTHT','','')
sysfield(165) =  field('SFRT', FLOAT2 , 'SFRT','','')
sysfield(166) =  field('SFTHP', FLOAT2 , 'SFTHP','','')
sysfield(167) =  field('SIGS',FLOAT2, 'SIGS','kg kg-2','sigma_s from turbulence&
 &scheme')
sysfield(168) =  field('SSO_DIR', FLOAT2 , 'SSO_DIR','deg','')
sysfield(169) =  field('SSO_SLOPE', FLOAT2 , 'SSO_SLOPE','','')
sysfield(170) =  field('SSO_STDEV', FLOAT2 , 'SSO_STDEV','m','')
sysfield(171) =  field('SSO_ANIS', FLOAT2 , 'SSO_ANIS','m','SSO_ANISOTROPY')
sysfield(172) =  field('SST', FLOAT2 , 'SST','','')
sysfield(173) =  field('SFRP', FLOAT2 , 'SFRP','','')
sysfield(174) =  field('SFSVT', FLOAT2 , 'SFSVT','','')
sysfield(175) =  field('SFSVP', FLOAT2 , 'SFSVP','','')
sysfield(176) =  field('SRFLWD', FLOAT2 , 'SRFLWD','','')
sysfield(177) =  field('SRFSWD', FLOAT2 , 'SRFSWD','','')
sysfield(178) =  field('SRCM',FLOAT2, 'SRCM','kg kg-2','normalized 2nd_order&
 &moment s_r_c/2Sigma_s2')
sysfield(179) =  field('SRCT',FLOAT2, 'SRCT','kg kg-2','normalized 2nd_order&
 &moment s_r_c/2Sigma_s2')
sysfield(180) =  field('SVMxxx', FLOAT2, 'SVM','','')
sysfield(181) =  field('SIL_ZS', FLOAT2, 'SIL_ZS','m','')
sysfield(182) =  field('SVTxxx',FLOAT2, 'SVT','','')
sysfield(183) =  field('T2', FLOAT2 , 'T2','','')
sysfield(184) =  field('THFRCxx', FLOAT2 , 'THFRCxx','','')
sysfield(185) =  field('THM', FLOAT2, 'THM','K','potential temperature')
sysfield(186) =  field('THT',FLOAT2, 'THT','','potential temperature')
sysfield(187) =  field('THVREFZ',FLOAT2, 'THVREFZ','K','thetavz for reference&
 & state without orography')
sysfield(188) =  field('TKEM', FLOAT2, 'TKEM','m2 s-2','')
sysfield(189) =  field('TKET',FLOAT2, 'TKET','m2 s-2','')
sysfield(190) =  field('THVREF',FLOAT2, 'THVREF','K','')
sysfield(191) =  field('T_ROOFx', FLOAT2, 'T_ROOFx','','')
sysfield(192) =  field('TGx', FLOAT2, 'TGx','','')
sysfield(193) =  field('T_ROADx', FLOAT2, 'T_ROADx','','')
sysfield(194) =  field('TS', FLOAT2 , 'TS','','')
sysfield(195) =  field('TSRAD', FLOAT2 , 'TSRAD','K','RADiative Surface&
 &Temperature')
sysfield(196) =  field('T_WALLx', FLOAT2, 'T_WALLx','','')
sysfield(197) =  field('UFRCxx', FLOAT2 , 'UFRCxx','','')
sysfield(198) =  field('UM', FLOAT2, 'UM','m s-1','x-wind component')
sysfield(199) =  field('UT',FLOAT2, 'U','m s-1','x-wind component')
sysfield(200) =  field('VEG', FLOAT2 , 'VEG','','')
sysfield(201) =  field('VFRCxx', FLOAT2 , 'VFRCxx','','')
sysfield(202) =  field('VM', FLOAT2, 'VM','m s-1','y-wind component')
sysfield(203) =  field('VT',FLOAT2, 'V','m s-1','y-wind component')
sysfield(204) =  field('W2', FLOAT2 , 'W2','','')
sysfield(205) =  field('WFRCxx', FLOAT2 , 'WFRCxx','','')
sysfield(206) =  field('WG', FLOAT2 , 'WG','','')
sysfield(207) =  field('WGIx', FLOAT2, 'WGIx','','')
sysfield(208) =  field('WGx', FLOAT2, 'WGx','','')
sysfield(209) =  field('WM', FLOAT2, 'WM','m s-1','z-wind component')
sysfield(210) =  field('WR', FLOAT2 , 'WR','','')
sysfield(211) =  field('WS', FLOAT2 , 'WS','','')
sysfield(212) =  field('WSUBCONV', FLOAT2 , 'WSUBCONV','','')
sysfield(213) =  field('WT',FLOAT2, 'W','m s-1','z-wind component')
sysfield(214) =  field('WTHVMF', FLOAT2 , 'WTHVMF','m K s-1','')
sysfield(215) =  field('X1D', FLOAT2, 'X1D','','')
sysfield(216) =  field('XHAT', FLOAT2, 'XHAT','m','position x in the conformal or&
 &cartesian plane')
sysfield(217) =  field('XOR', INT2, 'XOR','','')
sysfield(218) =  field('YHAT', FLOAT2, 'YHAT','m','position y in the conformal or&
 &cartesian plane')
sysfield(219) =  field('YOR', INT2, 'YOR','','')
sysfield(220) =  field('YTEXT', TEXT, 'YTEXT','','')
sysfield(221) =  field('Z0SEA', FLOAT2 , 'Z0SEA','','')
sysfield(222) =  field('Z0VEG', FLOAT2 , 'Z0VEG','','')
sysfield(223) =  field('Z0HVEG', FLOAT2 , 'Z0HVEG','','')
sysfield(224) =  field('Z0REL', FLOAT2 , 'Z0REL','','')
sysfield(225) =  field('Z0EFFIP', FLOAT2 , 'Z0EFFIP','','')
sysfield(226) =  field('Z0EFFIM', FLOAT2 , 'Z0EFFIM','','')
sysfield(227) =  field('Z0EFFJP', FLOAT2 , 'Z0EFFJP','','')
sysfield(228) =  field('Z0EFFJM', FLOAT2 , 'Z0EFFJM','','')
sysfield(229) =  field('ZENITH', FLOAT2 , 'ZENITH','rad','ZENITH')
sysfield(230) =  field('ZHAT', FLOAT2, 'ZHAT','m','height level without orography')
sysfield(231) =  field('ZR', FLOAT2, 'ZR','','')
sysfield(232) =  field('ZS', FLOAT2, 'ZS','m','orography')
sysfield(233) =  field('ZSMT', FLOAT2, 'ZSMT','m','smooth orography')
END SUBROUTINE init_sysfield

  FUNCTION get_ftype(hfname)
    CHARACTER(LEN=*) :: hfname
    INTEGER          :: get_ftype

    TYPE(field) :: tzf

    ! Is this a diachronic field ?
    IF (INDEX(hfname,".TY",.TRUE.)     /=0 .OR.& 
    &   INDEX(hfname,".TI",.TRUE.)  /=0 .OR.& 
    &   INDEX(hfname,".UN",.TRUE.)  /=0 .OR.&
    &   INDEX(hfname,".CO",.TRUE.)/=0) THEN
      get_ftype = TEXT
    ELSE IF (INDEX(hfname,".DI",.TRUE.) /= 0) THEN 
      get_ftype = INT2
    ELSE IF (INDEX(hfname,".PR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".TR",.TRUE.)/= 0 .OR.&
         &   INDEX(hfname,".DA",.TRUE.)/= 0) THEN
      get_ftype = FLOAT2
    ELSE IF (searchfield(hfname,tzf)) THEN
    ! search in databases  
      get_ftype = tzf%TYPE
    ELSE
      get_ftype = -1
    END IF
    
  END FUNCTION get_ftype

  FUNCTION get_ncname(hfname)
    CHARACTER(LEN=*) :: hfname
    CHARACTER(LEN=FM_FIELD_SIZE) :: get_ncname

    TYPE(field) :: tzf
    IF (searchfield(hfname,tzf)) THEN
      get_ncname = tzf%ncname
    ELSE
      get_ncname = hfname
    END IF
  END FUNCTION get_ncname

  FUNCTION get_ncunit(hfname)
    CHARACTER(LEN=*) :: hfname
    CHARACTER(LEN=FM_FIELD_SIZE) :: get_ncunit

    TYPE(field) :: tzf
    IF (searchfield(hfname,tzf)) THEN
      get_ncunit = tzf%ncunit
    ELSE
      get_ncunit = ''
    END IF
  END FUNCTION get_ncunit

  FUNCTION get_ncdes(hfname)
    CHARACTER(LEN=*) :: hfname
    CHARACTER(LEN=64) :: get_ncdes

    TYPE(field) :: tzf
    IF (searchfield(hfname,tzf)) THEN
      get_ncdes = tzf%ncdes
    ELSE
      get_ncdes = ''
    END IF
  END FUNCTION get_ncdes
  
  FUNCTION searchfield(hfname, tpf)
    CHARACTER(LEN=*), INTENT(IN) :: hfname
    TYPE(field), INTENT(OUT)     :: tpf
    LOGICAL                      :: searchfield

    INTEGER :: ji,iposx
    LOGICAL :: found

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
!             ELSE
!                     ! BEGIN MODIF SB
!                iposx = INDEX(sysfield(ji)%name,'x')
!                IF (iposx /= 0) THEN
!                   IF (isnumeric(hfname(iposx:LEN_TRIM(sysfield(ji)%name))) .AND. &
!                        sysfield(ji)%name(1:iposx-1)//&
!                        hfname(iposx:LEN_TRIM(sysfield(ji)%name))==hfname) THEN 
!                      found = .TRUE.
!                      tpf = sysfield(ji)
!                      EXIT
!                      ! END MODIF SB
!                   END IF
!                END IF
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
#endif
