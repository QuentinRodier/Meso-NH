!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!      ########################
       SUBROUTINE INI_STAND_ATM
!      ########################
!
!!****  *INI_STAND_ATM* - initialization of 5 standard atmospheres
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to initialize the tropical,
!!    summer/winter mid-latitudes and summer/winter polar standard atmospheres
!!
!!**  METHOD
!!    ------
!!      Each atmosphere is defined on 31 levels from 0 to 50 Km. For each level
!!    is given the height in km, the pressure in mbar, the temperature in K, the
!!    air density in kg/m3, the vapor content in kg/m3 and the ozone content in
!!    kg/m3
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_STAND_ATM : declares 5 arrays of standard atmospheres
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module INI_STAND_ATM)
!!
!!    AUTHOR
!!    ------
!!     J.-P. Pinty   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      26/02/95
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_STAND_ATM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
!
!*       0.2   declarations of local variables
!

!
!-------------------------------------------------------------------------------
!
!  Standard TROpical ATMosphere
!
XSTROATM ( 1,:) = (/ 0.0,1013.00,300.00,0.116700E+04,0.190000E+02,0.560000E-04/)
XSTROATM ( 2,:) = (/ 1.0, 904.00,294.00,0.106400E+04,0.130000E+02,0.560000E-04/)
XSTROATM ( 3,:) = (/ 2.0, 805.00,288.00,0.968900E+03,0.930000E+01,0.540000E-04/)
XSTROATM ( 4,:) = (/ 3.0, 715.00,284.00,0.875600E+03,0.470000E+01,0.510000E-04/)
XSTROATM ( 5,:) = (/ 4.0, 633.00,277.00,0.795100E+03,0.220000E+01,0.470000E-04/)
XSTROATM ( 6,:) = (/ 5.0, 559.00,270.00,0.719900E+03,0.150000E+01,0.450000E-04/)
XSTROATM ( 7,:) = (/ 6.0, 492.00,264.00,0.650100E+03,0.850000E+00,0.430000E-04/)
XSTROATM ( 8,:) = (/ 7.0, 432.00,257.00,0.585500E+03,0.470000E+00,0.410000E-04/)
XSTROATM ( 9,:) = (/ 8.0, 378.00,250.00,0.525800E+03,0.250000E+00,0.390000E-04/)
XSTROATM (10,:) = (/ 9.0, 329.00,244.00,0.470800E+03,0.120000E+00,0.390000E-04/)
XSTROATM (11,:) = (/10.0, 286.00,237.00,0.420200E+03,0.500000E-01,0.390000E-04/)
XSTROATM (12,:) = (/11.0, 247.00,230.00,0.374000E+03,0.170000E-01,0.410000E-04/)
XSTROATM (13,:) = (/12.0, 213.00,224.00,0.331600E+03,0.600000E-02,0.430000E-04/)
XSTROATM (14,:) = (/13.0, 182.00,217.00,0.292900E+03,0.180000E-02,0.450000E-04/)
XSTROATM (15,:) = (/14.0, 156.00,210.00,0.257800E+03,0.100000E-02,0.450000E-04/)
XSTROATM (16,:) = (/15.0, 132.00,204.00,0.226000E+03,0.760000E-03,0.470000E-04/)
XSTROATM (17,:) = (/16.0, 111.00,197.00,0.197200E+03,0.640000E-03,0.470000E-04/)
XSTROATM (18,:) = (/17.0,  93.70,195.00,0.167600E+03,0.560000E-03,0.690000E-04/)
XSTROATM (19,:) = (/18.0,  78.90,199.00,0.138200E+03,0.500000E-03,0.900000E-04/)
XSTROATM (20,:) = (/19.0,  66.60,203.00,0.114500E+03,0.490000E-03,0.140000E-03/)
XSTROATM (21,:) = (/20.0,  56.50,207.00,0.951500E+02,0.450000E-03,0.190000E-03/)
XSTROATM (22,:) = (/21.0,  48.00,211.00,0.793800E+02,0.510000E-03,0.240000E-03/)
XSTROATM (23,:) = (/22.0,  40.90,215.00,0.664500E+02,0.510000E-03,0.280000E-03/)
XSTROATM (24,:) = (/23.0,  35.00,217.00,0.561800E+02,0.540000E-03,0.320000E-03/)
XSTROATM (25,:) = (/24.0,  30.00,219.00,0.476300E+02,0.600000E-03,0.340000E-03/)
XSTROATM (26,:) = (/25.0,  25.70,221.00,0.404500E+02,0.670000E-03,0.340000E-03/)
XSTROATM (27,:) = (/30.0,  12.20,232.00,0.183100E+02,0.360000E-03,0.240000E-03/)
XSTROATM (28,:) = (/35.0,   6.00,243.00,0.860000E+01,0.110000E-03,0.920000E-04/)
XSTROATM (29,:) = (/40.0,   3.05,254.00,0.418100E+01,0.430000E-04,0.410000E-04/)
XSTROATM (30,:) = (/45.0,   1.59,265.00,0.209700E+01,0.190000E-04,0.130000E-04/)
XSTROATM (31,:) = (/50.0,   0.85,270.00,0.110100E+01,0.630000E-05,0.430000E-05/)
!
!  Standard Mid-Latitudes Summer ATMosphere
!
XSMLSATM( 1,:) = (/ 0.0,1013.00,294.00,0.119100E+04,0.140000E+02,0.600000E-04/)
XSMLSATM( 2,:) = (/ 1.0, 902.00,290.00,0.108000E+04,0.930000E+01,0.600000E-04/)
XSMLSATM( 3,:) = (/ 2.0, 802.00,285.00,0.975700E+03,0.590000E+01,0.600000E-04/)
XSMLSATM( 4,:) = (/ 3.0, 710.00,279.00,0.884600E+03,0.330000E+01,0.620000E-04/)
XSMLSATM( 5,:) = (/ 4.0, 628.00,273.00,0.799800E+03,0.190000E+01,0.640000E-04/)
XSMLSATM( 6,:) = (/ 5.0, 554.00,267.00,0.721100E+03,0.100000E+01,0.660000E-04/)
XSMLSATM( 7,:) = (/ 6.0, 487.00,261.00,0.648700E+03,0.610000E+00,0.690000E-04/)
XSMLSATM( 8,:) = (/ 7.0, 426.00,255.00,0.583000E+03,0.370000E+00,0.750000E-04/)
XSMLSATM( 9,:) = (/ 8.0, 372.00,248.00,0.522500E+03,0.210000E+00,0.790000E-04/)
XSMLSATM(10,:) = (/ 9.0, 324.00,242.00,0.466900E+03,0.120000E+00,0.860000E-04/)
XSMLSATM(11,:) = (/10.0, 281.00,235.00,0.415900E+03,0.640000E-01,0.900000E-04/)
XSMLSATM(12,:) = (/11.0, 243.00,229.00,0.369300E+03,0.220000E-01,0.110000E-03/)
XSMLSATM(13,:) = (/12.0, 209.00,222.00,0.326900E+03,0.600000E-02,0.120000E-03/)
XSMLSATM(14,:) = (/13.0, 179.00,216.00,0.288200E+03,0.180000E-02,0.150000E-03/)
XSMLSATM(15,:) = (/14.0, 153.00,216.00,0.246400E+03,0.100000E-02,0.180000E-03/)
XSMLSATM(16,:) = (/15.0, 130.00,216.00,0.210400E+03,0.760000E-03,0.190000E-03/)
XSMLSATM(17,:) = (/16.0, 111.00,216.00,0.179700E+03,0.640000E-03,0.210000E-03/)
XSMLSATM(18,:) = (/17.0,  95.00,216.00,0.153500E+03,0.560000E-03,0.240000E-03/)
XSMLSATM(19,:) = (/18.0,  81.20,216.00,0.130500E+03,0.500000E-03,0.280000E-03/)
XSMLSATM(20,:) = (/19.0,  69.50,217.00,0.111000E+03,0.490000E-03,0.320000E-03/)
XSMLSATM(21,:) = (/20.0,  59.50,218.00,0.945300E+02,0.450000E-03,0.340000E-03/)
XSMLSATM(22,:) = (/21.0,  51.00,219.00,0.805600E+02,0.510000E-03,0.360000E-03/)
XSMLSATM(23,:) = (/22.0,  43.70,220.00,0.687200E+02,0.510000E-03,0.360000E-03/)
XSMLSATM(24,:) = (/23.0,  37.60,222.00,0.586700E+02,0.540000E-03,0.340000E-03/)
XSMLSATM(25,:) = (/24.0,  32.20,223.00,0.501400E+02,0.600000E-03,0.320000E-03/)
XSMLSATM(26,:) = (/25.0,  27.70,224.00,0.428800E+02,0.670000E-03,0.300000E-03/)
XSMLSATM(27,:) = (/30.0,  13.20,234.00,0.132200E+02,0.360000E-03,0.200000E-03/)
XSMLSATM(28,:) = (/35.0,   6.52,245.00,0.651900E+01,0.110000E-03,0.920000E-04/)
XSMLSATM(29,:) = (/40.0,   3.33,258.00,0.333000E+01,0.430000E-04,0.410000E-04/)
XSMLSATM(30,:) = (/45.0,   1.76,270.60,0.175700E+01,0.190000E-04,0.130000E-04/)
XSMLSATM(31,:) = (/50.0,   0.95,276.00,0.951200E+00,0.630000E-05,0.430000E-05/)
!
!  Standard Mid-Latitudes Winter ATMosphere
!
XSMLWATM( 1,:) = (/ 0.0,1018.00,272.20,0.130100E+04,0.350000E+01,0.600000E-04/)
XSMLWATM( 2,:) = (/ 1.0, 897.30,268.70,0.116200E+04,0.250000E+01,0.540000E-04/)
XSMLWATM( 3,:) = (/ 2.0, 789.70,265.20,0.103700E+04,0.180000E+01,0.490000E-04/)
XSMLWATM( 4,:) = (/ 3.0, 693.80,261.70,0.923000E+03,0.120000E+01,0.490000E-04/)
XSMLWATM( 5,:) = (/ 4.0, 608.10,255.70,0.828200E+03,0.660000E+00,0.490000E-04/)
XSMLWATM( 6,:) = (/ 5.0, 531.30,249.70,0.741100E+03,0.380000E+00,0.580000E-04/)
XSMLWATM( 7,:) = (/ 6.0, 462.70,243.70,0.661400E+03,0.210000E+00,0.640000E-04/)
XSMLWATM( 8,:) = (/ 7.0, 401.60,237.70,0.588600E+03,0.850000E-01,0.770000E-04/)
XSMLWATM( 9,:) = (/ 8.0, 347.30,231.70,0.522200E+03,0.350000E-01,0.900000E-04/)
XSMLWATM(10,:) = (/ 9.0, 299.20,225.70,0.461900E+03,0.160000E-01,0.120000E-03/)
XSMLWATM(11,:) = (/10.0, 256.80,219.70,0.407200E+03,0.750000E-02,0.160000E-03/)
XSMLWATM(12,:) = (/11.0, 219.90,219.20,0.349600E+03,0.690000E-02,0.210000E-03/)
XSMLWATM(13,:) = (/12.0, 188.20,218.70,0.299900E+03,0.600000E-02,0.260000E-03/)
XSMLWATM(14,:) = (/13.0, 161.00,218.20,0.257200E+03,0.180000E-02,0.300000E-03/)
XSMLWATM(15,:) = (/14.0, 137.80,217.70,0.220600E+03,0.100000E-02,0.320000E-03/)
XSMLWATM(16,:) = (/15.0, 117.80,217.20,0.189000E+03,0.760000E-03,0.340000E-03/)
XSMLWATM(17,:) = (/16.0, 100.70,216.70,0.162000E+03,0.640000E-03,0.360000E-03/)
XSMLWATM(18,:) = (/17.0,  86.10,216.20,0.138800E+03,0.560000E-03,0.390000E-03/)
XSMLWATM(19,:) = (/18.0,  73.50,215.70,0.118800E+03,0.500000E-03,0.410000E-03/)
XSMLWATM(20,:) = (/19.0,  62.80,215.20,0.101700E+03,0.490000E-03,0.430000E-03/)
XSMLWATM(21,:) = (/20.0,  53.70,215.20,0.869000E+02,0.450000E-03,0.450000E-03/)
XSMLWATM(22,:) = (/21.0,  45.80,215.20,0.742100E+02,0.510000E-03,0.430000E-03/)
XSMLWATM(23,:) = (/22.0,  39.10,215.20,0.633800E+02,0.510000E-03,0.430000E-03/)
XSMLWATM(24,:) = (/23.0,  33.40,215.20,0.541500E+02,0.540000E-03,0.390000E-03/)
XSMLWATM(25,:) = (/24.0,  28.60,215.20,0.462400E+02,0.600000E-03,0.360000E-03/)
XSMLWATM(26,:) = (/25.0,  24.30,215.20,0.395000E+02,0.670000E-03,0.340000E-03/)
XSMLWATM(27,:) = (/30.0,  11.10,217.40,0.178300E+02,0.360000E-03,0.190000E-03/)
XSMLWATM(28,:) = (/35.0,   5.18,227.80,0.792400E+01,0.110000E-03,0.920000E-04/)
XSMLWATM(29,:) = (/40.0,   2.53,243.20,0.362500E+01,0.430000E-04,0.410000E-04/)
XSMLWATM(30,:) = (/45.0,   1.29,258.50,0.174100E+01,0.190000E-04,0.130000E-04/)
XSMLWATM(31,:) = (/50.0,   0.68,265.70,0.895400E+00,0.630000E-05,0.430000E-05/)
!
!  Standard POlar Summer ATMosphere
!
XSPOSATM( 1,:) = (/ 0.0,1010.00,287.00,0.122000E+04,0.910000E+01,0.490000E-04/)
XSPOSATM( 2,:) = (/ 1.0, 896.00,282.00,0.111000E+04,0.600000E+01,0.540000E-04/)
XSPOSATM( 3,:) = (/ 2.0, 792.90,276.00,0.997100E+03,0.420000E+01,0.560000E-04/)
XSPOSATM( 4,:) = (/ 3.0, 700.00,271.00,0.898500E+03,0.270000E+01,0.580000E-04/)
XSPOSATM( 5,:) = (/ 4.0, 616.00,266.00,0.807700E+03,0.170000E+01,0.600000E-04/)
XSPOSATM( 6,:) = (/ 5.0, 541.00,260.00,0.724400E+03,0.100000E+01,0.640000E-04/)
XSPOSATM( 7,:) = (/ 6.0, 473.00,253.00,0.651900E+03,0.540000E+00,0.710000E-04/)
XSPOSATM( 8,:) = (/ 7.0, 413.00,246.00,0.584900E+03,0.290000E+00,0.750000E-04/)
XSPOSATM( 9,:) = (/ 8.0, 359.00,239.00,0.523100E+03,0.130000E+00,0.790000E-04/)
XSPOSATM(10,:) = (/ 9.0, 310.70,232.00,0.466300E+03,0.420000E-01,0.110000E-03/)
XSPOSATM(11,:) = (/10.0, 267.70,225.00,0.414200E+03,0.150000E-01,0.130000E-03/)
XSPOSATM(12,:) = (/11.0, 230.00,225.00,0.355900E+03,0.940000E-02,0.180000E-03/)
XSPOSATM(13,:) = (/12.0, 197.70,225.00,0.305900E+03,0.600000E-02,0.210000E-03/)
XSPOSATM(14,:) = (/13.0, 170.00,225.00,0.263000E+03,0.180000E-02,0.260000E-03/)
XSPOSATM(15,:) = (/14.0, 146.00,225.00,0.226000E+03,0.100000E-02,0.280000E-03/)
XSPOSATM(16,:) = (/15.0, 125.00,225.00,0.194300E+03,0.760000E-03,0.320000E-03/)
XSPOSATM(17,:) = (/16.0, 108.00,225.00,0.167100E+03,0.640000E-03,0.340000E-03/)
XSPOSATM(18,:) = (/17.0,  92.80,225.00,0.143600E+03,0.560000E-03,0.390000E-03/)
XSPOSATM(19,:) = (/18.0,  79.80,225.00,0.123500E+03,0.500000E-03,0.410000E-03/)
XSPOSATM(20,:) = (/19.0,  68.60,225.00,0.106200E+03,0.490000E-03,0.410000E-03/)
XSPOSATM(21,:) = (/20.0,  58.90,225.00,0.912800E+02,0.450000E-03,0.390000E-03/)
XSPOSATM(22,:) = (/21.0,  50.70,225.00,0.784900E+02,0.510000E-03,0.360000E-03/)
XSPOSATM(23,:) = (/22.0,  43.60,225.00,0.675000E+02,0.510000E-03,0.320000E-03/)
XSPOSATM(24,:) = (/23.0,  37.50,225.00,0.580500E+02,0.540000E-03,0.300000E-03/)
XSPOSATM(25,:) = (/24.0,  32.27,226.00,0.496300E+02,0.600000E-03,0.280000E-03/)
XSPOSATM(26,:) = (/25.0,  27.80,228.00,0.424700E+02,0.670000E-03,0.260000E-03/)
XSPOSATM(27,:) = (/30.0,  13.40,235.00,0.133800E+02,0.360000E-03,0.140000E-03/)
XSPOSATM(28,:) = (/35.0,   6.61,247.00,0.661400E+01,0.110000E-03,0.920000E-04/)
XSPOSATM(29,:) = (/40.0,   3.40,262.00,0.340400E+01,0.430000E-04,0.410000E-04/)
XSPOSATM(30,:) = (/45.0,   1.81,274.00,0.181700E+01,0.190000E-04,0.130000E-04/)
XSPOSATM(31,:) = (/50.0,   0.99,277.00,0.986800E+00,0.630000E-05,0.430000E-05/)
!
!  Standard POlar Winter ATMosphere
!
XSPOWATM( 1,:) = (/ 0.0,1013.00,257.10,0.137200E+04,0.120000E+01,0.410000E-04/)
XSPOWATM( 2,:) = (/ 1.0, 887.80,259.10,0.119300E+04,0.120000E+01,0.410000E-04/)
XSPOWATM( 3,:) = (/ 2.0, 777.50,255.90,0.105800E+04,0.940000E+00,0.410000E-04/)
XSPOWATM( 4,:) = (/ 3.0, 679.80,252.70,0.936600E+03,0.680000E+00,0.430000E-04/)
XSPOWATM( 5,:) = (/ 4.0, 593.20,247.70,0.833900E+03,0.410000E+00,0.450000E-04/)
XSPOWATM( 6,:) = (/ 5.0, 515.80,240.90,0.745700E+03,0.200000E+00,0.470000E-04/)
XSPOWATM( 7,:) = (/ 6.0, 446.70,234.10,0.664600E+03,0.980000E-01,0.490000E-04/)
XSPOWATM( 8,:) = (/ 7.0, 385.30,227.30,0.590400E+03,0.540000E-01,0.710000E-04/)
XSPOWATM( 9,:) = (/ 8.0, 330.80,220.60,0.522600E+03,0.110000E-01,0.900000E-04/)
XSPOWATM(10,:) = (/ 9.0, 282.90,217.20,0.453800E+03,0.840000E-02,0.160000E-03/)
XSPOWATM(11,:) = (/10.0, 241.80,217.20,0.387900E+03,0.550000E-02,0.240000E-03/)
XSPOWATM(12,:) = (/11.0, 206.70,217.20,0.331500E+03,0.380000E-02,0.320000E-03/)
XSPOWATM(13,:) = (/12.0, 176.60,217.20,0.283400E+03,0.260000E-02,0.430000E-03/)
XSPOWATM(14,:) = (/13.0, 151.00,217.20,0.242200E+03,0.180000E-02,0.470000E-03/)
XSPOWATM(15,:) = (/14.0, 129.10,217.20,0.207100E+03,0.100000E-02,0.490000E-03/)
XSPOWATM(16,:) = (/15.0, 110.30,217.20,0.177000E+03,0.760000E-03,0.560000E-03/)
XSPOWATM(17,:) = (/16.0,  94.31,216.60,0.151700E+03,0.640000E-03,0.620000E-03/)
XSPOWATM(18,:) = (/17.0,  80.58,216.00,0.130000E+03,0.560000E-03,0.620000E-03/)
XSPOWATM(19,:) = (/18.0,  68.82,215.40,0.111300E+03,0.500000E-03,0.620000E-03/)
XSPOWATM(20,:) = (/19.0,  58.75,214.80,0.952900E+02,0.490000E-03,0.600000E-03/)
XSPOWATM(21,:) = (/20.0,  50.14,214.10,0.815500E+02,0.450000E-03,0.560000E-03/)
XSPOWATM(22,:) = (/21.0,  42.77,213.60,0.697600E+02,0.510000E-03,0.510000E-03/)
XSPOWATM(23,:) = (/22.0,  36.47,213.00,0.596600E+02,0.510000E-03,0.470000E-03/)
XSPOWATM(24,:) = (/23.0,  31.09,212.40,0.510000E+02,0.540000E-03,0.430000E-03/)
XSPOWATM(25,:) = (/24.0,  26.49,211.80,0.435800E+02,0.600000E-03,0.360000E-03/)
XSPOWATM(26,:) = (/25.0,  22.56,211.20,0.372200E+02,0.670000E-03,0.320000E-03/)
XSPOWATM(27,:) = (/30.0,  10.20,216.00,0.164500E+02,0.360000E-03,0.150000E-03/)
XSPOWATM(28,:) = (/35.0,   4.70,222.20,0.736800E+01,0.110000E-03,0.920000E-04/)
XSPOWATM(29,:) = (/40.0,   2.24,234.70,0.333000E+01,0.430000E-04,0.410000E-04/)
XSPOWATM(30,:) = (/45.0,   1.11,247.00,0.156900E+01,0.190000E-04,0.130000E-04/)
XSPOWATM(31,:) = (/50.0,   0.57,259.00,0.768200E+00,0.630000E-05,0.430000E-05/)
!
END SUBROUTINE INI_STAND_ATM
