!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! ECMWF_RAD2 2003/02/19 13:36:46
!-----------------------------------------------------------------
SUBROUTINE SUECOZO (PYTIME)

!**** *SUECOZO* - FOR THE YEARLY CYCLE OF THE OZONE DISTIBUTION.

!     J.F.GELEYN     E.C.M.W.F.     03/06/82.

!     J.-J. MORCRETTE               93/03/22   ADAPTATION TO I.F.S.

!     PURPOSE.
!     --------

!          THIS ROUTINE COMPUTES INSTANTANEOUS VALUES OF A T5 SPETRAL
!     DISTRIBUTION FOR TWO OZONE PARAMETERS (TOTAL QUANTITY AND PRESSURE
!     AT THE MAXIMUM OF CONCENTRATION,BOTH IN *PASCAL) FROM THE TIME
!     OF THE YEAR (SEE *UPDTIER*).

!**   INTERFACE.
!     ----------

!          *OZONE* IS CALLED FROM *PHYSC* AT THE FIRST LATITUDE ROW AT
!     THE TIME OF A FULL RADIATION COMPUTATION.
!          THERE ARE FIVE DUMMY ARGUMENTS: *PYTIME* IS THE TIME OF THE
!     YEAR (IN RADIANS).
!                                          *POZQC*, *POZQS*, *POZHC* AND
!     *POZHS* ARE ARRAYS FOR THE T5 DISTRIBUTIONS (*Q FOR QUANTITY, *H
!     FOR HEIGHT, *C FOR COSINE AND *S FOR SINE).

!     METHOD.
!     -------

!          STAIGHTFORWARD, A SECOND ORDER *FOURIER DEVELOPMENT FOR THE
!     TIME OF THE YEAR.

!     EXTERNALS.
!     ----------

!          NONE.

!     REFERENCE.
!     ----------

!          NONE.

!     ------------------------------------------------------------------

#include "tsmbkind.h"

USE YOEOZOC  , ONLY : COZQC    ,COZQS    ,COZHC    ,COZHS


IMPLICIT NONE


!     DUMMY REAL SCALARS
REAL_B :: PYTIME


!     ------------------------------------------------------------------

!*    DATA STATEMENTS.
!     ---- -----------

!          *ZOZ Q/H C/S N* (N=0,4) CORRESPONDS TO THE *POZ Q/H C/S*
!     (SEE ABOVE) AND TO THE FIVE TERMS OF THE *FOURIER DEVELOPMENT.

REAL_B ,DIMENSION(21) :: ZOZQC0=(/&
&+.6012E-01_JPRB,+.1887E-02_JPRB,+.7410E-02_JPRB,+.9950E-03_JPRB,-.1426E-02_JPRB,-.2072E-03_JPRB,&
          &-.4954E-03_JPRB,+.7955E-05_JPRB,-.3701E-03_JPRB,+.4116E-04_JPRB,-.4163E-04_JPRB,&
                     &-.2933E-03_JPRB,+.2154E-04_JPRB,-.2849E-03_JPRB,-.1604E-03_JPRB,&
                                &-.1054E-03_JPRB,+.4974E-03_JPRB,+.1047E-03_JPRB,&
                                           &+.8323E-04_JPRB,+.2874E-03_JPRB,&
                                                      &+.1333E-03_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZQS0=(/&
           &+.4210E-03_JPRB,-.9591E-03_JPRB,+.2811E-03_JPRB,-.2257E-03_JPRB,-.1713E-03_JPRB,&
                      &-.3538E-03_JPRB,+.1095E-03_JPRB,-.4390E-03_JPRB,-.5605E-05_JPRB,&
                                 &+.1478E-03_JPRB,+.2849E-03_JPRB,+.3430E-03_JPRB,&
                                            &+.8248E-04_JPRB,+.1442E-03_JPRB,&
                                                      &-.1375E-04_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZHC0=(/&
&+.3166E+04_JPRB,+.8663E+02_JPRB,+.9401E+03_JPRB,+.1999E+02_JPRB,-.3530E+03_JPRB,-.3311E+02_JPRB,&
           &-.4903E+02_JPRB,-.4015E+00_JPRB,-.1333E+02_JPRB,+.5675E+01_JPRB,+.7221E+01_JPRB,&
                      &-.3001E+02_JPRB,+.7570E+01_JPRB,-.1142E+02_JPRB,-.1365E+02_JPRB,&
                                 &-.1502E+02_JPRB,+.4911E+02_JPRB,+.1425E+02_JPRB,&
                                            &+.8983E+01_JPRB,+.3064E+02_JPRB,&
                                                      &+.1693E+02_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZHS0=(/&
           &+.4231E+02_JPRB,-.7391E+02_JPRB,+.1273E+02_JPRB,+.2086E+02_JPRB,-.1597E+02_JPRB,&
                      &-.3591E+02_JPRB,+.1059E+02_JPRB,-.2779E+02_JPRB,-.6923E+01_JPRB,&
                                 &+.1397E+02_JPRB,+.2387E+02_JPRB,+.2883E+02_JPRB,&
                                            &+.8626E+01_JPRB,+.1607E+02_JPRB,&
                                                      &-.2676E+01_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZQC1=(/&
&+.7090E-04_JPRB,+.4930E-05_JPRB,+.6829E-03_JPRB,+.1897E-03_JPRB,+.7226E-04_JPRB,-.2807E-03_JPRB,&
           &+.4970E-04_JPRB,-.1753E-03_JPRB,-.7843E-04_JPRB,-.1649E-03_JPRB,-.1037E-03_JPRB,&
                      &-.4830E-04_JPRB,-.6304E-04_JPRB,-.1100E-03_JPRB,-.7952E-04_JPRB,&
                                 &+.1326E-04_JPRB,+.2599E-04_JPRB,+.9926E-05_JPRB,&
                                            &-.9247E-05_JPRB,-.3521E-05_JPRB,&
                                                      &-.1780E-04_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZQS1=(/&
           &+.6333E-04_JPRB,+.1145E-03_JPRB,+.1192E-03_JPRB,+.4934E-04_JPRB,+.2699E-04_JPRB,&
                      &+.3684E-04_JPRB,-.2395E-05_JPRB,+.2045E-04_JPRB,-.8684E-04_JPRB,&
                                 &+.5301E-04_JPRB,-.4176E-05_JPRB,+.4103E-04_JPRB,&
                                            &+.2783E-04_JPRB,+.1754E-04_JPRB,&
                                                      &+.1116E-04_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZHC1=(/&
&-.3450E+02_JPRB,+.2148E+03_JPRB,+.3376E+02_JPRB,+.6535E+02_JPRB,-.1564E+02_JPRB,-.4273E+02_JPRB,&
           &+.9553E+01_JPRB,-.4647E+01_JPRB,-.6129E+01_JPRB,-.6727E+01_JPRB,-.6761E+01_JPRB,&
                      &-.2467E+01_JPRB,-.2181E+01_JPRB,-.5361E+01_JPRB,-.2395E+01_JPRB,&
                                 &+.5952E+00_JPRB,+.2106E+01_JPRB,-.1367E+01_JPRB,&
                                            &-.2349E+01_JPRB,+.3532E+00_JPRB,&
                                                      &-.3169E+01_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZHS1=(/&
           &+.3977E+01_JPRB,+.5032E+01_JPRB,+.6226E+01_JPRB,-.3625E+00_JPRB,-.1373E+01_JPRB,&
                      &+.4600E+01_JPRB,+.4312E+01_JPRB,+.2882E+01_JPRB,-.6351E+01_JPRB,&
                                 &+.5731E+01_JPRB,-.2574E+01_JPRB,+.3235E+00_JPRB,&
                                            &+.2806E+01_JPRB,+.8133E+00_JPRB,&
                                                      &+.2032E+01_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZQC2=(/&
&+.8571E-03_JPRB,+.3086E-02_JPRB,+.9287E-03_JPRB,+.2787E-03_JPRB,+.1826E-03_JPRB,-.1006E-03_JPRB,&
           &+.1092E-03_JPRB,-.1266E-03_JPRB,+.5372E-04_JPRB,-.1188E-03_JPRB,-.3285E-04_JPRB,&
                      &-.1783E-04_JPRB,-.3018E-05_JPRB,-.8709E-04_JPRB,-.8707E-04_JPRB,&
                                 &+.8633E-04_JPRB,+.3530E-04_JPRB,+.4863E-04_JPRB,&
                                            &+.3917E-05_JPRB,-.3252E-04_JPRB,&
                                                      &-.1936E-06_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZQS2=(/&
           &-.8822E-04_JPRB,+.1341E-03_JPRB,+.3095E-04_JPRB,+.8230E-04_JPRB,+.2735E-04_JPRB,&
                      &+.1714E-04_JPRB,-.9406E-04_JPRB,+.1912E-04_JPRB,-.5402E-04_JPRB,&
                                 &+.3571E-04_JPRB,+.3897E-04_JPRB,+.4487E-04_JPRB,&
                                            &+.3079E-04_JPRB,+.3196E-04_JPRB,&
                                                      &-.2391E-05_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZHC2=(/&
&+.5216E+02_JPRB,+.1613E+03_JPRB,+.3284E+02_JPRB,-.7670E+02_JPRB,-.9548E+01_JPRB,+.1608E+02_JPRB,&
           &+.1023E+02_JPRB,-.1090E+02_JPRB,+.2748E+01_JPRB,-.3846E+01_JPRB,-.4135E+01_JPRB,&
                      &+.1255E+01_JPRB,-.3301E-01_JPRB,-.5273E+01_JPRB,-.7247E+01_JPRB,&
                                 &+.1387E+02_JPRB,+.4184E+01_JPRB,+.6495E+01_JPRB,&
                                            &+.2944E+01_JPRB,-.1947E+01_JPRB,&
                                                      &+.1132E+01_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZHS2=(/&
           &-.1968E+02_JPRB,+.1192E+02_JPRB,-.1194E+01_JPRB,+.1084E+01_JPRB,+.2946E+01_JPRB,&
                      &+.2630E+01_JPRB,-.1256E+02_JPRB,+.1395E+01_JPRB,-.2222E+01_JPRB,&
                                 &+.4864E+01_JPRB,+.6450E+01_JPRB,+.5568E+01_JPRB,&
                                            &+.5292E+01_JPRB,+.4876E+01_JPRB,&
                                                      &-.7579E+00_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZQC3=(/&
&-.2759E-03_JPRB,-.2781E-03_JPRB,-.1087E-03_JPRB,-.1633E-03_JPRB,-.3627E-04_JPRB,-.4242E-04_JPRB,&
           &+.6045E-05_JPRB,-.1703E-04_JPRB,+.4562E-04_JPRB,-.1009E-04_JPRB,+.2663E-04_JPRB,&
                      &-.1786E-04_JPRB,+.1550E-04_JPRB,-.9135E-06_JPRB,+.2372E-04_JPRB,&
                                 &+.1100E-05_JPRB,+.2299E-04_JPRB,+.4659E-05_JPRB,&
                                            &+.2423E-05_JPRB,+.7321E-05_JPRB,&
                                                      &+.8852E-05_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZQS3=(/&
           &-.3678E-04_JPRB,-.2219E-04_JPRB,-.3911E-04_JPRB,-.4398E-04_JPRB,-.1142E-04_JPRB,&
                      &-.9121E-05_JPRB,-.2011E-04_JPRB,+.4711E-06_JPRB,-.3775E-05_JPRB,&
                                 &+.3866E-05_JPRB,+.2400E-04_JPRB,+.2043E-04_JPRB,&
                                            &-.1824E-05_JPRB,-.5550E-05_JPRB,&
                                                      &+.2506E-05_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZHC3=(/&
&-.1534E+03_JPRB,-.2095E+02_JPRB,-.1006E+03_JPRB,-.7385E+01_JPRB,+.5203E+01_JPRB,+.9434E+00_JPRB,&
           &-.3814E+00_JPRB,-.3175E+01_JPRB,+.3366E+01_JPRB,+.3378E+00_JPRB,+.2740E+00_JPRB,&
                      &-.2669E+01_JPRB,+.8452E+00_JPRB,+.3498E+00_JPRB,+.2192E+01_JPRB,&
                                 &-.4024E+00_JPRB,+.1544E+01_JPRB,-.4588E+00_JPRB,&
                                            &+.6998E+00_JPRB,+.6263E+00_JPRB,&
                                                      &+.1228E+01_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZHS3=(/&
           &-.3588E+01_JPRB,+.2076E+00_JPRB,-.2088E+01_JPRB,-.4159E+01_JPRB,+.2244E+00_JPRB,&
                      &-.7751E+00_JPRB,-.2749E+01_JPRB,+.7234E+00_JPRB,+.4390E+00_JPRB,&
                                 &-.1646E+00_JPRB,+.1700E+01_JPRB,+.1046E+01_JPRB,&
                                            &-.7856E+00_JPRB,-.1644E+01_JPRB,&
                                                      &+.2648E+00_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZQC4=(/&
&-.1460E-03_JPRB,+.3422E-03_JPRB,-.3529E-04_JPRB,+.1791E-03_JPRB,-.1917E-03_JPRB,-.2558E-04_JPRB,&
           &+.6547E-04_JPRB,+.6401E-04_JPRB,+.4823E-04_JPRB,+.7084E-05_JPRB,+.2895E-04_JPRB,&
                      &-.1561E-04_JPRB,+.8179E-06_JPRB,+.1028E-04_JPRB,-.7667E-05_JPRB,&
                                 &-.4347E-05_JPRB,+.7293E-05_JPRB,-.5735E-05_JPRB,&
                                            &+.7838E-05_JPRB,-.2933E-05_JPRB,&
                                                      &+.3686E-05_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZQS4=(/&
           &-.4560E-05_JPRB,-.5292E-04_JPRB,-.1252E-04_JPRB,+.1850E-04_JPRB,-.2273E-04_JPRB,&
                      &+.6552E-05_JPRB,+.1422E-04_JPRB,-.6545E-05_JPRB,+.7998E-06_JPRB,&
                                 &+.2845E-04_JPRB,+.2497E-04_JPRB,+.2844E-04_JPRB,&
                                            &+.3855E-06_JPRB,-.1487E-04_JPRB,&
                                                      &+.1954E-05_JPRB/)
REAL_B ,DIMENSION(21) :: ZOZHC4=(/&
&+.9260E+01_JPRB,-.9055E+01_JPRB,+.5460E+01_JPRB,-.7603E+01_JPRB,-.3329E+02_JPRB,-.1048E+02_JPRB,&
           &+.9328E+01_JPRB,+.4597E+01_JPRB,+.3827E+01_JPRB,-.3201E+01_JPRB,+.1708E+01_JPRB,&
                      &-.1548E+01_JPRB,-.5323E+00_JPRB,+.3039E+01_JPRB,+.5740E+00_JPRB,&
                                 &+.1353E+00_JPRB,-.2354E+01_JPRB,+.2818E+00_JPRB,&
                                            &+.1113E+01_JPRB,-.1891E+01_JPRB,&
                                                      &-.3074E+00_JPRB/)
REAL_B ,DIMENSION(15) :: ZOZHS4=(/&
           &-.2446E+01_JPRB,+.4199E+01_JPRB,-.2571E+01_JPRB,+.8194E+01_JPRB,+.4206E+00_JPRB,&
                      &+.3856E+01_JPRB,+.1159E+01_JPRB,+.2547E+01_JPRB,-.1314E+01_JPRB,&
                                 &+.2331E+01_JPRB,+.1144E+01_JPRB,-.4408E+00_JPRB,&
                                            &-.6797E+00_JPRB,-.2598E+01_JPRB,&
                                                      &+.8953E+00_JPRB/)

!     LOCAL INTEGER SCALARS
INTEGER_M :: JMN

!     LOCAL REAL SCALARS
REAL_B :: ZC1YT, ZC2YT, ZS1YT, ZS2YT, ZYTIME


!     ------------------------------------------------------------------

!*         1.     PRELIMINARY SETTING.
!                 ----------- --------


ZYTIME=PYTIME

!     ------------------------------------------------------------------

!*         2.     COMPUTATIONS.
!                 -------------


ZC1YT=COS(ZYTIME)
ZS1YT=SIN(ZYTIME)
ZC2YT=ZC1YT**2-ZS1YT**2
ZS2YT=_TWO_*ZS1YT*ZC1YT
DO JMN=1,21
  COZQC(JMN)=ZOZQC0(JMN)+_TWO_*(ZOZQC1(JMN)*ZC1YT+ZOZQC2(JMN)*ZS1YT &
   &+ZOZQC3(JMN)*ZC2YT+ZOZQC4(JMN)*ZS2YT)
  COZHC(JMN)=ZOZHC0(JMN)+_TWO_*(ZOZHC1(JMN)*ZC1YT+ZOZHC2(JMN)*ZS1YT &
   &+ZOZHC3(JMN)*ZC2YT+ZOZHC4(JMN)*ZS2YT)
ENDDO
DO JMN=1,15
  COZQS(JMN)=ZOZQS0(JMN)+_TWO_*(ZOZQS1(JMN)*ZC1YT+ZOZQS2(JMN)*ZS1YT &
   &+ZOZQS3(JMN)*ZC2YT+ZOZQS4(JMN)*ZS2YT)
  COZHS(JMN)=ZOZHS0(JMN)+_TWO_*(ZOZHS1(JMN)*ZC1YT+ZOZHS2(JMN)*ZS1YT &
   &+ZOZHS3(JMN)*ZC2YT+ZOZHS4(JMN)*ZS2YT)
ENDDO
!     ------------------------------------------------------------------



RETURN
END SUBROUTINE SUECOZO
