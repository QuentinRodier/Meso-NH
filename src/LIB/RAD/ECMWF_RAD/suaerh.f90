!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! ECMWF_RAD2 2003/02/19 13:36:42
!-----------------------------------------------------------------
SUBROUTINE SUAERH


!**** *SUAERH* - SETS UP THE AEROSOL HORIZONTAL DISTRIBUTION

!     PURPOSE.
!     --------

!          THIS ROUTINE FETCHES VALUES OF A T10 SPECTRAL DISTRIBUTION
!     FOR FOUR AEROSOL TYPES OF DIFFERENT ORIGINS (SEA,LAND,URBAN AREAS
!     AND DESERTS). THE VALUES TO BE OBTAINED ARE BETWEEN ZERO AND ONE.

!**   INTERFACE.
!     ----------

!          THERE ARE EIGHT DUMMY ARGUMENTS: *PAESC*, *PAESS*, *PAELC*,
!     *PAELS*, *PAEUC*, *PAEUS*, *PAEDC* AND *PAEDS*ARE ARRAYS FOR THE
!     T10 DISTRIBUTIONS (*S FOR SEA, *L FOR LAND, *U FOR URBAN AND *D
!     FOR DESERT, *C FOR COSINE AND *S FOR SINE).

!     METHOD.
!     -------

!          NONE.

!     EXTERNALS.
!     ----------

!          NONE.

!     REFERENCE.
!     ----------

!          NONE.

!     AUTHOR
!     ------
!     J.-F. GELEYN      E.C.M.W.F.     29/09/82.

!     MODIFICATIONS
!     -------------
!     J.-J. MORCRETTE   E.C.M.W.F.     91/07/14   ADAPTATION TO I.F.S.

!     ------------------------------------------------------------------


#include "tsmbkind.h"

USE YOEAERD  , ONLY : RCAEROS  ,RAESC    ,RAESS    ,RAELC    ,&
            &RAELS    ,RAEUC    ,RAEUS    ,RAEDC    ,RAEDS


IMPLICIT NONE


!     ------------------------------------------------------------------

!*    DATA STATEMENTS.
!     ---- -----------

!          *RAE S/L/U/D C/S*  (SEE ABOVE).

RAESC = (/&
&+.6688E+00_JPRB,-.1172E+00_JPRB,-.1013E+00_JPRB,+.1636E-01_JPRB,-.3699E-01_JPRB,+.1775E-01_JPRB,&
     &-.9635E-02_JPRB,+.1290E-02_JPRB,+.4681E-04_JPRB,-.9106E-04_JPRB,+.9355E-04_JPRB,&
           &-.7076E-01_JPRB,-.1782E-01_JPRB,+.1856E-01_JPRB,+.1372E-01_JPRB,+.8210E-04_JPRB,&
     &+.2149E-02_JPRB,+.4856E-03_JPRB,+.2231E-03_JPRB,+.1824E-03_JPRB,+.1960E-05_JPRB,&
                      &+.2057E-01_JPRB,+.2703E-01_JPRB,+.2424E-01_JPRB,+.9716E-02_JPRB,&
     &+.1312E-02_JPRB,-.8846E-03_JPRB,-.3347E-03_JPRB,+.6231E-04_JPRB,+.6397E-04_JPRB,&
                                 &-.3341E-02_JPRB,-.1295E-01_JPRB,-.4598E-02_JPRB,&
     &+.3242E-03_JPRB,+.8122E-03_JPRB,-.2975E-03_JPRB,-.7757E-04_JPRB,+.7793E-04_JPRB,&
                                            &+.4455E-02_JPRB,-.1584E-01_JPRB,&
     &-.2551E-02_JPRB,+.1174E-02_JPRB,+.1335E-04_JPRB,+.5112E-04_JPRB,+.5605E-04_JPRB,&
                                                       &+.7412E-04_JPRB,&
     &+.1857E-02_JPRB,-.1917E-03_JPRB,+.4460E-03_JPRB,+.1767E-04_JPRB,-.5281E-04_JPRB,&
     &-.5043E-03_JPRB,+.2467E-03_JPRB,-.2497E-03_JPRB,-.2377E-04_JPRB,-.3954E-04_JPRB,&
                &+.2666E-03_JPRB,-.8186E-03_JPRB,-.1441E-03_JPRB,-.1904E-04_JPRB,&
                           &+.3337E-03_JPRB,-.1696E-03_JPRB,-.2503E-04_JPRB,&
                                      &+.1239E-03_JPRB,-.9983E-04_JPRB,&
                                                 &-.5283E-04_JPRB &
         &/)
RAESS = (/&
           &-.3374E-01_JPRB,-.3247E-01_JPRB,-.1012E-01_JPRB,+.6002E-02_JPRB,+.5190E-02_JPRB,&
     &+.7784E-03_JPRB,-.1090E-02_JPRB,+.3294E-03_JPRB,+.1719E-03_JPRB,-.5866E-05_JPRB,&
                      &-.4124E-03_JPRB,-.3742E-01_JPRB,-.5054E-02_JPRB,+.3430E-02_JPRB,&
     &+.5513E-03_JPRB,-.6235E-03_JPRB,+.2892E-03_JPRB,-.9730E-04_JPRB,+.7078E-04_JPRB,&
                                 &-.3300E-01_JPRB,+.5104E-03_JPRB,-.2156E-02_JPRB,&
     &-.3194E-02_JPRB,-.5079E-03_JPRB,-.5517E-03_JPRB,+.4632E-04_JPRB,+.5369E-04_JPRB,&
                                            &-.2731E-01_JPRB,+.5126E-02_JPRB,&
     &+.2241E-02_JPRB,-.5789E-03_JPRB,-.3048E-03_JPRB,-.1774E-03_JPRB,+.1946E-05_JPRB,&
                                                       &-.8247E-02_JPRB,&
     &+.2338E-02_JPRB,+.1021E-02_JPRB,+.1575E-04_JPRB,+.2612E-05_JPRB,+.1995E-04_JPRB,&
     &-.1319E-02_JPRB,+.1384E-02_JPRB,-.4159E-03_JPRB,-.2337E-03_JPRB,+.5764E-04_JPRB,&
                &+.1495E-02_JPRB,-.3727E-03_JPRB,+.6075E-04_JPRB,-.4642E-04_JPRB,&
                           &+.5368E-03_JPRB,-.7619E-04_JPRB,+.3774E-04_JPRB,&
                                      &+.1206E-03_JPRB,-.4104E-06_JPRB,&
                                                 &+.2158E-04_JPRB &
         &/)
RAELC = (/&
&+.1542E+00_JPRB,+.8245E-01_JPRB,-.1879E-03_JPRB,+.4864E-02_JPRB,-.5527E-02_JPRB,-.7966E-02_JPRB,&
     &-.2683E-02_JPRB,-.2011E-02_JPRB,-.8889E-03_JPRB,-.1058E-03_JPRB,-.1614E-04_JPRB,&
           &+.4206E-01_JPRB,+.1912E-01_JPRB,-.9476E-02_JPRB,-.6780E-02_JPRB,+.1767E-03_JPRB,&
     &-.5422E-03_JPRB,-.7753E-03_JPRB,-.2106E-03_JPRB,-.9870E-04_JPRB,-.1721E-04_JPRB,&
                      &-.9536E-02_JPRB,-.9580E-02_JPRB,-.1050E-01_JPRB,-.5747E-02_JPRB,&
     &-.1282E-02_JPRB,+.2248E-03_JPRB,+.1694E-03_JPRB,-.4782E-04_JPRB,-.2441E-04_JPRB,&
                                 &+.5781E-03_JPRB,+.6212E-02_JPRB,+.1921E-02_JPRB,&
     &-.1102E-02_JPRB,-.8145E-03_JPRB,+.2497E-03_JPRB,+.1539E-03_JPRB,-.2538E-04_JPRB,&
                                            &-.3993E-02_JPRB,+.9777E-02_JPRB,&
     &+.4837E-03_JPRB,-.1304E-02_JPRB,+.2417E-04_JPRB,-.1370E-04_JPRB,-.3731E-05_JPRB,&
                                                       &+.1922E-02_JPRB,&
     &-.5167E-03_JPRB,+.4295E-03_JPRB,-.1888E-03_JPRB,+.2427E-04_JPRB,+.4012E-04_JPRB,&
     &+.1529E-02_JPRB,-.2120E-03_JPRB,+.8166E-04_JPRB,+.2579E-04_JPRB,+.3488E-04_JPRB,&
                &+.2140E-03_JPRB,+.2274E-03_JPRB,-.3447E-05_JPRB,-.1075E-04_JPRB,&
                           &-.1018E-03_JPRB,+.2864E-04_JPRB,+.3442E-04_JPRB,&
                                      &-.1002E-03_JPRB,+.7117E-04_JPRB,&
                                                 &+.2045E-04_JPRB &
         &/)
RAELS = (/&
           &+.1637E-01_JPRB,+.1935E-01_JPRB,+.1080E-01_JPRB,+.2784E-02_JPRB,+.1606E-03_JPRB,&
     &+.1860E-02_JPRB,+.1263E-02_JPRB,-.2707E-03_JPRB,-.2290E-03_JPRB,-.9761E-05_JPRB,&
                      &-.7317E-02_JPRB,+.2465E-01_JPRB,+.6799E-02_JPRB,-.1913E-02_JPRB,&
     &+.1382E-02_JPRB,+.6691E-03_JPRB,+.1414E-03_JPRB,+.3527E-04_JPRB,-.5210E-04_JPRB,&
                                 &+.1873E-01_JPRB,+.2977E-02_JPRB,+.4650E-02_JPRB,&
     &+.2509E-02_JPRB,+.3680E-03_JPRB,+.1481E-03_JPRB,-.6594E-04_JPRB,-.5634E-04_JPRB,&
                                            &+.1592E-01_JPRB,-.1875E-02_JPRB,&
     &-.1093E-02_JPRB,+.3022E-03_JPRB,+.2625E-03_JPRB,+.3252E-04_JPRB,-.3803E-04_JPRB,&
                                                       &+.4218E-02_JPRB,&
     &-.1843E-02_JPRB,-.1351E-02_JPRB,-.2952E-03_JPRB,-.8171E-05_JPRB,-.1473E-04_JPRB,&
     &+.9076E-03_JPRB,-.1057E-02_JPRB,+.2676E-03_JPRB,+.1307E-03_JPRB,-.3628E-04_JPRB,&
                &-.9158E-03_JPRB,+.4335E-03_JPRB,+.2927E-04_JPRB,+.6602E-04_JPRB,&
                           &-.3570E-03_JPRB,+.5760E-04_JPRB,-.3465E-04_JPRB,&
                                      &-.8535E-04_JPRB,-.2011E-04_JPRB,&
                                                 &+.6612E-06_JPRB &
         &/)
RAEUC = (/&
&+.8005E-01_JPRB,+.7095E-01_JPRB,+.2014E-01_JPRB,-.1412E-01_JPRB,-.2425E-01_JPRB,-.1332E-01_JPRB,&
     &-.2904E-02_JPRB,+.5068E-03_JPRB,+.9369E-03_JPRB,+.4114E-03_JPRB,+.7549E-04_JPRB,&
           &+.1922E-01_JPRB,+.2534E-01_JPRB,+.2088E-01_JPRB,+.1064E-01_JPRB,+.1063E-02_JPRB,&
     &-.2526E-02_JPRB,-.2091E-02_JPRB,-.9660E-03_JPRB,-.2030E-03_JPRB,+.3865E-04_JPRB,&
                      &-.9900E-02_JPRB,-.5964E-02_JPRB,+.2223E-02_JPRB,+.4941E-02_JPRB,&
     &+.3277E-02_JPRB,+.1038E-02_JPRB,-.1480E-03_JPRB,-.2844E-03_JPRB,-.1208E-03_JPRB,&
                                 &+.3999E-02_JPRB,+.6282E-02_JPRB,+.2813E-02_JPRB,&
     &+.1475E-02_JPRB,+.4571E-03_JPRB,-.1349E-03_JPRB,-.9011E-04_JPRB,-.1936E-04_JPRB,&
                                            &+.1994E-02_JPRB,+.3540E-02_JPRB,&
     &+.8837E-03_JPRB,+.1992E-03_JPRB,+.3092E-04_JPRB,-.7979E-04_JPRB,-.2664E-04_JPRB,&
                                                       &-.5006E-04_JPRB,&
     &+.6447E-03_JPRB,+.5550E-03_JPRB,+.1197E-03_JPRB,+.6657E-04_JPRB,+.1488E-04_JPRB,&
     &-.9141E-04_JPRB,-.2896E-03_JPRB,-.1561E-03_JPRB,-.6524E-04_JPRB,-.1559E-04_JPRB,&
                &-.1082E-03_JPRB,-.4126E-03_JPRB,-.1732E-03_JPRB,-.8286E-04_JPRB,&
                           &-.1993E-04_JPRB,+.3850E-04_JPRB,+.2870E-04_JPRB,&
                                      &+.4493E-04_JPRB,+.4721E-04_JPRB,&
                                                 &+.1338E-04_JPRB &
         &/)
RAEUS = (/&
           &+.6646E-02_JPRB,+.8373E-02_JPRB,+.5463E-02_JPRB,+.4554E-02_JPRB,+.3301E-02_JPRB,&
     &+.5725E-03_JPRB,-.7482E-03_JPRB,-.6222E-03_JPRB,-.2603E-03_JPRB,-.5127E-04_JPRB,&
                      &-.3849E-04_JPRB,+.9741E-02_JPRB,+.8190E-02_JPRB,+.5712E-02_JPRB,&
     &+.3039E-02_JPRB,+.5290E-03_JPRB,-.2044E-03_JPRB,-.2309E-03_JPRB,-.1160E-03_JPRB,&
                                 &+.9160E-02_JPRB,+.1286E-01_JPRB,+.1170E-01_JPRB,&
     &+.5491E-02_JPRB,+.1393E-02_JPRB,-.6288E-04_JPRB,-.2715E-03_JPRB,-.1047E-03_JPRB,&
                                            &+.4873E-02_JPRB,+.3545E-02_JPRB,&
     &+.3069E-02_JPRB,+.1819E-02_JPRB,+.6947E-03_JPRB,+.1416E-03_JPRB,-.1538E-04_JPRB,&
                                                       &-.4351E-03_JPRB,&
     &-.1907E-02_JPRB,-.5774E-03_JPRB,-.2247E-03_JPRB,+.5345E-04_JPRB,+.9052E-04_JPRB,&
     &-.3972E-04_JPRB,-.9665E-04_JPRB,+.7912E-04_JPRB,-.1094E-04_JPRB,-.6776E-05_JPRB,&
                &+.2724E-03_JPRB,+.1973E-03_JPRB,+.6837E-04_JPRB,+.4313E-04_JPRB,&
                           &-.7174E-05_JPRB,+.8527E-05_JPRB,-.2160E-05_JPRB,&
                                      &-.7852E-04_JPRB,+.3453E-06_JPRB,&
                                                 &-.2402E-05_JPRB &
         &/)
RAEDC = (/&
&+.2840E-01_JPRB,+.1775E-01_JPRB,-.1069E-01_JPRB,-.1553E-01_JPRB,-.3299E-02_JPRB,+.3583E-02_JPRB,&
     &+.2274E-02_JPRB,+.5767E-04_JPRB,-.3678E-03_JPRB,-.1050E-03_JPRB,+.2133E-04_JPRB,&
           &+.2326E-01_JPRB,+.1566E-01_JPRB,-.3130E-02_JPRB,-.8253E-02_JPRB,-.2615E-02_JPRB,&
     &+.1247E-02_JPRB,+.1059E-02_JPRB,+.1196E-03_JPRB,-.1303E-03_JPRB,-.5094E-04_JPRB,&
                      &+.1185E-01_JPRB,+.7238E-02_JPRB,-.1562E-02_JPRB,-.3665E-02_JPRB,&
     &-.1182E-02_JPRB,+.4678E-03_JPRB,+.4448E-03_JPRB,+.8307E-04_JPRB,-.3468E-04_JPRB,&
                                 &+.5273E-02_JPRB,+.3037E-02_JPRB,-.4014E-03_JPRB,&
     &-.1202E-02_JPRB,-.4647E-03_JPRB,+.5148E-04_JPRB,+.1014E-03_JPRB,+.2996E-04_JPRB,&
                                            &+.2505E-02_JPRB,+.1495E-02_JPRB,&
     &+.2438E-03_JPRB,-.1223E-03_JPRB,-.7669E-04_JPRB,-.1638E-04_JPRB,+.1869E-05_JPRB,&
                                                       &+.1094E-02_JPRB,&
     &+.6131E-03_JPRB,+.1508E-03_JPRB,+.1765E-04_JPRB,+.1360E-05_JPRB,-.7998E-06_JPRB,&
     &+.4475E-03_JPRB,+.2737E-03_JPRB,+.6430E-04_JPRB,-.6759E-05_JPRB,-.6761E-05_JPRB,&
                &+.1992E-03_JPRB,+.1531E-03_JPRB,+.4828E-04_JPRB,+.5103E-06_JPRB,&
                           &+.7454E-04_JPRB,+.5917E-04_JPRB,+.2152E-04_JPRB,&
                                      &+.9300E-05_JPRB,+.9790E-05_JPRB,&
                                                 &-.8853E-05_JPRB &
         &/)
RAEDS = (/&
           &+.9815E-02_JPRB,+.8436E-02_JPRB,+.1087E-02_JPRB,-.2717E-02_JPRB,-.1755E-02_JPRB,&
     &-.1559E-03_JPRB,+.2367E-03_JPRB,+.8808E-04_JPRB,+.2001E-05_JPRB,-.1244E-05_JPRB,&
                      &+.1041E-01_JPRB,+.8039E-02_JPRB,+.1005E-02_JPRB,-.1981E-02_JPRB,&
     &-.1090E-02_JPRB,+.1595E-05_JPRB,+.1787E-03_JPRB,+.4644E-04_JPRB,-.1052E-04_JPRB,&
                                 &+.6593E-02_JPRB,+.3983E-02_JPRB,-.1527E-03_JPRB,&
     &-.1235E-02_JPRB,-.5078E-03_JPRB,+.3649E-04_JPRB,+.1005E-03_JPRB,+.3182E-04_JPRB,&
                                            &+.3225E-02_JPRB,+.1672E-02_JPRB,&
     &-.7752E-04_JPRB,-.4312E-03_JPRB,-.1872E-03_JPRB,-.1666E-04_JPRB,+.1872E-04_JPRB,&
                                                       &+.1133E-02_JPRB,&
     &+.5643E-03_JPRB,+.7747E-04_JPRB,-.2980E-04_JPRB,-.2092E-04_JPRB,-.8590E-05_JPRB,&
     &+.2988E-03_JPRB,+.6714E-04_JPRB,-.6249E-05_JPRB,+.1052E-04_JPRB,+.8790E-05_JPRB,&
                &+.1569E-03_JPRB,-.1175E-04_JPRB,-.3033E-04_JPRB,-.9777E-06_JPRB,&
                           &+.1101E-03_JPRB,+.6827E-05_JPRB,-.1023E-04_JPRB,&
                                      &+.4231E-04_JPRB,+.4905E-05_JPRB,&
                                                 &+.6229E-05_JPRB &
         &/)

!     ------------------------------------------------------------------
RCAEROS= 0.1462E-16_JPRB
!     ------------------------------------------------------------------

RETURN
END SUBROUTINE SUAERH
