C  Details on all GKS COMMON variables are in the GKS BLOCKDATA.
      PARAMETER(MXNWK=15,NSEG=100,IWDIM=10000,NUMERS=130)
      COMMON/GKINTR/ NOPWK , NACWK , WCONID, NUMSEG,
     +               SEGS(NSEG)    , CURSEG, SEGLEN(NSEG)  , MXSREC,
     +               SEGT(NSEG,2,3), CURTM(2,3)            , SEGDEL,
     +               RWKSP(IWDIM)  , GKSCLP
      INTEGER        NOPWK , NACWK , WCONID, NUMSEG, SEGS  , CURSEG,
     +               SEGLEN, MXSREC, SEGDEL, GKSCLP
      COMMON/GKOPDT/ OPS   , KSLEV , WK    , LSWK(17)      ,
     +               MOPWK , MACWK , MNT
      INTEGER        OPS   , WK
      COMMON/GKSTAT/ SOPWK(MXNWK)  , SACWK(MXNWK)  , CPLI  , CLN   ,
     +               CLWSC , CPLCI , CLNA  , CLWSCA, CPLCIA, CPMI  ,
     +               CMK   , CMKS  , CPMCI , CMKA  , CMKSA , CPMCIA,
     +               CTXI  , CTXFP(2)      , CCHXP , CCHSP , CTXCI ,
     +               CTXFPA, CCHXPA, CCHSPA, CTXCIA, CCHH  , CCHUP(2),
     +               CTXP  , CTXAL(2)      , CFAI  , CFAIS , CFASI ,
     +               CFACI , CFAISA, CFASIA, CFACIA, CPA(2), CPARF(2),
     +               CNT   , LSNT(2)       , NTWN(2,4)     , NTVP(2,4),
     +               CCLIP , SWKTP(MXNWK)  , NOPICT, NWKTP ,
     +               LXWKID(MXNWK) , ECONID, CLLX  , CLLY  , CURX  ,
     +               CURY  , CPSCL , CCMDL,  COLMOD 
      INTEGER        SOPWK , SACWK , CPLI  , CLN   , CPLCI , CLNA  ,
     +               CLWSCA, CPLCIA, CPMI  , CMK   , CPMCI , CMKA  ,
     +               CMKSA , CPMCIA, CTXI  , CTXFP , CTXCI , CTXFPA,
     +               CCHXPA, CCHSPA, CTXCIA, CTXP  , CTXAL , CFAI  ,
     +               CFAIS , CFASI , CFACI , CFAISA, CFASIA, CFACIA,
     +               CNT   , LSNT  , CCLIP , SWKTP , NOPICT, NWKTP ,
     +               LXWKID, ECONID, CLLX  , CLLY  , CURX  , CURY  ,
     +               CPSCL , CCMDL,  COLMOD
      REAL           NTWN  , NTVP
      COMMON /GKETBI/IERNMS(NUMERS)
      INTEGER IERNMS
      COMMON /GKETBC/ERMSGS(NUMERS)
      CHARACTER*90 ERMSGS
      COMMON/GKEROR/ ERS   , ERF   , CUFLAG, XERMSG(160)   , MXERMG
      INTEGER        ERS   , ERF   , CUFLAG, XERMSG        , MXERMG
      COMMON/GKENUM/ GBUNDL, GINDIV, GGKCL , GGKOP , GWSOP , GWSAC ,
     +               GSGOP , GOUTPT, GINPUT, GOUTIN, GWISS , GMO   ,
     +               GMI   , GCGM  , GWSS  , GXWE  , GXWC  , GDMP  ,
     +               GPSMIN, GPSMAX
      INTEGER        GBUNDL, GINDIV, GGKCL , GGKOP , GWSOP , GWSAC ,
     +               GSGOP , GOUTPT, GINPUT, GOUTIN, GWISS , GMO   ,
     +               GMI   , GCGM  , GWSS  , GXWE  , GXWC  , GDMP  ,
     +               GPSMIN, GPSMAX
      COMMON/GKSNAM/ GNAM(109)     , SEGNAM(NSEG)  , GFNAME, GSEGRT
      CHARACTER      GNAM*6, SEGNAM*57     , GFNAME*256    ,
     +               GSEGRT*80
      COMMON/GKSIN1/ FCODE , CONT  , 
     +               IL1   , IL2   , ID(128)       ,
     +               IC1   , IC2   , IC(128)       ,
     +               RL1   , RL2   , RX(128)       , RY(128)       ,
     +               STRL1 , STRL2 , RERR
      COMMON/GKSIN2/ STR
      INTEGER        FCODE , CONT  , IL1   , IL2   , ID    , IC1   ,
     +               IC2   , IC    , RL1   , RL2   , STRL1 , STRL2 ,
     +               RERR
      REAL           RX    , RY
      CHARACTER*80   STR
