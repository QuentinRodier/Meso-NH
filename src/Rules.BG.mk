#MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
#MNH_LIC for details. version 1.
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=${workdir}
# -qsigtrap -qfloat=nans
# -qflttrap=enable:overflow:zerodivide:invalid
# -qextname 
#OPT_BASE  = -qflttrap=enable:overflow:zerodivide:invalid \
#            -qfloat=nans -qarch=450 -qmoddir=$(OBJDIR) \
#            -qautodbl=dbl4 -qzerosize -g -qfullpath -qspillsize=32648 \
#            -qinitauto=0 -qdpc=e -qmaxmem=-1

#OPT_BASE  = -qmoddir=$(OBJDIR) -qautodbl=dbl4 -qzerosize  
OPT_BASE  = -g -qautodbl=dbl4 -qzerosize -qextname=flush -qnohot -qnoescape \
            -qsigtrap -qflttrap=overflow:zerodivide:invalid:enable -qfloat=nans -qarch=450
# -qnopic 

OPT_PERF0   = -O0 -qnooptimize -qkeepparm -qfullpath 
OPT_PERF2   = -O2 -qmaxmem=-1
OPT_CHECK = -C 
OPT_I8      = -qintsize=8 -qxlf77=intarg
OPT_I4      = -qintsize=4 
#
# Integer 4/8 option
#
MNH_INT   ?=4
#
OPT_BASE_I4       := $(OPT_BASE)
ifeq "$(MNH_INT)" "8"
OPT_BASE          += $(OPT_I8)
LFI_INT           ?=8
else
LFI_INT           ?=4
endif
#
#
OPT       = $(OPT_BASE) $(OPT_PERF2) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
#
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
LIBS     += -L/bglocal/prod/TotalView/8.10.0-0/linux-power/lib/ -ltvheap_bluegene_p
endif
#
ifeq "$(OPTLEVEL)" "O3"
OPT_PERF3    = -O3 -qstrict -qmaxmem=-1
OPT       = $(OPT_BASE) $(OPT_PERF3) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3)
endif
#            
ifeq "$(OPTLEVEL)" "O3SMP"
OPT_PREF3SMP = -O3 -qsmp -qstrict -qmaxmem=-1
OPT       = $(OPT_BASE) $(OPT_PERF3SMP) 
OPT0      = $(OPT_BASE) $(OPT_PERF0)    
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3SMP)
endif
#            
#
ifeq "$(OPTLEVEL)" "O4"
OPT_PERF4    = -O4 
OPT       = $(OPT_BASE) $(OPT_PERF4) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF4)
endif
#
#
F90 = mpixlf95_r
F90FLAGS =       $(OPT) -qfree=f90 -qsuffix=f=f90 
F77 = $(F90)
F77FLAGS      =  $(OPT) -qfixed
FX90 = $(F90)
FX90FLAGS     =  $(OPT) -qfixed
FC = xlf_r
#
LDFLAGS   =  $(OPT) -Wl,--relax
AR = /bgsys/drivers/ppcfloor/gnu-linux/powerpc-bgp-linux/bin/ar 
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
CC  = mpixlc_r
#
CPPFLAGS_SURFEX    =
#CPPFLAGS_SURCOUCHE = -DMNH_SP4 -DMNH_MPI_ISEND
CPPFLAGS_SURCOUCHE = -DMNH_SP4 -DMNH_MPI_BSEND
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DLINUX  -DLFI_INT=${LFI_INT}
CPPFLAGS_MNH       = -DAMAX1=MAX -DMNH -DSFX_MNH
#
# LIBTOOLS flags
#
#if MNH_TOOLS exists => compile the tools
#MNH_TOOLS = no
#
## IOLFI flag
#
#if MNH_IOLFI exists => compile the NEWLFI library (for LFI files)
MNH_IOLFI=yes
#
## COMPRESS flag
#
#if MNH_COMPRESS exists => compile the COMPRESS library (for LFI files)
#MNH_COMPRESS=no
#
## S4PY flag
#
#if MNH_S4PY exists => compile the libs4py library (for epygram)
#MNH_S4PY=no
#
## ecCodes or grib_api selection
#MNH_GRIBAPI: if set to no:  use ecCodes
#             if set to yes: use grib_api (deprecated library)
#
MNH_GRIBAPI=no
#
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURFEX      += ARCH_SRC/surfex 
#DIR_MNH      += ARCH_SRC/bug_mnh_BG
#
include Makefile.MESONH.mk
#
##########################################################
#                                                        #
# extra VPATH, Compilation flag modification             #
#         systeme module , etc ...                       #
#         external precompiled module librairie          #
#         etc ...                                        #
#                                                        #
##########################################################
OBJS_NOCB += spll_prep_ideal_case.o spll_mesonh.o
$(OBJS_NOCB) : OPT = $(OPT_NOCB)
#
#IGNORE_OBJS += spll_abort.o spll_ch_make_lookup.o \
#spll_compute_ver_grid.o spll_convlfi.o spll_diag.o spll_example_fwd.o spll_latlon_to_xy.o \
#spll_prep_nest_pgd.o spll_prep_pgd.o spll_prep_real_case.o \
#spll_prep_surfex.o spll_rad1driv.o spll_rttov_ascii2bin_coef.o spll_rttovcld_testad.o spll_rttovcld_test.o \
#spll_rttovscatt_test.o spll_spawning.o spll_test_2_coef.o spll_test_coef.o spll_test_errorhandling.o \
#spll_test_q2v.o spll_xy_to_latlon.o spll_zoom_pgd.o 

ifneq "$(findstring 8,$(LFI_INT))" ""
OBJS_I8=spll_NEWLFI_ALL.o
$(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
endif

ifeq "$(MNH_INT)" "8"
OBJS_I4=spll_modd_netcdf.o
$(OBJS_I4) : OPT = $(OPT_BASE_I4)
endif

