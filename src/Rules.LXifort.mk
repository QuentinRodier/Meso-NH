#MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#MNH_LIC for details. version 1.
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE   =  -g -w -assume nosource_include -assume byterecl -fpe0 -ftz -fpic -traceback  -fp-model precise -switch fe_inline_all_arg_copy_inout
OPT_PERF0  =  -O0
OPT_PERF2  =  -O2
OPT_PERF3  =  -O3
OPT_CHECK  =  -CB -ftrapuv 
OPT_I8     =  -i8
OPT_R8     =  -r8
#
# Real/integer 4/8 option
#
MNH_REAL  ?=R8
MNH_INT   ?=I4
LFI_RECL  ?=512
#
ifneq "$(MNH_REAL)" "R4"
OPT_BASE           += $(OPT_R8)
CPPFLAGS_SURCOUCHE += -DMNH_MPI_DOUBLE_PRECISION
endif
#
ifeq "$(MNH_INT)" "I8"
OPT_BASE         += $(OPT_I8)
LFI_INT           ?=8
MNH_MPI_RANK_KIND ?=8
else
MNH_MPI_RANK_KIND ?=4
LFI_INT           ?=4
endif
#
OPT       = $(OPT_BASE) $(OPT_PERF2) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
#
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
CFLAGS   += -g
endif
ifeq "$(OPTLEVEL)" "O2PAR"
PAR= -parallel -diag-file -par-report2
OPT       = $(OPT_BASE) $(OPT_PERF2) $(PAR)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(PAR)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(PAR)
endif
ifeq "$(OPTLEVEL)" "O2NOVEC"
OPT       = $(OPT_BASE) $(OPT_PERF2) -no-vec
OPT0      = $(OPT_BASE) $(OPT_PERF0) -no-vec
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) -no-vec
endif
ifeq "$(OPTLEVEL)" "O3"
OPT       = $(OPT_BASE) $(OPT_PERF3)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3)
endif
#
#
FC = ifort
ifeq "$(VER_MPI)" "MPIAUTO"
ifneq "$(findstring TAU,$(XYZ))" ""
F90 = tau_f90.sh 
export TAU_MAKEFILE=/home/escj/PATCH/TAU/TAU-2.21.1-IFORT10-OMPI152-THREAD/x86_64/lib/Makefile.tau-mpi
LIBS += -lz 
else
F90 = mpif90
endif
else
ifeq "$(VER_MPI)" "MPIINTEL"
F90 = mpiifort
ifeq "$(MNH_INT)" "I8"
OPT_BASE         += -ilp64
endif
else
F90 = ifort
endif
endif
F90FLAGS  =  $(OPT)
F77  = $(F90)
F77FLAGS  =  $(OPT) 
# -132
FX90 = $(F90)
FX90FLAGS =  $(OPT)
# -132 
#
#LDFLAGS    =  -Wl,-noinhibit-exec  -Wl,-warn-once $(PAR)
LDFLAGS    =   -Wl,-warn-once $(PAR)
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE += -DMNH_LINUX -DDEV_NULL -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
CPPFLAGS_MNH       = -DMNH 
ifdef VER_GA
CPPFLAGS_SURCOUCHE += -DMNH_GA
INC                += -I${GA_ROOT}/include
LIBS               += -L${GA_ROOT}/lib -larmci -lga -lgfortran
endif
#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=ifort
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURCOUCHE   += ARCH_SRC/bug_surcouche
#DIR_MNH         += ARCH_SRC/bug_mnh
#DIR_RAD         += ARCH_SRC/bug_rad
#DIR_SURFEX      += ARCH_SRC/surfex
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
# Juan & Maud 20/03/2008 --> Ifort 10.1.008 Bug O2 optimization
OPT_PERF1  =  -O1
OBJS_O1= spll_schu.o spll_ps2str.o spll_p_abs.o spll_ini_one_way_n.o spll_urban_solar_abs.o
$(OBJS_O1) : OPT = $(OPT_BASE) $(OPT_PERF1)

ifneq "$(findstring 8,$(LFI_INT))" ""
OBJS_I8=spll_NEWLFI_ALL.o
$(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
endif

