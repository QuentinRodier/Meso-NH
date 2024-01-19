#!/bin/bash

#set -x
set -e
set -o pipefail #abort if left command on a pipe fails

#The folowing environment variables can be defined:
# REFDIR: directory in which the reference compilation directory can be found
# TARGZDIR: directory where tar.gz files are searched for
# MNHPACK: directory where tests are build

availTests="001_2Drelief, 002_3Drelief, 003_KW78, 004_Reunion, 005_ARM, 007_16janvier, 009_ICARTT, 011_KW78CHEM, 012_dust, 014_LIMA, 2DRelief, 3DRelief, ARMCU_1D_CONDSAMP, BLOWSNOW_c1b1D, BOMEX, COLD_BUBBLE, DOUBLE_GRIDNESTING, EOLIENNE_FAST, FIRE_1D/KHKO, FIRE_1D/KHKO_MALA, FIRE_1D/LIMA_MALA, FOG_1D/ICE3, FOG_1D/LIMA, HYDRO, IHOP_1D, LIMA_2D, Reunion, STATIONS_PROF_BALLON_AIRCR_4doms "
defaultTest="007_16janvier"
separator='_' #- be carrefull, gmkpack (at least on belenos) has multiple allergies (':', '.', '@')
              #- seprator must be in sync with prep_code.sh separator

PHYEXTOOLSDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
function usage {
  echo "Usage: $0 [-h] [-c] [-r] [-C] [-s] [--expand] [-t test] [--remove] commit [reference]"
  echo "commit          commit hash (or a directory)"
  echo "reference       commit hash or a directory or nothing for ref"
  echo "-s              suppress compilation pack"
  echo "-c              performs compilation"
  echo "-r              runs the tests"
  echo "-C              checks the result against the reference"
  echo "-t              comma separated list of tests to execute"
  echo "                or ALL to execute all tests"
  echo "--expand        use mnh_expand (code will use do loops)"
  echo "--repo-user     user hosting the PHYEX repository on github,"
  echo "                defaults to the env variable PHYEXREOuser (=$PHYEXREOuser)"
  echo "--repo-protocol protocol (https or ssh) to reach the PHYEX repository on github,"
  echo "                defaults to the env variable PHYEXREOprotocol (=$PHYEXREOprotocol)"
  echo "--remove        removes the pack"
  echo ""
  echo "If nothing is asked (compilation, running, check, removing) everything"
  echo "except the removing is done"
  echo
  echo "If no test is aked for, the default one ($defaultTest) is executed"
  echo
  echo "With the special reference REF commit, a suitable reference is guessed"
  echo
  echo "The directory (for commit only, not ref) can take the form server:directory"
  echo
  echo "If using a directory (for commit or reference) it must contain at least one '/'"
  echo "The commit can be a tag, written with syntagx tags/<TAG>"
}

compilation=0
run=0
check=0
commit=""
reference=""
tests=""
suppress=0
useexpand=0
remove=0

while [ -n "$1" ]; do
  case "$1" in
    '-h') usage;;
    '-s') suppress=1;;
    '-c') compilation=1;;
    '-r') run=$(($run+1));;
    '-C') check=1;;
    '-t') tests="$2"; shift;;
    '--expand') useexpand=1;;
    '--repo-user') export PHYEXREPOuser=$2; shift;;
    '--repo-protocol') export PHYEXREPOprotocol=$2; shift;;
    '--remove') remove=1;;
    #--) shift; break ;;
     *) if [ -z "${commit-}" ]; then
          commit=$1
        else
          if [ -z "${reference-}" ]; then
            reference=$1
          else
            echo "Only two commit hash allowed on command line"
            exit 1
          fi
        fi;;
  esac
  shift
done

[ "$reference" == 'REF' ] && reference="" #Compatibility with check_commit_arome.sh

MNHPACK=$HOME/MNHTESTING/MesoNH
REFDIR=$HOME/REF
TARGZDIR=$HOME
if [ -z "${tests-}" ]; then
  tests=$defaultTest
elif [ $tests == 'ALL' ]; then
  tests=$availTests
fi

if [ $compilation -eq 0 -a \
     $run -eq 0 -a \
     $check -eq 0 -a \
     $remove -eq 0 ]; then
  compilation=1
  run=1
  check=1
fi

if [ -z "${commit-}" ]; then
  echo "At least one commit hash must be provided on command line"
  exit 2
fi

refversion=MNH-V5-7-0

tag=$(echo $commit | sed 's/\//'${separator}'/g' | sed 's/:/'${separator}'/g' | sed 's/\./'${separator}'/g')
name=${refversion}-$tag
[ $suppress -eq 1 -a -d $MNHPACK/$name ] && rm -rf $MNHPACK/$name

path_user_beg=$MNHPACK/$name #pack directory containing the simulation
path_user_end= #to be appended to the 'run' simulation directory

#Name and directory for the reference
reffromdir=''
if echo $reference | grep '/' > /dev/null; then
  reffromdir=$reference
  reftag=$(echo $reference | sed 's/\//'${separator}'/g' | sed 's/:/'${separator}'/g' | sed 's/\./'${separator}'/g')
else
  reftag=$reference
fi
refname=${refversion}-$reftag
path_ref_end=
if [ "$reference" == "" ]; then
  path_ref_beg=$REFDIR/${refversion}
else
  path_ref_beg=$MNHPACK/${refversion}-$reftag
fi

if [ $compilation -eq 1 ]; then
  echo "### Compilation of commit $commit"

  if [ -d $MNHPACK/$name ]; then
    echo "Pack already exists ($MNHPACK/$name), suppress it to be able to compile it again (or use the -s option to automatically suppress it)"
    exit 5
  fi

  # Prepare the pack
  cd $MNHPACK
  cp -R ~/GIT/MNH-git_open_source-lfs .
  mv  MNH-git_open_source-lfs $name
  cd $name
  git checkout $commit
  cd src
  # Routine that changed names
  
  #Configure and compilation
  command -v module && modulelist=$(module -t list 2>&1 | tail -n +2) #save loaded modules
  ./configure
  set +e #file ends with a test that can return false
  . ../conf/profile_mesonh-* #This lines modifies the list of loaded modules
  set -e
  rm -f ../exe/* #Suppress old executables, if any
  make -j 8 2>&1 | tee ../Output_compilation
  make installmaster 2>&1 | tee -a ../Output_compilation
  command -v module && module load $modulelist #restore loaded modules
fi

if [ $run -ge 1 ]; then
  echo "### Running of commit $commit"

  if [ ! -f $MNHPACK/$name/exe/MESONH* ]; then
    echo "Pack does not exist ($MNHPACK/$name) or compilation has failed, please check"
    exit 6
  fi
  export POSTRUN=echo
  for t in $(echo $tests | sed 's/,/ /g'); do
    case=$(echo $t | cut -d / -f 1)
    exedir=$(echo $t | cut -d / -f 2)
    rep=$MNHPACK/$name/MY_RUN/KTEST/$case
    repLOCAL=$MNHPACK/$name/MY_RUN/INTEGRATION_CASES/LOCAL/$case
    repHPC=$MNHPACK/$name/MY_RUN/INTEGRATION_CASES/HPC/$case
    if [ -d $rep ]; then
      cd $rep
    elif [ -d $repLOCAL ]; then
      cd $repLOCAL
    elif [ -d $repHPC ]; then
      cd $repHPC
    else
      echo "The KTEST $case has not been found"
      exit 7
    fi
    set +e #file ends with a test that can return false
    [ $compilation -eq 0 ] && . $MNHPACK/$name/conf/profile_mesonh-*
    set -e
    set +o pipefail #We want to go through all tests
    yes | make | tee Output_run
    set -o pipefail
  done
fi

if [ $remove -eq 1 ]; then
  echo "### Remove model directory for commit $commit"
  [ -d $MNHPACK/$name ] && rm -rf $MNHPACK/$name
fi

exit $cmpstatus
