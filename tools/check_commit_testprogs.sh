#!/bin/bash

#set -x
set -e
set -o pipefail #abort if left command on a pipe fails

#This script:
# - compiles the PHYEX package using a specific commit
# - runs the different test progs and checks if results are identical to a given version

#ice_adjust: the ice adjust test case

#ref is commit 855b8f8 for ice_adjust, rain_ice
#ref is commit ??????? for turb
#ref is commit 7e44ab1 for shallow
#ref is commit e070d16 for rain_ice_old

#Commit e070d16 can be used for rain_ice_old (ref commit for this testprogs), and for
#turb, shallow, rain_ice and ice_adjust (as it gives the same results for these test cases).

#Some modifications have been introduced and new reference commit is 00148b1

#Data generation:
# - The last commit of the testprogs_data branch (based on 46t1) is able to produce the data
#   for the turb, shallow, rain_ice and ice_adjust testprogs. The code is present but must be
#   activated in the corresponding aro_* routine (as only one set of data can be produced during
#   a single execution).
# - The last commit of the testprogs_data2 branch (based on 48t3) is able to produce the data
#   for the rain_ice_old testprog.

#######################
#### CONFIGURATION ####
#######################

#Special pack names:
# - ref: symbolic name to the commit to use as a reference
#        useless for the commits containing a json file
specialName="ref"

#About the tests:
# - ALLTests is a list of tests to be done when '-t ALL' is used. This list is filled here
#   in case there is no ial_version.json file containig a 'testing' section. If this 'testing'
#   section exists, this list is overridden.
# - allowedTests is the list of allowed tests which can depend on platform, if we ask to perform an action
#   with a test not in the allowedTests list, the action is ignored
# - defaultTest is the list of tests to perform when no '-t' option is provided on the command line.
ALLTests="ice_adjust,rain_ice,rain_ice_old,turb,shallow"
defaultTest=${ALLTests}
allowedTests=${ALLTests}

separator='_' #- seprator must be in sync with prep_code.sh separator

PHYEXTOOLSDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

TESTDIR=${TESTPROGSDIR:=$HOME/TESTPROGS}

dirdata=$PHYEXTOOLSDIR/testprogs_data
if [ $(hostname | cut -c 1-7) == 'belenos' -o $(hostname | cut -c 1-7) == 'taranis' ]; then
  HPC=1
  defaultarchfile=MIMPIIFC1805.EPONA
else
  HPC=0
  defaultarchfile=gnu
fi
defaultRef=ref

################################
#### COMMAND LINE ARGUMENTS ####
################################

function usage {
  echo "Usage: $0 [-h] [-p] [-c] [-r] [-C] [-s] [--noexpand] [-t TEST] [--repo-user USER] [--repo-protocol PROTOCOL] [-a ARCH] [-A ARCH] [--remove] [--onlyIfNeeded] [--computeRefIfNeeded] commit [reference]"
  echo "commit          commit hash (or a directory, or among $specialName) to test"
  echo "reference       commit hash (or a directory, or among $specialName) REF to use as a reference"
  echo "-s              suppress compilation directory"
  echo "-p              creates pack"
  echo "-c              performs compilation"
  echo "-r              runs the tests"
  echo "-C              checks the result against the reference"
  echo "-t TEST         comma separated list of tests to execute"
  echo "                or ALL to execute all tests"
  echo "--noexpand      do not expand mnh_expand blocks (code will be in array-syntax)"
  echo "--repo-user USER"
  echo "                user hosting the PHYEX repository on github,"
  echo "                defaults to the env variable PHYEXREOuser (=$PHYEXREOuser)"
  echo "--repo-protocol PROTOCOL"
  echo "                protocol (https or ssh) to reach the PHYEX repository on github,"
  echo "                defaults to the env variable PHYEXREOprotocol (=$PHYEXREOprotocol)"
  echo "--remove        removes the pack"
  echo "-a arch ARCH    architecture name to use to build and run the commit (=$defaultarchfile)"
  echo "-A arch ARCH    architecture name to use for the reference simulation (=$defaultarchfile)"
  echo ""
  echo "If nothing is asked (pack creation compilation, running, check, removing) everything"
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

packcreation=0
compilation=0
run=0
check=0
commit=""
reference=""
tests=""
suppress=0
useexpand=""
archfile=$defaultarchfile
refarchfile=$defaultarchfile
remove=0
onlyIfNeeded=0
computeRefIfNeeded=0

while [ -n "$1" ]; do
  case "$1" in
    '-h') usage;;
    '-s') suppress=1;;
    '-p') packcreation=1;;
    '-c') compilation=1;;
    '-r') run=$(($run+1));;
    '-C') check=1;;
    '-t') tests="$2"; shift;;
    '--noexpand') useexpand=$1;;
    '--repo-user') export PHYEXREPOuser=$2; shift;;
    '--repo-protocol') export PHYEXREPOprotocol=$2; shift;;
    '--remove') remove=1;;
    '-a') archfile="$2"; shift;;
    '-A') refarchfile="$2"; shift;;
    '--onlyIfNeeded') onlyIfNeeded=1;;
    '--computeRefIfNeeded') computeRefIfNeeded=1;;

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

if [ $packcreation -eq 0 -a \
     $compilation -eq 0 -a \
     $run -eq 0 -a \
     $check -eq 0 -a \
     $remove -eq 0 ]; then
  packcreation=1
  compilation=1
  run=1
  check=1
fi

if [ -z "${commit-}" ]; then
  echo "At least one commit hash must be provided on command line"
  exit 2
fi

if [ $check -eq 1 -a -z "${reference-}" ]; then
  echo "To perform a comparison two commit hashes are mandatory on the command line"
  exit 3
fi

##############################
#### FUNCTION DEFINITIONS ####
##############################

function json_dictkey2value {
  # $1 must contain the json string
  # $2 must be the key name
  # $3 is the default value
  json_content="$1" python3 -c "import json; import os; result=json.loads(os.environ['json_content']).get('$2', '$3'); print(json.dumps(result) if isinstance(result, dict) else result)"
}

###########################
#### COMMIT ADAPTATION ####
###########################

#Name and directory for compiling and executing user pack
declare -A refByTest
if echo $commit | grep '/' | grep -v '^tags/' > /dev/null; then
  #The git repository is a directory
  name=$(echo $commit | sed 's/\//'${separator}'/g' | sed 's/:/'${separator}'/g' | sed 's/\./'${separator}'/g')
  content_testprogs_version=$(scp $commit/src/testprogs/testprogs_version.json /dev/stdout 2>/dev/null || echo "")
  [ $suppress -eq 1 -a -d $TESTDIR/$name ] && rm -rf $TESTDIR/$name
elif echo $specialName | grep -w $commit > /dev/null; then
  name="$commit"
else
  #The git repository is on github
  if [[ $commit == testprogs${separator}* ]]; then
    testprogs_version_file="testprogs_version.json"
  else
    testprogs_version_file="src/testprogs/testprogs_version.json"
  fi
  if echo $commit | grep '^tags/' > /dev/null; then
    urlcommit=$(echo $commit | cut -d / -f 2-)
  else
    urlcommit=$commit
  fi
  content_testprogs_version=$(wget --no-check-certificate https://raw.githubusercontent.com/$PHYEXREPOuser/PHYEX/${urlcommit}/$testprogs_version_file -O - 2>/dev/null || echo "")
  name="COMMIT$(echo $commit | sed 's/\//'${separator}'/g' | sed 's/:/'${separator}'/g' | sed 's/\./'${separator}'/g')"
  [ $suppress -eq 1 -a -d $TESTDIR/$name ] && rm -rf $TESTDIR/$name
fi
if [ ! "${content_testprogs_version}" == "" ]; then
  testing=$(json_dictkey2value "$content_testprogs_version" 'testing' '')
  refALL=$(json_dictkey2value "$testing" "ALL" '')
  if [ ! "$testing" == "" ]; then
    ALLTests='' #We reset the list of tests
    for t in $(echo $allowedTests | sed 's/,/ /g'); do
      ref=$(json_dictkey2value "$testing" "$t" "$refALL")
      if [ ! "$ref" == "" ]; then
        ALLTests="${ALLTests},$t"
        refByTest[$t]=$ref
      fi
    done
    ALLTests="${ALLTests:1}" #Remove first character (',')
  fi
fi

#Name and directory for the reference version
if [ ! -z "${reference-}" ]; then
  declare -A refnameByTest
  #Reference to use for each test
  for t in $(echo $ALLTests | sed 's/,/ /g'); do
    #Name of the reference
    if [ "$reference" == "REF" ]; then
      if [[ ! -z "${refByTest[$t]+unset}" ]]; then #the -v test is valid only with bash > 4.3
        #The json file contained the references to use on a per test case basis
        caseref=${refByTest[$t]}
      else
        caseref=$defaultRef
      fi
      refByTest[$t]=$caseref
    else
      #The exact reference to use was given on the command line
      caseref=$reference
    fi
    refByTest[$t]=$caseref
  
    #Conversion into directory name
    if echo $caseref | grep '/' > /dev/null; then
      refname=$(echo $reference | sed 's/\//'${separator}'/g' | sed 's/:/'${separator}'/g' | sed 's/\./'${separator}'/g')
    elif echo $specialName | grep -w $caseref > /dev/null; then
      refname="$caseref"
    else
      refname="COMMIT${caseref}"
    fi
    refnameByTest[$t]=$refname
  done
fi

if [ -z "${tests-}" ]; then
  tests=$defaultTest
elif echo "$tests" | grep -w 'ALL' > /dev/null; then
  tests=$(echo "$tests" | sed "s/\bALL\b/$ALLTests/g")
fi

#######################
#### PACK CREATION ####
#######################

if [ $packcreation -eq 1 ]; then
  if [ -d $TESTDIR/$name ]; then
    if [ $onlyIfNeeded -eq 0 ]; then
      echo "Directory already exists ($TESTDIR/$name), suppress it to be able to compile it again (or use the -s option to automatically suppress it)"
      exit 5
    fi
  else
    echo "### Pack creation for commit $commit"

    if echo $specialName | grep -w $commit > /dev/null; then
      echo "Special commit '$commit' cannot be compiled with this script"
      exit 4
    fi

    mkdir $TESTDIR/$name
    cd $TESTDIR/$name/
    cp -r $PHYEXTOOLSDIR/../build . #We use the compilation system from the same commit as the current script

    cd $TESTDIR/$name/build/with_fcm/
    rm -rf arch_*
    ./make_fcm.sh -p $useexpand --commit $commit --arch $archfile 2>&1 | tee Output_compilation_step1
  fi
fi

#####################
#### COMPILATION ####
#####################

if [ $compilation -eq 1 ]; then
  if [ $onlyIfNeeded -eq 0 -o ! -f $TESTDIR/$name/build/with_fcm/arch_${archfile}/build/bin/libphyex.so ]; then
    echo "### Compilation of commit $commit"

    cd $TESTDIR/$name/build/with_fcm/
    ./make_fcm.sh -c $useexpand --commit $commit --arch $archfile 2>&1 | tee Output_compilation_step2
  fi
fi

###################
#### EXECUTION ####
###################

if [ $run -ge 1 ]; then
  cd $TESTDIR/$name

  #Cleaning to suppress old results that may be confusing in case of a crash during the run
  if [ $onlyIfNeeded -eq 0 ]; then
    for t in $(echo $tests | sed 's/,/ /g'); do
      if [ -d tests/with_fcm/arch_${archfile}/$t ]; then
        rm -rf tests/with_fcm/arch_${archfile}/$t
      fi
    done
  fi

  #Run the tests one after the other
  firstrun=1
  for t in $(echo $tests | sed 's/,/ /g'); do
    if echo $allowedTests | grep -w $t > /dev/null; then #test is allowed on this plateform
      if  [ ! -d tests/with_fcm/arch_${archfile}/$t ]; then #We do not enter systematically this part if onlyIfNeeded=1
        if [ $firstrun -eq 1 ]; then
          echo "### Running of commit $commit"
          firstrun=0
        fi

        if [ ! -f $TESTDIR/$name/build/with_fcm/arch_${archfile}/build/bin/main_${t}.exe ]; then
          echo "Directory does not exist ($TESTDIR/$name) or compilation has failed, please check"
          exit 6
        fi

        #execution
        cd $TESTDIR/$name
        mkdir -p tests/with_fcm/arch_${archfile}/$t
        cd tests/with_fcm/arch_${archfile}/$t
        ln -s $dirdata/$t data
        $TESTDIR/$name/build/with_fcm/arch_${archfile}/build/bin/main_${t}.exe --check 2>&1 > Output_run
      fi
    fi
  done
fi

####################
#### COMPARISON ####
####################

if [ $check -eq 1 ]; then
  echo "### Check commit $commit against commit $reference"

  alltests=0
  message=""
  for t in $(echo $tests | sed 's/,/ /g'); do
    if echo $allowedTests | grep -w $t > /dev/null; then
      #Run the reference if needed
      if [ $computeRefIfNeeded -eq 1 ]; then
        $0 -p -c -r -t $t -a ${archfile} --onlyIfNeeded ${refByTest[$t]}
      fi

      #File comparison
      file1=$TESTDIR/$name/tests/with_fcm/arch_${archfile}/$t/Output_run
      file2=$TESTDIR/${refnameByTest[$t]}/tests/with_fcm/arch_${refarchfile}/$t/Output_run
      mess=""
      te=0
      if [ ! -f "$file1" ]; then
        mess="Result ($file1) for commit $commit does not exist, please run the simulation"
        te=1
      fi
      if [ ! -f "$file2" ]; then
        mess2="Result ($file2) for commit ${refByTest[$t]} does not exist, please run the simulation"
        te=1
        if [ "$mess" = "" ]; then
          mess=$mess2
        else
          mess="$mess and $mess2"
        fi
      fi
      if [ $te -eq 0 ]; then
        set +e
        mess=$(cmp <(cat $file1 | sed 's/\.\.//g' | sed 's/~=//g' | sed 's/!=//g' | grep -v 'Total time: ' | sed 's/-0.00000E+00|/ 0.00000E+00|/g' | sed 's/-0.00000E+00 / 0.00000E+00 /g' | sed 's/-0.00000E+00-/ 0.00000E+00-/g') \
                   <(cat $file2 | sed 's/\.\.//g' | sed 's/~=//g' | sed 's/!=//g' | grep -v 'Total time: ' | sed 's/-0.00000E+00|/ 0.00000E+00|/g' | sed 's/-0.00000E+00 / 0.00000E+00 /g' | sed 's/-0.00000E+00-/ 0.00000E+00-/g') 246 246 2>&1)
        te=$?
        set -e
        #The use of "<()" bash syntax replaces the actual file name seen by cmp
        #We modify the cmp output to display the actual file names
        mess=$(echo $mess | sed "s#^.*differ# $file1 $file2 differ#")
      fi
      [ $te -ne 0 ] && message="$message $mess \n"
      alltests=$(($alltests+$te))
    fi
  done
  if [ $alltests -eq 0 ]; then
    echo "SUCCESS, files are identical"
  else
    echo "*************** Files are different *******************"
    echo -e "$message"
    cmpstatus=50
  fi
fi

##################
#### CLEANING ####
##################

if [ $remove -eq 1 ]; then
  echo "### Remove model directory for commit $commit"
  [ -d $TESTDIR/$name ] && rm -rf $TESTDIR/$name
fi

exit $cmpstatus
