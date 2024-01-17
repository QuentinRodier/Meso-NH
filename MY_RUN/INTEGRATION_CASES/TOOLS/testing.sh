#!/bin/bash

set -x
set -e
set -o pipefail #abort if left command on a pipe fails

function usage {
  echo "Usage: $0 [-h] [--repo-user] [--repo-protocol] [--repo-repo] [--no-update] [--no-compil]"
  echo "               [--no-exec] [--no-comp] [--no-remove] [--commit SHA] [--ref REF] [--force] [MAIL]"
  echo "--repo-user     user hosting the PHYEX repository on github,"
  echo "                defaults to the env variable MESONHREPOuser (=$MESONHREPOuser)"
  echo "--repo-protocol protocol (https or ssh) to reach the PHYEX repository on github,"
  echo "                defaults to the env variable PHYEXREPOprotocol (=$PHYEXREPOprotocol)"
  echo "--repo-repo     repository name"
  echo "                defaults to the env variable MESONHREPOrepo (=$MESONHREPOrepo)"
  echo "--no-update     do not update the tools"
  echo "--no-compil     do not compil (only usefull after a first execution with --no-update)"
  echo "--no-exec       do not execute (only usefull after a first execution with --no-update)"
  echo "--no-comp       do not compare (only usefull after a first execution with --no-update)"
  echo "--no-remove     do not remove compilation directory"
  echo "--force         perform the test even if github commit status already exists"
  echo "--commit SHA    use the commit with sha SHA instead of the last one"
  echo "--ref REF       ref to use (defaults to refs/heads/master)"
  echo "--only-model MODEL"
  echo "                performs the test only using model MODEL (option can be provided several times)"
  echo "--no-enable-gh-pages"
  echo "                dont't try to enable the project pages on github"
  echo "MAIL            comma-separated list of e-mail addresses (no spaces); if not provided, mail is not sent"
  echo ""
  echo "This script provides functionality for automated tests."
  echo "It can be run with cron to periodically test the last commit on the PHYEX repository"
  echo "(eg '00 22 * * * bash -l -c \"SHELL=/bin/bash PHYEXWORKDIR=~/PHYEXTESTING ~/PHYEXTESTING/PHYEX/tools/testing.sh \\"
  echo "                             --repo-user UMR-CNRM --repo-protocol ssh --repo-repo PHYEX user@domain\"')"
  echo "The repository must be hosted on github as it relies on github project pages and github statuses."
  echo "A github token must be set in the .netrc file."
  echo ""
  echo "All the work is done within the \${PHYEXWORKDIR} directory (it defaults to ~/PHYEXTESTING)."
  echo "It may be necessary to fill the \${PHYEXWORKDIR}/PHYEX/tools/pack with base source code"
  echo "(for arome and/or mesonh) or with data (testprogs)."
  echo ""
  echo "The script compare the results against reference simulations. These reference simulations must"
  echo "be available in the different subdirectories in \${WORKDIR}."
}

MAIL=""
MESONHREPOuser=${MESONHREPOuser:=QuentinRodier}
MESONHREPOrepo=${MESONHREPOrepo:=Meso-NH}
PHYEXREPOprotocol=${PHYEXREPOprotocol:=ssh}
REF="refs/heads/MNH-57-branch"
WORKDIR=${PHYEXWORKDIR:=${HOME}/MNHTESTING}
update=0
compil=1
execute=1
comp=0
remove=0
commit=""
SHA=0
force=0
models="mesonh"
enableghpages=1

while [ -n "$1" ]; do
  case "$1" in
    '-h') usage; exit;;
    '--repo-user') export MESONHREPOuser=$2; shift;;
    '--repo-protocol') export PHYEXREPOprotocol=$2; shift;;
    '--repo-repo') export MESONHREPOrepo=$2; shift;;
    '--no-update') update=0;;
    '--no-compil') compil=0;;
    '--no-exec') execute=0;;
    '--no-comp') comp=0;;
    '--no-remove') remove=0;;
    '--force') force=1;;
    '--commit') SHA=$2; shift;;
    '--ref') REF=$2; shift;;
    '--only-model') models="${models} $2"; shift;;
    '--no-enable-gh-pages') enableghpages=0;;
    #--) shift; break ;;
     *) if [ -z "${MAIL-}" ]; then
          MAIL="$1"
        else
          echo "Only one email address allowed"
          exit 1
        fi;;
  esac
  shift
done
[ "${models}" == "" ] && models="ial mesonh testprogs lmdz"

[ ! -d ${WORKDIR} ] && mkdir -p ${WORKDIR}

#stdout and stderr redirection
logfile="${WORKDIR}/logfile"
exec > "${logfile}" 2>&1

#context for statuses
context="continuous-integration/${HOSTNAME}"

#Interactions with github
if [ "${PHYEXREPOprotocol}" == 'ssh' ]; then
  PHYEXREPOgiturl="git@github.com:${MESONHREPOuser}/${MESONHREPOrepo}.git"
else
  PHYEXREPOgiturl="https://github.com/${MESONHREPOuser}/${MESONHREPOrepo}.git"
fi
TOKEN=$(python3 -c "import netrc, socket; print(netrc.netrc().authenticators('github.com')[2])")

function get_last_commit {
  git ls-remote "${PHYEXREPOgiturl}" "${REF}" | cut -f1
}

function enable_gh_pages {
  result=$(curl -L --netrc \
                -H "Authorization: Bearer $TOKEN" \
                -H "Accept: application/vnd.github+json" \
                -H "X-GitHub-Api-Version: 2022-11-28" \
                "https://api.github.com/repos/${MESONHREPOuser}/${MESONHREPOrepo}/pages")
  if [ $(echo $result | grep 'Not Found' | wc -l) -eq 1 ]; then
    log 1 "Github project pages not yet enabled"
    #Pages are not yet activated
    curl -L --netrc \
         -X POST \
         -H "Authorization: Bearer $TOKEN" \
         -H "Accept: application/vnd.github+json" \
         -H "X-GitHub-Api-Version: 2022-11-28" \
         "https://api.github.com/repos/${MESONHREPOuser}/${MESONHREPOrepo}/pages" \
         -d '{"source":{"branch":"master","path":"/docs"}}'
  fi
}

function get_statuses {
  curl -L --netrc \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/repos/${MESONHREPOuser}/${MESONHREPOrepo}/commits/${SHA}/statuses"
}

function add_status {
  error=$1
  ret=$2
  SHA="$3"
  comment="$4"
  if [ $ret -eq 0 ]; then
    state="success"
  else
    if [ $error -eq 1 ]; then
      state="error"
    else
      state="failure"
    fi
  fi
  url="https://${MESONHREPOuser}.github.io/${MESONHREPOrepo}/displayparam.html?"
  url=${url}$(content=$(echo -e "$comment") python3 -c "import urllib.parse, os; print(urllib.parse.quote('<pre>' + os.environ['content'] + '</pre>', safe=':/='))")
  curl -L \
    -X POST \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer $TOKEN" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/repos/${MESONHREPOuser}/${MESONHREPOrepo}/statuses/${SHA}" \
    -d '{"state":"'${state}'","target_url":"'${url}'","context":"'${context}'"}'
}

function get_cases {
  SHA="$1"
  file="$2"

  url="https://raw.githubusercontent.com/${MESONHREPOuser}/${MESONHREPOrepo}/${SHA}/${file}"
  content=$(wget --no-check-certificate "${url}" -O - 2>/dev/null)
  if [ "${content}" != "" ]; then
    content="${content}" python3 -c "import json, os; print(' '.join([k+':'+v for k, v in json.loads(os.environ['content']).get('testing', {}).items()]))"
  fi
}

#reporting
function send_mail {
  message="$1"
  if [ "$MAIL" != "" ]; then
    mail -s "$context" "$MAIL" <<EOF
$(echo -e ${message})
EOF
  fi
}

header="${context}\n\n$(date)"
message=""
function report {
  error=$1
  ret=$2
  if [ ${ret} -eq 0 ]; then
    error_msg=""
  else
    error_msg="XXXXXXXXXXXXXXXXXXXX ERROR ${ret} XXXXXXXXXXXXXXXXXXXX"
    error_msg="${error_msg}\n\n"
  fi
  message="${header}\n${message}\n\n${error_msg}$(date)"
  if [ ${ret} -ne 0 ]; then
    send_mail "${message}"
  fi
  if [ "${SHA}" != 0 ]; then
    add_status $error $ret "${SHA}" "${message}"
  fi
}

log_message=""
function exit_error {
  ret=$1
  if [ ${ret} -ne 0 ]; then
    message="__ ABNORMAL EXIT ${ret} __\n${log_message}\n${message}"
    message="${message}\n\nMore information can be found in ${HOSTNAME}:${logfile}"
    report 1 ${ret}
  fi
}
trap 'exit_error $?' EXIT

function log {
  level=$1; shift
  echo "$@"
  if [ ${level} -eq 0 ]; then
    message="${message}\n$@"
  fi
  log_message="${log_message}\n$@"
}

#Test
if [ "${SHA}" -eq 0 ]; then
  log 1 "Getting last commit hash"
  SHA=$(get_last_commit)
  log 1 "Commit hash is ${SHA}"
fi
if [ ${force} -eq 1 -o $(get_statuses "${SHA}" | grep "${context}" | wc -l) -eq 0 ]; then
  log 1 "This commit has not been tested (or --force id provided)"
  ret=0
  
  #Checkout tools, set PATH and use the last version of the testing script
  currentdir="${PWD}"
  if [ ${update} -eq 1 ]; then
    currentMD5=$(md5sum "${BASH_SOURCE[0]}"  | cut -d\  -f1)
    if [ ! -d "${WORKDIR}/PHYEX" ]; then
      log 1 "Clonig PHYEX in ${WORKDIR}/PHYEX"
      git clone "${PHYEXREPOgiturl}" "${WORKDIR}/PHYEX"
  
      log 1 "Installing filepp"
      cd tools/mnh_expand
      wget https://www-users.york.ac.uk/~dm26/filepp/filepp-1.8.0.tar.gz
      tar xvf filepp-1.8.0.tar.gz
      cd filepp-1.8.0
      ./configure --prefix="${PWD}"
      make install
      cd ..
      ln -s filepp-1.8.0 filepp
  
      log 1 "Installing MNH_Expand_Array"
      git clone https://github.com/JuanEscobarMunoz/MNH_Expand_Array.git
    fi
    log 1 "Checkout commit ${SHA}"
    cd "${WORKDIR}/PHYEX"
    git fetch "${PHYEXREPOgiturl}"
    git checkout "${SHA}"
    cd "${currentdir}"
    . "${WORKDIR}/PHYEX/tools/env.sh"
    if [ -f "${WORKDIR}/PHYEX/tools/testing.sh" ]; then
      if [ "${currentMD5}" != $(md5sum "${WORKDIR}/PHYEX/tools/testing.sh" | cut -d\  -f1) ]; then
        log 1 "Script has changed, running the new version" #This log and the preivous ones are lost
        exec "${WORKDIR}/PHYEX/tools/testing.sh" $@
      fi
    fi
  fi

  #Enable the gihub project pages
  if [ $enableghpages -eq 1 ]; then
    log 1 "Test if github project pages are enabled"
    enable_gh_pages
  fi

  export MNHPACK="${WORKDIR}/MesoNH"
  for d in "${MNHPACK}" ; do
    if [ ! -d "${MNHPACK}" ]; then
      log 1 "Creating directory ${MNHPACK}"
      mkdir -p "${d}"
    #if [ ! -d "${d}" ]; then
    #  log 1 "Creating directory ${d}"
    #  mkdir -p "${d}"
    fi
  done

    retmodel=0
#   log 0 "Tests for model ${models}"
    log 0 "Tests for Méso-NH branch-57"
    #Model specific configuration
    compilation='-c'
    execution='-r'
    jsonfile="MY_RUN/INTEGRATION_CASES/TOOLS/local_version.json"
    docmp=1

    #Commande
    cmd="/home/rodierq/GIT/MNH-CI/MNH-ladev/MY_RUN/INTEGRATION_CASES/TOOLS/check_commit_${models}.sh --repo-user ${MESONHREPOuser} --repo-protocol ${PHYEXREPOprotocol} ${SHA}"

    #Compilation
    result=0
    if [ ${compil} -eq 1 ]; then
      compilecmd="$cmd ${compilation}"
      log 1 "Compilation with ${compilecmd}"
      set +e
      ${compilecmd}
      result=$?
      set -e
      if [ ${result} -ne 0 ]; then
        retmodel=1
        log 0 "  ${models} compilation: error"
      else
        log 0 "  ${models} compilation: OK"
      fi
    fi

    #Execution and comparison
    if [ ${result} -eq 0 ]; then
      #Get the list of cases with associated references
      cases=$(get_cases "${SHA}" "${jsonfile}")
      [ "${cases}" == "" ] && cases="DEF:DEF"
      for case_ref in ${cases}; do
        case=$(echo "${case_ref}" | cut -d: -f1)
        ref=$(echo "${case_ref}" | cut -d: -f2)
        if [ "${case}" == "DEF" -a "${ref}" == "DEF" ]; then
          casearg=""
          refarg="REF"
          casedescr="default case(s)"
          log 1 "No cases found in ${jsonfile}, we only test the default cases"
        else
          casearg="-t ${case}"
          refarg="${ref}"
          casedescr="${case} (ref=${ref})"
          log 1 "Testing case ${case} against reference ${ref}"
        fi
      
        result=0
        if [ ${execute} -eq 1 ]; then
          execcmd="$cmd ${execution} ${casearg}"
          #execcmd="$cmd ${execution} -t "ALL""
          log 1 "Excution with ${execcmd}"
          set +e
          ${execcmd}
          result=$?
          set -e
          if [ ${result} -ne 0 ]; then
            retmodel=1
            log 0 "  ${models} ${casedescr}: execution error"
          else
            log 0 "  ${models} ${casedescr}: execution OK (but status not reliable)"
          fi
        fi
        if [ ${result} -eq 0 -a ${docmp} -eq 1 -a ${comp} -eq 1 ]; then
          compcmd="$cmd -C ${casearg} ${refarg}"
          log 1 "Comparison with ${compcmd}"
          set +e
          ${compcmd}
          result=$?
          set -e
          if [ ${result} -ne 0 ]; then
            retmodel=1
            log 0 "  ${models} ${casedescr}: comparison error"
          else
            log 0 "  ${models} ${casedescr}: comparison OK"
          fi
        fi
      done

      #Cleaning
      if [ ${remove} -eq 1 ]; then
        cleancmd="${cmd} --remove"
        log 1 "Cleaning with ${cleancmd}"
        set +e
        ${cleancmd}
        result=$?
        set -e
        if [ ${result} -ne 0 ]; then
          retmodel=1
          log 0 "  ${models}: cleaning error"
        else
          log 0 "  ${models}: cleaning OK"
        fi
      fi
    fi
    if [ $retmodel -eq 0 ]; then
      log 0 "..... global result for model $model: OK"
    else
      ret=1
      log 0 "XXXXX global result for model $model: ERROR"
    fi

  #Report result
  report 0 ${ret}
fi
