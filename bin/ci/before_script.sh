#!/usr/bin/env bash

set -e

# Grab and save the path to this script
# http://stackoverflow.com/a/246128
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$(cd -P "$(dirname "$SOURCE")" && pwd)"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we
  need to resolve it relative to the path where the symlink file was located
done
SCRIPTDIR="$(cd -P "$(dirname "$SOURCE")" && pwd)"
# echo "${SCRIPTDIR}" # For debugging

if ! (command -v java >/dev/null); then
  echo "Installing OpenJDK 11"
  sudo apt install -y openjdk-11-jre
fi
if ! (command -v java >/dev/null); then
  echo "You must install OpenJDK 11 before running this."
  exit 1
fi

cd "${SCRIPTDIR}/../../../"
git clone https://github.com/chrisl8/witchazzan-client.git
