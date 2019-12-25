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

if ! (command -v curl >/dev/null);then
  echo "Installing curl"
  sudo apt install -y curl
fi

if ! (command -v java >/dev/null); then
  echo "Installing OpenJDK 11"
  sudo apt install -y openjdk-11-jre
fi
if ! (command -v java >/dev/null); then
  echo "You must install OpenJDK 11 before running this."
  exit 1
fi

cd "${SCRIPTDIR}"
if ! (command -v clj >/dev/null); then
  echo "Installing Clojure"
  VERSION=1.10.1.492
  curl -O "https://download.clojure.org/install/linux-install-${VERSION}.sh"
  chmod +x "linux-install-${VERSION}.sh"
  sudo "${SCRIPTDIR}/linux-install-${VERSION}.sh"
  rm "linux-install-${VERSION}.sh"
fi
if ! (command -v clj >/dev/null); then
  echo "You must install Clojure before running this."
  echo "See: https://clojure.org/guides/getting_started"
  exit 1
fi
if ! (command -v lein >/dev/null) && ! (command -v "${SCRIPTDIR}/lein" >/dev/null); then
  echo "Installing Leiningen"
  wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
  chmod +x lein
  "${SCRIPTDIR}/lein"
fi
if ! (command -v lein >/dev/null) && ! (command -v "${SCRIPTDIR}/lein" >/dev/null); then
  echo "You must install Leiningen before running this."
  echo "See: https://leiningen.org/#install"
  exit 1
fi
if ! [[ -e "${SCRIPTDIR}/config/config.clj" ]]; then
  cp "${SCRIPTDIR}/config/config.clj.default" "${SCRIPTDIR}/config/config.clj"
fi
echo "To start the Witchazzan Server run:"
echo "./lein repl"
