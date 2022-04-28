#!/bin/bash -e

server="gfortran.meteodat.ch"
instdir="/usr/local/gfortran-bin"
arch="x86_64"

if [ $# -eq 0 -o "$1" == "-h" -o "$1" == "--help" ]; then
  echo ""
  echo "Download and installation script for $arch binaries found on $server"
  echo ""
  echo -n "USAGE: $0 [ -i|--infrastructure ] [ -n|--no-infrastructure ]"
  echo    " [ -d|--instdir path ] fileind..."
  echo "       -i: approp. infrastructure package will be fetched and installed [default]"
  echo "       -n: no infrastructure package will be installed"
  echo "       -d: installation directory, will be created if needed [default: $instdir]"
  echo "       File indication fileind:"
  echo "        - either name (e.g. gcc-4.4-20120228.tar.xz, gcc-trunk-20140318-r208630.tar.xz"
  echo "             or gcc-5.1.0.tar.xz)"
  echo "        - or (only for snapshots or nightlies) GCC version indication (e.g. 5, 4.4, trunk),"
  echo "             latest package will be installed"
  echo ""
  echo "The script optionally uses patchelf to fix hardwired rpath in libgfortran.so and lib*san.so"
  echo ""
  exit
fi

infra="true"

if [ "$arch" == "i686" ]; then
  arch2="-32bit"
else
  arch2=""
fi
origpwd=`pwd`

if tty -s; then
  wgetopt=""
else
  wgetopt="--no-verbose"
fi

while [ $# -gt 0 ]; do

  if [ "$1" == "-d" -o "$1" == "--instdir" ]; then
    instdir="$2"
    shift 2
    continue
  fi

  fileind=$1
  shift

  if [ "$1" == "-i" -o "$1" == "--infrastructure" ]; then
    infra="true"
  fi
  if [ "$1" == "-n" -o "$1" == "--no-infrastructure" ]; then
    infra="false"
  fi
  if [ "$infra" == "true" ]; then
    infraurl="http://$server/download/$arch/gcc-infrastructure${arch2}.tar.xz"
  fi

  if [ ! -d "$instdir" ]; then mkdir -p "$instdir"; fi
  cd "$instdir" || exit 1
  rm -rf *

  if [ ! -f "$fileind" ]; then
    if [[ "${fileind:0:1}" =~ [0-9] ]] || [ "$fileind" == "trunk" ]; then
      if [[ "${fileind:0:1}" =~ [0-9] ]]; then
        urlbase="http://$server/download/$arch/snapshots"
      else
        urlbase="http://$server/download/$arch/nightlies"
      fi
      gfc_file=`wget --no-verbose "$urlbase/" -O - | grep "\<gcc-$fileind" \
        | tail -n 1 | sed 's/.*a href="\([^"]*\)".*/\1/'`
    else
      if [[ "$fileind" =~ trunk ]]; then
        urlbase="http://$server/download/$arch/nightlies"
      else
        urlbase="http://$server/download/$arch/snapshots"
      fi
      gfc_file="$fileind"
    fi
    #---get GCC binary and install it
    echo ""
    echo "downloading and installing $urlbase/$gfc_file ..."
    wget $wgetopt "$urlbase/$gfc_file" -O- | xzcat | tar -x --strip-components=1 -f-
  else
    #---install local binary
    echo ""
    echo "installing file $fileind ..."
    xzcat "$fileind" | tar -x --strip-components=1 -f-
  fi

  cd "$origpwd"
  #---patch rpath (libgfortran.so and lib*san.so)
  if { type patchelf >/dev/null ;}; then
    if { type readlink >/dev/null ;}; then  # canonicalize path, needed when relative path given
      instdir2=`readlink -m "$instdir"`
    else
      instdir2="$instdir"
    fi
    for libdir in lib lib64; do
      for fil in $instdir/$libdir/lib*.so*; do
        if [[ "$fil" =~ [.]py$ ]] || [[ "$fil" =~ libgcc_s.so$ ]]; then continue; fi
        if [ -f "$fil" -a ! -L "$fil" ]; then
          oldrpath=`patchelf --print-rpath "$fil"`
          if [ "$oldrpath" ]; then
            echo "patching rpath of $fil"
            patchelf --set-rpath "$instdir2/$libdir" "$fil"
          fi
        fi
      done
    done
  else
    echo "patchelf not found, no rpath-patching done."
  fi

  #---Infrastructure
  if [ "$infra" == "true" ]; then
    cd "$instdir"
    echo ""
    echo "downloading and installing infrastructure ..."
    wget $wgetopt "$infraurl" -O- | xzcat | tar -x -f-
  fi

  cd "$origpwd"
  echo ""
  echo ""
  $instdir/bin/gfortran -v
  echo ""
done

exit
