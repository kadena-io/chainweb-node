{ runCommand, patchelf, coreutils, binutils-unwrapped, find-libs }:

targetPrefix: inputPackage:

runCommand (inputPackage.pname + "-portable") {
  inherit inputPackage targetPrefix;
} ''
  set -euo pipefail

  cp -a "$inputPackage" "$out"
  chmod -R u+w "$out"
  mkdir -p "$out/lib"

  processLib() {
    lib="$1"
    target="$2"

    cp "$lib" "$target"
    chmod u+w "$target"
    if [ ! -z "$("${patchelf}/bin/patchelf" --print-rpath "$target")" ] ; then
      "${patchelf}/bin/patchelf" --set-rpath "$targetPrefix/lib" "$target"
    fi
  }

  addLib() {
    lib="$1"

    libname="$(${coreutils}/bin/basename "$lib")"
    target="$out/lib/$libname"
    if [ -e "$target" ] ; then
      tmpfile="$("${coreutils}/bin/mktemp" "make-portable.XXXXXXXX")"
      processLib "$lib" "$tmpfile"
      diff "$target" "$tmpfile" >/dev/null
      libsAreIdentical=$?
      rm "$tmpfile"

      if [ "$libsAreIdentical" -ne 0 ] ; then
        2>&1 echo "error: multiple libraries named $libname are required, and they are not identical files"
        exit 1
      fi
      echo "Library $lib already present - skipping"
    else
      echo "Adding library $lib"
      processLib "$lib" "$target"
    fi
  }

  for exe in $out/bin/* ; do
    echo "Processing executable $exe"
    for lib in $("${find-libs}/bin/find-libs" "$exe") ; do
      addLib "$lib"
    done
    interpreter="$("${patchelf}/bin/patchelf" --print-interpreter "$exe")"
    addLib "$interpreter"
    "${patchelf}/bin/patchelf" --set-interpreter "$targetPrefix/lib/$("${coreutils}/bin/basename" "$interpreter")" --set-rpath "$targetPrefix/lib" "$exe"
  done
''
