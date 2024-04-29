#!/usr/bin/env bash

#set -xe
set -e

# Default arguments
ribbit_args=("inputs/cubes.ribbit")
profile="debug"
verbose=""
build="false"
exe="ribbit"

pos=0
while test $# -gt 0 ; do
	#echo "$1"

	if [[ "$1" == "--release" ]] ; then
		profile="release"

	elif [[ "$1" == "--debug" ]] ; then
		profile="debug"

	elif [[ "$1" == "--verbose" ]] ; then
		verbose="--verbose"

	elif [[ "$1" == "--build" ]] ; then
		build="true"

	elif [[ "$1" == "--test" ]] ; then
		exe="test"

	elif [[ "$pos" == "0" ]] ; then
		ribbit_args[0]="$1"
		((++pos))
		#echo "pos = $pos"

	else
		ribbit_args+=("$1")

	fi

	shift
done

#echo "profile = $profile"
#echo "ribbit_args = ${ribbit_args[@]}"
#exit 0

# TODO: DRY up args between build and run.  Maybe just make "build" vs "run ribbit"
# or "run test" a variable?
if [[ "$build" == "true" ]]; then
	time fpm build --compiler ifx --c-compiler gcc --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile $profile $verbose -- "${ribbit_args[@]}"
else
	time fpm run "$exe" --compiler ifx --c-compiler gcc --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile $profile $verbose -- "${ribbit_args[@]}"
fi

