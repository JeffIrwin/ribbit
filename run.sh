#!/usr/bin/env bash

#set -xe
set -e

# Default arguments
ribbit_args=("inputs/cubes.ribbit")
profile="debug"
exe="ribbit"

pos=0
while test $# -gt 0 ; do
	#echo "$1"

	# TODO: add --verbose option to pass along to fpm
	if [[ "$1" == "--release" ]] ; then
		profile="release"

	elif [[ "$1" == "--debug" ]] ; then
		profile="debug"

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

time fpm run "$exe" --compiler ifx --c-compiler gcc --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile $profile -- "${ribbit_args[@]}"

