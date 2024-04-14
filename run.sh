#!/usr/bin/env bash

#set -xe
set -e

#if [[ $# -ge 1 ]] ; then
#	ribbit_file=$1
#else
#	ribbit_file="inputs/cubes.ribbit"
#fi

# Default arguments
ribbit_file="inputs/cubes.ribbit"
profile="debug"

pos=0
while test $# -gt 0 ; do
	#echo "$1"

	# TODO: add --verbose option to pass along to fpm
	if [[ "$1" == "--release" ]] ; then
		profile="release"
	elif [[ "$1" == "--debug" ]] ; then
		profile="debug"

	elif [[ "$pos" == "0" ]] ; then
		ribbit_file="$1"
		((++pos))
		#echo "pos = $pos"

	else
		echo "Error: bad argument \"$1\""
		exit -1
	fi

	shift
done

#echo "profile = $profile"
#exit 0

time fpm run --compiler ifx --c-compiler gcc --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile $profile -- "$ribbit_file"

