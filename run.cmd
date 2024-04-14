
rem c.f. run.sh

rem Default arguments
set ribbit_file="inputs/cubes.ribbit"
rem set profile="debug"
set profile="release"

rem TODO: parse cmd args.  or just convert to bash or consolidate with run.sh
rem and run it in git-bash
rem 
rem The only differences between run.cmd and run.sh are c compiler and Qmkl/qmkl
rem case sensitivity

rem echo "profile = $profile"

fpm run --compiler ifx --c-compiler cl --flag "-fpp -Qmkl -heap-arrays0" --verbose --profile %profile% -- %ribbit_file%

rem TODO: build may fail because cl likes to ignore "-o" flag since it thinks
rem it's cool to be different.  In that case, you may need to copy the
rem corresponding object file of utils.c to the build dir:
rem
rem     copy utils.obj build\ifx_{GUID}\ribbit\src_utils.c.o  
rem
rem And then run "fpm ..." again to link the exe

