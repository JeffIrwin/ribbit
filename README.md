
# ribbit

Rigid body dynamics 

## Build and run using fpm

Build and run with this one-liner:
```
time fpm.exe run --compiler ifx.exe --flag "-fpp -Qmkl -heap-arrays0" -- inputs/cubes.ribbit
```
where `inputs/cubes.ribbit` is the ribbit input file.  This example is included with
the repository.

Dependencies:
- fpm (fortran package manager)
- ifx (intel fortran compiler)

