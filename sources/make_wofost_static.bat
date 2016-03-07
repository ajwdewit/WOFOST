REM ---------------------------------------------------------------
REM Make script used to generate static WOFOST executable used 
REM for distributing with the WOFOST Control Centre under Windows.
REM ---------------------------------------------------------------

REM ---------------------------------------------------------------
REM Location to MinGW environment and FORTRAN compiler
REM
REM Change these values based on your MinGW setup and version!!
REM Note: Ensure that the location of MINGW is in your system path
set MINGW=d:\mingw\
set GF=%MINGW%\bin\gfortran.exe
set VERSION=4.8.1
REM ---------------------------------------------------------------


REM Compile CABOWE weather files
cd cabowe
del *.o
echo "Compiling CABOWE weather"
%GF% -march=i586 -static-libgcc -c -O2 *.for
cd ..

REM Compile TTUTIL
cd ttutil
del *.o
echo "Compiling TTUTIL"
%GF% -march=i586 -static-libgcc -c -O2 ttutil.for aux_routines.for
cd ..

REM Compile W60LIB
cd w60lib
del *.o
echo "Compiling W60lib"
%GF% -march=i586 -static-libgcc -c -O2 *.for
cd ..

REM Compile and link main program
echo "Compiling and linking main program."
del wofost.exe
%GF% -march=i586 -static-libgcc -O2 -o wofost.exe w70main.for cabowe\*.o ttutil\*.o w60lib\*.o %MINGW%\lib\gcc\mingw32\%VERSION%\libgfortran.a

REM Copy to bin folder
echo "copy executable to ..\bin\ folder." 
copy wofost.exe ..\bin\

pause
