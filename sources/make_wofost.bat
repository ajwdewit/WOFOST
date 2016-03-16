REM Compile CABOWE weather files
cd cabowe
del *.o
echo "Compiling CABOWE weather"
gfortran -c -O2 *.for
cd ..

REM Compile TTUTIL
cd ttutil
del *.o
echo "Compiling TTUTIL"
gfortran -c -O2 ttutil.for aux_routines.for
cd ..

REM Compile W60LIB
cd w60lib
del *.o
echo "Compiling W60lib"
gfortran -c -O2 *.for
cd ..

REM Compile and link main program
echo "Compiling and linking main program."
del wofost.exe
gfortran -O2 -o wofost.exe w70main.for cabowe\*.o ttutil\*.o w60lib\*.o

REM Copy to bin folder
echo "copy executable to ..\bin\ folder." 
copy wofost.exe ..\bin\

pause
