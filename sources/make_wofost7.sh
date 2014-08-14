# Compile CABOWE weather files
cd cabowe
rm *.o
echo "Compiling CABOWE weather"
gfortran -c -O2 *.for
cd ..

# Compile TTUTIL
cd ttutil
rm *.o
echo "Compiling TTUTIL"
gfortran -c -O2 ttutil.for aux_routines.for
cd ..

# Compile W60LIB
cd w60lib
rm *.o
echo "Compiling W60lib"
gfortran -c -O2 *.for
cd ..

# Compile and link main program
gfortran -O2 -o wofost7 w70main.for cabowe/*.o ttutil/*.o w60lib/*.o
mv wofost7 ../bin/
