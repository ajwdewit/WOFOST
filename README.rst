WOFOST
======

FORTRAN version of the WOrld FOod STudies (WOFOST) crop simulation model.

1. Contents of this package
---------------------------

After cloning this repository you will find the following folder structure::

     wofost71/
             |-bin/             : Location of executable and 'direct.dat'
             |-cropd/           : Crop data files
             |-doc/             : Documentation (this file)
             |-meteo/
                    |-cabowe/   : Daily weather files
                    |-climd/    : Climatic data
             |-output/          : WOFOST output will end up here
             |-runio/           : Configuration files for WOFOST runs
             |-soild/           : Soil data
             |-sources/         : Make scripts and top-level source code
                      |-cabowe/ : Source code for CABO weather system
                      |-ttutil/ : Source code for TTUTIL library
                      |-w60lib/ : Source code of WOFOST model

WOFOST comes with a set of example files for different crop types and soil
types. Moreover, example weather files are including from weather station 
in the Netherlands and the Philipines.


2. Compiling WOFOST
-------------------
 
This package does not include a WOFOST binary and therefore the WOFOST FORTRAN
source codes must be compiled using the following steps:

1. Install a fortran90 compiler. The WOFOST7 package has been tested using the
   GNU fortran90/95 compiler (e.g. gfortran) which can be installed on a wide 
   variety of platforms. For MicroSoft Windows, gfortran can be downloaded 
   from http://www.mingw.org/. For GNU/Linux, gfortran can usually be installed
   through the systems native package manager. For Mac OSX, gfortran can be 
   downloaded from http://hpc.sourceforge.net/ or installed via the MacPorts 
   system (http://www.macports.org/).
2. Move to the folder 'sources/' and run either the script 'make_wofost7.bat'
   (Windows) or 'make_wofost7.sh' (Linux and Mac OSX) from a command shell.
   Note that these scripts assume that the gfortran executable is in the search
   path of your shell. The script will compile all fortran source files and 
   link the final executable which will be copied to the 'bin/' folder.
   Note that these scripts do not perform any error checking, so watch the
   output.

.. note:: Some issues with compiling WOFOST:

    *  Although WOFOST7 has been written using FORTRAN77, it will not compile using
       the GNU FORTRAN77 compiler (g77), due to the missing SCAN() function in g77.
    *  When installing MinGW on Windows, make sure that the MinGW 'bin' folder is
       included in your path, otherwise WOFOST will complain about missing DLLs.


3. Checking WOFOST
------------------
   
The WOFOST7 package is configured to run a winter-wheat crop in the Netherlands
for the year 1976. Therefore, move to the folder 'bin/' and run the newly
created WOFOST7 executable. It will produce a file 'wofost.out' in the 'output'
folder. You can compare this file with the file 'reference_results.out' in 
order to check if the simulation results are reproduced correctly.


4. More information
-------------------

More information and documentation about WOFOST can be obtained from our website_. The `WOFOST7.1 reference manual`_  contains detailed information on the use of WOFOST and the meaning of variable names in the various files.

.. _website: http://www.wageningenur.nl/wofost
.. _WOFOST7.1 reference manual: http://www.wageningenur.nl/web/file?uuid=5c0873c3-8c07-4ddf-85a3-dd98bdb38781&owner=b875561e-c6d9-442d-b599-58e9d13cb80d
