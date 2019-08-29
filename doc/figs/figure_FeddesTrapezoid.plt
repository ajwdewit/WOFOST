# GNUPlot command file for generating figure 7 in the WCC/WOFOST manual
# Output is generated as a PNG file on figure7.png
# data points are read from figure7.csv
set xrange [0:0.35]
set yrange [-0.05:1.2]
set terminal svg size 800,600 enhanced font 'Verdana,10'
#set terminal pngcairo size 800,600 enhanced font 'Verdana,10'
set output 'figure_FeddesTrapezoid.svg'
set xlabel 'Soil Water Content [cm^{3} cm^{-3}]'
set ylabel 'T_{a}/T_{p} [-]'
plot 'figure_FeddesTrapezoid.csv' using 1:2 with lines t'' lt -1 lw 2,\
     'figure_FeddesTrapezoid.csv' using 1:3 with lines t'' lt 0 lw 2 
set output
set term X11
     
