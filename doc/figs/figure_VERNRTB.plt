# GNUPlot command file for generating the response function for
# vernalization in the WOFOST system description
# Output is generated as a PNG file on figure7.png
# data points are read from figure7.csv
set xrange [-10:20]
set yrange [-0.1:1.2]
#set terminal svg size 800,600 enhanced font 'Verdana,12'
set terminal pdf
set output 'figure_VERNRTB.pdf'
set xlabel 'Daily average temperature [C]'
set ylabel 'Vernalization response [-]'
plot 'figure_VERNRTB.csv' using 1:2 with lines t'' lt -1 lw 2,
set output
#set term X11
     
