set term png enhanced font "./verdana.ttf" 18 size 2000,2000
set output "activityplot.png"
set key outside

set multiplot
set grid
set size 1.0,0.5
set origin 0.0,0.5
plot 'activityplot.dat' using 1:5:(0) title 'Tasks completed' with lines lw 4
set xlabel "Time in seconds"
set size 1.0,0.5
set origin 0,0
plot 'activityplot.dat' using 1:2:(0) title 'Staging in' with filledcurves, \
     '' using 1:3:(0) title 'Active' with filledcurves, \
     '' using 1:3:(0) title "" w lines lt 2 lc rgb "black", \
     '' using 1:4:(0) title 'Staging out' with filledcurves, \
     '' using 1:4:(0) title "" w lines lt 2 lc rgb "black" 
unset multiplot
