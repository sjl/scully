set terminal png size 1200, 1200 linewidth 3

set output 'plot-iset-sizes-latenttictactoe.png'

set xtics font "Lucida Grande,20"
set ytics font "Lucida Grande,20"

set size 0.6, 0.8
set bmargin at screen 0.18
set tmargin at screen 0.9
set lmargin at screen 0.1
set rmargin at screen 0.95

set xlabel "States in Information Set" font "Lucida Grande,20" offset 0,-1
set ylabel "Nodes/Objects" font "Lucida Grande,20" offset -1

set key on font "Lucida Grande,20" bottom right at screen 0.98,0.05

# set xtics rotate by -40 offset 0,-1

set title "Information set sizes for LATENTTICTACTOE" font "Lucida Grande,24" offset 0,1

# set xrange[0.2:8]
# set yrange[0.5:1.3]

# Data columns: game, state count, node-count, object-size

plot 'data-iset-sizes' using 2:3 with points pointsize 2 pointtype 2 title "ZDD Node Count", \
     'data-iset-sizes' using 2:4 with points pointsize 2 pointtype 4 title "Vanilla Object Size"
