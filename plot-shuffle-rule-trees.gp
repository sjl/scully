set terminal png size 1200, 1200 linewidth 3

set xtics font "Lucida Grande,18"
set ytics font "Lucida Grande,18"


set size 0.6, 0.8
set bmargin at screen 0.18
set tmargin at screen 0.9
set lmargin at screen 0.1
set rmargin at screen 0.95

# set xlabel "Game" font "Lucida Grande,12"
set ylabel "Ratio of shuffled node counts to unshuffled node count" font "Lucida Grande,18" offset -1


set xtics rotate by -40 offset 0,-1

set output 'plot-shuffling-rule-trees.png'
set boxwidth 0.2 absolute
set title "Effects of variable order shuffling on rule tree sizes" font "Lucida Grande,24"

set xrange[0.2:8]
set yrange[0.5:1.3]

# Data columns: X Min 1stQuartile Median 3rdQuartile Max BoxWidth Titles

# set bars 4.0
set style fill empty
plot 'data-shuffling-rule-trees' using 1:3:2:6:5:7:xticlabels(8) with candlesticks notitle whiskerbars, \
  ''         using 1:4:4:4:4:7 with candlesticks lt -1 notitle
