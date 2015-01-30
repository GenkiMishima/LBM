set term png
set out 'FDM.png'
set title 'Diff2DAdvecDiffution'
set xrange [0:40]
set yrange [0:1]
set grid
plot 'data/F_001.d' title'2D_UPSTREAMFDM','data/L_001.d' title'D2Q4_LBM'





######set term png
######set out 'FDM.png'
######set xrange [0:40]
######set yrange [0:40]
######set zrange [0:1]
######set grid
######set pm3d map
######set palette rgbformulae 22,13,-31
#######splot 'data/FDM_001.d'#with pm3d title'FDM001'#,'data/FDM_002.d'w lp title'FDM002','data/FDM_003.d'w lp title'FDM003','data/FDM_004.d'w lp title'FDM004','data/FDM_005.d'w lp title'FDM005'
######splot 'data/FDM_005.d' title'FDM005'
######set out 'LBM.png'
######set xrange [0:40]
######set yrange [0:40]
######set zrange [0:1]
######set grid
######set pm3d map
######splot 'data/LBM_005.d' title'LBM005'
#######splot 'data/LBM_001.d'#w p title'LBM001'#,'data/LBM_002.d'w lp title'LBM002','data/LBM_003.d'w lp title'LBM003','data/LBM_004.d'w lp title'LBM004','data/LBM_005.d'w lp title'LBM005'
