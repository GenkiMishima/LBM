set term png
set out 'FDM_LBM.png'
set grid
set xrange [0:40]
plot 'data/FDM_001.d'w lp title'FDM001','data/FDM_002.d'w lp title'FDM002','data/FDM_003.d'w lp title'FDM003','data/FDM_004.d'w lp title'FDM004','data/FDM_005.d'w lp title'FDM005','data/LBM_001.d'w lp title'LBM001','data/LBM_002.d'w lp title'LBM002','data/LBM_003.d'w lp title'LBM003','data/LBM_004.d'w lp title'LBM004','data/LBM_005.d'w lp title'LBM005'
