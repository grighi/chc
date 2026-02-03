/******************************************************************************
Master file for:

The War on Poverty's Experiment in Public Medicine: 
The Impact of Community Health Centers on the Mortality of Older Americans
by Martha Bailey and Andrew Goodman-Bacon

Date: 10/11/2014
******************************************************************************/


*dofile directory (where this file is stored)
global dofile "C:\Users\ajgb\Desktop\aer_dofile"

*data directory (where posted datasets are stored)
global data "C:\Users\ajgb\Desktop\aer_data"

*output directory (where regression output, figures, and logs are saved)
global output "C:\Users\ajgb\Desktop\aer_output"

cd $output

*MAIN TEXT FIGURES AND TABLES
do $dofile/pscore
do $dofile/figure2 
do $dofile/figure4 
do $dofile/figure5 
do $dofile/figure6 
do $dofile/figure7  
do $dofile/figure8 
do $dofile/figure9a 
do $dofile/figure9b
do $dofile/figure9

do $dofile/table1
do $dofile/table2
do $dofile/table3
do $dofile/table4
do $dofile/table5


*APPENDIX C
do $dofile/appC/figureC1

*APPENDIX D
do $dofile/appD/figureD1A
do $dofile/appD/figureD1B
do $dofile/appD/figureD1C
do $dofile/appD/figureD1D
*figure D2 -- see log file from figure 2 for urban-by-year FE
do $dofile/appD/figureD3
do $dofile/appD/figureD4
do $dofile/appD/figureD5
do $dofile/appD/figureD6
do $dofile/appD/figureD7

do $dofile/appD/tableD1
do $dofile/appD/tableD2
do $dofile/appD/tableD3
do $dofile/appD/tableD4
do $dofile/appD/tableD5

*APPENDIX E
do $dofile/appE/figureE1
do $dofile/appE/figureE2
do $dofile/appE/figureE3

do $dofile/appE/tableE1

*APPENDIX F
do $dofile/appF/tableF3
do $dofile/appF/tableF4
do $dofile/appF/tableF5
do $dofile/appF/tableF6

*APPENDIX G
do $dofile/appG/figureG1
do $dofile/appG/figureG2
do $dofile/appG/figureG3

do $dofile/appG/tableG1
do $dofile/appG/tableG2
do $dofile/appG/tableG3
do $dofile/appG/tableG4

*APPENDIX H
do $dofile/appH/figureH1
do $dofile/appH/figureH2
do $dofile/appH/figureH3
do $dofile/appH/figureH4

do $dofile/appH/tableH1
do $dofile/appH/tableH2
do $dofile/appH/tableH3


