# About the project

Fusion is part of an ongoing project to study human-machine reciprocal learning. Fusion is a software system designed to support Human-Machine reciprocal learning configured for text classification tasks. Some aspects of the project's work have been published ([CSCW 2021 paper](https://doi.org/10.1145/3479587)).


## Fusion Development Requirements

For Fusion's development we used R version 4.0.2 (2020-06-22).
In order to install the required packages, please use the [init.R](fusion/init.R) script file.

<details><summary>More specific development session info</summary>
<p>

R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.10

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] shinydashboardPlus_0.7.5 shinyWidgets_0.5.4       shinydashboard_0.7.1     openxlsx_4.2.3          
 [5] httr_1.4.2               jsonlite_1.7.2           d3Tree_0.2.2             shinyPagerUI_0.2.0      
 [9] tidytext_0.3.0           highcharter_0.8.2        vip_0.3.2                ggthemes_4.2.0          
[13] pool_0.1.5               odbc_1.3.0               htmlTable_2.1.0          compareDF_2.3.1         
[17] DT_0.17                  forcats_0.5.0            stringr_1.4.0            dplyr_1.0.2             
[21] purrr_0.3.4              readr_1.4.0              tidyr_1.1.2              tibble_3.0.4            
[25] tidyverse_1.3.0          rsample_0.0.8            caTools_1.18.0           wesanderson_0.3.6       
[29] anytime_0.3.9            waffle_0.7.0             e1071_1.7-4              DMwR_0.4.1              
[33] AUC_0.3.0                caret_6.0-86             ggplot2_3.3.3            lattice_0.20-41         
[37] glmnet_4.0-2             Matrix_1.2-18            lsa_0.73.2               text2vec_0.6            
[41] SnowballC_0.7.0          tm_0.7-8                 NLP_0.2-1                qdap_2.4.3              
[45] RColorBrewer_1.1-2       qdapTools_1.3.5          qdapRegex_0.7.2          qdapDictionaries_1.0.7  
[49] config_0.3.1             shiny_1.5.0             

</p>
</details>


## Running a local server (accessible from remote clients on port 7775)
```R
library(shiny)
runApp(appDir = 'git/fusion-project/fusion', port=7775, host='0.0.0.0')
```

## Deploying to Shinyapps.io

1. Create an account on and log-in to [https://www.shinyapps.io](https://www.shinyapps.io).
2. Install the [rsconnect](https://cran.r-project.org/web/packages/rsconnect/index.html) package inside R-studio, and configure it to your personal shinyapps.io account following the instructions. 
3. Deploy Fusion to shinyapps.io (note this will deploy all files inside that folder, including local DB files if those exist):
```R
library(rsconnect)
rsconnect::deployApp('git/fusion-project/fusion')
```
4. It should now be accessible at `https://[your-username].shinyapps.io/fusion`.

## This project is lead by:
Prof. Dov Te'eni 
Dr. Inbal Yahav
Dr. Alexey Zagalsky
Prof. David G. Schwartz
