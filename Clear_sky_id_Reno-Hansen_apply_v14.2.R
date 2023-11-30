# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "*Identification of Periods of Clear Sky Irradiance in
#'  Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Construction of and examples of clear sky models"
#'
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#' - \usepackage{multicol}
#' - \setlength{\columnsep}{1cm}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     toc_depth:        4
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' ---
#'
#+ include=FALSE, echo=FALSE

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'    )

####_ Notes _####

#
#  Use the optimized alpha for the selected models and identify Clear-Sky
#  minutes.
#


####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "Clear_sky_id_Reno-Hansen_apply_v14_2.R"

if (!interactive()) {
    pdf( file = paste0("~/CS_id/REPORTS/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/CS_id/REPORTS/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}

options("width" = 130)



library(RColorBrewer)
library(scales)
library(pander)
library(caTools)
library(data.table)
library(yardstick)
source("~/CODE/R_myRtools/myRtools/R/trigonometric.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("~/Aerosols/RAerosols/R/statistics.R")



## some plot configs ####
def.par <- par(no.readonly = TRUE) # save default, for resetting...
kcols   <- brewer.pal(11, "Set3")

####  data walk function  ####
walk <- function(i, nt_hw, tot_p) {
    if (i <= nt_hw) {
        w_sta <<- 1
        w_end <<- i + nt_hw
    } else if ( i >= tot_p - nt_hw ) {
        w_sta <<- i - nt_hw
        w_end <<- tot_p
    } else {
        w_sta <<- i - nt_hw
        w_end <<- i + nt_hw
    }
}


## Use monthly optimization of models

# MONTHLY     <- T
MONTHLY     <- FALSE
TEST        <- FALSE
# TEST        <- TRUE

SAMPLE_DAYS <- 1000  ## The total number of days to sample from data
START_DAY   <- "1993-01-01"
END_DAY     <- Sys.Date()

if (TEST) {
    warning("Test is active")
    START_DAY <- "2022-01-01"
    END_DAY   <- "2022-12-01"
}

## load previous state have to override it for alpha to be used
if (MONTHLY) {
    load("~/Aerosols/DATA/model_opt/Combinations_results_m_2019-12-12_082356.Rds")
} else {
    load("~/Aerosols/DATA/model_opt/Combinations_results_2022-06-14_153313.Rds")
}


## Define the model to use for CS detection be inspecting the results of the 'a' optimization script.
model_selection <- "HAU"
monthly_alphas  <- gather_results[gather_results$CS_models == model_selection,]

## daily plots file name
plotsbase <- paste0("~/CS_id/REPORTS/DAILY/",
                    sub("\\.R$", "", basename(Script.Name)), "_")


par(mar = c(2, 4, 2, 1))



#+ include=F, echo=F
source("/home/athan/Aerosols/source_R/THEORY/Extraterrestrial_radiation_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Air_mass_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Clear_sky_irradiance_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Linke_turbidity_models.R")
#'



#'
#' \newpage
#'
#' ## DETECTION OF CLEAR PERIODS IN GHI AND DNI MEASUREMENTS ##
#'
#+ include=T, echo=FALSE


##  Load data from QCRad procedure   -------------------------------------------
## The "strict" input files were used before
strict_files <- list.files(path       = "/home/athan/DATA/Broad_Band/QCRad_LongShi/",
                           pattern    = "QCRad_LongShi_v8_apply_CM21_CHP1_[0-9]{4}.Rds",
                           full.names = TRUE ,
                           recursive  = FALSE)
strict_files <- sort(strict_files)

if (TEST) {
    gather <- c()
    year(START_DAY):year(Sys.Date())
    for (ay in year(START_DAY):year(END_DAY)) {
        gather <- c(gather, grep(ay, strict_files, value = T))
    }
    strict_files <- gather
}




## build one data.frame
strict <- data.table()
for (in_f in strict_files) {
    stctemp <- data.table(readRDS(in_f))

    ## remove older system flags
    stctemp$QCF_DIR <- NULL
    stctemp$QCF_GLB <- NULL

#     ##TODO we have setup filters with the assumption of good.
#     ## have to check the logic of other filters
#     allow   <- c( "good", "Possible Direct Obstruction (23)")
#     stctemp <- stctemp[ QCF_DIR %in% allow | QCF_GLB %in% allow ]


    ## some new columns from BB need to use fill=T
    names(strict) %in% names(stctemp)
    names(stctemp)[!names(stctemp) %in% names(strict)]

    strict  <- rbind(strict, stctemp, fill = TRUE)
    rm(stctemp)
}

## create the main flag to include exclude data from processing
strict$QCF_DIR      <- TRUE
strict$QCF_GLB      <- TRUE


## Select data to use!! --------------------------------------------------------

#'
#' ## Excluded data summary
#'
#' Some data are excluded from processing
#'

## we have no reason to ignore that data

## F1
strict$QCF_DIR_01   <- NULL
strict$QCF_GLB_01   <- NULL
strict$QCv9_01_dir_flag <- NULL
strict$QCv9_01_glb_flag <- NULL
## F2
strict$QCF_GLB_02   <- NULL
strict$QCF_DIR_02   <- NULL
strict$QCv9_02_dir_flag <- NULL
strict$QCv9_02_glb_flag <- NULL
## F4
strict$QCF_DIR_04_1 <- NULL
strict$QCF_DIR_04_2 <- NULL
strict$QCF_GLB_04_1 <- NULL
strict$QCF_GLB_04_2 <- NULL
strict$QCv9_04_dir_flag <- NULL
strict$QCv9_04_glb_flag <- NULL




strict$QCF_GLB_09   <- NULL
strict$QCF_BTH_06_1 <- NULL




## 3. COMPARISON TESTS PER BSRN “non-definitive”
## remove only some of the offending data
warning("Disabled this for trends !!")
# test <- strict[ !is.na(QCv9_03_low_flag) | !is.na(QCv9_03_upp_flag) ]
# hist(test$Elevat,  breaks = 100)
# hist(test$Azimuth, breaks = 100)
# ## trends use data above 5
# strict[((!is.na(QCv9_03_low_flag) | !is.na(QCv9_03_upp_flag)) & Elevat < 15), QCF_DIR := FALSE]
# strict[((!is.na(QCv9_03_low_flag) | !is.na(QCv9_03_upp_flag)) & Elevat < 15), QCF_GLB := FALSE]
strict$QCF_BTH_03_1 <- NULL
strict$QCF_BTH_03_2 <- NULL
strict$QCv9_03_low_flag <- NULL
strict$QCv9_03_upp_flag <- NULL
## check the tables
cat(print(table(strict$QCF_DIR)))
cat(print(table(strict$QCF_GLB)))




## 5. No tracking instances
## These will cause problems in CS detection
## No tracking or obstacles on Direct should be remove manual with exclusions?
strict$QCF_DIR_05 <- NULL


## 6.  Rayleigh Limit Diffuse Comparison
## On this region can not infer CS results
# Lim6_azim <- 130
# Lim6_elev <- 13
# test <- strict[ !is.na(QCF_BTH_06_2) & (!QCF_GLB) & (!QCF_DIR)  ]
# hist(test$Elevat,  breaks = 100)
# abline(v = Lim6_elev, col = "red")
# hist(test$Azimuth, breaks = 100)
# abline(v = Lim6_azim, col = "red")
# test2 <- strict[ !is.na(QCF_BTH_06_2) & (!QCF_GLB) & (!QCF_DIR) & Azimuth < Lim6_azim & Elevat < Lim6_elev ]
# hist(test2$Elevat,  breaks = 100)
# hist(test2$Azimuth, breaks = 100)

warning("Disabled this for trends !!")
# strict[!is.na(QCF_BTH_06_2) & (!QCF_GLB) & (!QCF_DIR) & Azimuth < Lim6_azim & Elevat < Lim6_elev, QCF_DIR := FALSE]
# strict[!is.na(QCF_BTH_06_2) & (!QCF_GLB) & (!QCF_DIR) & Azimuth < Lim6_azim & Elevat < Lim6_elev, QCF_GLB := FALSE]
strict$QCF_BTH_06_2 <- NULL
## check the tables
cat(print(table(strict$QCF_DIR)))
cat(print(table(strict$QCF_GLB)))



## TODO 7.



## 8. Ignore data with inverted values
table(strict$QCF_BTH_08_1)
table(strict$QCF_BTH_08_2)
## set inclusion flags
warning("Disabled this for trends !!")
# strict[!is.na(QCF_BTH_08_1), QCF_DIR := FALSE]
# strict[!is.na(QCF_BTH_08_1), QCF_GLB := FALSE]
# strict[!is.na(QCF_BTH_08_2), QCF_DIR := FALSE]
# strict[!is.na(QCF_BTH_08_2), QCF_GLB := FALSE]
strict$QCF_BTH_08_1 <- NULL
strict$QCF_BTH_08_2 <- NULL
## check the tables
cat(print(table(strict$QCF_DIR)))
cat(print(table(strict$QCF_GLB)))




## FIXME some duplicates rows exist in the database!!!
strong <- unique(strict)
rm(strict)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



##  Limit date span of the data ------------------------------------------------
cat(paste("Use data after date:", START_DAY), "\n\n")
strong[, Day := as.Date(Date)]
strong <- strong[ strong$Day >= as.Date(START_DAY), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## days to parse
dayslist      <- unique( strong$Day )

## add id column
strong$CSflag <- 99



#'
#' ### Data set input.
#'
#' First day:      `r min(dayslist)`
#'
#' Last day:       `r max(dayslist)`
#'
#' Days available: `r length(dayslist)`
#'

#### Exclude inversions ####
warning("Disabled this for trends !!")
# inverted <- strong$wattGLB < strong$wattHOR
inverted <- 0

#'
#' ### There are instances where global irradiance is less than direct.
#'
#' This happens for `r sum(inverted, na.rm = T)`
#' minutes (records), near sunset and sunrise due to obstacles,
#' or due to different sunrise/sunset time due to small spatial differences.
#' We will exclude all this data both for global and direct.
#'
#+ include=T, echo=FALSE
warning("Disabled this for trends !!")
# strong[inverted , QCF_DIR := FALSE ]
# strong[inverted , QCF_GLB := FALSE ]




#### Remove measurement without good quality code !! ####
warning("Remove measurement without good quality code !!")
warning("Disabled this for trends !!")
# strong[QCF_DIR == FALSE, wattDIR     := NA]
# strong[QCF_DIR == FALSE, wattHOR     := NA]
# strong[QCF_DIR == FALSE, wattDIR_sds := NA]
# strong[QCF_DIR == FALSE, wattDIF     := NA]
# strong[QCF_GLB == FALSE, wattGLB     := NA]
# strong[QCF_GLB == FALSE, wattGLB_sds := NA]
# strong[QCF_GLB == FALSE, wattDIF     := NA]







##  Filters to user  -----------------------------------------------------------
MeanVIP_active <- FALSE  ##  1. Mean value of irradiance during the time period
MaxVIP_active  <- FALSE  ##  2. Max value of irradiance during the time period
VIL_active     <- FALSE  ##  3. Variability in irradiance by the length (VIL)
VCT_active     <- FALSE  ##  4. Variance of Changes in the Time series (VCT)
VSM_active     <- FALSE  ##  5. Variability in the Shape of the irradiance Measurements
LDI_active     <- FALSE  ##  6. Low Direct Irradiance limit (LDI)
LGI_active     <- FALSE  ##  7. Low Global Irradiance limit (LGI)
FCS_active     <- FALSE  ##  8. Too Few CS point for the day (FCS)
FDP_active     <- FALSE  ##  9. Too Few data point for the day (FDP)
DST_active     <- FALSE  ## 11. Too low direct radiation (DsT)
FAST_SKIP      <- FALSE

## Reno-Hansen filters control -------------------------------------------------
MeanVIP_active <- TRUE   ##  1. Mean value of irradiance during the time period
MaxVIP_active  <- TRUE   ##  2. Max value of irradiance during the time period
VIL_active     <- TRUE   ##  3. Variability in irradiance by the length (VIL)
VCT_active     <- TRUE   ##  4. Variance of Changes in the Time series (VCT)
VSM_active     <- TRUE   ##  5. Variability in the Shape of the irradiance Measurements

## My filters control  ---------------------------------------------------------
LDI_active     <- TRUE   ##  6. Low Direct Irradiance limit (LDI)
                         ##     this also excludes points due to pole shade at
                         ##     afternoon and building in the morning
LGI_active     <- TRUE   ##  7. Low Global Irradiance limit (LGI)
                         ##     Global irradiance below this level can not be identified
FCS_active     <- TRUE   ##  8. Too Few CS point for the day (FCS)
FDP_active     <- TRUE   ##  9. Too Few data point for the day (FDP)
DST_active     <- TRUE   ## 11. Too low direct radiation (DsT)
FAST_SKIP      <- FALSE  ## allow faster skip of filters also reduce data kept


## Ignore filters with direct for pure GLB data process!!  ---------------------
## For GHI Trends
IGNORE_DIRE     <- TRUE
if (IGNORE_DIRE) {
    cat("\nIgnoring filters using Direct radiation For Trends!!\n\n")
    LDI_active <- FALSE  ##  6. Low Direct Irradiance limit (LDI)
    DST_active <- FALSE  ## 11. Too low direct radiation (DsT)
    LGI_active <- FALSE  ##  7. Low Global Irradiance limit (LGI) Allow all data for trends
    FCS_active <- FALSE  ##  9. Too Few data point for the day (FDP) No need can be done after
}





##  Variables initialization -- ------------------------------------------------

MS <- data.frame( nt                   = 11,          ##     Window size prefer odd numbers so not to be left right bias
                  MeanVIP_fct          =  1,          ##  1. Factor Mean Value of Irradiance during the time Period (MeanVIP)
                  CS_ref_rm_VIP_low    = 70,          ##  1. MIN Offset Mean Value of Irradiance during the time Period (MeanVIP) Old
                  CS_ref_rm_VIP_upp    = 80,          ##  1. MAX Offset Mean Value of Irradiance during the time Period (MeanVIP) Old
                  CS_ref_rm_VIP_RelUpp =  0.095,      ##  1. MAX Offset Mean Value of Irradiance during the time Period (MeanVIP) New
                  CS_ref_rm_VIP_RelLow =  0.09,       ##  1. MIN Offset Mean Value of Irradiance during the time Period (MeanVIP) New
                  CS_ref_rm_VIP_UppOff = 30,          ##  1. MAX Offset Mean Value of Irradiance during the time Period (MeanVIP) New
                  CS_ref_rm_VIP_LowOff = 20,          ##  1. MIN Offset Mean Value of Irradiance during the time Period (MeanVIP) New
                  MaxVIP_fct           =  1 ,         ##  2. Factor Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIP_off_upp       = 75,          ##  2. MAX Offset Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIP_off_low       = 75,          ##  2. MIN Offset Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIL_fct           =  1.3,        ##  3. MAX Factor Variability in irradiance by the length (VIL)
                  MinVIL_fct           =  1,          ##  3. MIN Factor Variability in irradiance by the length (VIL)
                  offVIL_upl           = 13,          ##  3. MAX Offset Variability in irradiance by the length (VIL)
                  offVIL_dwl           =  5,          ##  3. MIN Offset Variability in irradiance by the length (VIL)
                  offVCT               =  0.00011,    ##  4. Variance of Changes in the Time series (VCT)
                  offVSM               =  7.5,        ##  5. Variability in the Shape of the irradiance Measurements (VSM)
                  alpha                =  1,
                  relative_CS          =  0.3,        ## limit of CS day acceptance 0.3: 30% of the data points have to be id as clear within a day
                  sample_n_days        = SAMPLE_DAYS, ## Sample of that much days to be evaluated
                  tolerance            =  0.02,       ## Optimization algorithm tolerance
                  start_day            = START_DAY,   ## The first day of the data we are using
                  DIR_s_T_fact         = 25           ## percentage of direct under simple threshold CS_ref_DIR (TESTING)
)


CS_flags <- c( "MeanVIP",   #1
               "MaxVIP",    #2
               "VIL",       #3
               "VCT",       #4
               "VSM",       #5
               "LDI",       #6
               "LGI",       #7
               "FCS",       #8
               "FDP",       #9
               "NA",        #10
               "DsT",       #11
               NULL
             )


#'
#' ### Threshold Values for Criteria
#'
#' For mean and max Most evaluations of clear sky models find that the average
#' bias error of the model is less than 10%, often around 7% [1, 83]. Therefore,
#' a fixed threshold of $\pm 75 W / m^2$ within the mean and max of the clear sky model
#' was chosen.
#'

# alpha_models <- data.frame(
#     DPP      = c( 0.9  , 1.25 , 1.054567  ),
#     KC       = c( 1    , 1.25 , 1.15983   ),
#     HAU      = c( 0.8  , 1.20 , 0.9858765 ),
#     BD       = c( 0.95 , 1.25 , 1.073723  ),
#     ABCG     = c( 0.95 , 1.30 , 1.102558  ),
#     RS       = c( 0.90 , 1.25 , 1.00482   ),
#     KASTEN   = c( 0.95 , 1.25 , 1.12107   ),
#     INEICHEN = c( 0.85 , 1.25 , 1.155573  ),
#     row.names = c("lower","upper","best") )

## select diffuse model
CS_models_list <- c(
                    "DPP",
                    "KC",
                    "HAU",
                    # "BD",       ## too poor results
                    "ABCG",
                    "RS",
                    "KASTEN",
                    "INEICHEN"
                  )




#'
#' ### Clear Sky detection Algorithm values
#'
#'
#+ include=T, echo=F
# panderOptions('table.emphasize.rownames', F)
panderOptions('table.alignment.default',  "right")
panderOptions('table.alignment.rownames', "right")
panderOptions('table.split.cells',        c(50,10))
pander(t(MS))
pander(alpha_models)
pander(combinations)
#'
#+ include=T, echo=F

#### Model selection ####

#'
#' ### Reference clear sky irradiance model and air mass model (AM)
#'
#' We select the clear sky model, the air mass calculation
#' and the period of `r MS$nt`
#' minutes to use.
#'

AM       <- AM_simple
CS_model <- get(model_selection)
# alpha    = alpha_models["best",model_selection]
#+ include=T, echo=F



#### 6. LDI ###
MS$LDIlim <- 5

#'
#' ### 6. Low Direct Irradiance limit (LDI)
#'
#' Ignore Direct irradiance when it is bellow `r MS$LDIlim` $Watt/m^2$.
#' This limit is biased due to the slight location difference of the two Instruments.
#' The difference in shadow especially the afternoon "pole shadow" of some periods of the year.
#' May limit the implementation by time of day or/and period of the year
#'
#' Also this can not characterize all global measurements due to gaps on direct measurements.
#'
#+ include=T, echo=F


#### 7. LGI ####
MS$VGIlim <- 5

#'
#' ### 7. Low Global Irradiance limit (LGI)
#'
#' Global irradiance below `r MS$VGIlim` level can not be identified
#'
#+ include=T, echo=F

#### 8. FCS ####
FCSlim <- MS$nt

#'
#' ### 8. Too Few CS point for the day (FCS)
#'
#'
#' If in a day there less than `r FCSlim` clear sky points will exclude them from optimizing
#'
#+ include=T, echo=F


#### 9. FDP ####
FDPlim <- MS$nt * 3

#'
#' ### 9. Too Few data points for the day (FDP)
#'
#'
#' If in a day there are less than `r FDPlim` data points will be excluded from optimizing.
#'
#+ include=T, echo=F



#### 11. DsT ####

#'
#' ### 11. Too low Direct signal (DsT)
#'
#' This is a simple hard limit on the lower possible Direct radiation with clear sky.
#'
#' The threshold is `r MS$DIR_s_T_fact`% lower than a reference value.
#'
#' $$ I_d = I_0 * 0.7^{{AM}^{0.678}} * cos({ZSA}) $$
#'
#' Where ${AM}$ is the selected air-mass model.
#'
#' "Clear sky direct normal irradiance estimation based on adjustable inputs and error correction_Zhu2019.pdf"
#'
#+ include=T, echo=F


## create a list of days to use
if (TEST) {
    if ( length(dayslist) < MS$sample_n_days) { MS$sample_n_days = length(dayslist) }
    # dayslist <- sample(dayslist, MS$sample_n_days )
    # dayslist = dayslist[1:10]
    # dayslist = tail(dayslist, n = 20)
    # dayslist <- dayslist[year(dayslist)>=2019]
    # dayslist <- dayslist[year(dayslist)>=2019 &  month(dayslist) == 7]
    dayslist <- dayslist
    # dayslist <- dayslist[dayslist > as.Date("2023-03-18")]
}
dayslist <- sort( dayslist )


#'
#' ### Day range
#'
range(dayslist)
#'
#+ include=T, echo=F


#'
#' ### Baseline value for direct irradiance
#'
#' See: Clear sky direct normal irradiance estimation based on adjustable inputs and error correction_Zhu2019.pdf
strong[ , CS_ref_HOR := ( TSIextEARTH_comb * 0.7 ^ AM(SZA) ^ 0.678 ) * cosde(SZA) ]
#+ include=T, echo=F



# ## For parallel
# library(parallel)
# library(doParallel)
# library(foreach)
# n.cores    <- detectCores() - 1
# my.cluster <- makeCluster(
#     n.cores,
#     type = "FORK"
# )
# registerDoParallel(cl = my.cluster)




#+ include=T, echo=F
##  Iterate all years
for (yyyy in unique(year(dayslist))) {
# foreach(yyyy = unique(year(dayslist))) %dopar% {

    gather       <- data.table()
    daily_stats  <- data.frame()
    subdayslist  <- dayslist[year(dayslist) == yyyy]
    total_points <- sum(year(strong$Date) == yyyy)

    if (!interactive()) {
        if (TEST) {
            pdf(file = paste0(plotsbase, yyyy, "_test.pdf"), onefile = T, width = 10 )
        } else {
            pdf(file = paste0(plotsbase, yyyy, ".pdf"), onefile = T, width = 10 )
        }
    }

    ##  Iterate all days
    for (aa in subdayslist) {
        ## Day variables
        aday  <- as.Date(aa , origin = "1970-01-01")
        sell  <- strong$Day == (aday)
        doy   <- as.numeric(format(aday, "%j"))
        mont  <- as.numeric(format(aday, "%m"))
        nt_hw <- MS$nt %/% 2
        if (interactive()) cat(paste(aday),"\n")

        if (MONTHLY) {
            ## get alpha for month
            alpha      <- monthly_alphas$alpha[monthly_alphas$month == mont]
            MS$M_alpha <- monthly_alphas$alpha
        } else {
            ## get unique alpha for all days
            alpha <- gather_results[gather_results$CS_models == model_selection, "alpha"]
            stopifnot( length(alpha) == 1        )
            stopifnot( typeof(alpha) == "double" )
            MS$alpha <- alpha
        }


        ## empty daily variables
        MeanVIPcnt <- NA
        MaxVIPcnt  <- NA
        VILcnt     <- NA
        VCTcnt     <- NA
        VSMcnt     <- NA

        ## relative clear sky acceptance limit
        relative_CS_lim <- sum(sell) * MS$relative_CS

        ## Data selection for day
        subday     <- strong[ sell, ]
        have_glb   <- !is.na(subday$wattGLB)  ## use vectors!
        have_dir   <- !is.na(subday$wattDIR)  ## use vectors!
        tot_p      <- length(subday$wattGLB)


        ## Values from Clear sky model used
        ## this model is used as clear sky reference
        CS_ref <- CS_model(subday$SZA) * alpha
        ## Values from Air Mass model used
        AM_ref <- AM(subday$SZA)

        ## create a more logical model
        CS_ref_safe <- CS_ref
        CS_ref_safe[ CS_ref_safe < 0 ] <- NA


        ## Create some empty columns
        CS_ref_length <- CS_ref * NA
        GLB_length    <- CS_ref * NA
        GLB_sigma     <- CS_ref * NA
        GLB_Xi        <- CS_ref * NA
        GLB_Xi_test   <- CS_ref * NA

        #### Apply filters ####

        #---- 9. Too Few data point for the day (FDP) --------------------------
        if (FDP_active) {
            Flag_key <- 9
            if (nrow(subday) > 0 & tot_p <= FDPlim ) {
                subday$CSflag <- Flag_key
                subday[[paste0("CSflag_",Flag_key)]] <- TRUE
                cat(paste("Skip day FDP:", aday, tot_p),"\n")
                next
            }
        }



        #---- 1. Mean value of irradiance during the time period (MeanVIP) -----
        if (MeanVIP_active & any(have_glb)) {
            Flag_key <- 1
            ## create running mean of modeled values
            CS_ref_rm_base    <- runmean(x = CS_ref_safe, k = MS$nt, alg = "C")
            # ## first implementation with offset from reference limits
            # CS_ref_rm_VIP_low_0 <- MS$MeanVIP_fct * CS_ref_rm_base - MS$CS_ref_rm_VIP_low
            # CS_ref_rm_VIP_upp_0 <- MS$MeanVIP_fct * CS_ref_rm_base + MS$CS_ref_rm_VIP_upp
            ## newer implementation with relative to reference limits
            CS_ref_rm_VIP_low <- CS_ref_rm_base - CS_ref_rm_base * MS$CS_ref_rm_VIP_RelLow - MS$CS_ref_rm_VIP_LowOff
            CS_ref_rm_VIP_upp <- CS_ref_rm_base + CS_ref_rm_base * MS$CS_ref_rm_VIP_RelUpp + MS$CS_ref_rm_VIP_UppOff

            # plot(CS_ref_rm_VIP_upp, type = "l",col = 1)
            # lines(CS_ref_rm_VIP_low,           col = 2)
            # lines(CS_ref_rm_VIP_low_0,         col = 3)
            # lines(CS_ref_rm_VIP_upp_0,         col = 4)
            # lines(CS_ref_rm_base,              col = 6)

            GLB_rm            <- runmean(x = subday$wattGLB, k = MS$nt, alg = "C")

            ## don't allow negative values on models
            CS_ref_rm_base[    CS_ref_rm_base    < 0 ] <- NA
            CS_ref_rm_VIP_low[ CS_ref_rm_VIP_low < 0 ] <- NA
            CS_ref_rm_VIP_upp[ CS_ref_rm_VIP_upp < 0 ] <- NA
            GLB_rm[            GLB_rm            < 0 ] <- NA

            ## do characterization if pass as clear
            # MeanVIP_pass =  GLB_rm > CS_ref_rm_VIP_low                              ## apply only lower limit (default)
            MeanVIP_pass <- GLB_rm > CS_ref_rm_VIP_low & GLB_rm < CS_ref_rm_VIP_upp   ## apply only lower and upper limit

            indx_todo <- which( have_glb & MeanVIP_pass )
            if (length(indx_todo) > 0) {
                for (i in indx_todo) {
                    walk(i, nt_hw , tot_p )
                    subday$CSflag[w_sta:w_end][ ! subday$CSflag[w_sta:w_end] == 0 ] <- 0
                }
            }
            ## set MeanVIP flag
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 2. Max value of irradiance during the time period (MaxVIP) -------
        if (MaxVIP_active & any(have_glb)) {
            Flag_key            <- 2
            CS_ref_rmax_base    <- runmax(x = CS_ref_safe,    k = MS$nt, alg = "C")
            CS_ref_rmax_VIP_upp <- MS$MaxVIP_fct * CS_ref_rmax_base + MS$MaxVIP_off_upp
            CS_ref_rmax_VIP_low <- MS$MaxVIP_fct * CS_ref_rmax_base - MS$MaxVIP_off_low
            GLB_rmax            <- runmax(x = subday$wattGLB, k = MS$nt, alg = "C")

            ## TEST feature
            CS_ref_rmax_base[    CS_ref_rmax_base    < 0 ] <- NA
            CS_ref_rmax_VIP_upp[ CS_ref_rmax_VIP_upp < 0 ] <- NA
            CS_ref_rmax_VIP_low[ CS_ref_rmax_VIP_low < 0 ] <- NA
            GLB_rmax[            GLB_rmax            < 0 ] <- NA

            ##  pass as clear
            # MaxVIP_pass = GLB_rmax < CS_ref_rmax_VIP_upp                                 ## apply only upper filter (default)
            MaxVIP_pass <- GLB_rmax < CS_ref_rmax_VIP_upp & GLB_rmax > CS_ref_rmax_VIP_low  ## apply upper and lower filter

            indx_todo   <- which( have_glb & MaxVIP_pass )
            if ( length(indx_todo) > 0 ) {
                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99

                for (i in indx_todo ) {
                    walk(i, nt_hw , tot_p )
                    subday$CSflag[w_sta:w_end][ (!subday$CSflag[w_sta:w_end] == 0 ) &
                                                ( subday$CSflag[w_sta:w_end] == 99) ] <- 0
                } ##END for loop all time periods
            }
            ## set MaxVIP flag
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 3. Variability in irradiance by the length (VIL) -----------------
        if (VIL_active) {
            Flag_key  <- 3
            indx_todo <- which(have_glb)
            if (length(indx_todo) > 0) {
                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99

                for (i in 1:tot_p) {
                    ## resolve window by index
                    walk(i, nt_hw , tot_p )

                    ## Do calculation ##
                    data_win_glb   <- subday$wattGLB[w_sta:w_end]
                    # date_win_ref   = CS_ref[w_sta:w_end]
                    date_win_ref   <- CS_ref_safe[w_sta:w_end]
                    # print(c(length(data_win_glb),w_sta,i,w_end))

                    ## calculate length for data
                    DeltaVSq_GLB  <- (data.table::shift(data_win_glb) - data_win_glb)**2
                    DeltaTSq_GLB  <- as.numeric( !is.na(DeltaVSq_GLB) )**2
                    GLB_length[i] <- sum(sqrt(DeltaVSq_GLB + DeltaTSq_GLB), na.rm = TRUE)

                    ## calculate length for reference
                    DeltaVSq_ref     <- (data.table::shift(date_win_ref) - date_win_ref)**2
                    DeltaTSq_ref     <- as.numeric( !is.na(DeltaVSq_ref) )**2
                    CS_ref_length[i] <- sum(sqrt(DeltaVSq_ref + DeltaTSq_ref), na.rm = TRUE)

                    # ## store comparison values ?
                    # subday$VIL_glb[w_sta] <- CS_ref_length[i]
                    # subday$VIL_upp[w_sta] <- MS$MaxVIL_fct * CS_ref_length[i] + MS$offVIL_upl
                    # subday$VIL_low[w_sta] <- MS$MinVIL_fct * CS_ref_length[i] - MS$offVIL_dwl

                    ## pass test as clear
                    pass <- GLB_length[i] < (MS$MaxVIL_fct * CS_ref_length[i] + MS$offVIL_upl) &
                            GLB_length[i] > (MS$MinVIL_fct * CS_ref_length[i] - MS$offVIL_dwl)
                    if (is.na(pass)) pass <- FALSE

                    ## set VIL flag
                    subday$CSflag[w_sta:w_end][ (!subday$CSflag[w_sta:w_end] == 0)  &
                                                ( subday$CSflag[w_sta:w_end] == 99) &
                                                  pass                                ] <- 0
                } ##END for loop all points
                ## store comparison values ?
                subday$VIL_GLB   <- GLB_length
                subday$VIL_upp   <- MS$MaxVIL_fct * CS_ref_length + MS$offVIL_upl
                subday$VIL_low   <- MS$MinVIL_fct * CS_ref_length - MS$offVIL_dwl
            }

            #### if it is not clear is VIL
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 4. Variance of Changes in the Time series (VCT) ------------------
        if (VCT_active) {
            Flag_key  <- 4
            indx_todo <- which(have_glb)
            if (length(indx_todo) > 0) {
                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99
                s_i <- data.table::shift(subday$wattGLB) - subday$wattGLB ##  /(t_i+i  - t_i)

                for (i in indx_todo) {
                    ## resolve window by index
                    walk(i, nt_hw , tot_p )

                    ## Do calculation ##
                    data_win_glb <- subday$wattGLB[w_sta:w_end]
                    DeltaVSq_GLB <- (data.table::shift(data_win_glb) - data_win_glb) ##  /(t_i+i  - t_i)
                    s_bar        <- sum(DeltaVSq_GLB, na.rm = T) / ( MS$nt - 1 )

                    GLB_sigma[i] <- sqrt( sum( (s_i[w_sta:w_end] - s_bar)**2 , na.rm = TRUE ) / ( MS$nt - 1 ) ) /
                                    sum( data_win_glb , na.rm = T) / MS$nt

                    ## pass test as clear
                    pass <- GLB_sigma[i] < MS$offVCT
                    if (is.na(pass)) { pass <- FALSE }

                    ## set VCT flag
                    subday$CSflag[w_sta:w_end][ ( ! subday$CSflag[w_sta:w_end] == 0)  &
                                                (   subday$CSflag[w_sta:w_end] == 99) &
                                                    pass                                ] <- 0
                } ##END for loop all points
            }
            #### if it is not clear is VCT
            ## set newer flag
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            ## set old flag
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 5. Variability in the Shape of the irradiance Measurements (VSM) ----
        if (VSM_active) {
            Flag_key  <- 5
            indx_todo <- which(have_glb)
            if ( length(indx_todo) > 0 ) {
                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99
                x_i_CS <- data.table::shift(CS_ref) - CS_ref

                for (i in indx_todo) {
                    ## resolve window by index
                    walk(i, nt_hw, tot_p)

                    ## change in measurements
                    data_win_glb  <- subday$wattGLB[w_sta:w_end]
                    DeltaVSq_GLB  <- (data.table::shift(data_win_glb) - data_win_glb)

                    ## change in reference
                    data_win_ref  <- subday$CS_ref[w_sta:w_end]
                    DeltaVSq_ref  <- (data.table::shift(data_win_ref) - data_win_ref)

                    suppressWarnings({
                        # GLB_Xi[i]      <- max(abs(DeltaVSq_GLB - x_i_CS[i]   ), na.rm = TRUE)
                        GLB_Xi[i] <- max(abs(DeltaVSq_GLB - DeltaVSq_ref), na.rm = TRUE)
                    })

                    ## pass test as clear
                    # pass      <- GLB_Xi[i]      < MS$offVSM
                    pass <- GLB_Xi[i] < MS$offVSM

                    if (is.na(pass)) pass <- FALSE

                    ## an info warning
                    if (is.infinite(GLB_Xi[i]) & i > 1) {
                        if (interactive()) print(paste(GLB_Xi[i], format(aday, "%F"), i, pass))
                    }

                    ## set VCT flag
                    subday$CSflag[w_sta:w_end][ ( !subday$CSflag[w_sta:w_end] == 0)  &
                                                (  subday$CSflag[w_sta:w_end] == 99) &
                                                   pass                                ] <- 0

                } ##END for loop all points
            }
            #### if it is not clear is VCT
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 6. Low Direct Irradiance limit (LDI) -----------------------------
        if (LDI_active) {
            Flag_key  <- 6
            ## low direct and not "Possible Direct Obstruction (23)"
            ## probably we know sun was obscured
            subday[CSflag == 0 &
                   wattHOR < MS$LDIlim &
                   QCF_DIR != "Possible Direct Obstruction (23)",
                   CSflag := Flag_key ]

            subday[CSflag == 99 &
                   wattHOR < MS$LDIlim &
                   QCF_DIR != "Possible Direct Obstruction (23)",
                   CSflag := Flag_key ]

            subday[wattHOR < MS$LDIlim &
                   QCF_DIR != "Possible Direct Obstruction (23)",
                   paste0("CSflag_", Flag_key) := TRUE ]

            subday[wattHOR < MS$LDIlim &
                   QCF_DIR != "Possible Direct Obstruction (23)",
                   paste0("CSflag_", Flag_key) := TRUE ]
        }


        #---- 7. Low Global Irradiance limit (LGI) -----------------------------
        if (LGI_active) {
            Flag_key  <- 7
            subday[ (CSflag == 0 ) & (wattGLB < MS$VGIlim), CSflag := Flag_key ]
            subday[ (CSflag == 99) & (wattGLB < MS$VGIlim), CSflag := Flag_key ]

            subday[ wattGLB < MS$VGIlim, paste0("CSflag_", Flag_key) := TRUE ]
            subday[ wattGLB < MS$VGIlim, paste0("CSflag_", Flag_key) := TRUE ]
        }



        #---- 11. Too low direct radiation (DsT) -----------------````----------
        if (DST_active & any(have_dir)) {
            Flag_key  <- 11
            subday[(CSflag == 0 ) & (wattHOR < CS_ref_HOR * ( 1 - (MS$DIR_s_T_fact / 100))),
                   CSflag := Flag_key]
            subday[(CSflag == 99) & (wattHOR < CS_ref_HOR * ( 1 - (MS$DIR_s_T_fact / 100))),
                   CSflag := Flag_key]

            subday[wattHOR < CS_ref_HOR * ( 1 - (MS$DIR_s_T_fact / 100)), paste0("CSflag_", Flag_key) := TRUE ]
        }




        #---- 8. Too Few CS point for the day (FCS) ----------------------------
        if (FCS_active) {
            Flag_key  <- 8
            ## check all other CS flags to set this
            wecare <- grep("CSflag_", names(subday), value = T)
            acount <- nrow(subday[ rowSums( subday[, ..wecare], na.rm = T) == 0, ])
            if ( acount < FCSlim ) {
                subday[ subday$CSflag == 0 , CSflag := Flag_key]
                subday[ subday$CSflag == 99, CSflag := Flag_key]
                subday[, paste0("CSflag_", Flag_key) := TRUE]
            }
        }



        ## rest is clear sky (we hope)
        subday$CSflag[subday$CSflag == 99] <- 0

        ## Evaluation of CS detection
        clear_sky <- subday$CSflag == 0

        ##; ## skip non clear days
        ##; if (sum(clear_sky, na.rm = T) < relative_CS_lim)  {
        ##;     print(paste("Skip Day minutes CS/lim/total  ", sum(clear_sky, na.rm = T),"/",relative_CS_lim,"/", sum(sell)))
        ##;     next  ##  skip the rest of the loop and avoid alpha optimization
        ##; }

        ## Clear sky statistics  -----
        RMSE_r <- rmse_vec(CS_ref_safe[clear_sky], subday$wattGLB[clear_sky], na_rm = T)

        MBE  <- mean(CS_ref_safe[clear_sky] - subday$wattGLB[clear_sky], na.rm = T) /
                mean(subday$wattGLB[clear_sky], na.rm = T)

        cost <- sum( ( subday$wattGLB[clear_sky] - CS_ref_safe[clear_sky] )**2 , na.rm = T) /
                sum(clear_sky, na.rm = T)


        ## ID statistics
        MeanVIPcnt <- sum(subday$CSflag == 1, na.rm = T)
        MaxVIPcnt  <- sum(subday$CSflag == 2, na.rm = T)
        VILcnt     <- sum(subday$CSflag == 3, na.rm = T)
        VCTcnt     <- sum(subday$CSflag == 4, na.rm = T)
        VSMcnt     <- sum(subday$CSflag == 5, na.rm = T)



        #### PLOTS ####
        ## _  Main plot --------------------------------------------------------
        ylim <- range( c(subday$wattGLB, MS$MaxVIP_fct * CS_ref_safe), na.rm = T )
        if ( ylim[2] > 1500 ) ylim[2] = 1500
        # if ( ylim[2] > 400) ylim[2] = 400
        ylim[1] = -25


        par("mar" = c(2, 4.2, 1.5, 1) )
        # par(mar = c(0, 0, 0, 0), oma = c(2, 4.2, 2, 1))
        # par(mfrow = c(2,1))
        # layout(matrix(c(1,1,1,2,3), nrow = 5, ncol = 1, byrow = TRUE))
        layout(matrix(c(1), nrow = 1, ncol = 1, byrow = TRUE))

        ## plot global measurements
        plot(subday$Date, subday$wattGLB, 'l', col = "green" ,
             ylim = ylim, ylab = "Watt/m^2", xlab = "", xaxt = "n", lwd = 2 )
        abline( v = axis.POSIXct(1, at = pretty(subday$Date, n=24, min.n=24) ),col = "lightgray", lty = "dotted", lwd = .5)
        ## plot direct measurements
        lines(subday$Date, subday$wattHOR, "l", col = scales::alpha("blue", 0.8 ), lty = 1, lwd = 2 )
        # lines(subday$Date, subday$wattDIR, "l", col = scales::alpha("blue", 0.8 ), lty = 2, lwd = 2 )


        ## plot expected global reference line
        lines(subday$Date, CS_ref_safe, lty = 3, col = "green", lwd = 2 )

        ## direct irradiance test value !!
        lines(subday$Date, subday$CS_ref_HOR, lty = 3, col = "red", lwd = 2 )
        lines(subday$Date, subday$CS_ref_HOR * ( 1 - ( MS$DIR_s_T_fact / 100 )), "l", lty = 3, col = "red", lwd = 2 )

        if (MeanVIP_active & any(have_glb)) {
            lines(subday$Date, CS_ref_rm_VIP_low,   col = kcols[1], lty = 2, lwd = 1.5)
            lines(subday$Date, CS_ref_rm_VIP_upp,   col = kcols[1], lty = 2, lwd = 1.5)
        }

        if (MaxVIP_active & any(have_glb)) {
            lines(subday$Date, CS_ref_rmax_VIP_upp, col = kcols[2], lty = 3, lwd = 1.5)
            lines(subday$Date, CS_ref_rmax_VIP_low, col = kcols[2], lty = 3, lwd = 1.5)
        }

        abline( h = MS$VGIlim, lty = 2 , col = kcols[7])



        # _ Main plot ID points ------------------------------------------------

        ## 1. mean value of irradiance during the time period
        ddd = subday$Date[    subday$CSflag == 1 ]
        vvv = subday$wattGLB[ subday$CSflag == 1 ]
        points(ddd, vvv,            pch = 8, col = kcols[1], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[1], cex = .2)

        ## 2. Max value of irradiance during the time period
        ddd = subday$Date[    subday$CSflag == 2 ]
        vvv = subday$wattGLB[ subday$CSflag == 2 ]
        points(ddd, vvv,            pch = 8, col = kcols[2], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[2], cex = .2)

        ##  3. Variability in irradiance by the length (VIL)
        ddd <- subday$Date[    subday$CSflag == 3 ]
        vvv <- subday$wattGLB[ subday$CSflag == 3 ]
        points(ddd, vvv,            pch = 8, col = kcols[3], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[3], cex = .2)

        ##  4.  Variance of Changes in the Time series (VCT)
        ddd <- subday$Date[    subday$CSflag == 4 ]
        vvv <- subday$wattGLB[ subday$CSflag == 4 ]
        points(ddd, vvv,            pch = 8, col = kcols[4], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[4], cex = .2)

        ##  5.  Variability in the Shape of the irradiance Measurements (VSM)
        ddd <- subday$Date[    subday$CSflag == 5 ]
        vvv <- subday$wattGLB[ subday$CSflag == 5 ]
        points(ddd, vvv,            pch = 8, col = kcols[5], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[5], cex = .2)

        ##  6. Low Direct Irradiance limit (LDI)
        ddd    <- subday$Date[    subday$CSflag == 6 ]
        vvv    <- subday$wattGLB[ subday$CSflag == 6 ]
        LDIcnt <- sum(subday$CSflag == 6, na.rm = T)
        points(ddd, vvv,            pch = 8, col = kcols[6], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[6], cex = .2)


        ##  7. Low Global Irradiance limit (LGI)
        ddd    <- subday$Date[    subday$CSflag == 7 ]
        vvv    <- subday$wattGLB[ subday$CSflag == 7 ]
        VGLcnt <- sum(subday$CSflag == 7, na.rm = T)
        points(ddd, vvv,            pch = 8, col = kcols[7], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[7], cex = .2)


        ##  11. Too low direct radiation (DsT)
        ddd    <- subday$Date[    subday$CSflag_11 ]
        vvv    <- subday$wattHOR[ subday$CSflag_11 ]
        DsTcnt <- sum(subday$CSflag_11, na.rm = T)
        points(ddd, vvv,            pch = 8, col = kcols[11], cex = .4)
        points(ddd, vvv - vvv - 15, pch = 8, col = kcols[11], cex = .2)



        wecare    <- grep("CSflag_", names(subday), value = T)
        sel       <- rowSums( subday[, ..wecare ], na.rm = T ) == 0
        Clear_cnt <- sum(            sel, na.rm = T)

        ##  the rest is clear
        if (any(have_glb)) {
            ddd       <- subday$Date[    sel ]
            vvv       <- subday$wattGLB[ sel ]
            points(ddd, vvv - vvv - 25, pch = 8, col = "green", cex = .2)
        }
        if (any(have_dir)) {
            ddd       <- subday$Date[    sel ]
            vvv       <- subday$wattHOR[ sel ]
            points(ddd, vvv - vvv - 25, pch = 8, col = "green", cex = .2)
        }


        title(main = paste(aday,    format(aday, "%j"),
                           "RMSE:", round(RMSE_r, 3),
                           "MBE:",  round(MBE,  3) ), cex.main = 1)

        legend("topright", bty = "n",
               legend = c( paste("CS GLB: ", Clear_cnt),
                           paste("MeanVIP:", MeanVIPcnt),
                           paste("MaxVIP: ", MaxVIPcnt),
                           paste("VIL:    ", VILcnt),
                           paste("VCT:    ", VCTcnt),
                           paste("VSM:    ", VSMcnt),
                           paste("LDI:    ", LDIcnt),
                           paste("VGIlim: ", VGLcnt),
                           paste("DsT:    ", DsTcnt)   ),
               col    = c("green",
                          kcols[1],
                          kcols[2],
                          kcols[3],
                          kcols[4],
                          kcols[5],
                          kcols[6],
                          kcols[7],
                          kcols[11]),
               pch    = c(8) ,cex = 1.1)



        ## _ Filter Plots ------------------------------------------------------
        layou_n <- sum(MeanVIP_active, MaxVIP_active, VIL_active, VCT_active, VSM_active)
        layout(matrix(c(1,2,3,4,5), nrow = layou_n, ncol = 1, byrow = TRUE))

        par("mar" = c(.5, 4.2, .5, 1) )

        ## __ 1. Mean value of irradiance during the time period ---------------
        if (MeanVIP_active & any(have_glb)){
            par("mar" = c(0, 4.2, .5, 1) )

            # ## Old style offset thresholds
            # ylim <- range(c( - MS$CS_ref_rm_VIP_low * 1.7, MS$CS_ref_rm_VIP_upp * 1.7 ), na.rm = T)
            # plot( subday$Date, GLB_rm - CS_ref_rm_base , pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "Run. Mean VIP (1)")
            # abline( h = - MS$CS_ref_rm_VIP_low, lty = 2, col = kcols[1], lwd = 2)
            # abline( h =   MS$CS_ref_rm_VIP_upp, lty = 3, col = kcols[1], lwd = 2)
            # abline( h = 0 , lty = 1, col = kcols[1], lwd = 2)
            # text(x = subday$Date[20], y = - MS$CS_ref_rm_VIP_low, labels = - MS$CS_ref_rm_VIP_low, pos = 1)
            # text(x = subday$Date[20], y =   MS$CS_ref_rm_VIP_upp, labels =   MS$CS_ref_rm_VIP_upp, pos = 1)

            ## new style relative thresholds
            ylim <- range(GLB_rm - CS_ref_rm_base,
                          CS_ref_rm_VIP_low - CS_ref_rm_base,
                          CS_ref_rm_VIP_upp - CS_ref_rm_base,na.rm = T)
            plot(subday$Date, GLB_rm - CS_ref_rm_base,
                 pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "Run. Mean VIP (1)")
            lines(subday$Date, CS_ref_rm_VIP_low - CS_ref_rm_base, col = kcols[1], lty = 2)
            lines(subday$Date, CS_ref_rm_VIP_upp - CS_ref_rm_base, col = kcols[1], lty = 2)

        }

        ## __ 2. Max value of irradiance during the time period ----------------
        if (MaxVIP_active & any(have_glb)) {
            par("mar" = c(0, 4.2, 0, 1) )
            ylim <- range(c( MS$MaxVIP_off_upp * 1.7, -MS$MaxVIP_off_low * 1.7  ), na.rm = T)
            plot(  subday$Date, GLB_rmax - CS_ref_rmax_base , pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "Run. Max VIP (2)")
            abline( h =   MS$MaxVIP_off_upp, lty = 2, col = kcols[2], lwd = 2)
            abline( h = - MS$MaxVIP_off_low, lty = 3, col = kcols[2], lwd = 2)
            abline( h = 0 , lty = 1, col = kcols[2], lwd = 2)
            text(x = subday$Date[20], y = -MS$MaxVIP_off_low, labels = -MS$MaxVIP_off_low, pos = 1)
            text(x = subday$Date[20], y =  MS$MaxVIP_off_upp, labels =  MS$MaxVIP_off_upp, pos = 1)
        }

        ## __ 3. Variability in irradiance by the length (VIL) -----------------
        if (VIL_active) {
            par("mar" = c(0, 4.2, 0, 1) )
            ## old plots
            # ylim <- range(c( MS$offVIL_upl * 1.7, - MS$offVIL_dwl * 1.7), na.rm = T )
            # plot( subday$Date,  GLB_length - CS_ref_length, pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "VIL (3)")
            #
            # abline( h = - MS$offVIL_dwl, lty = 2, col = kcols[3], lwd = 2)
            # abline( h =   MS$offVIL_upl, lty = 3, col = kcols[3], lwd = 2)
            # abline( h = 0 , lty = 1, col = kcols[3], lwd = 2)
            # text(x = subday$Date[20], y = - MS$offVIL_dwl, labels = - MS$offVIL_dwl, pos = 1)
            # text(x = subday$Date[20], y =   MS$offVIL_upl, labels =   MS$offVIL_upl, pos = 1)

            ## new plots
            if (any(grepl("VIL_", names(subday)))) {
                ylim <- range(subday[, .(VIL_low, VIL_upp)], na.rm = T)
                ylim <- c(- abs(max(ylim)), 2 * abs(max(ylim)))

                plot( subday$Date, subday$VIL_GLB, pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "VIL (3)" )
                lines(subday$Date, subday$VIL_upp, lty = 3, col = kcols[3], lwd = 2)
                lines(subday$Date, subday$VIL_low, lty = 2, col = kcols[3], lwd = 2)
            } else {
                plot.new()
            }
        }

        if (VCT_active) {
            par("mar" = c(0, 4.2, 0, 1) )
            ylim <- range(c(0, MS$offVCT*5), na.rm = T)
            plot(subday$Date, GLB_sigma, pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "VCT (4)")
            abline(h = MS$offVCT, lty = 2, col = kcols[4] , lwd = 2)
            text(x = subday$Date[20], y = MS$offVCT, labels = MS$offVCT, pos = 1)
        }

        if (VSM_active) {
            par("mar" = c(.5, 4.2, 0, 1) )
            ylim <- range(c(0, MS$offVSM*5), na.rm = T)
            plot(subday$Date, GLB_Xi, pch = 18, cex = .8, col = "green", ylim = ylim, ylab = "VSM (5)")
            abline(h = MS$offVSM, lty = 2, col = kcols[5] , lwd = 2)
            text(x = subday$Date[20], y = MS$offVSM, labels = MS$offVSM, pos = 1)
        }



        ## _ Keep Clear Sky values  --------------------------------------------
        subday$CS_ref         <- CS_ref_safe
        gather <- rbind(gather, subday, fill = TRUE)

        strong$CSflag[sell]   <- subday$CSflag  ## old legacy flag?
        strong$CS_ref[sell]   <- CS_ref_safe


        ## _ Keep daily statistics  --------------------------------------------
        keepday <- data.frame(
            Date       = aday,
            RMSE       = RMSE_r,
            MBE        = MBE,
            cost       = cost,
            MeanVIPcnt = MeanVIPcnt,
            MaxVIPcnt  = MaxVIPcnt,
            VILcnt     = VILcnt,
            VCTcnt     = VCTcnt,
            VSMcnt     = VSMcnt
        )
        daily_stats <- rbind(daily_stats, keepday )

    } ##END day loop

    dev.off()

    ### create a yearly export
    export <- unique(strong[ year(Date) == yyyy ])
    export$CSflag[export$CSflag == 99] <- NA

    gather <- unique(gather)

    export <- merge(gather, export, all = T)


    test <- strong[ year(Date) == yyyy ]

    length(unique(test$Date))
    length(unique(gather$Date))

    ttt <- test[   Date %in% test$Date[duplicated(test$Date)]    ]
    ggg <- gather[ Date %in% gather$Date[duplicated(gather$Date)]]

    sum( !gather$Date %in% test$Date )

    stopifnot(nrow(export) == total_points)

    ## TODO check unmarked records

    as.Date(setdiff(unique(strong$Day),unique(export$Day)), origin = "1970-01-01")

    any(duplicated(unique(export$Day),
            unique(strong$Day)))

    table(export$CSflag)


    ## store data
    suppressWarnings({
        # sub("\\.R$", "", basename(Script.Name)), "_")
        write_RDS(object = export,
                            file = paste0("/home/athan/DATA/Broad_Band/CS_id/",
                                          sub("\\.R$", "", basename(Script.Name)), "_", yyyy)
        )

        write_RDS(object = daily_stats,
                            file = paste0("/home/athan/DATA/Broad_Band/CS_id/Daily_stats_",
                                          sub("\\.R$", "", basename(Script.Name)), "_", yyyy)
        )

    })

} ##END year loop



par(def.par)  # reset to default
layout(matrix(c(1), nrow = 1, ncol = 1, byrow = TRUE))

#'
#' | CS Flag | Test |
#' |:-------:|:--------------------------------------------------------------|
#' |   NA    | Undefined, untested                                           |
#' |    0    | Passed as clear sky                                           |
#' |    1    | Mean value of irradiance during the time period (MeanVIP)     |
#' |    2    | Max Value of Irradiance during the time Period (MaxVIP)       |
#' |    3    | Variability in irradiance by the length (VIL)                 |
#' |    4    | Variance of Changes in the Time series (VCT)                  |
#' |    5    | Variability in the Shape of the irradiance Measurements (VSM) |
#' |    6    | Low Direct Irradiance limit (LDI)                             |
#' |    7    | Low Global Irradiance limit (LGI)                             |
#' |    8    | Too Few CS point for the day (FCS)                            |
#' |    9    | Too Few data points for the day                               |
#' |   10    | Missing Data                                                  |
#' |   11    | Direct irradiance simple threshold                            |
#'



#+ include=T, echo=FALSE
pander(summary(strong$CSflag))

pander(print(table(strong$CSflag)))
#'


#+ include=T, echo=FALSE
par("mar" = c(3, 3, 2, 1) )

barplot( table(strong$CSflag),
         main = "Flag Count")

barplot(prop.table(table(strong$CSflag)),
        main = "Flag Frequency" )
#'


## Evaluation of CS detection
clear_sky <- strong$CSflag == 0

RMSE <- sqrt(mean( ( strong$CS_ref[clear_sky] - strong$wattGLB[clear_sky] )**2 , na.rm = T) ) /
        mean(strong$wattGLB[clear_sky] , na.rm = T)

MBE  <- mean(strong$CS_ref[clear_sky] - strong$wattGLB[clear_sky]  , na.rm = T) /
        mean(strong$wattGLB[clear_sky] , na.rm = T)


#'
#' ### Cost function for optimization of alpha value.
#'
#' $$ f(a) = \dfrac{ \sum_{i=1}^{n} ( a \cdot {GHI}_i - {CSI}_i )^2 }
#'                 { n } , \qquad a > 0 $$
#'
cost <- sum((( alpha * strong$wattGLB[clear_sky] ) - strong$CS_ref[clear_sky] )**2 , na.rm = T) /
        sum(clear_sky, na.rm = T)



# plot(daily_stats$Date, daily_stats$RMSE)
# abline(h = RMSE)
# plot(daily_stats$Date, daily_stats$MBE)
# abline(h = MBE)

# hist(daily_stats$RMSE)
# abline(v = RMSE)
# hist(daily_stats$MBE)
# abline(v = MBE)

# library(tdr)
# tdStats(strong$[clear_sky], strong$wattGLB[clear_sky])
# targetDiagram(tdStats(strong$CS_ref[clear_sky], strong$wattGLB[clear_sky]))
# targetDiagram(tafff)
# text(tafff$cvrmse, tafff$mbe, labels=row.names(tafff), cex= 0.7, offset = 0)
# plot(tafff$cvrmse, tafff$mbe)
# text(tafff$cvrmse, tafff$mbe, labels=row.names(tafff), cex= 0.7, pos = 1)
# plot( subset(tafff, select = -c(r2,tStone,mbe,nmbe,sdo,sdm,mo,mm,mae) ) )
# plot( subset(tafff, select = c(mbe,mae,rmse) ) )
# plot(tafff$rmse, tafff$mbe, xlab = "rmse", ylab = "mbe")
# text(tafff$rmse, tafff$mbe, labels=row.names(tafff), cex= 0.7, pos = 1)
# plot(tafff$cvrmse, tafff$cvmbe, xlab = "rmse", ylab = "mbe")
# plot(tafff$nrmse, tafff$nmbe, xlab = "rmse", ylab = "mbe")


mean(daily_stats$RMSE, na.rm = T)







#+ include=T, echo=FALSE

## complete days
daterange <- range(strong$Date)
all_dates <- data.frame(Day = seq(as.Date(daterange[1]),as.Date(daterange[2]),by="day"))
complete  <- base::merge(all_dates, strong, by.y = "Day", all.x = T, all.y = T )


layout(matrix(c(1,2), nrow = 2, ncol = 1, byrow = TRUE))
par("mar" = c(2, 3, 2, 0) )





for (yyyy in min(year(complete$Day)):max(year(complete$Day))) {
    yyyyselect <- format(complete$Day, "%Y") == paste(yyyy)
    counts     <- table(complete$CSflag[yyyyselect], complete$Day[yyyyselect] )
    barplot(counts, space = 0, col = c("green",kcols), border = NA)
}


for (yyyy in min(year(complete$Day)):max(year(complete$Day))) {
    yyyyselect <- format(complete$Day, "%Y") == paste(yyyy)
    counts <- table(complete$CSflag[yyyyselect], complete$Day[yyyyselect] )
    propt  <- prop.table(counts, margin = 2 )
    barplot(propt, space = 0, col = c("green",kcols), border = NA)
}
#'



# load("~/Aerosols/DATA/model_opt/Combinations_results_2017-05-25_194849.Rds")
# gather_results <- data.frame()
# for (fmm in unique( combinations_results$CS_models )) {
#     sell = (combinations_results$CS_models == fmm)
#     temp = combinations_results[sell,]
#     gather_results <- rbind(gather_results, temp[which(temp$cost == min(temp$cost, na.rm = T)), ][1,])
# }
# plot(gather_results$rmse, gather_results$CS_count)
# text(gather_results$rmse, gather_results$CS_count, labels = gather_results$CS_models, cex = .6,pos = 4)

## store params
write_RDS(MS, paste0("~/CS_id/PARAMS/", basename(sub("\\.R$","", Script.Name))))

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
# if (interactive()) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", basename(Script.Name), " 'R script ended'"))
# }
