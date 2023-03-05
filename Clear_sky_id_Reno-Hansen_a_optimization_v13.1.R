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
#'
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
#  data as data.tables may be problematic due to new syntax etc.
#

## TODO use data.table
## TODO check and apply for older dates


####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n")
                            return("Clear_sky_id_application") })
if(!interactive()) {
    pdf(  file = paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}

options("width" = 130)


library(RAerosols)
library(RColorBrewer)
library(scales)
library(pander)
library(caTools)
library(data.table)

##
##                            READ ME
##
##  This runs CS id and do an optimization for the a coefficient of each
##  diffuse model. The output of this procedure should be used to setup
##  the actual Clear-Sky identification algorithm. Which run as a separate
##  script. Take care to check if the set up variables are matching, between
##  the scripts/runs.
##


def.par <- par(no.readonly = TRUE) # save default, for resetting...

kcols <- brewer.pal(11, "Set3")

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
# TEST        <- TRUE
TEST        <- FALSE
SAMPLE_DAYS <- 3000000  ## The total number of days to sample from data
START_DAY   <- "2016-01-01"
# START_DAY   <- "2022-01-01"

## output files to used as input for CS detection
if (MONTHLY) {
    model_results_file = "/home/athan/Aerosols/DATA/model_opt/Combinations_results_m_"
} else {
    model_results_file = "/home/athan/Aerosols/DATA/model_opt/Combinations_results_"
}



par(mar = c(2,4,2,1))



#+ include=T, echo=F
source("/home/athan/Aerosols/source_R/THEORY/Extraterrestrial_radiation_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Air_mass_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Clear_sky_irradiance_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Linke_turbidity_models.R")
#'



#'
#' \newpage
#'
#' ## DETECTION OF CLEAR PERIODS IN GHI MEASUREMENTS ##
#'

#+ include=T, echo=FALSE


####   Load data from QCRad procedure   ########################################
## The "strict" input files were used before
strict_files <- list.files(path       = "/home/athan/DATA/Broad_Band/",
                           pattern    = "LAP_QCRad_CM21_CHP1_[0-9]{4}.Rds",
                           full.names = T ,
                           recursive  = F)
strict_files <- sort(strict_files)
## build one data.frame
strict <- data.table()
for (in_f in strict_files) {
    stctemp <- data.table(readRDS(in_f))

    ##TODO we have setup filters with the assumption of good.
    ## have to check the logic of other filters
    allow   <- c( "good", "Possible Direct Obstruction (23)")
    stctemp <- stctemp[ QCF_DIR %in% allow | QCF_GLB %in% allow ]
    strict  <- rbind(strict, stctemp)
    rm(stctemp)
}
## fill the names of dataframes
## FIXME some duplicates row exist in the database!!!
strong <- unique(strict)
rm(strict)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# --- Limit date span of the data ----------------------------------------------
cat(paste("Use data after date:", START_DAY), "\n\n")
strong[, Day := as.Date(Date)]
strong <- strong[ strong$Day >= as.Date(START_DAY), ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## days to parse
dayslist      <- unique( strong$Day )

## add id column
strong$CSflag <- 99

inverted      <- strong$wattGLB < strong$wattHOR


#' #### Data set input. ####
#' First day: `r min(dayslist)`
#'
#' Last day: `r max(dayslist)`
#'
#' Days available: `r length(dayslist)`
#'


#### Exclude inversions ####

#' ### There are instances where global irradiance is less than direct. ###
#' This happens for `r sum(inverted, na.rm = T)`
#' minutes (records), near sunset and sunrise due to obstacles.
#' We will exclude all this data both for global and direct.
#'

#+ include=T, echo=FALSE
strong$wattGLB[     inverted] <- NA
strong$wattGLB_sds[ inverted] <- NA
strong$wattHOR[     inverted] <- NA
strong$wattHOR_sds[ inverted] <- NA

## set id flag for missing data
# strong$CSflag[ is.na(strong$wattGLB) ] <- 10

#####################################################################


#### Model status ####
MeanVIP_active <- F
MaxVIP_active  <- F
VIL_active     <- F
VCT_active     <- F
VSM_active     <- F
LDI_active     <- F
LGI_active     <- F
FCS_active     <- F
FDP_active     <- F
FAST_SKIP      <- F

## Reno-Hansen filters
MeanVIP_active <- T
MaxVIP_active  <- T
VIL_active     <- T
VCT_active     <- T
VSM_active     <- T

## My filters
LDI_active     <- T   ## Low __Direct__ Irradiance limit (LDI)
                      ## careful this also excludes points due to pole shade at
                      ## afternoon and building in the morning
LGI_active     <- T   ## Low Global Irradiance limit (LGI)
                      ## Global irradiance below this level can not be identified
FCS_active     <- T   ## Skip with few cs
FDP_active     <- T   ## Skip with few data in a day
FAST_SKIP      <- T   ## allow faster skip of filters also reduce data kept




# -- all the variables for this run are initialized here --------------------- #

MIN_Elevation     <- 5                             ##  Minimum sun elevation for optimization input data

MS <- data.frame( nt                = 11,          ##     Window size prefer odd numbers so not to be left right bias
                  MeanVIP_fct       = 1,           ##  1. Factor Mean Value of Irradiance during the time Period (MeanVIP)
                  CS_ref_rm_VIP_low = 70,          ##  1. MIN Offset Mean Value of Irradiance during the time Period (MeanVIP)
                  CS_ref_rm_VIP_upp = 80,          ##  1. MAX Offset Mean Value of Irradiance during the time Period (MeanVIP)
                  MaxVIP_fct        = 1 ,          ##  2. Factor Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIP_off_upp    = 70,          ##  2. MAX Offset Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIP_off_low    = 75,          ##  2. MIN Offset Max Value of Irradiance during the time Period (MaxVIP)
                  MaxVIL_fct        = 1,           ##  3. MAX Factor Variability in irradiance by the length (VIL)
                  MinVIL_fct        = 1,           ##  3. MIN Factor Variability in irradiance by the length (VIL)
                  offVIL_upl        = 9,           ##  3. MAX Offset Variability in irradiance by the length (VIL)
                  offVIL_dwl        = 5,           ##  3. MIN Offset Variability in irradiance by the length (VIL)
                  offVCT            = 0.00011,     ##  4. Variance of Changes in the Time series (VCT)
                  offVSM            = 7.5,         ##  5. Variability in the Shape of the irradiance Measurements (VSM)
                  alpha             = 1,
                  relative_CS       = 0.3,         ## limit of CS day acceptance 0.3: 30% of the data points have to be id as clear (bias for overall clear days)
                  sample_n_days     = SAMPLE_DAYS, ## Sample of that much days to be evaluated (lower for testing)
                  tolerance         = 0.01,        ## Optimization algorithm tolerance ~0.01 (higher for testing)
                  start_day         = START_DAY,   ## The first day of the data we are using
                  DIR_s_T_fact      = 25           ## percentage of direct under simple threshold CS_ref_DIR (TESTING)
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


#' ### Threshold Values for Criteria ###
#' For mean and max Most evaluations of clear sky models find that the average
#' bias error of the model is less than 10%, often around 7% [1, 83]. Therefore,
#' a fixed threshold of $\pm 75 W / m^2$ within the mean and max of the clear sky model
#' was chosen.
#'

alpha_models <- data.frame(
    DPP       = c( 0.85 , 1.25 , 1.049 ),
    KC        = c( 0.9  , 1.25 , 1.154 ),
    HAU       = c( 0.8  , 1.20 , 0.991 ),
    BD        = c( 0.95 , 1.25 , 1.072 ),
    ABCG      = c( 0.90 , 1.30 , 1.100 ),
    RS        = c( 0.85 , 1.25 , 1.002 ),
    KASTEN    = c( 0.85 , 1.25 , 1.125 ),
    INEICHEN  = c( 0.85 , 1.25 , 1.160 ),
    row.names = c("lower","upper","best") )

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

## select air mass model
AM_models_list <- c(
                     "AM_simple"
                     # "AM_kasten_young",
                     # "AM_young",
                     # "AM_rodgers"
                   )

## select monthly evaluation or all data
if (MONTHLY) {
    mmonths <- 1:12
} else {
    mmonths <- 13     ## just a fake value
}


####  Create combinations or cases to optimize and evaluate  ####
combinations <- expand.grid( CS_models = CS_models_list,
                             AM_models = AM_models_list,
                             alpha     = NA,
                             cost      = NA,
                             rmse      = NA,
                             CS_count  = NA,
                             month     = mmonths,
                             stringsAsFactors = F)


#'
#' ### Clear Sky detection Algorithm values ###
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


#### 6. LDI ###
#' #### 6. Low Direct Irradiance limit (LDI)
#'
#'
MS$LDIlim <- 5
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
#' #### 7. Low Global Irradiance limit (LGI)
#'
#'
#' Global irradiance below `r MS$VGIlim` level can not be identified
#'

#+ include=T, echo=F

#### 8. FCS ####
FCSlim <- MS$nt
#'
#' #### 8. Too Few CS point for the day (FCS)
#'
#'
#' If in a day there less than `r FCSlim` clear sky points will exclude them from optimizing
#'
#+ include=T, echo=F


#### 9. FDP ####
FDPlim <- MS$nt * 3
#'
#' #### 9. Too Few data points for the day (FDP)
#'
#'
#' If in a day there are less than `r FDPlim` data points will be excluded from optimizing.
#'
#+ include=T, echo=F




#### 11. DsT ####
#'
#' #### 11. Too low Direct signal (DsT)
#'
#'
#' This is a simple hard limit on the lower possible Direct radiation with clear sky.
#'
#' The threshold is `r MS$DIR_s_T_fact`% lower than a reference value.
#'
#' $$ I_d = I_0 * 0.7^{{AM}^{0.678}} * cos({ZSA}) $$
#'
#' Where ${AM}$ is the selected air-mass model.
#'
#'
#+ include=T, echo=F



#### Minimum Elevation ####
#'
#' #### Exclude data near horizon from optimization ####
#'
#' Data with elevation below `r MIN_Elevation` will be excluded from optimizing.
#'



####  Reduce data that will be excluded anyway from optimization ####

## limit too low radiation
strong$wattGLB[ strong$wattGLB < MS$VGIlim ] <- NA
strong$wattHOR[ strong$wattHOR < MS$LDIlim ] <- NA
strong <- strong[ !(is.na(strong$wattHOR) & is.na(strong$wattGLB)), ]

## limit elevation
strong <- strong[ strong$Elevat >= MIN_Elevation, ]

##TODO exclude days with few data (not much anyway)



## create a list of days to use
if (length(dayslist) < MS$sample_n_days) MS$sample_n_days <- length(dayslist)

if (TEST) {
    dayslist <- sort(sample(dayslist, MS$sample_n_days ), decreasing = T)
}








## Data frames for data collection, use global to be available inside
## function and after function ends
optipp               <<- data.frame()
combinations_results <<- data.frame( stringsAsFactors = F )
daily_stats          <<- data.frame()


####    Clear sky function !!   ####
## This function do the CS id and calculates the best coefficient for one model
## combination setup
filter <- function( alpha, month ) {

    strong$CSflag <- 99
    ## can not identify missing data
    Flag_key <- 10
    strong$CSflag[ is.na(strong$wattGLB) ]                         <- Flag_key
    strong[[paste0("CSflag_", Flag_key)]][ is.na(strong$wattGLB) ] <- TRUE

    ## exclude days with few data

    if (MONTHLY) {
        ## monthly check
        subdaylist <- dayslist[as.numeric( format(dayslist, "%m") ) == month]
        if (length(subdaylist) < 10 ) {
            text <- paste("\nToo few days per month:",month,"\n")
            cat(text)
            warning(text)
        }
    } else {
        ## all data
        subdaylist <- dayslist
    }

    if (TEST) {
        ## sample some days only
        length(subdaylist)
    }

    # print(subdaylist)

    ## loop days from data base
    for (aa in subdaylist) {
        ## Day variables
        aday  <- as.Date( aa , origin = "1970-01-01")
        sell  <- strong$Day == (aday)
        doy   <- as.numeric(format(aday, "%j"))
        mont  <- as.numeric(format(aday, "%m"))
        nt_hw <- MS$nt %/% 2
        cat(paste(aday),"\n")

        ## empty daily variables
        MeanVIPcnt <- NA
        MaxVIPcnt  <- NA
        VILcnt     <- NA
        VCTcnt     <- NA
        VSMcnt     <- NA

        ## relative clear sky acceptance limit
        relative_CS_lim <- sum(sell) * MS$relative_CS

        ## Data selection for day
        subday       <- strong[ sell, ]
        have_glb     <- !is.na(subday$wattGLB)
        have_glb_idx <- which(have_glb)
        tot_p        <- length(subday$wattGLB)


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


        #### Apply filters ####

        #---- 9. Too Few data point for the day (FDP) --------------------------
        if (FDP_active) {
            Flag_key <- 9
            if (nrow(subday) > 0 & tot_p <= FDPlim ) {
                subday$CSflag <- Flag_key
                subday[[paste0("CSflag_",Flag_key)]] <- TRUE
                cat(paste("Skip day FDP:",aday,tot_p),"\n")
                next
            }
        }


        #---- 1. Mean value of irradiance during the time period ---------------
        if (MeanVIP_active) {
            Flag_key <- 1
            ## create some modeled values
            CS_ref_rm_base    <- runmean(x = CS_ref_safe, k = MS$nt, alg = "C")
            CS_ref_rm_VIP_low <- MS$MeanVIP_fct * CS_ref_rm_base - MS$CS_ref_rm_VIP_low
            CS_ref_rm_VIP_upp <- MS$MeanVIP_fct * CS_ref_rm_base + MS$CS_ref_rm_VIP_upp
            GLB_rm            <- runmean(x = subday$wattGLB, k = MS$nt, alg = "C")
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


        #---- 2. Max value of irradiance during the time period ----------------
        if (MaxVIP_active) {
            Flag_key            <- 2
            CS_ref_rmax_base    <- runmax(x = CS_ref_safe,    k = MS$nt, alg = "C")
            CS_ref_rmax_VIP_upp <- MS$MaxVIP_fct * CS_ref_rmax_base + MS$MaxVIP_off_upp
            CS_ref_rmax_VIP_low <- MS$MaxVIP_fct * CS_ref_rmax_base - MS$MaxVIP_off_low
            GLB_rmax            <- runmax(x = subday$wattGLB, k = MS$nt, alg = "C")
            ##  pass as clear
            MaxVIP_pass <- GLB_rmax < CS_ref_rmax_VIP_upp & GLB_rmax > CS_ref_rmax_VIP_low  ## apply upper and lower filter

            indx_todo   <- which( have_glb & MaxVIP_pass )
            if ( length(indx_todo) > 0 ) {
                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99

                for (i in indx_todo ) {
                    walk(i, nt_hw , tot_p )
                    subday$CSflag[w_sta:w_end][ (! subday$CSflag[w_sta:w_end] == 0 ) &
                                                (  subday$CSflag[w_sta:w_end] == 99) ] <- 0
                } ##END for loop all time periods

            } else {  ##END if there is work to do
                if (FAST_SKIP) next  ##  skip the rest of the loop
            }
            ## set MaxVIP flag
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #----  3. Variability in irradiance by the length (VIL) ----------------
        if (VIL_active) {
            Flag_key  <- 3
            indx_todo <- which( have_glb )
            if ( length(indx_todo) > 0 ) {
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
                    GLB_length[i] <- sum(sqrt(DeltaVSq_GLB + DeltaTSq_GLB), na.rm = T )

                    ## calculate length for reference
                    DeltaVSq_ref     <- (data.table::shift(date_win_ref) - date_win_ref)**2
                    DeltaTSq_ref     <- as.numeric( !is.na(DeltaVSq_ref) )**2
                    CS_ref_length[i] <- sum(sqrt(DeltaVSq_ref + DeltaTSq_ref), na.rm = T )

                    ## pass test as clear
                    pass <- GLB_length[i] < (MS$MaxVIL_fct * CS_ref_length[i] + MS$offVIL_upl) &
                            GLB_length[i] > (MS$MinVIL_fct * CS_ref_length[i] - MS$offVIL_dwl)
                    if (is.na(pass)) pass <- FALSE

                    ## set VIL flag
                    subday$CSflag[w_sta:w_end][ ( ! subday$CSflag[w_sta:w_end] == 0)  &
                                                (   subday$CSflag[w_sta:w_end] == 99) &
                                                    pass                                ] <- 0

                } ##END for loop all points
            } else {  ##END if there is work to do
                if (FAST_SKIP) next  ##  skip the rest of the loop
            }
            #### if it is not clear is VIL
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 4.  Variance of Changes in the Time series (VCT) -----------------
        if (VCT_active) {
            Flag_key  <- 4
            indx_todo <- which( have_glb )
            if ( length(indx_todo) > 0 ) {
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

                    GLB_sigma[i] <- sqrt( sum( (s_i[w_sta:w_end] - s_bar)**2 , na.rm = T )  / ( MS$nt - 1 ) ) /
                                    sum( data_win_glb , na.rm = T) / MS$nt

                    ## pass test as clear
                    pass <- GLB_sigma[i] < MS$offVCT
                    if (is.na(pass)) pass <- F

                    ## set VCT flag
                    subday$CSflag[w_sta:w_end][ ( ! subday$CSflag[w_sta:w_end] == 0)  &
                                                (   subday$CSflag[w_sta:w_end] == 99) &
                                                    pass                                ] <- 0

                } ##END for loop all points
            } else {  ##END if there is work to do
                if (FAST_SKIP) next  ##  skip the rest of the loop
            }
            #### if it is not clear is VCT
            subday[[paste0("CSflag_", Flag_key)]][ subday$CSflag == 99 ] <- TRUE
            subday$CSflag[subday$CSflag == 99]                           <- Flag_key
        }


        #---- 5. Variability in the Shape of the irradiance Measurements (VSM) ----
        if (VSM_active) {
            Flag_key  <- 5
            indx_todo <- which( have_glb )
            if ( length(indx_todo) > 0 ) {

                ## start with old clear as 99
                subday$CSflag[subday$CSflag == 0] <- 99
                x_i_CS <- data.table::shift(CS_ref) - CS_ref

                for (i in indx_todo) {
                    ## resolve window by index
                    walk(i, nt_hw , tot_p )

                    ## Do calculation ##
                    data_win_glb  <- subday$wattGLB[w_sta:w_end]
                    DeltaVSq_GLB  <- (data.table::shift(data_win_glb) - data_win_glb)

                    suppressWarnings(
                        {GLB_Xi[i] <- max( abs( DeltaVSq_GLB - x_i_CS[i] ), na.rm = T)}
                    )

                    ## pass test as clear
                    pass = GLB_Xi[i] < MS$offVSM
                    if (is.na(pass)) pass <- FALSE

                    ## an info warning
                    if ( is.infinite(GLB_Xi[i]) & i > 1 ) {
                        # print(paste(GLB_Xi[i], format(aday, "%F"),i, pass ))
                        warning(paste(GLB_Xi[i], format(aday, "%F"),i, pass ))
                    }

                    ## set VCT flag
                    subday$CSflag[w_sta:w_end][ ( ! subday$CSflag[w_sta:w_end] == 0)  &
                                                (   subday$CSflag[w_sta:w_end] == 99) &
                                                    pass                                ] <- 0

                } ##END for loop all points
            } else {  ##END if there is work to do
                if (FAST_SKIP) next  ##  skip the rest of the loop
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
            subday[ CSflag == 0 &
                    wattHOR < MS$LDIlim &
                    QCF_DIR != "Possible Direct Obstruction (23)",
                    CSflag := Flag_key ]

            subday[ CSflag == 99 &
                    wattHOR < MS$LDIlim &
                    QCF_DIR != "Possible Direct Obstruction (23)",
                    CSflag := Flag_key ]

            subday[ wattHOR < MS$LDIlim &
                    QCF_DIR != "Possible Direct Obstruction (23)",
                    paste0("CSflag_", Flag_key) := TRUE ]

            subday[ wattHOR < MS$LDIlim &
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


        #---- 8. Too Few CS point for the day (FCS) ----------------------------
        if (FCS_active) {
            Flag_key  <- 8
            ## check all other CS flags to set this
            wecare <- grep("CSflag_", names(subday), value = T)
            acount <- nrow(subday[ rowSums( subday[, ..wecare ], na.rm = T ) == 0, ])
            if ( acount < FCSlim ) {
                subday[ subday$CSflag == 0 , CSflag := Flag_key ]
                subday[ subday$CSflag == 99, CSflag := Flag_key ]
                subday[, paste0("CSflag_", Flag_key) := TRUE ]
            }
        }

        ## TODO combine new filters flags scheme


        ## rest is clear sky (we hope)
        subday$CSflag[subday$CSflag == 99] <- 0

        ## Evaluation of CS detection
        clear_sky <- subday$CSflag == 0

        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        ####  Skip day from training ####
        if (sum(clear_sky, na.rm = T) < relative_CS_lim )  {
            cat(paste(CS_name, "Skip week Day minutes CS/lim/total  ", sum(clear_sky, na.rm = T),"/",relative_CS_lim,"/", sum(sell)),"\n")
            next  ##  skip the rest of the loop and avoid alpha optimization
        }
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




        if ( all(is.na(CS_ref[clear_sky])) & all(is.na(subday$wattGLB[clear_sky])) ) {
            RMSE <- Inf
            MBE  <- Inf
            cost <- Inf
        } else {
            RMSE <- RMSE(CS_ref[clear_sky], subday$wattGLB[clear_sky])
            MBE  <- MBE(Meas =subday$wattGLB[clear_sky], Esti = CS_ref[clear_sky] )
            cost <- MSE(subday$wattGLB[clear_sky], CS_ref[clear_sky])
        }


        ## ID statistics
        MeanVIPcnt <- sum(subday$CSflag == 1, na.rm = T)
        MaxVIPcnt  <- sum(subday$CSflag == 2, na.rm = T)
        VILcnt     <- sum(subday$CSflag == 3, na.rm = T)
        VCTcnt     <- sum(subday$CSflag == 4, na.rm = T)
        VSMcnt     <- sum(subday$CSflag == 5, na.rm = T)


        ## keep Clear Sky detection
        strong$CSflag[sell]   <- subday$CSflag
        strong$CS_ref[sell]   <- CS_ref


        keepday <- data.frame( Date       = aday,
                               CS_model   = combinations$CS_models[mo],
                               AM_model   = combinations$AM_models[mo],
                               alpha      = alpha,
                               RMSE       = RMSE,
                               MBE        = MBE,
                               cost       = cost,
                               MeanVIPcnt = MeanVIPcnt,
                               MaxVIPcnt  = MaxVIPcnt,
                               VILcnt     = VILcnt,
                               VCTcnt     = VCTcnt,
                               VSMcnt     = VSMcnt)

        daily_stats <<- rbind( daily_stats , keepday )

    } ##END day loop


    strong$CSflag[strong$CSflag == 99] <- NA

    ## Evaluation of CS detection metrics
    clear_sky <- strong$CSflag == 0

    if ( all(is.na(strong$wattGLB[clear_sky])) & all(is.na(strong$CS_ref[clear_sky])) ) {

        RMSE <- Inf
        MBE  <- Inf
        cost <- Inf
    } else {

        RMSE <- RMSE( strong$wattGLB[clear_sky], strong$CS_ref[clear_sky] )
        MBE  <- MBE(  strong$wattGLB[clear_sky], strong$CS_ref[clear_sky] )
        cost <- MSE(  strong$wattGLB[clear_sky], strong$CS_ref[clear_sky] )
    }

    kkepr <- cbind( data.frame( combinations$CS_models[mo],
                                combinations$AM_models[mo],
                                cost,
                                RMSE,
                                MBE,
                                alpha,
                                sum(clear_sky, na.rm = T)),
                    table(strong$CSflag) )


    optipp <<- rbind(optipp, kkepr)

    ## keep each model run
    combinations$alpha[   mo] <- alpha
    combinations$cost[    mo] <- cost
    combinations$rmse[    mo] <- RMSE
    combinations$CS_count[mo] <- sum(clear_sky, na.rm = T)

    combinations_results <<- rbind(combinations_results, combinations[mo,])

    ## info
    cat(paste("opt:", CS_name, AM_name, alpha, cost, sum(clear_sky, na.rm = T), RMSE ,"\n"))

    ## this value is used be the optimization routine to determine fitness
    return(cost)

} ##END CS function



#---- Optimization for all combinations ----------------------------------------
# for ( mo in 1:3 ) {  ## for test
for ( mo in 1:nrow(combinations) ) {

    ## In case of paralisation beware that we use global variables in the  !!
    ## filter function and elsewhere!                                      !!

    ## Get a model instance
    CS_model <- get(combinations$CS_models[mo])
    AM       <- get(combinations$AM_models[mo])
    CS_name  <- combinations$CS_models[mo]
    AM_name  <- combinations$AM_models[mo]
    cm_month <- combinations$month[mo]

    A_low    <- alpha_models[,CS_name][1]
    A_upp    <- alpha_models[,CS_name][2]
    A_bes    <- alpha_models[,CS_name][3]


    ## print model info
    cat(paste("\nSTART: ",CS_name, AM_name, A_low, "-", A_upp, cm_month,"\n"))

    ## optimization method
    # aaaaaaa = optim(0.9:1.1 , filter, method = "Brent", lower = 0.9, upper = 1.1,
                    # control = list(maxit = 10, trace=10) )

    ## optimization method
    # aaaaaaa = optim(0.9:1 , filter,   control = list(maxit = 10) )


    #----    Optimize alpha coefficient for this model set    ------------------
    bbbbbbb <- optimise( filter,
                         month = cm_month,
                         lower = A_low,
                         upper = A_upp,
                         tol   = MS$tolerance )
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    ## gather results
    alpha_models[,CS_name][3] <- bbbbbbb$minimum
    combinations$alpha[mo]    <- bbbbbbb$minimum
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


gather_results <- data.frame()
for (fmm in unique( combinations_results$CS_models )) {
    for (mmm in unique( combinations_results$month )) {

        sell = (combinations_results$CS_models == fmm & combinations_results$month == mmm)
        temp = combinations_results[sell,]

        gather_results <- rbind(gather_results, temp[which(temp$cost == min(temp$cost, na.rm = T)), ][1,])
    }
}




## save output of optimization
filename <- paste0(model_results_file,format(Sys.time(), "%F_%H%M%S"))
save(alpha_models,
     combinations_results,
     combinations,
     gather_results,
     dayslist,
     MS,
     optipp,
     daily_stats,
     file = paste0(filename,".Rds"))



for (fffla in unique(optipp$Var1)) {
    sellsss = optipp$Var1 == fffla
    plot(optipp$alpha[sellsss], optipp$Freq[sellsss], main = fffla)
}





layout(matrix(c(1),1,1,byrow = TRUE),  TRUE) ; plot(1,1)



pdf(file = paste0(filename,".pdf"), width = 10, height = 7 )

layout(matrix(c(1,2,3),3,1,byrow = TRUE),  TRUE)
wemodels <- unique(combinations_results$CS_models)
for (acsmodel in wemodels) {
    sel_model = combinations_results$CS_models == acsmodel
    tmp = combinations_results[sel_model,]

    ammodels = unique( tmp$AM_models )

    best = alpha_models["best",acsmodel]

    best_cost = tmp[tmp$alpha == best,][1,]["cost"]
    best_rmse = tmp[tmp$alpha == best,][1,]["rmse"]
    best_coun = tmp[tmp$alpha == best,][1,]["CS_count"]


    par(mar= c(0,4,2,1))
    plot(tmp$alpha, tmp$cost , pch=18, ylab = "cost")
    abline(v=best, lty=3 )
    abline(h=best_cost,lty=3 )
    text(x = mean(tmp$alpha)*.99, y = best_cost , labels = paste("cost:", best_cost ),pos = 3 )
    title( main = acsmodel )

    ccc = 1
    for (aammodel in ammodels) {
        sel_amass = tmp$AM_models == aammodel
        ccc = ccc + 1
        points(tmp$alpha[sel_amass], tmp$cost[sel_amass], pch=19, col = ccc)
    }

    par(mar= c(0,4,0,1))
    plot(tmp$alpha, tmp$rmse , pch=18, ylab = "rmse")
    abline(v=best,     lty=3 )
    abline(h=best_rmse,lty=3 )
    text(x = mean(tmp$alpha)*.99, y = best_rmse , labels = paste("rmse:", best_rmse ),pos = 3 )


    ccc = 1
    for (aammodel in ammodels) {
        sel_amass = tmp$AM_models == aammodel
        ccc = ccc + 1
        points(tmp$alpha[sel_amass], tmp$rmse[sel_amass], pch=19, col = ccc)
    }


    par(mar= c(2,4,0,1))

    plot(tmp$alpha, tmp$CS_count , pch=18, ylab = "cs count")
    abline(v=best, lty=3 )
    abline(h=best_coun,lty=3 )
    text(x = mean(tmp$alpha)*.99, y = best_coun , labels = paste("count:", best_coun ),pos = 3 )


    ccc = 1
    for (aammodel in ammodels) {
        sel_amass = tmp$AM_models == aammodel
        ccc = ccc + 1
        points(tmp$alpha[sel_amass], tmp$CS_count[sel_amass], pch=19, col = ccc)
    }
    text(best,sum(range(tmp$CS_count[sel_amass]))/2, labels = paste("alpha:", best) )

}

layout(matrix(c(1),1,1,byrow = TRUE),  TRUE)
par(mar= c(2,4,2,1))



plot(gather_results$rmse, gather_results$CS_count , xlab = "RMSE" , ylab = "CS count")
text(gather_results$rmse, gather_results$CS_count, labels = gather_results$CS_models, cex = .6, pos = 4)

plot(gather_results$cost, gather_results$CS_count , xlab = "Cost" , ylab = "CS count")
text(gather_results$cost, gather_results$CS_count, labels = gather_results$CS_models, cex = .6, pos = 4)

best_results <- data.frame()
layout(matrix(c(1,2,3),3,1,byrow = TRUE),  TRUE)
for (amo in unique( gather_results$month ) ) {

    print(amo)
    selm = gather_results$month == amo


    par(mar= c(2,4,1,1))
    plot(gather_results$cost[selm],gather_results$CS_count[selm], ylab = "count")
    text(gather_results$cost[selm],gather_results$CS_count[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)
    title(main = amo)

    par(mar= c(2,4,0,1))
    plot(gather_results$cost[selm],gather_results$alpha[selm], ylab = "alpha")
    text(gather_results$cost[selm],gather_results$alpha[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)

    plot(gather_results$cost[selm],gather_results$rmse[selm], ylab = "rmse")
    text(gather_results$cost[selm],gather_results$rmse[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)

    line = which( gather_results$cost[selm] == min(gather_results$cost[selm]) )
    best_results <- rbind(best_results, gather_results[selm,][line,] )
}

par(mar= c(2,4,1,1))
plot(best_results$cost,best_results$CS_count, ylab = "count")
text(best_results$cost,best_results$CS_count, labels = best_results$CS_models, cex = .6, pos = 4)
title(main = "Best for month")

par(mar= c(2,4,0,1))
plot(best_results$cost,best_results$alpha, ylab = "alpha")
text(best_results$cost,best_results$alpha, labels = best_results$CS_models, cex = .6, pos = 4)

plot(best_results$cost,best_results$rmse, ylab = "rmse")
text(best_results$cost,best_results$rmse, labels = best_results$CS_models, cex = .6, pos = 4)

layout(matrix(c(1),1,1,byrow = TRUE),  TRUE)
par(mar= c(2,4,2,1))


dev.off()







#' ### Clear Sky detection Algorithm values ###
#+ include=T, echo=FALSE
# panderOptions('table.emphasize.rownames', F)
panderOptions('table.alignment.default',  "right")
panderOptions('table.alignment.rownames', "right")
panderOptions('table.split.cells',        c(50,10))
panderOptions('table.style',              'rmarkdown' )

pander(t(MS))
pander(alpha_models)
pander(combinations)
pander(combinations_results)

gather_results


# load("/home/athan/Aerosols/DATA/model_opt/Combinations_results_m_2017-05-27_001727.Rds")


plot(gather_results$month, gather_results$alpha)
plot(gather_results$month, gather_results$cost)
plot(gather_results$month, gather_results$rmse)


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
