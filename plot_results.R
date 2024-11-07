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
Script.Name <- "~/CS_id/plot_results.R"

options("width" = 130)


library(RAerosols)
library(RColorBrewer)
library(scales)
library(pander)
library(caTools)
library(data.table)


load("~/Aerosols/DATA/model_opt/Combinations_results_2022-06-14_153313.Rds")




stop("DD")



kcols <- brewer.pal(11, "Set3")


par(mar = c(2,4,2,1))



#+ include=T, echo=F
source("/home/athan/Aerosols/source_R/THEORY/Extraterrestrial_radiation_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Air_mass_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Clear_sky_irradiance_models.R")
source("/home/athan/Aerosols/source_R/THEORY/Linke_turbidity_models.R")
#'




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









for (fffla in unique(optipp$Var1)) {
    sellsss = optipp$Var1 == fffla
    plot(optipp$alpha[sellsss], optipp$Freq[sellsss], main = fffla)
}




layout(matrix(c(1),1,1,byrow = TRUE),  TRUE) ; plot(1,1)




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







#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
