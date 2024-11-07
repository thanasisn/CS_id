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
#'     keep_tex:         yes
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#' ---
#'
#+ include=FALSE, echo=FALSE

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "pdf"   )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'    )



####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/CS_id/plot_results.R"

options("width" = 130)


library(RColorBrewer)
library(scales)
library(pander)
library(caTools)
library(data.table)
library(ggplot2)

load("~/Aerosols/DATA/model_opt/Combinations_results_2022-06-14_153313.Rds")



#' ## Base plot
#'
#+ echo=F, include=T, results="asis"

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
