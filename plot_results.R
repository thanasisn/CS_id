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
library(cowplot)
library(ggrepel)
library(latex2exp)
library(data.table)

load("~/Aerosols/DATA/model_opt/Combinations_results_2022-06-14_153313.Rds")


## __ Set ggplot global theme  -------------------------------------------------

gg_text_size <- 12

theme_paper <- function(){
    # font <- "Georgia"   #assign font family up front

    theme_bw(
        base_size = gg_text_size  # global font size was 14
    ) %+replace%    #replace elements we want to change
        theme(
            # panel.grid.major = element_blank(),    #strip major gridlines
            # panel.grid.minor = element_blank(),    #strip minor gridlines
            panel.background   = element_rect(fill = 'transparent'), #transparent panel bg


            # axis.text = element_text(face = "bold"), # bold axis labels
            # text      = element_text(size = 15),     ## srarted at 14

            # axis.ticks = element_blank(),          #strip axis ticks

            #text elements
            # plot.title = element_text(             #title
            #     family = font,            #set font family
            #     size = 20,                #set font size
            #     face = 'bold',            #bold typeface
            #     hjust = 0,                #left align
            #     vjust = 2),               #raise slightly
            #
            # plot.subtitle = element_text(          #subtitle
            #     family = font,            #font family
            #     size = 14),               #font size
            #
            # plot.caption = element_text(           #caption
            #     family = font,            #font family
            #     size = 9,                 #font size
            #     hjust = 1),               #right align
            #
            # axis.title = element_text(             #axis titles
            #     family = font,            #font family
            #     size = 10),               #font size
            #
            # axis.text = element_text(              #axis text
            #     family = font,            #axis famuly
            #     size = 9),                #font size
            #
            # axis.text.x = element_text(            #margin for axis text
            #     margin=margin(5, b = 10)),

            plot.background       = element_rect(fill = 'transparent', color = NA), #transparent plot bg
            # panel.grid.major      = element_blank(), #remove major gridlines
            # panel.grid.minor      = element_blank(), #remove minor gridlines
            legend.background     = element_rect(fill = 'transparent',
                                                 linewidth = 0.5,
                                                 color = "black"), #transparent legend bg
            legend.box.background = element_rect(fill = 'transparent'), #transparent legend panel
            # axis.line             = element_line(linewidth = .5, colour = "black", linetype = 1),

            NULL
        )
}

theme_set(theme_paper())



#' ## Base plot
#'
#+ CS-models, echo=F, include=T, results="asis"

best_results <- data.frame()
layout(matrix(c(1,2,3),3,1,byrow = TRUE),  TRUE)
for (amo in unique( gather_results$month ) ) {

    print(amo)
    selm = gather_results$month == amo

    par(mar= c(2,4,1,1))
    plot(gather_results$cost[selm], gather_results$CS_count[selm], ylab = "count")
    text(gather_results$cost[selm], gather_results$CS_count[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)
    title(main = amo)

    par(mar= c(2,4,0,1))
    plot(gather_results$cost[selm], gather_results$alpha[selm], ylab = "alpha")
    text(gather_results$cost[selm], gather_results$alpha[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)

    plot(gather_results$cost[selm], gather_results$rmse[selm], ylab = "rmse")
    text(gather_results$cost[selm], gather_results$rmse[selm], labels = gather_results$CS_models[selm], cex = .6, pos = 4)

    line = which( gather_results$cost[selm] == min(gather_results$cost[selm]) )
    best_results <- rbind(best_results, gather_results[selm,][line,] )
}



#' ## Base plot
#'
#+ CS-models-multi, echo=F, include=T, results="asis"


DT <- data.table(gather_results)

p1 <- ggplot(DT,
       aes(x     = cost,
           y     = CS_count/1000,
           label = CS_models,
           color = CS_models))      +
    ylab(TeX("CS points [$10^3$]"))        +
    xlab(element_blank())           +
    geom_point()                    +
    geom_text_repel() +
    theme(legend.position = "none")
p1



p2 <- ggplot(DT,
             aes(x     = cost,
                 y     = alpha,
                 label = CS_models,
                 color = CS_models))      +
    ylab(TeX("Adj. factor $\\alpha$")) +
    xlab(element_blank())    +
    geom_point()+
    geom_text_repel() +
    theme(legend.position="none")
p2

p3 <- ggplot(DT,
             aes(x = cost,
                 y = rmse,
                 label = CS_models,
                 color = CS_models))      +
    ylab("RMSE") +
    xlab(element_blank())    +
    geom_point() +
    geom_text_repel() +
    theme(legend.position="none")
p3


# plot_grid(p1,
#           p2,
#           p3,
#           ncol    = 1,
#           align   = "v",
#           labels  = c("(a)","(b)","(c)"),
#           label_x = 0.16,
#           label_y = 0.93,
#           hjust   = 0,
#           vjust   = 1
# )


P <- plot_grid(
    p1 + xlab(element_blank()) + theme(plot.margin = margin(0, 0, 0, 0)),
    p2 + xlab(element_blank()) + theme(plot.margin = margin(0, 0, 0, 0)),
    p3 + xlab(element_blank()) + theme(plot.margin = margin(0, 0, 0, 0)),
    ncol = 1,
    align = "v",
    labels = c("(a)","(b)","(c)"),
    label_x = 0.10,
    label_y = 0.60,
    hjust = 0, vjust = 0,
    rel_heights = c(1,1,1)
) #+ theme(plot.margin = margin(0, 0, 0, 0))
P

ggdraw(add_sub(P, "Cost", hjust = 0, vjust = 0, size = gg_text_size))
























#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
