---
title:     Identification of Periods of Clear Sky Irradiance in Time Series of GHI Measurements Matthew J. Reno and Clifford W. Hansen.
author:    Natsis Athanasios
documentclass: article
classoption:   a4paper,oneside
fontsize:      10pt
geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"

link-citations:  yes
colorlinks:      yes

header-includes:
  - \usepackage{caption}
  - \usepackage{placeins}
  - \captionsetup{font=small}
  - \usepackage{multicol}
  - \setlength{\columnsep}{1cm}

output:
  bookdown::pdf_document2:
    number_sections:  yes
    fig_caption:      no
    keep_tex:         no
    toc_depth:        4
    latex_engine:     xelatex
    toc:              yes
    fig_width:        7
    fig_height:       4.5
---





<!-- `````{r echo=F, include=F, eval=F} -->
<!-- knitr::opts_chunk$set(comment    = ""      ) -->
<!-- # knitr::opts_chunk$set(dev        = "pdf"   ) -->
<!-- knitr::opts_chunk$set(dev        = "png"    ) -->
<!-- knitr::opts_chunk$set(out.width  = "100%"   ) -->
<!-- knitr::opts_chunk$set(fig.align  = "center" ) -->
<!-- # knitr::opts_chunk$set(fig.pos    = '!h'    ) -->


<!-- #  Use the optimized alpha for the selected models and identify Clear-Sky -->
<!-- #  minutes. -->

<!-- ## load previous state have to override it for alpha to be used -->
<!-- load("~/Aerosols/DATA/model_opt/Combinations_results_2022-06-14_153313.Rds") -->


<!-- ## Define the model to use for CS detection be inspecting the results of the 'a' optimization script. -->
<!-- model_selection <- "HAU" -->
<!-- monthly_alphas  <- gather_results[gather_results$CS_models == model_selection,] -->

<!-- ## daily plots file name -->
<!-- plotsbase <- paste0("~/CS_id/REPORTS/DAILY/", -->
<!--                     sub("\\.R$", "", basename(Script.Name)), "_") -->
<!-- ````` -->





\newpage

# Detection of clear periods in GHI measurements for SDR tredns.


## Only filters for GHI are used for SDR tredns

## Load all data from `DATA/Broad_Band/QCRad_LongShi/QCRad_LongShi_v8_apply_CM21_CHP1_[0-9]{4}.Rds`

##  Exclude data where `wattGLB < wattHOR`

There are instances where global irradiance is less than direct.

This happens, near sunset and sunrise due to obstacles,
or due to different sunrise/sunset time due to small spatial differences.
We will exclude all this data both for global and direct.


##  Exclude other bad data indentified in raw data

<!-- strong[QCF_DIR == FALSE, wattDIR     := NA] -->
<!-- strong[QCF_DIR == FALSE, wattHOR     := NA] -->
<!-- strong[QCF_DIR == FALSE, wattDIR_sds := NA] -->
<!-- strong[QCF_DIR == FALSE, wattDIF     := NA] -->
<!-- strong[QCF_GLB == FALSE, wattGLB     := NA] -->
<!-- strong[QCF_GLB == FALSE, wattGLB_sds := NA] -->
<!-- strong[QCF_GLB == FALSE, wattDIF     := NA] -->




### Threshold Values for Criteria

For mean and max Most evaluations of clear sky models find that the average
bias error of the model is less than 10%, often around 7% [1, 83]. Therefore,
a fixed threshold of $\pm 75 W / m^2$ within the mean and max of the clear sky model
was chosen.



### Clear Sky detection Algorithm values


<!-- `````{r include=F, eval=F, echo=F} -->
<!-- # panderOptions('table.emphasize.rownames', F) -->
<!-- panderOptions('table.alignment.default',  "right") -->
<!-- panderOptions('table.alignment.rownames', "right") -->
<!-- panderOptions('table.split.cells',        c(50,10)) -->
<!-- ````` -->




### Reference clear sky irradiance model and air mass model (AM)

We select the clear sky model, the air mass calculation
and the period of $11$ minutes to use.
The characterization is done to the center value of the window.







Air mass model

$$ AM = \frac{1}{\cos(z)} $$

### Clear Sky irradiance Haurwitz model (HAU) (1945)  

\begin{equation}
\text{GHI}_\text{Clear Sky} = 0.965 \times 1098 \times \cos( \text{SZA} ) \times \exp \left( \frac{ - 0.057}{\cos(\text{SZA})} \right)  (\#eq:ahau)
\end{equation}


## Clear sky Id procedure

<!-- FDP -->
### 9. Too Few data points for the day

Days with less than $33$ data points (minutes) will be ignored.



<!-- MeanVIP -->
### 1. Mean value of irradiance during the time period 

Comparing value in a moving window of $11$ minutes.
The mean of the measured value $\overline{G}_i$ inside an envelope based on the reference model $\text{GHI}_\text{Clear Sky}$.

\begin{equation}
0.91 \times \overline{\text{GHI}}_{i\text{Clear Sky}} - 20
< \overline{G}_i <
1.095 \times \overline{\text{GHI}}_{i\text{Clear Sky}} + 30
(\#eq:MeanVIP)
\end{equation}


<!-- MaxVIP -->
### 2. Max value of irradiance during the time period

The running max measured value $M_{Gi} = max[\text{GHI}_{i}]$ is compared to a similar constructed value from the reference $M_{CSi} = max[\text{GHI}_{i\text{Clear Sky}}]$.

\begin{equation}
1 \times M_{CSi} - 75
< M_{Gi} <
1 \times M_{CSi} + 75
(\#eq:MaxVIP)
\end{equation}


<!-- VIL -->
###  3. Variability in irradiance by line length

 The length L of the sequence of line segments connecting the points of the GHI time
series is calculated by 

\begin{equation}
L = \sum_{i=1}^{n-1}\sqrt{\left ( \text{GHI}_{i+1} - \text{GHI}_{i}\right )^2 + \left ( t_{i+1} - t_i \right )^2}
(\#eq:VILeq)
\end{equation}

for the measured values and similar $L_{CS}$ usidng the reference model.





### 6. Low Direct Irradiance limit (LDI)

Ignore Direct irradiance when it is bellow $5 Watt/m^2$.
This limit is biased due to the slight location difference of the two Instruments.
The difference in shadow especially the afternoon "pole shadow" of some periods of the year.
May limit the implementation by time of day or/and period of the year

Also this can not characterize all global measurements due to gaps on direct measurements.





### 7. Low Global Irradiance limit (LGI)

<!-- Global irradiance below  5 $Watt/m^2$ level can not be identified -->




### 8. Too Few CS point for the day (FCS)


If in a day there less than 11 clear sky points will exclude them from optimizing




### 9. Too Few data points for the day (FDP)


If in a day there are less than 33 data points will be excluded from optimizing.




### 11. Too low Direct signal (DsT)

This is a simple hard limit on the lower possible Direct radiation with clear sky.

The threshold is 25% lower than a reference value.

$$ I_d = I_0 * 0.7^{{AM}^{0.678}} * cos({ZSA}) $$

Where ${AM}$ is the selected air-mass model.

"Clear sky direct normal irradiance estimation based on adjustable inputs and error correction_Zhu2019.pdf"






### Baseline value for direct irradiance

See: Clear sky direct normal irradiance estimation based on adjustable inputs and error correction_Zhu2019.pdf






| CS Flag | Test |
|:-------:|:--------------------------------------------------------------|
|   NA    | Undefined, untested                                           |
|    0    | Passed as clear sky                                           |
|    1    | Mean value of irradiance during the time period (MeanVIP)     |
|    2    | Max Value of Irradiance during the time Period (MaxVIP)       |
|    3    | Variability in irradiance by the length (VIL)                 |
|    4    | Variance of Changes in the Time series (VCT)                  |
|    5    | Variability in the Shape of the irradiance Measurements (VSM) |
|    6    | Low Direct Irradiance limit (LDI)                             |
|    7    | Low Global Irradiance limit (LGI)                             |
|    8    | Too Few CS point for the day (FCS)                            |
|    9    | Too Few data points for the day                               |
|   10    | Missing Data                                                  |
|   11    | Direct irradiance simple threshold                            |










### Cost function for optimization of alpha value.

$$ f(a) = \dfrac{ \sum_{i=1}^{n} ( a \cdot {GHI}_i - {CSI}_i )^2 }
                { n } , \qquad a > 0 $$





**END**

