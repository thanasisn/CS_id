

####  Export CS data
library(data.table)
source("~/CODE/FUNCTIONS/R/data.R")

filelist <- list.files(path = "~/DATA/Broad_Band/CS_id/",
                       pattern = ".*_id_.*_v14_2.*.Rds",
                       full.names = T)
filelist <- grep("_stats_", filelist, invert = T, value = T)

filelist <- grep("2022|2023", filelist, invert = F, value = T)

DATA <- data.table()
for (af in filelist) {
    DATA <- rbind(DATA, readRDS(af), fill = T)
}


##_ Select only CM-21 flags for trends -------------------------------------
wecare <- grep("CSflag_", names(DATA), value = T)
wecare <- grep("_11", wecare, invert = T, value = T)

##_ Set flag for sky conditions --------------------------------------------
DATA[rowSums(DATA[, ..wecare], na.rm = T) == 0, TYPE := "Clear"]
DATA[rowSums(DATA[, ..wecare], na.rm = T) != 0, TYPE := "Cloud"]

## remove unused columns
rm.cols.DT(DATA, "CSflag_*")
rm.cols.DT(DATA, "QCF_*")
rm.cols.DT(DATA, "QCv*")
rm.cols.DT(DATA, "VIL*")
rm.cols.DT(DATA, "CS_ref*")
rm.cols.DT(DATA, "Dir*")
rm.cols.DT(DATA, "Glo*")
rm.cols.DT(DATA, "*temp*")
rm.cols.DT(DATA, "GLBINC*")
rm.cols.DT(DATA, "Clr*")
rm.cols.DT(DATA, "*tmp_cr*")
rm.cols.DT(DATA, "*ource")
rm.cols.DT(DATA, "*_strict")
rm.cols.DT(DATA, "wattHOR*")
DATA[, Day      := NULL]
DATA[, Elevat   := NULL]
DATA[, RaylDIFF := NULL]
DATA[, chp1TempCF := NULL ]
DATA[, TSIextEARTH_comb := NULL ]
DATA[, pressure := NULL ]

stop()

DATA <- DATA[, Date, TYPE]

write.csv(DATA, file = "Export_CS.csv", row.names = F)
saveRDS(DATA,  file = "Export_CS.Rds")


