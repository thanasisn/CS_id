

####  Export CS data
library(data.table)

filelist <- list.files(path = "~/DATA/Broad_Band/CS_id/",
                       pattern = ".*_id_.*_v14_2.*.Rds",
                       full.names = T)
filelist <- grep("_stats_", filelist, invert = T, value = T)

filelist <- grep("2021|2022|2023", filelist, invert = F, value = T)

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



