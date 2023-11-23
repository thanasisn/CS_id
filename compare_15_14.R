

library(data.table)




files14 <- list.files(path       = "/home/athan/DATA/Broad_Band/CS_id/",
                      pattern    = "Daily_stats_Clear_sky_id_Reno-Hansen_apply_v14_2_[0-9]{4}.Rds",
                      full.names = TRUE ,
                      recursive  = FALSE)

files15 <- list.files(path       = "/home/athan/DATA/Broad_Band/CS_id/",
                      pattern    = "Daily_stats_Clear_sky_id_Reno-Hansen_apply_v15_[0-9]{4}.Rds",
                      full.names = TRUE ,
                      recursive  = FALSE)


for (af in files15) {
    DT15 <- readRDS(af)

    yyyy <- unique(year(DT15$Date))

    DT14 <- readRDS(grep(paste(yyyy), files14, value = T))


    cat("Rows ", nrow(DT14), nrow(DT15))

    DT14$Date

}


