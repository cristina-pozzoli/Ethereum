
#WLUNA ----

setwd()

files <- list.files(pattern = "^statistics_WLUNA.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "statistics_combined_WLUNA.csv", row.names = FALSE)



# Triad census 

triad_census_list <- list()

files <- list.files(pattern = "\\.rds$", full.names = TRUE)

for (file in files) {
  week_num <- as.numeric(gsub("\\D", "", basename(file)))
  
  triad_census <- readRDS(file)
  
  triad_census_list[[paste0("week_", week_num)]] <- triad_census
}

combined_triad_census <- bind_rows(triad_census_list, .id = "week")
combined_triad_census <- t(combined_triad_census)

combined_triad_census <- as.data.frame(combined_triad_census)
combined_triad_census$week <- gsub("week_", "", rownames(combined_triad_census))
rownames(combined_triad_census) <- NULL
combined_triad_census <- combined_triad_census[, c(ncol(combined_triad_census), 1:(ncol(combined_triad_census)-1))]

write.csv(combined_triad_census, file = "combined_triad_census.csv", row.names = FALSE)




#UST ----

setwd()

files <- list.files(pattern = "^statistics_UST.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "statistics_combined_UST.csv", row.names = FALSE)



# Triad census 

triad_census_list <- list()

files <- list.files(pattern = "\\.rds$", full.names = TRUE)

for (file in files) {
  week_num <- as.numeric(gsub("\\D", "", basename(file)))
  
  triad_census <- readRDS(file)
  
  triad_census_list[[paste0("week_", week_num)]] <- triad_census
}

combined_triad_census <- bind_rows(triad_census_list, .id = "week")
combined_triad_census <- t(combined_triad_census)

combined_triad_census <- as.data.frame(combined_triad_census)
combined_triad_census$week <- gsub("week_", "", rownames(combined_triad_census))
rownames(combined_triad_census) <- NULL
combined_triad_census <- combined_triad_census[, c(ncol(combined_triad_census), 1:(ncol(combined_triad_census)-1))]

write.csv(combined_triad_census, file = "combined_triad_census.csv", row.names = FALSE)



#DAI ----

setwd()

files <- list.files(pattern = "^statistics_DAI.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "statistics_combined_DAI.csv", row.names = FALSE)



# Triad census 

triad_census_list <- list()

files <- list.files(pattern = "\\.rds$", full.names = TRUE)

for (file in files) {
  week_num <- as.numeric(gsub("\\D", "", basename(file)))
  
  triad_census <- readRDS(file)
  
  triad_census_list[[paste0("week_", week_num)]] <- triad_census
}

combined_triad_census <- bind_rows(triad_census_list, .id = "week")
combined_triad_census <- t(combined_triad_census)

combined_triad_census <- as.data.frame(combined_triad_census)
combined_triad_census$week <- gsub("week_", "", rownames(combined_triad_census))
rownames(combined_triad_census) <- NULL
combined_triad_census <- combined_triad_census[, c(ncol(combined_triad_census), 1:(ncol(combined_triad_census)-1))]

write.csv(combined_triad_census, file = "combined_triad_census.csv", row.names = FALSE)




#USDC ----

setwd()

files <- list.files(pattern = "^statistics_USDC.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "statistics_combined_USDC.csv", row.names = FALSE)





#USDP ----

setwd()

files <- list.files(pattern = "^statistics_USDP.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "USDP/statistics_combined_USDP.csv", row.names = FALSE)


# Triad census 

triad_census_list <- list()

files <- list.files(pattern = "\\.rds$", full.names = TRUE)

for (file in files) {
  week_num <- as.numeric(gsub("\\D", "", basename(file)))
  
  triad_census <- readRDS(file)
  
  triad_census_list[[paste0("week_", week_num)]] <- triad_census
}

combined_triad_census <- bind_rows(triad_census_list, .id = "week")
combined_triad_census <- t(combined_triad_census)

combined_triad_census <- as.data.frame(combined_triad_census)
combined_triad_census$week <- gsub("week_", "", rownames(combined_triad_census))
rownames(combined_triad_census) <- NULL
combined_triad_census <- combined_triad_census[, c(ncol(combined_triad_census), 1:(ncol(combined_triad_census)-1))]

write.csv(combined_triad_census, file = "combined_triad_census.csv", row.names = FALSE)






#USDT ----

setwd()

files <- list.files(pattern = "^statistics_USDT.*\\.csv$")

combined_data <- do.call(rbind, lapply(files, read.csv))

write.csv(combined_data, "statistics_combined_USDT.csv", row.names = FALSE)









