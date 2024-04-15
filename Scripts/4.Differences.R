#Check if there is any difference between the original dataset and the one 
# downloaded from BigQuery

setwd()

library(data.table)

dataset1 <- fread("merged_token_transfers_V3.0.0.csv")
dataset2 <- fread("merged_token_transfers_check.csv")


differences_contract_address <- dataset2$token_address[!(dataset2$token_address %in% dataset1$contract_address)]


#BLOCK NUMBER
differences_block_number <- dataset2$block_number[!(dataset2$block_number %in% dataset1$block_number)]

#TIME STAMP 
dataset2$block_timestamp <- as.numeric(as.POSIXct(dataset2$block_timestamp, format = "%Y-%m-%d %H:%M:%S", tz= "UTC"))
# Now, both datasets have the timestamp in a numeric format

differences_block_timestamp <- dataset2$block_timestamp[!(dataset2$block_timestamp %in% dataset1$time_stamp)]


#FROM and TO address
differences_from_address <- dataset2$from_address[!(dataset2$from_address %in% dataset1$from_address)]
differences_to_address <- dataset2$to_address[!(dataset2$to_address %in% dataset1$to_address)]


#SYMBOL AND NAME 
differences_symbol <- dataset2$symbol[!(dataset2$symbol %in% dataset1$symbol)]
unique(differences_symbol) #WLUNA ONLY 
differences_name <- dataset2$name[!(dataset2$name %in% dataset1$name)] #no differences 


#analysis of timestamp 
# Convert POSIXct timestamp to character
time_stamp <- format(differences_block_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Convert numeric timestamps back to POSIXct
timestamp_posixct <- as.POSIXct(differences_block_timestamp, origin = "1970-01-01", tz = "UTC")

# Convert POSIXct timestamps to character format
timestamp_char <- format(timestamp_posixct, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

write.csv(data.frame(timestamp_char), "time_stamp_char_diff.csv", row.names = FALSE)

summary(timestamp_posixct)

time_intervals <- diff(timestamp_posixct)
summary(time_intervals)


#TIMESTAMP ----
time_stamp  <- read.csv("time_stamp_char_diff.csv")
time_stamp$timestamp_posixct <- as.POSIXct(time_stamp$timestamp_char, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
time_stamp$timestamp_posixct <- paste(time_stamp$timestamp_posixct, "UTC", sep=" ")


# Find matching rows in dataset2 based on timestamps
matching_rows <- which(dataset2$block_timestamp %in% time_stamp$timestamp_posixct)
dataset2_matching <- dataset2[matching_rows, ]

date_part <- substr(dataset2_matching$block_timestamp, 1, 10)
unique_dates <- unique(date_part)
unique_dates

unique(dataset2_matching$symbol)


#BLOCK NUMBER ----
matching_rows_b <- which(dataset2$block_number %in% differences_block_number)
dataset2_matching_b <- dataset2[matching_rows_b, ]
date_part_b <- substr(dataset2_matching_b$block_timestamp, 1, 10)
unique(date_part_b)

unique(dataset2_matching_b$symbol)

#TO AND FROM ADDRESSES ----
matching_rows_from <- which(dataset2$from_address %in% differences_from_address)
dataset2_matching_from <- dataset2[matching_rows_from, ]
date_part_from <- substr(dataset2_matching_from$block_timestamp, 1, 10)
unique(date_part_from)

unique(dataset2_matching_from$symbol)

matching_rows_to <- which(dataset2$to_address %in% differences_to_address)
dataset2_matching_to <- dataset2[matching_rows_to, ]
date_part_to <- substr(dataset2_matching_to$block_timestamp, 1, 10)
unique(date_part_to)

unique(dataset2_matching_to$symbol)

    
# Check if all block_timestamp values in df_blocks are present in df_timestamps
all_values_present <- all(dataset2_matching_b$block_timestamp %in% dataset2_matching$block_timestamp)


if (all_values_present) {
  print("All block_timestamp values in df_blocks are present in df_timestamps.")
} else {
  print("Some block_timestamp values in df_blocks are not present in df_timestamps.")
}


all_values_present <- all(dataset2_matching$block_number %in% dataset2_matching_b$block_number)


if (all_values_present) {
  print("All block_timestamp values in df_blocks are present in df_timestamps.")
} else {
  print("Some block_timestamp values in df_blocks are not present in df_timestamps.")
}


matching_rows <- which(dataset2_matching_b$block_timestamp %in% dataset2_matching$block_timestamp)

unique_numbers_df_blocks <- unique(dataset2_matching_b$block_timestamp)
unique_numbers_df_timestamps <- unique(dataset2_matching$block_timestamp)

# Compare the sets of unique numbers
if (identical(unique_numbers_df_blocks, unique_numbers_df_timestamps)) {
  print("The sets of unique numbers in both columns are the same.")
} else {
  print("The sets of unique numbers in both columns are not the same.")
}


differing_numbers_blocks <- setdiff(unique_numbers_df_timestamps, unique_numbers_df_blocks)




