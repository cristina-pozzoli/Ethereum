setwd()
df <- read.csv("merged_token_transfers_check.csv", colClasses = c("character"))

dict1 <- read.csv("dict_contract_add.csv", colClasses = c("character"))
dict2 <- read.csv("dict_combined_add.csv", colClasses = c("character"))


#Contract address ----
ids <- vector("numeric", length = nrow(df))

for (i in 1:nrow(df)) {
  matching_row <- which(dict1$token_address == df$token_address[i])
  if (length(matching_row) > 0) {
    ids[i] <- dict1$id[matching_row]
  } else {
    ids[i] <- NA
  }
}

df$id <- ids

#double check 
unique(df$id)

sum(df$id =="1")
sum(df$id =="2")
sum(df$id =="3")
sum(df$id =="4")
sum(df$id =="5")
sum(df$id =="6")

names(df)[names(df) == "id"] <- "token_address_id"


#From address ----

merged_df <- merge(df, dict2, by.x = "from_address", by.y = "from_address", all.x = TRUE)

names(merged_df)[names(merged_df) == "id.y"] <- "from_address_id"
names(merged_df)[names(merged_df) == "id.x"] <- "token_address_id"

#To address ----
merged_df <- merge(merged_df, dict2, by.x = "to_address", by.y = "from_address", all.x = TRUE)
names(merged_df)[names(merged_df) == "id"] <- "to_address_id"


#save new df 
subset_df <- merged_df[, c("token_address_id", "from_address_id", "to_address_id", "block_timestamp", 
                           "value", "block_number", "symbol", "name", "decimals")]

write.csv(subset_df, "token_transfers_dict.csv", row.names = FALSE)





