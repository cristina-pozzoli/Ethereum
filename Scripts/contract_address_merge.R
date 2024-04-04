#set working-directory
library(here)
here()

#import dataset
df <- read.csv("token_transfers_V3.0.0.csv", colClasses = c("character"))
unique(df$contract_address)

#TOKENS

tokens <- read.csv("amended_tokens.csv", colClasses = c("character"))

colnames(tokens)[colnames(tokens) == "address"] <- "contract_address"
merged_df <- merge(df, tokens, by = "contract_address", all.x = TRUE)

unique(merged_df$symbol)
unique(merged_df$name)

#still have one missing 

missing_address <- subset(merged_df, is.na(symbol), select = contract_address)

unique(missing_address$contract_address)

#The missing address is "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9"

#By checking on the internet, this address corresponds to LUNA (Wrapped LUNA token)


merged_df$symbol[merged_df$contract_address == "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9"] <- "LUNA"
merged_df$name[merged_df$contract_address == "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9"] <- "Wrapped LUNA token"


write.csv(merged_df, file = "merged_token_transfers_V3.0.0.csv", row.names = FALSE)

