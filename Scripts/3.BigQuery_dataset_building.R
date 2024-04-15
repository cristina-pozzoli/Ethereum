setwd()

DAI <- read.csv("DAI.csv", colClasses = c("character"))

PAX <- read.csv("PAX.csv", colClasses = c("character"))

USDC1 <- read.csv("USDC1.csv", colClasses = c("character"))
USDC2 <- read.csv("USDC2.csv", colClasses = c("character"))
USDC3 <- read.csv("USDC3.csv", colClasses = c("character"))
USDC4 <- read.csv("USDC4.csv", colClasses = c("character"))

USDT1 <- read.csv("USDT1.csv", colClasses = c("character"))
USDT2 <- read.csv("USDT2.csv", colClasses = c("character"))
USDT3 <- read.csv("USDT3.csv", colClasses = c("character"))
USDT4 <- read.csv("USDT4.csv", colClasses = c("character"))

UST <- read.csv("UST.csv", colClasses = c("character"))

WLUNA <- read.csv("WLUNA.csv", colClasses = c("character"))


combined_dataset1 <- rbind(DAI, PAX, UST, WLUNA)

USDC <- rbind(USDC1, USDC2, USDC3, USDC4)
USDT <- rbind(USDT1, USDT2, USDT3, USDT4)

combined_dataset <- rbind(combined_dataset1, USDC, USDT)


write.csv(combined_dataset, file = "dataset.csv", row.names = FALSE)

combined_dataset <- read.csv("dataset.csv", colClasses = c("character"))

#merge with tokens

tokens <- read.csv("amended_tokens.csv", colClasses = c("character"))

colnames(tokens)[colnames(tokens) == "address"] <- "token_address"
merged_df <- merge(combined_dataset, tokens, by = "token_address", all.x = TRUE)

merged_df$symbol[merged_df$token_address == "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9"] <- "WLUNA"
merged_df$name[merged_df$token_address == "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9"] <- "Wrapped LUNA token"

write.csv(merged_df, file = "merged_token_transfers_check.csv", row.names = FALSE)




