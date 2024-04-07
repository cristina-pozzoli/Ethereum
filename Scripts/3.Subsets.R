
setwd("/Users/cristinapozzoli/Desktop/Behavelab/ERC20-stablecoins")
df <- read.csv("merged_token_transfers_V3.0.0.csv")

unique(df$symbol)

USDT <- df[df$symbol == "USDT", ]
write.csv(USDT, "USDT_subset.csv", row.names = FALSE)

USDC <- df[df$symbol == "USDC", ]
write.csv(USDC, "USDC_subset.csv", row.names = FALSE)

DAI <- df[df$symbol == "DAI", ]
write.csv(DAI, "DAI_subset.csv", row.names = FALSE)

LUNA <- df[df$symbol == "LUNA", ]
write.csv(LUNA, "LUNA_subset.csv", row.names = FALSE)

USDP <- df[df$symbol == "USDP", ]
write.csv(USDP, "USDP_subset.csv", row.names = FALSE)

UST <- df[df$symbol == "UST", ]
write.csv(UST, "UST_subset.csv", row.names = FALSE)

