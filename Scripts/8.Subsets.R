setwd()
options(scipen = 999)

library(data.table)

df <- fread("token_transfers_dict.csv")

USDT <- df[df$symbol == "USDT", ]
write.csv(USDT, "USDT.csv", row.names = FALSE)

USDC <- df[df$symbol == "USDC", ]
write.csv(USDC, "USDC.csv", row.names = FALSE)

DAI <- df[df$symbol == "DAI", ]
write.csv(DAI, "DAI.csv", row.names = FALSE)

USDP <- df[df$symbol == "USDP", ]
write.csv(USDP, "USDP.csv", row.names = FALSE)

UST <- df[df$symbol == "UST", ]
write.csv(UST, "UST.csv", row.names = FALSE)

WLUNA <- df[df$symbol == "WLUNA", ]
write.csv(WLUNA, "WLUNA.csv", row.names = FALSE)




