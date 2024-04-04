setwd()
df <- read.csv("merged_token_transfers_V3.0.0.csv", colClasses = c("character"))

library(data.table)

setDT(df)

contract_address_dict <- unique(df[, .(contract_address)])
contract_address_dict[, id := seq_len(nrow(contract_address_dict))]

from_address_dict <- unique(df[, .(from_address)])
from_address_dict[, id := seq_len(nrow(from_address_dict))]

to_address_dict <- unique(df[, .(to_address)])
to_address_dict[, id := seq_len(nrow(to_address_dict))]

# Save dictionaries as csv files
write.csv(from_address_dict, "from_address_dict.csv", row.names = FALSE)

write.csv(to_address_dict, "to_address_dict.csv", row.names = FALSE)

write.csv(contract_address_dict, "contract_address_dict.csv", row.names = FALSE)
