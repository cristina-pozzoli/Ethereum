setwd()
df <- read.csv("merged_token_transfers_check.csv", colClasses = c("character"))

library(data.table)

setDT(df)

contract_address_dict <- unique(df[, .(token_address)])
contract_address_dict[, id := seq_len(nrow(contract_address_dict))]

from_address_dict <- unique(df[, .(from_address)])
to_address_dict <- unique(df[, .(to_address)])

combined_address_dict <- merge(from_address_dict, to_address_dict, by.x = "from_address", by.y = "to_address", all = TRUE)

combined_address_dict[, id := seq_len(nrow(combined_address_dict))]


# Save dictionaries as csv files
write.csv(contract_address_dict, "dict_contract_add.csv", row.names = FALSE)

write.csv(combined_address_dict, "dict_combined_add.csv", row.names = FALSE)
