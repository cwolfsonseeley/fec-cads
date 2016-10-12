library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)
#all_cads_fec <- readRDS(...)

cmte_extra_filename <- paste("data/crp/", fec_year, "/cmtes", 
                             stringr::str_sub(fec_year, 3, 4), ".txt", sep = "")

committees <- committees(fec_year)
parties <- readRDS("data/fec/dd/dd_party.rds") %>% tbl_df

## woops: notice that party code NA became an <NA> value on import (oof)
parties %<>% mutate(party_code = ifelse(is.na(party_code), "NA", party_code))

## pre-check transaction types and make SURE you know what is there
## http://www.fec.gov/finance/disclosure/metadata/DataDictionaryTransactionTypeCodes.shtml
## note that several transaction types were added to indiv. file for 2016 cycle
all_cads_fec %>%
    group_by(transaction_tp) %>%
    summarise(n = n())

## and load extra table from csr/opensecrets to map committees to ideology/industry
cmte_extra <- readr::read_delim(file = cmte_extra_filename, 
                                col_names = c("cycle", "cmte_id", "pac_short", "affiliate",
                                              "parent", "recipient_id", "recipient_code", 
                                              "candidate_id", "party_cd", "cmte_code", "code_source",
                                              "sensitive", "foreign", "active"),
                                delim = ",",
                                quote = "|")

# and the related codebook:
cmte_codes <- readr::read_csv("data/crp/codedefs/cmte_codes.csv")
names(cmte_codes) <- tolower(names(cmte_codes))

all_cads_fec %>%
    mutate(transaction_dt = as.Date(mdy(transaction_dt)),
           transaction_amt = as.numeric(transaction_amt),
           transaction_amt = ifelse(transaction_tp %in% c("20Y", "22Y"),
                                    -transaction_amt, transaction_amt)) %>%
    filter(!transaction_tp %in% c('24I', '24T')) %>%
    left_join(committees, by = "cmte_id") %>% ## inner join should work here
    select(fec_id:cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation, 
           org_tp:cand_id) %>%
    left_join(parties, by = c("cmte_pty_affiliation" = "party_code")) %>%
    select(-notes) %>%
    left_join(cmte_extra, by = "cmte_id") %>%
    select(fec_id:party, cmte_code) %>%
    left_join(cmte_codes, by = c("cmte_code" = "catcode")) %>% 
    select(-sector) %>%
    rename(category = catname, sector = `sector long`) %>%
    mutate(cause = ifelse(is.na(party), category, party)) -> cads_political

output_file <- paste("matched/", fec_year, "/detailed_fec_", fec_year, ".rds", sep = "")
saveRDS(cads_political, output_file)