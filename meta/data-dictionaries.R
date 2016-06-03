# pick up data dictionaries for all tables
library(rvest)
library(magrittr)
library(dplyr)

to_coltype <- function(descriptor) {
    ifelse(stringr::str_detect(descriptor, "NUMBER"), "d", "c")
}

read_dd <- function(url) {
    dd_page <- xml2::read_html(url)
    dd <- rvest::html_node(dd_page, "table")
    dd <- rvest::html_table(dd, header = TRUE)
    dd <- dplyr::tbl_df(dd)
    names(dd) <- c("column", "field", "position",
                   "null", "data_type", "description")
    dd$coltype <- to_coltype(dd$data_type)
    dd
}

dd_indiv <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryContributionsbyIndividuals.shtml")
saveRDS(dd_indiv, "./data/fec/dd/dd_indiv.rds")

dd_cm <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCommitteeMaster.shtml")
saveRDS(dd_cm, "./data/fec/dd/dd_cm.rds")

dd_cn <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCandidateMaster.shtml")
saveRDS(dd_cn, "./data/fec/dd/dd_cn.rds")

dd_ccl <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCandCmteLinkage.shtml")
saveRDS(dd_ccl, "./data/fec/dd/dd_ccl.rds")

dd_oth <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCommitteetoCommittee.shtml")
saveRDS(dd_oth, "./data/fec/dd/dd_oth.rds")

dd_pas2 <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryContributionstoCandidates.shtml")
saveRDS(dd_pas2, "./data/fec/dd/dd_pas2.rds")

dd_oppexp <- read_dd("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryOperatingExpenditures.shtml")
saveRDS(dd_oppexp, "./data/fec/dd/dd_oppexp.rds")

#############################################################
# miscellaneous data dictionaries
dd_report_type <- read_html("http://www.fec.gov/finance/disclosure/metadata/ReportTypeCodes.shtml")
dd_report_type %<>%
    html_node("table") %>%
    html_table(header = TRUE)
names(dd_report_type) <- c("report_type_code", "report_type", "explanation")
saveRDS(dd_report_type, "./data/fec/dd/dd_report_type.rds")

dd_committee_type <- read_html("http://www.fec.gov/finance/disclosure/metadata/CommitteeTypeCodes.shtml")
dd_committee_type %<>%
    html_node("table") %>%
    html_table(header = TRUE)
names(dd_committee_type) <- c("committee_type_code", "committee_type", "explanation")
saveRDS(dd_committee_type, "./data/fec/dd/dd_committee_type.rds")

dd_party <- read_html("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryPartyCodeDescriptions.shtml")
dd_party %<>%
    html_node("table") %>%
    html_table(header = TRUE)
names(dd_party) <- c("party_code", "party", "notes")
saveRDS(dd_party, "./data/fec/dd/dd_party.rds")

dd_transaction_type <- read_html("http://www.fec.gov/finance/disclosure/metadata/DataDictionaryTransactionTypeCodes.shtml")
dd_transaction_type %<>%
    html_node("table") %>%
    html_table(header = TRUE, fill = TRUE)
names(dd_transaction_type) <- c("transaction_type_code", "transaction_type_description")
saveRDS(dd_transaction_type, "./data/fec/dd/dd_transaction_type.rds")

dd_disbursement_category <- read_html("http://www.fec.gov/finance/disclosure/metadata/CategoryCodes.shtml")
dd_disbursement_category %<>%
    html_node("table") %>%
    html_table(header = TRUE, fill = TRUE) %>%
    filter(!is.na(`Disbursment Category Code`))
names(dd_disbursement_category) <- c("category_code", "category_description")
saveRDS(dd_disbursement_category, "./data/fec/dd/dd_disbursement_category.rds")

###############################################
## descriptive meta-data from 3rd party sources:
poli_trans <- read_html("http://www.poligraphy.com/about/data/transactions/")
poli_codes <- poli_trans %>% 
    html_nodes("#main #thing") %>% 
    html_nodes("code") %>% 
    html_text
poli_desc <- poli_trans %>%
    html_nodes("#main #thing") %>%
    html_nodes("sourced") %>%
    html_text
poligraphy_transaction_types <- structure(poli_desc, names=poli_codes)
saveRDS(poligraphy_transaction_types, "./data/fec/dd/poligraphy_transaction_types.rds")
