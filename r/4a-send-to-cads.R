send_to_cads <- all_cads_fec %>%
    mutate(fec_cycle = fec_year) %>%
    select(fec_cycle, entity_id:sub_id) %>%
    mutate(fec_cycle = as.integer(fec_cycle),
           transaction_dt = as.Date(mdy(transaction_dt)),
           transaction_amt = as.numeric(transaction_amt),
           transaction_amt = ifelse(transaction_tp %in% c("20Y", "22Y"),
                                    -transaction_amt, transaction_amt)) %>%
    filter(!transaction_tp %in% c('24I', '24T')) %>%
    select(fec_cycle, entity_id, sub_id, cmte_id, image_num, transaction_tp,
           transaction_dt, transaction_amt)

Sys.setenv(TZ = "DB_TZ")
Sys.setenv(ORA_SDTZ = "DB_TZ")

# just looking for records that were not already in database
already_there <- get_cdw("
select distinct
    entity_id,
    sub_id
from rdata.fec")

need_to_add <- send_to_cads %>% select(entity_id, sub_id) %>%
    anti_join(already_there, by = c("entity_id", "sub_id")) %>%
    distinct

send_to_cads <- send_to_cads %>%
    inner_join(need_to_add, by = c("entity_id", "sub_id"))

# remember to remove verified nonmatches, blacklist is maintained in the warehouse
blacklist <- get_cdw("select * from rdata.fec_blacklist")

send_to_cads <- send_to_cads %>%
    anti_join(blacklist, by = c("entity_id", "sub_id"))

# now load new data
cdw <- getcdw::connect("URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "FEC", send_to_cads, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)