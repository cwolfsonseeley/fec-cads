## shadow builder:
library(readr)
readRDS("matched/2010/detailed_fec_2010.rds") %>%
    select(cmte_id, image_num, transaction_tp,
           entity_tp, name, city, state, zip_code,
           employer, occupation, transaction_dt,
           transaction_amt, sub_id, entity_id, 
           cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation,
           org_tp, connected_org_nm, cand_id, party,
           cmte_code, category, catorder, industry, sector, cause) %>%
    mutate(transaction_amt = format(transaction_amt, scientific = FALSE),
           entity_id = format(entity_id, scientific = FALSE)) %>%
    write_csv("reports/shadow.csv", append = FALSE, col_names = TRUE)
readRDS("matched/2012/detailed_fec_2012.rds") %>% 
    select(cmte_id, image_num, transaction_tp,
           entity_tp, name, city, state, zip_code,
           employer, occupation, transaction_dt,
           transaction_amt, sub_id, entity_id, 
           cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation,
           org_tp, connected_org_nm, cand_id, party,
           cmte_code, category, catorder, industry, sector, cause) %>%
    mutate(transaction_amt = format(transaction_amt, scientific = FALSE),
           entity_id = format(entity_id, scientific = FALSE)) %>%
    write_csv(path = "reports/shadow.csv",
              append = TRUE)
readRDS("matched/2014/detailed_fec_2014.rds") %>% 
    select(cmte_id, image_num, transaction_tp,
           entity_tp, name, city, state, zip_code,
           employer, occupation, transaction_dt,
           transaction_amt, sub_id, entity_id, 
           cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation,
           org_tp, connected_org_nm, cand_id, party,
           cmte_code, category, catorder, industry, sector, cause) %>%
    mutate(transaction_amt = format(transaction_amt, scientific = FALSE),
           entity_id = format(entity_id, scientific = FALSE)) %>%
    write_csv(path = "reports/shadow.csv",
              append = TRUE)
readRDS("matched/2016/detailed_fec_2016.rds") %>%
    select(cmte_id, image_num, transaction_tp,
           entity_tp, name, city, state, zip_code,
           employer, occupation, transaction_dt,
           transaction_amt, sub_id, entity_id, 
           cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation,
           org_tp, connected_org_nm, cand_id, party,
           cmte_code, category, catorder, industry, sector, cause) %>%
    mutate(transaction_amt = format(transaction_amt, scientific = FALSE),
           entity_id = format(entity_id, scientific = FALSE)) %>%
    write_csv("reports/shadow.csv", append = TRUE)
readRDS("matched/2018/detailed_fec_2018.rds") %>% 
    select(cmte_id, image_num, transaction_tp,
           entity_tp, name, city, state, zip_code,
           employer, occupation, transaction_dt,
           transaction_amt, sub_id, entity_id, 
           cmte_nm, cmte_dsgn, cmte_tp, cmte_pty_affiliation,
           org_tp, connected_org_nm, cand_id, party,
           cmte_code, category, catorder, industry, sector, cause) %>%
    mutate(transaction_amt = format(transaction_amt, scientific = FALSE),
           entity_id = format(entity_id, scientific = FALSE)) %>%
    write_csv(path = "reports/shadow.csv",
              append = TRUE)

## write to shared drive
readRDS("matched/2018/detailed_fec_2018.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2018.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2016/detailed_fec_2016.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2016.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2014/detailed_fec_2014.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2014.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2012/detailed_fec_2012.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2012.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2010/detailed_fec_2010.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2010.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2008/detailed_fec_2008.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2008.csv",
              append = FALSE, col_names = TRUE)

## create an export for the prospect discovery team
## per greg, that should:
# - summarized for each time period by entity id
# - include capacity rating and record type
# - include concat list of causes (for interest coding)
# - (jic: last rating date)
fec2018 <- readRDS("matched/2018/detailed_fec_2018.rds")

fec2014 <- readRDS("matched/2014/detailed_fec_2014.rds")
fec2016 <- readRDS("matched/2016/detailed_fec_2016.rds")

fec2010 <- readRDS("matched/2010/detailed_fec_2010.rds")
fec2012 <- readRDS("matched/2012/detailed_fec_2012.rds")

fec2008 <- readRDS("matched/2008/detailed_fec_2008.rds")

disco_context <- getcdw::get_cdw("
select 
  entity_id,
  capacity_rating_code,
  capacity_rating_desc,
  record_types,
  (select max(last_researched_date) from cdw.sf_entity_based_prspct_smry_mv
    where entity_id = d_entity_mv.entity_id) as last_researched
from cdw.d_entity_mv
where person_or_org = 'P' and record_status_code = 'A'
")

fec2018 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2018.csv", row.names = FALSE)

fec2014 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2014.csv", row.names = FALSE)
    
fec2016 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2016.csv", row.names = FALSE)

fec2010 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2010.csv", row.names = FALSE)

fec2012 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2012.csv", row.names = FALSE)

fec2008 %>% 
    mutate(cause = ifelse(is.na(cause), "Unknown", cause)) %>%
    group_by(entity_id) %>%
    summarise(total = sum(transaction_amt), 
              causes = paste(unique(cause), collapse = ", ")) %>%
    left_join(disco_context, by = "entity_id") %>% 
    write.csv("R:/Prospect Development/Prospect Analysis/external_datasets/prospect-discovery-team/fec2008.csv", row.names = FALSE)
