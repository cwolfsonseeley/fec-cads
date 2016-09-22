## shadow builder:
library(readr)
readRDS("matched/2014/detailed_fec_2014.rds") %>% 
    write_csv(path = "reports/shadow.csv",
              append = FALSE, col_names = TRUE)
readRDS("matched/2016/detailed_fec_2016.rds") %>%
    write_csv("reports/shadow.csv", append = TRUE)


## write to shared drive
readRDS("matched/2016/detailed_fec_2016.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2016.csv",
              append = FALSE, col_names = TRUE)

readRDS("matched/2014/detailed_fec_2014.rds") %>%
    write_csv(path = "R:/Prospect Development/Prospect Analysis/external_datasets/fec2014.csv",
              append = FALSE, col_names = TRUE)

## create an export for the prospect discovery team
## per greg, that should:
# - summarized for each time period by entity id
# - include capacity rating and record type
# - include concat list of causes (for interest coding)
# - (jic: last rating date)
fec2014 <- readRDS("matched/2014/detailed_fec_2014.rds")
fec2016 <- readRDS("matched/2016/detailed_fec_2016.rds")

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
