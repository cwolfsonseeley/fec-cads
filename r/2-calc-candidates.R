candidate_matrix <- getcdw::get_cdw("sql/candidates.sql", dsn = "URELUAT_DEVEL")
save.image()

library(getcdw)
source("r/sql_queries.R")
cads_employment <- get_cdw(cads_employment_query)

candidate_matrix <- candidate_matrix %>% 
    left_join(cads_employment, by = "entity_id") %>% 
    mutate(occupation = 
               stringdist(fec_occupation,
                          cads_occupation,
                          method = "cosine", q = 3),
           employer = 
               stringdist(fec_employer,
                          cads_employer,
                          method = "cosine", q = 3))

gamma_matrix <- candidate_matrix %>%
    transmute(fec_id, entity_id,
              first, mi, last, geo,
              occupation = occupation < .5,
              employer = employer < .5) %>%
    replace_na(list(first = 0, mi = 0, last = 0, geo = 0, 
                    occupation = FALSE, employer = FALSE)) %>%
    group_by(fec_id, entity_id) %>%
    summarise_all(funs(max)) %>% ungroup

source("r/fs-model-functions.R")
preds = c("geo", "occupation", "employer")
guess_u <- rep(.1, length(preds))
names(guess_u) <- preds
guess_p <- .1
guess_m <- rep(.9, length(preds))
names(guess_m) <- preds

df <- gamma_matrix %>%
    select_(.dots=preds) %>%
    group_by_(.dots=preds) %>%
    summarise(count=n()) %>%
    ungroup

ans <- fs_weights(df, preds, "count", guess_p, guess_m, guess_u)

m <- ans$m
u <- ans$u
p <- ans$p
m[m>.999] <- .999
u <- pmin(u, random_agreement(df, preds, "count"))
u[u<.005] <- .005
agree_weight <- log2(m/u)
disagree_weight <- log2((1-m)/(1-u))
rm(m, u, p, df, ans, guess_m, guess_p, preds)

## 
library(lubridate)
dt <- paste0(year(today()), 
             stringr::str_pad(month(today()), width = 2, pad = "0"),
             stringr::str_pad(day(today()), width = 2, pad = "0"))
fname <- paste(dt, ".rds", sep = "")
dirname <- paste("matched", fec_year, sep = "/")
if (!dir.exists(dirname)) dir.create(dirname, recursive = TRUE)
filename <- paste(dirname, fname, sep = "/")

if (!dir.exists("matched/matchscores")) dir.create("matched/matchscores")
saveRDS(
    list(agree_weight = agree_weight, disagree_weight = disagree_weight),
    file = paste0("matched/matchscores/weights-", dt, ".rds")
)

#####

# name weight tables:
name_weights <- get_cdw("sql/name-weight-tables.sql", dsn = "URELUAT_DEVEL")
saveRDS(name_weights,
        file = paste0("matched/matchscores/nameweights-", dt, ".rds"))

fec_names <- get_cdw("
select distinct
  fec_id, 
  first as fec_first,
  last as fec_last
from rdata.fec_stage_processed", dsn = "URELUAT_DEVEL")

cads_first_names <- get_cdw("
select distinct
    entity_id,
    regexp_replace(upper(first_name), '[^A-Z]', '') as cads_first
from cdw.d_bio_name_mv
where 
    entity_id in (
        select entity_id from cdw.d_entity_mv 
        where 
            person_or_org = 'P' 
            and record_status_code = 'A')")

matchscore <- gamma_matrix %>%
    inner_join(fec_names, by = "fec_id") %>% 
    inner_join(cads_first_names, by = "entity_id") %>% 
    replace_na(list(fec_first = "", cads_first = "")) %>% 
    transmute(fec_id, entity_id, 
              first, mi, last,
              fec_first, cads_first, fec_last, 
              wt_geo = ifelse(geo > 0, agree_weight["geo"], disagree_weight["geo"]),
              wt_occ = ifelse(occupation > 0, agree_weight["occupation"], 
                              disagree_weight["occupation"]),
              wt_emp = ifelse(employer > 0, agree_weight["employer"], 
                              disagree_weight["employer"])) %>%
    left_join(name_weights %>% 
                  filter(type == "first") %>% 
                  select(first_name = name,
                         first_weight = weight), 
              by = c("fec_first" = "first_name")) %>%
    left_join(name_weights %>% 
                  filter(type == "last") %>% 
                  select(last_name = name,
                         last_weight = weight), 
              by = c("fec_last" = "last_name")) %>%
    replace_na(list(first_weight = -5, last_weight = -5)) %>% 
    mutate(
        fname_sim = 1 - stringdist(fec_first, cads_first, method = "jw", p = .1),
        wt_first = ifelse(fec_first == cads_first, first_weight,
                          ifelse(fname_sim >= .8, 0, -5)),
        wt_last  = ifelse(last > 0, last_weight, -5))

matchdict <- matchscore %>% 
    replace_na(list(wt_geo = 0, wt_occ = 0, wt_emp = 0, 
                    wt_first = 0, wt_last = 0)) %>%
    group_by(fec_id, entity_id) %>%
    summarise(geo = max(wt_geo), occ = max(wt_occ), emp = max(wt_emp), 
              first = max(wt_first), last = max(wt_last),
              fname_sim = max(fname_sim)) %>%
    ungroup %>%
    mutate(score = geo + occ + emp + first + last) %>%
    group_by(fec_id) %>%
    mutate(maxscore = max(score)) %>%
    ungroup %>%
    filter(score == maxscore)

fec_mi <- get_cdw("
select distinct fec_id, middle_init from rdata.fec_stage_processed
", dsn = "URELUAT_DEVEL")

cads_mi <- get_cdw("
select distinct 
    entity_id,
    upper(substr(middle_name, 1, 1)) as middle_initial
from cdw.d_bio_name_mv
where 
    entity_id in (
        select entity_id from cdw.d_entity_mv 
        where 
            person_or_org = 'P' 
            and record_status_code = 'A')")

matchdict %>% 
    filter(round(score) >= 28 | (first > 0 & last > 0 & geo > 0 & score > 25) |
               (occ > 0 & geo > 0 & emp > 0 & (first > 15 | last > 15)) |
               (fname_sim > .8 & last > 0 & emp > 0 & geo > 0 & occ > 0)) %>%
    left_join(fec_mi, by = "fec_id") %>% 
    left_join(cads_mi, by = "entity_id") %>% 
    mutate(mi_score = ifelse(is.na(middle_init) | is.na(middle_initial) |
                                 str_length(str_trim(middle_init)) == 0 |
                                 str_length(str_trim(middle_initial)) == 0, 0,
                             ifelse(middle_init == middle_initial, 1, -1))) %>%
    group_by(fec_id) %>%
    filter(fname_sim == max(fname_sim)) %>%
    group_by(fec_id) %>%
    filter(mi_score == max(mi_score)) %>%
    ungroup %>%
    select(fec_id, entity_id) %>%
    distinct -> idmap

fec_subids <- get_cdw("select distinct fec_id, sub_id from rdata.fec_stage_processed",
                      dsn = "URELUAT_DEVEL")

rm(cads_employment, cads_first_names, cads_mi, 
   candidate_matrix, fec_mi, fec_names, gamma_matrix, name_weights,
   matchscore, matchdict)

fec <- idmap %>% inner_join(fec_subids, by = "fec_id") %>% 
    select(sub_id, entity_id) %>% distinct

to_cads <- fec %>%
    inner_join(fec_ind, by = "sub_id") %>% 
    mutate(fec_cycle = fec_year) %>%
    select(fec_cycle, entity_id, sub_id:transaction_amt) %>%
    mutate(fec_cycle = as.integer(fec_cycle),
           transaction_dt = as.Date(mdy(transaction_dt)),
           transaction_amt = as.numeric(transaction_amt),
           transaction_amt = ifelse(transaction_tp %in% c("20Y", "22Y"),
                                    -transaction_amt, transaction_amt)) %>%
    filter(!transaction_tp %in% c('24I', '24T')) %>%
    select(fec_cycle, entity_id, sub_id, cmte_id, image_num, transaction_tp,
           transaction_dt, transaction_amt)

to_cads <- to_cads %>% 
    mutate(date_added = today(), date_modified = today())

Sys.setenv(TZ = "DB_TZ")
Sys.setenv(ORA_SDTZ = "DB_TZ")

# just looking for records that were not already in database
already_there <- get_cdw("
                         select distinct
                         entity_id,
                         sub_id
                         from rdata.fec")

need_to_add <- to_cads %>% select(entity_id, sub_id) %>%
    anti_join(already_there, by = c("entity_id", "sub_id")) %>%
    distinct

to_cads <- to_cads %>%
    inner_join(need_to_add, by = c("entity_id", "sub_id"))

# remember to remove verified nonmatches, blacklist is maintained in the warehouse
blacklist <- get_cdw("select * from rdata.fec_blacklist")

to_cads <- to_cads %>%
    anti_join(blacklist, by = c("entity_id", "sub_id"))

# now load new data
cdw <- getcdw::connect("URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "FEC", to_cads, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)


committees <- committees(fec_year) %>% distinct
parties <- readRDS("data/fec/dd/dd_party.rds") %>% tbl_df

## woops: notice that party code NA became an <NA> value on import (oof)
parties <- parties %>% mutate(party_code = ifelse(is.na(party_code), "NA", party_code))
