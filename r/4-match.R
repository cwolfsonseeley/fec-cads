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

## use frequency tables to create conditional probabilities
## these are for frequency based weight adjustments
first_wt <- fec_frequency_first %>%
    inner_join(cads_frequency_first, by = c("first" = "first_name")) %>%
    mutate(n_fec_first = as.numeric(n_fec_first), n_cads_first = as.numeric(n_cads_first)) %>%
    mutate(h = ifelse(fec_first > 1 | cads_first > 1,
                      pmin(fec_first, cads_first),
                      .6)) %>%
    mutate(nab = sum(h),
           m_first = .98 * h / sum(h),
           u_first = .98 * (fec_first * cads_first - h) / (n_fec_first * n_cads_first - sum(h))) %>%
    mutate(weight = log2(m_first / u_first)) %>%
    select(first_name = first, first_weight = weight)

last_wt <- fec_frequency_last %>%
    inner_join(cads_frequency_last, by = c("last" = "last_name")) %>%
    replace_na(list(cads_last = 0, n_cads_last = 0,
                    fec_last = 0, n_fec_last = 0)) %>%
    mutate(h = ifelse(fec_last > 1 | cads_last > 1,
                      pmin(fec_last, cads_last),
                      .6)) %>%
    mutate(m_last = h / sum(h),
           u_last = .98 * (fec_last * cads_last - h) / (n_fec_last * n_cads_last - sum(h))) %>%
    mutate(wt = log2(m_last / u_last)) %>%
    select(last_name = last, 
           last_weight = wt)

saveRDS(list(first_wt = first_wt, last_wt = last_wt),
        file = paste0("matched/matchscores/nameweights-", dt, ".rds"))

candidate_matrix %>%
    mutate(occupation = occupation, employer = employer) %>%
    transmute(fec_id, entity_id, fec_first, cads_first, fec_last, cads_last,
              wt_geo = ifelse(fec_city == cads_city | fec_zip5 == cads_zip5, agree_weight["geo"], disagree_weight["geo"]),
              wt_occ = ifelse(occupation < .5, agree_weight["occupation"], 
                              disagree_weight["occupation"]),
              wt_emp = ifelse(employer < .5, agree_weight["employer"], 
                              disagree_weight["employer"])) %>%
    inner_join(first_wt, by = c("fec_first" = "first_name")) %>%
    inner_join(last_wt, by = c("fec_last" = "last_name")) %>%
    mutate(wt_first = 
               ifelse(fec_first == cads_first, first_weight,
                      ifelse(
                          stringdist(
                              fec_first, 
                              cads_first, 
                              method = "jw", p = .1) <= .2, 
                          0, -5)),
           wt_last  = ifelse(fec_last  == cads_last, last_weight, -5)) -> matchscore

rm(candidate_matrix)

matchscore %<>% 
    replace_na(list(wt_geo = 0, wt_occ = 0, wt_emp = 0, 
                    wt_first = 0, wt_last = 0))

matchscore %>%
    group_by(fec_id, entity_id) %>%
    summarise(geo = max(wt_geo), occ = max(wt_occ), emp = max(wt_emp), 
              first = max(wt_first), last = max(wt_last)) %>%
    ungroup %>%
    mutate(score = geo + occ + emp + first + last) %>%
    group_by(fec_id) %>%
    mutate(maxscore = max(score)) %>%
    ungroup %>%
    filter(score == maxscore) -> matchdict

## TODO: review, eg:
## matchdict %>% filter(occ > 0 & geo > 0 & emp > 0 & (first < 0 | last < 0) & (first > 15 | last > 15))
## perhaps allow a few more matches in these cases

matchdict %>% 
    filter(round(score) >= 28 | (first > 0 & last > 0 & geo > 0 & score > 25)) %>%
    select(fec_id, entity_id) %>%
    distinct -> idmap

fec_to_match %<>% inner_join(idmap, by = "fec_id")

fec_to_match %>%
    inner_join(cads_names, by = "entity_id") %>%
    mutate(mi_score = ifelse(is.na(fec_mi) | is.na(middle_initial) |
                                 str_length(str_trim(fec_mi)) == 0 |
                                 str_length(str_trim(middle_initial)) == 0, 0,
                             ifelse(fec_mi == middle_initial, 1, -1))) %>%
    mutate(fname_sim = 1 - stringdist(fec_first, first_name, method = "jw", p = .1)) %>%
    group_by(fec_id) %>%
    mutate(maxsim = max(fname_sim)) %>%
    ungroup %>%
    filter(fname_sim == maxsim) %>%
    group_by(fec_id) %>%
    mutate(maxmi = max(mi_score)) %>%
    ungroup %>%
    filter(mi_score == maxmi) %>%
    select(fec_id, entity_id) %>%
    distinct -> idmap
    
fec_idmap <- copy_to(fec_db, idmap, name = "idmap")

all_cads_fec <- fec_ind %>% 
    inner_join(fec_idmap, by = "fec_id") %>%
    select(fec_id, entity_id, cmte_id:sub_id) %>%
    collect(n = Inf)

## for verification:
#http://www.fec.gov/finance/disclosure/adv-search.shtml

saveRDS(all_cads_fec, filename)
