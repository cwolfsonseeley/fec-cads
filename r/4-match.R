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

candidate_matrix %>%
    transmute(fec_id, entity_id, fec_first, cads_first, fec_last, cads_last,
              wt_geo = ifelse(fec_city == cads_city | fec_zip5 == cads_zip5, agree_weight["geo"], disagree_weight["geo"]),
              wt_occ = ifelse(stringdist(fec_occupation, cads_occupation, "cosine", q = 3) < .5,
                              agree_weight["occupation"], disagree_weight["occupation"]),
              wt_emp = ifelse(stringdist(fec_employer, cads_employer, "cosine", q = 3) < .5,
                              agree_weight["employer"], disagree_weight["employer"])) %>%
    inner_join(first_wt, by = c("fec_first" = "first_name")) %>%
    inner_join(last_wt, by = c("fec_last" = "last_name")) %>%
    mutate(wt_first = ifelse(fec_first == cads_first, first_weight, -5),
           wt_last  = ifelse(fec_last  == cads_last, last_weight, -5)) -> matchscore

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

matchdict %>% 
    filter(score >= 28 | (first > 0 & last > 0 & geo > 0 & score > 25)) %>%
    select(fec_id, entity_id) %>%
    distinct -> idmap

fec_ind %>% 
    mutate(fec_mi = middle_initial) %>%
    inner_join(idmap, by = "fec_id") %>%
    inner_join(cads_names, by = "entity_id") %>%
    mutate(mi_score = ifelse(is.na(fec_mi) | is.na(middle_initial.y) |
                                 str_length(str_trim(fec_mi)) == 0 |
                                 str_length(str_trim(middle_initial.y)) == 0, 0,
                             ifelse(fec_mi == middle_initial.y, 1, -1))) %>%
    mutate(fname_sim = 1 - stringdist(first, first_name.y, method = "jw", p = .1)) %>%
    group_by(sub_id) %>%
    mutate(maxsim = max(fname_sim)) %>%
    ungroup %>%
    filter(fname_sim == maxsim) %>%
    group_by(sub_id) %>%
    mutate(maxmi = max(mi_score)) %>%
    ungroup %>%
    filter(mi_score == maxmi) %>%
    select(cmte_id:entity_id) %>%
    distinct -> all_cads_fec

## for verification:
#http://www.fec.gov/finance/disclosure/adv-search.shtml

#saveRDS(all_cads_fec, "matched/2014/20160602.rds")
saveRDS(all_cads_fec, "matched/2016/20160602.rds")