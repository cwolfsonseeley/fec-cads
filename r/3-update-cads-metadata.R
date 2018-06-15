# identify new committee codes not already in the database, and add them
# first: pull what is there from db
cads_committees <- get_cdw(
    paste0("select distinct cmte_id from rdata.fec_committees where fec_cycle = ", fec_year)
)

# find what's missing
new_cads_committees <- committees %>%
    anti_join(cads_committees, by = "cmte_id") %>%
    mutate(fec_cycle = fec_year) %>%
    select(fec_cycle, cmte_id, cmte_nm, cmte_dsgn, cmte_tp,
           cmte_pty_affiliation, org_tp, connected_org_nm, cand_id)

# load missing into db
cdw <- getcdw::connect("URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "FEC_COMMITTEES", new_cads_committees, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)

### 
# same with parties:
cads_parties <- getcdw::get_cdw('
select * from rdata.fec_cmte_party
')

## check
parties %>% select(party_code, party) %>% 
    anti_join(cads_parties, by = 'party_code')

#####
fec_candidates <- unzipper(fec_year, "cn")
cads_candidates <- getcdw::get_cdw(
    paste0("select * from rdata.fec_candidates where fec_cycle = ", fec_year)    
)
new_cads_candidates <- fec_candidates %>%
    mutate(fec_cycle = fec_year, cand_election_yr = as.integer(cand_election_yr)) %>% 
    anti_join(cads_candidates, by = c("fec_cycle", "cand_id")) %>%
    select(fec_cycle, cand_id, cand_name, cand_pty_affiliation, cand_election_yr,
           cand_office_st, cand_office, cand_office_district, cand_ici,
           cand_status, cand_pcc)

cdw <- getcdw::connect("URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "FEC_CANDIDATES", new_cads_candidates, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)
###

fec_ccl <- unzipper(fec_year, "ccl")
cads_ccl <- getcdw::get_cdw(
    paste0("select * from rdata.fec_ccl where fec_cycle = ", fec_year)    
)

new_cads_ccl <- fec_ccl %>% 
    mutate(fec_cycle = fec_year,
           linkage_id = as.integer(linkage_id),
           cand_election_yr = as.integer(cand_election_yr),
           fec_election_yr = as.integer(fec_election_yr)) %>% 
    anti_join(cads_ccl, by = c("fec_cycle", "linkage_id")) %>% 
    select(fec_cycle, everything())
cdw <- getcdw::connect("URELUAT_DEVEL")
res <- ROracle::dbWriteTable(
    cdw, "FEC_CCL", new_cads_ccl, 
    schema = 'RDATA',
    overwrite = FALSE, append = TRUE)
ROracle::dbCommit(cdw)