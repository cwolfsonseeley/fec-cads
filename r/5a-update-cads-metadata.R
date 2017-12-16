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