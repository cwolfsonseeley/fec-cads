source("r/dl-functions.R")
library(tidyverse)
library(getcdw)
library(stringdist)

# download the data and load the individual contributions file into memory
fec_year <- 2018
get_fec(fec_year)

# run once only
# getcdw::get_cdw("
# create table rdata.fec_stage (
#     sub_id varchar2(22),
#     name varchar2(400),
#     city varchar2(400),
#     state varchar2(2),
#     zip_code varchar2(25),
#     employer varchar2(400),
#     occupation varchar2(400),
# constraint fec_stage primary key (sub_id) )
# tablespace rdata_ts", dsn = "URELUAT_DEVEL")
# getcdw::get_cdw("grant all on rdata.fec_stage to tarak", dsn = "URELUAT_DEVEL")

cdw <- getcdw::connect("URELUAT_DEVEL")
getcdw::get_cdw("delete from rdata.fec_stage", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(cdw)

chunked_unzipper(fec_year, "indiv")
# just checking
# getcdw::get_cdw("select count(*) from rdata.fec_stage", dsn = "URELUAT_DEVEL")

getcdw::get_cdw("drop table rdata.fec_stage_processed", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(connect(dsn = "URELUAT_DEVEL"))

getcdw::get_cdw("sql/preprocess.sql", dsn = "URELUAT_DEVEL")
ROracle::dbCommit(connect(dsn = "URELUAT_DEVEL"))
