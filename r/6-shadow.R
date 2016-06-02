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