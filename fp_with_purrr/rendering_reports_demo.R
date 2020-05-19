library(tidyverse)
library(rmarkdown)
library(NHSRdatasets)

setwd("fp_with_purrr")

# get a list of organisation codes to use, these could be hardcoded, grabbed
# from a csv... in this case I'm using the first 5 values 
org_codes <- c("R1F", "R1H", "R1K", "RA2", "RA3")

# here's the code to generate these codes
# ae_attendances %>%
#   group_by(org_code) %>%
#   filter(org_code %>% str_starts("R"),
#          type == "1") %>%
#   count() %>%
#   filter(n == 36) %>%
#   pull(org_code) %>%
#   as.character() %>%
#   head(5)

# let's create a folder to store the reports in, delete it first if it already
# exists
unlink("ae_reports", TRUE, TRUE)
dir.create("ae_reports")

# now, we can iterate over the 5 org_codes and render a markdown report
org_codes %>%
  walk(~render(input = "ae_attendances.Rmd",
               output_file = paste0("ae_attendances_", .x, ".html"),
               output_dir = "ae_reports",
               params = list(org_code = .x)))

# we can view one of these reports
dir("ae_reports", full.names = TRUE)[[1]] %>%
  rstudioapi::viewer()
