setwd("fp_with_purrr")
library(tidyverse)
library(NHSRdatasets)

# first, let's have a look at the ae_attendances dataframe
ae_attendances

# NHS provider trusts org_codes start with an "R", so let's filter to just show
# these rows of data
ae_attendances %>%
  filter(org_code %>% str_starts("R"))

# Now, we have monthly data for each organisation by department type: we want to
# summarise these to get the monthly total. We can use a functional programming
# inspired variant of summarise to make our life easier:
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  # remember, summarise "peels" one grouping layer off: we want to keep the data
  # grouped by org_code as we want to produce a plot per org_code
  summarise_at(vars(attendances), sum)

# We are now left with 3 columns of data, org_code, period and attendances. The
# data is grouped still by org_code
groups(.Last.value)

# We can "nest" the ungrouped columns: this will leave us with a tibble with a
# single row per group, and a column called "data" which contains a tibble for
# each row: this nested tibble will contain the columns and rows of ungrouped
# data
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest()

# One thing to note here: this dataset is meant to contain 3 years (36 months)
# of data, but we can see that the first two org_codes listed don't have 36 rows
# of data in their nested tibbles. We could try filtering this data...
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest() %>%
  filter(nrow(data) == 36)

# Unfortunately, just calling nrow on the data column fails... nrow only knows
# how to deal with a single dataframe, not a list of dataframes! So, we need to
# use a map function here
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest() %>%
  filter(map_dbl(data, nrow) == 36)

# We can now create a plot for this data. It's worth reminding ourselves what
# columns are available in the "data" tibbles. Let's select the data column, and
# inspect the first item from this column: this will be a single tibble
.Last.value$data[[1]]

# We can now create a simple plot function that would accept a single one of
# these tibbles. We are going to also supply the org_code to title the plot
plot_fn <- function(org_code, data) {
  data %>%
    ggplot(aes(period, attendances)) +
    geom_line() +
    geom_point() +
    labs(title = org_code)
}

# We can now add a column to our dataframe to contain the plots. We also need to
# create a filename for the plot: I'm going to store these in a folder called
# "ae_plots", so let's create this folder
unlink("ae_plots", TRUE, TRUE) # remove it if it already exists
dir.create("ae_plots")

# We are going to use the ggsave function to save the plots, so let's have a
# quick look at the documentation to see what arguments we need to provide:
?ggsave

# We need to therefore have a column called "filename", and a column called
# "plot"
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest() %>%
  filter(map_dbl(data, nrow) == 36) %>%
  # transmute is like mutate+select, it allows us to create columns as well as
  # only returning the columns specified
  transmute(filename = paste0("ae_plots/", org_code, ".png"),
            plot = map2(org_code, data, plot_fn))

# At this point, we haven't saved the plots. We can quickly view one to see if
# it looks right though
.Last.value$plot[[1]]

# We can now proceed to saving the plots: as we are only interested in the
# side-effect of saving the plot we use a walk function.
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest() %>%
  filter(map_dbl(data, nrow) == 36) %>%
  # transmute is like mutate+select, it allows us to create columns as well as
  # only returning the columns specified
  transmute(filename = paste0("ae_plots/", org_code, ".png"),
            plot = map2(org_code, data, plot_fn)) %>%
  # let's just test this works on the first 3 rows
  head(3) %>%
  pwalk(ggsave)

# Having these messages come up can be a bit annoying: let's use the quietely
# function to hide these messages
ae_attendances %>%
  filter(org_code %>% str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise_at(vars(attendances), sum) %>%
  group_nest() %>%
  filter(map_dbl(data, nrow) == 36) %>%
  # transmute is like mutate+select, it allows us to create columns as well as
  # only returning the columns specified
  transmute(filename = paste0("ae_plots/", org_code, ".png"),
            plot = map2(org_code, data, plot_fn)) %>%
  pwalk(quietly(ggsave))

dir("ae_plots")
