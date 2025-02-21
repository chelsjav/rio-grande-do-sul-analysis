library(librarian)
# using librarian to install and load packages
librarian::shelf(censobr, geobr, sf, tidyverse, dplyr, ggplot2, arrow, viridis, corrplot)

# downloading the population dataset         
jobs <- read_population(year = 2010,
                        columns = c(
                          'code_muni', # municipalities
                          'abbrev_state', # state abbreviation
                          'V6526', # income in all jobs in number of minimum wages
                          'V0010' # sample weight
                        ),
                        add_labels = 'pt', # using 'pt' for Portuguese labels
                        showProgress = FALSE,
                        as_data_frame = TRUE,
                        cache = TRUE
)

# checking the structure of the dataset
dplyr::glimpse(jobs)

# checking for missing values
colSums(is.na(jobs))

# filtering dataset to Rio Grande do Sul
muni_income <- jobs %>%
  filter(abbrev_state == 'RS') %>%
  group_by(code_muni) %>%
  summarise(
    avg_income = mean(V6526, na.rm = TRUE),
    median_income = median(V6526, na.rm = TRUE),
    sd_income = sd(V6526, na.rm = TRUE),
    count = n()
  )

# display summary statistics
summary(muni_income$avg_income)

# plotting histogram of income distribution
ggplot(muni_income, aes(x = avg_income)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Income in RS (2010)",
       x = "Average Income (Min Wages)",
       y = "Frequency") +
  theme_minimal()

# checking correlations between variables
cor_matrix <- cor(muni_income %>% select(avg_income, median_income, sd_income), use = "complete.obs")

# visualizing correlation matrix
corrplot(cor_matrix, method = "circle")

# getting municipality boundaries
municipalities <- read_municipality(code_muni = "RS", year = 2010)

# merging geospatial and jobs income data
rsincome <- municipalities %>%
  left_join(muni_income, by = c("code_muni" = "code_muni"))

# plotting the results in a choropleth map
ggplot() +
  geom_sf(data = rsincome, aes(fill = avg_income), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1) + 
  theme_void() +
  labs(
    title = "Average Income in All Jobs (in Minimum Wages) - Rio Grande do Sul (2010)",
    fill = "Income (Min Wages)"
  )
