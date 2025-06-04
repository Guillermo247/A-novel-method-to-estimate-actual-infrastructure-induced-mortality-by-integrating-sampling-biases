### Paper title: A novel method to estimate actual infrastructure-induced mortality by integrating sampling biases

### Supplementary material S13
### R script to identify roadkill observations exclusive to a single survey method

## R version 4.2.2

library(dplyr)

setwd("...") # set path to location where Supplementary material S13 data file necessary to run the script is

# Load the dataset
df <- read.csv("Supplementary material S13 data.csv", sep=";")

# Count how many different methods each ID_ATROPELLO has
unique_collisions <- df %>%
  group_by(ID_ATROPELLO) %>%
  summarise(unique_methods = n_distinct(MEDIO)) %>%
  filter(unique_methods == 1)  # Keep only those with a single MEDIO

# Filter only the unique collisions without considering repeated observations in the robust design
df_unique<- df %>%
  filter(ID_ATROPELLO %in% unique_collisions$ID_ATROPELLO)

# View the results
print(df_unique)

# Count how many collisions with confirmed observation (VISTO == 1) have a certain number of unique methods
df %>%
  filter(VISTO == 1) %>%  # Filter only seen collisions
  group_by(ID_ATROPELLO) %>%  # Group by collision ID
  summarise(unique_methods = n_distinct(MEDIO)) %>%  # Count unique methods per collision
  count(unique_methods)  # Count how many collisions have N unique methods

# Filter only the collisions with VISTO == 1
df_seen_1 <- df %>% 
  filter(VISTO == 1)

# View the first records to confirm the filtering
head(df_seen_1)

# Filter collisions that have been seen with only one method
df_single_method <- df_seen_1 %>%
  group_by(ID_ATROPELLO) %>%
  filter(n_distinct(MEDIO) == 1) %>%
  ungroup()

# View the first records to confirm correct filtering
head(df_single_method)

# Step 1: Filter collision events that were observed either exclusively by one type
#of observer (Personnel (P) or Volunteer (V)), or by both (P_V), but retains only 
#the records reported by Personnel when both types of observers are present.
df_filtered <- df_single_method %>%
  group_by(ID_ATROPELLO) %>%
  filter(n_distinct(P_V) == 1 | (n_distinct(P_V) == 2 & P_V == "P")) %>%
  ungroup()

# View the result
head(df_filtered)

# Filter collisions seen only while walking survey method (ANDANDO)
df_filtered_walking <- df_single_method %>%
  filter(MEDIO == "ANDANDO")

roadkill_only_walking <- length(unique(df_filtered_walking$ID_ATROPELLO))
print(roadkill_only_walking)

# Filter collisions seen only while cycling survey method (BICI)
df_filtered_cycling <- df_single_method %>%
  filter(MEDIO == "BICI")

roadkill_only_cycling <- length(unique(df_filtered_cycling$ID_ATROPELLO))
print(roadkill_only_cycling)

# Filter collisions seen only while driving survey method (COCHE)
df_filtered_driving <- df_single_method %>%
  filter(MEDIO == "COCHE")

roadkill_only_driving <- length(unique(df_filtered_driving$ID_ATROPELLO))
print(roadkill_only_driving)
