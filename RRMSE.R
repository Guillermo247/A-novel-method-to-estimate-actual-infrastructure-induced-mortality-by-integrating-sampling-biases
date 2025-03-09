### Gomez et al. XXXXX

### R script to simulate and analyse road survey census data for vertebrate groups affected by carcass location bias

## R version 4.2.2

#Code developed by Guillermo Gómez Peña

#load necessary packages
library(R.utils)
library(ggplot2)
library(ggdist)
library(dplyr)
library(jagsUI)
library(grid)
library(gridExtra)

setwd("C:/Users/USUARIO/Documents/ESTUDIOS/Master/Conservación Pablo Olavide/TFM/Analisis datos/Scripts Github/Resultados 100 simulaciones") # set path to location where all files necessary to run the script are
path_data <- "C:/Users/USUARIO/Documents/ESTUDIOS/Master/Conservación Pablo Olavide/TFM/Resultados/Resultados 100 simulaciones"

##Vertebrate group along with their charateristics
select_group <- function(group_name) {
  if (group_name == "reptiles_g2") {
    se005ntransect10 <- "snakes/sites10_se05"
    se005ntransect10 <- "snakes/sites10_se05"
    se005ntransect100 <- "snakes/sites100_se05"
    se01ntransect10 <- "snakes/sites10_se10"
    se01ntransect100 <- "snakes/sites100_se10"
    path_group<-"Snakes"
    ppd_group <- 0.398477157
    RRMSE <- "RRMSE_Reptiles_G2"
    Group <- "Reptiles G2"
  } else if (group_name == "birds_bats_g1") {
    se005ntransect10 <- "birds_bats/sites10_se05"
    se005ntransect100 <- "birds_bats/sites100_se05"
    se01ntransect10 <- "birds_bats/sites10_se10"
    se01ntransect100 <- "birds_bats/sites100_se10"
    path_group<-"small_birds_and_bats"
    ppd_group <- 0.358632169
    RRMSE <- "RRMSE_Birds_G1"
    Group <- "Birds G1"
  } else if (group_name == "birds_g2") {
    se005ntransect10 <- "large_birds/sites10_se05"
    se005ntransect10 <- "large_birds/sites10_se05"
    se005ntransect100 <- "large_birds/sites100_se05"
    se01ntransect10 <- "large_birds/sites10_se10"
    se01ntransect100 <- "large_birds/sites100_se10"
    path_group<-"medium_and_large_birds"
    ppd_group <- 0.747433082
    RRMSE <- "RRMSE_Birds_G2"
    Group <- "Birds G2"
  } else if (group_name == "mammals_g4") {
    se005ntransect10 <- "carnivores/sites10_se05"
    se005ntransect10 <- "carnivores/sites10_se05"
    se005ntransect100 <- "carnivores/sites100_se05"
    se01ntransect10 <- "carnivores/sites10_se10"
    se01ntransect100 <- "carnivores/sites100_se10"
    path_group<-"medium_and_large_carnivores"
    ppd_group <- 0.805626598
    RRMSE <- "RRMSE_Mammals_G4"
    Group <- "Mammals G2"
  } else {
    stop("Invalid group. Please choose between 'reptiles_g2', 'birds_bats_g1', 'birds_g2' or 'mammals_g4'.")
  }
  
  return(list(se005ntransect10= se005ntransect10, se005ntransect100= se005ntransect100,se01ntransect10=se01ntransect10,
              se01ntransect100=se01ntransect100, path_group = path_group, ppd_group = ppd_group, RRMSE = RRMSE, Group = Group))
}

#Select vertebrate group along with their charateristics
group <- "reptiles_g2"  # write the group of interest
selected_group <- select_group(group)

# Number of days to simulate a month (time period for persistence)
num_days <- 30  

# Initial values for time (x) and persistence probability (y)
x_values <- 0:(num_days - 1)  # Days from 0 to num_days-1
y_values <- 1  # Initial persistence probability of 1 for roadkill event day

# Generate Persistence Curve
for (day in 1:num_days) {
  # Update the persistence probability for each day
  if (day > 1) {
    y_values[day] <- y_values[day - 1] * selected_group$ppd_group
  }
}

# Plot Persistence Curve
plot(x_values, y_values, type = "l", xlim = c(0, 6), ylim = c(0, 1),
     xlab = "Days", ylab = "Persistence Probability",
     main = "Persistence Curve",
     col = "blue", lwd = 2)

# Calculate D = Maximum Days Carcass Remains Without Decomposing
target_probability <- 0.05  # pp = 0.05, carcass is nearly disappeared on the road
D <- log(target_probability) / log(selected_group$ppd_group)

# Round D to report as a discrete value in days 
D <- round(D, 0)

# ------------------------------------------------------------------------------
#1.Standar Error carcass location and persistence bias = 0.05 nº transects = 10
# ------------------------------------------------------------------------------


# Function to load files and store them in a list
load_outputs <- function(base_path, prefix, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, prefix, "sim_", sim, "_1.RData")
  })
  list_data <- lapply(files, readRDS)
  return(list_data)
}

# Create the new object with the combined path
path_se005ntransect10 <- paste0(file.path(path_data, selected_group$se005ntransect10), "/")

# Print the path
print(path_se005ntransect10)

# Generate output prefixes with integrated path_group
output_prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all prefixes and simulations
outputs <- list()
for (prefix in output_prefixes) {
  data <- load_outputs(path_se005ntransect10, prefix, simulations)
  outputs[[prefix]] <- data
}

# Verify loaded data
print(outputs[[output_prefixes[1]]][[1]])

# Define prefixes for N
N_prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(prefix, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect10, prefix, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_prefixes)) {
  load_files(N_prefixes[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_prefixes)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_prefixes[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over prefixes and simulations to extract totalN
for (prefix in output_prefixes) {
  totalN_values[[prefix]] <- lapply(outputs[[prefix]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (prefix in output_prefixes) {
  means_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (prefix in output_prefixes) {
  vars_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (prefix in N_prefixes) {
  for (level in seq_along(results[[prefix]]$col_sum_by_level)) {
    results[[prefix]]$col_sum_by_level[[level]] <- results[[prefix]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_prefixes[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group, "_rrmse_list_se_005_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (prefix in output_prefixes) {
  # Get the current object
  temp_list <- get(object_name)
  
  # Initialize prefix entry
  temp_list[[prefix]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    actual <- results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[prefix]][[j]]
    variance <- vars_totalNs_sim_list[[prefix]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      temp_list[[prefix]][[j]] <- calculate_rrmse(estimated, actual, variance)
    } else {
      temp_list[[prefix]][[j]] <- NA  # Assign NA for missing data
      message(paste("Missing or invalid data for prefix:", prefix, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

# Create the RRMSE DataFrame from small_birds_bats_rrmse_list_se005_nsites10
rrmse_df_005_10 <- data.frame()

for (prefix in names(eval(parse(text = object_name)))) {
  for (sim in 1:length(eval(parse(text = object_name))[[prefix]])) {
    rrmse_df_005_10 <- rbind(rrmse_df_005_10, data.frame(
      RRMSE = eval(parse(text = object_name))[[prefix]][[sim]],
      Prefix = prefix
    ))
  }
}

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_005_10$Prefix))

# Define prefixes dynamically
prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
prefix_map <- setNames(labels, prefixes)

# Map prefixes to user-friendly labels
rrmse_df_005_10$Prefix <- factor(rrmse_df_005_10$Prefix, levels = names(prefix_map), labels = prefix_map)

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_005_10$Prefix))

# ------------------------------------------------------------------------------
#2.Standar Error carcass location and persistence bias = 0.05 nº transects = 100
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, prefix, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, prefix, "sim_", sim, "_1.RData")
  })
  list_data <- lapply(files, readRDS)
  return(list_data)
}

# Create the new object with the combined path
path_se005ntransect100 <- paste0(file.path(path_data, selected_group$se005ntransect100), "/")

# Print the path
print(path_se005ntransect100)

# Generate output prefixes with integrated path_group
output_prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all prefixes and simulations
outputs <- list()
for (prefix in output_prefixes) {
  data <- load_outputs(path_se005ntransect100, prefix, simulations)
  outputs[[prefix]] <- data
}

# Verify loaded data
print(outputs[[output_prefixes[1]]][[1]])

# Define prefixes for N
N_prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(prefix, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se005ntransect100, prefix, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_prefixes)) {
  load_files(N_prefixes[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_prefixes)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_prefixes[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over prefixes and simulations to extract totalN
for (prefix in output_prefixes) {
  totalN_values[[prefix]] <- lapply(outputs[[prefix]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (prefix in output_prefixes) {
  means_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (prefix in output_prefixes) {
  vars_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (prefix in N_prefixes) {
  for (level in seq_along(results[[prefix]]$col_sum_by_level)) {
    results[[prefix]]$col_sum_by_level[[level]] <- results[[prefix]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_prefixes[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group, "_rrmse_list_se_005_nsites_100")

# Create the list dynamically
assign(object_name, list())

for (prefix in output_prefixes) {
  # Get the current object
  temp_list <- get(object_name)
  
  # Initialize prefix entry
  temp_list[[prefix]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    actual <- results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[prefix]][[j]]
    variance <- vars_totalNs_sim_list[[prefix]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      temp_list[[prefix]][[j]] <- calculate_rrmse(estimated, actual, variance)
    } else {
      temp_list[[prefix]][[j]] <- NA  # Assign NA for missing data
      message(paste("Missing or invalid data for prefix:", prefix, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

# Create the RRMSE DataFrame from small_birds_bats_rrmse_list_se005_nsites100
rrmse_df_005_100 <- data.frame()

for (prefix in names(eval(parse(text = object_name)))) {
  for (sim in 1:length(eval(parse(text = object_name))[[prefix]])) {
    rrmse_df_005_100 <- rbind(rrmse_df_005_100, data.frame(
      RRMSE = eval(parse(text = object_name))[[prefix]][[sim]],
      Prefix = prefix
    ))
  }
}

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_005_100$Prefix))

# Define prefixes dynamically
prefixes <- c(
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_p1_0.05_SE_p2_0.05_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
prefix_map <- setNames(labels, prefixes)

# Map prefixes to user-friendly labels
rrmse_df_005_100$Prefix <- factor(rrmse_df_005_100$Prefix, levels = names(prefix_map), labels = prefix_map)

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_005_100$Prefix))



# ------------------------------------------------------------------------------
#3.Standar Error carcass location and persistence bias = 0.10 nº transects = 10
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, prefix, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, prefix, "sim_", sim, "_1.RData")
  })
  list_data <- lapply(files, readRDS)
  return(list_data)
}

# Create the new object with the combined path
path_se01ntransect10 <- paste0(file.path(path_data, selected_group$se01ntransect10), "/")

# Print the path
print(path_se01ntransect10)

# Generate output prefixes with integrated path_group
output_prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all prefixes and simulations
outputs <- list()
for (prefix in output_prefixes) {
  data <- load_outputs(path_se01ntransect10, prefix, simulations)
  outputs[[prefix]] <- data
}

# Verify loaded data
print(outputs[[output_prefixes[1]]][[1]])

# Define prefixes for N
N_prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(prefix, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect10, prefix, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_prefixes)) {
  load_files(N_prefixes[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_prefixes)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_prefixes[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over prefixes and simulations to extract totalN
for (prefix in output_prefixes) {
  totalN_values[[prefix]] <- lapply(outputs[[prefix]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (prefix in output_prefixes) {
  means_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (prefix in output_prefixes) {
  vars_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (prefix in N_prefixes) {
  for (level in seq_along(results[[prefix]]$col_sum_by_level)) {
    results[[prefix]]$col_sum_by_level[[level]] <- results[[prefix]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_prefixes[1]]][[1]])

# Calculate RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}

# Construct the dynamic object name
object_name <- paste0(selected_group$path_group, "_rrmse_list_se_01_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (prefix in output_prefixes) {
  # Get the current object
  temp_list <- get(object_name)
  
  # Initialize prefix entry
  temp_list[[prefix]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    actual <- results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[prefix]][[j]]
    variance <- vars_totalNs_sim_list[[prefix]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      temp_list[[prefix]][[j]] <- calculate_rrmse(estimated, actual, variance)
    } else {
      temp_list[[prefix]][[j]] <- NA  # Assign NA for missing data
      message(paste("Missing or invalid data for prefix:", prefix, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}

eval(parse(text = object_name))

# Create the RRMSE DataFrame from small_birds_bats_rrmse_list_se01_nsites10
rrmse_df_01_10 <- data.frame()

for (prefix in names(eval(parse(text = object_name)))) {
  for (sim in 1:length(eval(parse(text = object_name))[[prefix]])) {
    rrmse_df_01_10 <- rbind(rrmse_df_01_10, data.frame(
      RRMSE = eval(parse(text = object_name))[[prefix]][[sim]],
      Prefix = prefix
    ))
  }
}

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_01_10$Prefix))

# Define prefixes dynamically
prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_10_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
prefix_map <- setNames(labels, prefixes)

# Map prefixes to user-friendly labels
rrmse_df_01_10$Prefix <- factor(rrmse_df_01_10$Prefix, levels = names(prefix_map), labels = prefix_map)

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_01_10$Prefix))


# ------------------------------------------------------------------------------
#3.Standar Error carcass location and persistence bias = 0.10 nº transects = 10
# ------------------------------------------------------------------------------

# Function to load files and store them in a list
load_outputs <- function(base_path, prefix, simulations) {
  files <- sapply(simulations, function(sim) {
    paste0(base_path, prefix, "sim_", sim, "_1.RData")
  })
  
  list_outputs <- lapply(files, function(file) {
    tryCatch({
      # Attempt to read the RData file
      readRDS(file)
    }, error = function(e) {
      # Show a warning if the file cannot be opened
      warning(paste("Could not open file:", file, "- Error:", e$message))
      return(NULL) # Return NULL if the file cannot be loaded
    })
  })
  
  # Filter out NULL values (unloaded files)
  list_outputs <- Filter(Negate(is.null), list_outputs)
  
  return(list_outputs)
}


# Create the new object with the combined path
path_se01ntransect100 <- paste0(file.path(path_data, selected_group$se01ntransect100), "/")

# Print the path
print(path_se01ntransect100)

# Generate output prefixes with integrated path_group
output_prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0_output_analysis_")
)

# Simulations, in this case, we have 20
simulations <- 1:20

# Load data for all prefixes and simulations
outputs <- list()
for (prefix in output_prefixes) {
  data <- load_outputs(path_se01ntransect100, prefix, simulations)
  outputs[[prefix]] <- data
}

# Verify loaded data
print(outputs[[output_prefixes[1]]][[1]])

# Define prefixes for N
N_prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.05_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.15_N_t"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0_N_t")
)

# To ensure we correctly load and manage the N matrices, we assign these names
object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis",
  "SD_N_0.5_p2_0.15_output_analysis",
  "SD_N_0.5_p2_0_output_analysis",
  "SD_N_1.5_p2_0.05_output_analysis",
  "SD_N_1.5_p2_0.15_output_analysis",
  "SD_N_1.5_p2_0_output_analysis",
  "SD_N_0_p2_0.05_output_analysis",
  "SD_N_0_p2_0.15_output_analysis",
  "SD_N_0_p2_0_output_analysis"
)

# Load N files
load_files <- function(prefix, object_name) {
  data_list <- list()
  for (i in 1:D) {  # Levels after N_t (N_t1, N_t2 ...  N_tD) 
    for (j in 1:20) {  # 20 simulations per level
      compressed_file <- paste0(path_se01ntransect100, prefix, i, "_sim_", j, "_1.RData")
      temp_file <- tempfile(fileext = ".RData")
      
      if (file.exists(compressed_file)) {
        tryCatch({
          # Decompress if necessary
          if (grepl("\\.gz$", compressed_file)) {
            gunzip(compressed_file, destname = temp_file, remove = FALSE)
            data <- readRDS(temp_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
            unlink(temp_file) # Delete temp file
          } else {
            data <- readRDS(compressed_file)
            data_list[[paste0("N_t", i, "_sim_", j)]] <- data
          }
        }, error = function(e) {
          message(paste("Could not load file:", compressed_file, "Error:", e))
        })
      } else {
        message(paste("File does not exist:", compressed_file))
      }
    }
  }
  assign(object_name, data_list, envir = .GlobalEnv)
}

# Load all files into corresponding objects
for (i in seq_along(N_prefixes)) {
  load_files(N_prefixes[i], object_names[i])
}

# Sum N_t1, N_t2, etc., to calculate the total roadkill
sum_matrices <- function(object) {
  result_list <- list()
  for (j in 1:20) {  # 20 simulations per level
    sum_matrix <- NULL
    for (i in 1:3) {  # 3 levels after N_t
      element_name <- paste0("N_t", i, "_sim_", j)
      if (!is.null(object[[element_name]])) {
        if (is.null(sum_matrix)) {
          sum_matrix <- object[[element_name]]
        } else {
          sum_matrix <- sum_matrix + object[[element_name]]
        }
      }
    }
    result_list[[paste0("sim_", j)]] <- sum_matrix
  }
  return(result_list)
}

# Sum matrices for each object and store the result
for (object_name in object_names) {
  object <- get(object_name)
  sum_result <- sum_matrices(object)
  assign(paste0(object_name, "_summed"), sum_result, envir = .GlobalEnv)
}

# List of summed object names
summed_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_summed",
  "SD_N_0.5_p2_0.15_output_analysis_summed",
  "SD_N_0.5_p2_0_output_analysis_summed",
  "SD_N_1.5_p2_0.05_output_analysis_summed",
  "SD_N_1.5_p2_0.15_output_analysis_summed",
  "SD_N_1.5_p2_0_output_analysis_summed",
  "SD_N_0_p2_0.05_output_analysis_summed",
  "SD_N_0_p2_0.15_output_analysis_summed",
  "SD_N_0_p2_0_output_analysis_summed"
)


# Function to sum columns of each matrix in an object for comparison with analysis results
sum_columns <- function(object) {
  result_list <- list()
  for (element_name in names(object)) {
    matrix <- object[[element_name]]
    if (!is.null(matrix)) {
      col_sum <- colSums(matrix)
      result_list[[element_name]] <- col_sum
    }
  }
  return(result_list)
}

# Sum columns for each summed object and store the result
for (summed_object_name in summed_object_names) {
  object <- get(summed_object_name)
  col_sum_result <- sum_columns(object)
  assign(gsub("_summed$", "_col_sum", summed_object_name), col_sum_result, envir = .GlobalEnv)
}

# List of names of col_sum objects
col_sum_object_names <- c(
  "SD_N_0.5_p2_0.05_output_analysis_col_sum",
  "SD_N_0.5_p2_0.15_output_analysis_col_sum",
  "SD_N_0.5_p2_0_output_analysis_col_sum",
  "SD_N_1.5_p2_0.05_output_analysis_col_sum",
  "SD_N_1.5_p2_0.15_output_analysis_col_sum",
  "SD_N_1.5_p2_0_output_analysis_col_sum",
  "SD_N_0_p2_0.05_output_analysis_col_sum",
  "SD_N_0_p2_0.15_output_analysis_col_sum",
  "SD_N_0_p2_0_output_analysis_col_sum"
)

# Initialize the results object
results <- list()

# Populate the results object with the desired structure
for (i in seq_along(N_prefixes)) {
  col_sum_object_name <- col_sum_object_names[i]
  object <- get(col_sum_object_name)
  col_sum_by_level <- list()
  for (j in 1:20) {  # 20 simulations
    sim_name <- paste0("sim_", j)
    col_sum_by_level[[j]] <- object[[sim_name]]
  }
  results[[N_prefixes[i]]] <- list(col_sum_by_level = col_sum_by_level)
}

# Example of how to access the data
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])

# Create an object to store totalN values from output analysis for each level
totalN_values <- list()

# Iterate over prefixes and simulations to extract totalN
for (prefix in output_prefixes) {
  totalN_values[[prefix]] <- lapply(outputs[[prefix]], function(output) {
    output$sims.list$totalN
  })
}

# Create a list to store the means of each column at each level
means_totalNs_sim_list <- list()

# Calculate the means of each column at each level of totalN_values
for (prefix in output_prefixes) {
  means_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, mean)
  })
}

# Verify the results
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])


##########################################################################################
############################ Relative Root Mean Squared Error ############################
##########################################################################################

# Create a list to store the variances of each column at each level
vars_totalNs_sim_list <- list()

# Calculate the variances of each column at each level of totalN_values
for (prefix in output_prefixes) {
  vars_totalNs_sim_list[[prefix]] <- lapply(totalN_values[[prefix]], function(totalN_matrix) {
    apply(totalN_matrix, 2, var)
  })
}

# Add +1 to simulation results to avoid division by zero
for (prefix in N_prefixes) {
  for (level in seq_along(results[[prefix]]$col_sum_by_level)) {
    results[[prefix]]$col_sum_by_level[[level]] <- results[[prefix]]$col_sum_by_level[[level]] + 1
  }
}

# Verify the results before calculating RRMSE
print(means_totalNs_sim_list[[output_prefixes[1]]][[1]])
print(results[[N_prefixes[1]]]$col_sum_by_level[[1]])
print(vars_totalNs_sim_list[[output_prefixes[1]]][[1]])

# Calculate the RRMSE
calculate_rrmse <- function(estimated, actual, variance) {
  return(sqrt((estimated - actual)^2 + variance) / actual)
}
# Construct the dynamic object name
object_name <- paste0(selected_group$path_group, "_rrmse_list_se_01_nsites_10")

# Create the list dynamically
assign(object_name, list())

for (prefix in output_prefixes) {
  # Get the current object
  temp_list <- get(object_name, envir = .GlobalEnv)
  
  # Initialize prefix entry
  temp_list[[prefix]] <- list()
  
  for (j in 1:20) {  # 20 simulations
    if (!is.null(results[[N_prefixes[which(output_prefixes == prefix)]]]) &&
        length(results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level) >= j &&
        !is.null(means_totalNs_sim_list[[prefix]]) &&
        length(means_totalNs_sim_list[[prefix]]) >= j &&
        !is.null(vars_totalNs_sim_list[[prefix]]) &&
        length(vars_totalNs_sim_list[[prefix]]) >= j) {
      
      actual <- results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level[[j]]
      estimated <- means_totalNs_sim_list[[prefix]][[j]]
      variance <- vars_totalNs_sim_list[[prefix]][[j]]
      
      if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
        temp_list[[prefix]][[j]] <- calculate_rrmse(estimated, actual, variance)
      } else {
        temp_list[[prefix]][[j]] <- NA
        message(paste("Missing or invalid data for prefix:", prefix, "simulation:", j))
      }
    } else {
      temp_list[[prefix]][[j]] <- NA
      message(paste("Data missing for prefix:", prefix, "simulation:", j))
    }
  }
  
  # Update the object in the global environment
  assign(object_name, temp_list, envir = .GlobalEnv)
}


eval(parse(text = object_name))

# Inicializar un dataframe vacío
rrmse_df_01_100 <- data.frame(RRMSE = numeric(), Prefix = character(), Simulation = integer(), stringsAsFactors = FALSE)

for (prefix in output_prefixes) {
  for (j in 1:length(means_totalNs_sim_list[[prefix]])) {  # Iterate over the 20 simulations
    actual <- results[[N_prefixes[which(output_prefixes == prefix)]]]$col_sum_by_level[[j]]
    estimated <- means_totalNs_sim_list[[prefix]][[j]]
    variance <- vars_totalNs_sim_list[[prefix]][[j]]
    
    if (!is.null(estimated) && !is.null(actual) && !is.null(variance) && length(actual) > 0) {
      rrmse_value <- calculate_rrmse(estimated, actual, variance)
    } else {
      rrmse_value <- NA  # Assign NA if data is missing
      message(paste("Missing or invalid data for prefix:", prefix, "simulation:", j))
    }
    
    # Add row to the dataframe
    rrmse_df_01_100 <- rbind(rrmse_df_01_100, data.frame(
      RRMSE = rrmse_value,
      Prefix = prefix,
      Simulation = j
    ))
  }
}

# Verify that the prefixes have been correctly mapped
print(unique(rrmse_df_01_100$Prefix))


# Define prefixes dynamically
prefixes <- c(
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_0.5_SD_p2_0.15_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.05_output_analysis_"),
  paste0(selected_group$path_group, "_SE_P1_0.1_SE_P2_0.1_nsites_100_SD_N_1.5_SD_p2_0.15_output_analysis_")
)

# Define user-friendly labels
labels <- c(
  "SD N 0 p2 0", "SD N 0 p2 0.05", "SD N 0 p2 0.15",
  "SD N 0.5 p2 0", "SD N 0.5 p2 0.05", "SD N 0.5 p2 0.15",
  "SD N 1.5 p2 0", "SD N 1.5 p2 0.05", "SD N 1.5 p2 0.15"
)

# Create mapping using setNames()
prefix_map <- setNames(labels, prefixes)

# Map prefixes to user-friendly labels
rrmse_df_01_100$Prefix <- factor(rrmse_df_01_100$Prefix, levels = names(prefix_map), labels = prefix_map)

# Verify that all prefixes have been correctly mapped
print(unique(rrmse_df_01_100$Prefix))


# Add the 'SE nsites' column to each dataframe
rrmse_df_005_10$SE_nsites <- "SE 0.05 nsites 10"
rrmse_df_005_100$SE_nsites <- "SE 0.05 nsites 100"
rrmse_df_01_10$SE_nsites <- "SE 0.1 nsites 10"
rrmse_df_01_100$SE_nsites <- "SE 0.1 nsites 100"

# Combine all dataframes into a single one
combined_df <- rbind(rrmse_df_005_10, rrmse_df_005_100, rrmse_df_01_10, rrmse_df_01_100)

# Check the structure of the combined dataframe
head(combined_df)

# Create a copy of the combined dataframe and add the 'Group' column
selected_group$RRMSE <- combined_df
selected_group$RRMSE$Group <- selected_group$Group

# Check the structure of the new dataframe
head(selected_group$RRMSE)

# Initialize the final empty dataframe
RRMSE <- tibble(RRMSE = numeric(), Prefix = character(), Simulation = integer(), Group = character())

# Iterate over different values of selected_group (assuming it's a list or set of values)
for (group in list_of_selected_groups) {  # Make sure to define this list beforehand
  # Assign the combined dataframe with the current group
  group$RRMSE <- combined_df
  group$RRMSE$Group <- group$Group  # Add the 'Group' column
  
  # Append the data to the final dataframe
  RRMSE <- bind_rows(RRMSE, group$RRMSE)
}

# Check the structure of the final dataframe
print(dim(RRMSE))  # Check dimensions
print(unique(RRMSE$Group))  # Verify stored groups
