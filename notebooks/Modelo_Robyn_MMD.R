################################################################
#### Step 0: Setup environment


# install.packages
library(Robyn)

install.packages("extrafont")

# load library
library(extrafont)

# Import fonts available on your system. It might take a few minutes.
font_import()

# Load the fonts into R
loadfonts(device = "win")

## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

# Set to FALSE to avoid the creation of files locally
create_files <- TRUE



################################################################
#### Step 1: Load data

##  dataset 
setwd("C:/Users/AlbertodeTorres/OneDrive - Nektiu S.L/UOC/TFG_1/PEC 2")
library(readr)
MMM_data <- read_csv("MMM_data.csv")
#View(MMM_data)

# Convert the variable to a date format
library(dplyr)
df <- MMM_data %>% mutate(date = as.Date(wk_strt_dt, format = "%Y-%m-%d"))
#View(df)

## holidays from Prophet library 

data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory to export results 
#robyn_object <- "~/Desktop"


################################################################
#### Step 2a: For first time user: Model specification in 4 steps

#### 2a-1: First, specify input variables


df <- df[,1:35]

InputCollect <- robyn_inputs(
  dt_input = df,
  dt_holidays = dt_prophet_holidays,
  date_var = "wk_strt_dt", # date format must be "2020-01-01"
  dep_var = "sales", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend","season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "US", # input one country. dt_prophet_holidays includes 59 countries by default
  context_vars = c("mrkdn_valadd_edw", "mrkdn_pdm", 'me_ics_all','me_gas_dpg'), # e.g. competitors, discount, unemployment etc
paid_media_spends = c("mdsp_dm", "mdsp_inst", "mdsp_nsp", "mdsp_auddig", "mdsp_audtr", 'mdsp_vidtr','mdsp_viddig', 'mdsp_so', 'mdsp_on', 'mdsp_sem'), # mandatory input
  paid_media_vars = c("mdip_dm", "mdip_inst", "mdip_nsp", "mdip_auddig", "mdip_audtr", 'mdip_vidtr', 'mdip_viddig', 'mdip_so', 'mdip_on', 'mdip_sem'), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = c("mdip_em", 'mdip_sms', 'mdip_aff'), # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2014-08-03",
  window_end = "2018-07-29",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)

#### 2a-2: Second, define and add hyperparameters

## hyperparameter names are based on paid_media_spends names too.
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

## Guide to setup & understand hyperparameters

## hyperparameters have four components:
## Adstock parameters (theta or shape/scale).
## Saturation parameters (alpha/gamma).
## Regularisation parameter (lambda). No need to specify manually.
## Time series validation parameter (train_size).

## 1. IMPORTANT: set plot = TRUE to create example plots for adstock & saturation
## hyperparameters and their influence in curve transformation
plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)

## 2. Get correct hyperparameter names:
# All variables in paid_media_spends and organic_vars require hyperparameter and will be
# transformed by adstock & saturation.

## 3. Hyperparameter interpretation:

## Geometric adstock: Theta is the only parameter and means fixed decay rate. Assuming TV
# spend on day 1 is 100€ and theta = 0.7, then day 2 has 100*0.7=70€ worth of effect
# carried-over from day 1, day 3 has 70*0.7=49€ from day 2 etc. Rule-of-thumb for common
# media genre: TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3). Also,
# to convert weekly to daily we can transform the parameter to the power of (1/7),
# so to convert 30% daily to weekly is 0.3^(1/7) = 0.84.


## 4. Set individual hyperparameter bounds. They either contain two values e.g. c(0, 0.5),
# or only one value, in which case you'd "fix" that hyperparameter.
# Run hyper_limits() to check maximum upper and lower bounds by range
hyper_limits()

# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  mdip_aff_alphas = c(0.5, 3),
  mdip_aff_gammas = c(0.3, 1),
  mdip_aff_thetas = c(0.1, 0.4),
  mdip_em_alphas = c(0.5, 3),
  mdip_em_gammas = c(0.3, 1),
  mdip_em_thetas = c(0.1, 0.4),
  mdip_sms_alphas = c(0.5, 3),
  mdip_sms_gammas = c(0.3, 1),
  mdip_sms_thetas = c(0.1, 0.4),
  mdsp_auddig_alphas = c(0.5, 3),
  mdsp_auddig_gammas = c(0.3, 1),
  mdsp_auddig_thetas = c(0, 0.3),
  mdsp_audtr_alphas = c(0.5,3),
  mdsp_audtr_gammas= c(0.3,1),
  mdsp_audtr_thetas= c(0.1,0.4),
  mdsp_dm_alphas=c(0.5,3),
  mdsp_dm_gammas=c(0.3,1),
  mdsp_dm_thetas=c(0, 0.3),
  mdsp_inst_alphas = c(0.5, 3),
  mdsp_inst_gammas = c(0.3, 1),
  mdsp_inst_thetas = c(0, 0.3),
  mdsp_nsp_alphas = c(0.5, 3),
  mdsp_nsp_gammas = c(0.3, 1),
  mdsp_nsp_thetas = c(0.1, 0.4),
  mdsp_on_alphas = c(0.5, 3),
  mdsp_on_gammas = c(0.3, 1),
  mdsp_on_thetas = c(0, 0.3),
  mdsp_sem_alphas= c(0.5,3),
  mdsp_sem_gammas=c(0.3, 1),
  mdsp_sem_thetas=c(0, 0.3),
  mdsp_so_alphas=c(0.5, 3),
  mdsp_so_gammas=c(0.3, 1),
  mdsp_so_thetas=c(0,  0.3),
  mdsp_viddig_alphas = c(0.5, 3),
  mdsp_viddig_gammas = c(0.3, 1),
  mdsp_viddig_thetas = c(0.1, 0.4),
  mdsp_vidtr_alphas = c(0.5, 3),
  mdsp_vidtr_gammas = c(0.3, 1),
  mdsp_vidtr_thetas= c(0.3, 0.8),
  train_size = c(0.5, 0.8)
)


#### 2a-3: Third, add hyperparameters into robyn_inputs()

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)



#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}
# Library and environment for Reticulate/Nevergrad.
library("reticulate")
conda_create("r-reticulate")
conda_install("r-reticulate", "nevergrad", pip=TRUE)
use_condaenv("r-reticulate")


#### nevergrad installation via conda
# 1. load reticulate
library("reticulate")
# 2. Install conda if not available
#install_miniconda()
# 3. create virtual environment
conda_create("r-reticulate")
# 4. use the environment created
use_condaenv("r-reticulate")
# 5. point Python path to the python file in the virtual environment. Below is
#    an example for MacOS M1 or above. The "~" is my home dir "/Users/gufengzhou".
#    Show hidden files in case you want to locate the file yourself

Sys.setenv(RETICULATE_PYTHON = "C:/Users/alber/anaconda3/envs/r-reticulate/python.exe")
# 6. Check python path
py_config() # If the first path is not as 5, do 7
# 7. Restart R session, run #5 first, then load library("reticulate"), check
#    py_config() again, python should have path as in #5
# 8. Install numpy if py_config shows it's not available
conda_install("r-reticulate", "numpy", pip=TRUE)
# 9. Install nevergrad
conda_install("r-reticulate", "nevergrad", pip=TRUE)
# 10. If successful, py_config() should show numpy and nevergrad with installed paths
# 11. Everytime R session is restarted, you need to run #4 first to assign python
#    path before loading Robyn
py_config()


################################################################
#### Step 3: Build initial model

## Run all trials and iterations. 
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  #plot_folder = robyn_object, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)


################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "1_256_7" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)



################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition

# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends

# Scenario "max_historical_response": "What's the potential revenue/conversions lift with the
# same spend level in date_range and what is the spend and expected response mix?"
# For this scenario, we have several use cases:

# Case 1: date_range & total_budget both NULL (default for last month's spend)
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = NULL, # When NULL, will set last month (30 days, 4 weeks, or 1 month)
  channel_constr_low = 0.7,
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 3,
  scenario = "max_response_expected_spend",
  export = create_files
)
# Print the allocator's output summary
print(AllocatorCollect1)
# Plot the allocator one-pager
plot(AllocatorCollect1)

# Case 2: date_range defined, total_budget NULL (mean spend of date_range as initial spend)
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_26", # Last 26 periods, same as c("2018-07-09", "2018-12-31")
  channel_constr_low = c(0.8, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 3,
  scenario = "max_historical_response",
  export = create_files
)
print(AllocatorCollect2)
plot(AllocatorCollect2)

# Case 3: date_range (defined or not defined) and total_budget defined
AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = "last_4",
  total_budget = 5000000,
  channel_constr_low = 0.7,
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 5,
  scenario = "max_historical_response",
  export = create_files
)
print(AllocatorCollect3)
plot(AllocatorCollect3)

## A csv is exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R

## QA optimal response
# Pick any media variable: InputCollect$all_media
select_media <- "search_S"
# For paid_media_spends set metric_value as your optimal spend
metric_value <- AllocatorCollect1$dt_optimOut$optmSpendUnit[
  AllocatorCollect1$dt_optimOut$channels == select_media
]; metric_value
# # For paid_media_vars and organic_vars, manually pick a value
# metric_value <- 10000

## Saturation curve for adstocked metric results (example)
robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = select_media,
  metric_value = metric_value,
  metric_ds = "last_5"
)

################################################################
#### Step 6: Model refresh based on selected model and saved results

## Must run robyn_write() (manually or automatically) to export any model first, before refreshing.
## The robyn_refresh() function is suitable for updating within "reasonable periods".
## Two situations are considered better to rebuild model:
## 1. most data is new. If initial model has 100 weeks and 80 weeks new data is added in refresh,
## it might be better to rebuild the model. Rule of thumb: 50% of data or less can be new.
## 2. new variables are added.

# Provide JSON file with your InputCollect and ExportedModel specifications
# It can be any model, initial or a refresh model
json_file <- "~/Desktop/Robyn_202211211853_init/RobynModel-1_100_6.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 13,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)
# Now refreshing a refreshed model, following the same approach
json_file_rf1 <- "~/Desktop/Robyn_202208231837_init/Robyn_202208231841_rf1/RobynModel-1_12_5.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file_rf1,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 7,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)

# Continue with refreshed new InputCollect, OutputCollect, select_model values
InputCollectX <- RobynRefresh$listRefresh1$InputCollect
OutputCollectX <- RobynRefresh$listRefresh1$OutputCollect
select_modelX <- RobynRefresh$listRefresh1$OutputCollect$selectID

###### DEPRECATED (<3.7.1) (might work)
# # Run ?robyn_refresh to check parameter definition
# Robyn <- robyn_refresh(
#   robyn_object = robyn_object,
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   refresh_steps = 4,
#   refresh_mode = "manual",
#   refresh_iters = 1000, # 1k is estimation. Use refresh_mode = "manual" to try out.
#   refresh_trials = 1
# )

## Besides plots: there are 4 CSV outputs saved in the folder for further usage
# report_hyperparameters.csv, hyperparameters of all selected model for reporting
# report_aggregated.csv, aggregated decomposition per independent variable
# report_media_transform_matrix.csv, all media transformation vectors
# report_alldecomp_matrix.csv,all decomposition vectors of independent variables

################################################################
#### Step 7: Get budget allocation recommendation based on selected refresh runs

# Run ?robyn_allocator to check parameter definition
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  expected_spend = 2000000, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = FALSE
)
print(AllocatorCollect)
# plot(AllocatorCollect)

################################################################
#### Step 8: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, newsletter sendings etc.) as well as plotting individual saturation curves. New
## argument names "metric_name" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.
## ------------------------------------------------------------------------------------------ ##

# Get response for 80k from result saved in robyn_object
Spend1 <- 60000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_S",
  metric_value = c(Spend1),
  # date_range = "last_2"
)
Response1$response / Spend1 # ROI for search 80k
Response1$plot

#### Or you can call a JSON file directly (a bit slower)
# Response1 <- robyn_response(
#   json_file = json_file,
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   metric_name = "search_S",
#   metric_value = Spend1,
#   metric_ds = NULL
# )

# Get response for +10%
Spend2 <- Spend1 * 1.1
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "search_S",
  metric_value = Spend2,
  metric_ds = NULL
)
Response2$response / Spend2 # ROI for search 81k
Response2$plot

# Marginal ROI of next 1000$ from 80k spend level for search
(Response2$response - Response1$response) / (Spend2 - Spend1)

## Example of getting paid media exposure response curves
imps <- 50000000
response_imps <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_I",
  metric_value = imps,
  metric_ds = NULL
)
response_imps$response / imps * 1000
response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "newsletter",
  metric_value = sendings,
  metric_ds = NULL
)
response_sending$response / sendings * 1000
response_sending$plot

################################################################
#### Optional: recreate old models and replicate results [v3.7.1]

# From an exported JSON file (which is created automatically when exporting a model)
# we can re-create a previously trained model and outputs. Note: we need to provide
# the main dataset and the holidays dataset, which are NOT stored in the JSON file.
# These JSON files will be automatically created in most cases.

############ WRITE ############
# Manually create JSON file with inputs data only
robyn_write(InputCollect, dir = "~/Desktop")

# Manually create JSON file with inputs and specific model results
robyn_write(InputCollect, OutputCollect, select_model)


############ READ ############
# Recreate `InputCollect` and `OutputCollect` objects
# Pick any exported model (initial or refreshed)
json_file <- "~/Desktop/Robyn_202208231837_init/RobynModel-1_100_6.json"

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file,
  export = create_files)

# Or re-create both by simply using robyn_recreate()
RobynRecreated <- robyn_recreate(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)
InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

# Re-export model and check summary (will get exported in your current working directory)
myModel <- robyn_write(InputCollectX, OutputCollectX, dir = "~/Desktop")
print(myModel)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX, OutputCollectX, select_model = NULL, export = create_files)
# myModelPlot$`1_204_5`$patches$plots[[6]]

# Refresh any imported model
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = InputCollectX$dt_input,
  dt_holidays = InputCollectX$dt_holidays,
  refresh_steps = 6,
  refresh_mode = "manual",
  refresh_iters = 1000,
  refresh_trials = 1
)

# Recreate response curves
robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  metric_name = "newsletter",
  metric_value = 50000
)