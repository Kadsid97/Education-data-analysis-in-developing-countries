# ------------------------------------------------------
# ---------------- DATA CLEANING --------------
# ------------------------------------------------------

# This script will be about cleaning the datasets based on the DQA knowing that 
# we already dealt with data type conversions otherwise it would have complicated
# the data quality checks. The plan is as follow: 
### 1) HANDLING MISSING VALUES 
### 2) HANDLING DUPLICATES (making sure key idenfitifiers are unique and consistent)
### 3) HANDLING RANGE INCONSISTENCIES
### 4) HANDLING OUTLIERS
### 5) SAMPLE SELECTION AND PREPARING DATA FOR ANALYSES
### 6) SAVING THE CLEAN dataset


# ------------------------------------------------------


# Using study_sample_with_conversion from Script 02 to create another table (to not overwrite on the raw version)
pr_hr_clean <- pr_hr_with_conversion

# ---------------------#

### 1) HANDLING MISSING VALUES

# ---------------------#

# I am leaving the NAs in hv107 and hv140 for now. In DHS, these are often missing 
# because of skip patterns
# Since the missingness is likely due to the survey design, I won't do imputation

# ---------------------#

### 2) HANDLING DUPLICATES

# ---------------------#

# DQA confirmed no duplicate issues for the key identifiers.



# ---------------------#

### 3) HANDLING RANGE INCONSISTENCIES

# ---------------------#

# We want to clean numeric variables but EXCLUDE survey design/ID variables 
# To remain consistent, we use the same list as in the DQA script:
# * We will replace special codes (98, 99, etc.) with NA.
# *  We handle values that are outside our plausible ranges


### A) Handle Special Codes (Missing/Don't Know) 

# We loop through my 'special_codes' list
for (var_name in names(special_codes)) {
  if (var_name %in% names(pr_hr_clean)) {
    codes_to_na <- special_codes[[var_name]]
    
    # We replace these specific codes with NA
    pr_hr_clean[[var_name]][pr_hr_clean[[var_name]] %in% codes_to_na] <- NA
  }
}

# B) Handle Numeric Range Violations
# We loop through my 'num_range_constraints' list

for (var_name in names(num_range_constraints)) {
  if (var_name %in% names(pr_hr_clean)) {
    limits <- num_range_constraints[[var_name]]
    
    if (!(var_name %in% c("hhid", "hv001", "hv002", "hv021", "hv024"))) {
      
      # We convert to numeric to avoid the "factor" error message
      # This allows us to compare values like age or years of education
      current_values <- as.numeric(as.character(pr_hr_clean[[var_name]]))
      
      # Identify where the range is violated
      out_of_range <- which(current_values < limits[1] | current_values > limits[2])
      
      # Replace only those specific rows with NA
      pr_hr_clean[[var_name]][out_of_range] <- NA
    }
  }
}
# Note: For hv140 and hv201, we saw in DQA that 0 and 14 are valid for CI.
# Because they are in our 'categorical_constraints' list, they are not changed here.


# Quick check to compare missingness before and after cleaning

missingness_comparison <- tibble(
  variable = names(pr_hr_with_conversion),
  na_before = colSums(is.na(pr_hr_with_conversion)),
  na_after = colSums(is.na(pr_hr_clean))
) %>%
  filter(na_before > 0 | na_after > 0) %>%
  mutate(difference = na_after - na_before)

print(missingness_comparison)


# ---------------------#

### 4) HANDLING OUTLIERS

# ---------------------#

# Outliers should not be automatically removed or replaced. It really depends on 
# the context. 
# Based on the DQA outliers_records, most extreme values are plausible for the 
# Côte d'Ivoire context (e.g., large households).# #


# No more cleaning 

# ---------------------#

### 5) SAMPLE SELECTION AND PREPARING DATA FOR ANALYSES

# ---------------------#

# Let's create the variables for the regressions I will run in the data analysis 
# R script. 
# I am focusing on Preschool and Primary 



# Storing age interval info using UNESCO and +/- years buffer)

age_interv <- list(
  Preschool = 2:6,
  Primary = 4:13,
  Lower_secondary = 10:16,
  Upper_secondary = 13:19
)

# Create a new variable 'education_level' that I will use to select my sample and 
# assign levels (the goal no code change even if I add new countries, considers
#early/late starters and repetition)

pr_hr_clean <- pr_hr_clean %>%
  mutate(
    education_level = case_when(
      hv105 %in% age_interv$Preschool ~ "Preschool",
      hv105 %in% age_interv$Primary ~ "Primary",
      hv105 %in% age_interv$Lower_secondary ~ "Lower Secondary",
      hv105 %in% age_interv$Upper_secondary ~ "Upper Secondary",
      TRUE ~ "Other"
    )
  )

# Quick check
table(pr_hr_clean$education_level)

#Selecting the study sample (preschool and primary)

study_sample <- pr_hr_clean %>%
  filter(education_level %in% c("Preschool", "Primary")) %>%
  mutate(weight = hv005 / 1000000) # re-scaling the DHS weight to its 
# proper numeric scale (cf DHS convention to store weights as integers to avoid 
# decimals in the data files)


# Now I create the age_gap indicator for this specific sample bearing in 
# my mind that in most African countries, the official entry age is 6

# First, check if hv108 has the numbers we need
table(study_sample$hv108, useNA = "always") # it has!!

# Second given that it has numbers from 0 to 18 let's create the numeric version 
# of the years of education with this simple logic: If a 10-year-old has only 2 
# years of school, they are late. Which is equivalent to this formula : 
# Formula: (Age - 6) > Years Completed

# study_sample_clean <- study_sample %>%
#   mutate(
#     educ_years = as.numeric(as.character(hv108)),
#     
#     age_gap = case_when(
#       hv105 < 6 ~ 0,
#       is.na(hv105) | is.na(educ_years) ~ NA_real_,
#       (hv105 - 6) > educ_years ~ 1,
#       TRUE ~ 0
#     )
#   )

study_sample <- study_sample %>%
  mutate(
    educ_years = as.numeric(as.character(hv108)),
    age_gap = ifelse((hv105 - 6) > educ_years, 1, 0)
  )

# Let's check the result
table(study_sample$age_gap, useNA = "always")



### Creating simple indicators for the regression models

study_sample_clean <- study_sample %>%
  mutate(
    # dummy var for sex -> 1 for female
    female  = ifelse(hv104 == "female", 1, 0),
    
    # dummy for type of place (Urban/Rural) -> 1 for urban
    urban   = ifelse(hv025 == "urban", 1, 0),
    
    # Wealth -> grouping the bottom two quintiles (poor/poorest)
    poor_hh = ifelse(hv270 %in% c("poorest", "poorer"), 1, 0)
  )

# Checking if the recoding worked
table(study_sample_clean$hv104, study_sample_clean$female)
table(study_sample_clean$hv025, study_sample_clean$urban)
table(study_sample_clean$hv270, study_sample_clean$poor_hh)
                              

### Code to print pretty country names in the visuals


format_country_name <- function(country) {
  # replace underscores with spaces
  name <- gsub("_", " ", country)
  
  # replace special cases
  # update gradually if needed
  name <- ifelse(tolower(name) == "cote d ivoire", "Côte d'Ivoire", name)
  
  return(name)
}

country_display <- format_country_name(country)
country_display


# ---------------------#

### 6) SAVE THE FULLY CLEANED DATASET

# ---------------------#
write_csv(study_sample_clean, file.path(path_dhs, "dhs", country,"02_Clean", paste0(country, "_study_sample_clean.csv")))
 


message("Data Cleaning script completed")