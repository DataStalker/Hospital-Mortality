# Load required libraries
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(mice)

# Set options
options(tibble.print_max = 250)

# Step 1: Load Data
data <- read_csv("dataset.csv")

# Check data
glimpse(data)
summary(data)

# Step 2: Handle Missing Values
# Identify features with excessive missing values
missing_counts <- colSums(is.na(data))
large_missing <- missing_counts[missing_counts > 25000]

# Remove columns with excessive missing values and irrelevant columns
data <- data %>%
  select(-c(large_missing %>% names(), 'encounter_id', 'icu_admit_source', 'icu_id', 'icu_stay_type', 
            'patient_id', 'hospital_id'))

# Drop rows with missing values in 'bmi', 'weight', and 'height'
data <- data %>%
  filter(!is.na(bmi) & !is.na(weight) & !is.na(height))

# Step 3: Exploratory Data Analysis (EDA)

# Histogram of Age and Hospital Death by Gender
ggplot(data %>% drop_na(age, gender, hospital_death), aes(x = age, fill = as.factor(hospital_death))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~ gender) +
  labs(title = "Histogram of Age and Hospital Death by Gender", x = "Age", fill = "Hospital Death") +
  theme_minimal()

# Average Hospital Death Probability by Age and Gender
age_death <- data %>%
  group_by(age, gender) %>%
  summarize(hospital_death = mean(hospital_death, na.rm = TRUE))

ggplot(age_death, aes(x = age, y = hospital_death, color = gender)) +
  geom_line() +
  labs(title = "Average Hospital Death Probability by Age and Gender", x = "Age", y = "Average Hospital Death") +
  theme_minimal()

# Impacts of BMI and Weight on Hospital Death
weight_death <- data %>%
  group_by(weight = round(weight, 0)) %>%
  summarize(hospital_death = mean(hospital_death, na.rm = TRUE))

bmi_death <- data %>%
  group_by(bmi = round(bmi, 0)) %>%
  summarize(hospital_death = mean(hospital_death, na.rm = TRUE))

p1 <- ggplot(weight_death, aes(x = weight, y = hospital_death)) +
  geom_line() +
  labs(title = "Impact of Weight on Hospital Death", x = "Weight", y = "Average Hospital Death") +
  theme_minimal()

p2 <- ggplot(bmi_death, aes(x = bmi, y = hospital_death)) +
  geom_line() +
  labs(title = "Impact of BMI on Hospital Death", x = "BMI", y = "Average Hospital Death") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

# Survival Rate at Different ICU Types
ICU_type <- data %>%
  mutate(icu_type = recode(icu_type, 
                           'CTICU' = 'CCU-CTICU',
                           'Cardiac ICU' = 'CCT-CTICU',
                           'CSICU' = 'SICU')) %>%
  group_by(icu_type, age) %>%
  summarize(hospital_death = mean(hospital_death, na.rm = TRUE))

ggplot(ICU_type, aes(x = age, y = hospital_death, color = icu_type)) +
  geom_point() +
  labs(title = "Survival Rate at Different ICU Types", x = "Age", y = "Average Hospital Death") +
  theme_minimal()

# Survival Rate by Apache 3J Body System
apache3 <- data %>%
  group_by(apache_3j_bodysystem, age) %>%
  summarize(size = n(), mean_death = mean(hospital_death, na.rm = TRUE)) %>%
  ungroup()

ggplot(apache3, aes(x = age, y = mean_death, size = size, color = apache_3j_bodysystem)) +
  geom_point(alpha = 0.7) +
  labs(title = "Survival Rate by Apache 3J Body System", x = "Age", y = "Average Hospital Death") +
  theme_minimal()

# Step 4: Data Preprocessing for Modeling
convert_to_integer <- c('elective_surgery', 'apache_post_operative', 'arf_apache', 'gcs_unable_apache',
                        'intubated_apache', 'ventilated_apache', 'aids', 'cirrhosis', 'diabetes_mellitus',
                        'hepatic_failure', 'immunosuppression', 'leukemia', 'lymphoma', 'solid_tumor_with_metastasis')

data <- data %>%
  mutate(across(all_of(convert_to_integer), as.integer),
         hospital_death = factor(hospital_death, levels = c(0, 1)))

# Step 5: Split Data
set.seed(42)
split <- initial_split(data, prop = 0.75)
train_data <- training(split)
test_data <- testing(split)

# Step 6: Create a Preprocessing Recipe
rec <- recipe(hospital_death ~ ., data = train_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%  # Handle new levels in categorical variables
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%  # Remove zero-variance columns
  step_normalize(all_numeric_predictors())

# Step 7: Model Definition and Training
model <- logistic_reg() %>%
  set_engine("glm")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model)

fit <- fit(wf, data = train_data)

# Step 8: Model Evaluation
results <- fit %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = hospital_death, estimate = .pred_class)

cat("Model: Logistic Regression\n")
cat("Accuracy:", results %>% filter(.metric == "accuracy") %>% pull(.estimate), "\n")

# Step 9: Prediction for a Specific Patient
row_index <- 91611
dataset_row <- data[row_index, ] %>% select(-hospital_death)

prediction <- predict(fit, new_data = dataset_row)

cat("\n------------------------------------\n")
if (prediction$.pred_class == 0) {
  cat("\tPatient Death\n")
} else {
  cat("\tPatient Survived\n")
}
cat("\n------------------------------------\n")
```

