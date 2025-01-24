data<-data_dengue
library(readxl)
Mental_health_in_HCW_in_dengue_outbreak_Responses_ <- read_excel("Mental health in HCW in dengue outbreak (Responses).xlsx")
View(Mental_health_in_HCW_in_dengue_outbreak_Responses_)
data<-Mental_health_in_HCW_in_dengue_outbreak_Responses_
#view variables 
names(data)
str(data)
install.packages("Hmisc")  # If not already installed
library(Hmisc)
# Create a copy of your dataset
data_mh<-Mental_health_in_HCW_in_dengue_outbreak_Responses_
data_mh <- data_mh
remove(data)
remove(data_dengue)
data<-data_dengue
library(tidyverse)
# Shorten variable names
names(data) <- c(
  "timestamp", "age", "gender", "work_hours", "qualification", 
  "dept", "marital_status", "location", "work_location", "dengue_cases",
  "relax", "dry_mouth", "no_positive", "breath_difficulty", 
  "low_initiative", "overreact", "trembling", "nervous_energy", 
  "panic_worry", "no_look_forward", "agitated", "difficult_relax",
  "downhearted", "intolerant", "close_panic", "no_enthusiasm", 
  "low_self_worth", "touchy", "heart_sensation", "scared", "life_meaningless",
  "emotional_drain", "effort_work", "work_break", "frustrated_work",
  "work_hard", "stress_contact", "end_rope", "worthwhile",
  "full_energy", "understand_patients", "effective_problem", "calm_emotions",
  "positive_influence", "relaxed_atmosphere", "refreshed_work",
  "objectify_patients", "tired_morning", "responsible_problems",
  "end_patience", "careless_patients", "insensitive_work", "uncaring_job"
)

# Check updated names
names(data)
# Assign labels (original variable names)

label(data$age) <- "Age in years"
label(data$gender) <- "Gender"
label(data$work_hours) <- "Working hours per week"
label(data$qualification) <- "Qualification"
label(data$dept) <- "Working department"
label(data$marital_status) <- "Marital status"
label(data$location) <- "Living in home or hostel?"
label(data$work_location) <- "Working location"
label(data$dengue_cases) <- "Dengue cases seen per day"
label(data$relax) <- "I find hard to wind down( to relax)"
label(data$dry_mouth) <- "I was aware of dryness of my mouth"
label(data$no_positive) <- "I couldn't seem to experience any positive feeling at all"
label(data$breath_difficulty) <- "I experienced breathing difficulty (excessive rapid feeling or breathlessness in absence of physical activity)"
label(data$low_initiative) <- "I found it difficult to work up the initiative to do things"
label(data$overreact) <- "I tended to over-react to situations"
label(data$trembling) <- "I experienced trembling (e.g., in the considerable)"
label(data$nervous_energy) <- "I felt that I was using a lot of nervous energy"
label(data$panic_worry) <- "I was worried about situations in which I might panic and make a fool of myself"
label(data$no_look_forward) <- "I felt that I had nothing to look forward to"
label(data$agitated) <- "I found myself getting agitated"
label(data$difficult_relax) <- "I found it difficult to relax"
label(data$downhearted) <- "I felt down-hearted and blue"
label(data$intolerant) <- "I was intolerant of anything that kept me from getting on with what I was doing"
label(data$close_panic) <- "I felt I was close to panic"
label(data$no_enthusiasm) <- "I am unable to become enthusiastic about anything"
label(data$low_self_worth) <- "I felt I wasn’t worth much as a person"
label(data$touchy) <- "I felt that I was rather touchy"
label(data$heart_sensation) <- "[I was unaware of the actions of my heart in the absence of physical exertion (e.g., sense of heart rate increase, heart skipping a beat)]"
label(data$scared) <- "I felt scared without any good reason"
label(data$life_meaningless) <- "I felt that life is meaningless]"
label(data$emotional_drain) <- " sectionA-I feel emotionally drained by my work"
label(data$effort_work) <- "SECTION A [Working with people all day long requires a great deal of effort]"
label(data$work_break) <- "SECTION A [I feel like my work is breaking me down]"
label(data$frustrated_work) <- "SECTION A [I feel frustrated by my work]"
label(data$work_hard) <- "SECTION A [I feel I work too hard at my job]"
label(data$stress_contact) <- "SECTION A [It stresses me too much to work in direct contact with people]"
label(data$end_rope) <- "SECTION A [I feel I am at the end of my rope]"
label(data$worthwhile) <- "[I accomplish many worthwhile things in this job]"
label(data$full_energy) <- "[I feel full of energy]"
label(data$understand_patients) <- "[I am easily able to understand what my patients feel]"
label(data$effective_problem) <- "[I look after my patients' problems effectively]"
label(data$calm_emotions) <- "[In my work, I handle emotional problems very calmly]"
label(data$positive_influence) <- "[Through my work, I feel that I have a positive influence on people]"
label(data$relaxed_atmosphere) <- "[I am easily able to create a relaxed atmosphere with my patients]"
label(data$refreshed_work) <- "[I feel refreshed when I have been close to my patients at work]"
label(data$objectify_patients) <- "[I feel I look after certain patients impersonally as if they are objects]"
label(data$tired_morning) <- "[I feel tired when I get up in the morning and have to face another day at work]"
label(data$responsible_problems) <- "[I have the impression that my patients make me responsible for some of their problems]"
label(data$end_patience) <- "[I am at the end of my patience at the end of my day]"
label(data$careless_patients) <- "[I really don't care about what happens to some of my patients]"
label(data$insensitive_work) <- "[I have become more insensitive to people since I started working]"
label(data$uncaring_job) <- "[I am afraid that this job is making me uncaring]"

str(data)


# Check the updated dataset
tail(data)

library(gtsummary)
library(dplyr)
# Example descriptive table
tbl_summary(data,
            by = gender,  # Group results by gender (or any other grouping variable)
            statistic = list(all_continuous() ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)"),
            missing = "no") %>%  # Exclude missing values
  add_p() %>%               # Add p-values for comparisons
  add_overall() %>%         # Include overall summary column
  bold_labels()             # Bold variable labels
#########
###############Coding the likert scale ##############
table(data$emotional_drain)
# Load necessary library
library(dplyr)
remove(likert_mapping)
# Define the mapping of text to numeric
likert_mapping <- c(
  "never" = 0,
  "few times per year" = 1,
  "once a month" = 2,
  "few times per month" = 3,
  "once a week" = 4,
  "few times a week" = 5,
  "every day" = 6
)

# Correct the recode statement
data$emotional_drain<- recode(data$emotional_drain,
                               !!!likert_mapping,
                               .default = NA_real_)
data$effort_work<- recode(data$effort_work,
                             !!!likert_mapping,
                             .default = NA_real_)

data$work_break<- recode(data$work_break,
                              !!!likert_mapping,
                              .default = NA_real_)
data$frustrated_work<- recode(data$frustrated_work,
                         !!!likert_mapping,
                         .default = NA_real_)

data$work_hard<- recode(data$work_hard,
                         !!!likert_mapping,
                         .default = NA_real_)

data$stress_contact<- recode(data$stress_contact,
                         !!!likert_mapping,
                         .default = NA_real_)
data$end_rope<- recode(data$end_rope,
                             !!!likert_mapping,
                             .default = NA_real_)
data$worthwhile<- recode(data$worthwhile,
                             !!!likert_mapping,
                             .default = NA_real_)

data$full_energy<- recode(data$full_energy,
                             !!!likert_mapping,
                             .default = NA_real_)

data$understand_patients<- recode(data$understand_patients,
                             !!!likert_mapping,
                             .default = NA_real_)
data$effective_problem<- recode(data$effective_problem,
                             !!!likert_mapping,
                             .default = NA_real_)

data$calm_emotions<- recode(data$calm_emotions,
                             !!!likert_mapping,
                             .default = NA_real_)

data$positive_influence<- recode(data$positive_influence,
                             !!!likert_mapping,
                             .default = NA_real_)
data$relaxed_atmosphere<- recode(data$relaxed_atmosphere,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$refreshed_work<- recode(data$refreshed_work,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$objectify_patients<- recode(data$objectify_patients,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$tired_morning<- recode(data$tired_morning,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$responsible_problems<- recode(data$responsible_problems,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$end_patience<- recode(data$end_patience,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$careless_patients<- recode(data$careless_patients,
                                 !!!likert_mapping,
                                 .default = NA_real_)
data$insensitive_work<- recode(data$insensitive_work,
                                !!!likert_mapping,
                                .default = NA_real_)
data$uncaring_job<- recode(data$uncaring_job,
                                !!!likert_mapping,
                                .default = NA_real_)
####################################################
names(data)
####constructs measurement Emotional_Exhaustion
# Ensure the variables are numeric
data$Emotional_Exhaustion <- rowSums(data[, c("emotional_drain", "effort_work", 
                                              "work_break", "frustrated_work", 
                                              "work_hard", "stress_contact", 
                                              "end_rope")], na.rm = TRUE)

# Ensure the variables are numeric and calculate row-wise sum. (Personal_Accomplishment)
data$Personal_Accomplishment <- rowSums(data[, c("worthwhile", "full_energy", 
                                                 "understand_patients", 
                                                 "effective_problem", 
                                                 "calm_emotions", 
                                                 "positive_influence", 
                                                 "relaxed_atmosphere", 
                                                 "refreshed_work")], na.rm = TRUE)
# Ensure the variables are numeric and calculate row-wise sum
data$Depersonalization <- rowSums(data[, c("objectify_patients", "tired_morning", 
                                           "responsible_problems", "end_patience", 
                                           "careless_patients", "insensitive_work", 
                                           "uncaring_job")], na.rm = TRUE)
# Ensure the variables are numeric and calculate row-wise sum
data$Stress <- rowSums(data[, c("relax", "dry_mouth", "no_positive", 
                                "breath_difficulty", "low_initiative", 
                                "overreact", "trembling")], na.rm = TRUE)
# Ensure the variables are numeric and calculate row-wise sum
data$Anxiety <- rowSums(data[, c("nervous_energy", "panic_worry", 
                                 "no_look_forward", "agitated", 
                                 "difficult_relax")], na.rm = TRUE)

# Ensure the variables are numeric and calculate row-wise sum
data$Depression <- rowSums(data[, c("downhearted", "intolerant", "close_panic", 
                                    "no_enthusiasm", "low_self_worth", "touchy", 
                                    "heart_sensation", "scared", "life_meaningless")], na.rm = TRUE)
data$EE_Level <- cut(data$Emotional_Exhaustion, 
                     breaks = c(-Inf, 16, 26, Inf), 
                     labels = c("Low", "Moderate", "High"))

data$PA_Level <- cut(data$Personal_Accomplishment, 
                     breaks = c(-Inf, 31, 36, Inf), 
                     labels = c("Low", "Moderate", "High"))

data$DP_Level <- cut(data$Depersonalization, 
                     breaks = c(-Inf, 6, 12, Inf), 
                     labels = c("Low", "Moderate", "High"))
##############################
# Convert specified variables to factors
data$age <- as.factor(data$age)
data$gender <- as.factor(data$gender)
data$work_hours <- as.factor(data$work_hours)
data$qualification <- as.factor(data$qualification)
data$dept <- as.factor(data$dept)
data$marital_status <- as.factor(data$marital_status)
data$location <- as.factor(data$location)
data$work_location <- as.factor(data$work_location)
data$dengue_cases <- as.factor(data$dengue_cases)


# Load the gtsummary package
library(gtsummary)

# Create a summary table for selected variables
summary_table <- data %>%
  select(age, gender, work_hours, qualification, dept, marital_status, Emotional_Exhaustion, Personal_Accomplishment, Depersonalization) %>%
  tbl_summary(
    by = gender,  # Optional: Group the summary by gender
    statistic = list(all_continuous() ~ "{mean} ({sd})",  # Display mean and standard deviation for continuous variables
                     all_categorical() ~ "{n} ({p}%)"),    # Display counts and percentages for categorical variables
    missing = "ifany"                                     # Show missing data if present
  ) %>%
  add_p()                                                # Add p-values for group comparisons (optional)

summary_table
# Load the gtsummary package
library(gtsummary)

# Create a basic summary table
simple_summary <- data %>%
  select(age, gender, work_hours, qualification, dept, marital_status, Emotional_Exhaustion, Personal_Accomplishment, Depersonalization) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",  # Mean and standard deviation for continuous variables
                     all_categorical() ~ "{n} ({p}%)"),  # Counts and percentages for categorical variables
    missing = "ifany"                                    # Show missing data if present
  )
simple_summary
# Load the gtsummary package
library(gtsummary)

# Load necessary libraries
library(gtsummary)
library(dplyr)

# Calculate mean, SD, and confidence intervals for the selected variables
summary_data <- data %>%
  summarise(
    Emotional_Exhaustion_Mean = mean(Emotional_Exhaustion, na.rm = TRUE),
    Emotional_Exhaustion_SD = sd(Emotional_Exhaustion, na.rm = TRUE),
    Emotional_Exhaustion_CI_Low = mean(Emotional_Exhaustion, na.rm = TRUE) - 1.96 * sd(Emotional_Exhaustion, na.rm = TRUE) / sqrt(n()),
    Emotional_Exhaustion_CI_High = mean(Emotional_Exhaustion, na.rm = TRUE) + 1.96 * sd(Emotional_Exhaustion, na.rm = TRUE) / sqrt(n()),
    
    Personal_Accomplishment_Mean = mean(Personal_Accomplishment, na.rm = TRUE),
    Personal_Accomplishment_SD = sd(Personal_Accomplishment, na.rm = TRUE),
    Personal_Accomplishment_CI_Low = mean(Personal_Accomplishment, na.rm = TRUE) - 1.96 * sd(Personal_Accomplishment, na.rm = TRUE) / sqrt(n()),
    Personal_Accomplishment_CI_High = mean(Personal_Accomplishment, na.rm = TRUE) + 1.96 * sd(Personal_Accomplishment, na.rm = TRUE) / sqrt(n()),
    
    Depersonalization_Mean = mean(Depersonalization, na.rm = TRUE),
    Depersonalization_SD = sd(Depersonalization, na.rm = TRUE),
    Depersonalization_CI_Low = mean(Depersonalization, na.rm = TRUE) - 1.96 * sd(Depersonalization, na.rm = TRUE) / sqrt(n()),
    Depersonalization_CI_High = mean(Depersonalization, na.rm = TRUE) + 1.96 * sd(Depersonalization, na.rm = TRUE) / sqrt(n())
  )
library(tidyverse)

# Create a data frame for gtsummary
summary_table <- summary_data %>%
  pivot_longer(cols = everything(), names_to = "Measure", values_to = "Value")

# Create the gtsummary table
burnout_summary <- summary_table %>%
  tbl_summary(
    by = NULL,  # No grouping
    statistic = all_continuous() ~ "{value}",
    missing = "no"
  ) %>%
  modify_header(label = "**Burnout Measures**")
# Load gt package
library(gt)

# Create a simple table for pre-calculated summaries
burnout_summary <- summary_data %>%
  t() %>%                            # Transpose the data for easier formatting
  as.data.frame() %>%
  rownames_to_column(var = "Measure") %>%
  rename(Value = V1) %>%             # Rename the column for clarity
  gt() %>%
  tab_header(
    title = "Burnout Measures Summary",
    subtitle = "Mean, SD, and 95% Confidence Intervals"
  ) %>%
  cols_label(
    Measure = "**Burnout Measure**",
    Value = "**Value**"
  )
burnout_summary
# Load the gtsummary package
library(gtsummary)

# Create a summary table for the categorical burnout levels
categorical_summary <- data %>%
  select(EE_Level, PA_Level, DP_Level) %>%  # Select the categorical variables
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",  # Show counts and percentages
    missing = "ifany"                             # Display missing data if present
  ) %>%
  modify_header(label = "**Burnout Levels**")      # Add a custom header for the table
categorical_summary
names(data)

# Load required package
library(gt)
library(tidyverse)

# Create the summary table (assuming `summary_df` is your data frame of results)
# Load required package
library(gt)

# Ensure column names are properly sanitized and referenced
colnames(summary_df) <- make.names(colnames(summary_df))

# Create the summary table using gt
summary_gt <- summary_df %>%
  gt() %>%
  tab_header(
    title = "Summary Table of Variables by High Burnout",
    subtitle = "P-values for Categorical and Numeric Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Statistic_Type = "Statistic Type",
    p.value = "P-value"
  ) %>%
  fmt_number(
    columns = vars(p.value),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(p.value),
      rows = p.value < 0.05
    )
  ) %>%
  tab_footnote(
    footnote = "Bold p-values indicate significance at p < 0.05.",
    locations = cells_body(
      columns = vars(p.value),
      rows = p.value < 0.05
    )
  )

# Print the table
summary_gt
# Load the required library
library(gtsummary)

# Select demographic variables and High_Burnout for analysis
demographic_vars <- c("age", "gender", "work_hours", "qualification", 
                      "dept", "marital_status", "location", 
                      "work_location", "dengue_cases" )

# Create a summary table using tbl_summary
summary_table <- data %>%
  select(all_of(demographic_vars), High_Burnout) %>%
  tbl_summary(
    by = High_Burnout, # Group by High_Burnout
    missing = "ifany", # Include missing values if any
    percent = "row"    # Show percentages within each row
  ) %>%
  add_p() %>%          # Add p-values for comparisons
  modify_header(label ~ "**Variable**") %>% # Rename the label column
  bold_labels()        # Bold the variable names

# Display the table
summary_table

# Load the required library
library(gtsummary)

# Select demographic and numeric variables for analysis
variables <- c("age", "gender", "work_hours", "qualification", "dept",
               "marital_status", "location", "work_location", "dengue_cases",
               "Stress", "Anxiety", "Depression")

# Create a summary table using tbl_summary
summary_table <- data %>%
  select(all_of(variables), High_Burnout) %>%
  tbl_summary(
    by = High_Burnout,          # Group by High_Burnout
    missing = "ifany",          # Include missing values if any
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Frequency and percentage for categorical variables
      all_continuous() ~ "{mean} ({sd})" # Mean and standard deviation for numeric variables
    ),
    digits = all_continuous() ~ 2        # Format numeric variables to 2 decimal places
  ) %>%
  add_p() %>%                           # Add p-values for comparisons
  modify_header(label ~ "**Variable**") %>%  # Rename the label column
  bold_labels()                          # Bold the variable names

# Display the table
summary_table
names(data)
# Create High_Burnout variable
data$High_Burnout <- with(data, ifelse(
  EE_Level == "High" & DP_Level == "High" & PA_Level == "Low",
  "Yes",
  "No"
))

# Check the distribution of High_Burnout
table(data$High_Burnout)

str(data)
