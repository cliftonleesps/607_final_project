# Libraries
library(tidyverse)

library(DiceDesign)
library(tidymodels)
library(workflows)
library(tune)
library(mlbench)
library(rsample)
library(recipes)
library(parsnip)
library(yardstick)
library(tm)
library(tidytext)

start_time <- Sys.time()


# Load data
accounting_programs <- read_csv("https://github.com/cliftonleesps/607_final_project/blob/master/Acct_Curricula2.csv?raw=true", show_col_types = FALSE, )
technical_skills <- read_csv("https://github.com/cliftonleesps/607_final_project/raw/master/technical_skills.csv", show_col_types = FALSE)
soft_skills <- read_csv("https://github.com/cliftonleesps/607_final_project/raw/master/soft_skills.csv", show_col_types = FALSE)


# initialize some counters
current_school <- accounting_programs$School[1]
description <- accounting_programs$Description[1]
temp_schools <- tibble(
  name = "",
  description = "",
  match_technical_skills = 0, 
  match_soft_skills = 0
)

# Iterate through
for (row in 2:nrow(accounting_programs)) {
  
  # if we detect a different school name, then save the data to the tibble
  if (current_school != accounting_programs$School[row]) {
      temp_schools <- temp_schools %>%
        add_row( 
                name = current_school, 
                description = paste0(description, accounting_programs$Description[row]),
                match_technical_skills = 0, 
                match_soft_skills = 0
        )
      description <- accounting_programs$Description[row]
      current_school <- accounting_programs$School[row]
  } else if (!is.na(accounting_programs$Description[row])) {
    # Just keep pasting the description for later
    description <- paste0(description, accounting_programs$Description[row])
  }
}


# Add the last school to the tibble
temp_schools <- temp_schools %>%
  add_row( 
    name = current_school, 
    description = paste0(description, accounting_programs$Description[row]),
    match_technical_skills = 0, 
    match_soft_skills = 0
  )

# delete the first row
nrow(temp_schools)
temp_schools <- temp_schools[-1,]
nrow(temp_schools)

# now iterate through the schools and split the descriptions

for (count in 1:nrow(temp_schools)) {
  ts <- temp_schools[count,]
  
  school_name <- ts[1]
  
  w <- ts[2] %>%
    str_to_lower() %>%
    str_replace_all ("\n", " ") %>%
    str_replace_all ("[[:punct:]]", "") %>% 
    str_split(" ") %>% 
    unique()
  
  school_descriptions <- tibble(
    word = ""
  )

  for (i in w[[1]]) {
    school_descriptions <- school_descriptions %>% 
      add_row(word = i)
  }
  school_descriptions <- school_descriptions %>% arrange() %>% unique()

  # now join with the technical skills
  technical_skill_match <- inner_join(technical_skills, school_descriptions,by="word")
  if (nrow(technical_skill_match) > 0) {
    #print (school_name)
    temp_schools[count,][3] <- 1
  }
  
  # now join with the soft skills
  soft_kills_match <- inner_join(soft_skills, school_descriptions,by="word")
  if (nrow(soft_kills_match) > 0) {
    #print (school_name)
    temp_schools[count,][4] <- 1
  }
  
  
}

temp_schools <- temp_schools %>% mutate (school_score = match_technical_skills + match_soft_skills)
temp_schools <- temp_schools %>% mutate (good_data_science_program = ifelse( school_score >= 2, "YES", "NO"))



# drop the description column
temp_schools <- subset(temp_schools, select = -c(2))
ncol(temp_schools)


end_time <- Sys.time()

print (end_time - start_time)


# Now we have temp_schools with metrics and a column for "good_data_science_program"
# Start building the model
set.seed(4393003)
sample_size <- 100



glimpse(temp_schools)
random_schools <- sample(temp_schools, size= 2,  replace = FALSE)
random_schools

# Randomly select schools
schools_bad <- temp_schools %>% filter(good_data_science_program == "NO")
schools_good <- temp_schools %>% filter(good_data_science_program == "YES")


sample_schools <- schools_good[sample(nrow(schools_good), sample_size - nrow(schools_bad)), ]
for (c in 1:nrow(schools_bad)) {
  row <- schools_bad[c,]
  sample_schools <- add_row(sample_schools, tibble(
      name = row$name,
      match_technical_skills = row$match_technical_skills,
      match_soft_skills = row$match_soft_skills,
      school_score = row$school_score, 
      good_data_science_program = row$good_data_science_program
    )
  )
}

# now we have our samples
school_split <- initial_split(sample_schools, 
                             prop = 3/4)
school_split

school_train <- training(school_split)
school_test <- testing(school_split)


school_cv <- vfold_cv(school_train)
school_cv

# define the recipe
school_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(good_data_science_program ~ match_technical_skills + match_soft_skills + school_score, 
         data = sample_schools) %>%
  step_normalize(all_numeric()) %>%
  step_impute_knn(all_predictors())

school_recipe


school_train_preprocessed <- school_recipe %>%
  # apply the recipe to the training data
  prep(school_train) %>%
  # extract the pre-processed training dataset
  juice()
school_train_preprocessed


rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 


# set the workflow
rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(school_recipe) %>%
  # add the model
  add_model(rf_model)
rf_workflow

?metric_set


rf_grid <- expand.grid(mtry = c(2,3))
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = school_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

rf_tune_results %>%
  collect_metrics()


param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)


rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(school_split)
rf_fit


test_performance <- rf_fit %>% collect_metrics()
test_performance

test_predictions <- rf_fit %>% collect_predictions()
test_predictions

final_model <- fit(rf_workflow, sample_schools)
final_model


# predict fictitious colleges
test_bad_college <- tibble(
  name = "Test Bad college",
  match_technical_skills = 0,
  match_soft_skills = 1, 
  school_score = 1
)

test_good_college <- tibble(
  name = "Test Good college",
  match_technical_skills = 1,
  match_soft_skills = 1, 
  school_score = 2
)

# Predict will output if the college has a good data science program
predict(final_model, new_data = test_bad_college)
predict(final_model, new_data = test_good_college)




