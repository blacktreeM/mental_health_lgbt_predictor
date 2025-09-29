rm(list=ls())
library(reticulate); library(dplyr); library(ggplot2)
use_condaenv("C:/Users/masan/miniconda3", required = TRUE)
#py_install(c("numpy", "pandas", "scikit-learn", "seaborn", "matplotlib", 'lightgbm', 'xgboost', 'catboost'))
py_run_string("
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
import pandas as pd
import numpy as np
data =  pd.read_csv('brfss/brfss1424clean.csv')
print(data.describe())
print(data.head())
")
py_run_string("
features = ['lgbt', 'female', 'age1829', 'age30s', 'age40s', 'age50s', 'age60s', 'age70s', 'age80s',
            'white', 'black', 'hispanic', 'other', 'employed', 'selfemp', 'unemployed', 'homemaker',
            'student', 'retired', 'disable', 'noschool', 'highschool', 'somecollege', 'college',
            'married', 'divorced', 'widow', 'separate', 'single', 'couple', 'income1', 'income2',
            'income3', 'income4', 'income5', 'income6', 'income7', 'income8', 'income9',
            'home', 'insurance', 'veteran']
target = 'freq'
models = {
    'Logistic Regression': LogisticRegression(solver='liblinear'),
    'Random Forest': RandomForestClassifier(n_estimators=300, random_state=1),
    'Gradient Boosting': GradientBoostingClassifier(n_estimators=300, random_state=1),
}
all_results = []
for i in range(2014, 2025):
    df = data[data['year'] == i].copy()
    if not df.empty:
        X = df[features]
        y = df[target]
        for model_name, model in models.items():
            model.fit(X, y)
            if hasattr(model, 'coef_'):
                importance_values = model.coef_[0]
            elif hasattr(model, 'feature_importances_'):
                importance_values = model.feature_importances_
            else:
                importance_values = [np.nan] * len(features) # Fallback for unknown model types
            importance_df = pd.DataFrame({
                'feature': features,
                'importance': importance_values
            })
            importance_df['model'] = model_name
            importance_df['year'] = i
            importance_df = importance_df.sort_values(by='importance', ascending=False).reset_index(drop=True)
            importance_df['rank'] = importance_df.index + 1
            all_results.append(importance_df)
            print(all_results)
")
py_run_string("
final_results_df = pd.concat(all_results, ignore_index=True)
final_results_df.to_csv('model_feature_importance.csv', index=False)
")
df = read.csv('model_feature_importance.csv'); head(df)
feature_map <- c(
  "lgbt" = "LGBT",
  "female" = "Female",
  "age1829" = "Age 18-29",
  "age30s" = "Age 30-39",
  "age40s" = "Age 40-49",
  "age50s" = "Age 50-59",
  "age60s" = "Age 60-69",
  "age70s" = "Age 70-79",
  "age80s" = "Age 80+",
  "white" = "White (Non-Hispanic)",
  "black" = "Black (Non-Hispanic)",
  "hispanic" = "Hispanic",
  "other" = "Other race or multiracial",
  "employed" = "Employed",
  "selfemp" = "Self-employed",
  "unemployed" = "Unemployed",
  "homemaker" = "Homemaker",
  "student" = "Student",
  "retired" = "Retired",
  "disable" = "Unable to work",
  "noschool" = "Highschool dropout",
  "highschool" = "Highschool graduate",
  "somecollege" = "Some college",
  "college" = "College graduate",
  "married" = "Married",
  "divorced" = "Divorced",
  "widow" = "Widowed",
  "separate" = "Separated",
  "single" = "Never married",
  "couple" = "Cohabitating",
  "income1" = "Household income <$10,000",
  "income2" = "Household income $10,000-$15,000",
  "income3" = "Household income $15,000-$20,000",
  "income4" = "Household income $20,000-$25,000",
  "income5" = "Household income $25,000-$35,000",
  "income6" = "Household income $35,000-$50,000",
  "income7" = "Household income $50,000-$75,000",
  "income8" = "Household income >$75,000",
  "income9" = "Household income refused/unknown",
  "home" = "Homeowner",
  "insurance" = "Have health insurance",
  "veteran" = "Veteran"
)
df = df %>% mutate(feature_full = recode(feature, !!!feature_map)); head(df)
feature_order <- c(
  "LGBT", "Female", "Age 18-29", "Age 30-39", "Age 40-49", "Age 50-59", 
  "Age 60-69", "Age 70-79", "Age 80+", "White (Non-Hispanic)", 
  "Black (Non-Hispanic)", "Hispanic", "Other race or multiracial", 
  "Employed", "Self-employed", "Unemployed", "Homemaker", "Student", 
  "Retired", "Unable to work", "Highschool dropout", "Highschool graduate", 
  "Some college", "College graduate", "Married", "Divorced", "Widowed", 
  "Separated", "Never married", "Cohabitating", "Household income <$10,000", 
  "Household income $10,000-$15,000", "Household income $15,000-$20,000", 
  "Household income $20,000-$25,000", "Household income $25,000-$35,000", 
  "Household income $35,000-$50,000", "Household income $50,000-$75,000", 
  "Household income >$75,000", "Household income refused/unknown", 
  "Homeowner", 'Have health insurance', "Veteran"
)
plot_rank = function(model_name, file_name){
  ggplot(subset(df, model == model_name),
         aes(factor(year), factor(feature_full, levels = rev(feature_order)), fill = rank)) +
    geom_tile(color = "white", lwd = 1, linetype = 1) +
    geom_text(aes(label = rank), color = "white", size = 4) +
    scale_fill_gradient(low = "black", high = "grey", na.value = "white") +
    theme(legend.position = "none") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  ggsave(file=file_name, height=8, width=6)
}
plot_rank('Logistic Regression', 'logistic.png')
plot_rank('Random Forest', 'rf.png')
plot_rank('Gradient Boosting', 'gb.png')
