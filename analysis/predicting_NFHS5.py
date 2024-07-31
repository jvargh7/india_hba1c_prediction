#### IMPORTING LIBRARIES
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor, StackingRegressor
import xgboost as xgb
from sklearn.model_selection import KFold, GridSearchCV
import warnings
warnings.filterwarnings("ignore")

#### CONSTANTS
TRAINING_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/CARRS/only_fup_imputed.csv"
NFHS5_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Data/NFHS5/noNA_healthNFHS5.csv"
EXCEL_FILE_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Prediction/predictedNFHS5.xlsx"
PREDICTIONS_CSV_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Prediction/predictions.csv"
COLUMNS = ["glucose", "waistcircumference", "hipcircumference", "weight", "height", "sbp", "dbp", "bmi", "age", "sex", "htn", "dm", "bp_treatment", "dm_treatment"]
TARGET = "log_hba1c"
KFOLD_SPLITS = 10

# CARRS FOR TRAINING
carrs_data = pd.read_csv(TRAINING_PATH)
data_train = carrs_data[carrs_data["fup"] == 0]
data_test = carrs_data[carrs_data["fup"] == 4]

df_train = data_train[COLUMNS]
target_train = data_train[TARGET]
target_test = data_test[TARGET]

# NFHS5 FOR PREDICTING
nfhs5_data = pd.read_csv(NFHS5_PATH)
df_nfhs5 = nfhs5_data[COLUMNS]

kf = KFold(n_splits=KFOLD_SPLITS, shuffle=True, random_state=123)

#### DEFINE HYPERPARAMETER SEARCH SPACES
param_grid_xgb = {
    'n_estimators': [50, 100, 200],
    'max_depth': [3, 4, 5],
    'learning_rate': [0.01, 0.1, 0.2]
}

param_grid_gbr = {
    'n_estimators': [50, 100, 200],
    'max_depth': [3, 4, 5],
    'learning_rate': [0.01, 0.1, 0.2]
}

param_grid_rf = {
    'n_estimators': [50, 100, 200],
    'max_features': ['sqrt', 'log2', None],
    'max_depth': [10, 20, None]
}

param_grid_meta = {
    'n_estimators': [50, 100, 200],
    'max_depth': [3, 4, 5],
    'learning_rate': [0.01, 0.1, 0.2]
}

#### FUNCTION TO TUNE MODELS
def tune_model(model, param_grid, df_train, target_train, kf):
    search = GridSearchCV(estimator=model, param_grid=param_grid, cv=kf, scoring='neg_mean_absolute_error', n_jobs=-1)
    search.fit(df_train, target_train)
    return search.best_estimator_

# Tuning base models
xgb_tuned = tune_model(xgb.XGBRegressor(objective='reg:absoluteerror', random_state=123), param_grid_xgb, df_train, target_train, kf)
gbr_tuned = tune_model(GradientBoostingRegressor(loss="absolute_error", random_state=123), param_grid_gbr, df_train, target_train, kf)
rf_tuned = tune_model(RandomForestRegressor(random_state=123), param_grid_rf, df_train, target_train, kf)

# Tuning meta model
meta_tuned = tune_model(GradientBoostingRegressor(random_state=123), param_grid_meta, df_train, target_train, kf)

# Defining base models 
models = [
    ('XGBOOST Abs Error', xgb_tuned),
    ('Gradient Boosting', gbr_tuned),
    ('Random Forest', rf_tuned)
]

#### DEFINE STACKING REGRESSOR WITH TUNED MODELS
stacked_model = StackingRegressor(
    estimators=[('xgboost', xgb_tuned), ('gradient_boosting', gbr_tuned), ('random_forest', rf_tuned)],
    final_estimator=meta_tuned,
    cv=kf
)

def evaluate_model(name, model, df_train, target_train, df_nfhs5, kf):
    model.fit(df_train, target_train)
    NFHS_pred = model.predict(df_nfhs5)
    return NFHS_pred

# Collect predictions from each model
predictions = {
    "Model": [],
    "Predictions": []
}

for name, model in models + [('stacked_model', stacked_model)]:
    NFHS_pred = evaluate_model(name, model, df_train, target_train, df_nfhs5, kf)
    predictions["Model"].append(name)
    predictions["Predictions"].append(NFHS_pred)

# Convert to DataFrame
predictions_df = pd.DataFrame(predictions)

# Expand the predictions into separate columns
expanded_df = pd.DataFrame()
for model_name, preds in zip(predictions["Model"], predictions["Predictions"]):
    expanded_df[f"{model_name}_predictions"] = preds

# Optionally include actual values and indices
expanded_df.index.name = 'Index'

# Load the original dataset for merging
original_data = pd.read_csv(NFHS5_PATH)

# Merge predictions with original data
merged_data = original_data.join(expanded_df.set_index(original_data.index), how='left')

# Save merged dataset with predictions
merged_data.to_csv('/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/Prediction/merged_predictions_with_original.csv', index=False)

print("Merged predictions with original data have been exported to merged_predictions_with_original.csv")