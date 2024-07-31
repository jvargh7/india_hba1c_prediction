#### IMPORTING LIBRARIES
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor, StackingRegressor
import xgboost as xgb
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import cross_val_score, KFold, cross_val_predict, cross_validate, GridSearchCV, RandomizedSearchCV
from joblib import Parallel, delayed

import warnings
warnings.filterwarnings("ignore")


#### CONSTANTS
FILE_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/final imputation/only_fup_imputed.csv"
EXCEL_FILE_PATH = "/Users/alaynabaker/Documents/Hba1c Prediction/FINAL/new_results.xlsx"
COLUMNS = ["glucose", "waistcircumference", "hipcircumference", "weight", "height", "sbp", "dbp", "bmi", "age", "sex", "htn", "dm", "bp_treatment", "dm_treatment"]
TARGET = "log_hba1c"
KFOLD_SPLITS = 10

data = pd.read_csv(FILE_PATH)
data_train = data[data["fup"] == 0]
data_test = data[data["fup"] == 4]

df_train = data_train[COLUMNS]
df_test = data_test[COLUMNS]
target_train = data_train[TARGET]
target_test = data_test[TARGET]

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

#### DEFINE BASE MODELS
models = [
    ('XGBOOST Abs Error', xgb.XGBRegressor(objective='reg:absoluteerror', evalmetric=mean_absolute_error, random_state=123)),
    ('Gradient Boosting',  GradientBoostingRegressor(loss="absolute_error", random_state=123)),
    ('Random Forest', RandomForestRegressor(random_state=123))]

meta_model = GradientBoostingRegressor(random_state=123)

#### DEFINE STACKING REGRESSOR WITH TUNED MODELS
stacked_model = StackingRegressor(
    estimators=[
        ('xgboost', xgb_tuned),
        ('gradient_boosting', gbr_tuned),
        ('random_forest', rf_tuned)
    ],
    final_estimator=meta_tuned,
    cv=kf
)

#### FUNCTION TO TRAIN AND EVALUATE MODELS
def evaluate_model(name, model, df_train, target_train, df_test, target_test, kf):
    y_pred = cross_val_predict(model, df_train, target_train, cv=kf)
    model.fit(df_train, target_train)
    y_test_pred = model.predict(df_test)
    
    scoring = ['neg_mean_squared_error', 'neg_mean_absolute_error', 'neg_root_mean_squared_error', 'max_error', 'r2']
    scores = cross_validate(model, df_train, target_train, cv=kf, scoring=scoring, return_train_score=True)
    
    results = {
        "Model": name,
        "Average MSE": round(-np.mean(scores["test_neg_mean_squared_error"]), 4),
        "Average MAE": round(-np.mean(scores["test_neg_mean_absolute_error"]), 4),
        "Average RMSE": round(-np.mean(scores["test_neg_root_mean_squared_error"]), 4),
        "Average R^2": round(np.mean(scores["test_r2"]), 4),
        "Average Max Error": round(np.mean(scores["test_max_error"]), 4)
    }
    return results, y_pred, y_test_pred

all_models = models + [('stacked_model', stacked_model)]

results = []


with Parallel(n_jobs=4, backend="loky") as parallel:
    model_results = parallel(
        delayed(evaluate_model)(name, model, df_train, target_train, df_test, target_test, kf)
        for name, model in [
            ('xgboost_tuned', xgb_tuned),
            ('gradient_boosting_tuned', gbr_tuned),
            ('random_forest_tuned', rf_tuned),
            ('stacked_model', stacked_model)
        ]
    )

for res, y_pred, y_test_pred in model_results:
    results.append(res)

df_results = pd.DataFrame(results)
df_results.to_excel(EXCEL_FILE_PATH, sheet_name="Results", index=False)

print("DONE WITH METRICS")

import pandas as pd

# Collect predictions from each model
predictions = {
    "Model": [],
    "Predictions": []
}

for name, model in [
    ('xgboost_tuned', xgb_tuned),
    ('gradient_boosting_tuned', gbr_tuned),
    ('random_forest_tuned', rf_tuned),
    ('stacked_model', stacked_model)
]:
    _, y_pred, _ = evaluate_model(name, model, df_train, target_train, df_test, target_test, kf)
    predictions["Model"].append(name)
    predictions["Predictions"].append(y_pred)

# Convert to DataFrame
predictions_df = pd.DataFrame(predictions)

# Expand the predictions into separate columns if needed
# Example: creating a DataFrame where each column represents predictions from a model
expanded_df = pd.DataFrame()
for i, (model_name, preds) in enumerate(zip(predictions["Model"], predictions["Predictions"])):
    expanded_df[f"{model_name}_predictions"] = preds

# Optionally include actual values and indices
expanded_df['Actual'] = target_test.values
expanded_df.index.name = 'Index'

# Save to CSV
expanded_df.to_csv('/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/predictions.csv', index=True)


