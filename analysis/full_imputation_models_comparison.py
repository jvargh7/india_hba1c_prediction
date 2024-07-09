#### IMPORTING LIBRARIES

import numpy as np 
import pandas as pd 

# imputed data - all 5 imputations included

file_paths = {
    "random_forest": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_rf.csv",
    "linear_regression_prediction": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_prediction.csv",
    "linear_regression_bootstrap": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_bootstrap.csv",
    "general_variation": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_variation.csv"
}

# columns to include
columns = ["glucose", "waistcircumference", "hipcircumference", "weight", "height",
  "sbp", "dbp", "bmi", "age", "sex", "htn", "dm", "bp_treatment",
  "dm_treatment"]

target = "log_hba1c"


#### DEFINE MODELS 
from sklearn.linear_model import LinearRegression, Ridge, RidgeCV, Lasso, LassoCV, ElasticNet
from sklearn.kernel_ridge import KernelRidge
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor, StackingRegressor, VotingRegressor, HistGradientBoostingRegressor
from sklearn.svm import LinearSVR 
from sklearn.neighbors import KNeighborsRegressor
import xgboost as xgb
from sklearn.metrics import mean_absolute_error

# NOTE: HANDLE NAs WITH XGBOOST AND KNEIGHBORSREGRESSOR

models = {
    # models that can handle NAs

    'XGBOOST SQ Error': xgb.XGBRegressor(objective='reg:squarederror', evalmetric = mean_absolute_error, random_state=123), 
    'XGBOOST Abs Error': xgb.XGBRegressor(objective='reg:absoluteerror', evalmetric = mean_absolute_error, random_state=123), 
    'Decision Tree': DecisionTreeRegressor(criterion="absolute_error", random_state=123), 

    # models that cannot handle NAs
    'Linear Regression': LinearRegression(),
    'Linear Regression - Ridge': Ridge(alpha=1.0),
    'Gradient Boosting': GradientBoostingRegressor(loss="absolute_error", random_state=123), 
    'Support Vector Machine': LinearSVR(random_state=123), 
    'K-Nearest Neighbors': KNeighborsRegressor(),
    'Random Forest': RandomForestRegressor(random_state=123)
}
# INITIALIZING KFOLD WITH SPLITS

from sklearn.model_selection import LeavePOut, LeaveOneOut, cross_val_score, KFold, cross_val_predict, cross_validate

k = 5
kf = KFold(n_splits = k, shuffle=True, random_state = 123)
loo = LeaveOneOut()

from sklearn.metrics import make_scorer, mean_squared_error, r2_score

# Initialize Excel writer
excel_file_path = "/Users/alaynabaker/Documents/Hba1c Prediction/Model Results/First Iteration/model_comparison_results.xlsx"

all_results = []

for sheet_name, file_path in file_paths.items():
    print(sheet_name)
    imputation_results = {}

    data0 = pd.read_csv(file_path)
    imputed_data = data0[data0[".imp"] == 1]

    data_train = imputed_data[imputed_data["fup"] == 0]
    data_test = imputed_data[imputed_data["fup"] == 4]

    df_train = data_train[columns]
    df_test = data_test[columns]
    target_train = data_train[target]
    target_test = data_test[target]

    results = {}
    predictions = {}
    other_predictions = {}

    for name, model in models.items(): 

        print(name)

        # getting predictions 
        y_pred = cross_val_predict(model, df_train, target_train, cv=kf)
        predictions[name] = y_pred


        # other predictions - test for visualizations
        model.fit(df_train, target_train)
        y_test_pred = model.predict(df_test)
        other_predictions[name] = y_test_pred

        # scores! 
        scoring = ['neg_mean_squared_error', 'neg_mean_absolute_error', 'neg_root_mean_squared_error', 'max_error', 'r2'
            ]
        
        scores = cross_validate(model, df_train, target_train, cv=kf, 
                                scoring=scoring, 
                                return_train_score=True)


        results[name] = {
            "Average MSE": round(-np.mean(scores["test_neg_mean_squared_error"]), 4), 
            "Average MAE": round(-np.mean(scores["test_neg_mean_absolute_error"]), 4), 
            "Average RMSE": round(-np.mean(scores["test_neg_root_mean_squared_error"]), 4), 
            "Average R^2": round(np.mean(scores["test_r2"]), 4), 
            "Average Max Error": round(np.mean(scores["test_max_error"]), 4)
        }
        imputation_results[sheet_name] = results

        for sheet_name, results in imputation_results.items():
            for model_name, metrics in results.items():
                all_results.append({"Imputation": sheet_name, "Model": model_name, **metrics})
    

df_results = pd.DataFrame(all_results)
print(df_results)

df_results.to_excel(excel_file_path, index=False)

print("DONE")