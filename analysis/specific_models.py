# simplified models based on results of full_imputation_models_comparison

#### IMPORTING LIBRARIES

import numpy as np 
import pandas as pd 

file_paths = {
    # "random_forest": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_rf.csv",
    "linear_regression_prediction": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_prediction.csv"
    # "linear_regression_bootstrap": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_bootstrap.csv",
    # "general_variation": "/Users/alaynabaker/Documents/Hba1c Prediction/Modeling Code/carrs_imputed_variation.csv"
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

    'XGBOOST Abs Error': xgb.XGBRegressor(objective='reg:absoluteerror', evalmetric = mean_absolute_error, random_state=123), 
    'Gradient Boosting': GradientBoostingRegressor(loss="absolute_error", random_state=123), 
    'Random Forest': RandomForestRegressor(random_state=123)
}
# INITIALIZING KFOLD WITH SPLITS

from sklearn.model_selection import LeavePOut, LeaveOneOut, cross_val_score, KFold, cross_val_predict, cross_validate

k = 5
kf = KFold(n_splits = k, shuffle=True, random_state = 123)

from sklearn.metrics import make_scorer, mean_squared_error, r2_score

excel_file_path = "/Users/alaynabaker/Documents/Hba1c Prediction/Model Results/Second Iteration/full_imputation_results.xlsx"
with pd.ExcelWriter(excel_file_path, engine='openpyxl') as writer:
    for sheet_name, file_path in file_paths.items():
        print(sheet_name)

        imputation_results = {}

        data0 = pd.read_csv(file_path)

        for imp_index in range(1, 6): 

            print(imp_index)
            imputed_data = data0[data0[".imp"] == imp_index]

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


                # other predictions - visualization potential
                model.fit(df_train, target_train)
                y_test_pred = model.predict(df_test)
                other_predictions[name] = y_test_pred


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
            imputation_results[imp_index] = results

        df_results = []
        for imp_index, results in imputation_results.items():
            for model_name, metrics in results.items():
                df_results.append({"Imputation": imp_index, "Model": model_name, **metrics})

        df_results = pd.DataFrame(df_results)
        print(df_results)

        df_results.to_excel(writer, sheet_name=sheet_name, index=False)

print("DONE")
