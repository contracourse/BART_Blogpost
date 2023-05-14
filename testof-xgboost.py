import numpy as np
import pandas as pd
import xgboost
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from scipy.stats import pearsonr

data = pd.read_csv('Book1.csv')
# assuming column names exist in CSV file
X = data[["T10Y3M","EFFR","UNRATE","STLFSI4","CPIAUCSL_PC1"]] # replace feature1, feature2, feature3 with appropriate column names
y = data['SPY']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

xgb = xgboost.XGBRegressor()
xgb.fit(X_train, y_train)
y_pred = xgb.predict(X_test)

print(f'RMSE:{np.sqrt(mean_squared_error(y_test, y_pred))}')
print(f'r2: {r2_score(y_test, y_pred)}')
print(f'pearsonr: {pearsonr(y_test, y_pred)}')
