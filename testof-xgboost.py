import numpy as np
import xgboost
from sklearn.datasets import load_boston
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error
from scipy.stats import pearsonr
#you need to install older version of scikit for the Boston housing
# use pip3 install scikit-learn==1.1.3
data = load_boston()
X = data['data']
y = data['target']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

xgb = xgboost.XGBRegressor()
xgb.fit(X_train, y_train)
y_pred = xgb.predict(X_test)

print(f'RMSE:{np.sqrt(mean_squared_error(y_test, y_pred))}')
print(f'r2: {r2_score(y_test, y_pred)}')
print(f'pearsonr: {pearsonr(y_test, y_pred)}')