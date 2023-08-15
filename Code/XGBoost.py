# Libraries ----
import pandas as pd
from scipy.stats import uniform, randint
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import f1_score
from xgboost import XGBClassifier

# Data ----
df = pd.read_csv("./Data/model.csv")
scaler = StandardScaler()

X = df.iloc[:, 2:]
y = df.iloc[:, 1]

scaler = scaler.fit(X)
X = scaler.transform(X)

# Train-Test Split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 123)

# XGBoost ----
boost = XGBClassifier(objective="multi:softprob", random_state=123)

params = {
    "colsample_bytree": uniform(0.7, 0.3),
    "gamma": uniform(0, 0.5),
    "learning_rate": uniform(0.03, 0.3), 
    "max_depth": randint(2, 20),
    "n_estimators": randint(10, 150),
    "subsample": uniform(0.6, 0.4)
}

boost = RandomizedSearchCV(boost, 
                           param_distributions=params,
                           random_state=123, 
                           scoring="f1_micro",
                           n_iter=100,
                           cv=5,
                           verbose=3)

boost = boost.fit(X_train, y_train)

boost_best = boost.best_estimator_

print(boost_best.get_params())

test_pred = boost_best.predict(X_test)
mse_XGB = f1_score(y_test, test_pred, average = "micro")
print(f"Micro F1 Test Set: {mse_XGB}")

y_hat = boost_best.predict_proba(X)

print(y_hat[1:10])
y_hat = pd.DataFrame(y_hat).to_csv("./Data/predictions_mob.csv")

