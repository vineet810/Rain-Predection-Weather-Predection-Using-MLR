## Rain-Predection-Weather-Predection-Using-MLR

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import StandardScaler


data = pd.read_csv('weatherAUS.csv')

# Select relevant columns for modeling
data = data[['MaxTemp', 'Humidity3pm', 'WindSpeed3pm', 'Cloud3pm', 'Rainfall']]

# Drop rows with missing values
data = data.dropna()

# Split the data into independent variables (X) and the dependent variable (y)
X = data[['MaxTemp', 'Humidity3pm', 'WindSpeed3pm', 'Cloud3pm']]
y = data['Rainfall']

# Perform feature scaling
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Split the scaled data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X_scaled, y, test_size=0.2, random_state=42)

# Create a linear regression model
model = LinearRegression()

# Train the model using the training data
model.fit(X_train, y_train)

# Make predictions on the testing data
y_pred = model.predict(X_test)

# Evaluate the model using root mean square error (RMSE)
rmse = mean_squared_error(y_test, y_pred, squared=False)
print("Root Mean Square Error (RMSE):", rmse)

# Prediction
new_data = pd.DataFrame({'MaxTemp': [28], 'Humidity3pm': [65], 'WindSpeed3pm': [12], 'Cloud3pm': [3]})
new_data_scaled = scaler.transform(new_data)
prediction = model.predict(new_data_scaled)
print("Predicted Rainfall:", prediction[0])
