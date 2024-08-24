---

## Predicting In-Hospital Mortality: A Logistic Regression Approach

Understanding the predictors of in-hospital mortality for admitted patients is crucial for improving healthcare outcomes. This project focuses on developing and validating a prediction model for all-cause in-hospital mortality among patients.

### Dataset Overview
Our dataset consists of 85 columns, with 78 numeric and 7 categorical features. This comprehensive set of features provides a rich foundation for our analysis and modeling.

### Approach
We employed a systematic approach to build our predictive model:
1. **Data Preprocessing**: We utilized the `tidymodels` framework for efficient preprocessing. This included combining numeric and categorical transformations to prepare the data for modeling.
2. **Model Building**: A Logistic Regression model was constructed within a pipeline that integrated the preprocessing steps with the model training process.

### Exploratory Data Analysis (EDA)
During the EDA phase, we used `ggplot2` for static visualizations to explore and understand the data:
- **Histograms** and **line plots** to visualize the distribution of features and their relationships with mortality.
- **Box plots** to examine the impact of numerical features on mortality.
- **Scatter plots** to analyze the effects of categorical variables, such as ICU types and Apache 3J body systems, on mortality rates.

This style of EDA helps in uncovering patterns and relationships in the data, providing insights into the factors influencing patient outcomes.

### Model Performance
Our Logistic Regression model demonstrated impressive predictive performance, achieving an accuracy of approximately 92.34% on the validation set. This high level of accuracy indicates that the model effectively predicts patient survival or death.

### Conclusion
This project highlights the successful application of a Logistic Regression model for predicting in-hospital mortality. With an accuracy of 92.34% on the validation set, the model offers significant potential for use in healthcare settings. By leveraging diverse patient data—including demographics, medical history, and vital signs—healthcare professionals can gain valuable insights into patient outcomes and improve decision-making processes.

---
