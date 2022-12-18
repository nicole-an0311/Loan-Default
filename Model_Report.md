# MODEL REPORT - Detailed Analysis & Steps 





# Target Summary 


The target variable is loan status, which has two levels indicating whether the loan status is current or defaulted. Default is the positive class. The bar graph above shows the default rate to be 15% while the current rate to be 85%. That is, out of all 29,777 events from the dataset, 4477 transactions were labelled as fraud. The default accuracy rate for the target variable is 85%, the rate of the majority class, the current class. The two levels are relatively unbalanced in terms of sizes, so accuracy of the model will tend to be very high for correctly predicting the current class. Therefore, it alone is not a good evaluation of the model. When selecting the best predictive model, not only should the model accuracy rate be higher than the default accuracy, but the model should also have relatively high precision and recall as indication of generating correct predictions for the positive class. 
Loan_status
<chr>	n
<int>	pct
<dbl>
default	4477
	0.850
current	25297	0.150
# Exploratory Data Analysis & Screening 
## Descriptive Statistics
Below is an overview of the distribution of all numerical variables with key descriptive statistics such as number of distinct observations, mean, min, and max. There are a total of 29 numeric variables, including loan amount, funded amount, past due amount, annual income, number of payment terms, last payment date, and so on. 3 of the variables have 0 variance, which may influence prediction results if included in the process of building models. Based on the bar graphs, many numeric variables are left skewed, so a variety of plots and profiling are made to study the relationship between each feature. The outliers are presidential in this dataset based on these bar graphs, and an isolation forest model is constructed to detect if the outliers are anomalies. During data cleaning process, the potential data quality issues will be addressed to improve prediction performances.
                                            
## Frequency Analysis 
Below is an overview of the distribution of categorical variables with stack bar graphs. The frequency analysis depicts categorical variables and their relationship with the target, loan status. Verification status from LC does not seem to relate to loan status. On the other hand, payment plan is strongly related to the label we are predicting, that is when a borrower implements a payment plan for the loan, his or her default rate decreases significantly. The default rate increases as grade and subgrade decrease, indicating that these 2 variables may influence the loan status and may contribute significantly in prediction default rate. The change in default rate from different levels within each feature may not be directly related to loan status, but they are still good practice to get familiar with the features, and they could also be useful for guiding feature engineering.
         
## Correlation Analysis 
 
A correlation map is developed for the numerical and label encoded variables. The goal of correlation analysis is to discover any patterns before modelling, and generate insights on data that could lead to business action plans. 
This technique investigate the relationship between two quantitative variables and check for possible multicollinearity (dependence that may violate model assumptions). Pearson’s correlation is the most common one; the coefficient scales from -1 to 1, where 1/-1 represents the perfect positive/negative correlation, and 0 represents complete irrelevance. As the heatmap above presents, the stronger the coefficient, the brighter the cell is while the weaker the correlation, the paler the cell is. The variables with little to 0 variance are not considered during correlation analysis and thus are left with grey cells. 
Fico ranges are strongly and negatively correlated with interest rates (r=-0.7). The graphs below are 2-numeric variable with target scatterplot displaying the negative correlation. Since the default class is in light blue, it is clear that default cases tend to concentrate in areas with high loan interest rates and low fico benchmarks. Loan amount, funded amount, investor committed funded amount, and installment are positively highly correlated with each other (r>0.85).
  
# Initial Screening & Exploration 
After descriptive analysis on the continuous variables, some initial insights are established with target. The boxplots of interest rates and fico ranges depict the distribution of the variables compared by loan status. Here the default cases have a higher IQR range of interest rate, meaning that cases with high loan interest rates may be more likely to be defaulted. On the other hand, cases with lower fico boundaries are also likely to be defaulted.
Cases with lower payment amount from last record tend to be defaulted. The more revolving credit used leads to higher default rate. These are all good indications of what is causing a case to be defaulted, and they will help in interpreting predictive models that are more different to draw recommendations. 
     
The 2-numeric-variable-with-target scatterplots help us screen out significant variables that are likely to influence the target. The label we are predicting is loan status, and we can see that the light blue dots that represent defaulted loans are concentrated on the left sides of the scatterplots. Cases with lower amount received from last payment and lower funded amounts are likely to be defaulted. In addition, cases with lower amount received from last payment and low-to-medium lower fico boundaries are also likely to be defaulted. 
  
After frequency analysis on the discrete variables, these variables are found to be interesting factors that may relate to the target. Home ownership status seems to relate to loan label and not owning a property has a default rate higher than the average default rate. Payment terms seem to also influence loan status and borrowers with 60 payments instead of 36 would be more likely to default their loans.
  
# Data Preparation & Transformation
The datasets are formatted in neat forms with a label called loan status. However, as previous summary and analysis have touched on, a variety of data quality issues exist, so it would still need different types of cleaning methods. The preparation and transformation done are shown below:
(1) Drop features: Some numerical variables are highly correlated and thus one is reduced from the recipe (e.g., loan amount and funded amount). Some features have very low variance and are irrelevant factors to the target (e.g. chargeoff_within_12_mths, collections_12_mths_ex_med, etc). High cardinality variables can be computational heavy and cannot be used in predictive modelling (e.g. emp_title,issue_d,url,desc,title,zip_code,earliest_cr_line). Lastly, the unique identifiers are dropped from modelling (e.g. id, member_id).
(2) Feature engineering: 
•	Numeric Predictors
i.	Replaced missing numeric variables with median 
ii.	Removed variables with low variances (almost all of the same number)
iii.	Normalize numeric features for Neural Network model
•	Categorical Predictors
i.	Replaced missing categorical variables with a new level called “unknown”
ii.	Assigned a novel factor level to potential new levels in testing dataset
iii.	Encoded categorical variables with dummy variables of 0s and 1s with one-hot encoded
iv.	Pooled rarely occurring levels together and created a level called “other”
•	Target Variable	
i.	Down-sampling negative class for training data to 1:1 positive:negative ratio for Neural Network model and 1:3 ratio for other models.
(3) Specific Imputation: Units are dropped to utilize as numeric variables (e.g. int_rate, revol_util). Variables that are in character data type, including the target, are transformed to factors to improve model’s predictive performance.
Anomaly Detections
An isolation forest model is constructed to detect anomalies from the dataset. The observations identified by this model are rare events and they may influence model performance negatively. This technique only explores the predictor variables, meaning that the target label, loan status, does not influence the model’s prediction. An anomaly score of -1 is assigned to anomalies and 1 to normal points and the average anomaly score here is 0.51. With this score, the surrogate model identifies anomaly for predictions with anomaly score >= 0.51.
 
#  Below are the global anomalous rules:
  
1	IF int_rate <  14 & funded_amnt <  20163 & emp_length_X7.years >= 0.5  THEN Anomaly  coverage   3%
2	IF int_rate <  14 & funded_amnt >= 20163      THEN  Anomaly  coverage   5%
3	IF int_rate >= 14          THEN  Anomaly  coverage  31%
4	IF int_rate <  14 & funded_amnt <  20163 & emp_length_X7.years <  0.5  THEN  Normal  coverage  61%
# Below are the local anomalous records that with the highest anomaly scores (most rare cases):

 	 

 	 

 	 

We can see that interest rate < 14 is an important anomaly detector. In addition, annual income > 399998 is also significant, and it helps for us to notice some extreme outlier with an annual income of $6,000,000 while the rest observations revolve around $200,000. Therefore, to reduce outlier’s influence from models that are sensitive to noises such as Neural Network, it is dropped from the dataset.
# Model Building 
1.	Data partitioning
I.	The holdout dataset is splitted randomly by 70/30 where 70% were training data and 30% were testing data.
2.	Model Formula
I.	loan_status ~ loan_amnt+term+grade+sub_grade+emp_length+home_ownership+annual_inc+verification_status+pymnt_plan+purpose+addr_state+dti+delinq_2yrs+fico_range_low+fico_range_high+inq_last_6mths+open_acc+pub_rec+revol_bal+total_acc+out_prncp+out_prncp_inv+total_rec_late_fee+last_pymnt_amnt+collections_12_mths_ex_med+policy_code+application_type+acc_now_delinq+chargeoff_within_12_mths+delinq_amnt+pub_rec_bankruptcies+tax_liens+issue_d+earliest_cr_line+last_pymnt_d+next_pymnt_d+last_credit_pull_d
II.	XGBoost with 3-fold CV: loan_status ~ term+total_rec_late_fee+last_pymnt_amnt+last_pymnt_d+loan_amnt
3.	Model Specification  
Variables are selected based on explanatory analysis of all predictor variables and business requirements from stakeholders
I.	Random Forest Models
i.	Tree = 200; mtry = 28; target node size = 10
II.	Neural Network Model
i.	Hidden units = 10; dropout = 0.01; epochs = 20
III.	XGBoost Models
i.	Trees = 200; learn_rate = 0.3103493
ii.	3-fold CV hyperparameter tuning: tree_depth=15; learn_rate = 0.3103493
# Model Comparison 
## Model 	Partition	AUC	Precision	Recall	LogLoss	F1 Score
Random Forest (Tree = 200; mtry = 28; target node size = 10)	train	0.998	0.944	0.934	0.2	0.939
 	test	0.905	0.751	0.386	0.309	0.510
Neural Network (Hidden units = 10; dropout = 0.01; epochs = 20)	train	0.932	0.469	0.917	0.476	0.621
 	test	0.91	0.443	0.861	0.486	0.585
XGBoost (Trees = 200; learn_rate = 0.3103493)	train	0.996	0.912	0.966	0.0847	0.938
	test	0.978	0.832	0.847	0.139	0.839
XGBoost with 3-fold CV Tuning (tree_depth=15; learn_rate = 0.3103493) 	train	0.968	0.849	0.718	0.195	0.778
 	test	0.943	0.765	0.635	0.223	0.694

## Model Selection & Hyperparameter Tuning
The model performance of the 4 models constructed are shown above. There are 5 metrics to consider, AUC, precision, recall, logloss, and F1 score. Based on the overall performance, the best performing model is the XGBoost model with tree=200 and learn rate=0.3103. 
One XGBoost model is tuned by 3-fold CV with a recipe different from the best model. However, that model is not yielding the best outcome. The best model is manually tuned several times to receive a high prediction performance. To prevent the best model from further overfitting, it also utilized this tuned learning rate from the other XGBoost model, which yields a better AUC, reducing overfitting.
## ROC Chart & Distribution of Probability
   
## Train & Test Confusion Matrices
   
## Selected Model Operating Tables
fpr
<dbl>	threshold
<dbl>	tpr
<dbl>		
0.00	0.912	0.000		
0.01	0.742	0.382		
0.02	0.568	0.667		
0.03	0.455	0.804		
0.04	0.371	0.870		
0.05	0.313	0.908		
0.06	0.268	0.932		
0.07	0.238	0.949		
0.08	0.210	0.958		
0.09	0.191	0.961		

## Operational Business Rules w. Expected Performance (Precision & Recall) 
   
The operational rule is to detect default loans before it is defaulted at a 5% false positive rate (FPR) or lower. The best performing model allows operating at 5% FPR to leverage the tradeoff between FPR and recall (true positive rate) and finds the sweet spot that minimizes damage of making errors from both sides. The ROC chart for the best performing model above shows that false positive rate and recall are positively related to each other. The costing of missing a default loan is huge, so the recall should be as high as possible. Rising recall will cause the FPR to increase, and the costs of inspection increase as well. The best model sets the prediction score threshold to be 0.313. In other words, if a transaction has a 31.3% or higher probability of being past-due, the model marks the loan to be suspicious and askes the detection system to investigate and stop issuing this loan. There is a 5% chance that this suspicious case is a good loan. 
By operating at 5% FPR, the chart threshold vs TPR (recall) indicates that a threshold of 31.3% yields a 0.908 recall. That is, out of 1000 default loans, the model will correctly classify 908 of them. The chart also indicates that precision and recall are negatively related to each other. Therefore, the model yields a moderate precision rate when the recall rate is very high. The precision rate is 0.750, displayed in the chart precision vs recall. That is, out of 1000 transactions predicted as defaulted, 75% of them are correct classification. 
Global Explanations
 
Below are the top 10 most important variables from the best model selected. Being the most influential features in predicting past-dues, we want to examine their impact across all predictions. The Partial Dependence Plots (PDP) depict the how the predictions partially depend on values of the most important variables. The effect of a variable is measured in change in the mean response. We can see that 60 months payments are driving the default rate to go up. As loan amount increase, the probability of defaulting this loan also increases. Last payment amount and annual income both indicate outliers because at they increase to a high level, they become less impactful.
       
# Local Explanations
Below are the top 10 true positive, false positive, and false negative predictions. These are the most significant predictions from the model, and it is important to understand what variables are impacting these predictions, and the details of their influences. From the Break Down Profile for the best true positive prediction, it is clear that the total payment amount is the most impactful predictor. The total amount paid is recorded to be 412.98 and 33.8% of the prediction that extreme can be explained. 
## Top 10 True Positive Predictions:
 
          
## Top 10 False Positive Predictions:
 
          

## Top 10 False Negative Predictions
 


          
