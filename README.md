# Customer-Churn-in-the-Airline-Industry
Southeast airlines needed to lower their customer churn. They have already spent a lot on the loyalty program. However, their customers were valuing the loyalty program less, which was one reason why just relying on their loyalty program might be insufficient in keeping low customer churn. So alternatively, the airline company wants to identify some leading indicators of when a customer is about to stop flying Southeast. Therefore, Southeast airlines can take actions that can avoid having their customers leave.

## Attribute Ananlysis
Before starting some modeling, a thorough attribute analysis is carried out. The groups want to be very confident knowing how each attribute looks like, what they are standing for, and how those attributes are going to help the subsequent analysis. 
First of all, the group creates some visualizations for each individual attribute to identify a single pattern. Then, a series of explanatory analysis focusing on Net Promoter Score (NPS) is carried out.

By looking at the frequencies of NPS under different categories, some characteristics of detractors become clear. As for service class, the majority of detractors are concentrated in the Eco class. It is also noticeable that even though there are more business travels, personal travel includes more detractors. 
![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/%E5%9B%BE%E7%89%871.png)

In terms of the airline status, passenger group with the blue status has the highest percentage of detractors. As for the punctuality, the group notes that passengers taking a delayed flight tend to be detractors. 
![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/%E5%9B%BE%E7%89%872.png)

According to the histogram of gender and age with NPS, female passengers, and young as well as elder people tend to be more detractive.
![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/%E5%9B%BE%E7%89%873.png)

Apart from that, the detractors are also gathering where total frequent flyer account is zero, price sensitivity is one. Additionally, there is no significant difference in the distribution of NPS across Flights per Year, spending, eating and drinking amounts, by simply looking at the histograms. The group then tries multiple means to explore and demonstrates those patterns

## Correlation
The group also looks at the correlation between the likelihood the recommendation and other relevant variables. It can be concluded that the significance of independent variables (from strong to weak) is: Type of Travel > Flight per Year > Airline Status > Age > Loyalty > Gender > Arrival Delay in Minutes > Departure Delay in Minutes > Price Sensitivity > Eating and Drinking > Total Frequent Flyer Accounts > Class > Shopping Amount.

![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/correlation.jpg)

## Associate Rule Mining
Association rules are if-then statements that help the group to find the relationships between the attributions. An associate rule has two parts: an antecedent and a consequent. In this project, there is a need to find out the relationship between the detractors (the consequent) and the other indicators (the antecedent). The associate rules will provide a bunch of relationships that can be interpreted -- if a customer is a, b, c, and d, then this person would more likely to be a detractor.

![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/associate%20rule%20mining.png)

## Decision Tree & SVM 
Decision tree is a decision algorithm that represents classification rules. This method empowers predictive models with high accuracy, stability and ease of interpretation. The target variable is categorical, and the decision tree calculate the probability that a given record belong to each of the category or to classify the record by assigning it to the most likely class (or category).  A Support Vector Machine (SVM) is a discriminative classifier formally defined by a separating hyperplane. In other words, given labeled training data (supervised learning), the algorithm outputs an optimal hyperplane which categorizes new examples. In two-dimensional space this hyperplane is a line dividing a plane in two parts where in each class lay in either side. 
The decision tree and SVM modeling for this project is as below. 

![plot](https://github.com/JKYang01/Customer-Churn-in-the-Airline-Industry/blob/main/data%20visualization/Decission%20Tree%20and%20SVM.png)
