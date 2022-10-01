# Association-rule-mining-using-Apriory-algorithm
Market-Basket Analysis
A dataset “Online Retail “has been analyzed which consists of the Member number, date of transaction and the Item Description of the retail company. 
Business Problem: A retail company Instamart is recently facing a huge loss in its sales. The retailers are trying to gain insights into which items are frequently purchased together by customers to increase their sales. 
Source:  http://archive.ics.uci.edu/ml/machine-learning-databases/00352/ 

 

# Step 1: Importing the dataset and creating the data frame 

getwd() 

setwd("C:/Users/AnishaPc/Desktop") 

groceries <- read.csv("Groceries_dataset.csv") 

# Step 2: Checking NA values 

sum(is.na(groceries)) 

# Step3: Installing the required packages and libraries  

library(arules) 

library(arulesViz) 

library(ggplot2) 

library(plyr) 

library(data.table) 

#Step 4: Visualizing the structure of the data frame 

str(groceries) 

#Step 5 : Sorting Groceries with Member number 

sorted <- groceries[order(groceries$Member_number),] 

#Step 6: Converting item description to categorical format 

sorted$Member_number <- as.numeric(sorted$Member_number) 

str(sorted) 

#Step 7: Grouping all the items that were bought together by the same customer on the same date 

itemList <- ddply(sorted, c("Member_number","Date"), function(df1) paste(df1$itemDescription, collapse = ",")) 

head(itemList,15) 

#Step 8: Removing member number and date 

itemList$Member_number <- NULL 

itemList$Date <- NULL 

colnames(itemList) <- c("itemList") 

write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE) 

head(itemList) 

#Step 9: Converting CSV file to Basket Format  

trans = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1); 

print(trans) 

#Step 10: Removing quotes from Transaction 

trans@itemInfo$labels <- gsub("\"","",trans@itemInfo$labels) 

summary(trans) 

# Step 11: Calculating support for frequent items 

frequentItems <- eclat(trans, parameter = list(supp = 0.05, maxlen = 15))  
inspect(head(frequentItems,10)) 
# Step 12: plotting frequent items 
itemFrequencyPlot(trans, topN=10, type="absolute", main="Item Frequency") 
## Step 13: Finding Association Rules 
rules <- apriori(trans) 
rules 
# Step 14: Creating Data table to analyze the Apriori algorithm with various support level 

#  Support and confidence values 

supportLevels <- c(0.05, 0.01, 0.005,0.001) 

confidenceLevels <- c(0.5,0.45,0.4,0.35,0.3,0.25,0.2,0.15,0.1) 

# Step 16: Empty integers  

rules_sup5 <- integer(length=9) 

rules_sup1 <- integer(length=9) 

rules_sup0.5 <- integer(length=9) 

rules_sup0.1 <- integer(length=9) 

# Step 17: Apriori algorithm with a support level of 5% 

for (i in 1:length(confidenceLevels)) { 
  rules_sup5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1],  

                                                        conf=confidenceLevels[i], target="rules")))} 

  # Apriori algorithm with a support level of 1% 

for (i in 1:length(confidenceLevels)){ 

   

  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2],  

                                                        conf=confidenceLevels[i], target="rules")))} 

  # Apriori algorithm with a support level of 0.5% 

for (i in 1:length(confidenceLevels)){ 

  rules_sup0.5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3],  

                                                          conf=confidenceLevels[i], target="rules")))} 

# Apriori algorithm with a support level of 0.1% 

for (i in 1:length(confidenceLevels)){ 
  rules_sup0.1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[4],  
                                                          conf=confidenceLevels[i], target="rules")))} 
num_rules <- data.table(rules_sup5, rules_sup1, rules_sup0.5, rules_sup0.1, confidenceLevels) 
ggplot(data=num_rules, aes(x=confidenceLevels)) + 
 # Step 18: Plot line and points (support level of 5%) 

  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +  

  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) + 

  # Plot line and points (support level of 1%) 

  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 

  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) + 

  # Plot line and points (support level of 0.5%) 

  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +  

  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) + 
   
  # Plot line and points (support level of 0.1%) 

  geom_line(aes(y=rules_sup0.1, colour="Support level of 0.1%")) + 

  geom_point(aes(y=rules_sup0.1, colour="Support level of 0.1%")) + 
  # Labs and theme 

  labs(x="Confidence levels", y="Number of rules found",  

       title="Apriori algorithm with different support levels") + 

  theme_bw() + 

  theme(legend.title=element_blank()) 
## Step 19 : Rules with specified parameters (support=0.1% and confidence 10% with a minimum length of 2) 
rules <- apriori(trans, parameter=list(minlen=2, 

                                       supp=0.001,  

                                       conf=0.1)) 

summary(rules) 
inspect(head(rules,20)) 
## Step 20: Rules sorted by confidence. 
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)  
inspect(head(rules_conf,20)) 
## Rules sorted by Highest Lift. 
rules_lift <- sort (rules, by="lift", decreasing=TRUE)  
inspect(head(rules_lift,20)) 
subrules <- rules[quality(rules)$confidence > 0.12] 
subrules 
## set of 71 rules 
head(inspect(subrules)) 
plot(subrules, method = "grouped", control = list(k = 50),engine="grid")  
#This clearly shows a strong association between {whole milk, yougurt} & {sausage}. 
## Removing Redundancy 
redundent <- is.redundant(rules, measure="lift") 
which(redundent) 
rules.pruned <- rules[!redundent] 
inspect(rules.pruned) 
#Hence, there are no redundant rules now. 
#Graph-based visualization offers a very clear representation of rules but they tend to easily become cluttered and thus are only viable for very small sets of rules. 
plot(rules.pruned, method = "graph") 
#10 rules with the highest lift. 
subrules2 <- head(rules.pruned, n = 10, by = "lift") 
inspect(subrules2) 
plot(subrules2, method = "graph", engine="igraph") 

## Step 22: conditional rules 
subrules3 <- rules.pruned[quality(rules.pruned)$confidence > 0.12 & quality(rules.pruned)$lift > 1.2] 

subrules3 <- head(subrules3, n = 10, by = c("lift","confidence")) 

inspect(subrules3) 

plot(subrules3, method = "graph",engine="igraph") 

plot(subrules3, method = "paracoord", control=list(reorder=TRUE)) 

**RESULT:**
Fig1: This depicts 10 rules with the highest lift after considering rules with confidence > 0.12 and lift > 1.2 

This also depicts that there are higher chances of a customer buying sausage if the customer has purchased whole milk & yogurt. 


Fig 2: We have analyzed the graph above, with the “Apriori algorithm having various support levels”, 

Support Level 5%: We have almost negligible rules so it’s of no use for us. 

Support Level 1%: We have some rules but at a very low confidence level of around 3.5%. We need to look at a support level below 1% to get some rules with a reasonable level of confidence. 

Support Level 0.5%: It has almost 25 rules with a confidence level of 10%. 

Support Level 0.1%: It has almost 120 rules with a confidence level of 10%. 

Hence considered a support level of 0.1% and a confidence level of 10%. 

But, with default parameters for support (0.1) the algorithm is not returning any rules. Therefore we plotted the highest lift rule after considering” rules with confidence > 0.12 and lift > 1.2” 

 

After sorting the rules with the highest confidence and highest lift, we easily observed that there is a strong association in {whole milk, yogurt} => {sausage} as it has a high Lift of > 2 and a good confidence score as well. (Fig. 3) 

Hence there are higher chances of a customer buying sausage if he has purchased whole milk & yogurt. 


**CONCLUSION AND RECOMMENDATION**

1.Hence from this above observation, we recommend the retail store manager to place the commodities such as whole milk, yogurt, and sausages on the nearby shelves and   increase their quantity to enhance profits and accelerate sales in the store.  

2.It is also recommended to increase customer engagement to help boost sales and increase RoI thereby improving customer experience. 

3.Finally, through the association rule mining and market basket analysis method the retail store can optimize marketing strategies and campaigns to help understand     customers better and therefore identify customer behavior and patterns. 
