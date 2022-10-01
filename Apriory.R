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

