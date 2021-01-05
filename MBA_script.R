library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(arules)
library(arulesViz)

orders=readRDS("C:/Users/GOKUL SUSEENDRAN/Documents/Market Basket Analysis(Working)/orders_products.RDs")
orders=data.frame(orders)
colnames(orders)
str(orders)
head(orders)
dim(orders)
#Data Cleaning

orders$days_since_prior_order[orders$days_since_prior_order=="NA"]
orders$days_since_prior_order <- ifelse(is.na(orders$days_since_prior_order),0,orders$days_since_prior_order)
sum(is.na(orders$days_since_prior_order))

#Exploratory Data Analysis


summary(orders)


cv=data.frame(orders$product_name,orders$aisle,orders$department,orders$eval_set,orders$row_type)
head(cv)
no=data.frame(orders$order_id,orders$add_to_cart_order,orders$reordered,orders$user_id,orders$order_number,orders$order_dow,orders$order_hour_of_day,orders$days_since_prior_order)
head(no)


#correlation matrix
cp=cor(no)
corrplot(cp,method = "square",addCoef.col = "black",title="Correlation Plot")

#insights using groupby  fns


#####which day sales is more######
max_sales=orders %>% 
  group_by(order_dow) %>% 
  summarise(no_of_sales=n())
max_sales
#######how many reorders######
max_reordered=orders %>% 
  group_by(reordered) %>% 
  summarise(no_of_customers=n())
max_reordered

#######regular customers#######
reg_cus=orders %>% 
  group_by(user_id) %>% 
  summarise(no_of_regular_customers=n())
reg_cus
reg_cust= reg_cus %>% 
  arrange(desc(no_of_regular_customers))
reg_cust=head(reg_cust, n = 10)
reg_cust

#########which hour the sales is more#######
max_sales_hr=orders %>% 
  group_by(order_hour_of_day) %>% 
  summarise(no_of_sales=n())
max_sales_hr

#######which products sold more######

max_prod=orders %>% 
  group_by(product_name) %>% 
  summarise(no_of_products=n())
max_prod

max_prod=data.frame(max_prod)
head(max_prod)

mpss=max_prod %>% 
  arrange(desc(no_of_products))
max_prod_df=data.frame(head(mpss,n=10))
max_prod_df

# days since prior order#########(later do it)
days_pr_ord=orders %>% 
  group_by(days_since_prior_order) %>% 
  summarise(no_of_people=n())
days_pr_ord
dspo=days_pr_ord %>% 
  arrange(desc(no_of_people))
dspo=head(dspo,n=10)
dspo
#max days since prior order######(later do it)


#######regular customer and reorder#####
reg_reor=orders %>% 
  filter(user_id,reordered==1) 
head(reg_reor)

user_id_reor=reg_reor %>% 
  select(user_id,reordered)
user_id_reor  

reg_reo=data.frame(user_id_reor)
head(reg_reo)

s=reg_reo %>% 
  group_by(user_id,reordered) %>% 
  summarise(no_of_customers_reordered=n())
s
f=s %>% 
  arrange(desc(no_of_customers_reordered))
f
df=f %>% 
  select(user_id,no_of_customers_reordered)
df
reg_cust_reo_df=data.frame(head(df,n=10))
reg_cust_reo_df

###########final dataframes#############

max_sales_df=data.frame(max_sales)       #!
max_sales_df

reorderd_df=data.frame(max_reordered)         #2
reorderd_df

reg_cust_df=data.frame(reg_cust)       #3
reg_cust_df

hour_max_sale_df=data.frame(max_sales_hr)              #4
hour_max_sale_df


max_prod_df=data.frame(head(mpss,n=10))      #5
max_prod_df

#dspod_df=data.frame(dspo)                     #6
#dspod_df
#rm(dspod_df)

days_prior_order_df=data.frame(dspo)     ###not necessary###
days_prior_order_df

reg_cust_reo_df=data.frame(head(df,n=10))                    #7
reg_cust_reo_df

#barplots
ggplot(max_sales_df, aes(x = order_dow, y = no_of_sales)) + theme_classic() + geom_bar(stat = "identity")+ ggtitle("MAXIMUM SALES IN A WEEK")
ggplot(reorderd_df, aes(x = reordered, y = no_of_customers)) + theme_classic() + geom_bar(stat = "identity")+ ggtitle("NO OF PEOPLE REORDER")
ggplot(reg_cust_df, aes(x = user_id, y = no_of_regular_customers)) + theme_classic() + geom_bar(stat = "identity")+ ggtitle("TOP 10 REGULAR CUSTOMERS")
ggplot(hour_max_sale_df, aes(x = order_hour_of_day, y = no_of_sales)) + theme_classic() + geom_bar(stat = "identity")+ggtitle("MAX SALES BY HOURS IN A DAY")
ggplot(max_prod_df, aes(x = product_name, y = no_of_products)) + theme_classic() + geom_bar(stat = "identity")+ggtitle("SALES OF TOP 10 PRODUCTS")
ggplot(days_prior_order_df, aes(x = days_since_prior_order, y = no_of_people)) + theme_test() + geom_bar(stat = "identity")+ ggtitle("DAYS SINCE PRIOR ORDER")
ggplot(reg_cust_reo_df, aes(x = user_id, y = no_of_customers_reordered)) + theme_test() + geom_bar(stat = "identity")+ggtitle("REGULAR CUSTOMER REORDERS")

orders$order_dow
unique(orders$product_name)

#################Aishu's Work#################
#Exploratory Data Analysis
#product
prd <- orders %>%
  arrange(user_id, order_number, product_name) %>%
  group_by(user_id, product_name) %>%
  mutate(product_time = row_number())
prd
ungroup()

head(prd,5)
prd <- prd %>%
  group_by(product_name) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),    
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2))
head(prd,10)
#calculating the probability
prd$prod_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders
prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
head(prd,20)

ggplot(prd, aes(x=prd$product_name, y=prd$prod_probability,bins=30)) + geom_histogram(stat ="identity")

#user
users<- orders%>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )
head(users,10)
us <- orders %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_distinct_products = n_distinct(product_name)
  )

head(us,10)



####Apriori#######
#Apriori
order_baskets <- orders %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))
transactions <- as(order_baskets$basket, "transactions")

itemset<- apriori(transactions, parameter=list(supp=0.001, conf=0.8, target="frequent"))
length(itemset)
summary(itemset)
inspect(sort(itemset[1:10], decreasing=T))
itemrules<-apriori(transactions, parameter=list(supp=0.001, conf=0.1, target="rules"))
length(itemrules)
summary(itemrules)
inspect(sort(itemrules[1:10], decreasing=T))
summary(itemrules)
supp<-sort(itemrules, by="support", decreasing=TRUE)
inspect(supp[1:10])
conf<-sort(itemrules, by="confidence", decreasing=TRUE)
inspect(conf[1:10])
li<-sort(itemrules, by="lift", decreasing=TRUE)
inspect(li[1:10])
##product combinations
#second product
rhsrules<- apriori(data=transactions,parameter=list(supp=0.01, conf=0.10), 
                   appearance=list(default="rhs",lhs="Bag of Organic Bananas"),control=list(verbose=F))
rhsrules<-sort(rhsrules, decreasing=T, by="confidence")
inspect(rhsrules[1:2])
#first product
lhsrules<- apriori(data=transactions,parameter=list(supp=0.01, conf=0.10), 
                   appearance=list(default="lhs",rhs="Bag of Organic Bananas"),control=list(verbose=F))
lhsrules<-sort(lhsrules, decreasing=T, by="confidence")
inspect(lhsrules[1:5])

