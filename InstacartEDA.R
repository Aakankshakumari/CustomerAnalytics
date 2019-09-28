
# Set working directory
setwd("D:/Customer Analytics/data")

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)

# Import data set
orders <- fread('data/orders.csv')
aisles <- fread('data/aisles.csv')
departments <- fread('data/departments.csv')
order_products_prior <- fread("data/order_products_prior.csv")
order_products_train <- fread('data/order_products_train.csv')
orders <- fread('data/orders.csv')
products <- fread('data/products.csv')

# Viewing the Dataset
kable(head(orders,10))
kable(head(order_products_prior,10))
kable(head(order_products_train,10))
kable(head(products,10))
kable(head(aisles,10))
kable(head(departments,10))

# Explolatory Data Analysis
## Recode variables
## We should do some recoding and convert character variables to factors.
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))


## When do people order?
##Let's have a look when people buy groceries online.
### Hour of Day
###There is a clear effect of hour of day on order volume. Most orders are between 9.00-18.00
orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="blue")

### Day of Week
###There is a clear effect of day of the week. Most orders are on days 0 and 1
### 0 means Sunday and 1 means Monday
orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="blue")


### When do they order again?
####People tend to place a new order mostly within 7 days of the last order, predominantly exactly 7 days after. The 30 day spike seems to be a long-tail effect.
orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="blue")

#### How many prior orders are there?
###We can see that there are always at least 3 prior orders.

orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")

#### How many items do people buy?
###How many items do people buy?
##Let's have a look how many items are in the orders. We can see that people most often order around 5 items. The distributions are comparable between the train and prior order set.
order_products_train %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug() + 
  coord_cartesian(xlim=c(0,80))

### Bestsellers
##Let's have a look which products are sold most often (top10). And the clear winner is: Bananas
tmp <- order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp)
tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())
## How often do people order the same items again?
## 59% of the ordered items are reorders.
tmp <- order_products_train %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)
tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

## Most often reordered
##Now here it becomes really interesting. These 10 products have the highest probability of being reordered.
tmp <-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

kable(tmp)
tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

## Which item do people put into the cart first?
## People seem to be quite certain about Multifold Towels and if they buy them, put them into their cart first in 66% of the time
tmp <- order_products_train %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

kable(tmp)
tmp %>% 
  ggplot(aes(x=reorder(product_name,-pct), y=pct))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))

## Association between time of last order and probability of reorder
## This is interesting: We can see that if people order again on the same day, they order the same product more often. Whereas when 30 days have passed, they tend to try out new things in their order.
order_products_train %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="red")
## Association between number of orders and probability of reordering
##Products with a high number of orders are naturally more likely to be reordered. However, there seems to be a ceiling effect.
order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n,y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="red")+
  coord_cartesian(xlim=c(0,2000))

## Organic vs Non-organic
##What is the percentage of orders that are organic vs. not organic?
products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))

tmp <- order_products_train %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))
kable(tmp)
tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity")

## Reordering Organic vs Non-Organic
## People more often reorder organic products vs non-organic products.

tmp <- order_products_train %>% left_join(products,by="product_id") %>% group_by(organic) %>% summarize(mean_reordered = mean(reordered))
kable(tmp)
tmp %>% 
  ggplot(aes(x=organic,fill=organic,y=mean_reordered))+geom_bar(stat="identity")

## Visualizing the Product Portfolio
## Here is use to treemap package to visualize the structure of instacarts product portfolio. In total there are 21 departments containing 134 aisles.

library(treemap)
tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")

tmp2<-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

## How are aisles organized within departments?
treemap(tmp2,index=c("department","aisle"),vSize="onesize",vColor="department",palette="Set3",title="",sortID="-sumcount", border.col="#FFFFFF",type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")


## Exploring Customer Habits
### Here i look for customers who just reorder the same products again all the time. To search those I look at all orders (excluding the first order), where the percentage of reordered items is exactly 1 (This can easily be adapted to look at more lenient thresholds). We can see there are in fact 3,487 customers, just always reordering products.
##Customers reordering only
tmp <- order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(m = mean(reordered),n=n()) %>% 
  right_join(filter(orders,order_number>2), by="order_id")

tmp2 <- tmp %>% 
  filter(eval_set =="prior") %>% 
  group_by(user_id) %>% 
  summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>% 
  filter(percent_equal == 1) %>% 
  arrange(desc(n_equal))

datatable(tmp2, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))
## The customer with the strongest habit
## The coolest customer is id #99753, having 97 orders with only reordered items. That's what I call a strong habit. She/he seems to like Organic Milk :-)

uniqueorders <- filter(tmp, user_id == 99753)$order_id
tmp <- order_products_prior %>% 
  filter(order_id %in% uniqueorders) %>% 
  left_join(products, by="product_id")

datatable(select(tmp,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

### Let's look at his order in the train set. One would assume that he would buy "Organic Whole Milk" and "Organic Reduced Fat Milk":
tmp <- orders %>% filter(user_id==99753, eval_set == "train")
tmp2 <- order_products_train %>%  
  filter(order_id == tmp$order_id) %>% 
  left_join(products, by="product_id")

datatable(select(tmp2,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 't'))
## Prediction 100% correct.
