# Instacart Problem ------ Kaggle
setwd("~/Desktop/My_Data/kaggle/instacart_problem")

# Load the required libraries which are required for the analysis
library(dplyr)
library(ggplot2)

# First all the required datasets
aisles <- read.csv("/aisles.csv")
order_products_train <- read.csv("/order_products__train.csv")
orders <- read.csv("/orders.csv")
products <- read.csv("/products.csv")
departments <- read.csv("/departments.csv")

str(order_products_train)
str(aisles)
str(products)

# If a particular datasets has various different files then work on it one by one to get a bigger idea of the problem

# Analysing the "ORDERS" file first

names(orders)
nrow(orders) # counting the total number of observations in the dataset
View(orders)
table(is.na(orders)) # Checking the missing values in the file
colSums(is.na(orders)) #Checking the missing values column wise
str(orders)

##### --------- WHEN DO PEOPLE ORDERS ??

### ---- AT WHAT HOUR OF THE DAY MOST OF THE ORDERS ARE PLACED
options(scipen=10000) ### ----- This is used to show number of orders in numbers
max_order_hour <- orders %>% ggplot(aes(x = order_hour_of_day)) + geom_histogram(stat = "count",fill="blue") + xlab("Most Ordered hour of the Day") + ylab("Number of Orders") + ggtitle("Maximum number of Orders placed in a Day(in terms of Hours)")
max_order_hour
# This shows that most of the ordes are placed between 10am to 8pm

### ---- AT WHICH DAY OF THE WEEK MOST OF THE ORDERS ARE PLACED "OREDR_DOW - INDICATES ORDERS DAY OF THE WEEK"
max_order_day <- orders %>% ggplot(aes(x=order_dow)) + geom_histogram(stat="count",fill="red") + xlab("Most Ordered day of the Week") + ylab("Number of Orders") + ggtitle(" Maximum number of orders placed in a week(in terms of Days)")
max_order_day
# This shows that most of the orders are placed during the "0th and 1st day" of the week.            

#### ---------- WHEN DO THEY REOREDR AGAIN ??
max_reordered_item <- orders %>% ggplot(aes(x=days_since_prior_order)) + geom_histogram(stat = "count",fill="blue") + xlab("Days since prior order") + ylab("Number of Orders") + ggtitle("After How many Days Customer buy again")
max_reordered_item
# This shows that most of the orders are reordered after 7th day or after the 3oth day of the prior order

day_of_week_vs_hour <- orders %>%group_by(order_dow,order_hour_of_day) %>% summarise(count = n())
library(tidyr) # this library is used to convert the dataframe to heat map dataset
heat_map_data <- day_of_week_vs_hour %>% spread(order_hour_of_day,count) %>% as.data.frame() # This will convert the 
heat_map_plot <- heat_map_data
rownames(heat_map_plot) <- heat_map_data[,1]
colnames(heat_map_plot) <- colnames(heat_map_data)
heat_map_plot <- heat_map_plot[,-1]
heat_map_plot_mat <- as.matrix(heat_map_plot)
heat_map_plot <-  heatmap(heat_map_plot_mat, Rowv=NA, Colv=NA,scale="column" ,col= heat.colors(256), margins=c(5,10),xlab ="Hour of the Day",ylab ="Day of the Week")

# Analysing the "Orders product Train" file to analyse the dataset
names(order_products_train)

# What is the most ordered product
most_ordered_product <- order_products_train %>% group_by(product_id) %>% summarise(count=n()) %>% top_n(20)

# Merge the "most ordered product" with the "product" file
most_demandable_product <- merge(most_ordered_product,products,by="product_id")
# plotting the results to get better insights
most_demandable_product %>% ggplot(aes(reorder(product_name,count),count)) + geom_bar(stat="identity",fill="blue") + xlab("Top 20 Ordered Products") + ylab("No. of Orders") + title("Most Ordered Top 20 Products") +coord_flip()

# What is the most reordered Product
most_reorder_product <- filter(order_products_train,reordered == "1") %>% group_by(product_id) %>% summarise(count=n()) %>% top_n(20)

# merging the products to get the product name
most_reorder_product_name <- merge(most_reorder_product,products,by="product_id") 
View(most_reorder_product_name)

# plotting the product for getting the better insights
most_reorder_product_name %>% ggplot(aes(reorder(product_name,count),count)) + geom_bar(stat="identity",fill="blue") + ylab("No. of Orders") + xlab("Top 20 Reordered Products")+ coord_flip()

# How many percentage of people reorder again
reordered_rate <- order_products_train %>% group_by(reordered) %>% summarise(count = n()) %>% mutate(percentage = count/sum(count)*100)
reordered_rate$percentage <- round(reordered_rate$percentage,2)
reordered_rate$reordered <- as.factor(reordered_rate$reordered)
reordered_rate$percentage <- paste(reordered_rate$percentage,"%",sep="")

levels(reordered_rate$reordered)[1] <- "Not Reordered"
levels(reordered_rate$reordered)[2] <- "Reordered"
reordered_rate %>% ggplot(aes(x=reordered,y=count,fill=reordered)) + geom_bar(stat ="identity",width = 0.5) + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + geom_text(aes(label=percentage),vjust=0) + xlab("Rate of Reordered") + ylab("Percentage of Items Reordered") + ggtitle("How Many Percentage of Items Reordered Again")

# Considering the Products file
# What is the percentage of Organic and Inorganic
names(products)

library(stringr)
products <- products %>% mutate(organic= ifelse(str_detect(str_to_lower(products$product_name),"organic"),"organic","not organic"),organic = as.factor(organic))

# Detect the percentage of organic vs unorganic in the order list
# Here first we are first gathering the products which are organic then merging with the products in the order list
product_organic_unorganic <- products %>% right_join(order_products_train,by="product_id") %>% group_by(organic) %>% summarise(count=n()) %>% mutate(percentage = count/sum(count)*100)
product_organic_unorganic$percentage <- round(product_organic_unorganic$percentage,2)
product_organic_unorganic$percentage <- paste(product_organic_unorganic$percentage,"%","")
#View(product_organic_unorganic)
product_organic_unorganic %>% ggplot(aes(x=organic,y=count,fill=organic)) + geom_bar(stat="identity",width = 0.3) + geom_text(aes(label=product_organic_unorganic$percentage),vjust=0) + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + xlab("Types of Products in Order list") + ylab("Percenatge of Types of Products") + ggtitle("Percentage of Organic Products in the Order List")

# Reordering of organic and not organic products
# merging the order_product_train and the products data for getting the reorder item
reorder_organic_item <- merge(products,order_products_train,by="product_id")
reordered_items_type <- reorder_organic_item %>% filter(reordered == "1") %>% group_by(organic) %>% summarise(count = n()) %>% mutate(percent=count/sum(count)*100)
reordered_items_type$percent <- round(reordered_items_type$percent,2)
reordered_items_type$percent <- paste(reordered_items_type$percent,"%","")
reordered_items_type %>% ggplot(aes(x=organic,y=count,fill=organic)) + geom_bar(stat="identity",width = 0.3) + geom_text(aes(label=reordered_items_type$percent),vjust=0) + theme(axis.text.y=element_blank(),axis.ticks.y = element_blank()) + xlab("Types of Products Reordered") + ylab("Percentage Distribution of Reordered Products types") + ggtitle("How much 'Organic' Products are Reorder")

product_distribution <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
product_distribution <- product_distribution %>% left_join(departments,by="department_id")
product_distribution <- product_distribution %>% left_join(aisles,by="aisle_id")

product_distribution2<-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(product_distribution, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

library(treemap)
treemap(product_distribution2,index=c("department","aisle"),vSize="onesize",vColor="department",palette="Set3",title="",sortID="-sumcount", border.col="#FFFFFF",type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")

## Which Department products are highly oredered
# Merging the "Departments" , "Products" and "order_products train" file
dept_prod_data <- merge(departments , products,on = "department_id")
order_dept_prod <- merge(dept_prod_data,order_products_train ,on = "product_id")
View(order_dept_prod)
## Finding the Most ordered product from the department
dept_prod_data_grouped <- order_dept_prod %>% group_by(department_id) %>% summarise(count = n()) 
dept_prod_data_grouped <- dept_prod_data_grouped[order(-dept_prod_data_grouped$count),]

## Appending the department name
dept_order <- merge(dept_prod_data_grouped , departments , on = "department_id")
dept_order <- dept_order[order(-dept_order$count),]
top_dept_order <- head(dept_order,10)
## most ordered items are from "Produce Group"
options("scipen" = 10)
top_dept_order %>% ggplot(aes(reorder(department,count),count,fill = count)) + geom_bar(stat="identity") + xlab("Department Name") + ylab("Number of Items ordered") + ggtitle("Highest Items sold Department wise") 

## From which department most items are reordered??
View(order_dept_prod)
## Filter the data containg itmes which are reordered
top_reorder_dept <- filter(order_dept_prod ,reordered == 1 )

top_reorder_dept <- top_reorder_dept %>% group_by(department,reordered) %>% summarise(count = n())
top_reorder_dept <- top_reorder_dept[order(-top_reorder_dept$count),]
top_10_reorder_dept <- head(top_reorder_dept,10)
top_10_reorder_dept %>% ggplot(aes(reorder(department,count),count,fill = count)) + geom_bar(stat="identity") + xlab("Department Name") + ylab("Number of Items Reordered") + ggtitle("Highest Items sold Department wise") 















