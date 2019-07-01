#load packages ----

pacman::p_load(caret,mlbench,ggplot2,dplyr,arules,arulesViz,readr,tidyr,scales,Matrix)


#load data ----

orders.dataset <- read.csv2('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/orders_translated.csv')

line.item.dataset <- read.csv2('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/lineitems.csv')

trans <-read.transactions('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/trans - trans.csv', format = 'basket', sep = ',', header = F, rm.duplicates = T)

trans.csv <- read.csv2('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/trans - trans.csv', header = F)

x <- as(trans, 'matrix')

x <- as.data.frame(x)

table(x$`8MO0001`)

#Exploring the data----

  #We have 10453 registered transacions 
  #The order Id are both in the line item dataset and the orders data set.


  #I remove the orders that are individual

require("dplyr")

orders.two.or.more <- line.item.dataset %>%
  group_by(id_order) %>%
  filter(n() > 1)
  
  #I can still see that there are 48 K orders left, need to match it with the transactions
  #I remove the not completed


orders_completed <- orders.dataset %>% filter(state == "Completed")

  #check thenumber, how many completed orders are there?

length(which(orders_completed$id_order %in% orders.two.or.more$id_order))

  #We filter

match <- which(orders.two.or.more$id_order %in% orders_completed$id_order)

orders.trans <- orders.two.or.more[match,]

length(unique(orders.trans$id_order))

  #transactions and unique orders now match

  #Checking price relations between the files

price.comparison.table <- orders.trans[,c(2,4,7)]

price.comparison.table$manual.price <- price.comparison.table$product_quantity*price.comparison.table$unit_price


price.comparison.table <- price.comparison.table %>%
                            group_by(id_order) %>%
                            summarise(order.price = sum(unit_price),
                                      manual.order.price = sum(manual.price))

orders.dataset.for.join <- orders.dataset

price.comparison.table<- left_join(price.comparison.table,
                                   orders.dataset.for.join,
                                   by = c('id_order'='id_order'))  

price.comparison.table$difference <- round(price.comparison.table$total_paid - price.comparison.table$order.price)

  
ggplot(price.comparison.table,aes(x=difference))+
  geom_bar(bins = 40)+
  coord_cartesian(xlim = c(0,50))

  #most of the data difference conforms to reasonable patterns that could be explained
  #by coupons, discounts and shipping cost. Plotting difference percentage


price.comparison.table$percentual.difference <- price.comparison.table$difference/price.comparison.table$total_paid

price.comparison.table$percentual.difference <- percent(price.comparison.table$percentual.difference) 

ggplot(price.comparison.table, aes(x=percentual.difference)) +
  geom_bar(bins = 40)
  

  #We can obseve that set percentages have a tendency to be repeated, which makes sense
  #in the context of regular dsicounts or other promotions.



#Epic 2 ----

#Exploring transactions ----

inspect(trans) #prints the transactions in the file

length(trans) #Consist of the 10454 transactions we had view before

size(trans) #number of items per transaction ?

LIST(trans) #creates a list of the elements in thesparce matrix

itemLabels(trans) #each item on each transaction


  #Visualizing the top baskets 

itemFrequencyPlot(trans, type = 'relative', support = percent(0.20), topN = 5)

itemFrequencyPlot(trans, type = 'relative', topN = 5, population = trans, lift = F)



  #For some reason i need to write the following lines in order to visualize 
  # graphics.off()
  # par('mar')
  # par(mar=c(1,1,1,1))


image(trans) #Visualizing the transactions in our data set

image(sample(trans, 1000)) #Visualizing a smaller sample

#frequent items

frequent.items <- eclat(trans, 
                        parameter = list(supp = 0.02, maxlen = 5))

#Highest I could get the parameters with at least 4 frequent products 


inspect(frequent.items) 

#getting a plot for item frequency

itemFrequencyPlot(trans, topN = 20, type = 'absolute')

# Apriori processing ----


apriori.baskets <- apriori(trans,
                           parameter = list(supp = 0.0025, conf = 0.35, minlen = 2))

  #min lenght doesnt make a difference when up  to 2 because we have already rejected
  #transactions with lenght one so no possible rule can come  from this paramenter
  #until the minlen = 2

inspect(apriori.baskets[3,])

top.rules.by.support <- sort(apriori.baskets,
                             decreasing = T,
                             na.last = NA,
                             by = 'lift')

inspect(head(top.rules.by.support, 2))

#option to visualize rules directly

ruleExplorer(apriori.baskets)

#Top 5 rules I could get. 
#You can select the rules like rows from a dataframe
#to  see specific rules you can sort and then call the number you want with 
# inspect(head(x, Number of results))


#Checking the results 

summary(apriori.baskets)

#Trying to select an specific product Rule

#APP2155


specific.rule <- subset(apriori.baskets, items %in% 'APP2155')

#We need to use inspect to visualize rule objects

inspect(specific.rule)

#Only that rule got stored, succesful test. 


#Now we check for redundancy on the rules created

isTRUE(is.redundant(apriori.baskets))

#No redundant rules. 


#Plotting the rules ----


plot(x = apriori.baskets)

#Plotting specific rule

plot(apriori.baskets[1:3],
     method = 'graph',
     control = list(type='grid'))


#Epic 3 ----

#Loading the new data with categories

products.with.categories <- read_table2('c:/Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/products_with_category.txt')

summary(products.with.categories)


#they have a different amount of items, need to find out the difference

trans.2<- read.csv2('c:/Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/trans - trans.csv', sep = ',')

not.in.list<- c()

comparacion<-trans@itemInfo$labels %in% products.with.categories$sku

faltantes <-trans@itemInfo$labels[!comparacion]

faltantes.df <- cbind(faltantes,'Unknown')

colnames(faltantes.df) <- c('sku', 'manual_categories')

products.with.categories <- rbind(products.with.categories, faltantes.df)

products.with.categories<- products.with.categories[-4230,]


#after adding the missing values to our list of products as unknown, we are reading to

#proceed with the creation of the new level for the transaction matrix. 

trans@itemInfo[["category"]]<-trans@itemInfo[["labels"]]

test <- trans@itemInfo[["labels"]]

test <- as.data.frame(test)

colnames(test) <- c('sku')

test <- left_join(test,products.with.categories, by = c('sku','sku') )


test <- test[,2]

test <- as.character(test)

trans@itemInfo[["category"]] <- test

test.2<- trans@itemInfo[["category"]]


new.test<- aggregate(x = trans,itemInfo(trans)[["category"]])

View(new.test)


#new apriori processing


category.rules <- apriori(new.test,
                           parameter = list(supp = 0.003, conf = 0.70, minlen = 2))


inspect(category.rules)


#loading blackwell products.

blackwell.products <- read.csv('c://Users/riqui/desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/Blackwell products.csv')


#transaction rule spoint between items and accesories, however information is inconclusive
#trying some feature engineering


products.with.categories.2 <- as.list(trans@itemInfo[["labels"]])


products.with.categories.2  <- as.data.frame(products.with.categories.2)

product.with.categories.2$brand_category <- product.with.categories.2$`trans@itemInfo[["labels"]]`  

colnames(product.with.categories.2) <- c('sku','brand_category')

product.with.categories.3 <- left_join(product.with.categories.2,products.with.categories,
                                       by = c('sku', 'sku'))

product.with.categories.3$brand_category <- paste(substr(product.with.categories.3$sku, 1,3),product.with.categories.3$manual_categories, sep = ' ')


product.with.categories.3 <- product.with.categories.3[,c(2)]

trans.4 <- trans

trans.4@itemInfo$category <- product.with.categories.3


trans.5 <- aggregate(x = trans.4,itemInfo(trans.4)[["category"]])

brand.category.rules <- apriori(trans.5,
                          parameter = list(supp = 0.0001, conf = 0.10, minlen = 2))

inspect(brand.category.rules)

ruleExplorer(brand.category.rules)

