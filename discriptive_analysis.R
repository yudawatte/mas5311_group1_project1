# Prerequisits
library(ggplot2)
library(sf)
library(geojsonR)
library(ggpubr)
library(reshape2)
library(ggcorrplot)
library(tidyverse)
library(dplyr)
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Dataset")

# Load and preprocess data
df <- read_csv('Dataset/train.csv')
df <- df %>%  mutate(id = as.character(id),
                     Driving_License = ifelse(Driving_License == 1,"Licensed","Not Licensed"),
                     Region_Code = as.character(Region_Code),
                     Previously_Insured = ifelse(Previously_Insured == 1,"Insured","Not Insured"),
                     Vehicle_Damage = ifelse(Vehicle_Damage == "Yes", "Damaged", "Not Damaged"),
                     Policy_Sales_Channel = as.character(Policy_Sales_Channel),
                     Response = ifelse(Response == 1, "Interested", "Not Interested")) 

# Gender proportions
df %>% group_by(Gender) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt))

# Gender wise response proportions
df %>% group_by(Gender, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt))

# Figure 1: Gender wise age distribution
df %>% select(Gender,Age) %>% arrange(Gender) %>% ggplot(aes(Age)) + 
  geom_boxplot(mapping = aes(x = Age, y = Gender , fill = Gender))+
  labs(title = "Gender wise age distributions",x = "Age",y = NULL) + 
  theme_minimal() + theme(legend.position="none")

# Gender wise age quartile margins
df %>% filter(Gender=='Male') %>% select(Age) %>% quantile(na.rm = TRUE)
df %>% filter(Gender=='Female') %>% select(Age) %>% quantile(na.rm = TRUE)

# Figure 2: Age quartile wise male and female responses
df_age_m <- df %>% filter(Gender=='Male') %>% select('Gender', 'Age', 'Response')
df_age_m$Age_Quartile <- cut(df_age_m$Age,quantile(df_age_m$Age),include.lowest=TRUE,labels= c('1','2','3','4'))

df_age_f <- df %>% filter(Gender=='Female') %>% select('Gender', 'Age', 'Response')
df_age_f$Age_Quartile <- cut(df_age_f$Age,quantile(df_age_f$Age),include.lowest=TRUE,labels= c('1','2','3','4'))

df_age = rbind(df_age_m,df_age_f)

df_age %>% group_by(Gender, Age_Quartile, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% 
  ggplot(aes(x=Age_Quartile, y=percent, fill=Response)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Gender, ncol=2) +
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) + 
  labs(title = "Age quartile wise male and female responses", x = "Age Quartile") +
  theme_minimal()+ 
  theme(strip.text = element_text(face = 'bold'), 
        plot.caption = element_text(face = 'italic'), 
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 3: Positive responses - male and female age quartile percentages
df_age %>% filter(Response=="Interested") %>% 
  group_by(Gender, Age_Quartile) %>% summarise(cnt = n()) %>% 
  mutate(percent = cnt/sum(cnt)*100) %>% 
  ggplot(aes(x=Age_Quartile, y=percent, fill=Gender)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Gender, ncol=2) +
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) + 
  labs(title = "Positive responses - male and female age quartile percentages", x = "Age Quartile") +
  theme_minimal()+ 
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 4: Previously insured status impact on the response
df %>% group_by(Previously_Insured, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% 
  ggplot(aes(x=Response, y=percent, fill=Response)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~Previously_Insured, ncol=2) +
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) + 
  labs(title = "Previously Insured Status effect on Response", y = NULL) +
  theme_minimal()+ 
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 5: Region wise responses
d1 <- df %>% group_by(Region_Code, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% arrange(Region_Code) 
d1 <- d1 %>%  mutate(order = as.numeric(Region_Code))

d2 <- d1 %>% filter(Response=='Not Interested') %>% select(percent)

d1 %>% ggplot(aes(x= reorder(Region_Code, order), y=percent, fill=Response)) + 
  geom_bar(stat="identity") + 
  labs(title = "Region wise Response Percentages", y = "%", x="Region Code") +
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),) +
  geom_hline(aes(yintercept=mean(d2$percent))) + 
  annotate("text", x=50, y=round(mean(d2$percent),2)-2,label=paste(round(mean(d2$percent),2),"%")) +
  geom_hline(yintercept=mean(d2$percent)-2*sd(d2$percent), linetype='dotted') + 
  annotate("text", x=50, y=mean(d2$percent)-2*sd(d2$percent), label="-2*sd") +
  geom_hline(yintercept=mean(d2$percent)+2*sd(d2$percent), linetype='dotted') + 
  annotate("text", x=50, y=mean(d2$percent)+2*sd(d2$percent), label="+2*sd")

ggsave("region_wise_response.jpg", width = 30, height = 10, units = "cm")

# Insurance impact on Response
df %>% group_by(Previously_Insured, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100)

# Figure 6: Driving license status effect on the response
df %>% group_by(Driving_License) %>% summarise(avg_age = mean(Age))

df %>% group_by(Driving_License, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% 
  ggplot(aes(x=Response, y=percent, fill=Response)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~Driving_License, ncol=2) +
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) + 
  labs(title = "Driving License status effect on Response", y = NULL) + theme_minimal() +
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 7: Vehicle age and responses
df %>% group_by(Vehicle_Age) %>% summarise(avg_premium = mean(Annual_Premium))

d3 <- df %>% group_by(Vehicle_Age, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100)
d3 <- d3 %>% mutate(age_order = case_when(Vehicle_Age=="< 1 Year"~1, Vehicle_Age=="1-2 Year"~2, TRUE ~ 3))

d3 %>% ggplot(aes(x=reorder(Vehicle_Age, age_order), y=percent, fill=Response)) +
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) +
  labs(title = "Vehicle Age impact on Premium and Response", x="Vehicle Age") + 
  theme_minimal() +
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 8: Vehicle condition and responses
df %>% group_by(Vehicle_Damage) %>% summarise(Average_Premium = mean(Annual_Premium))

df %>% group_by(Vehicle_Damage, Response) %>% summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% 
  ggplot(aes(x=Vehicle_Damage, y=percent, fill=Response)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste(round(percent,2), "%")),vjust = -.5) +
  labs(title = "Damage Status impact on Premium and Response", x="Vehicle Condition") +
  theme_minimal()+
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Figure 9: Sales channel with higher positive responses (min 100 responses)
d4 <- df %>% group_by(Policy_Sales_Channel, Response) %>% 
  summarise(cnt = n()) %>% mutate(percent = cnt/sum(cnt)*100) %>% 
  arrange(percent) 

df %>% select(Policy_Sales_Channel) %>% unique() %>% count()

df %>% group_by(Policy_Sales_Channel) %>% summarise(cnt = n()) %>% filter((cnt>=100)) %>% count()

d4 %>% filter((Response=="Interested") & (cnt>=100) & (percent>=15)) %>% select(Policy_Sales_Channel)  %>% unique() %>% count()

d4 %>% filter((Response=="Interested") & (cnt>=100)) %>% arrange(desc(percent)) %>% 
  ggplot(aes(x= reorder(Policy_Sales_Channel, percent), y=percent, fill=Response)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  labs(title ="Sales channels with higher positive responses(min 100 responses)", y = "%", x="Sales Channel") + 
  theme_minimal()+
  theme(strip.text = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'),
        panel.grid.major = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# Figure 10: Response wise vintage distribution
df %>% select(Response,Vintage) %>% arrange(Response) %>% ggplot(aes(Vintage)) + 
  geom_boxplot(mapping = aes(x = Vintage, y = Response , fill = Response)) +
  labs(title = "Vintage Distribution",x = "Vintage",y = NULL) + 
  theme_minimal() + 
  theme(legend.position="none")

########################################

# Association between response variable and each predictor variable
# response and gender, significant, males show higher interest
prop.table(table(Gender,Response),1)*100
chisq.test(Gender,Response)

# response and driving license, significant, driving license holders show higher interest of obtaining vehicle insurance policy
prop.table(table(Driving_License,Response),1)*100
chisq.test(Driving_License,Response)

# response and customer's region, significant 
prop.table(table(Region_Code,Response),1)*100
chisq.test(Region_Code,Response)

# response and holding vehicle insurance, significant, holder of vehicle insurance shows lesser interest
prop.table(table(Previously_Insured,Response),1)*100
chisq.test(Previously_Insured,Response)

# response and vehicle age, significant, interest increases with vehicle age
prop.table(table(Vehicle_Age,Response),1)*100
chisq.test(Vehicle_Age,Response)

# response and vehicle damage, significant, past experience of vehicle damage increases the interest
round(prop.table(table(Vehicle_Damage,Response),1)*100,1)
chisq.test(Vehicle_Damage,Response)

# response and age, significant, interested people are older than others
t.test(Age~Response)

# response and annual premium, significant, higher premium payers are interested in vehicle insurance policy
t.test(Annual_Premium~Response)

# response and vintage, no significant association
t.test(Vintage~Response)

# annual premium vs vehicle age
result1<-aov(Annual_Premium~Vehicle_Age)
summary(result1)
tapply(Annual_Premium, Vehicle_Age,mean)

# annual premium vs vehicle damage
t.test(Annual_Premium~Vehicle_Damage)
tapply(Annual_Premium, Vehicle_Damage, mean)

# annual premium vs previous vehicle insurance
t.test(Annual_Premium~Previously_Insured)
tapply(Annual_Premium, Previously_Insured, mean)

# annual premium vs driving license
t.test(Annual_Premium~Driving_License)
tapply(Annual_Premium, Driving_License, mean)


# average annual premium by vehicle age, response
train_dataset_Health_Insurance_Cross_Sell_Prediction %>% group_by(Vehicle_Age, Response) %>% summarise(Annual_Premium=mean(Annual_Premium))

# average annual premium by vehicle damage, response
train_dataset_Health_Insurance_Cross_Sell_Prediction %>% group_by(Vehicle_Damage, Response) %>% summarise(Annual_Premium=mean(Annual_Premium))

# average annual premium by previous vehicle insurance, response
train_dataset_Health_Insurance_Cross_Sell_Prediction %>% group_by(Previously_Insured, Response) %>% summarise(Annual_Premium=mean(Annual_Premium))

# average annual premium by driving license, response
train_dataset_Health_Insurance_Cross_Sell_Prediction %>% group_by(Driving_License, Response) %>% summarise(Annual_Premium=mean(Annual_Premium))


