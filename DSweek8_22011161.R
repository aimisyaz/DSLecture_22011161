install.packages("dplyr")
library(dplyr)
library(readr)
student_data <- read.csv("C:/Users/ASUS/Downloads/student_data.csv")
student_fail <- student_data %>% filter(final_exam_mark < 40)
View(student_fail)
View(student_data)
summary(student_data)
head(student_data,15)
tail(student_data)
student_fail <- student_fail <- student_data(student_data$final_exam_mark < 40,)
View(student_fail)
mydata <- student_data %>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View(mydata)
mydata1 <- student_data %>% filter(final_exam_mark > 40) %>% arrange(desc(final_exam_mark))
View(mydata1)
mydata <- student_data %>% select(student_id, coursework_mark, final_exam_mark)
View(mydata)
mydata2 <- df_ds23[ , c("student_id", "Coursework_mark", "final_exam_mark" )]
View(mydata2)
glimpse(mydata)

mydata = student_data%>% mutate(Totak_mark=(coursework_mark + final_exam_mark/200*100))
View(mydata)
mydata <- cbind(student_data , Tota_mark = (student_data $coursework_mark + student_data$final_exam_mark/200*100))
View(mydata)
#Descriptive analytics
data <-iris
head(data)
tail(data)
str(data)
min(data$Sepal.Length)
max(data$Sepal.Length)
range(data$Sepal.Length)
range(data$Sepal.Length) [1]
range(data$Sepal.Length) [2]
range_val <- range(data$Sepal.Length)
range_val [1]
range_val [2]
sd(data$Sepal.Length)
var(data$Sepal.Length)
mean(data$Sepal.Length)
median(data$Sepal.Length)
summary(data)
summary(data$Sepal.Length)

A<- c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A)
quantile(A, 0.25)
quantile(A, 0.75)
IQR (A)
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length (cm)",
        col = c("lightgreen", "lightpink", "lightyellow"))

plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     col = as.numeric(iris$Species),
     pch = 19)
nrow (data)
nrow(data['Sepal.Length'])
ncol(data)
sum(is.na(data$Sepal.Length))
sum(data$Sepal.Length <0 )

#Outliers
library(readr)
dfplayers <- read.csv("C:/Users/ASUS/Downloads/players.csv")
is.na(dfplayers)
sum(is.na(player))

#MEDIAN
median_age <- median(player$Age, na.rm =TRUE) #replace missing value with median value

#HANDLE OUTLIERS
data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")

first_q<-quantile(data,0.25) 
third_q<-quantile(data,0.75) 

data_new<-data
data <- data_new
iqr<-IQR(data)
le<-first_q - 1.5 * iqr 
ue<-third_q + 1.5 * iqr

data_new<-data
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new

data_new <- data
avg <- round(mean(data_new)) #for the purpose of example we round up value
data_new[data_new<le] <- avg
data_new[data_new>ue] <- avg
data_new

data_new <- data
data_new[data_new<le] <- le
data_new[data_new>ue] <- ue
data_new

boxplot(data_new, main = "Boxplot")

