setwd("C:/Users/pawlusm/Desktop")

library(readr)

anon <- read.csv("anon_can2.csv", stringsAsFactors = FALSE)

dim(anon)
names(anon)
str(anon)
summary(anon)

anon$log_tg <- log(anon$ttl_giving + 1)
anon$log_14 <- log(anon$fy14 + 1)
anon$log_15 <- log(anon$fy15 + 1)
anon$log_16 <- log(anon$fy16 + 1)

# anon[is.na(anon)] <- 0

for (x in 3:5) {
    anon[,x] <- as.factor(anon[,x])
}

anons <- anon[,c(1,2,3,4,5,11,21)]



jpeg("pairs1.jpeg", width = 4, height = 4, units = 'in', res = 300)
pairs(anons, upper.panel = panel.smooth) # Make plot
dev.off()

pairs(iris)

## feature engineering

## RFM

## recency quintiles

# convert data type to date and then numeric timestamp for sorting into quintiles

anon$date1 <- strptime(anon$latest_date, "%m/%d/%Y")

anon$date2 <- as.numeric(anon$date1)

# divide your data set into two parts

anon.r <- anon[!(is.na(anon[,26])), ]

anon.s <- anon[(is.na(anon[,26])), ]

# create your quintile function

ApplyRecencyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon.r$date2, probs = seq(0, 1, by = 0.20))), 
      labels=c(1:5), include.lowest=TRUE)
}

## apply the function to score results

anon.r$recency <- sapply(anon.r$date2, ApplyRecencyQuintiles)

anon.r$recency <- as.numeric(as.character(anon.r$recency))

## add a zero ("0") result for all those non-applicable

anon.s$recency <- 0

## merge the divided data sets back together

anon <- rbind(anon.r, anon.s)



## frequency quintiles

anon.r <- anon[anon$cons_giving>0,]

anon.s <- anon[anon$cons_giving==0, ]

ApplyFrequencyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon.r$cons_giving, probs = seq(0, 1, by = 0.20))), 
      labels=c(1:5), include.lowest=TRUE)
}

anon.r$frequency  <- sapply(anon.r$cons_giving, ApplyFrequencyQuintiles)  ## results in an error

## in this case adjust by first exploriing how many groups you can make and editing your function

result <- cut(anon$cons_giving,unique(quantile(anon.r$cons_giving,probs=seq(0,1,0.2))),include.lowest=TRUE)

table(result)

ApplyFrequencyQuintiles <- function(x) {
  cut(x, breaks=c(unique(quantile(anon.r$cons_giving, probs = seq(0, 1, by = 0.20)))), 
      labels=c(1:4), include.lowest=TRUE)
}

anon.r$frequency  <- sapply(anon.r$cons_giving, ApplyFrequencyQuintiles)

anon.r$frequency <- as.numeric(as.character(anon.r$frequency))

anon.s$frequency  <- 0

anon <- rbind(anon.r, anon.s)




## monetary

anon.r <- anon[anon$ttl_giving>0,]

anon.s <- anon[anon$ttl_giving==0, ]

ApplyMonetaryQuintiles <- function(x) {
  cut(x, breaks=c(quantile(anon.r$ttl_giving, probs = seq(0, 1, by = 0.20))), 
      labels=c(1:5), include.lowest=TRUE)
}

anon.r$monetary <- sapply(anon.r$ttl_giving, ApplyMonetaryQuintiles)

anon.r$monetary <- as.numeric(as.character(anon.r$monetary))

anon.s$monetary  <- 0

anon <- rbind(anon.r, anon.s)

## create the combined RFM score

#for (x in 28:30) {
#  anon[,x] <- as.numeric(anon[,x] + 1)
#}

anon$rfm <- anon$recency + anon$monetary + anon$frequency

## check for patterns

anons <- anon[,c(1,2,11,21,27:30)]

jpeg("pairs2.jpeg", width = 4, height = 4, units = 'in', res = 300)
pairs(anons, upper.panel = panel.smooth) # Make plot
dev.off()


## create one more feature for excutives

anon$title <- trimws(tolower(anon$title))

titles <- unique(anon$title)
sort(titles)


#anon.exec <- anon[grep("\\b[^a-z]*c[a-z]o[^a-z]* | \\b[^a-z]*president[^a-z]* | c[a-z]o[^a-z]* | president", anon$title), ]

#anon.exec <- anon[grep("pres[^a-h,j-z]|ceo|coo$|cfo|.*owner|vp|chief.*officer|.*founder", anon$title), ]
#anon.exec <- anon.exec[anon.exec$median_income>80000,]

anon$exec <- 0

#anon2 <- anon

anon$exec[grep("pres[^a-h,j-z]|ceo|coo$|cfo|.*owner|vp|chief.*officer|.*founder", anon$title) ] <- 1
#anon$exec[anon$exec!=1] <- 0
anon$exec[anon$median_income<80000] <- 0
#anon$exec <- as.numeric(anon$exec)

## check for correlation with giving

anons <- anon[,c(2,3,12,22,28:32)]
pairs(anons, upper.panel = panel.smooth)

## attempt to predict

## edit to remove givinf info

anon$donor <- 0
anon$donor[anon$ttl_giving>0] <- 1


######## seperate try
for (x in 6:9) {
  anon[,x] <- as.factor(anon[,x])
}

## create a test and train set

anon$grad_yr[is.na(anon$grad_yr)] <- 0
anon$date2[is.na(anon$date2)] <- 0
anon.p <- anon[,c(1:9,11,31)]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(anon.p))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(anon.p)), size = smp_size)

train <- anon.p[train_ind, ]
test <- anon.p[-train_ind, ]

## check that classes are still balanced

prop.table(table(train$donor))

prop.table(table(test$donor))

prop.table(table(anon.p$donor))


feature.names <- names(train)[c(3:9)]

for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    train[[f]] <- as.integer(train[[f]])
    test[[f]]  <- as.integer(test[[f]])
  }
}



library(xgboost)

clf <- xgboost(data             = data.matrix(train[,c(2:10)]),
               label            = train$donor,
               eta              = 0.1,
               nrounds          = 1000,
               objective        = "reg:logistic",
               colsample_bytree = 0.7,
               subsample        = 0.7
)


pred <- predict(clf, data.matrix(test[,2:10]))

test <- cbind(test,pred)

table(pred,test$donor)

test$anony <- 0
test$anony[test$pred>0.5] <- 1

table(test$anony,test$donor)

prop.table(table(test$anony,test$donor),2)


# Get the feature real names
names <- dimnames(anon.p[2:10])[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = clf)

# Nice graph
xgb.plot.importance(importance_matrix[1:8])

jpeg("xgb_imp.jpeg", width = 4, height = 4, units = 'in', res = 300)
xgb.plot.importance(importance_matrix[1:8]) # Make plot
dev.off()
