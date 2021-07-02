# Change working dir
# Or set it to the dir where the files will be sitting, 
# specially the data file
setwd("C:/Users/alber/projects/capstone/cyo")


# ##########################################################
# # Load libraries
# ##########################################################


library(tidyverse)
library(caret)
library(data.table)
library(reshape)
library(e1071)


# Set random seed for results replicability
set.seed(42, sample.kind="Rounding")

# Load data
df <- read.csv(file = './data.csv') #%>% as_tibble()
df<-df %>%  mutate(Date..GMT. = as.POSIXlt(Date..GMT.,format="%Y-%m-%d %H:%M:%S",tz="GMT")) 
df<-df[order(df$Date..GMT.),]



tail(df)

# Show some data summary statistics
# Dimensions
dim(df)
# Structure
str(df)
# Summary
summary(df)

# Colnames
colnames(df)

# Top 6 Head
head(df)

# Bottom 6 
tail(df)

par(mar=c(15,4,4,4))
## Missing Values
nulls<-sapply(df, function(x) sum(is.na (x)))/dim(df)[1]
nulls<-nulls[order(desc(nulls))]
names(nulls)
par(mfrow=c(1,1))
barplot(nulls,las=2,cex.names=.7, cex.axis=0.7)
par(mar=c(2.5,2.5,2.5,2.5)) # Reset Sizes

####
print("Warning!!, charts won't show if temp folder RtmpItb0Gh does not exists, please create")
print("this folder in the temp address, this is a known R issue")
print("https://github.com/rstudio/rstudio/issues/2214")



## Main missing columns
par(mfrow=c(2,2))
for (cc in c('Air.Temperature.ÂºC.','Highest.Wave.Period.s.','Mean.comming.from.Direc..at.Waves.Peak.grados.','Maximum.Waves.Height.m.')){
  x<-drop_na(df[cc])[[1]]
  print(cc)
  # print(c(cc, "Mean", mean(x), "Std",sd(x), "Skew",skewness(x), "Kurt",kurtosis(x)))
  hist(x,main=paste('Histogram',cc))
  # plot(df$Date..GMT.,df[[cc]],main=paste('Time series',cc))
}

## Main missing columns
res<-data.frame()
par(mfrow=c(2,2))
for (cc in c('Air.Temperature.ÂºC.','Highest.Wave.Period.s.','Mean.comming.from.Direc..at.Waves.Peak.grados.','Maximum.Waves.Height.m.')){
  x<-drop_na(df[cc])[[1]]
  plot(df$Date..GMT.,df[[cc]],main=paste('Time series',cc))
  
  # Create Stats Matrix
  if (dim(res)[1]==0){
  res<-data.frame(name=cc, mean=mean(x), sd=sd(x), skw=skewness(x), kurt=kurtosis(x))
  } else {
  res<-rbind(res, data.frame(name=cc, mean=mean(x), sd=sd(x), skw=skewness(x), kurt=kurtosis(x)))
  }
  
}

## Null Data Frame Summary Stats
res %>% as_tibble()

par(mfrow=c(1,1))
## Air Temperature
cc<-'Air.Temperature.ÂºC.'
x<-drop_na(df[cc])[[1]]
plot(df$Date..GMT.,df[[cc]],main=paste('Time series',cc))

# Adjusted Curve
# Trend 1
t0 = '2020-06-01 00:00:00'
t0<-as.POSIXct(t0,format="%Y-%m-%d %H:%M:%S")
trend1 = ifelse(df$Date..GMT.>=t0,1,0)

# Trend 2
t0 = '2020-10-01 00:00:00'
trend2 = ifelse(df$Date..GMT.>=t0,1,0)

airtemp<-df[cc][[1]]
airtemp<-as_tibble(airtemp) %>% mutate(
  trend = seq(1,length(airtemp),1),
  trend1_sq=cumsum(trend1)**2,
  trend2_sq=cumsum(trend2)**2)

train <- drop_na(airtemp)
train

model<-lm(value~trend+trend1_sq+trend2_sq,data=train)

airtemp<-airtemp%>%mutate(yh=predict(model,newdata = airtemp),ynew = ifelse(is.na(value),yh,value)) 
airtemp %>% ggplot()+ 
  geom_point(aes(trend, value)) + 
  geom_line(aes(trend,yh),color='red')+
  labs(title="Adjusted Time series plot")

# # We will use this curve to fill missing values
df[cc]<-airtemp$ynew
par(mfrow=c(1,1))
plot(df$Date..GMT.,df[[cc]],main=paste('Time series for Air Temperature after missing values filled'))


# Other three columns 
# we will use the mean value to fill the missing values.
for (cc in c('Highest.Wave.Period.s.','Mean.comming.from.Direc..at.Waves.Peak.grados.','Maximum.Waves.Height.m.')){
  x<-drop_na(df[cc])[[1]]
  df[cc][is.na(df[cc])]<-mean(x)
}



par(mar=c(15,4,4,4))
## Missing Values
nulls<-sapply(df, function(x) sum(is.na (x)))/dim(df)[1]
nulls<-nulls[order(desc(nulls))]
names(nulls)
barplot(nulls,las=2,cex.names=.7, cex.axis=0.7)
par(mar=c(2.5,2.5,2.5,2.5)) # Reset Sizes


## Remaining missing values

missing_val_names<-names(nulls[nulls>0])
target_var <-"Significant.Wave.Height.m."
missing_val_names<-missing_val_names[missing_val_names!=target_var]

for (cc in missing_val_names){
  x<-drop_na(df[cc])[[1]]
  df[cc][is.na(df[cc])]<-mean(x)
}

# Drop missing Significant Wave Heights records
df<-drop_na(df)


# Analysing target variable
a<-df[target_var] %>% head(72) 
alag12<-lag(df[target_var],12) %>% head(72)
alag24<-lag(df[target_var],24) %>% head(72)

data.frame(x=seq(1,72,1),WaveHeight=a[[1]],Lag12=alag12[[1]],Lag24=alag24[[1]]) %>%
  melt(id=c("x")) %>% ggplot(aes(x=x,y=value,color=variable)) +
  geom_line()+
  labs(title="Lagged Time series plot")


# Adding seasonality indicators
a<-df %>% mutate(
  Ind12hrs = as_factor(seq(0, dim(df)[1] - 1 ) - floor(seq(0, dim(df)[1] - 1 )/12) * 12),
  Weekday = as_factor(strftime(Date..GMT.,"%a")),
  MoonCycle = as_factor(seq(0, dim(df)[1] - 1 ) - floor(seq(0, dim(df)[1] - 1 )/672) * 672),
  Monthly = as_factor(strftime(Date..GMT.,"%b")),
  ) %>% select(Ind12hrs, Weekday, MoonCycle, Monthly)


library(mltools, include.only = 'one_hot')
# One hot encoding 
b<-one_hot(as.data.table(a)) %>% 
  select(-c("Monthly_Jun","MoonCycle_0","Weekday_Mon","Ind12hrs_0"))

df<-bind_cols(df,b) %>%  select(-c("Source.of.data","Source.of.data.1","Source.of.data.2","Source.of.data.3"))


# Model selection and cross validation
x<-df %>% select(-c("Significant.Wave.Height.m.","Maximum.Waves.Height.m.","Date..GMT."))
y<-df %>% select("Significant.Wave.Height.m.")


trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)

x_baseline<-x %>% mutate(intercept=seq(1,1,length.out=dim(x)[1])) %>% select(intercept)

baseline_nb_fit <- train(x=x_baseline, y=y$Significant.Wave.Height.m., method = "lm", trControl=trctrl, 
                         tuneLength = 0, tuneGrid  = expand.grid(intercept = FALSE))

lm_nb_fit <- train(x=x, y=y$Significant.Wave.Height.m., method = "lm", trControl=trctrl, tuneLength = 0, standardize = FALSE)

ridge_nb_fit <- train(
  x=x, y=y$Significant.Wave.Height.m., method = "glmnet", trControl=trctrl, 
  tuneLength = 0, standardize = FALSE, tuneGrid = expand.grid(alpha = 0,lambda = seq(0,5,0.5)))

# Almost 40mins to Train this random forest
rf_nb_fit <- train(x=x, y=y$Significant.Wave.Height.m., method = "rf", ntree = 100, 
                   trControl=trctrl, tuneLength = 0,
                   tuneGrid  = expand.grid(mtry = seq(20, 50, 5)))




# Results
res<-rbind(
  baseline_nb_fit$results[order(baseline_nb_fit$results$RMSE),] %>% head(1) %>% select(RMSE, MAE) %>% mutate(Model="Baseline"),
  lm_nb_fit$results[order(lm_nb_fit$results$RMSE),] %>% head(1) %>% select(RMSE, MAE) %>% mutate(Model="LinearRegression"),
  ridge_nb_fit$results[order(ridge_nb_fit$results$RMSE),] %>% head(1) %>% select(RMSE, MAE) %>% mutate(Model="Ridge"),
  rf_nb_fit$results[order(rf_nb_fit$results$RMSE),] %>% head(1) %>% select(RMSE, MAE) %>% mutate(Model="RandomForest")
) %>% select(Model, RMSE, MAE)

res[order(res$RMSE),]


# Best Model
res[order(res$RMSE),] %>% head(1)

# mean(y$Significant.Wave.Height.m.)


ypred<-predict(rf_nb_fit,newdata = x)
residuals_swh <- y$Significant.Wave.Height.m. - ypred



# Autocorrelation
acf(residuals_swh,lag.max=30)


# Adding Autocorrelation to our model

swhlag1<-lag(y$Significant.Wave.Height.m., 1)
swhlag1<-ifelse(is.na(swhlag1),0,swhlag1)

# Plot Series and Lagged Series
data.frame(x=seq(1,72,1),WaveHeight=head(y$Significant.Wave.Height.m.,72),
           swhlag1=head(swhlag1,72)) %>%
  melt(id=c("x")) %>% ggplot(aes(x=x,y=value,color=variable)) +
  geom_line()+
  labs(title="Lagged Time series plot")



# Retrain Model
x_with_acf<-x %>% mutate(swhlag1=swhlag1)


trctrl_modelfinal <- trainControl(method = "cv", number = 10, savePredictions=TRUE)

# Almost 10 mins to Train this random forest
modelfinal <- train(x=x_with_acf, y=y$Significant.Wave.Height.m., 
                    method = "rf", ntree = 100, tuneLength = 0,
                    trControl = trctrl_modelfinal,
                   tuneGrid  = expand.grid(mtry = 50))


ypred<-predict(modelfinal, newdata = x_with_acf)
residuals_swh <- y$Significant.Wave.Height.m. - ypred

# Autocorrelation
acf(residuals_swh,lag.max=30)

modelfinal$results

