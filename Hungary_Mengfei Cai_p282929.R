#1a and 1c : read the dataset and rename
library(haven)
EBdata <- read_sav("ZA4977_v2-0-0.sav")
EBdata_subset <-subset(EBdata,select=paste("v",c(6,150:154,119:122,330,331,337),sep=""))
EBdata_subset <-subset(EBdata,select=paste("v",c(6,150:154,119:122,330,331,337),sep=""))
names(EBdata_subset) <-c("country","alc_12m","alcfreq_5dr","alc_30da","alcfreq_30d","alc_am_dd","pa7d_work","pa7d_mov","pa7d_house","pa7d_recr","gender","age","child_am")

#1b make a small dataset of Hungary
EBdata_Hungary<-subset(EBdata_subset,EBdata_subset$country==22)

#1d: describle rough data
summary(EBdata_Hungary)
str(EBdata_Hungary)

#1e: use  unique function to check each variable
unique(EBdata_Hungary$alcfreq_30d)
# [1] NA  6  4  5  1  2  3


#1f  numerical variables that are categorical ones into factors 
#and drop the levels that are non-informative
data_converter<-function(data,drop_level){
     data<-factor(data,levels=attr(data,"labels"),labels=names(attr(data,"labels")))
      data<-droplevels(data,exclude=drop_level)
     }

EBdata_Hungary$alc_12m<-data_converter(EBdata_Hungary$alc_12m,c("DK/Refusal","Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$alcfreq_5dr<-data_converter(EBdata_Hungary$alcfreq_5dr,c("DK/Refusal","Inap. (not 1 in V150)" ,"Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$alc_30da<-data_converter(EBdata_Hungary$alc_30da,c("DK/Refusal","Inap. (not 1 in V150)" ,"Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$alcfreq_30d<-data_converter(EBdata_Hungary$alcfreq_30d,c("Don't remember/Refusal (SPONT.)","Inap. (not 1 in V152)" ,"Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$alc_am_dd<-data_converter(EBdata_Hungary$alc_am_dd,c("DK/ Refusal","Inap. (not 1 in V152)" ,"Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$pa7d_work<-data_converter(EBdata_Hungary$pa7d_work,c("DK","Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$pa7d_mov<-data_converter(EBdata_Hungary$pa7d_mov,c("DK","Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$pa7d_house<-data_converter(EBdata_Hungary$pa7d_house,c("DK","Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$pa7d_recr<-data_converter(EBdata_Hungary$pa7d_recr,c("DK","Inap. (not 1 to 30 in V6)"))
EBdata_Hungary$gender<-data_converter(EBdata_Hungary$gender,NULL)


# 1g discriptive statistics of alc_12m
table(EBdata_Hungary$alc_12m)

#1g: discriptive statistics of pa7d_work
table(EBdata_Hungary$pa7d_work)

# 1h: barplot of pa7d_work
hist(EBdata_Hungary$pa7d_work)
counts_pa7d_work<-table(EBdata_Hungary$pa7d_work)
barplot(counts_pa7d_work,main="pa7d_work")

######2a  # number of drinking days/week???
compute.n_drinkswk<-function(alcfreq){
    
        no.drinkswk<-switch(alcfreq,"once"=0.25,"once a week"=1,"2 - 3 times a month"=0.5,
                              "Daily"=7,"4 - 5 times a week"=4,"2 - 3 times a week"=2,"NA"=NA)
      return(no.drinkswk)  }
EBdata_Hungary$n_drinkswk_temp <- mapply(compute.n_drinkswk,EBdata_Hungary$alcfreq_30d)

############2b  # quantity of alcohol consumed on a drinking day???
compute.quantday<-function(quant_oneday){
         quantday<-switch(quant_oneday,"Less than 1 drink"=6,"1 - 2 drinks"=18,
                "3 - 4 drinks"=42,"5 - 6 drinks"=66, 
                "7 - 9 drinks"= 8," 10 drinks or more"=120,
                "It depends (SPONT.)"=NA,"NA"=NA)
return(quantday) }
EBdata_Hungary$quantday_temp <- mapply(compute.quantday,EBdata_Hungary$alc_am_dd)


#### 2c # average quantity of alcohol per week
EBdata_Hungary$quantperweek_temp <-EBdata_Hungary$quantday_temp*EBdata_Hungary$n_drinkswk_temp



######2d  histogram / barplot of daily intake.
## for men
EBdata_Hungary_men<-subset(EBdata_Hungary,EBdata_Hungary$gender=="Male")
EBdata_Hungary_men$daily<-EBdata_Hungary_men$quantperweek_temp/7
hist(EBdata_Hungary_men$daily,xlab = "Men: daily alcohol intake")
barplot(table(EBdata_Hungary_men$daily),xlab = "Men: daily alcohol intake")

## for women
EBdata_Hungary_women<-subset(EBdata_Hungary,EBdata_Hungary$gender=="Female")
EBdata_Hungary_women$daily<-EBdata_Hungary_women$quantperweek_temp/7
hist(EBdata_Hungary_women$daily, xlab = "Women: daily alcohol intake")
barplot(table(EBdata_Hungary_women$daily),xlab = "Women: daily alcohol intake")

#### 2e heavy drinker??? for men and women
EBdata_Hungary$alc_daily<-EBdata_Hungary$quantperweek_temp/7
EBdata_Hungary$heavy_drinker_temp<-ifelse(EBdata_Hungary$gender==
                "Male"& EBdata_Hungary$alc_daily >40, "YES","EBdata_Hungary$heavy_drinker_temp")
EBdata_Hungary$heavy_drinker_temp<-ifelse(EBdata_Hungary$gender==
                                       "Female"& EBdata_Hungary$alc_daily >20, "YES","NO")

## 2f  binge
EBdata_Hungary$nums_drinks_daily<-EBdata_Hungary$quantday_temp/12
EBdata_Hungary$binge_temp<-ifelse(EBdata_Hungary$quantday_temp/12 > 5,1,0)

## 2g problem drinker
EBdata_Hungary$problem_drinker<-ifelse(EBdata_Hungary$binge_temp==1
                  | EBdata_Hungary$heavy_drinker_temp=="YES", "YES","NO")

##2i a new variable total physical activity
## as.numeric to convert the factor into numeric values.
EBdata_Hungary$pa7d_work_numeric<-as.numeric(EBdata_Hungary$pa7d_work)
EBdata_Hungary$pa7d_recr_numeric<-as.numeric(EBdata_Hungary$pa7d_recr)
EBdata_Hungary$pa7d_mov_numeric<-as.numeric(EBdata_Hungary$pa7d_mov)
EBdata_Hungary$pa7d_house_numeric<-as.numeric(EBdata_Hungary$pa7d_house)

EBdata_Hungary$pa_total<-EBdata_Hungary$pa7d_work_numeric+EBdata_Hungary$pa7d_recr_numeric
+EBdata_Hungary$pa7d_mov_numeric+EBdata_Hungary$pa7d_house_numeric

EBdata_Hungary$state_pa<-ifelse(EBdata_Hungary$pa_total>=7,"inactive",
                                ifelse(EBdata_Hungary$pa_total>=3 & EBdata_Hungary$pa_total<7, "moderately active","active"))


##3a Print a table contrasting the variable on physical activity (state_pa)
table(EBdata_Hungary$state_pa,EBdata_Hungary$problem_drinker)

##3a  choose the 
qqnorm(EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="YES")],xlab = "Problem drinkers's total physical activity")
qqline(EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="YES")])

qqnorm(EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="NO")],xlab = "Non-Problem drinkers's total physical activity")
qqline(EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="NO")])
## these data are not normally distributed
wilcox.test((EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="YES")]),
       (EBdata_Hungary$pa_total[which(EBdata_Hungary$problem_drinker=="NO")]))



#3b Make a graph of percentages with at least moderate physical activity
EBdata_Hungary$age10<-cut(EBdata_Hungary$age,breaks=seq(10,100,10))
#EBdata_Hungary_moderate<-subset(EBdata_Hungary,EBdata_Hungary$state_pa_temp="moderately active")
freq_age10<-prop.table(table(EBdata_Hungary$state_pa_temp,EBdata_Hungary$age10),margin=2)
barplot(freq_age10[2,],xlab = "moderate physical activity by 10-age",ylim = c(0,0.2))  # plot the Moderately active

freq_gender<-prop.table(table(EBdata_Hungary$state_pa_temp,EBdata_Hungary$gender),margin=2)
barplot(freq_age10[,1],xlab = "moderate physical activity by male",ylim = c(0,1.2)) 
barplot(freq_age10[,2],xlab = "moderate physical activity by female",ylim = c(0,1.2)) 

#3c daily alcohol use in grams and check if normal distribution
qqnorm(EBdata_Hungary$alc_daily)
qqline(EBdata_Hungary$alc_daily)

#3d 
#mod4 <- lm(Volume ~ Height+Girth, data=trees)
mod_age<-lm(EBdata_Hungary$alc_daily~EBdata_Hungary$age)
summary(mod_age)

mod_gender<-lm(EBdata_Hungary$alc_daily~EBdata_Hungary$gender)
summary(mod_gender)

mod_pa<-lm(EBdata_Hungary$alc_daily~EBdata_Hungary$pa_total)
summary(mod_pa)

# 3e 

# 3f
mod_pa_age<-lm(alc_daily~pa_total+age,data=EBdata_Hungary)
summary(mod_pa_age)

mod_pa_gender<-lm(alc_daily~pa_total+gender,data=EBdata_Hungary)
summary(mod_pa_age)


