## Sample selection correction | The Heckman selection model

# Sample selection correction to the data of married women agreed working time
data <- read.csv2("data_v.csv")

# subset: Frauen
data_hm_v_f <- subset(data, männlich == 0)


install.packages("sampleSelection")
install.packages("maxLik")
install.packages("miscTools")
library(sampleSelection)
library(maxLik)
library(miscTools)
attach(data_hm_v_f)

# Estimate OLS regression
regOLS <- lm(log(Bruttolohn_pro_h_vereinbart+1) ~ Alter + I(Alter^2) + Migrationshintergrund + Ausbildung)

# Estimate Heckman (2 step) selection model 
heckit <- selection(erwerbstätig ~ Ausbildung + Alter + I(Alter^2) + Kind_u7 + Migrationshintergrund
                    + Gesundheit + sonstiges_Einkommen + ostdeutsch,
                    log(Bruttolohn_pro_h_vereinbart+1) ~ Alter + I(Alter^2) + Migrationshintergrund + Ausbildung,
                    data = data_hm_v_f, method = "2step")

# summary of the results:
library(stargazer)
stargazer(regOLS, heckit, title = "Frauen, verheiratet", 
          type = "text",
          keep.stat = c("n", "rsq"),
          out = "heckman_v_f.html")

summary(regOLS)
summary(heckit)

predictions <- predict(heckit, newdata = data_hm_v_f)
predictions_new <- exp(predictions)
print(predictions)
View(predictions)

data_hm_v_f$Bruttolohn_pro_h_hm <- predictions_new


#Implement heckman wages/hour

data_hm_v_f_0 <- subset(data_hm_v_f, income_2 == 0)

data_hm_v_f_0$income_2 <- data_hm_v_f_0$Bruttolohn_pro_h_hm * 10
data_hm_v_f_0$income_3 <- data_hm_v_f_0$Bruttolohn_pro_h_hm * 20
data_hm_v_f_0$income_4 <- data_hm_v_f_0$Bruttolohn_pro_h_hm * 30
data_hm_v_f_0$income_5 <- data_hm_v_f_0$Bruttolohn_pro_h_hm * 40
data_hm_v_f_0$income_6 <- data_hm_v_f_0$Bruttolohn_pro_h_hm * 60

table(data_hm_v_f_0$income_2)

data_hm_v_f_0$income_2[data_hm_v_f_0$income_2 == 0] <- NA
data_hm_v_f_0$income_3[data_hm_v_f_0$income_3 == 0] <- NA
data_hm_v_f_0$income_4[data_hm_v_f_0$income_4 == 0] <- NA
data_hm_v_f_0$income_5[data_hm_v_f_0$income_5 == 0] <- NA
data_hm_v_f_0$income_6[data_hm_v_f_0$income_6 == 0] <- NA

data_hm_v_f_1 <- subset(data_hm_v_f, income_2 >= 1)

data_hm_v_f <- rbind(data_hm_v_f_1,data_hm_v_f_0)

data_hm_v_f <- subset(data_hm_v_f, select = -X)

write.csv2(data_hm_v_f, "data_hm_v_f.csv")


## Compare estimated real wages with actual real wages
data_v_f_filtered <- data_hm_v_f[data_hm_v_f$Bruttolohn_pro_h_vereinbart <=200, ]
data_v_f_filtered <- data_v_f_filtered[data_v_f_filtered$Bruttolohn_pro_h_vereinbart > 0, ]

plot(data_v_f_filtered$Bruttolohn_pro_h_vereinbart, data_v_f_filtered$Bruttolohn_pro_h_hm, 
     xlab="Tatsächliche Bruttostundenlöhne (in €)", ylab="Geschätzte Bruttostundenlöhne (in €)", 
     main = "Frauen, verheiratet",
     col="cornflowerblue")

reg <- lm(data_v_f_filtered$Bruttolohn_pro_h_hm ~ data_v_f_filtered$Bruttolohn_pro_h_vereinbart, data_v_f_filtered = data_v_f_filtered)

abline(reg, col="blue")
