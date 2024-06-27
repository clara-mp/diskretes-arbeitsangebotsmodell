## Sample selection correction | The Heckman selection model

# Sample selection correction to the data of not married women 

data_hm_nv_f <- read.csv2("data_nv_f.csv")

install.packages("sampleSelection")
install.packages("maxLik")
install.packages("miscTools")
library(sampleSelection)
library(maxLik)
library(miscTools)
attach(data_hm_nv_f)

# Estimate OLS regression
regOLS <- lm(log(Bruttolohn_pro_h_vereinbart+1) ~ Alter + I(Alter^2) + Migrationshintergrund + Ausbildung)

# Estimate Heckman (2 step) selection model 
heckit <- selection(erwerbstätig ~ Ausbildung + Alter + I(Alter^2) + Kind_u7 + Migrationshintergrund
                    + Gesundheit + sonstiges_Einkommen + ostdeutsch,
                    log(Bruttolohn_pro_h_vereinbart+1) ~ Alter + I(Alter^2) + Migrationshintergrund + Ausbildung,
                    data = data_hm_nv_f, method = "2step")

# summary of the results:
library(stargazer)
stargazer(regOLS, heckit, title = "Frauen, nicht verheiratet", 
          type = "text",
          out = "heckman_nv_f.html",
          keep.stat = c("n", "rsq"))

summary(regOLS)
summary(heckit)

predictions <- predict(heckit, newdata = data_hm_nv_f)
predictions_new <- exp(predictions)
print(predictions)
View(predictions)
summary(predictions_new)

data_hm_nv_f$Bruttolohn_pro_h_hm <- predictions_new
data_hm_nv_f$Bruttolohn_pro_h_hm <- round(data_hm_nv_f$Bruttolohn_pro_h_hm, 0)


#Implement heckman wages/hour
data_hm_nv_f_0 <- subset(data_hm_nv_f, income_2 == 0)

data_hm_nv_f_0$income_2 <- data_hm_nv_f_0$Bruttolohn_pro_h_hm * 10
data_hm_nv_f_0$income_3 <- data_hm_nv_f_0$Bruttolohn_pro_h_hm * 20
data_hm_nv_f_0$income_4 <- data_hm_nv_f_0$Bruttolohn_pro_h_hm * 30
data_hm_nv_f_0$income_5 <- data_hm_nv_f_0$Bruttolohn_pro_h_hm * 40
data_hm_nv_f_0$income_6 <- data_hm_nv_f_0$Bruttolohn_pro_h_hm * 60

table(data_hm_nv_f_0$income_2)

data_hm_nv_f_0$income_2[data_hm_nv_f_0$income_2 == 0] <- NA
data_hm_nv_f_0$income_3[data_hm_nv_f_0$income_3 == 0] <- NA
data_hm_nv_f_0$income_4[data_hm_nv_f_0$income_4 == 0] <- NA
data_hm_nv_f_0$income_5[data_hm_nv_f_0$income_5 == 0] <- NA
data_hm_nv_f_0$income_6[data_hm_nv_f_0$income_6 == 0] <- NA

data_hm_nv_f_1 <- subset(data_hm_nv_f, income_2 >= 1)

data_hm_nv_f <- rbind(data_hm_nv_f_1,data_hm_nv_f_0)

data_hm_nv_f <- subset(data_hm_nv_f, select = -X)

write.csv2(data_hm_nv_f, "data_hm_nv_f.csv")

## Compare estimated real wages with actual real wages
data_nv_f_filtered <- data_hm_nv_f[data_hm_nv_f$Bruttolohn_pro_h_vereinbart <=200, ]
data_nv_f_filtered <- data_nv_f_filtered[data_nv_f_filtered$Bruttolohn_pro_h_vereinbart > 0, ]

plot(data_nv_f_filtered$Bruttolohn_pro_h_vereinbart, data_nv_f_filtered$Bruttolohn_pro_h_hm, 
     xlab="tatsächliche Bruttolstundenlöhne (in €)", ylab="geschätzte Bruttostundenlöhne (in €)", 
     main = "Frauen, nicht verheiratet",
     col="cornflowerblue")

reg <- lm(data_nv_f_filtered$Bruttolohn_pro_h_hm ~ data_nv_f_filtered$Bruttolohn_pro_h_vereinbart, data_nv_f_filtered = data_nv_f_filtered)

abline(reg, col="blue")

