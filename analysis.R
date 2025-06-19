############Analysis#############

install.packages("car") #for VIF 
install.packages("plm") #for panel model
install.packages("tseries")
install.packages("lmtest")
install.packages("sandwich")
install.packages("modelsummary")
install.packages("ARDL")
install.packages("dynlm")
install.packages("urca")
install.packages("summarytools")
install.packages("stargazer")
install.packages("pandoc")


library(ARDL)
library(dynlm)
library(urca)
library(summarytools)
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)
library(plm)
library(lmtest)
library(tseries)
library(stargazer)
library(pandoc)

### checking VIF
names(regg)

names(regg)[names(regg)== "Net Emission"]<- "Net_Emission_01" # changing name of net emission
names(regg)[names(regg)== "R&D_exp_of_GDP"]<- "R_D_EXP_GDP_01"

m_vif<-lm(`Net_Emission_01`~ GDP_PC + GDP_PC_SQR +Openness + popdensity + CI + Patents + `R_D_EXP_GDP_01`,data = regg)
m_vif
vif(m_vif)
ols<-summary(m_vif)


#### 

desp_stat<-stargazer(m_vif,type = "text",title = "OLS",
                     digits = 1,out = "table.txt")
table1(~ ln_GDP_PC + CI + Patents + Openness + popdensity, data = regg_df)


# Step 1: Log GDP
ln_GDP_PC <- log(regg$GDP_PC)

# Step 2: Center it to break perfect linearity
ln_GDP_PC_centered <- scale(ln_GDP_PC, center = TRUE, scale = FALSE)

# Step 3: Square the centered variable
ln_GDP_PC_SQ <-ln_GDP_PC_centered^2
ln_GDP_PC_SQ
m_vif_fixed <- lm(`Net_Emission` ~ ln_GDP_PC_centered + ln_GDP_PC_SQ + Openness + popdensity + CI + Patents + `R_D_EXP_GDP_01`, data = regg)
vif(m_vif_fixed)

######### Running ARDL


#####convert as data frame

regg_df<-as.data.frame(regg_01)

#running ARDL WITHOUT LOG
names(regg)
names(regg)[names(regg)== "R&D_exp_of_GDP"]<- "R_D_EXP_GDP_01"

ardl_3<-auto_ardl(`Net_Emission_01`~ GDP_PC + GDP_PC_SQR +Openness + popdensity + CI 
                  + Patents + `R_D_EXP_GDP_01`+ H_dummy + UM_dummy + LM_dummy,data = regg 
                  ,max_order = 2,selection = "AIC")
ardl_3
summary(ardl_3$best_model)

print(summary(ardl_3$best_model))

fit<- ardl_3$best_model
plot(fit)

# If your model is saved as fit:
stargazer(fit, 
          type = "html",                   # Output format: text, html, latex, word
          title = "Table: ARDL Model Results", 
          align = TRUE, 
          omit.stat = c("f", "ser"),        # Omitting F-stat and Standard Error if preferred
          out = "ARDL_Results.html")        # Output file, opens in a browser for Word copy-paste

#modelsummary(fit,output = "Regression_Result.docx")
#modelsummary(fit)
#install.packages("rstudioapi")
#library(rstudioapi)
#getwd()

# Breusch-Godfrey test for autocorrelation
bgtest(fit)

# Durbin-Watson test (for first-order autocorrelation)
dwtest(fit)

bptest(fit)  #  Heteroskedasticity (Breusch-Pagan Test)

jarque.bera.test(residuals(fit))   #Normality of Residuals (Jarque-Bera Test)

install.packages("modelsummary")
library(modelsummary)


coeftest(fit, vcovHC(fit, type = "HC1"))     #Apply coeftest() with Robust VCOV

vcovHC(fit, type = "HC0")  # White's original estimator
vcovHC(fit, type = "HC3")  # More conservative (like in small samples)








###### Descriptive statics 
install.packages("psych")
library(psych)
describe(regg)
summary(regg)

install.packages("stargazer")
library(stargazer)
desp_stat<-stargazer(regg,type = "text",title = "Descriptive Statistics",
                     digits = 1,out = "table.txt")
summary(desp_stat)
install.packages("corrplot")
library(corrplot)
corr_regg<-cor(regg)
corrplot(regg,method = 'number')


# adding data into dataset

regg$ln_GDP_PC<-ln_GDP_PC
regg$ln_GDP_PC_SQ<-ln_GDP_PC_SQ
attach(regg)
names(regg_01)
attach(regg_01)
names(regg_01)[names(regg_01)== "Net Emission"]<- "Net_Emission"
names(regg_01)[names(regg_01)== "R&D_exp_of_GDP"]<- "R_D_EXP_GDP"

ardl_1<-auto_ardl(Net_Emission ~ `ln_GDP_PC` + `ln_GDP_PC_SQ` + `popdensity` + `CI` + `Patents` +`R_D_EXP_GDP`+ Openness + H_dummy + UM_dummy + LM_dummy, data = regg_df ,max_order = 2,selection = "AIC")
ardl_1
summary(ardl_1$best_model)


fit1<- ardl_1$best_model

# Breusch-Godfrey test for autocorrelation
bgtest(fit1)

# Durbin-Watson test (for first-order autocorrelation)
dwtest(fit1)

bptest(fit1)  #  Heteroskedasticity (Breusch-Pagan Test)

jarque.bera.test(residuals(fit1))   #Normality of Residuals (Jarque-Bera Test)


coeftest(fit, vcovHC(fit, type = "HC1"))     #Apply coeftest() with Robust VCOV

vcovHC(fit, type = "HC0")  # White's original estimator
vcovHC(fit, type = "HC3")  # More conservative (like in small samples)



###### Running direct ARDL test 
ardl_2<-ardl(Net_Emission ~ `ln_GDP_PC` + `ln_GDP_PC_SQ` + `popdensity` + `CI` + `Patents` +`R_D_EXP_GDP` + Openness + H_dummy + UM_dummy + LM_dummy, data = regg_df ,order= c(1,2,0,0,1,1,1,0,1,1,1))
ardl_2

summary(ardl_2)
detach(regg_01)

#Test for cointegration

bounds_f_test(ardl_1$best_model,case = "rc")


##### test for Error Correction Model (ECM)
install.packages("ecm")
library(ecm)
as.data.frame(ardl_2)
library(ARDL)
regg_df
longrun<-lm(`Net Emission`~ ln_GDP_PC + ln_GDP_PC_SQ + popdensity + `R_D_EXP_GDP` + Openness + H_dummy + UM_dummy + LM_dummy, data = regg_df)
regg_01$ECT<-residuals(longrun)

order=c(1,2,0,0,1,1,1,0,1,1,1)
library(dynlm)
library(zoo)
library(ARDL)
#ensure ts format if needed
regg_ts<-zoo(regg_df,order.by = regg_df$TIME_PERIOD)


uecm<-uecm(ardl_2)
summary(uecm)


# Load necessary libraries
library(readxl)
library(dplyr)
library(broom)


# Step 2: Create a normalized time trend (starting from 0 for each country)
regg <- regg %>%
  group_by(Country) %>%
  arrange(TIME_PERIOD) %>%
  mutate(trend = TIME_PERIOD - min(TIME_PERIOD)) %>%
  ungroup()
names(regg)
# Step regg# Step 3: Run Net Emission ~ trend regression for each country
trend_results <- regg %>%
  group_by(Country) %>%
  do(tidy(lm(`Net_Emission_01` ~ trend, data = .))) %>%
  filter(term == "trend") %>%
  select(Country, beta = estimate, p_value = p.value, std_error = std.error)

# Step 4: View the slope (β) of trend for each country
print(trend_results,n=53)

summary(trend_models)
summary(trend_results)
summary(trend_coeffs)
library(plm)

## Fixed EFFECT model
fe_model <- plm(`Net_Emission_01`~ GDP_PC + GDP_PC_SQR +Openness + popdensity + CI + Patents + `R_D_EXP_GDP_01`+ H_dummy + UM_dummy + LM_dummy,
                data = regg ,
                model = "within",      # "within" indicates fixed effects
                effect = "individual"  # you can also try "twoways" if you want both entity and time FE
)
fe_model
summary(fe_model)

## random effect
re_model <- plm(`Net_Emission_01`~ GDP_PC + GDP_PC_SQR +Openness + popdensity + CI + Patents + `R_D_EXP_GDP_01`+ H_dummy + UM_dummy + LM_dummy,
                data = regg ,
                model = "random",       # This specifies random effects
                effect = "individual"   # Random intercepts across banks (not over time)
)

summary(re_model)
phtest(fe_model,re_model)











#Now checking causality for variables 
library(plm)
###Convert your dataset into a pdata.frame, which is the panel data structure
pdata<-pdata.frame(regg_df,index = c("Country","TIME_PERIOD"))

##Conduct Panel Granger Causality Tests
pgrangertest(`Net_Emission`~`ln_GDP_PC`,data = pdata)


# List of independent variables
independent_vars <- c("ln_GDP_PC", "ln_GDP_PC_SQ", "popdensity", "CI", "Patents", "Openness","R_D_EXP_GDP")

# Loop through each variable and perform the Granger causality test
for (var in independent_vars) {
  formula <- as.formula(paste("Net_Emission ~", var))
  test_result <- pgrangertest(formula, data = pdata, order = 2)
  cat("\nGranger causality test for:", var, "\n")
  print(test_result)
}

###net_emission ln_gdp_pc ln_gdp_pc_sq popdensity r_d_exp_of_gdp openness h_dummy um_dummy lm_dummy
