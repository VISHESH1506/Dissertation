#installed.packages(c("tidyverse", "scales", "viridis", "plotly","reshape2","readxl"))
install.packages("tidyverse")
install.packages("scale")
install.packages("viridis")
install.packages("plotly")
install.packages("reshape2")
install.packages("readxl")
library(tidyverse)

#library(readxl)  # if reading from Excel
library(reshape2) # for melting
library(scales)
library(viridis)
library(plotly)  
net_emission
# change data name to data_emission
data_emission<-net_emission
data_emission
#now changing data into long form
data_long_emission<-melt(data_emission,id.vars = "TIME_PERIOD",variable.name = "Country",value.name = "Net Emission")
data_long_emission

###### Now changing data of GDP_PC into the long form
data_GDP_PC<-GDP_PC
data_GDP_PC
#Now changing in the long form
data_long_GDP_PC<-melt(data_GDP_PC,id.vars = "TIME_PERIOD",variable.name = "Country",value.name = "GDP_PC")
data_long_GDP_PC

###### Now changing data of emission_from import 
#Now changing in the long form 
data_long_imp_emission<-melt(EMISSION_FROM_IMP,id.vars = "Year",variable.name = "Country",value.name = "Emission")
names(data_long_imp_emission)
data

###### Now changing data of emission_from import 
#Now changing in the long form
data_long_exp_emission<-melt(EMISSION_FROM_EXP,id.vars = "TIME_PERIOD",variable.name = "Country",value.name = "Net Emission")
data_long_exp_emission

###### Now changing data of patent into long form
names(one_family_patent_)
long_patent<-melt(one_family_patent_,id.vars = "Time period",variable.name = "Year",value.name = "Patents")
long_patent

###### Now changing trade openness in long form
names(openness_Trade_of_GDP_)
long_openness<-melt(openness_Trade_of_GDP_,id.vars = "Country Name",variable.name = "Year",value.name = "Openness")
long_openness

##### Now changing popdensity in long form
long_popdensity<-melt(popdensity,id.vars = "Country",variable.name = "Year",value.name = "popdensity")
long_popdensity

##### Now changing carbon intensity in long form
long_CI<-melt(Carbon_Intensityof_GDP_kg_CO2e_per_2021_PPP_,id.vars = "Country",variable.name = "Year",value.name = "CI")
long_CI

##### Now changing technology lavel in long from 
long_tech<-melt(Research_and_development_expenditure_of_GDP_,id.vars = "Country",variable.name = "Year",value.name = "R&D_exp_of_GDP")
long_tech

#### Now changing income group in long form #######
long_IG<-melt(income_group_1,id.vars="Country",variable.name = "Year",value.name = "Income_Group")
long_IG

########## Interpolation popdensity

long_popdensity_intrept <- long_popdensity %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(popdensity = na.approx(popdensity, na.rm = FALSE)) %>%
  ungroup()
long_popdensity_intrept

########## interpolation patent ############
library(dplyr)
library(zoo)
names(long_patent)[names(long_patent)=="Time period"]<-"Country"
long_patent
names(long_patent)
#interpolation missing values
long_patent_interp<-long_patent %>% 
  group_by(Country) %>% 
  arrange(Year,.by_group = TRUE) %>% 
  mutate(Patents = na.approx(Patents,na.rm = FALSE)) %>% 
  ungroup()
long_patent_interp

sum(is.na(long_patent_interp$Patents))

# To save the long form data
install.packages("readr")
library(readr)
#Path where document is saving
getwd()
#saving the data
write.csv(data_long_emission,"data_long_emission.csv")
write.csv(data_long_GDP_PC,"data_long_GDP_PC.csv")
write.csv(data_long_imp_emission,"data_long_imp_emission.csv")
write.csv(data_long_exp_emission,"data_long_exp_emission.csv")
write.csv(long_patent,"long_patent.csv")
write.csv(long_patent_interp,"long_patent_interp.csv")
write.csv(long_openness,"long_openness.csv")
write.csv(long_popdensity_intrept,"long_popdensity_intrept.csv")
write.csv(long_CI,"long_CI.csv")
write.csv(long_tech,"long_tech.csv")
write.csv(long_IG,"long_IG.csv")

# NOW working on combine data
COM_DATA
attach(COM_DATA)
library(lmtest)
# making the gdp sqr term
GDP_PC_SQ<- COM_DATA$GDP_PC*COM_DATA$GDP_PC

#Now running the regression 
regg_1 <- lm(`Net Emission`~ GDP_PC + GDP_PC_SQ,data = COM_DATA)

#Result of regression
regg_1
#summary of regression
summary(regg_1)

#Fitting the regression line
fitt_r1<-fitted(regg_1)

#ploting the 

plot(fitt_r1~GDP_PC,data = COM_DATA)
############
class(data_long$TIME_PERIOD)
colnames(data_long)<-c("Year", "Country", "Net_Emissions")
p<-ggplot(data_long,aes(x=Year,y=Net_Emissions,color=Country))+
  geom_line(size = 1)+
  scale_color_viridis_d(option = "turbo") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net CO₂ Emissions by Country (1995–2018)",
    x="Year",y="Net_Emission",color="Country")+
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(face = "bold",size=18),
    axis.text.x = element_text(angle = 45,hjust=1),
    length.position ="right"
  )
p
ggplotly(p)
install.packages("htmlwidgets")
library(htmlwidgets)

#save the interactive plot

saveWidget(ggplotly(p),file="net_emission_plot.html")
