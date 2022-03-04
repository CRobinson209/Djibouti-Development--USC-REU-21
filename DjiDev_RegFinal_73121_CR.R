#DjiDev Regression Data Management
#Created by Zack Johnson and Cassiana RObinson
#7.6.21
install.packages("texreg")
install.packages("nlme")

#libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(texreg)
library(ggplot2)
library(nlme)
#I can't set my wd to google drive even though I have it downloaded? Maybe redownload GD
#setwd()

#never run out of a downloads folder - draw it from shared folder 
#CHANGE, go back to orig video 
#file path to excel file from google drive 
path_x <- "C:/Users/circulating/Documents/Cassi R SPEC work/DjiDev_RegressionDraft1_CR6.29.21.xlsx"
path_y <- "C:/Users/circulating/Documents/Cassi R SPEC work/DjiDev_Data_HDI_AZ.xlsx"
path_oda <- "C:/Users/circulating/Documents/Cassi R SPEC work/DjiDev_ODAReg_7.30.21.xlsx"
#names of the desired sheets
sheets_x <- readxl::excel_sheets(path_x)
sheets_y <- readxl::excel_sheets(path_y)
sheets_oda <- readxl::excel_sheets(path_oda)

#apply read_excel() function to all relevent sheets
dat_x <- lapply(sheets_x, function(x) read_excel(path = path_x, sheet = x))
dat_y <- lapply(sheets_y, function(y) read_excel(path = path_y, sheet = y))
dat_oda <- lapply(sheets_oda, function(z) read_excel(path = path_oda, sheet = z))

#create dataframes
military_bases_spec <- dat_x[[1]] 

  
polity5_csp <- dat_x[[2]]

dyadic_trade_cow <- dat_x[[3]] %>%
  arrange(year) 

oda_oecd <- dat_x[[4]] %>%
  rename(year = Year) %>%
  select(year, WBODA)


hdi_Y <- dat_y[[1]] %>%
  filter(year %in% c(1995:2014))

#working on oda ONLY with MORE years to interpret the r-squared btwn orig data and this one
oda_ONLY <- dat_oda[[1]]

#merge military base dataset and polity5 dataset
#merged dataset will be what we use going forward
merged_miltpol <- full_join(military_bases_spec, polity5_csp, by = "year")
  
  
merged_milt_polity_oda <- full_join(merged_miltpol, oda_oecd, by = "year") %>%
  filter(year %in% c(1995:2014))

#idk if this works



#**Averaging with correct years attached
wide_data <- pivot_wider(dyadic_trade_cow, names_from = "year", values_from = "flowab") 
            
            
# make a subset of the years, make it into an empty dataframe, pivot this data frame so that           
#group by year and take an average of the values and put it into a new column 


no_na_trade_data <- wide_data[c(82:86,88:281),c(22:46)] #ok so this isn't the greatest code. I just took out row 87 bc it had -9.0 which means NA in this dataset

#manually average, so you have to change the tibble into a vector
mean_tradeflow_vec = c(
                       mean(no_na_trade_data$`1995`, na.rm = T),
                       mean(no_na_trade_data$`1996`, na.rm = T),
                       mean(no_na_trade_data$`1997`, na.rm = T),
                       mean(no_na_trade_data$`1998`, na.rm = T),
                       mean(no_na_trade_data$`1999`, na.rm = T),
                       mean(no_na_trade_data$`2000`, na.rm = T),
                       mean(no_na_trade_data$`2001`, na.rm = T),
                       mean(no_na_trade_data$`2002`, na.rm = T),
                       mean(no_na_trade_data$`2003`, na.rm = T),
                       mean(no_na_trade_data$`2004`, na.rm = T),
                       mean(no_na_trade_data$`2005`, na.rm = T),
                       mean(no_na_trade_data$`2006`, na.rm = T),
                       mean(no_na_trade_data$`2007`, na.rm = T),
                       mean(no_na_trade_data$`2008`, na.rm = T),
                       mean(no_na_trade_data$`2009`, na.rm = T),
                       mean(no_na_trade_data$`2010`, na.rm = T),
                       mean(no_na_trade_data$`2011`, na.rm = T),
                       mean(no_na_trade_data$`2012`, na.rm = T),
                       mean(no_na_trade_data$`2013`, na.rm = T),
                       mean(no_na_trade_data$`2014`, na.rm = T)
                       ) 
            

#make dyadic trade BACK into a dataframe so we can merge all of the "X data" together

dyadic_trade_avg_cow <- data_frame("year" = 1995:2014, "mean_tradeflow_ab" = mean_tradeflow_vec) #i think i did ittttt

Dji_Reg_Dataset_Full <- full_join(merged_milt_polity_oda, dyadic_trade_avg_cow, by = c("year"))

###########################################################
#Regression Type A
#now begin saving the "x value(s)" and "y value(s)" ONLY as objects so that the regression code is short and easy to call
reg_ctrlx <- Dji_Reg_Dataset_Full %>%
  select(military_base, intl_owned_port, polity2, mean_tradeflow_ab)

reg_odax <-  Dji_Reg_Dataset_Full %>%
  select(WBODA)
  
reg_y <- hdi_Y %>% 
  select(`HDI index`)

reg_oda_only <- oda_ONLY %>%
  select(`HDI Index`, WBODA)

#saving as a matrix
ctrlx <- as.matrix(reg_ctrlx)
y <- as.matrix(reg_y)
x_oda <- as.matrix(reg_odax)

#this provides coefficients, saved as an object
multivar_reg_ctrl <- t(cov(y, ctrlx) %*% solve(cov(ctrlx)))
multivar_reg_oda <- t(cov(y, x_oda) %*% solve(cov(x_oda)))

##################################################################33


#typeB - used this type for final product *****
reg_XY_values <- full_join(Dji_Reg_Dataset_Full, hdi_Y, by = c("year"))

reg_ctrl <- lm(`HDI index` ~ military_base + intl_owned_port + polity2 + mean_tradeflow_ab, data = reg_XY_values)
reg_oda <- lm(cbind(`HDI index`) ~ WBODA, data = reg_XY_values)
reg_allx <- lm(cbind(`HDI index`) ~ WBODA + military_base + intl_owned_port + polity2, data = reg_XY_values)
reg_o_ONLY <- lm(`HDI Index` ~ WBODA, data = reg_oda_only)
#only control x's graphed
ggplot(reg_XY_values, aes(x = military_base + intl_owned_port + polity2 + mean_tradeflow_ab, y = `HDI index` )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#only ODA, principle x, graphed
ggplot(reg_XY_values, aes(x = WBODA, y = `HDI index` )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#ALL X's graphed
ggplot(reg_XY_values, aes(x = WBODA + military_base + intl_owned_port + polity2 + mean_tradeflow_ab, y = `HDI index` )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") #+
#****Why is All x graph the exact same as ODA only graph?





#now check your residuals to see if there is/isn't a pattern
#GIVE MORE INFO about what WE want to see?
plot(mlm1$residuals, pch = 16, col = "red")
plot_resid_v_fitted <- plot(lm(cbind(`HDI index`) ~ military_bases_spec + intl_owned_port + polity2 + mean_tradeflow_ab, data = maybe))
#####################################################################################
#Making Regression Table - use htmlreg to output to an html link
#current output is to my personal desktop 
htmlreg(list(reg_ctrl, reg_oda, reg_allx),
          file = "~/Cassi R SPEC work/djibouti_regs_CR_72921.html",
          caption = "International Activity, Aid, and Human Development of Djibouti",
          caption.above = TRUE,
          custom.model.names = c("Control X's", "ODA only", "ODA + Control X's") )
#WORKS!! - Outputs an html

#now output an html table for the oda ONLY (to compare R squared)
htmlreg(list(reg_o_ONLY),
        file = "~/Cassi R SPEC work/djibouti_ODA_CR_72921.html",
        caption = "ODA ONLY: Aid and Human Development of Djibouti",
        caption.above = TRUE,
        custom.model.names = c("ODA only") )




