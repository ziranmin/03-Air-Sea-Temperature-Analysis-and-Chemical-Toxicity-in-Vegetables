
# MA415/615
# Assignment 5, Porject 2, Part 2
# 03/19/2018
# Name: Ziran Min, Xi Cheng, ShihChing Huang

library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
library(RColorBrewer)
options(warn = -1)

########################################
# Step 1: Cleaning the original dataset 
######################################## 

veg.1 <- read_xlsx("veg1.xlsx")

# First use code from Class 19

cnames.1 <- colnames(veg.1)

c <- apply(veg.1, 2, n_distinct)

c[c>1]

d <- names(c[c==1])

e <- names(c[c>1])

# delete all useless columns (which only have one distinct value)

veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)

apply(veg.2, 2, n_distinct)

# change some columns names 
veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)

a1 <- separate(veg.3, Category, into = c("Label", "Quant"), sep=",")

# We only focus on the data with "RESTRICTED USE CHEMICAL"

a2 <- filter(a1, Label=="RESTRICTED USE CHEMICAL")

# We have 2 commodity left, BROCCOLI and CAULIFLOWER

unique(a2[,"Commodity"])

a21 <- a2 %>% select(Label, Quant) %>% unique()

# We have about 28 chemicals left 

a21 %>% print(n=30)

# Keep clean the dataset 

a3 <- separate(a2, Quant, into=c("Treatment Product","Chemical and Code"), sep = ":")

# Delete unnecessary column

a4 <- select(a3, -Domain)

a5 <- separate(a4 ,Data, into = c("Commodity Name", "Measureed Value"), sep = "-")

a6 <- separate(a5, "Measureed Value", into = c("Application or Treated", "Way of Measurement", "AVG or not"), sep = ",")

unique(a6[,"Geo"])

unique(a6[,"State"])

unique(a6[,"Region"])

# Delete dupilcate column and columns that has only one distinct value

a7 <- select(a6, -Geo, -State, -Region, -Commodity)

a8 <- a7 %>% separate(`Chemical and Code` , into = c("Chemical Name", "Code"), sep = "=") %>% 
  separate(`Chemical Name`, into = c("Useless1", "Chemical Name"), sep = "[()]") %>%
  separate(Code, into = c("Chemical Code", "Useless2"), sep = "[())]") %>%
  select(-Useless1, -Useless2)

# For different ways of Measurement, we are only instersted in the value of each chemical "MEASURED IN LB"

a9 <- filter(a8, `Way of Measurement`==" MEASURED IN LB")
             
# Delete the chemicals whose value we don't know

a10 <- filter(a9, !Value %in% c("(D)", "(Z)", "(NA)", NA))

# Delete unnecessary column

a11 <- select(a10, -`AVG or not`)

a11$Value <- as.numeric(a11$Value)
a11$Year <- as.character(a11$Year)
a11$`Chemical Name` <- as.character(a11$`Chemical Name`)

# So far, we have narrowed down our dataset from a huge one to this table a11 which only has 57 rows


###################################################
# Step 2: Dealing with LD5O data for each chemicals 
###################################################

unique(a11[,"Chemical Name"])

# Now, we only need to check the LD50 for 15 chemicals
# They are:
# 1 "BETA-CYFLUTHRIN "    
# 2 "BIFENTHRIN "         
# 3 "ESFENVALERATE "      
# 4 "IMIDACLOPRID "       
# 5 "LAMBDA-CYHALOTHRIN " 
# 6 "METHOMYL "           
# 7 "NALED "              
# 8 "PERMETHRIN "         
# 9 "ZETA-CYPERMETHRIN "  
# 10 "CHLORANTRANILIPROLE "
# 11 "CHLORPYRIFOS "       
# 12 "DIAZINON "           
# 13 "PRONAMIDE "          
# 14 "DISULFOTON "         
# 15 "EMAMECTIN BENZOATE " 

# Except BETA-CYFLUTHRIN and ZETA-CYPERMETHRIN, 
# we can find the LD50 for all other above chemicals from https://comptox.epa.gov/dashboard
# Let's build a table for the LD50 of the other 13 chemicals
# Then we can add the LD50 we find from other website to this table

BIFENTHRIN <- read.csv("BIFENTHRIN.csv")
as.tibble(BIFENTHRIN)
ESFENVALERATE <- read.csv("ESFENVALERATE.csv")
as.tibble(ESFENVALERATE)
IMIDACLOPRID <- read.csv("IMIDACLOPRID.csv")
as.tibble(IMIDACLOPRID)
LAMBDA_CYHALOTHRIN <- read.csv("LAMBDA-CYHALOTHRIN.csv")
as.tibble(LAMBDA_CYHALOTHRIN)
METHOMYL <- read.csv("METHOMYL.csv")
as.tibble(METHOMYL)
NALED <- read.csv("NALED.csv")
as.tibble(NALED)
PERMETHRIN <- read.csv("PERMETHRIN.csv")
as.tibble(PERMETHRIN)
CHLORANTRANILIPROLE <- read.csv("CHLORANTRANILIPROLE.csv")
as.tibble(CHLORANTRANILIPROLE)
CHLORPYRIFOS <- read.csv("CHLORPYRIFOS.csv")
as.tibble(CHLORPYRIFOS)
DIAZINON <- read.csv("DIAZINON.csv")
as.tibble(DIAZINON)
PRONAMIDE <- read.csv("LD50 Data/PRONAMIDE.csv")
as.tibble(PRONAMIDE)
DISULFOTON <- read.csv("DISULFOTON.csv")
as.tibble(DISULFOTON)
EMAMECTIN_BENZOATE <- read.csv("EMAMECTIN BENZOATE.csv")
as.tibble(EMAMECTIN_BENZOATE)

# The experiment animal we choose is rat

BIFENTHRIN <- BIFENTHRIN %>% mutate( `Chemical Name` = "BIFENTHRIN") %>% filter(SPECIES == "rat")
ESFENVALERATE <- ESFENVALERATE %>% mutate( `Chemical Name` = "ESFENVALERATE") %>% filter(SPECIES == "rat")
IMIDACLOPRID <- IMIDACLOPRID %>% mutate( `Chemical Name` = "IMIDACLOPRID") %>% filter(SPECIES == "rat")
LAMBDA_CYHALOTHRIN <- LAMBDA_CYHALOTHRIN %>% mutate( `Chemical Name` = "LAMBDA-CYHALOTHRIN") %>% filter(SPECIES == "rat")
METHOMYL <- METHOMYL %>% mutate( `Chemical Name` = "METHOMYL") %>% filter(SPECIES == "rat")
NALED <- NALED %>% mutate( `Chemical Name` = "NALED") %>% filter(SPECIES == "rat")
PERMETHRIN <- PERMETHRIN %>% mutate( `Chemical Name` = "PERMETHRIN") %>% filter(SPECIES == "rat")
CHLORANTRANILIPROLE <- CHLORANTRANILIPROLE %>% mutate( `Chemical Name` = "CHLORANTRANILIPROLE") %>% filter(SPECIES == "rat")
CHLORPYRIFOS <- CHLORPYRIFOS %>% mutate( `Chemical Name` = "CHLORPYRIFOS") %>% filter(SPECIES == "rat")
DIAZINON <- DIAZINON %>% mutate( `Chemical Name` = "DIAZINON") %>% filter(SPECIES == "rat")
PRONAMIDE <- PRONAMIDE %>% mutate( `Chemical Name` = "PRONAMIDE") %>% filter(SPECIES == "rat")
DISULFOTON <- DISULFOTON %>% mutate( `Chemical Name` = "DISULFOTON") %>% filter(SPECIES == "rat")
EMAMECTIN_BENZOATE <- EMAMECTIN_BENZOATE %>% mutate( `Chemical Name` = "EMAMECTIN BENZOATE") %>% filter(SPECIES == "rat")


# Combine them into one table, and the next step is to clean it

LD50_1 <- rbind(BIFENTHRIN, ESFENVALERATE, IMIDACLOPRID, LAMBDA_CYHALOTHRIN,
              METHOMYL, NALED, PERMETHRIN, CHLORANTRANILIPROLE, CHLORPYRIFOS, 
              DIAZINON, PRONAMIDE, DISULFOTON, EMAMECTIN_BENZOATE)


unique(LD50_1[,"Chemical Name"])

LD50_2 <- filter(LD50_1, UNITS  == "mg/kg")

unique(LD50_2[,"SOURCE"])

LD50_3 <- filter(LD50_2, SOURCE  == "AcuteTox")

unique(LD50_3[,"Chemical Name"])

unique(LD50_3[,"TYPE"])

unique(LD50_3[,"STUDY_TYPE"])


# Now we have make sure each sample is oral LD50 measured on rats in mg/kg from source AcuteTox
# But we don't have this data for EMAMECTIN BENZOATE after filtering, 
# so besides BETA-CYFLUTHRIN and ZETA-CYPERMETHRIN, we also need to find 
# the data of EMAMECTIN_BENZOATE from other sources. 
# Before that, let's keep cleaning the data

LD50_4 <- LD50_3 %>% group_by(`Chemical Name`) %>%
  dplyr::summarize(round(mean(VALUES)))

LD50_4 <- plyr::rename(LD50_4, c(`round(mean(VALUES))` = "LD50 for Rats (mg/kg)" ))

# From http://pmep.cce.cornell.edu, we get the LD5O on rats for BETA-CYFLUTHRIN and ZETA-CYPERMETHRIN,
# are 869 - 1271 mg/kg(take average as 1070 mg/kg) and 250 mg/kg respectively. 
# From http://www.rayfull.com/UploadFiles/PDF/201368165633.pdf, we know that the LD50 on rats for 
# EMAMECTIN BENZOATE is 1516 mg/kg. 
# Add these into LD50_4


aa <- c("BETA-CYFLUTHRIN", 1070)
bb <- c("ZETA-CYPERMETHRIN", 250)
cc <- c("EMAMECTIN BENZOATE", 1516)

LD50_4[nrow(LD50_4) + 1,] <- aa
LD50_4[nrow(LD50_4) + 1,] <- bb
LD50_4[nrow(LD50_4) + 1,] <- cc

LD50_4$`LD50 for Rats (mg/kg)` <- as.numeric(LD50_4$`LD50 for Rats (mg/kg)`)


###################################################
# Step 3: Combine tables from Step 1 and 2 as one 
###################################################

# combine two tables by the name of chemicals
# make the chemical name column in a11 is the same in LD50_4

a12 <- a11 %>% separate(`Chemical Name` , into = c("Chemical Name", "nothing"), sep = " ") %>%
  select(-nothing)

a12[46,7] <- "EMAMECTIN BENZOATE"

unique(a12[,"Chemical Name"])
unique(LD50_4[,"Chemical Name"])

merge1 <- left_join(a12, LD50_4, by = "Chemical Name")

# Now we have combined them in to one, but we can make it look better 

merge2 <- merge1[,c(1,2,6:10)]

merge2 <- plyr::rename(merge2, c(`Chemical Name` = "Restricted Use Chemical Name" ))
merge2 <- plyr::rename(merge2, c(Value = "Value of the Chemical Applied to the Commodity in (LB)" ))
merge2 <- plyr::rename(merge2, c(`LD50 for Rats (mg/kg)` = "Chemical's LD50 for Rats (mg/kg)" ))

# We also notice that the unit of Chemical Value and Chemical LD50 are different.
# We also want to more derictly show the effect of chemical toxicity.
# So we unify the unit and add a column to show how many rats could be killed by each amount of chemical 

improve1 <- merge2 %>% mutate("Value of the Chemical Applied to the Commodity in (mg)" = `Value of the Chemical Applied to the Commodity in (LB)` * 453592)

improve1 <- improve1[,c(1:6,8,7)]

# We want to know how many rats can be killed be each amount of chemical 
# We assume the mass of each rat is 300g (0.3kg)
# Prevent Scientific Notation of large numbers
options(scipen=999)
improve2 <- improve1 %>% 
  mutate(round("Number of 300g Rats Can Be Killed" = `Value of the Chemical Applied to the Commodity in (mg)` /(2 * 0.3 * `Chemical's LD50 for Rats (mg/kg)`)))

improve2 <- plyr::rename(improve2, c( "round(`Number of 300g Rats Can Be Killed` = `Value of the Chemical Applied to the Commodity in (mg)`/(2 * \n    0.3 * `Chemical's LD50 for Rats (mg/kg)`))" = "Number of 300g Rats Can Be Killed"))

# Now we have our final cleaned table 
Final_Table <- improve2
View(Final_Table)


############################
# Step 4: Data Visualization 
############################

# The bar pot shows the LD50 on rats for every chemical 
# We notice that CHLORANTRANILIPROLE and PRONAMIDE has the most toxicity
ggplot(LD50_4,aes(`Chemical Name`,`LD50 for Rats (mg/kg)`))+
  geom_bar(stat="identity",fill="#009E73")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "The LD50 on Rats for each Chemical") 



# Barplot for Restricted Use Chemical applied on Broccoli by Year
part1 <- filter(Final_Table, `Commodity Name`  == "BROCCOLI ")
ggplot(part1, aes( x = `Restricted Use Chemical Name`, y = `Value of the Chemical Applied to the Commodity in (LB)`, fill = Year))+
         geom_bar(position = "dodge",stat="identity",na.rm = TRUE) + 
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          scale_fill_brewer(palette = "Greens") +
        labs(title = "Restricted Use Chemical applied on Broccoli by Year")

# Barplot for Restricted Use Chemical applied on Cauliflower by Year
part2 <- filter(Final_Table, `Commodity Name`  == "CAULIFLOWER ")
ggplot(part2, aes( x = `Restricted Use Chemical Name`, y = `Value of the Chemical Applied to the Commodity in (LB)`, fill = Year))+
  geom_bar(position = "dodge",stat="identity") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "Restricted Use Chemical applied on Cauliflower by Year")

#Barplots that shows the Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Broccoli in different years
part3 <- filter(Final_Table, `Commodity Name`  == "BROCCOLI ", Year == 2016)
ggplot(part3, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Broccoli in 2016")

part4 <- filter(Final_Table, `Commodity Name`  == "BROCCOLI ", Year == 2014)
ggplot(part4, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Broccoli in 2014")

part5 <- filter(Final_Table, `Commodity Name`  == "BROCCOLI ", Year == 2010)
ggplot(part5, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Broccoli in 2010")

part6 <- filter(Final_Table, `Commodity Name`  == "BROCCOLI ", Year == 2006)
ggplot(part6, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Broccoli in 2006")

#Barplots that shows the Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Cauliflower in different years
part7 <- filter(Final_Table, `Commodity Name`  == "CAULIFLOWER ", Year == 2016)
ggplot(part7, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Cauliflower in 2016")

part8 <- filter(Final_Table, `Commodity Name`  == "CAULIFLOWER ", Year == 2014)
ggplot(part8, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Cauliflower in 2014")

part9 <- filter(Final_Table, `Commodity Name`  == "CAULIFLOWER ", Year == 2010)
ggplot(part9, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Cauliflower in 2010")

part10 <- filter(Final_Table, `Commodity Name`  == "CAULIFLOWER ", Year == 2006)
ggplot(part10, aes( x = `Restricted Use Chemical Name`, y = `Number of 300g Rats Can Be Killed`))+
  geom_bar(stat="identity", fill = "skyblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Number of 300g Rats Can Be Killed by Restricted Use Chemical applied on Cauliflower in 2006")

















