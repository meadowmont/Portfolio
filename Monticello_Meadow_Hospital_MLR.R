## Meadow Monticello
# Prof Hurms
# DSS 445 
# 11/9/2025

### HW 6 - Data Mining I 

# # Clear environment 
rm(list = ls()); graphics.off()

# Load readxl 
library(readxl)

# Read Excel file 
hospital <- read_excel("C:/Users/Meadow/OneDrive - Saint Joseph's University/dss445/Datasets/hospital_staffing_satisfaction.xlsx")

# View first few rows and summary
head(hospital)
summary(hospital)

# Dummy Variables
hospital$WeekendDummy <- ifelse(hospital$Weekend == "Yes", 1,0) # Yes is 1, No is 0
hospital$EmergencyLevelDummy <- ifelse(hospital$EmergencyLevel == "High", 3,
                                     ifelse(hospital$EmergencyLevel == "Medium", 2, 1)) #AI quest

# warehouse$EmergencyLevelDummy <- ifelse(hospital$EmergencyLevel == "High", 1,0) # my first attempt
head(hospital[, c("EmergencyLevel", "EmergencyLevelDummy")]) # check to see if it worked

# Model 1: All precitors
model1 <- lm(SatisfactionScore ~ PatientsSeen + AvgWaitTime + NursesOnShift + DoctorsOnShift 
             + WeekendDummy + EmergencyLevelDummy, data=hospital)
summary(model1)

# Model 2: Remove Insignificant Variables
model2 <- lm(SatisfactionScore ~ PatientsSeen + AvgWaitTime + NursesOnShift 
             + WeekendDummy + EmergencyLevelDummy, data=hospital) # remove DoctorsOnShift
summary(model2)

# Model 3: Add an Interaction Term
hospital$Nurses_Weekend <- hospital$NursesOnShift * hospital$WeekendDummy
model3 <- lm(SatisfactionScore ~ PatientsSeen + AvgWaitTime + NursesOnShift 
             + WeekendDummy + EmergencyLevelDummy + Nurses_Weekend, data=hospital) # remove DoctorsOnShift
summary(model3)

# Interaction Plot
with (hospital,
      interaction.plot(x.factor = NursesOnShift,
                       trace.factor = Weekend,
                       response = SatisfactionScore,
                       main="Interaction Plot: NursesOnShift x Weekend",
                       xlab="NursesOnShift",
                       ylab="Weekend",
                       col=c("red","darkgreen"),
                       lwd = 2
      )
)