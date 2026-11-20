patient_data <- data.frame(
  Patient_ID = c(101, 102, 103, 104, 105,
                 106, 107, 108, 109, 110),
  
  Age = c(25, 32, 45, 38, 29,
          51, 42, 35, 48, 30),
  
  Sex = c("Female", "Male", "Female", "Male", "Female",
          "Male", "Female", "Male", "Female", "Male"),
  
  Treatment = c("Drug A", "Drug B", "Drug A", "Drug B", "Drug A",
                "Drug B", "Drug A", "Drug B", "Drug A", "Drug B"),
  
  Baseline_Weight = c(70, 82, 65, 90, 68,
                      85, 72, 88, 75, 80),
  
  Week12_Weight = c(67, 78, 63, 86, 65,
                    81, 69, 84, 72, 77)
)

# Display data set
print(patient_data)
head(patient_data)
str(patient_data)
summary(patient_data)
table(patient_data$Sex)
table(patient_data$Treatment)
mean(patient_data$Age)
median(patient_data$Age)
sd(patient_data$Age)
mean(patient_data$Baseline_Weight[patient_data$Treatment=="Drug A"])
mean(patient_data$Baseline_Weight[patient_data$Treatment=="Drug B"])
mean(patient_data$Week12_Weight[patient_data$Treatment=="Drug A"])
mean(patient_data$Baseline_Weight[patient_data$Treatment=="Drug B"])
patient_data$Weight_change<-patient_data$Week12_Weight- patient_data$Baseline_Weight
print(patient_data)
mean(patient_data$Weight_change)
min(patient_data$Weight_change)
max(patient_data$Weight_change)
aggregate(patient_data$Weight_change,by=list(Treatment=patient_data$Treatment),FUN=mean)

ae_data <- data.frame(
  
  Patient_ID = c(101, 102, 103, 104, 105,
                 106, 107, 108),
  
  AE_Term = c("Headache", "Nausea", "Dizziness",
              "Headache", "Fatigue", "Nausea",
              "Vomiting", "Headache"),
  
  Severity = c("Mild", "Moderate", "Mild",
               "Severe", "Mild", "Moderate",
               "Severe", "Mild"),
  
  Treatment = c("Drug A", "Drug B", "Drug A",
                "Drug B", "Drug A", "Drug B",
                "Drug B", "Drug A")
)

# Display Adverse Events data set
print(ae_data)
table(ae_data$AE_Term)
table(ae_data$Severity)
table(ae_data$Treatment,ae_data$Severity)
colSums(is.na(patient_data))   #checking missing values
duplicated(patient_data$Patient_ID)    #check duplicate patientID
sum(duplicated(patient_data$Patient_ID))

patient_data_clean <- patient_data

patient_data_clean <- patient_data_clean[
  !duplicated(patient_data_clean$Patient_ID),
]

print(patient_data_clean)
colSums(is.na(ae_data))  
ae_data_clean<- na.omit(ae_data)   #removes the values with missing values
print(ae_data_clean)
mean(patient_data$Age)
median(patient_data$Age)
sd(patient_data$Age)
min(patient_data$Age)
max(patient_data$Age)
table(patient_data$Treatment)

prop.table(table(patient_data$Treatment))*100

aggregate(patient_data$Weight_change,by=list(Treatment=patient_data$Treatment),FUN=mean)

#graphs

install.packages("ggplot2")
library(ggplot2)

ggplot(patient_data,
       aes(x = Treatment)) +                      #axis
  geom_bar() +                                    #for bar chart
  labs(                                           #for graph title and axis names
    title = "Patient Distribution by Treatment",
    x = "Treatment Group",
    y = "Number of Patients"
  )

#age range

ggplot(patient_data,
       aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(
    title = "Patient Age Distribution",
    x = "Age",
    y = "Number of Patients"
  )

#Boxplot- weight_change

ggplot(patient_data,
       aes(x = Treatment,
           y = Weight_change)) +
  geom_boxplot() +
  labs(
    title = "Weight Change by Treatment",
    x = "Treatment",
    y = "Weight Change (kg)"
  )

#Adverse events bar chart

ggplot(ae_data,
       aes(x = AE_Term)) +
  geom_bar() +
  labs(
    title = "Adverse Events Distribution",
    x = "Adverse Event",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#Clinical_summary_Tables

patient_summary<- as.data.frame(table(patient_data$Treatment))
print(patient_summary)

age_summary<-aggregate(Age~Treatment,data=patient_data, FUN=mean)
print(age_summary)                                   #Avg age by treatment

weight_summary<-aggregate(Weight_change~Treatment, data=patient_data,FUN=mean)
print(weight_summary)                                #Weight change summary

#Adverse Event Summary

ae_summary <- as.data.frame(
  table(ae_data$AE_Term)
)

names(ae_summary) <- c("Adverse_Event", "Frequency")

print(ae_summary)

#Severity Summary

severity_summary <- as.data.frame(
  table(ae_data$Severity)
)

names(severity_summary) <- c("Severity", "Frequency")

print(severity_summary)

#Creating output folder

dir.create("Output", showWarnings = FALSE)

write.csv(
  patient_data_clean,                          #save patient data set
  "Output/Patient_Data.csv",
  row.names = FALSE
)

write.csv(                                    #save AE data set
  ae_data_clean,
  "Output/Adverse_Events.csv",
  row.names = FALSE
)


write.csv(
  age_summary,                               #save summary tables
  "Output/Age_Summary.csv",
  row.names = FALSE
)

write.csv(
  weight_summary,
  "Output/Weight_Summary.csv",                  
  row.names = FALSE
)

write.csv(
  ae_summary,
  "Output/AE_Summary.csv",
  row.names = FALSE
)

ggsave(
  "Output/Patient_Distribution.png",                      #save graphs
  width = 7,
  height = 5
)

