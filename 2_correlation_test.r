direct = "C:/Users/ricar/OneDrive - FCT NOVA/ISEG/Risk Models/Scripts/"; file = "Advertising.csv"
data = read.csv(paste0(direct, file, sep=""), header = TRUE, sep = ",")


cor.test(data$Sales, data$TV)

cor.test(data$Sales, data$TV, alternative = "greater")


