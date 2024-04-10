# Define the observed random sample
data <- c(1.1, 1.1, 2.8, 1.5, 2.4, 1.5, 3.1, 3.1)

# Calculate the empirical cumulative distribution function (ECDF)
ecdf_function <- ecdf(data)

plot(ecdf_function)


D1 <- read.csv('C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\Scripts\\Data_A.csv')

ecdf_function <- ecdf(D1$Number_of_Accidents)
plot(ecdf_function)

D2 <- read.csv('C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\Scripts\\Data_B.csv')

ecdf_function <- ecdf(D2$payment)
plot(ecdf_function)
