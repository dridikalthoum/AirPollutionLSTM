#install.packages("ggplot2")
#install.packages("corrplot")

library(ggplot2)
library(corrplot)

#Import datafile
file_path1 <- file.choose()
data1 <- read.csv(file_path1)

#Data Stucture and Summary
str(data1)
summary(data1)

#Parsing Datetime format
data1$date <- as.POSIXct(data1$date)

#Checking for missing values
na_columns <- colSums(is.na(data1)) > 0
print(names(na_columns)[na_columns])

#Creating a dataframe
Manually specify column names
col_names <- c('date','pollution', 'dew', 'temp', 'press', 'wnd_dir', 'wnd_spd', 'snow', 'rain')
colnames(data1) <- col_names
View(data1)

# Boxplot for each column
par(mfrow = c(2, 4))  # Set up the layout for subplots
for (i in 2:ncol(data1)) {  # Exclude the first column (date)
  boxplot(data1[, i], main = colnames(data1)[i], ylab = "Values")
}
par(mfrow = c(1, 1))


# Computing the correlation matrix for selected columns
cor_cols <- c('pollution', 'wnd_spd', 'rain', 'snow', 'temp')
correlation_matrix <- cor(data1[, cor_cols])
options(repr.plot.width = 8, repr.plot.height = 8)  # Set the width and height of the plot
corrplot(correlation_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45)

#Data Understanding plot
ggplot(data1, aes(x = date, y = pollution)) +
  geom_line() +
  labs(title = "Pollution Over Time",
       x = "Date",
       y = "Pollution") +
  theme_minimal()


