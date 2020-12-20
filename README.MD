# Herrarchial Clustering Using R

## Process

1. Install R 
2. Prepare csv from input file (Excel .xlsx)
3. Start R

a. Read the data

```
DF=read.csv('inputFile.csv',sep=';') # Reads the csv with the mentioned seperator character ;
```

b. Find column names

```
colnames(DF) # Prints out column names in the Data Frame
```
```str(DF) # Gives observations count and column variables with levels of variance
summary(DF) # Gives a nice outline of the dataframe, column wise
```
any(is.na(DF)) # returns if there is any missing value in the Data Frame
as.numeric(as.factor # converts Categories into integer representations
```
