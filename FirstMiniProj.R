#creating directory of the unzipped file
specdata <- "/Users/rgran/OneDrive/Desktop"

list.files("specdata")

###########################################################

#Write a function named pollutantMean that calculates the mean of a pollutant (sulfate or nitrate) across a 
#specified list of monitors. The function pollutantMean takes 3 argument: directory, pollutant, and id. Given a 
#vector monitor ID numbers, pollutantMean reads that monitor's particulate matter data from the directory specified 
#in the directory argument and returns the means of the pollutant across all of the monitors, ignoring any missing 
#values coded as NA

pollutantmean <- function(directory, pollutant, id = 1:332)
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating 
  ## the name of the pollutant for which we will calculate the
  ## mean; either sulfate or nitrate
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## You do not need to round the results.
  {
    #create a list of files
    files_full <- list.files(directory, full.names = TRUE) 
    # create an empty data frame
    dat <- data.frame()
    for (i in id)
    {
      #add files to main data
      dat <- rbind(dat, read.csv(files_full[i]))
      
    }
    #Calulate mean
    mean_data <- mean(dat[, pollutant], na.rm = TRUE)
    return(mean_data)
}


#OUTPUT
#pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064128

#pollutantmean("specdata", "nitrate", 70:72)
#[1] 1.706047

#pollutantmean("specdata", "nitrate", 23)
#[1] 1.280833

###########################################################

#Write a function named complete that reads a directory full of 
#files and reports the number of completely observed cases in each data file. The function should return a data frame 
#where the first column is the name of the file, and the second column is the number of complete cases.

complete <- function(directory, id = 1:332)
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## return a data frame form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where id is the monitor ID number and nobs is the number
  ## of complete cases
{
  #Create a list of file
  files_full <- list.files(directory, full.names= TRUE)
  # Create empty data frame 
  dat <- data.frame()
  for (i in id)
  {
    # Read files
    temp <- read.csv(files_full[i])
    # nobs are sum of all complete cases
    nobs <-sum(complete.cases(temp))
    # Enamurtates complete cass by index
    dat <-rbind(dat, data.frame(i, nobs))
    
  }
  colnames(dat) <- c("id", "nobs")
  return(dat)
}


#OUTPUT
#complete("specdata", 1)
#id nobs
#1  1  117

#complete("specdata", c(2, 4, 8, 10, 12))
# id nobs
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96

#complete("specdata", 30:25)
#id nobs
#1 30  932
#2 29  711
#3 28  475
#4 27  338
#5 26  586
#6 25  463

#complete("specdata", 3)
#id nobs
#1  3  243

###########################################################

#Write a function named corr that takes a directory of data files and a 
#threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the 
#number of completely observed cases (on all variables) is greater than the threshold. The function should return a 
#vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold 
#requirement, then the function should return a numeric vector of length 0. 

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of ompletely observed observations (on all variables) 
  ## required to compute the correlation between 
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## Do not round the result.
  df = complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  corrr = numeric()
  for (i in ids) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
complete <- function(directory, id = 1:332) {
  f <- function(i) {
    data = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                          ".csv", sep = ""))
    sum(complete.cases(data))
  }
  nobs = sapply(id, f)
  return(data.frame(id, nobs))
}

#OUTPUT
#cr <- corr("specdata", 150)
#head(cr); summary(cr)

##[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
##Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

#cr <- corr("specdata", 400)
#head(cr); summary(cr)

##[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
##Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

#cr <- corr("specdata", 5000)
#head(cr); summary(cr); length(cr)

##numeric(0)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##[1] 0

#cr <- corr("specdata")
#head(cr); summary(cr); length(cr)

##[1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
##Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##-1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
##[1] 323

###########################################################


outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)

#ncol(outcome)
#nrow(outcome)
#names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

#modified code
hist(outcome[, 11], col = " sky blue",
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", 
     xlab = "Deaths", ylab = "Frequency")

