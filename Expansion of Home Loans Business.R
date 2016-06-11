setwd('C:\\Users\\dalalshobhit\\Desktop\\CapitalOneData') 

# Provide paths for loan data and institution data (Data location on local computer)
loanDataPath <- "C:\\Users\\dalalshobhit\\Desktop\\CapitalOneData\\2012_to_2014_loans_data.csv"
institutionDataPath <- "C:\\Users\\dalalshobhit\\Desktop\\CapitalOneData\\2012_to_2014_institutions_data.csv"

# hmda_init() implements a method to merge and get expanded data
hmda_init <- function() {
  loanData <- read.csv(loanDataPath, header = TRUE)
  typeof(loanData)
  institutionData <- read.csv(institutionDataPath, header = TRUE)
  
  mergedData <- merge(loanData, institutionData, by=c("Agency_Code","Respondent_ID", "As_of_Year"), all.x = TRUE, na.rm = TRUE)
  
  return(mergedData)
} 
# call hmda_init() and create a data frame mergedExpandedData
mergedExpandedData <- hmda_init()

##################################################################################


library(jsonlite)                 # import jsonlite library
# hmda_to_json(data, states, conventional_conforming) exports filtered data in json to disk
hmda_to_json <- function(data, state, conventional_conforming) {
  # if else loops for filtering data
  if(missing(state) ) {
    if(missing(conventional_conforming) ){        # if state is missing & not conventional_conforming
      filteredData <- data  
    }
    else {                                        # if state is missing & conventional_conforming
      filteredData <- subset(data, Conventional_Conforming_Flag == conventional_conforming)
    }
  }
  else {
    if(missing(conventional_conforming)) {        # if state is given & not conventional_conforming
      filteredData <- subset(data, State == state)
    }
    else {                                        # if state is given & conventional_conforming
      filteredData <- subset(data, State == state & Conventional_Conforming_Flag == conventional_conforming)
    } 
  }
  
  # Convert to JSON data
  sink("jsonData.json")
  cat(toJSON(filteredData))
  sink()
  # export the expanded dataset to disk (Give a path on your local machine where you want to write this file)
  write.table(filteredData, "C:\\Users\\dalalshobhit\\Desktop\\CapitalOneData\\jsonData.json", sep=":")
  return(filteredData)
}

# call hmda_to_json function and store it in showFilteredData
showFilteredData <- hmda_to_json(mergedExpandedData,,"N")

######################################################################################

library(maps)
library(mapproj)

library(sqldf)

# Create SQL queries for each type of loan grouped by year
conventionalLoanSQL <- "SELECT As_of_Year,count(*) AS Conventional_Loans FROM mergedExpandedData WHERE Loan_Type_Description LIKE 'Conventional' GROUP BY As_of_Year"
VALoanSQL <- "SELECT As_of_Year,count(*) AS VA_Loans FROM mergedExpandedData WHERE Loan_Type_Description LIKE 'VA guaranteed' GROUP BY As_of_Year"
FSA_RHS_LoanSQL <- "SELECT As_of_Year,count(*) AS FSA_RHS_Loans FROM mergedExpandedData WHERE Loan_Type_Description LIKE 'FSA/RHS guaranteed' GROUP BY As_of_Year"
FHALoanSQL <- "SELECT As_of_Year,count(*) AS FHA_Loans FROM mergedExpandedData WHERE Loan_Type_Description LIKE 'FHA insured' GROUP BY As_of_Year"

# Create tables for each type of loans
conventionalLoans <- sqldf(conventionalLoanSQL)
VALoans <- sqldf(VALoanSQL)
FSA_RHSLoans <- sqldf(FSA_RHS_LoanSQL)
FHA_Loans <- sqldf(FHALoanSQL)

# Query to merge all tables
all_LoansSQL <- "SELECT c.As_of_Year, c.Conventional_Loans, v.VA_Loans, f.FSA_RHS_Loans, fh.FHA_Loans 
                  FROM ((conventionalLoans c INNER JOIN VALoans v
                  ON c.As_of_Year = v.As_of_Year) INNER JOIN FSA_RHSLoans f
                  ON c.As_of_Year = f.As_of_Year) INNER JOIN FHA_Loans fh
                  ON c.As_of_Year = fh.As_of_Year"
allLoans <- sqldf(all_LoansSQL)

library(ggplot2)

library(reshape)

dfm <- melt(allLoans[,c('As_of_Year','Conventional_Loans','VA_Loans', 'FSA_RHS_Loans', 'FHA_Loans')],id.vars = 1)

# Bar plot each type of loans grouped by year
options(scipen=10000)
ggplot(dfm, aes(x=As_of_Year,y=value)) + 
  geom_bar(stat='identity', aes(fill=variable), position='dodge')

###################################################################################


# Create SQL queries for conventional_conforming loans vs. not conventional_conforming loans
# grouped by state
ConventionalConformingLoanSQL <- "SELECT State,count(*) AS ConventionalConforming_Loans FROM mergedExpandedData WHERE Conventional_Conforming_Flag LIKE 'Y' GROUP BY State"
ConventionalConformingLoans <- sqldf(ConventionalConformingLoanSQL)

NonConventionalConformingLoanSQL <- "SELECT State,count(*) AS NonConventionalConforming_Loans FROM mergedExpandedData WHERE Conventional_Conforming_Flag LIKE 'N' GROUP BY State"
NonConventionalConformingLoans <- sqldf(NonConventionalConformingLoanSQL)

totalLoansSQL <- "SELECT c.State, c.ConventionalConforming_Loans, n.NonConventionalConforming_Loans
                FROM ConventionalConformingLoans c INNER JOIN NonConventionalConformingLoans n
                ON c.State = n.State
                ORDER BY 1"
totalLoans <- sqldf(totalLoansSQL)

totLoans <- melt(totalLoans[,c('State','ConventionalConforming_Loans','NonConventionalConforming_Loans')],id.vars = 1)

# Bar plot each type of loans grouped by year
options(scipen=10000)
ggplot(totLoans, aes(x=State,y=value)) + 
  geom_bar(stat='identity', aes(fill=variable), position='dodge')

######################################################################################

# Pie chart of total loans grouped by State (no segregation of loans)
total_loansSQL <- "SELECT State, COUNT(*) AS Tot_loans FROM mergedExpandedData GROUP BY State"
total_loans <- sqldf(total_loansSQL)
Loan_Count <- as.vector(total_loans['Tot_loans'])
State <- as.vector(total_loans['State'])

pct <- round(Loan_Count/sum(Loan_Count)*100)
pct <- as.integer(unlist(pct))
State <- unlist(State)
lbls <- paste(State, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

pie(pct,labels = lbls, col=c("purple", "violetred1", "green3",
                                                 "cornsilk", "cyan", "white"))


##################################################################################################

mergedExpandedData <- hmda_init()

# Create a function cleaningData() for Quality Assssment of Applicant_Income_000
cleaningData <- function(mergedExpandedData) {
  
  mergedExpandedData$Applicant_Income_000[mergedExpandedData$Applicant_Income_000=='NA  '] = '0'
  mergedExpandedData$Applicant_Income_000=as.numeric(as.character(mergedExpandedData$Applicant_Income_000))
  class(mergedExpandedData$Applicant_Income_000)
  
  # Cleaning the data
  meanIncome <- mean(mergedExpandedData$Applicant_Income_000)
  mergedExpandedData$Applicant_Income_000[mergedExpandedData$Applicant_Income_000==0] = meanIncome
  
  return(mergedExpandedData)
}

clean_data <- cleaningData(mergedExpandedData)


##################################################################################################


# Create a function cleaning_LoanAmount() for Quality Assssment of Loan_Amount_000
cleaning_LoanAmount <- function(clean_data) {
  
  clean_data$Loan_Amount_000=as.numeric(as.character(clean_data$Loan_Amount_000))
  class(clean_data$Loan_Amount_000)
  
  # clean_data$Loan_Amount_000[ which(clean_data$Loan_Amount_000 < 999)]
  clean_data$Loan_Amount_000[clean_data$Loan_Amount_000>999 & clean_data$Loan_Amount_000<99999] =
    clean_data$Loan_Amount_000/1000
  
  return(clean_data)
}
# Call cleaning_LoanAmount(data) function for Quality assessment of Loan_Amount_000
clean_data <- cleaning_LoanAmount(clean_data)

cleanedData <- clean_data$Loan_Amount_000[ which(clean_data$Loan_Amount_000 > 999)]
length(clean_data$Loan_Amount_000[ which(clean_data$Loan_Amount_000 > 999)])
clean_data$Loan_Amount_000[clean_data$Loan_Amount_000 < 999]

##################################################################################################

library(plyr)

# Mean applicant income grouped by state and county
mean_Data <- aggregate(clean_data$Applicant_Income_000,by=list(x=clean_data$State,y=clean_data$County_Name),mean,na.rm=TRUE)
class(mainData)
mean(clean_data$Applicant_Income_000[clean_data$County_Name=='RUSSELL' & clean_data$State=='VA'])

##################################################################################################

# Mean applicant income grouped by state
mean_income_state <- aggregate(clean_data$Applicant_Income_000,by=list(State=clean_data$State),mean,na.rm=TRUE)

ggplot(mean_income_state, aes(x=reorder(State,-x),y=x)) + 
  geom_bar(stat='identity', aes(fill=x)) +
  labs(x="State", y="Avg. applicant income (000's)")


##################################################################################################

# Create SQL queries for analyzing data for Maryland
Maryland_dataSQL <- "SELECT County_Name,count(*) AS no_of_loans,Conforming_Limit_000  FROM clean_data WHERE State LIKE 'MD' GROUP BY County_Name"
Maryland_data <- sqldf(Maryland_dataSQL)

Maryland_data <- Maryland_data[order(-Maryland_data$no_of_loans),] 


##################################################################################################

# Create SQL queries for analyzing data for Virginia
Virginia_dataSQL <- "SELECT County_Name,count(*) AS no_of_loans,Conforming_Limit_000  FROM clean_data WHERE State LIKE 'VA' GROUP BY County_Name"
Virginia_data <- sqldf(Virginia_dataSQL)

Virginia_data <- Virginia_data[order(-Virginia_data$no_of_loans),] 


##################################################################################################

DC_dataSQL <- "SELECT County_Name,count(*) AS no_of_loans,Conforming_Limit_000  FROM clean_data WHERE State LIKE 'DC' GROUP BY County_Name"
DC_data <- sqldf(DC_dataSQL)

DC_data <- DC_data[order(-DC_data$no_of_loans),]


##################################################################################################


# Average loan amount grouped by state
mean_loanAmt_state <- aggregate(clean_data$Loan_Amount_000,by=list(State=clean_data$State),mean,na.rm=TRUE)
mean_loanAmt_state <- mean_loanAmt_state[order(-mean_loanAmt_state$x),]

ggplot(mean_loanAmt_state, aes(x=reorder(State,-x),y=x)) + 
  geom_bar(stat='identity', aes(fill=x)) +
  labs(x="State", y="Avg. loan amount (000's)")

