
##ASSIGNMENT-02 -- NUMERICAL COMPUTING------------------------------
##--------------------------------------------------------------------
  # Student Name:- Joshua Ayyasamy
  # Date :- November 12, 2021
  # Instruction for running the assignment: -enter the date in format dd-mm-yyyy
bestFitFun <- function(){
  optionEntered <- as.numeric(readline(prompt = cat("MENU\n1.Exponential Fit\n2.Quit")))
  while(optionEntered == "1" || optionEntered == "2"){
    if(optionEntered == "1"){
      fileName <- readline(prompt= "Please enter the file name to open:- ")
      library(readxl)
      COVID19_data <- data.frame(read_excel(fileName, sheet="Sheet1", 
                                            col_types=c("numeric", "date","numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "numeric", "numeric",
                                                        "numeric"), na="**"))
      print(summary(COVID19_data))
      
      startDateNumber = 0
      
      startDate <- (as.Date(readline(prompt="Please enter the start date (dd-mm-yyyy) :- "), format = "%d-%m-%Y"))
      startDateInteger <- as.numeric(startDate)
      for(i in 1:282){
        if(as.Date(COVID19_data$date[i], format="%d-%m-%Y") == startDate){
          startDateNumber <- COVID19_data$Number[i]
        }
      }
      
      while(startDateNumber < 32){
        startDate <- (as.Date(readline(prompt="Please enter the start date (dd-mm-yyyy) :- "), format = "%d-%m-%Y"))
        for(i in 1:282){
          if(as.Date(COVID19_data$date[i], format="%d-%m-%Y") == startDate){
            startDateNumber <- COVID19_data$Number[i]
          }
        }
      }
      endDate <- (as.Date(readline(prompt="Please enter the end date (dd-mm-yyyy) :- "), format = "%d-%m-%Y"))
      for(i in 1:282){
        if(as.Date(COVID19_data$date[i], format="%d-%m-%Y") == endDate){
          endDateNumber <- COVID19_data$Number[i]
        }
      }
      while(endDateNumber < (startDateNumber+60)){
        endDate <- (as.Date(readline(prompt="Please enter the end date (dd-mm-yyyy) :- "), format = "%d-%m-%Y"))
        for(i in 1:282){
          if(as.Date(COVID19_data$date[i], format="%d-%m-%Y") == endDate){
            endDateNumber <- COVID19_data$Number[i]
          }
        }
      }
      
      numberOfDays <- endDateNumber - startDateNumber + 1
      #function is of the form TV = a * e^bt where a and b are constants t is the day 
      #and TV is the total-vaccinations
      # so:- 
      #log TV = log a + bt(ln e)
      #log TV = log a + bt
      #which is of the form Z = a0 + a1.t
      #a0 = ln a
      #a1 = b
      #Z = ln TV
      #
      #a1 = (nsum(z*t) - sum(z)sum(t))/(nsum(t^2) - (sum(t))^2)
      date <- c()
      totalVaccinations <- c()
      
      #Storing the dates from the start_date to the end_date in a vector
      #date[1] = startDate
      #for(i in 2:numberOfDays){
      #  date[i] = date[i-1] + 1
      #  date[numberOfDays] = endDate
      #}
      
      #Storing the values of day
      day <- c()
      
      i=0
      while(startDateNumber <= endDateNumber){
        i=i+1
        day[i] <- startDateNumber
        totalVaccinations[i]<- COVID19_data$total_vaccinations[startDateNumber]
        startDateNumber = startDateNumber +1
        
      }
      
      
      #calculating the sum of z
      #totalVaccinations <- c(COVID19_data$total_vaccinations)
      total_z = 0
      x=0
      for(i in 1:numberOfDays){
        x <- log(totalVaccinations[i])
        total_z <- total_z + x
      }
      
      
      #calculating the sum of t
      #date<-c(as.integer(as.Date(COVID19_data$date, format="%d-%m-%Y")))
      total_t = 0
      for(i in 1:numberOfDays){
        total_t <- total_t + day[i]
      }
      
      square_of_total_date <- (total_t^2)
      
      
      
      # calculating the sum of square of t
      total_of_square_date = 0
      for(i in day){
        i<- i^2
        total_of_square_date <- total_of_square_date + i
      }
      
      #calculating the sum of product of z and t
      total_product_z_t = 0
      #total_product_z_t01 = total_z * total_t
      for(i in 1:length(totalVaccinations)){
        x= (log(totalVaccinations[i]) * day[i])
        total_product_z_t <- total_product_z_t + x
      }
      total_product_z_t <- numberOfDays * total_product_z_t
      final_total = total_z * total_t
      #calculation of a0
      a1= ((total_product_z_t) - final_total)/((numberOfDays*total_of_square_date)- square_of_total_date)
      
      mean_z = (total_z/length(totalVaccinations))
      mean_t = (total_t/length(day))
      a0 =   (mean_z - (a1 * mean_t))
      
      
      #a0 = ln a
      a = exp(a0)
      
      #a1 = b
      print(paste("The value of a1 is ", a1 , " and value of a0 is ", a0, sep = " "))
      
      fittedTotalVaccinations <- c()
      for(b in 1:length(day)){
        fittedTotalVaccinations[b] <- (a * exp(a1 * day[b]))
      }
      #print(fittedTotalVaccinations)
     
      library(ggplot2)
      theme_set(theme_minimal())
      mainDataFrame <- data.frame(
        day = day, totalVaccinations = totalVaccinations, FittedVaccinations = fittedTotalVaccinations
      )
      #print(mainDataFrame)
      head(mainDataFrame)
       
      #g <- ggplot(data = mainDataFrame, aes(x = day, y = totalVaccinations), color="red") + geom_line()
      #print(g)
      
      g <- ggplot(mainDataFrame, aes(x=day)) + 
        geom_line(aes(y = totalVaccinations), color = "darkred") + 
        geom_line(aes(y = FittedVaccinations), color="steelblue") +
        scale_color_manual(values = c("darkred", "steelblue"))
      print(g)
      #ggplot(data = COVID19_data, aes(x = day, y = totalVaccinations))
      optionEntered02 <- as.numeric(readline(prompt = cat("MENU\n1.Extrapolation\n2.Main Menu")))
      while(optionEntered02 == "1" || optionEntered02 == "2"){
        if(optionEntered02 == "1"){
          date<- (as.Date(readline(prompt="Please enter the date to extrapolate to (dd-mm-yyyy) :- "), format = "%d-%m-%Y"))
          baseDate <- as.Date("2021-01-01", format="%Y-%m-%d")
          Date1 <- as.integer(baseDate)
          Date2 <- as.integer(date)
          valueOf_t <- Date2 - Date1 + 1
          
          print(paste("Applying the function:- TV = ", a, "e^(", a1, " * t)"))
          numberVaccinations = a * exp(a1 * valueOf_t)
          print(paste("The number of vaccinations comes out to be:- ", numberVaccinations))
          break
        }
        if(optionEntered02 == "2"){
          bestFitFun()
          break
        }
      }
      break
    }
    else if(optionEntered == "2"){
      print("Goodbye!")
      break
    }
    else{
      print("Wrong!!!")
      break
    }
  }
 
  #return(optionEntered)
}




