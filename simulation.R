# Number of iterations you need to make
iter <- 1000;
############
#DATA INPUT GOES HERE
InterArrivalInput <- list(InterArrival = c(1,2,3,4),
                          Probability = c(0.25,0.40,0.20,0.15))
AbleInput <- list(InterArrival = c(1,2,3,4),
                  Probability = c(0.30, 0.28, 0.25, 0.17))
BakerInput <- list(InterArrival = c(1,2,3,4),
                   Probability = c(0.35, 0.25, 0.20, 0.20))

#Probability tables
countDF <- function(input) {
  #A function that takes input list and calculate cumulative
  #probability and Random number assignment
  #Then returns a data frame
  
  if(sum(input$Probability) != 1){
    print("Probability vector must be equal to 1")
  }
  Cumulative <- cumsum(input$Probability)
  RandomNumber <- Cumulative * 100
  
  df <- data.frame(InterArrival = input$InterArrival,
                   Probability = input$Probability,
                   Cumulative = Cumulative,
                   RandomNumber = RandomNumber)
}
#calculate probability tables into data frames then view them.
small.interarrival <- countDF(InterArrivalInput)
small.able <- countDF(AbleInput)
small.baker <- countDF(BakerInput)

View(small.interarrival); View(small.able); View(small.baker)

#Complete vectors
#Tanks to are for prob method :-)
InterArrivalTime <- sample(InterArrivalInput$InterArrival, size=iter ,replace=T,
                           prob=InterArrivalInput$Probability)
hist(InterArrivalTime)
AbleServiceTime <-  sample(AbleInput$InterArrival, size=iter ,replace=T,
                           prob=AbleInput$Probability)
hist(AbleServiceTime)
BakerServiceTime <- sample(BakerInput$InterArrival, size=iter ,replace=T,
                           prob=BakerInput$Probability)
hist(BakerServiceTime)
ArrivalTime <- cumsum(InterArrivalTime)
#Simulation vectors and initializations
when.able <- c(0)
when.baker <- c(0,0)
server.choosen <- c('Able')
start <- c(0)
end.able <- c(0)
end.baker <- c(0)
delay <- c(0)
total.time <- c(0)
########################
for(i in 1:iter){
  when.able[i] <-  (max(end.able[c(1:i)], na.rm=T));
  when.baker[i] <- (max(end.baker[c(1:i)], na.rm=T));
  
  ifelse((when.able[i] <= ArrivalTime[i]) || (ArrivalTime[i] <= when.baker[i]),
         server.choosen[i] <- 'Able',
         server.choosen[i] <- 'Baker');
  
  ifelse(server.choosen[i] == 'Able',
         start[i] <- (max(when.able[i], ArrivalTime[i])),
         start[i] <- (max(when.baker[i], ArrivalTime[i])));
  
  ifelse(server.choosen[i] == 'Able',
         end.able[i] <- (start[i] + AbleServiceTime[i]),
         end.able[i] <- 0);
  
  ifelse(server.choosen[i] == 'Baker',
         end.baker[i] <- (start[i] + BakerServiceTime[i]),
         end.baker[i] <- 0);
  
  delay[i] <- (start[i] - ArrivalTime[i]);
  total.time[i] <- (max(end.able[i], end.baker[i]) - ArrivalTime[i])
  
}

BigTable <- data.frame(InterArrivalTime = InterArrivalTime,
                 ArrivalTime = ArrivalTime,
                 WhenAbleAvailable = when.able,
                 AbleServiceTime = AbleServiceTime,
                 BakerServiceTime = BakerServiceTime,
                 whenBakerAvailable = when.baker,
                 ServerChoosen = server.choosen,
                 Start = start,
                 EndAble = end.able,
                 EndBaker = end.baker,
                 Delay = delay,
                 TotalTime = total.time)
View(BigTable)

#SYSTEM ANALYSIS Goes here if you need

hist(BigTable$Delay)

avg.time.in.system <- sum(BigTable$TotalTime)/iter
View(avg.time.in.system)