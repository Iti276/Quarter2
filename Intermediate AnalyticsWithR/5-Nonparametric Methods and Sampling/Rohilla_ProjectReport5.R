#Section13-2 Q6
#HO:MEDIAN = 3000
#H1:MEDIAN != 3000
median <- 3000

alpha <-0.05

games <- c(6210, 3150, 2700, 3012, 4875,
          3540, 6127, 2581, 2642, 2573,
          2792, 2800, 2500, 3700, 6030,
          5437, 2758, 3490, 2851, 2720)
Difference <- games - median

neg <-length(Difference[Difference < 0])

pos <-length(Difference[Difference > 0]) 

result <- binom.test(x=c(pos,neg),alternative = "two.sided")
result$p.value

# Create a dataframe
result_table <- data.frame(
  "Statistic" = c("Number of successes", "Number of trials", "P-value", "Alternative hypothesis", "95% Confidence Interval (Lower)", "95% Confidence Interval (Upper)", "Sample estimate"),
  "Value" = c(10, 20, 1, "true probability of success is not equal to 0.5", 0.2719578, 0.7280422, 0.5)
)
#write.csv(result_table,file = "result_table.csv",row.names = FALSE)

ifelse(result$p.value> alpha,"Accept the Null","Reject the Null")

#not enough evidence to reject the null hypothesis
#Q10
#H0: Median is less than 200 tickets
#H1: Median is grater than 200 Tickets
median <-200
pos2 <- 25
neg2 <- 15
alpha2 <- 0.05
result2<- binom.test(x =c(pos2,neg2),alternative = "less") 

ifelse(result2$p.value>alpha2,"Accept the Null","Reject the Null")
# Create a dataframe
result_table2 <- data.frame(
  "Statistic" = c("Number of successes", "Number of trials", "P-value", "Alternative hypothesis", "95% Confidence Interval (Lower)", "95% Confidence Interval (Upper)", "Sample estimate"),
  "Value" = c(25, 40, "less than 0.5", "true probability of success is less than  0.5", 0.0000000, 0.7527053, 0.625)
)
#write.csv(result_table2,file = "result_table2.csv",row.names = FALSE)
#not enough evidence to fail the null hypothesis
#Section 13-3Q4
#H0- There is no difference in the sentence received by each gender.
#H1: There is a difference in the sentence received by each gender.
males <- c(8, 12, 6, 14, 22, 27, 3, 2, 2, 2, 4, 6, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 3, 9, 4, 0, 17, 23, 12, 11, 16)
alpha4 <- 0.05
result4 <- wilcox.test(x =males,y=females,alternative='two.sided',correct=FALSE)
ifelse(result4$p.value>alpha4,"Accept the Null","Reject the Null")
# Create a dataframe
result_table4 <- data.frame(
  "Statistic" = c("Data", "W", "P-value", "Alternative hypothesis"),
  "Value" = c("males and females", 110.5, "0.9338", "true location shift is not equal to 0")
)
write.csv(result_table4,file = "result_table4.csv",row.names = FALSE)
# There is no enough evidence to reject the Null hypothesis , so there is a difference
#in the sentence received by each gender
#H0:There is no difference in the number of wins 
#H1:There is a  difference in the number of wins 
NL <- c( 89,96,88,101,90,91,92,96,108,100,95)
AL <-c(108,86,91,97,100,102,95,104,95,89,88,101)
alpha5<- 0.05
result5<- wilcox.test(x=NL,y=AL,alternative = 'two.sided',correct = FALSE)
alpha5 <- 0.05
ifelse(result5$p.value > alpha5,"Accept the NULL","Reject the NULL")
# Create a dataframe
result_table5 <- data.frame(
  "Statistic" = c("Data", "W", "P-value", "Alternative hypothesis"),
  "Value" = c("NL and AL", 59, 0.6657, "true location shift is not equal to 0")
)
#write.csv(result_table5,file = "result_table5.csv",row.names = FALSE)

# There is no enough evidence to reject the Null hypothesis , so there is a difference
#in the sentence received by each gender
#Section13-5
#H0=There is no difference in the means of means
#H1: H1= There is a difference of means
alpha1352<-0.05
Western_Hemisphere <- data.frame(OECD = c(527,406,474,381,411),group = rep("Western_Hemisphere",5))
Europe <- data.frame(OECD = c(520,510,513,548,496),group = rep("Europe",5))
Eastern_Asia <- data.frame(OECD = c(523,547,547,391,549),group = rep("Eastern_Asia",5))
data <-rbind(Western_Hemisphere,Europe,Eastern_Asia)
result1352 <- kruskal.test(OECD ~group, data =data)
ifelse(result1352$p.value>alpha1352,"Accept the NULL Hypothesis","Reject the NULL Hypothesis")
# Create a dataframe
result_table1352 <- data.frame(
  "Statistic" = c("Data", "Kruskal-Wallis chi-squared", "P-value", "Degree Of Freedom"),
  "Value" = c("OECD by group", 4.1674,0.1245,2)
)
#write.csv(result_table1352,file = "result_table1352.csv",row.names = FALSE)
#Q13-6

Subway <- c(845,494,425,313,108,41)
Rail <- c(39,291,142,103,33,38)
City <- c(1,2,3,4,5,6)
alpha136 <- 0.05
data136 <-data.frame(city= City,rail=Rail,subway=Subway)
result136 <-cor.test(x=data136$rail,y=data136$subway,method = 'spearman')
result136$p.value
result136$estimate
ifelse(result136$p.value>alpha136,"Accept the NULL Hypothesis","Reject the NULL Hypothesis")
# Create a dataframe
result_table136 <- data.frame(
  "Statistic" = c("Data", "S", "P-value", "alternative hypothesis","sample estimates"),
  "Value" = c("Rail and Subway",14,0.2417,"true rho is not equal to 0","rho=0.6")
)
write.csv(result_table136,file = "result_table136.csv",row.names = FALSE)
#########################################################################
# Function to simulate buying boxes until all four prizes are obtained
simulate_caramel_corn <- function() {
  prizes <- c(1, 2, 3, 4)  # Four different prizes
  boxes <- numeric(4)  # Array to keep track of obtained prizes
  num_boxes <- 0  # Counter for the number of boxes bought
  
  # Repeat until all four prizes are obtained
  while (length(unique(boxes)) < 4) {
    num_boxes <- num_boxes + 1
    box <- sample(prizes, 1)  # Randomly select a prize from the available ones
    boxes[box] <- box  # Mark the obtained prize in the array
  }
  
  return(num_boxes)
}

# Number of times to repeat the experiment
num_experiments <- 40

# Perform the experiments and store the results
results <- replicate(num_experiments, simulate_caramel_corn())

# Calculate the average number of boxes needed
average_boxes <- mean(results)

# Print the result
cat("Average number of boxes needed to get all four prizes:", average_boxes, "\n")
#################################################################################
# Function to simulate buying tickets until the word "big" is spelled correctly
simulate_lotto <- function() {
  letters <- c("b", "i", "g")  # Letters needed to spell "big"
  ticket_count <- 0  # Counter for the number of tickets bought
  found_letters <- c(FALSE, FALSE, FALSE)  # Array to keep track of found letters
  
  # Repeat until all letters are found
  while (!all(found_letters)) {
    ticket_count <- ticket_count + 1
    letter <- sample(letters, 1, prob = c(0.60, 0.30, 0.10), replace = TRUE)  # Randomly select a letter with probabilities
    
    # Update found_letters array if the selected letter is needed
    if (letter == "b") {
      found_letters[1] <- TRUE
    } else if (letter == "i") {
      found_letters[2] <- TRUE
    } else if (letter == "g") {
      found_letters[3] <- TRUE
    }
  }
  
  return(ticket_count)
}

# Number of times to repeat the experiment
num_experiments <- 30

# Perform the experiments and store the results
results <- replicate(num_experiments, simulate_lotto())

# Calculate the average number of tickets needed
average_tickets <- mean(results)

# Print the result
cat("Average number of tickets needed to win the prize:", average_tickets, "\n")

