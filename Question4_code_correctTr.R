#getting the input
input <- read.csv("nuthead.csv", header = TRUE, sep = ",", row.names = NULL)
str(input)
# Part - a: G-test for 2x2 table with expected values >5
  #Specific factors
  input$monkeys <-as.factor(input$monkeys)
  input$nutcount <-as.factor(input$nutcount)
  #bar-graph between monkey and hit
  count.hit <- table(input$monkey,input$hit)
  colnames(count.hit)<-c("no", "monkey") 
  rownames(count.hit)<-c("no.hit", "hit")
  
  #Test with Chisq
  chisq.test(count.hit)
  fisher.test(count.hit)
  
  #Test with G-test
  library(DescTools) 
  GTest(count.hit)
  barplot(count.hit, xlab="Monkeys", beside=TRUE, legend=rownames(count.hit))
  
  #Expected count for G-test
  counts.exp <- matrix(NA,2,2) 
  counts.exp [1,1]<-(((sum(count.hit[1,]))/sum(count.hit))*((sum(count.hit[,1]))/sum(count.hit))*(sum(count.hit))) 
  counts.exp [1,2]<-(((sum(count.hit[1,]))/sum(count.hit))*((sum(count.hit[,2]))/sum(count.hit))*(sum(count.hit))) 
  counts.exp [2,1]<-(((sum(count.hit[2,]))/sum(count.hit))*((sum(count.hit[,1]))/sum(count.hit))*(sum(count.hit))) 
  counts.exp [2,2]<-(((sum(count.hit[2,]))/sum(count.hit))*((sum(count.hit[,2]))/sum(count.hit))*(sum(count.hit)))
  counts.exp  

# Part c - explain the occurrences of hit with coconut number
  #Model selection
  library(MuMIn)
  model.full = glm(hit~monkeys*nutcount, data = input, family = binomial)
  options(na.action = "na.fail") 
  summary(model.full)
  dredge(model.full, rank = AIC)
  
  #The best model
  models.best = glm (hit~monkeys + nutcount, data = input, family = binomial)
  summary(models.best)
  anova(models.best, test="Chisq")
  
  library(car)
  Anova(models.best, type=3)
  
  #Extract predicted values
  input$pred <- predict(models.best, input, type = "response")
  #Inspect coefficients
  coefs <- coefficients(models.best)
  coefs
  
#part d -Visualize by bar-graph
  means<-aggregate(input$pred, list(input$nutcount), mean)
  sds<-aggregate(input$pred, list(input$nutcount), sd)
  bp<-barplot(means$x, names.arg=c("5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"), xlab="nut counts", ylab = "probability of hit", ylim=c(0,1))
  arrows(bp, means$x-sds$x, bp, means$x+sds$x, code=3, angle=90)
