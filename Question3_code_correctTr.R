#getting the input
input <- read.csv("nutcrack.csv", header = TRUE, sep = ",", row.names = NULL)
str(input)
input$cracked <-as.factor(input$cracked)
input$hard <-as.factor(input$hard)
input$fungus <-as.factor(input$fungus)

#part- a: Test that the nuts fall on hard ground cracks more easily (ignore fungus)
  counts <- table(input$cracked,input$hard)
  colnames(counts)<-c("soft", "hard") 
  rownames(counts)<-c("no.carcked", "cracked")
  chisq.test(counts)
  fisher.test(counts)
  library(DescTools) 
  GTest(counts)
  barplot(counts, xlab="Ground", beside=TRUE, legend=rownames(counts))
  
  counts.exp <- matrix(NA,2,2) 
  counts.exp [1,1]<-(((sum(counts[1,]))/sum(counts))*((sum(counts[,1]))/sum(counts))*(sum(counts))) 
  counts.exp [1,2]<-(((sum(counts[1,]))/sum(counts))*((sum(counts[,2]))/sum(counts))*(sum(counts))) 
  counts.exp [2,1]<-(((sum(counts[2,]))/sum(counts))*((sum(counts[,1]))/sum(counts))*(sum(counts))) 
  counts.exp [2,2]<-(((sum(counts[2,]))/sum(counts))*((sum(counts[,2]))/sum(counts))*(sum(counts))) 
  counts.exp
  
# part - b: GLM test 
  # create a count table for three variables
  y = xtabs(~ cracked + hard + fungus, input) 
  # change data format to cross-tabulated format
  count3 = as.data.frame(y) 
  
  ##creat a full model
    library(MuMIn)
    ##Full model
      models.full = glm (Freq ~ cracked*hard*fungus, data = count3, family = poisson)
      options(na.action = "na.fail") 
      dredge(models.full, rank=AIC)
    ##Select the best model which include all interactions
      models.best1 = glm (Freq ~ hard + fungus + cracked + cracked*hard + cracked*fungus+ hard*fungus, data = count3, family = poisson)
      summary(models.best1)
      ###anova test with Chisq
      anova(models.best1, test="Chisq")
      ###anova test with type 3
      library(car)
      Anova(models.best1, type=3)

    ##Select the best model excluded hard*fungus interaction 
      models.best2 = glm (Freq ~ hard + fungus + cracked + cracked*hard + cracked*fungus, data = count3, family = poisson)
      summary(models.best2)
      ###anova test with Chisq
      anova(models.best2, test="Chisq")
      ###anova test with type 3
      library(car)
      Anova(models.best2, type=3)
      
    ##Select the best model excluded cracked*fungus interaction
      models.best2 = glm (Freq ~ hard + fungus + cracked + cracked*hard + hard*fungus, data = count3, family = poisson)
      summary(models.best2)
      ###anova test with Chisq
      anova(models.best2, test="Chisq")
      ###anova test with type 3
      library(car)
      Anova(models.best2, type=3)