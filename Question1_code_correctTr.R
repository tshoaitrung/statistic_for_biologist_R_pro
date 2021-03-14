##get the input
input <- read.csv("nutweight.csv", header = TRUE, sep = ",", row.names = NULL)

#show the input information
str(input)

# part a - Calculate means of nut weight of each tree
  # A null hypothesis (H0) is that no difference of nuts weight among trees
  library(psych)
  des.mat <- describeBy(input$weight, input$tree,mat=TRUE,digits=2) #save the result in a matrix
  des.mat

# part b - Plot a graph of nuts weight per tree
  x <- as.numeric(des.mat$group1)
  y <- des.mat$mean
  y.se <- des.mat$sd/(sqrt(length(na.omit(input$weight))))
  library(Hmisc)
  plot(x,y, ylim=c(1000, 2000),xlab="Tree", ylab="Nut's weight (gram)", main="Nuts weight per tree")
  errbar(x,y,y+y.se,y-y.se,add = TRUE)

# part c - Test the assumption of part a by using one-way Anova
  ## Assumption for one-way ANOVA test: Normality and independence of sample, Variance equality and continuous depedent variable  
  
  ## Shapiro-Wilks test 
    shapiro.test(input$weight)
  
  ## plot Q-Q plot
  qqnorm(input$weight) 
  qqline(input$weight)
  
  ## Levene test for variance equality
  library(car)
  leveneTest(input$weight,input$tree)
  
  # One-way ANOVA test
  ## H0: No difference of nuts weight among tree; H1: True difference
  ## Reject H0 if p-value < alpha level 0.05
  input$tree <-as.factor(input$tree)
  nut.aov <-aov(input$weight~input$tree)
  summary(nut.aov)

# part d - Find the particular tree with Pairewise t-test, BH: Benjamini-Hochberg method
    pairwise.t.test(input$weight, input$tree, p.adjust.method = "BH")
    
