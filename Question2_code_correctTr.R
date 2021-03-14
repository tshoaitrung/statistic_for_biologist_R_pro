#get the input
input <- read.csv("nutfungus.csv", header = TRUE, sep = "," , row.names = NULL)
input$fungus <-as.factor (input$fungus)
str(input)

# part a - test the correlation bwtween "nut weight" and "tree height" while ignoring "fungus" variable
  ## Assumpation tests

  ## Histogram of each variable 
    hist(input$height, breaks = 10)
    hist(input$weight, breaks = 10)
  ##Shapiro-Wilks test 
    shapiro.test(input$height)
    shapiro.test(input$weight)
  ## plot Q-Q plot
    qqnorm(input$height) 
    qqline(input$height)
    
    qqnorm(input$weight) 
    qqline(input$weight)
  
  ## Scatter plot bwtween "tree height" and "nut weight"
    plot(input$height,input$weight,xlab = "Height of tree", ylab = "weight of nut",col = "red", ylim = c(0, 2500))
  ##Hypothesis test
  ##Pearson correlation with significant test
    remove.fus = data.frame(input$height,input$weight)
    colnames(remove.fus) <- c("height", "weight")
    library(Hmisc)
    rcorr(as.matrix(remove.fus), type="pearson")
    cor.test(input$height,input$weight)
    
# part b - approach 1: Pearson correlation in a matrix         
    # Possible interaction (Note! Remove line 3: input$fungus <-as.factor (input$fungus) before test)
    input$HaF <- input$height*input$fungus
    # Pearson correlation in a matrix
    library(Hmisc)
    rcorr(as.matrix(input[1:4], type="pearson"))  

#part b -approach 2: Pearson correlation in 2 separated tests.
    fungus <- subset (input, input$fungus == 1)
    nofungus <- subset (input, input$fungus == 0)
    
    # We have two groups of with and withoug fungus 
    ## For no-fungus group
    ### QQ plot
      qqnorm(nofungus$height) 
      qqline(nofungus$height)
    
      qqnorm(nofungus$weight) 
      qqline(nofungus$weight)  
    ### Shapiro test
      shapiro.test(nofungus$height)
      shapiro.test(nofungus$weight)
      remove.fus = data.frame(nofungus$height,nofungus$weight)
      
    ### Pearson correlation test  
      library(Hmisc)
      rcorr(as.matrix(remove.fus), type="pearson")
      cor.test(nofungus$height,nofungus$weight)
      
    ## For the fungus group 
      ## QQ plot
      qqnorm(fungus$height) 
      qqline(fungus$height)
      
      qqnorm(fungus$weight) 
      qqline(fungus$weight)
      ## Shapiro test
      shapiro.test(fungus$height)
      shapiro.test(fungus$weight)
      remove.fus = data.frame(fungus$height,fungus$weight)
      
      ## Pearson correlation test
      library(Hmisc)
      rcorr(as.matrix(remove.fus), type="pearson")
      cor.test(fungus$height,fungus$weight)

# part c - plotting
    fug.fit <- lm(fungus$weight~fungus$height)
    nofug.fit <- lm(nofungus$weight~nofungus$height)
    plot(fungus$height,fungus$weight,xlab = "Height of tree", ylab = "weight of nut",col = "red", ylim = c(0, 2500))
    points(nofungus$height,nofungus$weight, col = "blue")
    abline(fug.fit, col = "red") 
    abline(nofug.fit, col = "blue")
    
# part d - test assumption in part b
    # Assumption 1: equality of slopes-interaction is not significant
    full.model <- aov(weight ~ fungus+height + fungus*height,data = input)
    summary(full.model)
    
    # Assumption 2: Linearity of slopes - show in par c
    # Assumption 3: Equality of two groups on the covariate (only 2 groups- use t-test)
    t.test(height ~ fungus, data = input)
    
    # Assumption 4: Homogeneity of variance
    library(car)
    leveneTest(weight ~ fungus, center = mean, data = input)
    
# Calculate residuals of best.model1 (after determine the best model in part e) 
    input$resid <- resid(best.model1)
    input$resid
    
    #Make histogram
    hist(input$resid, breaks = 10)
    # Shapiro test
    shapiro.test(input$resid)
    # QQ plot
    qqnorm (input$resid)
    qqline(input$resid)
    # Scatter plot of residuals
    plot(input$height, input$resid)
    
# part e - write a model - ANCOVA
    full.model <- lm(weight ~ fungus+height + fungus*height,data = input)
    summary(full.model)
    
    # Model selection
    library(MuMIn)
    options(na.action = "na.fail")
    output <- dredge (full.model, rank = AIC)
    output
    
    best.model1 <-lm(weight~fungus+height, data = input)
    summary(best.model1)
    
    best.model2 <-lm(weight~fungus+fungus*height, data = input)
    summary(best.model2)
    
    # Check for collinearity
    model.full <- model.matrix(~fungus+height + fungus*height, data = input)
    kappa(model.full)
    
    model.best1 <- model.matrix(~fungus+height, data = input)
    kappa(model.best1)
    
    coef(best.model1)
    
    
    
    ####not checked
    model.best2 <- model.matrix(~fungus+fungus:height, data = input)
    kappa(model.best2)
    
    

    