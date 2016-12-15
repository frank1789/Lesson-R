#---------------------------------------------------Lesson9
    #Manufacturing plant for ICs
    # A = aperture setting
    # B = Exposure time
    # C = Develop time
    # D = Mask dimension
    # E = Etch time
    # Response: Yield(ICs/h)
    #
    # 2^(5-1) FFP -> I = ABCDE => E = ABCD
    rm(list = ls())
    lvl <- c(-1, 1)
    df <- expand.grid(
      A = lvl,
      B = lvl,
      C = lvl,
      D = lvl
    )
    attach(df) 				#all the column become regular variable
    df$E <- A * B * C * D
    detach(df)
    #specify value factor
    df$A <- as.factor(df$A)
    df$B <- as.factor(df$B)
    df$C <- as.factor(df$C)
    df$D <- as.factor(df$D)
    df$E <- as.factor(df$E)
    #--------------------------------------
    #convert number into string
    levels(df$A)
    #--------------------------------------
    
    #...usual randomization, test execution, data collection
    df$Yield <- c(8, 9, 34, 52, 16, 22, 45, 60,
                  6, 10, 30, 50, 15, 21, 44, 63)
    #to verify sum(df$Yield) = 485
    
    df.lm <- lm(Yield ~ A * B * C * D * E, data = df)
    
    n <- length(df.lm$effects)
    effects <- as.vector(df.lm$effects [2:n])
    qn <- qqnorm(effects, datax = T, ylim = c(-70, 30))
    text(qn$x, qn$y, lab = names(df.lm$effects)[2:n], pos = 4)
    qqline(effects, datax = T)
    #
    df.lm <- lm(Yield ~ A * B * C + D, data = df)
    anova(df.lm) #D isn't significant
    #
    df.lm <- lm(Yield ~ A * B + C, data = df)
    anova(df.lm)
    #-------------------------------------------------------------------Lesson9-1
    #Quality control curves
    #
    library(qcc) # necessary library (to download)
    
    rm(list = ls())
    data("orangejuice")
    
    attach(orangejuice)
    q <- qcc(D[trial], type = "np", sizes = size[trial])
    #
    q
    q$violation
    q$violation$beyond
    #
    #remove point of violation
    tuning <- setdiff(which(trial), q$violation$beyond.limits)
    #
    q <- qcc(D[tuning], type = "np", sizes = size[tuning], newdata = [!trial], newsizes = size[!trial])
    #new set of data
    q <- qcc(D[sample > 33], type = "np", sizes = size[sample > 33])
    detach(orangejuice)
    