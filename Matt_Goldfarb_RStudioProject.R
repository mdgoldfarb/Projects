#Matt Goldfarb
#Description of the function
#This function uses if statements using the columns of the 2 sample data frames to give the appropriate test (t test, wilcoxon, corrected T/Z test, etc.) and the 9 cases have some extensions because if data1,2,3 or 4 only have 1 row, then there can be no test
#The last case uses the corrected test and uses the Cat function to group all the strings and variables together
#You make your 2 vectors and store them into a data frame to put into the data frame variable (the columns in the data frame HAVE to be the same length)
#The method HAS to be a string of an upper or lowercase 'z' or 't'
#alter must be a string of either 'less','greater',or 'two.sided'
#It starts off making all the necessary variables needed for this function
#I included the method for wanted the correct t test or z test, if you are testing for any other case, you can just type 't' or 'z' and it will not do anything
#The mu is your guess/predictions (difference in means or what the true mean is), the alpha is what you want your confidence level to be (in terms of 1-alpha)
Project <- function(dataframe, alpha, mu, alter, method)
{
  if (length(dataframe) == 0) {
    stop("No data")
  }
  data1 <-
    dataframe[is.na(dataframe[, 2]) == FALSE &
                is.na(dataframe[, 1]) == FALSE, ]
  
  data2 <-
    dataframe[is.na(dataframe[, 2]) == TRUE &
                is.na(dataframe[, 1]) == FALSE, ]
  
  data3 <-
    dataframe[is.na(dataframe[, 1]) == TRUE &
                is.na(dataframe[, 2]) == FALSE, ]
  
  data4 <-
    dataframe[is.na(dataframe[, 1]) == TRUE &
                is.na(dataframe[, 2]) == TRUE, ]
  
  Dbar <- mean(data1[, 1] - data1[, 2])
  
  Sd1 <- sd(data1[, 1] - data1[, 2])
  
  if (is.na(Sd1) == TRUE) {
    Sd1 = 0
  }
  
  Tbar <- mean(data2[, 1])
  
  ST <- sd(data2[, 1])
  
  if (is.na(ST) == TRUE) {
    ST = 0
  }
  
  Nbar <- mean(data3[, 2])
  
  SN <- sd(data3[, 2])
  
  if (is.na(SN) == TRUE) {
    SN = 0
  }
  
  Tbarz <- mean(c(data1[, 1], data2[, 1]))
  
  STz <- sd(c(data1[, 1], data2[, 1]))
  
  if (is.na(STz) == TRUE) {
    STz = 0
  }
  
  Nbarz <- mean(c(data1[, 2], data3[, 2]))
  
  SNz <- sd(c(data1[, 2], data3[, 2]))
  
  if (is.na(SNz) == TRUE) {
    SNz = 0
  }
  
  STN1 <- cov(data1[, 1], data1[, 2])
  
  if (is.na(STN1) == TRUE) {
    STN1 = 0
  }
  
  n1 <- length(data1[, 1])
  
  n2 <- length(data2[, 1])
  
  n3 <- length(data3[, 1])
  
  n4 <- length(data4[, 1])
  
  nh <- (2 / ((1 / n2) + (1 / n3)))
  
  sterrz <-
    sqrt(((STz ** 2) / (n1 + n2)) + ((SNz ** 2) / (n1 + n3)) - ((2 * n1 * STN1) /
                                                                  ((n1 + n2) * (n1 + n3))))
  
  znumer = Tbarz - Nbarz - mu
  
  
  if (n2 == 0 & n3 == 0 & n1 != 0)
  {
    if (n1 >= 2) {
      if (shapiro.test(dataframe[, 1])$p.value > alpha &
          shapiro.test(dataframe[, 2])$p.value > alpha) {
        t.test(
          dataframe[, 1],
          dataframe[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha),
          paired = TRUE
        )
      }
      else{
        wilcox.test(
          dataframe[, 1],
          dataframe[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha),
          paired = TRUE
        )
      }
    }
    else if (n1 == 1) {
      print('No test can be done')
    }
  }
  
  else if (n2 == 0 & n1 != 0 & n3 != 0)
  {
    if (n3 == 1 &
        n1 >= 2) {
      if (shapiro.test(data1[, 1])$p.value > alpha &
          shapiro.test(data1[, 2])$p.value > alpha) {
        t.test(
          data1[, 1],
          data1[, 2],
          mu = mu,
          conf.level = (1 - alpha),
          alternative = alter
        )
      } else{
        wilcox.test(
          data1[, 1],
          data1[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
    } else if (n3 >= 2 &
               n1 == 1) {
      if (shapiro.test(data3[, 2])$p.value > alpha) {
        t.test(
          data3[, 2],
          mu = mu,
          conf.level = (1 - alpha),
          alternative = alter
        )
      } else{
        wilcox.test(
          data3[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
    } else if (n1 == 1 & n3 == 1) {
      print('Not enough data')
    } else{
      t.test(
        data1[, 1],
        data3[, 2],
        mu = mu,
        conf.level = (1 - alpha),
        alternative = alter
      )
    }
  }
  
  else if (n3 == 0 & n1 != 0 & n2 != 0)
  {
    if (n2 == 1 &
        n1 >= 2) {
      if (shapiro.test(data1[, 1])$p.value > alpha &
          shapiro.test(data1[, 2])$p.value > alpha) {
        t.test(
          data1[, 1],
          data1[, 2],
          mu = mu,
          conf.level = (1 - alpha),
          alternative = alter
        )
      } else{
        wilcox.test(
          data1[, 1],
          data1[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
    } else if (n2 >= 2 &
               n1 == 1) {
      if (shapiro.test(data2[, 1])$p.value > alpha) {
        t.test(
          data2[, 1],
          mu = mu,
          conf.level = (1 - alpha),
          alternative = alter
        )
      }  else{
        wilcox.test(
          data2[, 1],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
    } else if (n1 == 0 & n2 == 0) {
      print('Not enough data')
    } else{
      t.test(
        data1[, 2],
        data2[, 1],
        mu = mu,
        conf.level = (1 - alpha),
        alternative = alter
      )
    }
  }
  
  else if (n1 == 0 & n3 == 0 & n2 != 0)
  {
    if (n2 == 1) {
      print('Not enough data')
    }
    else if (shapiro.test(data2[, 1])$p.value > alpha &
             n2 >= 2) {
      t.test(
        data2[, 1],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    } else{
      wilcox.test(
        data2[, 1],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    }
  }
  
  else if (n1 == 0 & n2 == 0 & n3 != 0)
  {
    if (n3 == 1) {
      print('Not enough data')
    }
    else
      if (shapiro.test(data3[, 2])$p.value > alpha &
          n3 >= 2) {
        t.test(
          data3[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      } else{
        wilcox.test(
          data3[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
  }
  
  else if (n1 == 0 & n2 != 0 & n3 != 0 & n2 >= 2 & n3 >= 2)
  {
    if (shapiro.test(data2[, 1])$p.value > alpha &
        shapiro.test(data3[, 2])$p.value > alpha) {
      if (var.test(data2[, 1], data3[, 2])$p.value > alpha) {
        t.test(
          data2[, 1],
          data3[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha),
          var.equal = TRUE
        )
      } else{
        t.test(
          data2[, 1],
          data3[, 2],
          mu = mu,
          alternative = alter,
          conf.level = (1 - alpha)
        )
      }
    } else{
      wilcox.test(
        data2[, 1],
        data3[, 2],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    }
  }
  
  else if (n1 == 0 & n2 != 0 & n3 != 0 & n2 >= 2 & n3 == 1)
  {
    if (shapiro.test(data2[, 1])$p.value > alpha) {
      t.test(
        data2[, 1],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    } else{
      wilcox.test(
        data2[, 1],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    }
  }
  
  else if (n1 == 0 & n2 != 0 & n3 != 0 & n2 == 1 & n3 >= 1)
  {
    if (shapiro.test(data3[, 2])$p.value > alpha) {
      t.test(
        data3[, 2],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    } else{
      wilcox.test(
        data3[, 2],
        mu = mu,
        alternative = alter,
        conf.level = (1 - alpha)
      )
    }
  }
  else if (n1 == 0 & n2 != 0 & n3 != 0 & n2 == 1 & n3 == 1) {
    print("Not enough data")
  }
  
  else if (n1 == 0 &
           n2 == 0 & n3 == 0 & n4 != 0) {
    print('All missing values')
  }
  
  else
  {
    t3 <-
      (n1 * Dbar + nh * (Tbar - Nbar)) / (sqrt(n1 * (Sd1) ** 2 + (nh ** 2) * (ST **
                                                                                2 / n2 + SN ** 2 / n3)))
    tnumer <- (n1 * Dbar + nh * (Tbar - Nbar)) - mu
    tsterr <- (sqrt(n1 * (Sd1) ** 2 + (nh ** 2) * (ST **
                                                     2 / n2 + SN ** 2 / n3)))
    zstat = (znumer / sterrz)
    if (alter == "less") {
      if ((method == 'Z') | (method == 'z')) {
        alterhyp <-
          cat(
            "Corrected Z-Test",
            "\nAlternative Hypothesis: True difference in means is less than",
            mu
          )
        upperb <-
          Inf
        lowerb <-
          znumer - qnorm(1 - alpha / 2) * sterrz
        pvalue <- pnorm((znumer / sterrz), lower.tail = TRUE)
      }
      if ((method == 'T') | (method == 't')) {
        alterhyp <-
          cat(
            "Corrected T-Test",
            "\nAlternative Hypothesis: True difference in means is less than",
            mu
          )
        upperb <-
          Inf
        lowerb <-
          tnumer - qnorm(1 - alpha / 2) * tsterr
        pvalue <- pnorm(t3, lower.tail = TRUE)
      }
    }
    if (alter == "greater") {
      if ((method == 'Z') | (method == 'z')) {
        alterhyp <-
          cat(
            "Corrected Z-Test",
            "\nAlternative Hypothesis: True difference in means is greater than",
            mu
          )
        lowerb <-
          -Inf
        upperb <-
          znumer + qnorm(1 - (alpha / 2)) * sterrz
        pvalue <- pnorm((znumer / sterrz), lower.tail = FALSE)
      }
      if ((method == 'T') | (method == 't')) {
        alterhyp <-
          cat(
            "Corrected T-Test",
            "\nAlternative Hypothesis: True difference in means is greater",
            mu
          )
        lowerb <-
          -Inf
        upperb <-
          tnumer + qnorm(1 - (alpha / 2)) * tsterr
        pvalue <- pnorm((t), lower.tail = FALSE)
      }
    }
    if (alter == "two.sided") {
      if ((method == 'Z') | (method == 'z')) {
        alterhyp <-
          cat(
            "Corrected Z-Test",
            "\nAlternative Hypothesis: True difference in means is not equal to",
            mu
          )
        lowerb <-
          znumer - qnorm(1 - alpha / 2) * sterrz
        upperb <-
          znumer + qnorm(1 - (alpha / 2)) * sterrz
        pvalue <-
          2 * pnorm(abs((znumer / sterrz)), lower.tail = FALSE)
      }
      else if ((method == 'T') | (method == 't')) {
        alterhyp <-
          cat(
            "Corrected Z-Test",
            "\nAlternative Hypothesis: True difference in means is not equal to",
            mu
          )
        lowerb <-
          tnumer - qnorm(1 - alpha / 2) * tsterr
        upperb <-
          tnumer + qnorm(1 - (alpha / 2)) * tsterr
        pvalue <- 2 * pnorm(abs(t3), lower.tail = FALSE)
      }
      
    }
    if ((method == 'Z') | (method == 'z')) {
      test_statistic <-
        znumer / sterrz
      estimate <- znumer + mu
    }
    if ((method == 'T') | (method == 't')) {
      test_statistic <-
        t3
      estimate <- znumer + mu
      
    }
    result <-
      cat(
        c(
          '\nmu:',
          mu,
          '  alpha:',
          alpha,
          ' p-value:',
          pvalue,
          '\n',
          ((1 - alpha) * 100),
          'percent confidence interval',
          '\nlower bound=',
          lowerb,
          '\nupper bound=' ,
          upperb,
          "\n",
          'test_statistic:',
          test_statistic,
          "\n",
          'estimate:',
          estimate
        )
      )
    #result
  }
}