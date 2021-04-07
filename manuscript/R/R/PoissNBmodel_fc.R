### SIRVERA DATA: POISS AND NB REGRESSION FUNCTIONS ###
## Poisson distribution
NLLpois = function(data, lambda){-sum(dpois(data, lambda=lambda, log=TRUE))}

PRmodel  <-  function(data){
  ### FC to generate a univariate poisson regression model and report stats
  model  <-  glm(incidence~months, data=data, family=poisson, control = list(maxit = 100)) 
  
  ## how wel it fits compared to null model
  chi <- model$null.deviance - model$deviance 
  df <- model$df.null - model$df.residual
  chsq.prob <- 1 - pchisq(chi, df) # chi-square probability
  
  goodfit <- pchisq(model$deviance, df=model$df.residual, lower.tail=FALSE) # goodnes of fit 
  # >0.05 indicates that the model fits the data
  
  #(summary(model.NB)$null.deviance-summary(model.NB)$deviance)/summary(model.NB)$null.deviance 
  #(NULL.dev-res.dev)/NULL.dev
  
  ## coefficients
  coeff <- exp(model$coefficients)[2] # determine trend - if > 1 increasing
  pval <- summary(model)$coefficients[2,4] # p-value
  pseudoR2 <- pR2(model)["McFadden"]  # pseudo R-squared: 'McFadden' btw 0 (no predictive power) and 1
  
  ## fitted values
  fitted <- model$fitted # fitted values
  
  m <- list(chsq.prob=chsq.prob, goodfit=goodfit, coeff = coeff, pval = pval, fitted = fitted)
}

# ## alternative way how to compare a model with its null model
# mymonths <- 1:132
# model  <-  glm(states[,7]~mymonths, family=poisson, control = list(maxit = 100)) 
# modelnul <- glm(states[,7]~1, family=poisson, control = list(maxit = 100)) 
# logLik(model); logLik(modelnul)
# AIC(model, modelnul)

## Negative binomial distribution
NLLnb= function(m,v,data){
  k = (m^2)/(v-m) # clumping parameter k: (mean^2)/(variance-mean)
  -sum(dnbinom(data, mu=m, size=k, log=TRUE))
}