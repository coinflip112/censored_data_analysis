variables <- c("bili", "albumin", "platelet")
PlotCoefFactor <- function(variable, ngroups = 10){
  if(ngroups < 2){
    stop("ngroups has to be at least 2!")}
  othervar <- setdiff(variables, variable)
  qq <- quantile(subdata[,variable], probs = seq(0,1,length.out = ngroups+1))
  ff <- cut(subdata[,variable], breaks = c(-Inf, qq[2:ngroups], Inf))
  model <- glm(as.formula(paste("delta ~ ftrt + fsex + fedema + ff +",
                                paste(setdiff(variables, variable), collapse = " + "), 
                                " + offset(log(time))", sep = "")), 
               family = poisson, data = subdata)
  yy <- c(0, # coefficient for the first (reference group)
          model$coefficients[grep("ff", names(model$coefficients))]) 
  xx <- (qq[1:ngroups]+qq[2:(ngroups+1)])/2
  plot(yy ~ xx, 
       type = "b", 
       xlab = variable, 
       ylab = "Coefficient",
       cex = summary(ff)/max(summary(ff))*2+0.5)
  }

# For example Handtime
par(mar = c(4,4,1,1))
PlotCoefFactor("bili")
