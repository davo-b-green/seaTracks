# Dive residual calculation
dive.residual <- function(x){  # ensure data order is id,time,dur,depth
  # library(nlme)
  lmc <- lmeControl(maxIter=100,msMaxIter=100,niterEM=100,returnObject=TRUE) # give result even if non-conv

  dat <- x
  names(dat) <- c("id", "time", "ddur", "ddepth")
  dat <- dat %>%
    mutate(ddur = as.numeric(ddur),
           tm = as.numeric(time),
           yday = as.POSIXlt(time)$yday+1)
  # dat$yday[dat$yday>300] <- -c(366-dat$yday[dat$yday>300])    # few data late Dec
  dat$dup <-unlist(by(dat$tm,dat$id,duplicated))  # can't have duplicate timestamps
  xx2 <- which(dat$dup==TRUE);

  if (length(xx2)) dat$tm[xx2] <- dat$tm[xx2] + runif(length(xx2), min=-30, max=30)

  dat$ddur[dat$ddur==0] <- NA    # neccessary for log models, flunks poly models
  dat$ddepth[dat$ddepth==0] <- 6
  dat$log.ddur <- log(dat$ddur)
  dat$log.ddepth <- log(dat$ddepth)

  fit <- lme(log.ddur ~ log.ddepth,
             random=~1+log.ddepth|id,
             control=lmc,
             data=dat,
             na.action=na.exclude,
             method="REML")

  # OUTPUT save the predicted and various residual values
  dat$pred <- as.vector(fitted(fit))
  dat$pred <- exp(dat$pred)

  dat$res.raw <- as.vector(resid(fit, type="response"))  # raw
  dat$res.pears <- as.vector(resid(fit, type="pearson"))   # these and raw give same pos/negs; norm'd different
  dat$res.norm <- as.vector(resid(fit, type="normalized"))  # takes a long time with corAR1

  dv.res <- list(data=dat , model=fit)   # return data plus result and model
  dv.res

}
