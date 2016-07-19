setwd("Dropbox/data/")

dat <- read.csv("allwells_final.csv")

IgG <- dat[,9:20]
IgGU <- dat[,21:32]
IgM <- dat[,33:44]


plot(1:12,IgG[1,],type='l')

x=1:12
y=as.numeric(IgG[1,])
mod  <- loess(y~x)
pred <- predict(mod)

plot(pred,type='l')


cols= rainbow(length(dat$num))

plot(smooth.spline(1:11,y[1:11]),type='l',ylim=c(0,1.5))


for(ii in seq_along(IgG[,1])){
    lines(smooth.spline(1:11,IgG[ii,1:11]), col=cols[ii])
}


plot(1,1,xlim=c(1,2),ylim=c(0,1.655))
for(ii in seq_along(IgG[,1])){
    lines(1:2,IgG[ii,1:2], col=cols[ii])
}


