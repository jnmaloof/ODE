data <- read.csv("spar55t10.csv",row.names=1)

head(data)

time <- as.numeric(rownames(data))

plot.TL <- function(TL) {
	par(mfrow=c(4,4))

	for (i in 1:ncol(TL)) {
		plot(as.numeric(rownames(TL)),TL[,i],xlab = "Time", ylab = "Growth", type="l",main=colnames(TL)[i],
		ylim=range(TL,na.rm=T))
		lim <- par()$usr # get coordinate limits for the current graph
		dusk <- seq(480,lim[2],by=1440)
		rect(xleft=dusk,ybottom=lim[3],xright=dusk+960,ytop=lim[4],density=10,col="gray")
		abline(h=0)
			}#for i
		par(mfrow=c(1,1))
}#plot.TL

get.resid <- function(x,time) {
	residuals(lm(x~time,na.action=na.exclude))
	}

remove.SD <- function(data) {
	for(i in 1:ncol(data)) {
		x <- 1
		while(x < nrow(data) && !(is.na(data[x,i])))
		{
			x <- x + 144
		}
		x <- x - 144
		y <- 1
		while(y < x)
		{
			a <- y + 144
			subset <- data[y:a,i]
			stddev <- sd(subset, na.rm=T)
			for(k in y:a) {
				
				data[k,i] <- (data[k,i]/stddev)
			}
			y <- y + 144
		}
	} 
	return(data)
}
newdata <- apply(data,2,get.resid,time)

newdata <- remove.SD(newdata)

print(summary(newdata))


plot.TL(newdata)

plot.TL(data)