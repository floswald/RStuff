# try to do this with stata.

# load data
hdata <- read.csv("helpdesk log.csv",header=TRUE,sep=",")
hdata$date <- as.Date(hdata$Timestamp,format="%m/%d/%Y")	# Timestamp is in mm/dd/YYYY format

daycount <- table(hdata$date)       # count how many hits per day

# daycount is what I call the "short" vector. It misses some days.

##################################
# daily response wrong graph

oldmar <- c(5,4,4,2) + 0.1      # store default pars 
par(mar = c(7, 4, 4, 2) + 0.1)  # setup bigger plotting window
png(file="daily-wrong.png")
plot(daycount,xaxt="no",type="l",main="Daily number of Interactions",ylab = "Number of Interactions",xlab="")
axis(1,at=seq(1,44,by=6),labels=FALSE)
labs <- names(daycount)
text(seq(1,44,by=6), par("usr")[3] - 0.5, srt = 45, adj = 1,    # text() allows to print axis labels rotated (srt=45)
          labels = labs[seq(1,44,by=6)], xpd = TRUE)
          mtext("Date",side=1,line = 5)
grid()
dev.off()
par(mar = oldmar)
##################################

# generate vector of all dates
alldays <- seq(hdata$date[1],length=62,by="+1 day")			# this vector has all days
                                                            # notice how R recognizes the date format and chooses the appropriate method for the seq() function. You don't have to worry about 30 days in June, 31 in July etc. R does it all for you.

allcount <- table(alldays)      # create table object from alldays.
actindex <- match(names(allcount),names(daycount),nomatch = 0)  
# create "active" index: vector of length(allcount), i.e. all days. on days with no activity (i.e. a missing day in daycount), this has value 0 (nomatch = 0). For days with activity, actindex holds the index of the matching position in daycount. 

# function to get entries of daycount corresponding to actindex
# indexing is a bit tricky. i loops over all days. get correct date by
# substracting all "zero-activity" days accumulated so far.
days <- function(actindex,daycount){
	n <- length(actindex)
	x <- rep(NA,times=n)
	zero <- 0
	for (i in 1:n){
		if (actindex[i]==0) {
			zero <- zero +1
			x[i] <- 0
			} else {
				x[i] <- daycount[i-zero]
			}			
		}
		return(x)
		}

alldaycount <- array(days(actindex,daycount))   # construct vector with number of hits per day
names(alldaycount) <- names(allcount)           # name entries by consecutive dates.

##################################
# daily response: correct graph
png(file="dailyrespons.png")    # save next graph as .png
par(mar = c(7, 4, 4, 2) + 0.1)
plot(alldaycount,axes=FALSE,type="l",main="Daily number of Interactions",ylab = "Number of Interactions",xlab="")
axis(1,at=seq(1,62,by=5),labels=FALSE)
axis(2,at=c(0,1,2,3,4,5,8))
labs <- names(alldaycount)[seq(1,62,by=5)]
text(seq(1,62,by=5), par("usr")[3] - 0.5, srt = 45, adj = 1,
          labels = labs, xpd = TRUE)
          mtext("Date",side=1,line = 5)
grid()
dev.off()
par(mar = oldmar)
##################################
