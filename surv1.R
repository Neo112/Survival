# implement with yangshan supermarket data

library(data.table)
library(survival)

# dt <- fread("SalesDetails.csv",integer64 = "numeric",stringsAsFactors = FALSE, data.table = TRUE)
dtt <- fread("MemberDetails.csv",integer64 = "numeric",stringsAsFactors = FALSE, data.table = TRUE)
#names(dt)
dtt <- dtt[, .(id = 会员姓名,time= as.Date(时间))][,time:=as.integer(time-min(time)+1)]
dtt <- unique(dtt)
dtt <- dtt[, id_n := .GRP, by=id][,id:=NULL][,.(id_n,time)] # replace name by num

dtt <- dtt[,status := 1]
id_n <- unique(dtt[,id_n])
max_time <- max(dtt[,time])
dtt_adtion <- data.table(id_n=rep(id_n,2),time=c(rep(0,length(id_n)),rep(max_time,length(id_n))),status=0)
mdtt <- rbind(dtt, dtt_adtion)
setkey(mdtt,id_n,time)
start <- c(0,mdtt[,time])
start <- start[-length(start)]
mdtt <- mdtt[,start := start]
mdtt <- mdtt[,.(id_n, start, stop=time, status)]
mdtt <- mdtt[stop !=0 & start != stop]

mdtt[,.N, by = id_n][order(-N)]
test1 <- mdtt[,z_buy:=1*(.N>3),by=id_n]
sfit <- coxph(Surv(start,stop,status)~1+strata(z_buy), data=test1)
# sfit1 <- survfit(Surv(start,stop,status)~1+strata(id_n),data=test1, type='fleming')
plot(survfit(sfit), lty=1:2,fun="cumhaz", xlab = "time", ylab = "Cumhaz")
legend("topleft",legend=c("buy_time<=3","buy_time>3"), lty = 1:2)


temp <- survfit(sfit)
time <- temp$time
cumhaz <- temp$cumhaz
stra=temp$strata

# first smoothing ------------------------------------
smoothingSpline = smooth.spline(time[(stra[1]+1):sum(stra)],cumhaz[(stra[1]+1):sum(stra)], spar=0.3)
plot(smoothingSpline$x,c(0,diff(smoothingSpline$y)),"l",lty=2)
smoothingSpline = smooth.spline(time[1:stra[1]],cumhaz[1:stra[1]], spar=0.35)
lines(smoothingSpline$x,c(0,diff(smoothingSpline$y)),"l",add = T,lty=1)
legend("topleft",legend=c("buy_time>3","buy_time<=3"), lty = c(2,1))

# second smoothing ------------------------------------
f1=splinefun(time[1:stra[1]],cumhaz[1:stra[1]],method = "monoH.FC")
plot(time[1:stra[1]],cumhaz[1:stra[1]])
curve(f1,add = T,col="blue")
curve(f1(x,deriv = 1),add=T,time[1],time[stra[1]], col="red")

f2=splinefun(time[(stra[1]+1):sum(stra)],cumhaz[(stra[1]+1):sum(stra)],method = "monoH.FC")
plot(time[(stra[1]+1):sum(stra)],cumhaz[(stra[1]+1):sum(stra)])
time[(stra[1]+1):sum(stra)]
curve(f2,add = T,col="blue")
curve(f2(x,deriv = 0),time[stra[1]+1],time[sum(stra)], col="red")
curve(f2(x,deriv = 1),add=T,time[stra[1]+1],time[sum(stra)], col="red")

curve(f2(x,deriv = 1),time[stra[1]+1],time[sum(stra)], col="blue")
curve(f1(x,deriv = 1),time[1],time[stra[1]], col="red", add=T)

# test -------------------------
fit <- survfit(Surv(stop,status)~id_n, test1)
plot(fit, lty=2:3, fun="cumhaz")
