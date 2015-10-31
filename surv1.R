# implement with yangshan supermarket data

library(data.table)
library(survival)

# dt <- fread("SalesDetails.csv",integer64 = "numeric",stringsAsFactors = FALSE, data.table = TRUE)
dtt <- fread("MemberDetails.csv",integer64 = "numeric",stringsAsFactors = FALSE, data.table = TRUE)
#names(dt)
dtt <- dtt[, .(id = 会员姓名,time= as.Date(时间))][,time:=time-min(time)+1]
dtt <- dtt[,time:=as.integer(time)]
dtt <- unique(dtt)
dtt <- dtt[, id_n := .GRP, by=id][,id:=NULL][,.(id_n,time)]


dtt <- dtt[,status := 1]
id_n <- unique(dtt[,id_n])
max_time <- max(dtt[,time])
dtt_adtion <- data.table(id_n=rep(id_n,2),time=c(rep(0,length(id_n)),rep(max_time,length(id_n))),status=0)
mdtt <- rbind(dtt, dtt_adtion)
setkey(mdtt,id_n,time)
start <- c(0,mdtt[,time])
start <- time0[-length(start)]
mdtt <- mdtt[,start := start]
mdtt <- mdtt[,.(id_n, start, stop=time, status)]
mdtt <- mdtt[stop !=0 & start != stop]

mdtt[,.N, by = id_n][order(-N)]
test1 <- mdtt[id_n %in% c(1,2)]
sfit <- survfit(coxph(Surv(start,stop,status)~1+strata(id_n), data=test1))
sfit1 <- survfit(Surv(start,stop,status)~1+strata(id_n),data=test1, type='fleming')
plot(sfit1, lty=1:2, fun="cumhaz")
plot(sfit,lty=nrow(test1), fun="cumhaz")


# test -------------------------
fit <- survfit(Surv(stop,status)~id_n, test1)
plot(fit, lty=2:3, fun="cumhaz")
