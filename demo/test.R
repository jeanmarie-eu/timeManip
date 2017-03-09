library(timeManip)

a <- YYYYmmddHH_chr("2015010102")
b <- YYYYmmddHH_chr("2015010110")
truc <- as.POSIXlt(seq(a,b,by=3600))
YYYY(truc)

 YYYYmmddHHMMSS(as.POSIXlt(truc))

test1 <- timeManip(fromPeriod="2000090106",toPeriod="2014123106",timeResolution="daily")
test1$nbStep()
test1$seqPeriod()

test2 <- timeManip(fromPeriod="2000090100",toPeriod="2014123100",timeResolution="daily")
test2$nbStep()
test2$seqPeriod()

test3 <- timeManip(fromPeriod="2000090100",toPeriod="2014123100",timeResolution="hourly")
test3$nbStep()
test3$seqPeriod()

a <- timeManip::dateTimeSerie(timeResolution="daily",fromPeriod="2017010106",toPeriod="2017011006")$seqPeriod
b <- timeManip::dateTimeSerie(timeResolution="daily",fromPeriod="2017010101",toPeriod="2017013106")$seqPeriod
timeManip::contain(a,b)
