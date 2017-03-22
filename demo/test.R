library(timeManip)

# create a POSIXlt date from a string of char
YYYY_chr("2015")
YYYYmm_chr("201501")
YYYYmmdd_chr("20150101")
YYYYmmddHH_chr("2015010102")
YYYYmmddHHMM_chr("201501010235")
YYYYmmddHHMMSS_chr("20150101023515")

# or
standard(precision="yearly","2015")
standard(precision="monthly","201501")
standard(precision="daily",  "20150101")
standard(precision="hourly", "2015010102")
standard(precision="minute", "201501010224")
standard(precision="second", "20150101022419")

# create a POSIXlt timeserie
tmp <- timeserie(timeResolution="hourly",fromPeriod="2015010102",toPeriod="2015010110")
tmp <- timeserie("three-hourly","2015010102","2015010110")
tmp <- timeserie("daily","2015010102","2015011210")
tmp <- timeserie("monthly","2015010102","2015011210")
tmp <- timeserie("yearly","2015010102","2019011210")
tmp <- timeserie(timeResolution="monthly",fromPeriod="2015010102",toPeriod="2015070110",precision="hourly")


# extract a part of a POSIXlt
year(tmp$seqPeriod)
month(tmp$seqPeriod)
day(tmp$seqPeriod)
hour(tmp$seqPeriod)
minute(tmp$seqPeriod)
second(tmp$seqPeriod)
wday(tmp$seqPeriod)
yday(tmp$seqPeriod)
isdst(tmp$seqPeriod)
tzone(tmp$seqPeriod)

# POSIXlt2char
YYYY(tmp$seqPeriod)
YYYYmm(tmp$seqPeriod)
YYYYmmdd(tmp$seqPeriod)
YYYYmmddHH(tmp$seqPeriod)
YYYYmmddHHMM(tmp$seqPeriod)
YYYYmmddHHMMSS(tmp$seqPeriod)

# operation on POSIXlt
tmp <- standard(precision="minute", "201501010224")
addition_nonsec(date=tmp,"hourly",v=10)
addition_nonsec(date=tmp,"monthly",v=-10)

# difference between 2 POSIXlt in second
tmp <- standard(precision="minute", "201501010224")
tmp2 <- standard(precision="second", "20150101022435")
difference(tmp,tmp2)

# contain between toe POSIXlt
a <- timeserie(timeResolution="daily",fromPeriod="2017010106",toPeriod="2017011006")$seqPeriod
b <- timeserie(timeResolution="daily",fromPeriod="2017010101",toPeriod="2017013106")$seqPeriod
contain(a,b)

# object timeManip
test1 <- timeManip(fromPeriod="2000090106",toPeriod="2014123106",timeResolution="daily")
test1$fromPeriod()
test1$toPeriod()
test1$timeResolution()
test1$Timeresinsec()
test1$nbStep()
test1$seqPeriod()




tmp <-timeManip::timeManip("2016122106","2016122506","hourly","hourly")
tmp$timeResolution()
tmp$seqPeriod()
