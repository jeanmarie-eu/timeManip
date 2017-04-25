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
tmp <- timeserie("hourly",v=3,"2015010102","2015010110")
tmp <- timeserie("daily","2015010102","2015011210")
tmp <- timeserie("daily",v=2,"2015010102","2015011210")
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


# POSIXlt2matrix
YYYY_m(tmp$seqPeriod)
YYYYmm_m(tmp$seqPeriod)
YYYYmmdd_m(tmp$seqPeriod)
YYYYmmddHH_m(tmp$seqPeriod)
YYYYmmddHHMM_m(tmp$seqPeriod)
YYYYmmddHHMMSS_m(tmp$seqPeriod)


# operation on POSIXlt
tmp <- standard(precision="minute", "201501010224")
addition_nonsec(date=tmp,"hourly",v=10)
addition_nonsec(date=tmp,"daily",v=-10)
addition_nonsec(date=tmp,"monthly",v=-10) # do not work yet

# difference between 2 POSIXlt in second
tmp <- standard(precision="minute", "201501010224")
tmp2 <- standard(precision="second", "20150101022435")
difference(tmp,tmp2)

# contain between toe POSIXlt
ts <- timeserie(timeResolution="daily",fromPeriod="20170101",toPeriod="20170131")$seqPeriod
ts_sub <- timeserie(timeResolution="daily",fromPeriod="20170105",toPeriod="20170110")$seqPeriod
contain(ts=ts,ts_sub=ts_sub)

# contain between toe POSIXlt
ts <- timeserie(timeResolution="daily",fromPeriod="2017010106",toPeriod="2017013106")$seqPeriod
ts_sub <- timeserie(timeResolution="daily",fromPeriod="2017010507",toPeriod="2017011007")$seqPeriod
contain(ts=ts,ts_sub=ts_sub)

a <- timeManip(fromPeriod="2013060205",toPeriod="2013060605",timeResolution="hourly",precision="hourly")
b <- timeManip(fromPeriod="2013060305",toPeriod="2013060505",timeResolution="hourly",precision="hourly")
res <- indice_subdate(date=a,d=b)
str(res)

a <- timeManip(fromPeriod="2013060205",toPeriod="2013060605",timeResolution="daily",precision="hourly")
b <- timeManip(fromPeriod="2013060308",toPeriod="2013060508",timeResolution="daily",precision="hourly")
res <- indice_subdate(date=a,d=b)
str(res)


# object timeManip
test1 <- timeManip(fromPeriod="2000090106",toPeriod="2014123106",timeResolution="daily")
test1$fromPeriod()
test1$toPeriod()
test1$timeResolution()
test1$Timeresinsec()
test1$nbStep()
test1$seqPeriod()
