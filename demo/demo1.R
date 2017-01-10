rm(list=ls())

library(timeManip)

test1 <- timeManip(fromPeriod="2000090106",toPeriod="2014123106",timeResolution="daily")
test1$nbStep()
test1$seqPeriod()

test2 <- timeManip(fromPeriod="2000090100",toPeriod="2014123100",timeResolution="daily")
test2$nbStep()
test2$seqPeriod()

test3 <- timeManip(fromPeriod="2000090100",toPeriod="2014123100",timeResolution="hourly")
test3$nbStep()
test3$seqPeriod()
