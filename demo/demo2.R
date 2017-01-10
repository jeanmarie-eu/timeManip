rm(list=ls())

library(timeManip)
a <- timeManip::dateTimeSerie(timeResolution="daily",fromPeriod="2017010106",toPeriod="2017011006")$seqPeriod
b <- timeManip::dateTimeSerie(timeResolution="daily",fromPeriod="2017010101",toPeriod="2017013106")$seqPeriod
timeManip::contain(a,b)
