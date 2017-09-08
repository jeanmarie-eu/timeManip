# timeManip 0.91

* create a function df2time() that create POSIXlt vector from a data.frame with date information
* some new and potentially more accurate names for the files

# timeManip 0.9

* update POSIXlt2matrix functions: YYYY_m, ..., YYYYmmddHHMMSS_m
* delete obsolete function convert_format
* update demos
* make the object timeManip build a date function only when asked to save memory

# timeManip 0.1

* build POSIXlt from char: add standard, YYYY_chr, YYYYmm_chr, YYYYmmdd_chr, YYYYmmddHH_chr, YYYYmmddHHSS_chr
* extract part from POSIXlt, and get char: add YYYY, YYYYmm, YYYYmmdd, YYYYmmddHH, YYYYmmddHHSS
* extract part from POSIXlt, and get numeric: add year, month, day, hour, minute, second
* update operation functions: addition and difference
* wrote timeserie: in replacement of dateTimeSerie, with a cleaner code and add argument precision
* update timeManip: cleaner code

# timeManip 0.0.4

* add standard

# timeManip 0.0.3

* add POSIXlt2char
* add char2POSIXlt
* add getP: get a part of a POSIXlt
* add addition_char_char: addition of a date written i character to a value in an unit to be specified
* add addition_plt_sec: addition of a POSIXlt date to a value in second

# timeManip 0.0.2

* dateTimeSerie can build daily timeseries with hourly precision
* dateTimeSerie can build three-hourly timeseries

# timeManip 0.0.1

* Added a `NEWS.md` file to track changes to the package.
