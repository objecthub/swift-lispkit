# LispKit Date-Time

Library `(lispkit date-time)` provides functionality for handling time zones, dates, and times. Time zones are represented by string identifiers referring to the region and corresponding city, e.g. `"America/Los_Angeles"`. Dates and times are represented via `date-time` data structures. These encapsulate the following components:

   - _time zone_: the time zone of the date
   - _date_: the date consisting of its year, month, and day
   - _time_: the time on _date_ consisting of the hour (>= 0, < 24), the minute (>= 0, < 60), the second (>= 60, <60), and the nano second.

The library uses a floating-point representation of seconds since 00:00 UTC on January 1, 1970, as a means to refer to specific points in time independent of timezones. This means that, for instance, for comparing date-times with each other, a user would have to convert them to seconds and then compare the seconds instead. Here is an example:

```scheme
(define initial-time (date-time "Europe/Zurich"))
(define later-time (date-time "GMT"))
(date-time< initial-time later-time)
  ⇒  #t
; the following line is equivalent:
(< (date-time->seconds initial-time) (date-time->seconds later-time))
  ⇒  #t
```

For now, `(lispkit date-time)` assumes all dates are based on the Gregorian calendar, independent of the settings at the operating system-level. 


## Time zones

Time zones are represented by string identifiers referring to the region and corresponding city, e.g. `"America/Los_Angeles"`. Procedure `timezones` returns a list of all supported time zone identifiers. Each time zone has a locale-specific name and an offset in seconds from Greenwhich Mean Time. Some time zones also have an abbreviation which can be used as an alternative way to identify a time zone.

**(timezones)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(timezones _filter_)**  

Returns a list of string identifiers for all supported time zones. If _filter_ is provided, it can either be set to `#f`, in which case a list of abbreviations is returned instead, or it is a string, and only time zone identifiers which contain _filter_ are returned.

```scheme
(timezones #f)
⇒  ("CEST" "GST" "NZDT" "BRST" "WEST" "AST" "MSD" "CDT" "WIT" "MSK" "COT" "IST" "EST" "BST" "CLST" "NDT" "TRT" "EET" "IRST" "EDT" "BRT" "ICT" "CST" "AKST" "BDT" "PHT" "SGT" "WET" "ART" "CLT" "CAT" "UTC" "EEST" "ADT" "JST" "HST" "PET" "MST" "NST" "NZST" "GMT" "MDT" "PKT" "WAT" "HKT" "AKDT" "KST" "PST" "CET" "PDT" "EAT")
```

**(timezone? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid time zone identifier or time zone abbreviation; returns `#f` otherwise.

**(timezone)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(timezone _ident_)**  

Returns the identifier for the time zone specified by _ident_. _ident_ can either be an identifier, an abbreviation or a GMT offset as a floating-point number or integer. If _ident_ does not refer to a supported time zone, procedure `timezone` will fail.

**(timezone-name _tz_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(timezone-name _tz locale_)**  
**(timezone-name _tz locale format_)**  

Returns a locale-specific name for time zone _tz_. If _locale_ is not specified, the current locale defined at the operating-system level is used. _format_ specifies the name format. It can have one of the following symbolic values:

   - `standard`
   - `standard-short`
   - `dst`
   - `dst-short`
   - `generic`
   - `generic-short`

**(timezone-abbreviation _tz_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string representing a time zone abbreviation for _tz_; e.g. `"PDT"`. If the time zone _tz_ does not have an abbreviation, this function returns `#f`.

**(timezone-gmt-offset _tz_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the difference in seconds between time zone _tz_ and Greenwich Mean Time. The difference is returned as a floating-point number (since seconds are always represented as such by this library).


## Time stamps

Time stamps, i.e. discreet points in time, are represented as floating-point numbers corresponding to the number of seconds since 00:00 UTC on January 1, 1970.

**(current-seconds)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970.

**(seconds-\>date-time _secs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(seconds-\>date-time _secs tz_)**  

Converts the given number of seconds _secs_ into date-time format for the given time zone _tz_. _secs_ is a floating-point number. It is interpreted as the number of seconds since 00:00 UTC on January 1, 1970. _secs_ is negative if the date-time is earlier than 00:00 UTC on January 1, 1970. If _tz_ is missing, the current, operating-system defined time zone is used.

**(date-time-\>seconds _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970 for the given date-time object _dtime_.


## Date-times

**(date-time? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a date-time object; returns `#f` otherwise.

**(date-time)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(date-time _year month day_)**  
**(date-time _year month day hour_)**  
**(date-time _year month day hour min_)**  
**(date-time _year month day hour min sec_)**  
**(date-time _year month day hour min sec nano_)**  
**(date-time _tz_)**  
**(date-time _tz year month day_)**  
**(date-time _tz year month day hour_)**  
**(date-time _tz year month day hour min_)**  
**(date-time _tz year month day hour min sec_)**  
**(date-time _tz year month day hour min sec nano_)**  

Constructs a date-time representation out of the given date time components. _tz_ is the only string argument; it is referring to a time zone. All other arguments are numeric arguments. This procedure returns a date-time object for the specified time at the given date. If no date components are provided as arguments, procedure `date-time` returns a date-time for the current date and time.

**(week-\>date-time _year week_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(week-\>date-time _year week wday_)**  
**(week-\>date-time _year week wday hour_)**  
**(week-\>date-time _year week wday hour min_)**  
**(week-\>date-time _year week wday hour min sec_)**  
**(week-\>date-time _year week wday hour min sec nano_)**  
**(week-\>date-time _tz year week_)** 
**(week-\>date-time _tz year week wday_)**  
**(week-\>date-time _tz year week wday hour_)**  
**(week-\>date-time _tz year week wday hour min_)**  
**(week-\>date-time _tz year week wday hour min sec_)**  
**(week-\>date-time _tz year week wday hour min sec nano_)**  

Constructs a date-time representation out of the given date time components. _tz_ is the only string argument; it is referring to a time zone. All other arguments are numeric arguments. Argument _wday_ specifies the week day in the given week. Week days are given numbers from 1 (= Monday) to 7 (= Sunday). This procedure returns a date-time object for the specified time at the given date.

The difference to `date-time` is that this procedure does not refer to a month and day. It rather refers to the week number as well as the weekday within this specified week number.

**(date-time-in-timezone _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(date-time-in-timezone _dtime tzone_)**  

Constructs a date-time representation of the same point in time like _dtime_, but in a potentially different time zone _tzone_. If _tzone_ is not given, the default time zone specified by the user in the operating system will be used.

**(string-\>date-time _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-\>date-time _str tz_)**  
**(string-\>date-time _str tz locale_)**  
**(string-\>date-time _str tz locale format_)**  

Extracts a date and time from the given string _str_ in the time zone _tz_, or the current time zone if _tz_ is omitted. The format of the string representation is defined in terms of _locale_ and _format_. _format_ can have three different forms:

1. Combined format identifier for date and time: `date-time` parsing is based on the settings of the operating system. _format_ is one of the following symbols: `none`, `short`, `medium`, `long`, or `full`.
2. Separate format identifiers for date and time: `date-time` parsing is based on the settings of the operating system, but the format for dates and times is specified separately. _format_ is a list of the form `(`_dateformat_ _timeformat_`)` where both _dateformat_ and _timeformat_ are one of the 5 symbols listed under 1. This makes it possible, for instance, to just parse a date (without time) in string form to a date-time object, e.g. by using `(short none)` as _format_.
3. Custom format specifier: `date-time` parsing is based on a custom format string. _format_ is a string using the following characters as placeholders. Repetitions of the placeholder characters are used to specify the width and format of the field.
    - `y`: Year
    - `M`: Month
    - `d`: Day
    - `H`: Hour (12 hours)
    - `h`: Hour (24 hours)
    - `m`: Minute
    - `s`: Second
    - `S`: Micro second
    - `Z`: Time zone
    - `a`: AM/PM
    - `E`: Weekday

Here are a few examples:
```
EEEE, MMM d, yyyy       ~~>  Thursday, Feb 8, 1973
dd/MM/yyyy              ~~>  08/02/1973
dd-MM-yyyy HH:mm        ~~>  08-02-1973 17:01
MMM d, h:mm a           ~~>  Thu 8, 2:11 AM
yyyy-MM-dd'T'HH:mm:ssZ  ~~>  1973-08-02T17:01:31+0000
HH:mm:ss.SSS            ~~>  11:02:19.213
```

**(date-time-\>string _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(date-time-\>string _dtime locale_)**  
**(date-time-\>string _dtime locale format_)**  

Returns a string representation of the date-time object _dtime_. The format of the string is defined in terms of _locale_ and _format_. _format_ can have three different forms (just like for `string->date-time`):

1. Combined format identifier for date and time: `date-time` formatting is based on the settings of the operating system. _format_ is one of the following symbols: `none`, `short`, `medium`, `long`, or `full`.
2. Separate format identifiers for date and time: `date-time` formatting is based on the settings of the operating system, but the format for dates and times is specified separately. _format_ is a list of the form `(`_dateformat_ _timeformat_`)` where both _dateformat_ and _timeformat_ are one of the 5 symbols listed under 1. This makes it possible, for instance, to just output a date (without time) in string form, e.g. by using `(short none)` as _format_.
3. Custom format specifier: `date-time` formatting is based on a custom format string. _format_ is a string using the following characters as placeholders. Repetitions of the placeholder characters are used to specify the width and format of the field.
    - `y`: Year
    - `M`: Month
    - `d`: Day
    - `H`: Hour (12 hours)
    - `h`: Hour (24 hours)
    - `m`: Minute
    - `s`: Second
    - `S`: Micro second
    - `Z`: Time zone
    - `a`: AM/PM
    - `E`: Weekday

Here are a few examples:
```
EEEE, MMM d, yyyy       ~~>  Thursday, Feb 8, 1973
dd/MM/yyyy              ~~>  08/02/1973
dd-MM-yyyy HH:mm        ~~>  08-02-1973 17:01
MMM d, h:mm a           ~~>  Thu 8, 2:11 AM
yyyy-MM-dd'T'HH:mm:ssZ  ~~>  1973-08-02T17:01:31+0000
HH:mm:ss.SSS            ~~>  11:02:19.213
```

**(date-time-\>iso8601-string _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string representation of the date-time object _dtime_ in ISO 8601 format.

**(date-time-timezone _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the time zone of _dtime_.

**(date-time-year _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the year of _dtime_.

**(date-time-month _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the month of _dtime_.

**(date-time-day _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the day of _dtime_.

**(date-time-hour _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the hour of _dtime_.

**(date-time-minute _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minute of _dtime_.

**(date-time-second _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the second of _dtime_.

**(date-time-nano _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the nano-second of _dtime_.

**(date-time-weekday _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the week day of _dtime_. Week days are represented as fixnums where 1 is Monday, 2 is Tuesday, ..., and 7 is Sunday.

**(date-time-week _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the week number of _dtime_ according to the ISO-8601 standard. Based on this standard, weeks start on Monday. The first week of the year is the week that contains that year's first Thursday.

**(date-time-dst-offset _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the daylight saving time offset of _dtime_ in seconds related to GMT. If daylight savings time is not active, `date-time-dst-offset` returns `0.0`. The result is always a floating-point number.

**(date-time-hash _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hash code for the given date-time object. This hash code can be used in combination with both `date-time=?` and `date-time-same?`.


## Date-time predicates

**(date-time-same? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ and _dtime2_ have the same timezone and refer to the same point in time, i.e. `(date-time->seconds dtime1)` and `(date-time->seconds dtime2)` are equals.

```scheme
(define d1 (date-time 'CET))
(define d2 (date-time-in-timezone d1 'PST))
(date-time-same? d1 d1)  ⇒  #t
(date-time-same? d1 d2)  ⇒  #f
(date-time=? d1 d2)      ⇒  #t
```

**(date-time=? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ and _dtime2_ specify the same point in time, i.e. `(date-time->seconds dtime1)` and `(date-time->seconds dtime2)` are equals.

```scheme
(define d1 (date-time 'CET))
(define d2 (date-time-in-timezone d1 'PST))
(date-time=? d1 d2)                ⇒  #t
(date-time=? d1 (date-time 'CET))  ⇒  #f
```

**(date-time\<? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ specifies an earlier point in time compared to _dtime2_, i.e. `(date-time->seconds dtime1)` is less than `(date-time->seconds dtime2)`.

**(date-time\>? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ specifies a later point in time compared to _dtime2_, i.e. `(date-time->seconds dtime1)` is greater than `(date-time->seconds dtime2)`.

**(date-time\<=? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ specifies an earlier or equal point in time compared to _dtime2_, i.e. `(date-time->seconds dtime1)` is less than or equal to `(date-time->seconds dtime2)`.

**(date-time\>=? _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if date-time _dtime1_ specifies a later or equal point in time compared to _dtime2_, i.e. `(date-time->seconds dtime1)` is greater than or equal to `(date-time->seconds dtime2)`.

**(date-time-has-dst? _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if daylight saving time is active for _dtime_; returns `#f` otherwise.


## Date-time operations

**(date-time-add _dtime days_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(date-time-add _dtime days hrs_)**  
**(date-time-add _dtime days hrs min_)**  
**(date-time-add _dtime days hrs min sec_)**  
**(date-time-add _dtime days hrs min sec nano_)**  

Compute a new date-time from adding _days_, _hrs_, _min_, _sec_, and _nano_ (all fixnums) to the given date-time _dtime_. The resulting date-time is using the same timezone like _dtime_.

**(date-time-add-seconds _dtime sec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Compute a new date-time from adding the number of seconds _sec_ (a flonum) to the given date-time _dtime_.

**(date-time-diff-seconds _dtime1 dtime2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the difference between _dtime2_ and _dtime1_ as a number of seconds (a flonum).

**(next-dst-transition _dtime_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the date and time when the next daylight savings time transition takes place after _dtime_. `next-dst-transition` returns `#f` if there is no daylight savings time for the time zone of _dtime_.
