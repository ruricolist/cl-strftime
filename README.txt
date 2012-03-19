CL-STRFTIME is a Common Lisp compiler for the strftime language. It is
not an interface to a foreign function, but a re-implementation from
scratch.

FORMAT-TIME takes four arguments: a stream, a strftime-style control
string, a universal time (defaults to now), and a time zone (defaults
to here).

     (cl-strftime:format-time t "%D" (get-universal-time))
     => 02/12/12

The second exported function, MAKE-TIME-FORMATTER, is to FORMAT-TIME
as FORMATTER is to FORMAT.

As an extension, FORMAT-TIME understands a number of named formats. To
use a named format, pass a keyword in place of the control string.

     (cl-strftime:format-time t :rfc-3339 (get-universal-time))
     => 2012-03-19T06:16:29Z

FORMAT-TIME understands the following flags and directives:

FLAGS

- Do not pad
_ Pad with spaces
0 Pad with zeros
# Toggle case
^ Upcase

DIRECTIVES

%% Literal percent sign
%A Weekday (Sunday)
%a Abbreviated weekday (Sun)
%B Month (January)
%b Abbreviated month (Jan)
%C Century (20)
%d Day of month (01-31)
%D Same as %m/%d/%y
%e Same as %_2d
%F Same as %Y-%m-%d
%G ISO 8601 year
%g ISO 8601 year in century
%h Same as %b
%H Hour (01–24)
%I Hour (01–12)
%j Day of year (001-366)
%k Hour ( 1-24)
%l Hour ( 1-12)
%m Month (01-12)
%M Minute (00-59)
%n Literal newline
%p AM or PM
%P am or pm
%r Same as %I:%M:%S %p
%R Same as %H:%M
%s Epoch time
%S Second (00-59)
%t Literal tab
%T Same as %H:%M:%S
%u Day of week (weeks start on Monday)
%U Week number
%V ISO 8601 week number
%w Day of week
%W Week number (weeks start on Monday)
%y Year of century (12)
%Y Year (2012)
%z RFC 2822 timezone
%Z Name of timezone

NAMED FORMATS

:ARPA :EMAIL :RFC-822
:ASCTIME :CTIME
:COOKIE
:HTTP :RFC-2616
:NEWS :RFC-850 :USENET
:RSS :RFC-2822 :RFC-1036 :RFC-2086
:RFC-1132
:UNIX
:W3C :HTML :RFC-3339 :XML :ATOM

There is no support for localization at this time.

Depends on CL-PPCRE and DATE-CALC, which are Quicklisp-installable.
