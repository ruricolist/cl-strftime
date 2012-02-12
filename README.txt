CL-STRFTIME is a Common Lisp compiler for the strftime language. It is
not an interface to a foreign function, but a re-implementation from
scratch.

Two functions are exported: FORMAT-TIME and MAKE-TIME-FORMATTER.

FORMAT-TIME takes a stream, a format, and a time. The format is
compiled and applied to the time, writing to the stream.

     (format-time t "%D" (get-universal-time))
     => 02/12/12

If the format argument to FORMAT-TIME is a constant, it is compiled in
advance. Otherwise, MAKE-TIME-FORMATTER returns a function to pass as
the format argument to FORMAT-TIME.

The following flags and directives are supported:

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
%Z Timezone

There is no support for localization at this time.

Depends on DATE-CALC, which is Quicklisp-installable.
