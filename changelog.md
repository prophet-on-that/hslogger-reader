# 1.0.3 - 14-Aug-2017

* Add --print-unparsable option in `filter-logs` to print unparsable
  lines to stderr.

# 1.0.2 - 24-Oct-2016

* Now skipping unparsable lines in `filter-logs` by default. Previous
  behaviour may be recovered with --no-continue option;
* Supporting GHC 8.0.1 and [lts-7.0](https://www.stackage.org/lts-7.0).

# 1.0.1

* Supporting base == 4.8.* and time == 1.5.*, and providing an
  `old-locale' flag in order to build with time < 1.5.
