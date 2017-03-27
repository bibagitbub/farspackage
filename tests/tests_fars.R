library(testthat)

expect_that(farspackage::fars_read(filename="accident_2014.csv.bz2"),is_a("tbl_df"))

expect_that(farspackage::make_filename(year=2014),is_a("character"))

expect_that(farspackage::fars_read_years(years=c(2014,2015)),is_a("list"))

expect_error(farspackage::fars_map_state(state.num="STATE",year=2014))
