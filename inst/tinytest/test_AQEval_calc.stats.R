# AQEval data

if(at_home()){
  test <- calcDateRangeStat(AQEval::aq.data, 2001, 2002, pollutant="no2")
  expect_equal(as.numeric(test[1,3]), 40.4101, tolerance = 0.01)
  test <- calcDateRangeStat(AQEval::aq.data, 2001, 2002, pollutant="no2",
                            stat=function(x){max(x, na.rm=TRUE)})
  expect_equal(as.numeric(test[1,3]), 220.265975, tolerance = 0.01)
  test <- calcRollingDateRangeStat(AQEval::aq.data, res="2 year",method=1)
  ref <- calcRollingDateRangeStat(AQEval::aq.data, res="2 year",method=2)
  expect_equivalent(test, ref)
  ref <- calcRollingDateRangeStat(AQEval::aq.data, res="2 year",method=3)
  expect_equivalent(test, ref)
}
