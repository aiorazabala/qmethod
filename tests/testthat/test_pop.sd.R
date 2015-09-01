# Test the population standard deviation function
context(desc = "Population standard deviation helper function")

testdata <- rnorm(100)

test_that(
  desc = "population standard deviation differs from sample standard deviation",
  code = {
    expect_less_than(object = pop.sd(testdata), expected = sd(testdata))
  }
)

test_that(
  desc = "population standard deviation delives correct result on example data",
  code = {
    # just a random example from http://stackoverflow.com/questions/6457755/standard-deviation-in-r-seems-to-be-returning-the-wrong-answer-am-i-doing-some
    expect_equal(object = pop.sd(c(2,4,4,4,5,5,7,9)), expected = 2)
  }
)
