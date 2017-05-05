library(pavo)
context('coldist')

test_that('Errors', {
  data(flowers)
  
  expect_error(coldist(vismodel(flowers), v=0.2), "deprecated")
  expect_error(coldist(vismodel(flowers), n1=1), "deprecated")
  expect_error(coldist(vismodel(flowers), n2=2), "deprecated")
  expect_error(coldist(vismodel(flowers), n3=2), "deprecated")
  expect_error(coldist(vismodel(flowers), n4=4), "deprecated")
  
  expect_error(coldist(vismodel(flowers, relative=FALSE), n=c(1,2,3,4,5)), "different length")
  expect_error(coldist(vismodel(flowers, relative=FALSE), n=c(1,2,3)), "different length")
})

test_that('Equivalent', {
  data(flowers)
  
  expect_equal(
    coldist(vismodel(flowers, relative=FALSE), weber.ref='longest'), 
	coldist(vismodel(flowers, relative=FALSE), weber.ref=4)
	)
	
  expect_equal(
    coldist(vismodel(flowers, relative=FALSE), weber.ref='longest'), 
    coldist(vismodel(flowers, relative=FALSE), weber.ref=4)
    )

  expect_equal(
    coldist(vismodel(flowers, relative=FALSE), achro=FALSE), 
    suppressWarnings(coldist(colspace(vismodel(flowers, relative=FALSE)), achro=FALSE)),
    check.attributes = FALSE)	

})