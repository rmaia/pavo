library(pavo)
context('general')

test_that('Class assignment', {
  data(flowers)
  
  # Check rspec
  flowers <- as.rspec(as.data.frame(flowers))
  expect_is(flowers, "rspec")
  
  # Check vismodel
  vis.flowers <- vismodel(flowers, visual = 'apis')
  expect_is(vis.flowers, "vismodel")
  
  # Check a few colorspaces
  vis.cie <- vismodel(flowers, vonkries = TRUE, relative = FALSE, achro = 'none', visual = 'cie10')
  col.cie <- colspace(vis.cie, space = 'cielab')
  expect_is(col.cie, 'colspace')
  
  vis.tcs <- vismodel(flowers, visual = 'bluetit')
  col.tcs <- colspace(vis.tcs, space = 'tcs')
  expect_is(col.cie, 'colspace')
  
  vis.hex <- vismodel(flowers, relative = FALSE, qcatch = 'Ei', vonkries = TRUE, visual = 'apis')
  col.hex <- colspace(vis.hex, space = 'hex')
  expect_is(col.hex, 'colspace')
})