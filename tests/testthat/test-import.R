library(pavo)
context('import')

test_that('getspec', {
  
## Run through a bunch of file types
  avantes1 <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'ttt'))
  expect_is(avantes1, "rspec")
  
  transmit <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'Transmission'))
  expect_is(transmit, "rspec")
  
  irr <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'IRR'))
  expect_is(irr, "rspec")
  
  jazspec <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'jaz'))
  expect_is(jazspec, "rspec")
  
  jazirrad <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'JazIrrad'))
  expect_is(jazirrad, "rspec")
  
## Error handling
  # should fail completely
  expect_error(getspec(system.file("testdata", package = 'pavo'), ext = 'TRM', sep = ''))  
  
  # should partly succeed (1/2)
  expect_warning(getspec(system.file("testdata", package = 'pavo'), ext = 'txt'), 'Could not import')
  oceanview <- suppressWarnings(getspec(system.file("testdata", package = 'pavo'), ext = 'txt'))  
  expect_is(oceanview, "rspec")
  
})