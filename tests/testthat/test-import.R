library(pavo)
context('import')

test_that('getspec', {
  
  # Run through a bunch of file types
  avantes1 <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'ttt'))
  expect_is(avantes1, "rspec")
  
  # avantes2 <- getspec(system.file("testdata", package = 'pavo'), ext = 'TRM', sep = '')
  # expect_is(avantes2, "rspec")
  # 
  # craic <- getspec(system.file("testdata", package = 'pavo'), ext = 'spc', sep = '')
  # expect_is(craic, "rspec")
  
  oceanview <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'txt'))
  expect_is(oceanview, "rspec")
  
  transmit <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'Transmission'))
  expect_is(transmit, "rspec")
  
  irr <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'IRR'))
  expect_is(irr, "rspec")
  
  jazspec <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'jaz'))
  expect_is(jazspec, "rspec")
  
  jazirrad <- suppressMessages(getspec(system.file("testdata", package = 'pavo'), ext = 'JazIrrad'))
  expect_is(jazirrad, "rspec")
  
})