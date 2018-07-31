library(pavo)
context("import")

test_that("getspec", {

  ## Run through a bunch of file types
  avantes1 <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "ttt"))
  expect_is(avantes1, "rspec")

  avantes2 <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "trt"))
  expect_is(avantes2, "rspec")

  transmit <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "Transmission"))
  expect_is(transmit, "rspec")

  irr <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "IRR"))
  expect_is(irr, "rspec")

  jazspec <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "jaz"))
  expect_is(jazspec, "rspec")

  jazirrad <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "JazIrrad"))
  expect_is(jazirrad, "rspec")

  proc <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "ProcSpec"))
  expect_is(proc, "rspec")
  expect_length(proc, 4)

  # getspec should ignore the case of the ext argument by default
  proccase <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "procspec"))
  expect_identical(proccase, proc)

  trm <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "TRM"))
  expect_is(trm, "rspec")
  # avantes.TRM and avantes2.TRM don't use the same wavelengths. The import
  # should be able to handle this.
  expect_length(trm, 3)

  # Test csv import and scientific format regex
  csv <- suppressMessages(getspec(system.file("testdata", package = "pavo"), ext = "csv", sep = ","))
  expect_is(csv, "rspec")
  expect_length(csv, 3)

  ## Error handling
  # should fail completely; ROH files only have scope data, which are not imported by getspec
  expect_error(getspec(system.file("testdata", package = "pavo"), ext = "ROH"),
               "Could not import spectra")

  # should partly succeed (1/2)
  expect_warning(getspec(system.file("testdata", package = "pavo"), ext = "txt"), "Could not import")
  oceanview <- suppressWarnings(getspec(system.file("testdata", package = "pavo"), ext = "txt"))
  expect_is(oceanview, "rspec")

  # should fail if ignore.case is set to FALSE and user don't use correct case
  expect_error(getspec(system.file("testdata", package = "pavo"), ext = "procspec", ignore.case = FALSE),
               "No files found.")
})
