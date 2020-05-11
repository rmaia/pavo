context("import")

test_that("getspec", {

  ## Run through a bunch of file types
  avantes1 <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "ttt"))
  expect_s3_class(avantes1, "rspec")

  transmit <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "Transmission"))
  expect_s3_class(transmit, "rspec")

  jazspec <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "jaz"))
  expect_s3_class(jazspec, "rspec")

  jazirrad <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "JazIrrad"))
  expect_s3_class(jazirrad, "rspec")

  proc <- suppressMessages(getspec(system.file("testdata/procspec_files", package = "lightr"), ext = "ProcSpec"))
  expect_s3_class(proc, "rspec")
  expect_length(proc, 5)

  # getspec should ignore the case of the ext argument by default
  proccase <- getspec(system.file("testdata/procspec_files", package = "lightr"), ext = "procspec")
  expect_length(proccase, 5)
  expect_identical(proccase, proc)

  trm <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "TRM"))
  expect_s3_class(trm, "rspec")
  # avantes.TRM and avantes2.TRM don't use the same wavelengths. The import
  # should be able to handle this.
  expect_length(trm, 3)

  csv <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "csv", sep = ","))
  expect_s3_class(csv, "rspec")
  expect_length(csv, 2)

  ## Error handling
  # should fail; ROH files only have scope data, which are not imported by getspec
  expect_null(
      expect_warning(
      getspec(system.file("testdata", package = "lightr"), ext = "ROH"),
      "File import failed"
    )
  )

  # should partly succeed (1/2)
  expect_warning(getspec(system.file("testdata", package = "lightr"), ext = c("txt", "fail")), "Could not import")
  oceanview <- suppressWarnings(getspec(system.file("testdata", package = "lightr"), ext = "txt"))
  expect_s3_class(oceanview, "rspec")

  # should fail if ignore.case is set to FALSE and user don't use correct case
  expect_warning(
    getspec(system.file("testdata/procspec_files", package = "lightr"), ext = "procspec", ignore.case = FALSE),
    "No files found."
  )

  skip_if_not_installed("lightr", "1.1")
  irr <- suppressMessages(getspec(system.file("testdata", package = "lightr"), ext = "IRR"))
  expect_s3_class(irr, "rspec")

  non_EN <- suppressMessages(getspec(system.file("testdata/non_english", package = "lightr"), ext = "txt", decimal = ","))
  expect_s3_class(non_EN, "rspec")
  
})


test_that("getimg", {
  expect_s3_class(
    getimg(system.file("testdata", "images", "formats", package = "pavo")),
    "rimg"
  )
})
