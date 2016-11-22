context("wsXXX and wrXXX functions OUTPUT")

test_that("wsVal() results",{
  data(WSlit)
  ## Do Bluegill results match
  bg1 <- wsVal("Bluegill")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="metric",]
  bg2 <- bg2[,-which(names(bg2) %in% c("max.len","quad","comment"))]
  expect_equivalent(bg1,bg2)
  bg1 <- wsVal("Bluegill",units="English")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,-which(names(bg2) %in% c("max.len","quad","comment"))]
  expect_equivalent(bg1,bg2)
  bg1 <- wsVal("Bluegill",units="English",simplify=TRUE)
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,which(names(bg2) %in% c("species","min.len","int","slope"))]
  expect_equivalent(bg1,bg2)
  ## Do Ruffe results match
  ruf1 <- wsVal("Ruffe")
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  expect_equivalent(ruf1,ruf2)
  ruf1 <- wsVal("Ruffe",simplify=TRUE)
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[,which(names(ruf2) %in% c("species","min.len","max.len","int","slope","quad"))]
  expect_equivalent(ruf1,ruf2)
  ##
  expect_message(capture.output(wsVal("List")),"must be one of following")
  expect_output(suppressMessages(wsVal("List")))
})
