require(gWidgets)
options("guiToolkit"="tcltk")

files <- list.files(system.file("tests",package="gWidgets"),
                    pattern = "\\.R$",
                    full.names = TRUE)


for(unitTest in files) {
  source(unitTest)
}
