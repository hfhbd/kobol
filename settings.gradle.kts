rootProject.name = "CobolIDEA"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

include(":lexer-parser")
include(":compiler")
include(":gradle-plugin")
include(":intellij-plugin")
