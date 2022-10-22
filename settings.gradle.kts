rootProject.name = "kobol"

enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

include(":kobol-lexer-parser")
include(":kobol-fir")
include(":kobol-ir")

include(":kobol-kotlin")
include(":kobol-sqldelight-precompiler")

include(":kobol-java")
include(":kobol-jdbc")

include(":kobol-gradle-plugin")
include(":kobol-intellij-plugin")

include(":kobol-flow-graph")
