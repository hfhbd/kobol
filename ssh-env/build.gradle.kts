plugins {
    id("kotlinSetup")
}

dependencies {
    api("com.hierynomus:sshj:0.35.0")

    implementation("net.java.dev.jna:jna:5.13.0")
    implementation("net.java.dev.jna:jna-platform:5.13.0")
}

licensee {
    allow("MIT")
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allow("CC0-1.0")
}
