plugins {
    id("kotlinSetup")
}

dependencies {
    api("com.hierynomus:sshj:0.35.0")

    implementation("net.java.dev.jna:jna:5.14.0")
    implementation("net.java.dev.jna:jna-platform:5.14.0")
}

licensee {
    allow("MIT")
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") {
        because("BSD")
    }
    allowUrl("https://www.bouncycastle.org/licence.html") {
        because("MIT")
    }
    allow("CC0-1.0")
}
