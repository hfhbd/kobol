plugins {
    setup
}

dependencies {
    api("com.hierynomus:sshj:0.35.0")
    api("com.jcraft:jsch.agentproxy.sshj:0.0.9") // remove stupid open net.schmizz:sshj:[0.8.1,)
    api("com.jcraft:jsch.agentproxy.pageant:0.0.9")
}

licensee {
    allow("MIT")
    allowDependency("net.java.dev.jna", "jna", "4.1.0") {
        because("Apache-2.0")
    }
    allowDependency("net.java.dev.jna", "jna-platform", "4.1.0") {
        because("Apache-2.0")
    }
    allowUrl("http://www.jcraft.com/jsch-agent-proxy/LICENSE.txt") // BSD
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") // BSD
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allowUrl("https://creativecommons.org/publicdomain/zero/1.0/")
}
