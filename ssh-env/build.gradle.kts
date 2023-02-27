plugins {
    setup
    repos
}

dependencies {
    api("com.hierynomus:sshj:0.35.0")
    api("com.jcraft:jsch.agentproxy.sshj:0.0.13")
    api("com.jcraft:jsch.agentproxy.pageant:0.0.13")
}

licensee {
    allow("MIT")
    allowUrl("http://www.jcraft.com/jsch-agent-proxy/LICENSE.txt") // BSD
    allowUrl("http://www.jcraft.com/jzlib/LICENSE.txt") // BSD
    allowUrl("https://www.bouncycastle.org/licence.html") // MIT
    allowUrl("https://creativecommons.org/publicdomain/zero/1.0/")
}
