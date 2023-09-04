package com.jcraft.jsch.agentproxy

import com.jcraft.jsch.agentproxy.connector.PageantConnector
import com.jcraft.jsch.agentproxy.sshj.AuthAgent
import net.schmizz.sshj.userauth.method.AbstractAuthMethod

public object PAgentProxy {
    public fun authAgents(): List<AbstractAuthMethod> {
        val proxy = AgentProxy(PageantConnector())
        val authAgents = proxy.identities?.map {
            AuthAgent(proxy, it)
        }
        return authAgents ?: emptyList()
    }
}
