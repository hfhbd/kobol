/*
Copyright (c) 2012 ymnk, JCraft,Inc. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the distribution.

  3. The names of the authors may not be used to endorse or promote products
     derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JCRAFT,
INC. OR ANY CONTRIBUTORS TO THIS SOFTWARE BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// https://github.com/ymnk/jsch-agent-proxy
// Changes by hfhbd: Refactor to Kotlin

package com.jcraft.jsch.agentproxy

import com.jcraft.jsch.agentproxy.connector.PageantConnector

internal class AgentProxy(private val connector: PageantConnector) {
    val identities: List<Identity>?
        get() {
            val buf = ByteArray(1024)
            val buffer = Buffer(buf)
            buffer.putByte(SSH2_AGENTC_REQUEST_IDENTITIES)
            buffer.insertLength()
            try {
                connector.query(buffer)
            } catch (_: AgentProxyException) {
                buffer.rewind()
                buffer.putByte(SSH_AGENT_FAILURE)
                return null
            }
            buffer.byte
            val count = buffer.int
            val identities = List(count) {
                Identity(buffer.string, buffer.string)
            }
            return identities
        }

    fun sign(blob: ByteArray, data: ByteArray): ByteArray {
        val requiredSize = 1 + 4 * 4 + blob.size + data.size
        val buf = ByteArray(requiredSize)
        val buffer = Buffer(buf)
        buffer.putByte(SSH2_AGENTC_SIGN_REQUEST)
        buffer.putString(blob)
        buffer.putString(data)
        buffer.putInt(0)
        buffer.insertLength()
        try {
            connector.query(buffer)
        } catch (_: AgentProxyException) {
            buffer.rewind()
            buffer.putByte(SSH_AGENT_FAILURE)
        }
        buffer.byte
        return buffer.string
    }

    companion object {
        private const val SSH_AGENT_FAILURE: Byte = 5
        private const val SSH2_AGENTC_REQUEST_IDENTITIES: Byte = 11
        private const val SSH2_AGENTC_SIGN_REQUEST: Byte = 13
    }
}
