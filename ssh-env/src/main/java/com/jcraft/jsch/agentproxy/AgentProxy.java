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

package com.jcraft.jsch.agentproxy;

import com.jcraft.jsch.agentproxy.connector.PageantConnector;

public class AgentProxy {
    private static final byte SSH_AGENT_FAILURE = 5;

    private static final byte SSH2_AGENTC_REQUEST_IDENTITIES = 11;
    private static final byte SSH2_AGENTC_SIGN_REQUEST = 13;

    private final byte[] buf = new byte[1024];
    private final Buffer buffer = new Buffer(buf);

    private final PageantConnector connector;

    public AgentProxy(PageantConnector connector) {
        this.connector = connector;
    }

    public synchronized Identity[] getIdentities() {
        buffer.reset();
        buffer.putByte(SSH2_AGENTC_REQUEST_IDENTITIES);
        buffer.insertLength();

        try {
            connector.query(buffer);
        } catch (AgentProxyException e) {
            buffer.rewind();
            buffer.putByte(SSH_AGENT_FAILURE);
            return new Identity[0];
        }

        buffer.getByte();

        int count = buffer.getInt();

        Identity[] identities = new Identity[count];

        for (int i = 0; i < identities.length; i++) {
            identities[i] = new Identity(buffer.getString(), buffer.getString());
        }

        return identities;
    }

    public synchronized byte[] sign(byte[] blob, byte[] data) {
        int required_size = 1 + 4 * 4 + blob.length + data.length;
        buffer.reset();
        buffer.checkFreeSize(required_size);
        buffer.putByte(SSH2_AGENTC_SIGN_REQUEST);
        buffer.putString(blob);
        buffer.putString(data);
        buffer.putInt(0);
        buffer.insertLength();

        try {
            connector.query(buffer);
        } catch (AgentProxyException e) {
            buffer.rewind();
            buffer.putByte(SSH_AGENT_FAILURE);
        }
        buffer.getByte();

        return buffer.getString();
    }
}
