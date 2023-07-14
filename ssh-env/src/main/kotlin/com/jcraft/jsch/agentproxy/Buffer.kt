/*
Copyright (c) 2011 ymnk, JCraft,Inc. All rights reserved.

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

public class Buffer {
    final byte[] tmp = new byte[4];
    public byte[] buffer;
    int index = 0;
    int s = 0;

    public Buffer(byte[] buffer) {
        this.buffer = buffer;
    }

    public void putByte(byte foo) {
        buffer[index++] = foo;
    }

    public void putByte(byte[] foo, int begin, int length) {
        System.arraycopy(foo, begin, buffer, index, length);
        index += length;
    }

    public void putString(byte[] foo) {
        putString(foo, 0, foo.length);
    }

    public void putString(byte[] foo, int begin, int length) {
        putInt(length);
        putByte(foo, begin, length);
    }

    public void putInt(int val) {
        tmp[0] = (byte) (val >>> 24);
        tmp[1] = (byte) (val >>> 16);
        tmp[2] = (byte) (val >>> 8);
        tmp[3] = (byte) (val);
        System.arraycopy(tmp, 0, buffer, index, 4);
        index += 4;
    }

    void skip(int n) {
        index += n;
    }

    public int getLength() {
        return index - s;
    }

    public int getInt() {
        int foo = getShort();
        foo = ((foo << 16) & 0xffff0000) | (getShort() & 0xffff);
        return foo;
    }

    int getShort() {
        int foo = getByte();
        foo = ((foo << 8) & 0xff00) | (getByte() & 0xff);
        return foo;
    }

    public int getByte() {
        return (buffer[s++] & 0xff);
    }

    void getByte(byte[] foo, int len) {
        System.arraycopy(buffer, s, foo, 0, len);
        s += len;
    }

    public byte[] getString() {
        int i = getInt();  // uint32
        if (i < 0 ||  // bigger than 0x7fffffff
                i > 256 * 1024) {
            // TODO: an exception should be thrown.
            i = 256 * 1024; // the session will be broken, but working around OOME.
        }
        byte[] foo = new byte[i];
        getByte(foo, i);
        return foo;
    }

    public void reset() {
        index = 0;
        s = 0;
    }

    public void rewind() {
        s = 0;
    }

    public void checkFreeSize(int n) {
        if (buffer.length < index + n) {
            byte[] tmp = new byte[(index + n) * 2];
            System.arraycopy(buffer, 0, tmp, 0, index);
            buffer = tmp;
        }
    }

    void insertLength() {
        int length = getLength();
        System.arraycopy(buffer, 0, buffer, 4, length);
        reset();
        putInt(length);
        skip(length);
    }
}
