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
// Changes by hfhbd: Refactor to Kotlin

package com.jcraft.jsch.agentproxy

internal class Buffer(var buffer: ByteArray) {
    private val tmp = ByteArray(4)
    private var index = 0
    private var s = 0

    fun putByte(foo: Byte) {
        buffer[index++] = foo
    }

    private fun putByte(foo: ByteArray, begin: Int, length: Int) {
        System.arraycopy(foo, begin, buffer, index, length)
        index += length
    }

    fun putString(foo: ByteArray, begin: Int = 0, length: Int = foo.size) {
        putInt(length)
        putByte(foo, begin, length)
    }

    fun putInt(value: Int) {
        tmp[0] = (value ushr 24).toByte()
        tmp[1] = (value ushr 16).toByte()
        tmp[2] = (value ushr 8).toByte()
        tmp[3] = value.toByte()
        System.arraycopy(tmp, 0, buffer, index, 4)
        index += 4
    }

    private fun skip(n: Int) {
        index += n
    }

    val length: Int get() = index - s
    val int: Int
        get() {
            var foo = short
            foo = foo shl 16 and -0x10000 or (short and 0xffff)
            return foo
        }

    private val short: Int
        get() {
            var foo = byte
            foo = foo shl 8 and 0xff00 or (byte and 0xff)
            return foo
        }

    val byte: Int
        get() = buffer[s++].toInt() and 0xff

    private fun getByte(foo: ByteArray, len: Int) {
        System.arraycopy(buffer, s, foo, 0, len)
        s += len
    }

    val string: ByteArray
        get() {
            var i = int // uint32
            if (i < 0 || // bigger than 0x7fffffff
                i > 256 * 1024
            ) {
                i = 256 * 1024 // the session will be broken, but working around OOME.
            }
            val foo = ByteArray(i)
            getByte(foo, i)
            return foo
        }

    fun reset() {
        index = 0
        s = 0
    }

    fun rewind() {
        s = 0
    }

    fun checkFreeSize(n: Int) {
        if (buffer.size < index + n) {
            val tmp = ByteArray((index + n) * 2)
            System.arraycopy(buffer, 0, tmp, 0, index)
            buffer = tmp
        }
    }

    fun insertLength() {
        val length = length
        System.arraycopy(buffer, 0, buffer, 4, length)
        reset()
        putInt(length)
        skip(length)
    }
}
