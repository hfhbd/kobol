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

package com.jcraft.jsch.agentproxy.connector

import com.jcraft.jsch.agentproxy.AgentProxyException
import com.jcraft.jsch.agentproxy.Buffer
import com.sun.jna.Memory
import com.sun.jna.Native
import com.sun.jna.Pointer
import com.sun.jna.Structure
import com.sun.jna.platform.win32.*
import com.sun.jna.platform.win32.WinDef.HWND
import com.sun.jna.platform.win32.WinDef.WPARAM
import com.sun.jna.platform.win32.WinNT.PAGE_READWRITE
import com.sun.jna.platform.win32.WinNT.SECTION_MAP_WRITE
import com.sun.jna.win32.W32APIOptions

internal class PageantConnector {
    private val libU = User32.INSTANCE
    private val libK = Kernel32.INSTANCE

    override fun toString(): String = "pageant"

    internal interface User32 : com.sun.jna.platform.win32.User32 {
        fun SendMessage(hWnd: HWND, msg: Int, num1: WPARAM?, num2: ByteArray): Long

        companion object {
            internal val INSTANCE = Native.loadLibrary(
                "user32",
                User32::class.java,
                W32APIOptions.DEFAULT_OPTIONS
            )
        }
    }

    internal class COPYDATASTRUCT64 : Structure() {
        @JvmField
        var dwData = 0
        @JvmField
        var cbData: Long = 0
        @JvmField
        var lpData: Pointer? = null
        override fun getFieldOrder(): List<String> = listOf("dwData", "cbData", "lpData")
    }

    fun query(buffer: Buffer) {
        val hwnd = libU.FindWindow("Pageant", "Pageant")
            ?: throw AgentProxyException("Pageant is not runnning.", null)
        val mapname = String.format("PageantRequest%08x", libK.GetCurrentThreadId())
        val sharedFile = libK.CreateFileMapping(
            WinBase.INVALID_HANDLE_VALUE,
            null,
            PAGE_READWRITE,
            0,
            8192,  // AGENT_MAX_MSGLEN
            mapname
        )
        val sharedMemory = Kernel32.INSTANCE.MapViewOfFile(
            sharedFile,
            SECTION_MAP_WRITE,
            0, 0, 0
        )
        try {
            sharedMemory.write(0, buffer.buffer, 0, buffer.length)
            val cds64 = COPYDATASTRUCT64()
            val data = install64(mapname, cds64)
            val rcode = sendMessage(hwnd, data)
            buffer.rewind()
            if (rcode != 0L) {
                sharedMemory.read(0, buffer.buffer, 0, 4) // length
                val i = buffer.int
                buffer.rewind()
                buffer.checkFreeSize(i)
                sharedMemory.read(4, buffer.buffer, 0, i)
            }
        } finally {
            libK.UnmapViewOfFile(sharedMemory)
            libK.CloseHandle(sharedFile)
        }
    }

    private fun install64(mapname: String, cds: COPYDATASTRUCT64): ByteArray {
        cds.dwData = -0x7fb1af46 // AGENT_COPYDATA_ID
        cds.cbData = (mapname.length + 1).toLong()
        cds.lpData = Memory((mapname.length + 1).toLong())
        val foo = mapname.toByteArray()
        (cds.lpData as Memory).write(0, foo, 0, foo.size)
        (cds.lpData as Memory).setByte(foo.size.toLong(), 0.toByte())
        cds.write()
        val data = ByteArray(24)
        val cdsp = cds.pointer
        cdsp.read(0, data, 0, 24)
        return data
    }

    fun sendMessage(hwnd: HWND, data: ByteArray): Long = libU.SendMessage(
        hwnd,
        0x004A,  //WM_COPYDATA
        null,
        data
    )
}
