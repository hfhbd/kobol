package app.softwork.kobol.compiler

/*
 * Copyright (C) 2017 Square, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.intellij.core.*
import com.intellij.lang.*
import com.intellij.openapi.diagnostic.*
import com.intellij.openapi.extensions.*
import com.intellij.openapi.project.*
import com.intellij.openapi.roots.*
import com.intellij.openapi.roots.impl.*
import com.intellij.openapi.util.*
import com.intellij.openapi.vfs.*
import com.intellij.psi.*
import com.intellij.psi.impl.smartPointers.*
import java.io.*
import java.util.concurrent.atomic.*

private object ApplicationEnvironment {
  private val logger = object : DefaultLogger("") {
    override fun warn(message: String?, t: Throwable?) = Unit
    override fun error(message: Any?) = Unit
  }
  var initialized = AtomicBoolean(false)

  val coreApplicationEnvironment: CoreApplicationEnvironment by lazy(mode = LazyThreadSafetyMode.SYNCHRONIZED) {
    CoreApplicationEnvironment(Disposer.newDisposable()).apply {
      Logger.setFactory { logger }

      CoreApplicationEnvironment.registerExtensionPoint(
        Extensions.getRootArea(),
        MetaLanguage.EP_NAME, MetaLanguage::class.java
      )
      CoreApplicationEnvironment.registerExtensionPoint(
        Extensions.getRootArea(), SmartPointerAnchorProvider.EP_NAME,
        SmartPointerAnchorProvider::class.java
      )
    }
  }
}

class CoreEnvironment(sourceFolders: List<File>) {
  private val fileIndex: CoreFileIndex

  private val projectEnvironment = CoreProjectEnvironment(
    ApplicationEnvironment.coreApplicationEnvironment.parentDisposable,
    ApplicationEnvironment.coreApplicationEnvironment
  )

  private val localFileSystem = VirtualFileManager.getInstance().getFileSystem(
    StandardFileSystems.FILE_PROTOCOL
  )

  init {
    projectEnvironment.registerProjectComponent(
      ProjectRootManager::class.java,
      ProjectRootManagerImpl(projectEnvironment.project)
    )

    projectEnvironment.project.registerService(
      DirectoryIndex::class.java,
      DirectoryIndexImpl(projectEnvironment.project)
    )

    fileIndex = CoreFileIndex(sourceFolders, localFileSystem, projectEnvironment.project)
    projectEnvironment.project.registerService(ProjectFileIndex::class.java, fileIndex)
  }

  fun<T: PsiFile> forSourceFiles(action: (T) -> Unit) {
    val psiManager = PsiManager.getInstance(projectEnvironment.project)
    fileIndex.iterateContent { file ->
      val psiFile = psiManager.findFile(file) as? T ?: return@iterateContent true
      action(psiFile)
      return@iterateContent true
    }
  }

  fun initializeApplication(block: CoreApplicationEnvironment.() -> Unit) {
    if (!ApplicationEnvironment.initialized.getAndSet(true)) {
      ApplicationEnvironment.coreApplicationEnvironment.block()
    }
  }
}

private class CoreFileIndex(
  val sourceFolders: List<File>,
  private val localFileSystem: VirtualFileSystem,
  project: Project
) : ProjectFileIndexImpl(project) {
  override fun iterateContent(iterator: ContentIterator): Boolean {
    return sourceFolders.all {
      val file = localFileSystem.findFileByPath(it.absolutePath)
        ?: throw NullPointerException("File ${it.absolutePath} not found")
      iterateContentUnderDirectory(file, iterator)
    }
  }

  override fun iterateContentUnderDirectory(file: VirtualFile, iterator: ContentIterator): Boolean {
    if (file.isDirectory) {
      file.children.forEach { if (!iterateContentUnderDirectory(it, iterator)) return false }
      return true
    }
    return iterator.processFile(file)
  }
}
