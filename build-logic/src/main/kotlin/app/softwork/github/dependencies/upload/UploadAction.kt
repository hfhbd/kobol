package app.softwork.github.dependencies.upload

import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.runBlocking
import kotlinx.datetime.Clock
import kotlinx.serialization.json.Json
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.provider.MapProperty
import org.gradle.api.provider.Property
import org.gradle.workers.WorkAction
import org.gradle.workers.WorkParameters

abstract class UploadAction : WorkAction<UploadAction.UploadActionParameters> {

    companion object {
        val client = HttpClient(CIO)
    }

    interface UploadActionParameters : WorkParameters {
        val repository: Property<String>
        val token: Property<String>
        val version: Property<Int>
        val sha: Property<String>
        val ref: Property<String>
        val jobID: Property<String>
        val jobCorrelator: Property<String>
        val jobUrl: Property<String>
        val dependencies: MapProperty<String, Resolved>
        val manifestFileName: Property<String>
        val manifestFileLocation: Property<String>
        val projectName: Property<String>
        val outputDirectory: DirectoryProperty
    }

    override fun execute() = runBlocking {
        val upload = Upload(
            version = parameters.version.get(),
            job = Job(
                id = parameters.jobID.get(),
                correlator = parameters.jobCorrelator.get(),
                html_url = parameters.jobUrl.orNull
            ),
            sha = parameters.sha.get(),
            ref = parameters.ref.get(),
            detector = Detector(
                name = "GradleDepsUpload",
                version = "0.0.1",
                url = "https://github.com/hfhbd/kobol"
            ),
            metadata = null,
            manifests = mapOf(
                parameters.projectName.get() to Manifest(
                    name = parameters.manifestFileName.get(),
                    file = parameters.manifestFileLocation.orNull?.let { File(it) },
                    metadata = null,
                    resolved = parameters.dependencies.get()
                )
            ),
            scanned = Clock.System.now()
        )
        val body = Json.encodeToString(Upload.serializer(), upload)
        val response = client.post(
            "https://api.github.com/repos/${parameters.repository.get()}/dependency-graph/snapshots"
        ) {
            accept(ContentType("application", "vnd.github+json"))
            bearerAuth(parameters.token.get())
            header("X-GitHub-Api-Version", "2022-11-28")
            setBody(body)
        }
        val responseText = response.bodyAsText()
        require(response.status == HttpStatusCode.Created) {
            response
        }
        println("$body $responseText")
        java.io.File(parameters.outputDirectory.asFile.get(), "response.txt").writeText(responseText)
    }
}
