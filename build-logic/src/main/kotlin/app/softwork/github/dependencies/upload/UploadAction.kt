package app.softwork.github.dependencies.upload

import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.runBlocking
import kotlinx.datetime.Clock
import kotlinx.serialization.json.Json
import org.gradle.api.artifacts.component.ModuleComponentIdentifier
import org.gradle.api.artifacts.result.ResolvedComponentResult
import org.gradle.api.artifacts.result.ResolvedDependencyResult
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.provider.Property
import org.gradle.workers.WorkAction
import org.gradle.workers.WorkParameters

abstract class UploadAction : WorkAction<UploadAction.UploadActionParameters> {
    interface UploadActionParameters : WorkParameters {
        val repository: Property<String>
        val token: Property<String>
        val scope: Property<Scope>
        val version: Property<Int>
        val sha: Property<String>
        val ref: Property<String>
        val jobID: Property<String>
        val jobCorrelator: Property<String>
        val jobUrl: Property<String>
        val dependencies: Property<ResolvedComponentResult>
        val manifestFileName: Property<String>
        val manifestFileLocation: Property<String>
        val projectName: Property<String>
        val outputDirectory: DirectoryProperty
    }

    // pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1
    private fun PackageUrl(moduleComponentIdentifier: ModuleComponentIdentifier) =
        "pkg:maven/${moduleComponentIdentifier.group}/${moduleComponentIdentifier.module}@${moduleComponentIdentifier.version}"


    private fun ResolvedDependencyResult.handle(
        relationShip: RelationShip,
        scope: Scope?,
        dependencies: MutableMap<String, Resolved>
    ) {
        val id = selected.id
        val module = id as? ModuleComponentIdentifier ?: return
        val purl = PackageUrl(module)
        val deps = selected.dependencies.mapNotNull {
            if (it is ResolvedDependencyResult) {
                val depPurl = PackageUrl(it.selected.id as ModuleComponentIdentifier)
                if (depPurl !in dependencies.keys) {
                    dependencies[depPurl] = Resolved(depPurl, null, relationShip, scope, emptyList())
                    it.handle(RelationShip.Indirect, scope, dependencies)
                }
                depPurl
            } else null
        }
        dependencies[purl] = Resolved(purl, null, relationShip, scope, deps)
    }

    override fun execute() = runBlocking {
        val client = HttpClient(CIO)
        val resolved = parameters.dependencies.get()

        val dependencies = mutableMapOf<String, Resolved>()
        val scope = parameters.scope.get()
        for (dependency in resolved.dependencies) {
            if (dependency is ResolvedDependencyResult) {
                dependency.handle(RelationShip.Direct, scope, dependencies)
            }
        }

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
                url = ""
            ),
            metadata = null,
            manifests = mapOf(
                parameters.projectName.get() to Manifest(
                    name = parameters.manifestFileName.get(),
                    file = parameters.manifestFileLocation.orNull?.let { File(it) },
                    metadata = null,
                    resolved = dependencies
                )
            ),
            scanned = Clock.System.now()
        )
        val response = client.post(
            "https://api.github.com/repos/${parameters.repository.get()}/dependency-graph/snapshots"
        ) {
            accept(ContentType("application", "vnd.github+json"))
            bearerAuth(parameters.token.get())
            header("X-GitHub-Api-Version", "2022-11-28")
            setBody(Json.encodeToString(Upload.serializer(), upload))
        }
        require(response.status == HttpStatusCode.Created) {
            response.bodyAsText()
        }
        java.io.File(parameters.outputDirectory.asFile.get(), "response.txt").writeText(response.bodyAsText())
    }
}
