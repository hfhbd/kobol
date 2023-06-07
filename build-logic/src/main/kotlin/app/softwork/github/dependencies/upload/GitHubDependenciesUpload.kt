package app.softwork.github.dependencies.upload

import org.gradle.api.DefaultTask
import org.gradle.api.artifacts.Configuration
import org.gradle.api.artifacts.component.ModuleComponentIdentifier
import org.gradle.api.artifacts.result.ResolvedComponentResult
import org.gradle.api.artifacts.result.ResolvedDependencyResult
import org.gradle.api.model.ObjectFactory
import org.gradle.api.provider.Property
import org.gradle.api.provider.Provider
import org.gradle.api.provider.ProviderFactory
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction
import org.gradle.kotlin.dsl.property
import org.gradle.workers.WorkerExecutor
import zip
import javax.inject.Inject

@CacheableTask
abstract class GitHubDependenciesUpload
@Inject constructor(
    providers: ProviderFactory,
    objects: ObjectFactory
) : DefaultTask() {
    @get:Input
    val scope = objects.property<Scope>()

    @get:Input
    val token: Property<String> = objects.property<String>().convention(providers.environmentVariable("GITHUB_TOKEN"))

    @get:Input
    val repository: Property<String> =
        objects.property<String>().convention(providers.environmentVariable("GITHUB_REPOSITORY"))

    @get:Input
    val version: Property<Int> = objects.property<Int>().convention(0)

    @get:Input
    val sha: Property<String> = objects.property<String>().convention(providers.environmentVariable("GITHUB_SHA"))

    @get:Input
    val ref: Property<String> = objects.property<String>().convention(providers.environmentVariable("GITHUB_REF"))

    @get:Input
    val jobID: Property<String> = objects.property<String>().convention(providers.environmentVariable("GITHUB_RUN_ID"))

    @get:Input
    val projectName: Property<String> = objects.property<String>().convention(project.name)

    @get:Input
    val jobCorrelator: Property<String> = objects.property<String>().convention(
        zip(
            providers.environmentVariable("GITHUB_WORKFLOW"),
            providers.environmentVariable("GITHUB_JOB"),
            projectName,
            scope
        ) { workflow, job, projectName, scope ->
            "$workflow $job $projectName ${scope.name}"
        }
    )

    @get:Input
    val jobUrl: Property<String> = objects.property<String>().convention(
        zip(
            providers.environmentVariable("GITHUB_SERVER_URL"),
            providers.environmentVariable("GITHUB_REPOSITORY"),
            providers.environmentVariable("GITHUB_RUN_ID")
        ) { serverUrl, repository, runId ->
            "$serverUrl/$repository/actions/run/$runId"
        }
    )

    @get:Input
    val buildFileLocation = project.buildFile.toRelativeString(project.rootProject.buildFile)

    @get:Input
    val buildFileName: String = project.buildFile.name

    @get:Input
    internal abstract val resolvedComponentResult: Property<ResolvedComponentResult>

    fun uploadConfiguration(configuration: Configuration) {
        resolvedComponentResult.set(configuration.incoming.resolutionResult.rootComponent)
    }

    fun uploadConfiguration(configuration: Provider<Configuration>) {
        resolvedComponentResult.set(configuration.flatMap { it.incoming.resolutionResult.rootComponent })
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @get:OutputDirectory
    internal val outputDirectory = objects.directoryProperty().convention(
        project.layout.buildDirectory.dir("github/dependencies/${name}")
    )

    @TaskAction
    fun submit() {
        val dependencies = mutableMapOf<String, Resolved>()
        val scope = scope.get()
        for (dependency in resolvedComponentResult.get().dependencies) {
            if (dependency is ResolvedDependencyResult) {
                dependency.handle(RelationShip.Direct, scope, dependencies)
            }
        }

        workerExecutor.noIsolation().submit(UploadAction::class.java) {
            this.repository.set(this@GitHubDependenciesUpload.repository)
            this.token.set(this@GitHubDependenciesUpload.token)
            this.version.set(this@GitHubDependenciesUpload.version)
            this.sha.set(this@GitHubDependenciesUpload.sha)
            this.ref.set(this@GitHubDependenciesUpload.ref)
            this.jobID.set(this@GitHubDependenciesUpload.jobID)
            this.jobCorrelator.set(this@GitHubDependenciesUpload.jobCorrelator)
            this.jobUrl.set(this@GitHubDependenciesUpload.jobUrl)
            this.dependencies.set(dependencies)
            this.manifestFileName.set(this@GitHubDependenciesUpload.buildFileName)
            this.manifestFileLocation.set(this@GitHubDependenciesUpload.buildFileLocation)
            this.projectName.set(this@GitHubDependenciesUpload.projectName)
            this.outputDirectory.set(this@GitHubDependenciesUpload.outputDirectory)
        }
    }
}

private fun ResolvedDependencyResult.handle(
    relationShip: RelationShip,
    scope: Scope?,
    dependencies: MutableMap<String, Resolved>
) {
    val id = selected.id
    val module = id as? ModuleComponentIdentifier ?: return
    val purl = purl(module)
    val deps = selected.dependencies.mapNotNull {
        if (it is ResolvedDependencyResult) {
            val depPurl = purl(it.selected.id as ModuleComponentIdentifier)
            if (depPurl !in dependencies.keys) {
                dependencies[depPurl] = Resolved(depPurl, null, relationShip, scope, emptyList())
                it.handle(RelationShip.Indirect, scope, dependencies)
            }
            depPurl
        } else null
    }
    dependencies[purl] = Resolved(purl, null, relationShip, scope, deps)
}

// pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1
private fun purl(moduleComponentIdentifier: ModuleComponentIdentifier) =
    "pkg:maven/${moduleComponentIdentifier.group}/${moduleComponentIdentifier.module}@${moduleComponentIdentifier.version}"
