package app.softwork.github.dependencies.upload

import org.gradle.api.DefaultTask
import org.gradle.api.artifacts.Configuration
import org.gradle.api.artifacts.result.ResolvedDependencyResult
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.model.ObjectFactory
import org.gradle.api.provider.ListProperty
import org.gradle.api.provider.Property
import org.gradle.api.provider.Provider
import org.gradle.api.provider.ProviderFactory
import org.gradle.api.tasks.CacheableTask
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction
import org.gradle.kotlin.dsl.property
import org.gradle.workers.WorkerExecutor
import org.jetbrains.grammarkit.zip
import javax.inject.Inject

@CacheableTask
abstract class GitHubDependenciesUpload
@Inject constructor(
    providers: ProviderFactory,
    objects: ObjectFactory
) : DefaultTask() {
    @get:Input
    abstract val scope: Property<Scope>

    @get:Input
    abstract val token: Property<String>

    @Input
    val repository = objects.property<String>().convention(providers.environmentVariable("GITHUB_REPOSITORY"))

    @Input
    val version = objects.property<Int>().convention(0)

    @Input
    val sha = objects.property<String>().convention(providers.environmentVariable("GITHUB_SHA"))

    @Input
    val ref = objects.property<String>().convention(providers.environmentVariable("GITHUB_REF"))

    @Input
    val jobID = objects.property<String>().convention(providers.environmentVariable("GITHUB_RUN_ID"))

    @Input
    val jobCorrelator = objects.property<String>().convention(
        providers.environmentVariable("GITHUB_WORKFLOW")
            .zip(providers.environmentVariable("GITHUB_JOB")) { workflow, job ->
                "$workflow $job"
            }
    )

    @Input
    val jobUrl = objects.property<String>().convention(
        providers.environmentVariable("GITHUB_SERVER_URL").zip(
            providers.environmentVariable("GITHUB_REPOSITORY"),
            providers.environmentVariable("GITHUB_RUN_ID")
        ) { serverUrl, repository, runId ->
            "$serverUrl/$repository/actions/run/$runId"
        }
    )

    @Input
    val buildFileLocation = project.buildFile.toRelativeString(project.rootProject.buildFile)

    @Input
    val buildFileName = project.buildFile.name

    @Input
    val projectName = project.name

    @get:Input
    internal abstract val resolvedComponentResult: ListProperty<ResolvedDependencyResult>

    fun uploadConfiguration(configuration: Configuration) {
        resolvedComponentResult.set(configuration.incoming.resolutionResult.rootComponent.map {
            it.dependencies.mapNotNull {
                it as? ResolvedDependencyResult
            }
        })
    }

    fun uploadConfiguration(configuration: Provider<Configuration>) {
        resolvedComponentResult.set(configuration.flatMap { it.incoming.resolutionResult.rootComponent }.map {
            it.dependencies.mapNotNull {
                it as? ResolvedDependencyResult
            }
        })
    }

    @get:Inject
    internal abstract val workerExecutor: WorkerExecutor

    @get:OutputDirectory
    internal abstract val outputDirectory: DirectoryProperty

    init {
        outputDirectory.convention(
            project.layout.buildDirectory.dir("github/dependencies/${name}")
        )
    }

    @TaskAction
    fun submit() {
        workerExecutor.noIsolation().submit(UploadAction::class.java) {
            this.repository.set(this@GitHubDependenciesUpload.repository)
            this.token.set(this@GitHubDependenciesUpload.token)
            this.scope.set(this@GitHubDependenciesUpload.scope)
            this.version.set(this@GitHubDependenciesUpload.version)
            this.sha.set(this@GitHubDependenciesUpload.sha)
            this.ref.set(this@GitHubDependenciesUpload.ref)
            this.jobID.set(this@GitHubDependenciesUpload.jobID)
            this.jobCorrelator.set(this@GitHubDependenciesUpload.jobCorrelator)
            this.jobUrl.set(this@GitHubDependenciesUpload.jobUrl)
            this.dependencies.set(this@GitHubDependenciesUpload.resolvedComponentResult)
            this.manifestFileName.set(this@GitHubDependenciesUpload.buildFileName)
            this.manifestFileLocation.set(this@GitHubDependenciesUpload.buildFileLocation)
            this.projectName.set(this@GitHubDependenciesUpload.projectName)
            this.outputDirectory.set(this@GitHubDependenciesUpload.outputDirectory)
        }
    }
}
