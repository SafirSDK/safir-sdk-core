/* Calling isUnix() in multiple places in the scripts make the output very ugly, so we make our own
   version of the function. */
def betterIsUnix() {
    return env.SystemRoot == null
}

def runCommand(Map map) {
    def command = map.command
    if (betterIsUnix()) {
        if (map.linux_arguments != null)
            command = command + " " + map.linux_arguments
        sh (script: command, label: "Running (through sh) " + command)
    }
    else {
        command = command.replaceAll("/","\\\\")
        if (map.windows_arguments != null)
            command = command + " " + map.windows_arguments
        bat (script: command, label: "Running (through bat) " + command)
    }
}

def runPython(Map map) {
    map.command = "python " + map.command
    runCommand(map)
}


def clean_check_and_build(platform, arch) {
    runCommand (command: "git clean -fxd")
    runPython (command: "build/check_source_tree.py")
    runPython (command: "build/build.py --jenkins --package",
                windows_arguments: "--use-studio ${platform} --arch ${arch}")
}

def archive_and_analyze(platform, arch, buildType){
    def buildIdentifier = "${platform}-${arch}-${buildType}"

    catchError {
        if (betterIsUnix()) {
            sh label: "Moving artifacts to build-${buildIdentifier}.",
               script: """
                       mkdir build-${buildIdentifier}
                       mv buildlog.html build-${buildIdentifier}
                       mv tmp/*.deb build-${buildIdentifier}
                       """
        }
        else {
            bat label: "Moving artifacts to build-${buildIdentifier}.",
                script: """
                        md build-${buildIdentifier}
                        move buildlog.html build-${buildIdentifier}
                        move build\\packaging\\windows\\*.exe build-${buildIdentifier}
                        """
        }
    }

    // create a zip file that contains any output files left behind by tests. For this to work
    // the directory containing the files must be named exactly 'test_output'
    zip zipFile: "build-${buildIdentifier}/test-output.zip",
        glob: "**/test_output/**"

    archiveArtifacts artifacts: "**/buildlog.html, build-${buildIdentifier}/*.deb, build-${buildIdentifier}/*.exe, build-${buildIdentifier}/*.zip",
                     fingerprint: true, onlyIfSuccessful: false

    def cmake = scanForIssues (
        tool: cmake(pattern:"**/buildlog.html",
                    id:"cmake_${buildIdentifier}",
                    name:"CMake ${buildIdentifier}"),
        sourceCodeEncoding: 'UTF-8'
    )
    def java = scanForIssues (
        tool: java(pattern:"**/buildlog.html",
                   id:"java_${buildIdentifier}",
                   name:"Java ${buildIdentifier}"),
        sourceCodeEncoding: 'UTF-8'
    )
    def doxygen = scanForIssues (
        tool: doxygen(pattern:"**/buildlog.html",
                      id:"doxygen_${buildIdentifier}",
                      name:"Doxygen ${buildIdentifier}"),
        sourceCodeEncoding: 'UTF-8'
    )

    issueList = [cmake, java, doxygen]

    if (betterIsUnix()) {
        def gcc = scanForIssues (
            tool: gcc(pattern:"**/buildlog.html",
                      id:"gcc_${buildIdentifier}",
                      name:"GCC ${buildIdentifier}"),
            sourceCodeEncoding: 'UTF-8'
        )
        issueList.add(0,gcc)

        //This script has to be inserted into jenkins configuration. See comment at very bottom of this file
        def lintian = scanForIssues (
            tool: groovyScript(parserId: "lintian",
                               pattern:"**/buildlog.html",
                               id:"lintian_${buildIdentifier}",
                               name:"lintian ${buildIdentifier}"),
            sourceCodeEncoding: 'UTF-8'
        )
        issueList.add(lintian)
    }
    else {
        msbuild = scanForIssues (
            tool: msBuild(pattern:"**/buildlog.html",
                          id:"msbuild_${buildIdentifier}",
                          name:"MSBuild ${buildIdentifier}"),
            sourceCodeEncoding: 'UTF-8'
        )
        issueList.add(0,msbuild)
    }

    publishIssues (
        issues: issueList,
        sourceCodeEncoding: 'UTF-8',
        id: "warnings_${buildIdentifier}",
        name: "Warnings for ${buildIdentifier}",
        skipPublishingChecks: true,
        qualityGates: [[threshold: 1, type: 'TOTAL', unstable: true]],
        trendChartType: 'AGGREGATION_ONLY'
    )
}


def render_documentation() {
    sh label:  "Run Asciidoctor to generate users guide and requirements specification.",
       script: """
               cd docs
               cmake . -G Ninja
               ninja
               ninja install
               mv rendered_docs ..
               """
    archiveArtifacts artifacts: 'rendered_docs/*, rendered_docs/images/*', fingerprint: true
}

def clean_and_copy_artifacts(platform, arch, buildType, sourceJob, sourceBuildNumber){
    def buildIdentifier = "${platform}-${arch}-${buildType}"
    runCommand(command: "git clean -fxd")

    copyArtifacts filter: "build-${buildIdentifier}/*",
                  flatten: true,
                  fingerprintArtifacts: true,
                  projectName: "${sourceJob}",
                  selector: specific("${sourceBuildNumber}")
}

def run_test_suite(platform, arch, buildType, sourceJob, sourceBuildNumber, testType){
    def buildIdentifier = "${platform}-${arch}-${buildType}"
    catchError {
        //languages are picked up from environment variable
        //and for the multicomputer tests the artifacts to copy are inferred from environment too,
        //in run_test.py (in start_slave())
        runPython (command: "build/jenkins_stuff/run_test.py --test ${testType}")
    }

    //we also need to move the folders, or jenkins will merge them all
    fileOperations([fileRenameOperation(destination: "${testType}-${buildIdentifier}", source: "dose_test_output")])

    archiveArtifacts artifacts: '**/*.output.txt'
    junit keepLongStdio: true, skipPublishingChecks: true, testResults: '**/*.junit.xml'
}


def build_examples(){
    runPython (command: "build/jenkins_stuff/run_test.py --test build-examples")
}

pipeline {
    parameters {
        choice(name: 'PLATFORM_FILTER',
               choices: ['all', 'ubuntu-focal', 'ubuntu-jammy', 'debian-bullseye', 'vs2015', 'vs2017', 'vs2019', 'vs2022'],
               description: "Run on specific platform. Note that multicomputer tests will only run if 'all' or 'debian-bullseye' is selected.")

        booleanParam(name: 'SKIP_SLOW_TESTS',
                     defaultValue: false,
                     description: 'If this is true, all unit tests that take "a long time" will be skipped. The system test suite will still be run.')

    }
    agent none
    environment {
        SAFIR_SKIP_SLOW_TESTS = "${SKIP_SLOW_TESTS}"
    }

    stages {
        stage('Build') {
            matrix {
                when {
                    anyOf {
                        expression { params.PLATFORM_FILTER == 'all' }
                        expression { params.PLATFORM_FILTER == env.BUILD_PLATFORM }
                    }
                }
                agent {
                    label "${BUILD_PLATFORM}-${BUILD_ARCH}-build"
                }
                axes {
                    axis {
                        name 'BUILD_PLATFORM'
                        values 'ubuntu-focal', 'ubuntu-jammy', 'debian-bullseye', 'vs2015', 'vs2017', 'vs2019', 'vs2022'
                    }
                    axis {
                        name 'BUILD_ARCH'
                        values 'x86', 'amd64'
                    }
                    axis {
                        name 'BUILD_TYPE'
                        values 'RelWithDebInfo', 'DebugOnly'
                    }
                }
                excludes {
                    exclude {
                        axis { //ubuntu does no longer support 32 bit builds
                            name 'BUILD_PLATFORM'
                            values 'ubuntu-focal', 'ubuntu-jammy'
                        }
                        axis {
                            name 'BUILD_ARCH'
                            values 'x86'
                        }
                    }
                }
                stages {
                    stage('Build and Unit Test') { steps { script {
                        //clean, source_check and build the code
                        clean_check_and_build(BUILD_PLATFORM, BUILD_ARCH)
                    }}}
                }
                post {
                    always {
                        //archive artifacts and check for warnings
                        archive_and_analyze(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE)

                        junit keepLongStdio: true, skipPublishingChecks: true, testResults: '**/*.junit.xml'
                        cleanWs()
                    }
                }
            }
        }

        stage('Render documentation') {
            agent { label 'debian-bullseye-amd64-build' }
            steps { script {
                render_documentation()
            }}
        }

        stage('Test suite') {
            matrix {
                when {
                    anyOf {
                        expression { params.PLATFORM_FILTER == 'all' }
                        expression { params.PLATFORM_FILTER == env.BUILD_PLATFORM }
                    }
                }
                agent {
                    label "${BUILD_PLATFORM}-${BUILD_ARCH}-test"
                }
                axes {
                    axis {
                        name 'BUILD_PLATFORM'
                        values 'ubuntu-focal', 'ubuntu-jammy', 'debian-bullseye', 'vs2015', 'vs2017', 'vs2019', 'vs2022'
                    }
                    axis {
                        name 'BUILD_ARCH'
                        values 'x86', 'amd64'
                    }
                    axis {
                        name 'BUILD_TYPE'
                        values 'RelWithDebInfo', 'DebugOnly'
                    }
                    axis {
                        name 'Languages'
                        values 'dotnet-java-cpp-dotnet-java',
                               'java-cpp-dotnet-java-cpp',
                               'cpp-cpp-cpp-cpp-cpp'
                    }
                }
                excludes {
                    exclude {
                        axis { //ubuntu does no longer support 32 bit builds
                            name 'BUILD_PLATFORM'
                            values 'ubuntu-focal', 'ubuntu-jammy'
                        }
                        axis {
                            name 'BUILD_ARCH'
                            values 'x86'
                        }
                    }
                }
                stages {
                    stage('Standalone Tests') { steps { script {
                        clean_and_copy_artifacts(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE, JOB_NAME, BUILD_NUMBER)
                        run_test_suite(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE, JOB_NAME, BUILD_NUMBER, "standalone-tests")
                    }}}
                    stage('Multinode Tests') { steps { script {
                        //artifacts are left over from previous stage
                        run_test_suite(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE, JOB_NAME, BUILD_NUMBER, "multinode-tests")
                    }}}
                    stage('Multicomputer Tests') {
                        when { allOf {
                            expression {Languages == "cpp-cpp-cpp-cpp-cpp"}
                            anyOf {
                                //The multicomputer test slave uses the debian-bullseye-x86 release build,
                                //so we can't run unless they are part of the build
                                expression {params.PLATFORM_FILTER == 'all'}
                                expression {params.PLATFORM_FILTER == 'debian-bullseye'}
                            }
                        }}
                        steps {
                            lock( 'multicomputer-test-slaves' ) {
                                script {
                                    echo "Took multicomputer-test-slaves lock: ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE} ${BUILD_NUMBER} ${JOB_NAME}"
                                    //artifacts are left over from previous stage
                                    run_test_suite(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE, JOB_NAME, BUILD_NUMBER, "multicomputer-tests")
                                    echo "Releasing multicomputer-test-slaves lock: ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"
                    }}}}



                }
                post {
                    always {
                        cleanWs()
                    }
                }

            }
        }
        
        stage('Build examples') {
            matrix {
                when {
                    anyOf {
                        expression { params.PLATFORM_FILTER == 'all' }
                        expression { params.PLATFORM_FILTER == env.BUILD_PLATFORM }
                    }
                }
                agent {
                    label "${BUILD_PLATFORM}-${BUILD_ARCH}-build"
                }
                axes {
                    axis {
                        name 'BUILD_PLATFORM'
                        values 'ubuntu-focal', 'ubuntu-jammy', 'debian-bullseye', 'vs2015', 'vs2017', 'vs2019', 'vs2022'
                    }
                    axis {
                        name 'BUILD_ARCH'
                        values 'x86', 'amd64'
                    }
                    axis {
                        name 'BUILD_TYPE'
                        values 'RelWithDebInfo', 'DebugOnly'
                    }
                }
                excludes {
                    exclude {
                        axis { //ubuntu does no longer support 32 bit builds
                            name 'BUILD_PLATFORM'
                            values 'ubuntu-focal', 'ubuntu-jammy'
                        }
                        axis {
                            name 'BUILD_ARCH'
                            values 'x86'
                        }
                    }
                }
                stages {
                    stage('Build') { steps { script {
                        clean_and_copy_artifacts(BUILD_PLATFORM, BUILD_ARCH, BUILD_TYPE, JOB_NAME, BUILD_NUMBER)
                        build_examples ()
                    }}}
                }
                post {
                    always {
                        cleanWs()
                    }
                }

            }
        }
    }
    options {
        buildDiscarder(logRotator(numToKeepStr: '30',
                                  artifactNumToKeepStr: '10'))
    }

}


/*

name: lintian
id: lintian

regular expression:
^(E|W): ([a-z-]*): ([a-z-]*) (.*)

Mapping script:
import edu.hm.hafner.analysis.Severity

def severity
if (matcher.group(1) == "W")
    severity = Severity.WARNING_NORMAL
else
    severity = Severity.ERROR

builder.setFileName(matcher.group(2))
        .setSeverity(severity)
        .setCategory(matcher.group(3))
        .setMessage(matcher.group(4))

return builder.buildOptional();

example log message:
W: safir-sdk-core-tools: no-manual-page usr/bin/safir_entity_viewer


*/
