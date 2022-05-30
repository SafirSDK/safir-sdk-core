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

pipeline {
    parameters {
        choice(name: 'PLATFORM_FILTER',
               choices: ['all', 'ubuntu-focal', 'ubuntu-jammy', 'debian-bullseye', 'vs2015', 'vs2017', 'vs2019', 'vs2022'],
               description: 'Run on specific platform')
    }
    agent none
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
                        values 'DebugOnly', 'RelWithDebInfo'
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
                    stage('Build and Unit Test') {
                        steps {
                            runCommand (command: "git clean -fxd")
                            runPython (command: "build/check_source_tree.py")
                            runCommand (command: "build/build.py --jenkins --package",
                                        windows_arguments: "--use-studio ${BUILD_PLATFORM} --arch ${BUILD_ARCH}")
                        }
                    }
                    stage('Archive and analyze') {
                        steps {
                            script {
                                if (betterIsUnix()) {
                                    sh label: "Moving artifacts to build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                       script: """
                                               mkdir build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                               mv tmp/*.deb build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                               """

                                    archiveArtifacts artifacts: "build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*.deb", fingerprint: true
                                }
                                else {
                                    bat label: "Moving artifacts to build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                        script: """
                                                md build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                move build\\packaging\\windows\\*.exe build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                """

                                    archiveArtifacts artifacts: "build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*.exe", fingerprint: true
                                }

                                if (betterIsUnix()) {
                                    recordIssues(sourceCodeEncoding: 'UTF-8',
                                                 skipBlames: true,
                                                 skipPublishingChecks: true,
                                                 healthy: 1,
                                                 unhealthy:10,
                                                 tools: [cmake(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_cmake",
                                                               name:"CMake ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         gcc(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_gcc",
                                                             name:"GCC ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         java(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_java",
                                                              name:"Java ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         doxygen(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_doxygen",
                                                                 name:"Doxygen ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                        ])
                                }
                                else {
                                    recordIssues(sourceCodeEncoding: 'UTF-8',
                                                 skipBlames: true,
                                                 skipPublishingChecks: true,
                                                 healthy: 1,
                                                 unhealthy:10,
                                                 tools: [cmake(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_cmake",
                                                               name:"CMake ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         java(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_java",
                                                              name:"Java ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         doxygen(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_doxygen",
                                                                 name:"Doxygen ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}"),
                                                         msBuild(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_msbuild",
                                                                 name:"MSBuild ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}")
                                                        ])
                                }
                            }
                        }
                    }

                }
                post {
                    always {
                        junit keepLongStdio: true, skipPublishingChecks: true, testResults: '**/*.junit.xml'
                        cleanWs()
                    }
                }
            }
        }

        stage('Render documentation') {
            agent { label 'debian-bullseye-amd64-build' }
            steps {
                script {
                    sh label:  "Run Asciidoc to generate users guide and requirements specification.",
                       script: """
                               cd docs/users_guide
                               make -j2 all
                               cd ../requirements
                               make -j2 all
                               cd ../..
                               mkdir -p rendered_docs/images
                               cp docs/users_guide/users_guide.pdf rendered_docs/
                               cp docs/users_guide/users_guide.html rendered_docs/
                               cp docs/users_guide/images/*.png rendered_docs/images
                               cp docs/requirements/requirements_specification.pdf rendered_docs/
                               """
                }

                archiveArtifacts artifacts: 'rendered_docs/*, rendered_docs/images/*', fingerprint: true
            }
        }

        stage('Test') {
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
                        values 'DebugOnly', 'RelWithDebInfo'
                    }
                    axis {
                        name 'Languages'
                        values 'cpp-cpp-cpp', 'dotnet-java-cpp', 'java-cpp-dotnet', 'cpp-dotnet-java'
                    }
                    axis {
                        name 'TEST_KIND'
                        values 'multinode-tests', 'standalone-tests'
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
                    stage('Run Tests') {
                        steps {
                            runCommand(command: "git clean -fxd")

                            copyArtifacts filter: "build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*",
                                          flatten: true,
                                          fingerprintArtifacts: true,
                                          projectName: '${JOB_NAME}',
                                          selector: specific('${BUILD_NUMBER}')

                            //languages are picked up from environment variable
                            runPython (command: "build/jenkins_stuff/run_test.py --test ${TEST_KIND}")

                            //we also need to move the folders, or jenkins will merge them all
                            fileOperations([fileRenameOperation(destination: '${TEST_KIND}-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}', source: 'dose_test_output')])

                            archiveArtifacts artifacts: '**/*.output.txt'
                            junit keepLongStdio: true, skipPublishingChecks: true, testResults: '**/*.junit.xml'
                        }
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
                        values 'DebugOnly', 'RelWithDebInfo'
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
                    stage('Build') {
                        steps {
                            runCommand(command: "git clean -fxd")

                            copyArtifacts filter: "build-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*",
                                          flatten: true,
                                          fingerprintArtifacts: true,
                                          projectName: '${JOB_NAME}',
                                          selector: specific('${BUILD_NUMBER}')
                            runPython (command: "build/jenkins_stuff/run_test.py --test build-examples")
                        }
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
