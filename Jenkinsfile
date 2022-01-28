pipeline {
    agent none
    stages {
        stage('Build') {
            matrix {
                agent {
                    label "${BUILD_PLATFORM}-${BUILD_ARCH}-build"
                }
                axes {
                    axis {
                        name 'BUILD_PLATFORM'
                        values 'ubuntu-focal', 'debian-bullseye', 'vs2015', 'vs2019'
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
                            values 'ubuntu-focal'
                        }
                        axis {
                            name 'BUILD_ARCH'
                            values 'x86'
                        }
                    }
                }
                stages {
                    stage('Clean') {
                        steps {
                            script {
                                if (isUnix()) {
                                    sh 'git clean -fxd'
                                }
                                else {
                                    bat 'git clean -fxd'
                                }
                            }
                        }
                    }
                    stage('Check source tree') {
                        steps {
                            script {
                                if (isUnix()) {
                                    sh label:  "Running check_source_tree.py.",
                                       script: """
                                               build/check_source_tree.py
                                               """
                                }
                                else {
                                    bat label:  "Running check_source_tree.py.",
                                        script: """
                                                build\\check_source_tree.py
                                                """
                                }
                            }
                        }
                    }
                    stage('Build and Unit Test') {
                        steps {
                            script {
                                if (isUnix()) {
                                    sh label:  "Running build script.",
                                       script: """
                                               export PATH=$PATH:/home/jenkins/.local/bin
                                               build/build.py --jenkins --package
                                               """
                                }
                                else {
                                    bat label:  "Running build script.",
                                        script: """
                                                build\\build.py --jenkins --package --use-studio ${BUILD_PLATFORM} --arch ${BUILD_ARCH}
                                                """
                                }
                            }
                        }
                    }
                    stage('Archive') {
                        steps {
                            script {
                                if (isUnix()) {
                                    sh label: "Moving artifacts to ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                       script: """
                                               mkdir ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                               mv tmp/*.deb ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                               """

                                    archiveArtifacts artifacts: "${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*.deb", fingerprint: true
                                }
                                else {
                                    bat label: "Moving artifacts to ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                        script: """
                                                md ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                move build\\packaging\\windows\\*.exe ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                """

                                    archiveArtifacts artifacts: "${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*.exe", fingerprint: true
                                }
                            }
                        }
                    }
                    stage('Analyze') {
                        steps {
                            script {
                                if (isUnix()) {
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

        stage('Test') {
            matrix {
                agent {
                    label "${BUILD_PLATFORM}-${BUILD_ARCH}-test"
                }
                axes {
                    axis {
                        name 'BUILD_PLATFORM'
                        values 'ubuntu-focal', 'debian-bullseye', 'vs2015', 'vs2019'
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
                            values 'ubuntu-focal'
                        }
                        axis {
                            name 'BUILD_ARCH'
                            values 'x86'
                        }
                    }
                    exclude { //TODO fix x86 tests on windows (needs 32 bit windows?)
                        //TODO: 32on64?
                        axis {
                            name 'BUILD_PLATFORM'
                            values 'vs2015', 'vs2019'
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
                            script {
                                if (isUnix()) {
                                    sh 'git clean -fxd'
                                }
                                else {
                                    bat 'git clean -fxd'
                                }
                            }

                            copyArtifacts filter: "${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}/*",
                                          flatten: true,
                                          fingerprintArtifacts: true,
                                          projectName: '${JOB_NAME}',
                                          selector: specific('${BUILD_NUMBER}')

                            script {
                                //languages are picked up from environment variable
                                //we also need to move the folders, or jenkins will merge them all
                                if (isUnix()) {
                                    sh script: """
                                               build/jenkins_stuff/run_test.py --test ${TEST_KIND}
                                               mv dose_test_output ${TEST_KIND}-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                               """
                                }
                                else {
                                    bat script: """
                                                python build/jenkins_stuff/run_test.py --test ${TEST_KIND}
                                                move dose_test_output ${TEST_KIND}-${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                """
                                }
                            }

                            archiveArtifacts artifacts: '**/*.output.txt'
                            junit keepLongStdio: true, skipPublishingChecks: true, testResults: '**/*.junit.xml'
                        }
                    }
                }
            }
        }
    }
}
