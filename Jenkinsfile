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
                        values 'ubuntu-focal', 'debian-bullseye', 'vs2015' //, 'vs2019'
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
                        axis { //ubuntu does no longer support 32 bit builds, and we haven't fixed windows x86 yet
                            name 'BUILD_PLATFORM'
                            values 'ubuntu-focal', "vs2015"
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
                                                build\\build.py --jenkins --package --use-studio ${BUILD_PLATFORM}
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

                                    archiveArtifacts artifacts: '**/*.deb', fingerprint: true
                                }
                                else {
                                    bat label: "Moving artifacts to ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                        script: """
                                                md ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                move build\\packaging\\windows\\*.exe ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                                """

                                    archiveArtifacts artifacts: '**/*.exe', fingerprint: true
                                }
                            }
                        }
                    }
                    stage('Analyze') {
                        steps {
                            recordIssues(sourceCodeEncoding: 'UTF-8',
                                         skipBlames: true,
                                         skipPublishingChecks: true,
                                         healthy: 1,
                                         unhealthy:10,
                                         tools: [cmake(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_cmake"),
                                                 gcc(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_gcc"),
                                                 java(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_java"),
                                                 doxygen(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_doxygen"),
                                                 MSBuild(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_doxygen")
                                                ])
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
    }
}
