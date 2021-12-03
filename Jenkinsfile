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
                        values 'ubuntu-focal', 'debian-bullseye' //, 'msvc2015', 'msvc2019', 'msvc2022'
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
                    stage('Install prerequisites') {
                        steps {
                            script {
                                if (isUnix()) {
                                    sh 'pip install conan'
                                }
                                else {
                                    bat 'pip install conan'
                                }
                            }
                        }
                    }
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
                                sh label:  "Running check_source_tree.py.",
                                   script: """
                                           build/check_source_tree.py
                                           """
                            }
                        }
                    }
                    stage('Build and Unit Test') {
                        steps {
                            script {
                                sh label:  "Running build script.",
                                   script: """
                                           export PATH=$PATH:/home/jenkins/.local/bin
                                           build/build.py --jenkins --package
                                           """
                            }
                        }
                    }
                    stage('Archive') {
                        steps {
                            script {
                                sh label: "Moving artifacts to ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}.",
                                   script: """
                                           mkdir ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                           mv tmp/*.deb ${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}
                                           """

                                archiveArtifacts artifacts: '**/*.deb', fingerprint: true
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
                                                 doxygen(id:"${BUILD_PLATFORM}-${BUILD_ARCH}-${BUILD_TYPE}_doxygen")
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
