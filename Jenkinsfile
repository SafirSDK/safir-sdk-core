pipeline {
    agent none
    stages {
        stage('Build') {
            matrix {
                agent {
                    label "${PLATFORM}-${ARCH}-build"
                }
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'ubuntu-focal', 'debian-bullseye' //, 'msvc2015', 'msvc2019', 'msvc2022'
                    }
                    axis {
                        name 'ARCH'
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
                            name 'PLATFORM'
                            values 'ubuntu-focal'
                        }
                        axis {
                            name 'ARCH'
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
                                sh label: "Moving artifacts to ${PLATFORM}-${ARCH}-${BUILD_TYPE}.",
                                   script: """
                                           mkdir ${PLATFORM}-${ARCH}-${BUILD_TYPE}
                                           mv tmp/*.deb ${PLATFORM}-${ARCH}-${BUILD_TYPE}
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
                                         tools: [cmake(id:"${PLATFORM}-${ARCH}-${BUILD_TYPE}_cmake"),
                                                 gcc(id:"${PLATFORM}-${ARCH}-${BUILD_TYPE}_gcc"),
                                                 java(id:"${PLATFORM}-${ARCH}-${BUILD_TYPE}_java"),
                                                 doxygen(id:"${PLATFORM}-${ARCH}-${BUILD_TYPE}_doxygen")
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
