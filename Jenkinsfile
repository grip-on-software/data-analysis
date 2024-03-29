pipeline {
    agent { label 'docker' }

    environment {
        AGENT_TAG = env.BRANCH_NAME.replaceFirst('^master$', 'latest')
        AGENT_NAME = "${env.DOCKER_REGISTRY}/gros-data-analysis-dashboard"
        AGENT_IMAGE = "${env.AGENT_NAME}:${env.AGENT_TAG}"
        REPO_NAME = "${env.DOCKER_REPOSITORY}/gros-data-analysis-dashboard"
        REPO_IMAGE = "${env.REPO_NAME}:${env.AGENT_TAG}"
        GITLAB_TOKEN = credentials('data-analysis-gitlab-token')
        SCANNER_HOME = tool name: 'SonarQube Scanner 3', type: 'hudson.plugins.sonar.SonarRunnerInstallation'
    }

    options {
        gitLabConnection('gitlab')
        buildDiscarder(logRotator(numToKeepStr: '10'))
    }
    triggers {
        gitlab(triggerOnPush: true, triggerOnMergeRequest: true, branchFilterType: 'All', secretToken: env.GITLAB_TOKEN)
    }

    post {
        success {
            updateGitlabCommitStatus name: env.JOB_NAME, state: 'success'
        }
        failure {
            updateGitlabCommitStatus name: env.JOB_NAME, state: 'failed'
        }
        aborted {
            updateGitlabCommitStatus name: env.JOB_NAME, state: 'canceled'
        }
        always {
            archiveArtifacts 'schema/**/*.json'
        }
    }

    stages {
        stage('Build') {
            steps {
                updateGitlabCommitStatus name: env.JOB_NAME, state: 'running'
                sh 'docker build -t $AGENT_IMAGE .'
            }
        }
        stage('Lint') {
            agent {
                docker {
                    image "${env.AGENT_IMAGE}"
                }
            }
            steps {
                sh 'Rscript lint.r'
            }
        }
        stage('SonarQube Analysis') {
            steps {
                withSonarQubeEnv('SonarQube') {
                    sh '${SCANNER_HOME}/bin/sonar-scanner -Dsonar.projectKey=data-analysis:$BRANCH_NAME -Dsonar.projectName="Data analysis $BRANCH_NAME"'
                }
            }
        }
        stage('Push') {
            when { branch 'master' }
            steps {
                sh 'docker push $AGENT_IMAGE'
                withDockerRegistry(credentialsId: 'docker-credentials', url: env.DOCKER_URL) {
                    sh 'docker tag $AGENT_IMAGE $REPO_IMAGE'
                    sh 'docker push $REPO_IMAGE'
                }
            }
        }
    }
}
