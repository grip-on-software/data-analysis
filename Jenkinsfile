pipeline {
    agent { label 'docker' }

    options {
        gitLabConnection('gitlab')
        buildDiscarder(logRotator(numToKeepStr: '10'))
    }
    triggers {
        gitlab(triggerOnPush: true, triggerOnMergeRequest: true, branchFilterType: 'All')
    }

    post {
        success {
            updateGitlabCommitStatus name: env.JOB_NAME, state: 'success'
        }
        failure {
            updateGitlabCommitStatus name: env.JOB_NAME, state: 'failed'
        }
    }

    stages {
        stage('Build') {
            steps {
                updateGitlabCommitStatus name: env.JOB_NAME, state: 'running'
                sh 'docker build -t $DOCKER_REGISTRY/gros-data-analysis .'
            }
        }
		stage('Compose') {
			steps {
				sh 'docker-compose build'
			}
		}
		stage('Push') {
			when { branch 'master' }
			steps {
				sh 'docker push $DOCKER_REGISTRY/gros-data-analysis:latest'
				sh 'docker push $DOCKER_REGISTRY/gros-data-analysis-dashboard:latest'
			}
		}
    }
}
