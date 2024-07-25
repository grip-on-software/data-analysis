SELECT commits.project_id, repodata.git_name, commits.commit_id
FROM gros.commits,
	(SELECT project_id, repo_id, repo.repo_name, MAX(commit_date) AS max_date
	FROM gros.commits LEFT JOIN gros.repo ON commits.repo_id = repo.id
	GROUP BY project_id, repo_id, repo.repo_name) AS repodata
WHERE commits.project_id = repodata.project_id
AND commits.repo_id = repodata.repo_id
AND repodata.max_date = commits.commit_date
GROUP BY commits.project_id, repodata.git_name, commits.commit_id;
