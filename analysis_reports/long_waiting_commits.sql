SELECT data.project_id, repo.repo_name, repo.url, data.file, data.later_date, MAX(earlier_commits.commit_date) AS earlier_date
FROM (
	SELECT commits.project_id, commits.repo_id, change_path.file, commits.commit_date AS later_date
	FROM gros.change_path
	JOIN gros.commits ON change_path.repo_id = commits.repo_id AND change_path.version_id = commits.version_id
) AS data
JOIN gros.change_path AS earlier_path ON data.repo_id = earlier_path.repo_id AND data.file = earlier_path.file
JOIN gros.commits AS earlier_commits ON data.repo_id = earlier_commits.repo_id AND earlier_path.version_id = earlier_commits.version_id
	AND data.later_date > earlier_commits.commit_date
	AND EXTRACT(day FROM (data.later_date - earlier_commits.commit_date)) < 365
JOIN gros.repo ON data.repo_id = repo.id
WHERE data.file NOT LIKE '%/pom.xml' AND data.file <> 'pom.xml' ${category_conditions}
GROUP BY data.project_id, repo.repo_name, repo.url, data.file, data.later_date
HAVING EXTRACT(day FROM (data.later_date - MAX(earlier_commits.commit_date))) > 31
ORDER BY data.project_id, data.later_date
