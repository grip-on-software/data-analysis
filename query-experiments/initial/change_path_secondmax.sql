SELECT data.project_id, data.repo_id, data.file, data.later_date, MAX(earlier_commits.commit_date) AS earlier_date
FROM (
SELECT commits.project_id, commits.repo_id, change_path.file, MAX(commits.commit_date) AS later_date
FROM gros.change_path
JOIN gros.commits ON change_path.repo_id = commits.repo_id AND change_path.version_id = commits.version_id
GROUP BY commits.project_id, commits.repo_id, change_path.file
) AS data
JOIN gros.change_path AS earlier_path ON data.repo_id = earlier_path.repo_id AND data.file = earlier_path.file
JOIN gros.commits AS earlier_commits ON data.repo_id = earlier_commits.repo_id AND earlier_path.version_id = earlier_commits.version_id AND data.later_date > earlier_commits.commit_date
--WHERE data.project_id = <select project ID here>
GROUP BY data.project_id, data.repo_id, data.file, data.later_date
HAVING EXTRACT(day FROM (data.later_date - MAX(earlier_commits.commit_date))) > 14
LIMIT 10;
