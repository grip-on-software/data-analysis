-- Number of VCS committers each day in sprint

SELECT sprint_devs.project_id, sprint_devs.commit_date, COUNT(*) AS value FROM (
    SELECT DISTINCT commits.project_id, CAST((CASE WHEN sprint.start_date IS NULL THEN commits.commit_date ELSE sprint.start_date END) AS DATE) AS commit_date, commits.developer_id FROM gros.commits
	LEFT JOIN gros.sprint ON commits.sprint_id = sprint.sprint_id
	JOIN gros.vcs_developer ON commits.developer_id = vcs_developer.alias_id
	WHERE vcs_developer.jira_dev_id <> -1
) AS sprint_devs
GROUP BY sprint_devs.project_id, sprint_devs.commit_date
ORDER BY sprint_devs.project_id, sprint_devs.commit_date
