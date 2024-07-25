SELECT sprint_devs.project_id, sprint_devs.sprint_id, COUNT(*) AS number_of_jira_devs FROM
(SELECT issue.project_id, issue.sprint_id, issue.updated_by AS developer_id FROM gros.issue
	GROUP BY issue.project_id, issue.sprint_id, developer_id HAVING issue.sprint_id <> 0
	UNION ALL
	SELECT issue.project_id, issue.sprint_id, comment.author AS developer_id FROM gros.comment
	JOIN gros.issue ON issue.issue_id = comment.issue_id
	GROUP BY issue.project_id, issue.sprint_id, developer_id HAVING issue.sprint_id <> 0
) AS sprint_devs
GROUP BY sprint_devs.project_id, sprint_devs.sprint_id;
