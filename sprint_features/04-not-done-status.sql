SELECT project_id, sprint_id, COUNT(*) AS num_not_done FROM
	(SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
		FROM gros.issue, gros.sprint,
		(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
		WHERE issue.issue_id = max_issue.issue_id
		AND issue.changelog_id = max_issue.changelog_id
		AND issue.sprint_id = sprint.sprint_id
		AND issue.updated > sprint.start_date
		AND ${issue_not_done} AND ${issue_overdue}
		AND issue.type = 7
	) AS not_done
GROUP BY project_id, sprint_id
