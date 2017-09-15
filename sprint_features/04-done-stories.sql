-- Number of stories that are done in time
SELECT project_id, sprint_id, COUNT(*) AS num_done_stories FROM 
	(SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id
		FROM gros.issue, gros.sprint,
		(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
		WHERE issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
		AND issue.issue_id = max_issue.issue_id
		AND ${issue_done}
		--AND NOT ${issue_overdue}
		AND issue.updated > sprint.start_date
		AND issue.type = 7
		AND issue.sprint_id <> 0
	) AS done_stories
GROUP BY project_id, sprint_id
