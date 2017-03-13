SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_not_done FROM gros.issue,
(SELECT DISTINCT(issue.issue_id) FROM gros.issue, gros.sprint,
	(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
	WHERE issue.issue_id = maxdata.issue_id
	AND issue.changelog_id = maxdata.max_changelog_id
	AND issue.sprint_id = sprint.sprint_id
	AND ${issue_not_done} AND ${issue_overdue}) AS issuedata
AND issue.issue_id = issuedata.issue_id
AND issue.sprint_id <> 0
AND issue.type = 7
GROUP BY issue.project_id, issue.sprint_id
