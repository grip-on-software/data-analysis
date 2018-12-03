SELECT interval_sprint.project_id, interval_sprint.sprint_id,
issue.key, MAX(${s(story_points)}) AS story_points
FROM gros.issue
LEFT JOIN gros.issue AS newer_issue
	ON issue.issue_id = newer_issue.issue_id
	AND newer_issue.changelog_id = issue.changelog_id + 1
LEFT JOIN gros.sprint
	ON issue.project_id = sprint.project_id
	AND issue.sprint_id = sprint.sprint_id,
gros.sprint AS interval_sprint
WHERE issue.project_id = interval_sprint.project_id
AND (sprint.sprint_id = NULL OR ${sprint_open} >= interval_sprint.start_date)
AND ${s(issue_not_done)}
AND ${issue_backlog}
AND issue.updated <= interval_sprint.start_date
AND (
	newer_issue.updated IS NULL OR
	newer_issue.updated > interval_sprint.start_date
)
GROUP BY interval_sprint.project_id, interval_sprint.sprint_id, issue.key
