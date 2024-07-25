SELECT edited.project_id, num_edited, num_in_sprint FROM
(
	SELECT project_id, COUNT(*) AS num_edited FROM (
		SELECT DISTINCT issue.project_id, issue.key FROM gros.issue
		JOIN gros.sprint ON issue.sprint_id = sprint.sprint_id
		JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id
			AND issue.changelog_id = prev_issue.changelog_id+1
			AND issue.resolution = prev_issue.resolution AND issue.status = prev_issue.status
			AND issue.assignee = prev_issue.assignee AND issue.title = prev_issue.title
		WHERE issue.status = 3
	) AS edit
) AS edited
LEFT JOIN (
	SELECT project_id, COUNT(*) AS num_in_sprint FROM (
		SELECT DISTINCT project_id, issue_id FROM gros.issue
		WHERE issue.sprint_id <> 0
	) AS issues GROUP BY project_id
) AS in_sprint
ON edited.project_id = in_sprint.project_id;