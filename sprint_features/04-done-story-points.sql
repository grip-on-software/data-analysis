-- Number of done story points
SELECT done_stories.project_id, done_stories.sprint_id, SUM(issue.story_points) AS done_story_points FROM
	gros.issue,
	(SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id, max_issue.changelog_id
		FROM gros.issue, gros.sprint,
		(SELECT issue_id, sprint_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id, sprint_id) AS max_issue
		WHERE issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
		AND issue.issue_id = max_issue.issue_id AND issue.sprint_id = max_issue.sprint_id
		AND ${issue_done}
		--AND NOT ${issue_overdue}
		AND issue.updated > sprint.start_date
		AND issue.type = 7
		AND issue.sprint_id <> 0
	) AS done_stories
WHERE issue.issue_id = done_stories.issue_id
AND issue.changelog_id = done_stories.changelog_id
GROUP BY done_stories.project_id, done_stories.sprint_id
