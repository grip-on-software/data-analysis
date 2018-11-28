-- Number of done story points
SELECT done_stories.project_id, done_stories.sprint_id, issue.key, ${s(story_points)} AS story_points FROM
	gros.issue,
	(SELECT DISTINCT issue.project_id, issue.sprint_id, issue.issue_id, max_issue.changelog_id
		FROM gros.issue
		JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
		JOIN (SELECT issue_id, sprint_id, MAX(changelog_id) AS changelog_id
			FROM gros.issue GROUP BY issue_id, sprint_id
		) AS max_issue ON issue.issue_id = max_issue.issue_id AND issue.sprint_id = max_issue.sprint_id
		JOIN gros.issue AS resolve_issue ON resolve_issue.issue_id = max_issue.issue_id AND resolve_issue.changelog_id = max_issue.changelog_id
    	LEFT JOIN gros.subtask ON resolve_issue.issue_id = subtask.id_subtask
		WHERE ${s(issue_done)}
		AND ${s(issue_done, issue="resolve_issue")}
		--AND NOT ${issue_overdue}
		AND issue.updated > ${sprint_open}
		AND ${story_point_types}
		AND issue.story_points IS NOT NULL
		AND issue.sprint_id IS NOT NULL
    	AND subtask.id_parent IS NULL
	) AS done_stories
WHERE issue.issue_id = done_stories.issue_id
AND issue.changelog_id = done_stories.changelog_id
