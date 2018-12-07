-- Number of done story points
SELECT DISTINCT issue.project_id, issue.sprint_id, issue.key, ${s(story_points, issue="resolve_issue")} AS story_points
FROM gros.issue
JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN (SELECT issue_id, status, resolution, MAX(changelog_id) AS changelog_id
	FROM gros.issue GROUP BY issue_id, status, resolution
) AS max_issue ON issue.issue_id = max_issue.issue_id
JOIN gros.issue AS resolve_issue ON resolve_issue.issue_id = max_issue.issue_id AND resolve_issue.changelog_id = max_issue.changelog_id
LEFT JOIN gros.issue AS later_issue ON later_issue.issue_id = resolve_issue.issue_id AND later_issue.changelog_id = resolve_issue.changelog_id + 1
LEFT JOIN gros.subtask ON resolve_issue.issue_id = subtask.id_subtask
WHERE ${s(issue_done)}
AND ${s(issue_done, issue="resolve_issue")}
AND issue.sprint_id = resolve_issue.sprint_id
AND (issue.status = 6 OR resolve_issue.status <> 6)
AND (later_issue.issue_id IS NULL OR later_issue.updated >= ${sprint_close})
--AND NOT ${issue_overdue}
AND issue.updated > ${sprint_open}
AND ${story_point_types}
AND issue.story_points IS NOT NULL
AND issue.sprint_id IS NOT NULL
AND subtask.id_parent IS NULL
