SELECT DISTINCT issue.project_id, issue.sprint_id, issue.key, ${s(story_points)} AS story_points
FROM gros.issue
JOIN gros.sprint ON issue.sprint_id = sprint.sprint_id
JOIN (SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
ON issue.issue_id = max_issue.issue_id
LEFT JOIN gros.subtask ON issue.issue_id = subtask.id_subtask
AND issue.changelog_id = max_issue.changelog_id
WHERE issue.updated > ${sprint_open}
AND ${s(issue_not_done)} AND ${issue_overdue}
AND issue.story_points IS NOT NULL
AND subtask.id_parent IS NULL
