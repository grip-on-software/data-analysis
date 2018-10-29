SELECT DISTINCT issue.project_id, issue.sprint_id, issue.key, issue.story_points
FROM gros.issue
JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN gros.issue AS old_issue
ON issue.issue_id = old_issue.issue_id AND issue.changelog_id = old_issue.changelog_id+1
LEFT JOIN gros.subtask ON issue.issue_id = subtask.id_subtask
WHERE issue.story_points IS NOT NULL
AND old_issue.sprint_id IS NOT NULL AND issue.sprint_id IS NOT NULL
AND issue.sprint_id <> old_issue.sprint_id
AND issue.updated > ${planned_end}
AND ${s(issue_not_done)}
AND subtask.id_parent IS NULL
