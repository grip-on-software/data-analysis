-- Completed story points
SELECT start_issue.project_id, start_issue.sprint_id,
	-start_issue.story_points AS story_points, start_issue.key,
	MIN(issue.updated) AS close_date, 'completed' AS event_type
FROM gros.issue AS start_issue
JOIN gros.issue AS issue ON start_issue.issue_id = issue.issue_id
	AND start_issue.changelog_id < issue.changelog_id AND ${s(issue_done)}
JOIN gros.sprint ON start_issue.project_id = sprint.project_id
	AND start_issue.sprint_id = sprint.sprint_id
WHERE issue.type = 7 AND ${s(issue_done)}
	AND start_issue.updated > sprint.start_date
	AND start_issue.story_points IS NOT NULL 
GROUP BY start_issue.project_id, start_issue.sprint_id, start_issue.key,
	start_issue.story_points
-- Stories added during the sprint
UNION ALL
SELECT late_issue.project_id, late_issue.sprint_id, late_issue.story_points,
	late_issue.key, late_issue.updated AS close_date, 'scope_add' AS event_type
FROM gros.issue AS late_issue
LEFT JOIN gros.issue ON late_issue.issue_id = issue.issue_id
	AND late_issue.changelog_id = issue.changelog_id + 1
JOIN gros.sprint ON late_issue.project_id = sprint.project_id
	AND late_issue.sprint_id = sprint.sprint_id
WHERE COALESCE(issue.sprint_id,0) <> late_issue.sprint_id
	AND (late_issue.changelog_id = 0 OR issue.changelog_id IS NOT NULL)
	AND late_issue.updated BETWEEN sprint.start_date AND sprint.end_date
	AND late_issue.type = 7
-- Stories removed during the sprint
UNION ALL
SELECT remove_issue.project_id, remove_issue.sprint_id,
	-remove_issue.story_points AS story_points, remove_issue.key,
	remove_issue.updated AS close_date, 'scope_remove' AS event_type
FROM gros.issue AS remove_issue
JOIN gros.issue ON remove_issue.issue_id = issue.issue_id
	AND remove_issue.changelog_id = issue.changelog_id + 1
JOIN gros.sprint ON issue.project_id = sprint.project_id
	AND issue.sprint_id = sprint.sprint_id
WHERE issue.sprint_id <> remove_issue.sprint_id
	AND remove_issue.updated BETWEEN sprint.start_date AND sprint.end_date
	AND remove_issue.type = 7
-- Story points change during the sprint
UNION ALL
SELECT edit_issue.project_id, edit_issue.sprint_id,
	edit_issue.story_points - COALESCE(issue.story_points, 0) AS story_points,
	edit_issue.key, edit_issue.updated AS close_date, 'points' AS event_type
FROM gros.issue AS edit_issue
LEFT JOIN gros.issue ON edit_issue.issue_id = issue.issue_id
	AND edit_issue.changelog_id = issue.changelog_id + 1
JOIN gros.sprint ON edit_issue.project_id = sprint.project_id
	AND edit_issue.sprint_id = sprint.sprint_id
WHERE issue.sprint_id = edit_issue.sprint_id
	AND COALESCE(issue.story_points, 0) <> edit_issue.story_points
	AND edit_issue.updated BETWEEN sprint.start_date AND sprint.end_date
	AND edit_issue.type = 7
-- Initial story points
UNION ALL
SELECT sprint.project_id, sprint.sprint_id,
	COALESCE(SUM(init_issue.story_points), 0) AS story_points, null as key,
	sprint.start_date AS close_date, 'initial' AS event_type
FROM gros.sprint
LEFT JOIN (
	SELECT issue_id, project_id, sprint_id, story_points, MIN(updated) AS updated
	FROM gros.issue
	WHERE issue.type = 7 AND issue.sprint_id IS NOT NULL
	GROUP BY issue_id, project_id, sprint_id, story_points
) AS init_issue
ON sprint.project_id = init_issue.project_id
	AND sprint.sprint_id = init_issue.sprint_id
	AND init_issue.updated < sprint.start_date
GROUP BY sprint.project_id, sprint.sprint_id, sprint.start_date
-- Close date
UNION ALL
SELECT project_id, sprint_id, null AS story_points, null as key,
	${sprint_close} AS close_date, 'close' AS event_type
FROM gros.sprint
ORDER BY project_id, sprint_id, close_date;
