SELECT start_issue.project_id, start_issue.sprint_id, -start_issue.story_points AS story_points, MIN(issue.updated) AS close_date
FROM gros.issue AS start_issue
JOIN gros.issue AS issue ON start_issue.issue_id = issue.issue_id AND start_issue.changelog_id < issue.changelog_id
JOIN gros.sprint ON start_issue.project_id = sprint.project_id AND start_issue.sprint_id = sprint.sprint_id
WHERE start_issue.type = 7 AND start_issue.status = 3 AND ${issue_done}
AND start_issue.updated > sprint.start_date AND start_issue.story_points IS NOT NULL 
GROUP BY start_issue.project_id, start_issue.sprint_id, start_issue.story_points
UNION ALL
SELECT sprint.project_id, sprint.sprint_id, SUM(max_points) AS story_points,
	sprint.start_date AS close_date FROM gros.sprint
LEFT JOIN (
	SELECT issue_id, project_id, sprint_id, MAX(story_points) AS max_points, MIN(updated) AS in_progress_date
		FROM gros.issue WHERE type = 7 AND status = 3
		GROUP BY issue_id, project_id, sprint_id
) AS points
ON sprint.project_id = points.project_id AND sprint.sprint_id = points.sprint_id
WHERE points.in_progress_date > sprint.start_date
GROUP BY sprint.project_id, sprint.sprint_id, sprint.start_date
UNION ALL
SELECT project_id, sprint_id, null AS story_points,
	${sprint_close} AS close_date FROM gros.sprint
ORDER BY project_id, sprint_id, close_date;
