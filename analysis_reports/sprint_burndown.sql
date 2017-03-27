SELECT project_id, sprint_id, -story_points AS story_points,
	MIN(updated) AS close_date FROM gros.issue
WHERE story_points <> 0 AND ${issue_done} AND sprint_id <> 0
GROUP BY project_id, sprint_id, issue_id, story_points
UNION ALL
SELECT sprint.project_id, sprint.sprint_id, SUM(max_points) AS story_points,
	sprint.start_date AS close_date FROM gros.sprint
LEFT JOIN (
	SELECT issue_id, project_id, sprint_id, MAX(story_points) AS max_points
		FROM gros.issue GROUP BY issue_id, project_id, sprint_id
) AS points
ON sprint.project_id = points.project_id AND sprint.sprint_id = points.sprint_id
GROUP BY sprint.project_id, sprint.sprint_id, sprint.start_date
UNION ALL
SELECT project_id, sprint_id, 0.0 AS story_points,
	sprint.end_date AS close_date FROM gros.sprint
ORDER BY project_id, sprint_id, close_date;
