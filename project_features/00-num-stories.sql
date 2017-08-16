-- Number of stories in a sprint
SELECT project_id, AVG(num_issues) AS num_stories FROM (
	SELECT project_id, sprint_id, COUNT(DISTINCT issue_id) AS num_issues
	FROM gros.issue WHERE type = 7 AND sprint_id <> 0
	GROUP BY project_id, sprint_id
) AS sprint_issue GROUP BY project_id;
