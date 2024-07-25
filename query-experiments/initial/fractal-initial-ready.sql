SELECT issue."key",
	initial_issue.changelog_id AS initial_version,
	issue.story_points AS initial_points,
	ready_issue.changelog_id AS ready_version,
	ready_issue.story_points AS ready_points,
	initial_issue.last_id AS number_of_versions
	FROM gros.issue
JOIN (
	SELECT issue_id, MIN(changelog_id) AS changelog_id, MAX(changelog_id) AS last_id FROM gros.issue
	WHERE project_id = 58
	AND story_points IS NOT NULL
	GROUP BY issue_id
) AS initial_issue
ON issue.issue_id = initial_issue.issue_id
AND issue.changelog_id = initial_issue.changelog_id
LEFT JOIN (
	SELECT issue_id, story_points, MIN(changelog_id) AS changelog_id FROM gros.issue
	WHERE ready_status = 11700
	--AND project_id = <select project ID here>
	GROUP BY issue_id, story_points
) AS ready_issue
ON issue.issue_id = ready_issue.issue_id
--WHERE issue.project_id = <select project ID here>
ORDER BY issue.issue_id, initial_issue.changelog_id, ready_issue.changelog_id
