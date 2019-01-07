SELECT project_id, sprint_id, start_date, COUNT(*) AS num_stories, CAST(COALESCE(SUM(story_points),0) AS INT) AS num_points, SUM(epics) AS num_epics, SUM(links) AS num_epic_links, SUM(link_points) AS num_epic_points
FROM (
	SELECT sprint.project_id, sprint.sprint_id, sprint.start_date, issue.issue_id, MAX(issue.story_points) AS story_points, 0 AS epics, 0 AS links, 0 AS link_points
	FROM gros.issue
	RIGHT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id
	JOIN (SELECT issue_id, MAX(sprint_id) AS sprint_id FROM gros.issue GROUP BY issue_id) AS max_issue ON issue.issue_id = max_issue.issue_id
	WHERE start_date IS NOT NULL AND
	issue.updated < ${s(sprint_close)} AND
	(issue.resolution_date IS NULL OR issue.resolution_date > ${s(sprint_close)})
	AND max_issue.sprint_id <> sprint.sprint_id
	AND issue."type" <> 6 --IN (6,7,20)
	AND issue."status" = 1
	AND issue.story_points > 0
	AND issue.ready_status IN (0, 11903)
	GROUP BY sprint.project_id, sprint.sprint_id, sprint.start_date, issue.issue_id

	UNION ALL

	SELECT epic.project_id, epic.sprint_id, epic.start_date, epic.issue_id, epic.story_points, 1 AS epics, COUNT(epic.key) AS links, CAST(COALESCE(SUM(subtasks.story_points),0) AS INT) AS link_points
	FROM (
		SELECT sprint.project_id, sprint.sprint_id, sprint.start_date, issue.issue_id, issue.key, MAX(issue.story_points) AS story_points
		FROM gros.issue
		RIGHT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id
		JOIN (SELECT issue_id, MAX(sprint_id) AS sprint_id FROM gros.issue GROUP BY issue_id) AS max_issue ON issue.issue_id = max_issue.issue_id
		WHERE start_date IS NOT NULL AND
		issue.updated < ${s(sprint_close)} AND
		(issue.resolution_date IS NULL OR issue.resolution_date > ${s(sprint_close)})
		AND max_issue.sprint_id <> sprint.sprint_id
		AND issue."type" = 6 --IN (6,7,20)
		AND issue."status" = 1
		GROUP BY sprint.project_id, sprint.sprint_id, sprint.start_date, issue.issue_id, issue.key
	) AS epic
	RIGHT OUTER JOIN (
		SELECT issue_id, epic, MAX(story_points) AS story_points, MAX(changelog_id) AS changelog_id FROM gros.issue
		WHERE epic IS NOT NULL
		GROUP BY issue_id, epic
	) AS subtasks
	ON subtasks.epic = epic.key 
	GROUP BY epic.project_id, epic.sprint_id, epic.start_date, epic.issue_id, epic.story_points
) AS did
GROUP BY project_id, sprint_id, start_date
HAVING project_id IS NOT NULL
ORDER BY project_id, start_date;
