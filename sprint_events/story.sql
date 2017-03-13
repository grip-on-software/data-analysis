SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issuedata.date AS date, issuedata.end_date AS end_date 
FROM (SELECT start_issue.project_id, start_issue.sprint_id, start_issue.issue_id, MIN(start_issue.updated) AS date, MIN(end_issue.updated) AS end_date
	FROM gros.issue AS start_issue
	JOIN gros.issue AS issue ON start_issue.issue_id = issue.issue_id AND start_issue.changelog_id < issue.changelog_id
	WHERE start_issue.type = 7 AND start_issue.status = 3 AND ${issue_done}
	GROUP BY start_issue.project_id, start_issue.sprint_id, start_issue.issue_id) AS issuedata
LEFT OUTER JOIN gros.sprint ON issuedata.project_id = sprint.project_id AND issuedata.sprint_id = sprint.sprint_id
JOIN gros.project ON issuedata.project_id = project.project_id
