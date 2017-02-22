SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issuedata.date AS date, issuedata.end_date AS end_date 
FROM (SELECT issue.project_id, issue.sprint_id, issue.issue_id, MIN(issue.updated) AS date, MIN(end_issue.updated) AS end_date
	FROM gros.issue
	JOIN gros.issue AS end_issue ON issue.issue_id = end_issue.issue_id AND issue.changelog_id < end_issue.changelog_id
	WHERE issue.type = 7 AND issue.status = 3 AND (end_issue.resolution = 1 OR end_issue.status = 6)
	GROUP BY issue.project_id, issue.sprint_id, issue.issue_id) AS issuedata
LEFT OUTER JOIN gros.sprint ON issuedata.project_id = sprint.project_id AND issuedata.sprint_id = sprint.sprint_id
JOIN gros.project ON issuedata.project_id = project.project_id
