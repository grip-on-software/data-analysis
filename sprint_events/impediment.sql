SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date
FROM gros.issue
JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN gros.project ON issue.project_id = project.project_id
WHERE issue.impediment = TRUE AND prev_issue.impediment = FALSE