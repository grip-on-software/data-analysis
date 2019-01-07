SELECT ${s(project_name)} AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date
FROM gros.issue
LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN gros.project ON issue.project_id = project.project_id
JOIN gros.issue AS older_issue ON issue.issue_id = older_issue.issue_id AND issue.changelog_id = older_issue.changelog_id + 1 
WHERE issue.rank_change IS NOT NULL
AND (older_issue.rank_change IS NULL OR older_issue.rank_change <> issue.rank_change)
AND issue.updated < ${s(sprint_close)}
ORDER BY project.project_id, sprint.start_date, issue.updated
