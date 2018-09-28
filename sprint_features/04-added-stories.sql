SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_added_stories
FROM gros.issue
JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
JOIN gros.issue AS old_issue
ON issue.issue_id = old_issue.issue_id AND issue.changelog_id = old_issue.changelog_id+1
WHERE issue.type = 7
AND old_issue.sprint_id <> 0 AND issue.sprint_id <> 0
AND issue.sprint_id <> old_issue.sprint_id
AND issue.updated > sprint.start_date
AND ${s(issue_not_done)}
GROUP BY issue.project_id, issue.sprint_id
