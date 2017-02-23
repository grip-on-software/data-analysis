SELECT issue.project_id, issue.sprint_id, AVG(num_changes) AS avg_changes
FROM gros.issue, (
    SELECT issue.project_id, issue.issue_id, MAX(issue.changelog_id) AS changelog_id, COUNT(*) AS num_changes
    FROM gros.issue
    GROUP BY issue.project_id, issue.issue_id
) AS max_issue
WHERE issue.issue_id = max_issue.issue_id AND issue.changelog_id = max_issue.changelog_id
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
