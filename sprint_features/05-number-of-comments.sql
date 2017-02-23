SELECT max_issue.project_id, max_issue.sprint_id, COUNT(*) AS num_comments
FROM gros.comment, (
    SELECT issue.project_id, issue.issue_id, issue.sprint_id, MAX(issue.updated) AS updated
    FROM gros.issue
    WHERE issue.sprint_id <> 0
    GROUP BY issue.project_id, issue.issue_id, issue.sprint_id
) AS max_issue
WHERE comment.issue_id = max_issue.issue_id AND comment."date" < max_issue.updated
GROUP BY max_issue.project_id, max_issue.sprint_id
