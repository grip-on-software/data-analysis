SELECT issue.project_id, issue.sprint_id, SUM(comment_count) AS total_comments FROM gros.issue,
(SELECT issue_id, COUNT(*) AS comment_count FROM gros.comment GROUP BY issue_id) AS comment_counts
WHERE issue.issue_id = comment_counts.issue_id AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
ORDER BY issue.project_id, issue.sprint_id;
