SELECT issue.issue_id, issue.title, COUNT(*) AS comment_count FROM gros.issue, gros.comment
WHERE comment.issue_id = issue.issue_id
GROUP BY issue.issue_id, issue.title
ORDER BY comment_count DESC;

SELECT DISTINCT issue.issue_id, issue.title, issue.sprint_id, comment_count FROM gros.issue INNER JOIN
(SELECT issue_id, count(*) AS comment_count FROM gros.comment GROUP BY issue_id) AS issue_comment ON issue.issue_id = issue_comment.issue_id
ORDER BY comment_count DESC;
