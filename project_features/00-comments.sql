SELECT project_id, COUNT(*) AS num_comments
FROM gros.comment
JOIN gros.issue ON comment.issue_id = issue.issue_id AND issue.changelog_id = 0
GROUP BY project_id;
