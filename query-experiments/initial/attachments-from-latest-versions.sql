SELECT attachments, COUNT(*) FROM issue,
(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
WHERE issue.issue_id = max_issue.issue_id AND issue.changelog_id = max_issue.changelog_id
GROUP BY attachments ORDER BY attachments DESC;
